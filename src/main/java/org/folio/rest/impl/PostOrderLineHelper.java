package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.ResourcePathResolver.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrderLineHelper extends AbstractHelper {

  PostOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL) {
    compPOL.setId(UUID.randomUUID().toString());
    JsonObject line = JsonObject.mapFrom(compPOL);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    subObjFuts.add(createAdjustment(compPOL, line, compPOL.getAdjustment()));
    subObjFuts.add(createCost(compPOL, line, compPOL.getCost()));
    subObjFuts.add(createDetails(compPOL, line, compPOL.getDetails()));
    subObjFuts.add(createEresource(compPOL, line, compPOL.getEresource()));
    subObjFuts.add(createPhysical(compPOL, line, compPOL.getPhysical()));
    subObjFuts.add(createVendorDetail(compPOL, line, compPOL.getVendorDetail()));
    subObjFuts.add(createAlerts(compPOL, line, compPOL.getAlerts()));
    subObjFuts.add(createClaims(compPOL, line, compPOL.getClaims()));
    subObjFuts.add(createSource(compPOL, line, compPOL.getSource()));
    subObjFuts.add(createLocations(compPOL, line));
    subObjFuts.add(createReportingCodes(compPOL, line, compPOL.getReportingCodes()));
    subObjFuts.add(createFundDistribution(compPOL, line, compPOL.getFundDistribution()));

    return CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[0]))
      .thenCompose(v -> {
        try {
          Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
          return httpClient.request(HttpMethod.POST, polBuf, resourcesPath(PO_LINES), okapiHeaders)
            .thenApply(HelperUtils::verifyAndExtractBody)
            .thenApply(body -> {
              logger.info("response from /po_line: " + body.encodePrettily());

              compPOL.setId(body.getString(ID));
              return compPOL;
            });
        } catch (Exception e) {
          logger.error("Exception calling POST /po_line", e);
          throw new CompletionException(e);
        }
      });
  }



  private CompletableFuture<Void> createAdjustment(CompositePoLine compPOL, JsonObject line, Adjustment adjustment) {
    return createSubObjIfPresent(line, adjustment, ADJUSTMENT, resourcesPath(ADJUSTMENT))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ADJUSTMENT);
          compPOL.setAdjustment(null);
        } else {
          compPOL.getAdjustment().setId(id);
          compPOL.getAdjustment().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Adjustment", t);
        throw new CompletionException(t.getCause());
      });
  }


  private CompletableFuture<Void> createCost(CompositePoLine compPOL, JsonObject line, Cost cost) {
    return createSubObjIfPresent(line, cost, COST, resourcesPath(COST))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(COST);
          compPOL.setCost(null);
        } else {
          compPOL.getCost().setId(id);
          compPOL.getCost().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Cost", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createDetails(CompositePoLine compPOL, JsonObject line, Details details) {
    return createSubObjIfPresent(line, details, DETAILS, resourcesPath(DETAILS))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(DETAILS);
          compPOL.setDetails(null);
        } else {
          compPOL.getDetails().setId(id);
          compPOL.getDetails().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Details", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createEresource(CompositePoLine compPOL, JsonObject line, Eresource eresource) {
    return createSubObjIfPresent(line, eresource, ERESOURCE, resourcesPath(ERESOURCE))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ERESOURCE);
          compPOL.setEresource(null);
        } else {
          compPOL.getEresource().setId(id);
          compPOL.getEresource().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create EResource", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createLocations(CompositePoLine compPOL, JsonObject line) {
    List<Location> locations = compPOL.getLocations();
    if (CollectionUtils.isNotEmpty(locations)) {
      List<CompletableFuture<String>> futures = new ArrayList<>();
      Iterator<Location> iterator = locations.iterator();
      while (iterator.hasNext()) {
        Location location = iterator.next();
        futures.add(createSubObjIfPresent(line, location, LOCATIONS, resourcesPath(LOCATIONS))
          .thenApply(id -> {
            if (StringUtils.isEmpty(id)) {
              iterator.remove();
            } else {
              location.setId(id);
              location.setPoLineId(compPOL.getId());
            }
            return id;
        }));
      }

    return extractIdsOnSuccess(futures)
      .thenAccept(ids -> {
        if (CollectionUtils.isEmpty(ids)) {
          line.remove(LOCATIONS);
          compPOL.setLocations(Collections.emptyList());
        } else {
          line.put(LOCATIONS, ids);
        }
      })
      .exceptionally(t -> {
        logger.error("Failed to create Locations", t);
        throw new CompletionException(t.getCause());
      });
    } else {
      line.remove(LOCATIONS);
    }

    return completedFuture(null);
  }

  private CompletableFuture<List<String>> extractIdsOnSuccess(List<CompletableFuture<String>> futures) {
    return CompletableFuture
      .allOf(futures.toArray(new CompletableFuture[0]))
      .thenApply(v -> futures
        .stream()
        .map(CompletableFuture::join)
        .filter(Objects::nonNull)
        .collect(Collectors.toList())
      );
  }

  private CompletableFuture<Void> createPhysical(CompositePoLine compPOL, JsonObject line, Physical physical) {
    return createSubObjIfPresent(line, physical, PHYSICAL, resourcesPath(PHYSICAL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(PHYSICAL);
          compPOL.setPhysical(null);
        } else {
          compPOL.getPhysical().setId(id);
          compPOL.getPhysical().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Physical", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createVendorDetail(CompositePoLine compPOL, JsonObject line, VendorDetail vendor) {
    return createSubObjIfPresent(line, vendor, VENDOR_DETAIL, resourcesPath(VENDOR_DETAIL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(VENDOR_DETAIL);
          compPOL.setVendorDetail(null);
        } else {
          compPOL.getVendorDetail().setId(id);
          compPOL.getVendorDetail().setPoLineId(compPOL.getId());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create VendorDetail", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createReportingCodes(CompositePoLine compPOL, JsonObject line,
                                                       List<ReportingCode> reportingCodes) {

    List<String> reportingIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if(null!=reportingCodes)
      reportingCodes
        .forEach(reportingObject ->
          futures.add(createSubObjIfPresent(line, reportingObject, REPORTING_CODES, resourcesPath(REPORTING_CODES))
            .thenAccept(id -> {
              if (id != null) {
                reportingObject.setId(id);
                reportingIds.add(id);
              }
            }))
        );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(t -> {
        line.put(REPORTING_CODES, reportingIds);
        compPOL.setReportingCodes(reportingCodes);
      })
      .exceptionally(t -> {
        logger.error("failed to create Reporting Codes", t);
        throw new CompletionException(t.getCause());
      });

  }

  private CompletableFuture<Void> createSource(CompositePoLine compPOL, JsonObject line, Source source) {
    return createSubObjIfPresent(line, source, SOURCE, resourcesPath(SOURCE))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(SOURCE);
          compPOL.setSource(null);
        } else {
          compPOL.getSource().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Source", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createClaims(CompositePoLine compPOL, JsonObject line, List<Claim> claims) {

    List<String> claimsIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if(null!=claims)
      claims
        .forEach(claimObject ->
          futures.add(createSubObjIfPresent(line, claimObject, CLAIMS, resourcesPath(CLAIMS))
            .thenAccept(id -> {
              if (id != null) {
                claimObject.setId(id);
                claimObject.setPoLineId(compPOL.getId());
                claimsIds.add(id);
              }
            }))
        );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(t -> {
        line.put(CLAIMS, claimsIds);
        compPOL.setClaims(claims);
      })
      .exceptionally(t -> {
        logger.error("failed to create Claims", t);
        throw new CompletionException(t.getCause());
      });

  }



  private CompletableFuture<Void> createAlerts(CompositePoLine compPOL, JsonObject line, List<Alert> alerts) {

    List<String> alertIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if(null!=alerts)
      alerts.forEach(alertObject ->
        futures.add(createSubObjIfPresent(line, alertObject, ALERTS, resourcesPath(ALERTS))
          .thenAccept(id -> {
            if (id != null) {
              alertObject.setId(id);
              alertObject.setPoLineId(compPOL.getId());
              alertIds.add(id);
            }
          }))
      );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(t -> {
        line.put(ALERTS, alertIds);
        compPOL.setAlerts(alerts);
      })
      .exceptionally(t -> {
        logger.error("failed to create Alerts", t);
        throw new CompletionException(t.getCause());
      });

  }

  private CompletableFuture<String> createSubObjIfPresent(JsonObject line, Object obj, String field, String url) {
    if (obj != null) {
      JsonObject json = JsonObject.mapFrom(obj);
      if (!json.isEmpty()) {
        return createSubObj(line, json, field, url);
      }
    }
    return completedFuture(null);
  }


  private CompletableFuture<String> createSubObj(JsonObject pol, JsonObject obj, String field, String url) {
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);
    if (!(SOURCE.equals(field) || REPORTING_CODES.equals(field))) {
      obj.put("po_line_id", pol.getString(ID));
    }
    try {
      operateOnSubObj(HttpMethod.POST, url, obj, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(body -> {
          String id = JsonObject.mapFrom(body).getString("id");
          pol.put(field, id);
          future.complete(id);
          logger.debug("The '{}' sub-object successfully created with id={}", field, id);
        })
        .exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  private CompletableFuture<Void> createFundDistribution(CompositePoLine compPOL, JsonObject line, List<FundDistribution> fundDistribution) {

    List<String> fundDistributionIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if (null != fundDistribution)
      fundDistribution
        .forEach(fundObject ->
          futures.add(createSubObjIfPresent(line, fundObject, FUND_DISTRIBUTION, resourcesPath(FUND_DISTRIBUTION))
            .thenAccept(id -> {
              if (id != null) {
                fundObject.setId(id);
                fundObject.setPoLineId(compPOL.getId());
                fundDistributionIds.add(id);
              }
            }))
        );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(t -> {
        line.put(FUND_DISTRIBUTION, fundDistributionIds);
        compPOL.setFundDistribution(fundDistribution);
      })
      .exceptionally(t -> {
        logger.error("failed to create FundDistribution", t);
        throw new CompletionException(t.getCause());
      });

  }


  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = Orders.PostOrdersOrderLinesResponse.respond400WithTextPlain(error.getMessage());
        break;
      case 401:
        result = Orders.PostOrdersOrderLinesResponse.respond401WithTextPlain(error.getMessage());
        break;
      case 422:
        result = Orders.PostOrdersOrderLinesResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = Orders.PostOrdersOrderLinesResponse.respond500WithTextPlain(error.getMessage());
    }
    return result;
  }
}
