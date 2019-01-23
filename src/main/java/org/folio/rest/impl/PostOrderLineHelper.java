package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.ResourcePathResolver.ADJUSTMENT;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.CLAIMS;
import static org.folio.orders.utils.ResourcePathResolver.COST;
import static org.folio.orders.utils.ResourcePathResolver.DETAILS;
import static org.folio.orders.utils.ResourcePathResolver.ERESOURCE;
import static org.folio.orders.utils.ResourcePathResolver.FUND_DISTRIBUTION;
import static org.folio.orders.utils.ResourcePathResolver.LOCATION;
import static org.folio.orders.utils.ResourcePathResolver.PHYSICAL;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.SOURCE;
import static org.folio.orders.utils.ResourcePathResolver.VENDOR_DETAIL;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.Claim;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Source;
import org.folio.rest.jaxrs.model.VendorDetail;
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

  public CompletableFuture<PoLine> createPoLine(PoLine compPOL) {
    JsonObject line = JsonObject.mapFrom(compPOL);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    subObjFuts.add(createAdjustment(compPOL, line, compPOL.getAdjustment()));
    subObjFuts.add(createCost(compPOL, line, compPOL.getCost()));
    subObjFuts.add(createDetails(compPOL, line, compPOL.getDetails()));
    subObjFuts.add(createEresource(compPOL, line, compPOL.getEresource()));
    subObjFuts.add(createLocation(compPOL, line, compPOL.getLocation()));
    subObjFuts.add(createPhysical(compPOL, line, compPOL.getPhysical()));
    subObjFuts.add(createVendorDetail(compPOL, line, compPOL.getVendorDetail()));
    subObjFuts.add(createAlerts(compPOL, line, compPOL.getAlerts()));
    subObjFuts.add(createClaims(compPOL, line, compPOL.getClaims()));
    subObjFuts.add(createSource(compPOL, line, compPOL.getSource()));
    subObjFuts.add(createReportingCodes(compPOL, line, compPOL.getReportingCodes()));
    subObjFuts.add(createFundDistribution(compPOL, line, compPOL.getFundDistribution()));

    return CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[subObjFuts.size()]))
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



  private CompletableFuture<Void> createAdjustment(PoLine compPOL, JsonObject line, Adjustment adjustment) {
    return createSubObjIfPresent(line, adjustment, ADJUSTMENT, resourcesPath(ADJUSTMENT))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ADJUSTMENT);
          compPOL.setAdjustment(null);
        } else {
          compPOL.getAdjustment().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Adjustment", t);
        throw new CompletionException(t.getCause());
      });
  }


  private CompletableFuture<Void> createCost(PoLine compPOL, JsonObject line, Cost cost) {
    return createSubObjIfPresent(line, cost, COST, resourcesPath(COST))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(COST);
          compPOL.setCost(null);
        } else {
          compPOL.getCost().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Cost", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createDetails(PoLine compPOL, JsonObject line, Details details) {
    return createSubObjIfPresent(line, details, DETAILS, resourcesPath(DETAILS))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(DETAILS);
          compPOL.setDetails(null);
        } else {
          compPOL.getDetails().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Details", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createEresource(PoLine compPOL, JsonObject line, Eresource eresource) {
    return createSubObjIfPresent(line, eresource, ERESOURCE, resourcesPath(ERESOURCE))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ERESOURCE);
          compPOL.setEresource(null);
        } else {
          compPOL.getEresource().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create EResource", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createLocation(PoLine compPOL, JsonObject line, Location location) {
    return createSubObjIfPresent(line, location, LOCATION, resourcesPath(LOCATION))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(LOCATION);
          compPOL.setLocation(null);
        } else {
          compPOL.getLocation().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Location", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createPhysical(PoLine compPOL, JsonObject line, Physical physical) {
    return createSubObjIfPresent(line, physical, PHYSICAL, resourcesPath(PHYSICAL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(PHYSICAL);
          compPOL.setPhysical(null);
        } else {
          compPOL.getPhysical().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Physical", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createVendorDetail(PoLine compPOL, JsonObject line, VendorDetail vendor) {
    return createSubObjIfPresent(line, vendor, VENDOR_DETAIL, resourcesPath(VENDOR_DETAIL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(VENDOR_DETAIL);
          compPOL.setVendorDetail(null);
        } else {
          compPOL.getVendorDetail().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create VendorDetail", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createReportingCodes(PoLine compPOL, JsonObject line,
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

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]))
      .thenAccept(t -> {
        line.put(REPORTING_CODES, reportingIds);
        compPOL.setReportingCodes(reportingCodes);
      })
      .exceptionally(t -> {
        logger.error("failed to create Reporting Codes", t);
        throw new CompletionException(t.getCause());
      });

  }

  private CompletableFuture<Void> createSource(PoLine compPOL, JsonObject line, Source source) {
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

  private CompletableFuture<Void> createClaims(PoLine compPOL, JsonObject line, List<Claim> claims) {

    List<String> claimsIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if(null!=claims)
      claims
        .forEach(claimObject ->
          futures.add(createSubObjIfPresent(line, claimObject, CLAIMS, resourcesPath(CLAIMS))
            .thenAccept(id -> {
              if (id != null) {
                claimObject.setId(id);
                claimsIds.add(id);
              }
            }))
        );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]))
      .thenAccept(t -> {
        line.put(CLAIMS, claimsIds);
        compPOL.setClaims(claims);
      })
      .exceptionally(t -> {
        logger.error("failed to create Claims", t);
        throw new CompletionException(t.getCause());
      });

  }



  private CompletableFuture<Void> createAlerts(PoLine compPOL, JsonObject line, List<Alert> alerts) {

    List<String> alertIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if(null!=alerts)
      alerts.forEach(alertObject ->
        futures.add(createSubObjIfPresent(line, alertObject, ALERTS, resourcesPath(ALERTS))
          .thenAccept(id -> {
            if (id != null) {
              alertObject.setId(id);
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

  private CompletableFuture<Void> createFundDistribution(PoLine compPOL, JsonObject line, List<FundDistribution> fundDistribution) {

    List<String> fundDistributionIds = new ArrayList<>();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if (null != fundDistribution)
      fundDistribution
        .forEach(fundObject ->
          futures.add(createSubObjIfPresent(line, fundObject, FUND_DISTRIBUTION, resourcesPath(FUND_DISTRIBUTION))
            .thenAccept(id -> {
              if (id != null) {
                fundObject.setId(id);
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
