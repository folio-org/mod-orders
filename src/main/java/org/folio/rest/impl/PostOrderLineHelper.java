package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.HelperUtils.FUND_DISTRIBUTION;

public class PostOrderLineHelper extends AbstractOrderLineHelper  {

  private static final Map<String,String> subObjectApisPost = HelperUtils.getSubObjectapisForPost();

  PostOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx);
  }


  public CompletableFuture<PoLine> createPoLine(PoLine compPOL) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

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
    subObjFuts.add(createRenewal(compPOL, line, compPOL.getRenewal()));
    subObjFuts.add(createReportingCodes(compPOL, line, compPOL.getReportingCodes()));
    subObjFuts.add(createFundDistribution(compPOL, line, compPOL.getFundDistribution()));

    CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[subObjFuts.size()]))
      .thenAccept(v -> {
        try {
          Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
          httpClient.request(HttpMethod.POST, polBuf, subObjectApisPost.get(PO_LINES), okapiHeaders)
            .thenApply(HelperUtils::verifyAndExtractBody)
            .thenAccept(body -> {
              logger.info("response from /po_line: " + body.encodePrettily());

              compPOL.setId(body.getString("id"));
              future.complete(compPOL);
            })
            .exceptionally(t -> {
              logger.error("failed to update CompositePoLine", t);
              future.completeExceptionally(t.getCause());
              return null;
            });
        } catch (Exception e) {
          logger.error("Exception calling POST /po_line", e);
          future.completeExceptionally(e.getCause());
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create POLine sub-object", t);
        future.completeExceptionally(t.getCause());
        return null;
      });
    return future;
  }

  private CompletableFuture<Void> createAdjustment(PoLine compPOL, JsonObject line, Adjustment adjustment) {
    return createSubObjIfPresent(line, adjustment,
      ADJUSTMENT, subObjectApisPost.get(ADJUSTMENT))
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
    return createSubObjIfPresent(line, cost,
      COST, subObjectApisPost.get(COST))
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
    return createSubObjIfPresent(line, details, DETAILS, subObjectApisPost.get(DETAILS))
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
    return createSubObjIfPresent(line, eresource, ERESOURCE, subObjectApisPost.get(ERESOURCE))
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
    return createSubObjIfPresent(line, location, LOCATION, subObjectApisPost.get(LOCATION))
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
    return createSubObjIfPresent(line, physical, PHYSICAL, subObjectApisPost.get(PHYSICAL))
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
    return createSubObjIfPresent(line, vendor, VENDOR_DETAIL, subObjectApisPost.get(VENDOR_DETAIL))
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
          futures.add(createSubObjIfPresent(line, reportingObject,
            REPORTING_CODES, subObjectApisPost.get(REPORTING_CODES))
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

  private CompletableFuture<Void> createRenewal(PoLine compPOL, JsonObject line, Renewal renewal) {
    return createSubObjIfPresent(line, renewal, RENEWAL, subObjectApisPost.get(RENEWAL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(RENEWAL);
          compPOL.setRenewal(null);
        } else {
          compPOL.getRenewal().setId(id);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Renewal", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createSource(PoLine compPOL, JsonObject line, Source source) {
    return createSubObjIfPresent(line, source, SOURCE, subObjectApisPost.get(SOURCE))
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
          futures.add(createSubObjIfPresent(line, claimObject, CLAIMS,
            subObjectApisPost.get(CLAIMS))
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
        futures.add(createSubObjIfPresent(line, alertObject, ALERTS,
          subObjectApisPost.get(ALERTS))
          .thenAccept(id -> {
            if (id != null) {
              alertObject.setId(id);
              alertIds.add(id);
            }
          }))
      );

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]))
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
    return CompletableFuture.completedFuture(null);
  }


  private CompletableFuture<String> createSubObj(JsonObject pol, JsonObject obj, String field, String url) {
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);

    try {
      logger.debug("Calling POST {}", url);
      httpClient.request(HttpMethod.POST, obj.toBuffer(), url, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
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
          futures.add(createSubObjIfPresent(line, fundObject, FUND_DISTRIBUTION,
            subObjectApisPost.get(FUND_DISTRIBUTION))
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
  Response buildErrorResponse(int code, String message) {
    final Response result;
    switch (code) {
      case 400:
        result = Orders.PostOrdersLinesByIdResponse.respond400WithTextPlain(message);
        break;
      case 401:
        result = Orders.PostOrdersLinesByIdResponse.respond401WithTextPlain(message);
        break;
      case 422:
        Errors errors = new Errors();
        errors.getErrors()
          .add(new Error().withMessage(message));
        result = Orders.PostOrdersLinesByIdResponse.respond422WithApplicationJson(errors);
        break;
      default:
        result = Orders.PostOrdersLinesByIdResponse.respond500WithTextPlain(message);
    }
    return result;
  }
}
