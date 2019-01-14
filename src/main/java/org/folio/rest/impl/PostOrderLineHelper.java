package org.folio.rest.impl;

import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.*;
import static org.folio.rest.tools.client.Response.isSuccess;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import javax.ws.rs.core.Response;

import org.folio.orders.rest.exceptions.HttpException;
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
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrderLineHelper extends AbstractHelper {

  private static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  private static final String DEFAULT_STATUS_CODE = "temp";
  private static final String LOCATION_HEADER = "Location";

  PostOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }


  public CompletableFuture<PoLine> createPoLine(PoLine compPOL, boolean updateInventory) {

    if (updateInventory) {
      return getProductTypesMap(compPOL)
        .thenCompose(productTypesMap -> getInstanceRecord(compPOL, productTypesMap))
        .thenCompose(instanceId -> createPoLine(compPOL, instanceId));
    }
    return createPoLine(compPOL, null);
  }

  private CompletableFuture<PoLine> createPoLine(PoLine compPOL, String instanceId) {
    if (instanceId != null) {
      compPOL.setInstanceId(instanceId);
    }
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

              compPOL.setId(body.getString("id"));
              return compPOL;
            });
        } catch (Exception e) {
          logger.error("Exception calling POST /po_line", e);
          throw new CompletionException(e);
        }
      });
  }

  /**
   * Retrieves product type details associated with given PO line
   * and builds 'product type name' -> 'product type id' map.
   *
   * @param compPOL the PO line to retrieve product type details for
   * @return product types map
   */
  private CompletableFuture<Map<String, String>> getProductTypesMap(PoLine compPOL) {
    // do not fail if no productId is provided, should be enforced on schema level if it's required
    if (compPOL.getDetails() == null || compPOL.getDetails().getProductIds().isEmpty()) {
      return completedFuture(Collections.emptyMap());
    }

    String endpoint = compPOL.getDetails().getProductIds().stream()
      .map(productId -> String.format("name==%s", productId.getProductIdType().toString()))
      .collect(joining(" or ", "/identifier-types?query=", ""));

    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(productTypes -> {
        if (productTypes.getJsonArray("identifierTypes").size() != compPOL.getDetails().getProductIds().size()) {
          throw new CompletionException(new HttpException(422,
            "Invalid product type(s) is specified for the PO line with id " + compPOL.getId()));
        }
        return productTypes;
      })
      .thenApply(productTypes -> productTypes.getJsonArray("identifierTypes").stream()
        .collect(toMap(jsonObj -> ((JsonObject) jsonObj).getString("name"),
          jsonObj -> ((JsonObject) jsonObj).getString("id"),
          (k1, k2) -> k1)));
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param compPOL PO line to retrieve Instance Record Id for
   * @param productTypesMap product types Map used to build Inventory query
   * @return future with Instance Id
   */
  private CompletionStage<String> getInstanceRecord(PoLine compPOL, Map<String, String> productTypesMap) {
    // proceed with new Instance Record creation if no productId is provided
    if (compPOL.getDetails() == null || compPOL.getDetails().getProductIds().isEmpty()) {
      return createInstanceRecord(compPOL, productTypesMap);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(productId -> buildProductIdQuery(productId, productTypesMap))
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = null;
    try {
      endpoint = "/inventory/instances?query=" + URLEncoder.encode(query, "UTF-8");
    } catch (UnsupportedEncodingException e) {
      logger.error(String.format("Error during query encoding: %s", e.getMessage()));
      throw new CompletionException(e);
    }

    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(instances -> {
        if (instances.getJsonArray("instances").size() > 0) {
          return completedFuture(instances.getJsonArray("instances").getJsonObject(0).getString("id"));
        }
        return createInstanceRecord(compPOL, productTypesMap);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param compPOL PO line to create Instance Record for
   * @param productTypesMap product types Map used to build Instance Record json object
   * @return id of newly created Instance Record
   */
  private CompletableFuture<String> createInstanceRecord(PoLine compPOL, Map<String, String> productTypesMap) {
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = getInstanceType(DEFAULT_INSTANCE_TYPE_CODE)
      .thenAccept(lookupObj::mergeIn);
    CompletableFuture<Void> statusFuture = getStatus(DEFAULT_STATUS_CODE)
      .thenAccept(lookupObj::mergeIn);

    return VertxCompletableFuture.allOf(ctx, instanceTypeFuture, statusFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(compPOL, productTypesMap, lookupObj))
      .thenCompose(instanceRecJson -> {
        try {
          return httpClient.request(HttpMethod.POST, instanceRecJson.toBuffer(), "/inventory/instances", okapiHeaders)
            .thenApply(response -> {
              logger.debug("Validating received response");
              if (!isSuccess(response.getCode())) {
                throw new CompletionException(
                  new HttpException(response.getCode(), response.getError().getString("errorMessage")));
              }
              return response;
            })
            .thenApply(response -> {
              String location = response.getHeaders().get(LOCATION_HEADER);
              return location.substring(location.lastIndexOf('/')+1);
            });
        } catch (Exception e) {
          throw new CompletionException(e);
        }
      });
  }

  private CompletableFuture<JsonObject> getInstanceType(String typeName) {
    String endpoint = String.format("/instance-types?query=code==%s", typeName);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  private CompletableFuture<JsonObject> getStatus(String statusCode) {
    String endpoint = String.format("/instance-statuses?query=code==%s", statusCode);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  private String buildProductIdQuery(ProductId productId, Map<String, String> productTypes) {
    return String.format("(identifiers adj \"\\\"identifierTypeId\\\": \\\"%s\\\"\" " +
        "and identifiers adj \"\\\"value\\\": \\\"%s\\\"\")",
      productTypes.get(productId.getProductIdType().toString()),
      productId.getProductId());
  }

  private JsonObject buildInstanceRecordJsonObject(PoLine compPOL, Map<String, String> productTypes, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();
    if (compPOL.getSource() != null) {
      instance.put("source", compPOL.getSource().getCode());
    }
    if (compPOL.getTitle() != null) {
      instance.put("title", compPOL.getTitle());
    }
    if (compPOL.getEdition() != null) {
      instance.put("editions", new JsonArray(singletonList(compPOL.getEdition())));
    }
    instance.put("statusId", lookupObj.getJsonArray("instanceStatuses").getJsonObject(0).getString("id"));
    instance.put("instanceTypeId", lookupObj.getJsonArray("instanceTypes").getJsonObject(0).getString("id"));

    if (compPOL.getPublisher() != null || compPOL.getPublicationDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put("publisher", compPOL.getPublisher());
      publication.put("dateOfPublication", compPOL.getPublicationDate());
      instance.put("publication", new JsonArray(Arrays.asList(publication)));
    }

    if (compPOL.getDetails() != null && compPOL.getDetails().getProductIds() != null) {
      List<JsonObject> identifiers = compPOL.getDetails().getProductIds().stream()
        .map(pId -> {
          JsonObject identifier = new JsonObject();
          identifier.put("identifierTypeId", productTypes.get(pId.getProductIdType().toString()));
          identifier.put("value", pId.getProductId());
          return identifier;
        })
        .collect(toList());
      instance.put("identifiers", new JsonArray(identifiers));
    }
    return instance;
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
