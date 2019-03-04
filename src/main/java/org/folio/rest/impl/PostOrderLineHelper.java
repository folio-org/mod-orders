package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.ResourcePathResolver.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

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

  public static final String PO_LINE_URL_WITH_PARAM = "%s?purchaseOrderId=%s";

  PostOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL) {
    compPOL.setId(UUID.randomUUID().toString());
    JsonObject line = JsonObject.mapFrom(compPOL);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

//    subObjFuts.add(createAdjustment(compPOL, line));
//    subObjFuts.add(createCost(compPOL, line));
//    subObjFuts.add(createDetails(compPOL, line));
//    subObjFuts.add(createEresource(compPOL, line));
//    subObjFuts.add(createPhysical(compPOL, line));
//    subObjFuts.add(createVendorDetail(compPOL, line));
    subObjFuts.add(createAlerts(compPOL, line));
//    subObjFuts.add(createClaims(compPOL, line));
//    subObjFuts.add(createSource(compPOL, line));
//    subObjFuts.add(createLocations(compPOL, line));
    subObjFuts.add(createReportingCodes(compPOL, line));
//    subObjFuts.add(createFundDistribution(compPOL, line));

    return CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[0]))
      .thenCompose(v -> handleGetRequest(getPoLineNumberEndpoint(compPOL.getPurchaseOrderId()), httpClient, ctx, okapiHeaders, logger)
      .thenAccept(sequenceNumber -> addSuffixToPoLineNumber(line, sequenceNumber)))
      .thenCompose(v -> createPoLineSummary(compPOL, line));
  }

  private CompletionStage<CompositePoLine> createPoLineSummary(CompositePoLine compPOL, JsonObject line) {
    try {
      Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
      return httpClient.request(HttpMethod.POST, polBuf, resourcesPath(PO_LINES), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenApply(body -> {
          logger.info("response from /po_line: " + body.encodePrettily());

          compPOL.setId(body.getString(ID));
          compPOL.setPoLineNumber(body.getString(PO_LINE_NUMBER));
          return compPOL;
        });
    } catch (Exception e) {
      logger.error("Exception calling POST /po_line", e);
      throw new CompletionException(e);
    }
  }

  private String getPoLineNumberEndpoint(String id) {
    return String.format(PO_LINE_URL_WITH_PARAM, resourcesPath(PO_LINE_NUMBER), id);
  }

  private void addSuffixToPoLineNumber(JsonObject line, JsonObject sequenceNumber) {
    String poLineNumberSuffix = sequenceNumber.getString("sequenceNumber");
    String purchaseOrderNumber = line.getString(PO_LINE_NUMBER);
    line.put(PO_LINE_NUMBER, purchaseOrderNumber + "-" + poLineNumberSuffix);
  }


  private CompletableFuture<Void> createAdjustment(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getAdjustment(), ADJUSTMENT, resourcesPath(ADJUSTMENT))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ADJUSTMENT);
          compPOL.setAdjustment(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Adjustment", t);
        throw new CompletionException(t.getCause());
      });
  }


  private CompletableFuture<Void> createCost(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getCost(), COST, resourcesPath(COST))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(COST);
          compPOL.setCost(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Cost", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createDetails(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getDetails(), DETAILS, resourcesPath(DETAILS))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(DETAILS);
          compPOL.setDetails(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Details", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createEresource(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getEresource(), ERESOURCE, resourcesPath(ERESOURCE))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(ERESOURCE);
          compPOL.setEresource(null);
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
            }
            return id;
        }));
      }

    return collectResultsOnSuccess(futures)
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

  private CompletableFuture<Void> createPhysical(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getPhysical(), PHYSICAL, resourcesPath(PHYSICAL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(PHYSICAL);
          compPOL.setPhysical(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Physical", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createVendorDetail(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getVendorDetail(), VENDOR_DETAIL, resourcesPath(VENDOR_DETAIL))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(VENDOR_DETAIL);
          compPOL.setVendorDetail(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create VendorDetail", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createReportingCodes(CompositePoLine compPOL, JsonObject line) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<ReportingCode> reportingCodes = compPOL.getReportingCodes();
    if (null != reportingCodes)
      reportingCodes
        .forEach(reportingObject ->
          futures.add(createSubObjIfPresent(line, reportingObject, REPORTING_CODES, resourcesPath(REPORTING_CODES))
            .thenApply(id -> {
              if (id != null)
                reportingObject.setId(id);
              return id;
            }))
        );

    return collectResultsOnSuccess(futures)
      .thenAccept(reportingIds -> line.put(REPORTING_CODES, reportingIds))
      .exceptionally(t -> {
        logger.error("failed to create Reporting Codes", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createSource(CompositePoLine compPOL, JsonObject line) {
    return createSubObjIfPresent(line, compPOL.getSource(), SOURCE, resourcesPath(SOURCE))
      .thenAccept(id -> {
        if (id == null) {
          line.remove(SOURCE);
          compPOL.setSource(null);
        }
      })
      .exceptionally(t -> {
        logger.error("failed to create Source", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createClaims(CompositePoLine compPOL, JsonObject line) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<Claim> claims = compPOL.getClaims();
    if(null!=claims)
      claims
        .forEach(claimObject ->
          futures.add(createSubObjIfPresent(line, claimObject, CLAIMS, resourcesPath(CLAIMS)))
        );

    return collectResultsOnSuccess(futures)
      .thenAccept(ids -> line.put(CLAIMS, ids))
      .exceptionally(t -> {
        logger.error("failed to create Claims", t);
        throw new CompletionException(t.getCause());
      });

  }

  private CompletableFuture<Void> createAlerts(CompositePoLine compPOL, JsonObject line) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<Alert> alerts = compPOL.getAlerts();
    if (null != alerts)
      alerts.forEach(alertObject ->
        futures.add(createSubObjIfPresent(line, alertObject, ALERTS, resourcesPath(ALERTS)))
      );

    return collectResultsOnSuccess(futures)
      .thenAccept(ids -> line.put(ALERTS, ids))
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
//    if (!(SOURCE.equals(field) || REPORTING_CODES.equals(field))) {
//      obj.put("poLineId", pol.getString(ID));
//    }
    try {
      operateOnSubObj(HttpMethod.POST, url, obj, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(body -> {
          String id = JsonObject.mapFrom(body).getString("id");
          logger.debug("Field= '{}' id={}", field, id);
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

//  private CompletableFuture<Void> createFundDistribution(CompositePoLine compPOL, JsonObject line) {
//    List<CompletableFuture<String>> futures = new ArrayList<>();
//
//    List<FundDistribution> fundDistribution = compPOL.getFundDistribution();
//    if (null != fundDistribution)
//      fundDistribution
//        .forEach(fundObject ->
//          futures.add(createSubObjIfPresent(line, fundObject, FUND_DISTRIBUTION, resourcesPath(FUND_DISTRIBUTION)))
//        );
//
//    return collectResultsOnSuccess(futures)
//      .thenAccept(fundDistributionIds -> line.put(FUND_DISTRIBUTION, fundDistributionIds))
//      .exceptionally(t -> {
//        logger.error("failed to create FundDistribution", t);
//        throw new CompletionException(t.getCause());
//      });
//  }


  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = Orders.PostOrdersOrderLinesResponse.respond400WithApplicationJson(withErrors(error));
        break;
      case 401:
        result = Orders.PostOrdersOrderLinesResponse.respond401WithApplicationJson(withErrors(error));
        break;
      case 404:
        result = Orders.PostOrdersOrderLinesResponse.respond400WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = Orders.PostOrdersOrderLinesResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = Orders.PostOrdersOrderLinesResponse.respond500WithApplicationJson(withErrors(error));
    }
    return result;
  }
}
