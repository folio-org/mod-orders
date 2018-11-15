package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.VendorDetail;
import org.folio.rest.jaxrs.resource.OrdersResource.PostOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrdersHelper {

  private static final Logger logger = Logger.getLogger(PostOrdersHelper.class);

  // epoch time @ 9/1/2018.
  // if you change this, you run the risk of duplicate poNumbers
  private static final long PO_NUMBER_EPOCH = 1535774400000L;

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public PostOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    compPO.setPoNumber(generatePoNumber());

    try {
      JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
      if (purchaseOrder.containsKey("adjustment")) {
        purchaseOrder.remove("adjustment");
      }
      if (purchaseOrder.containsKey("po_lines")) {
        purchaseOrder.remove("po_lines");
      }
      httpClient.request(HttpMethod.POST, purchaseOrder, "/purchase_order", okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(poBody -> {
          CompositePurchaseOrder po = poBody.mapTo(CompositePurchaseOrder.class);
          String poNumber = po.getPoNumber();
          String poId = po.getId();
          compPO.setId(poId);

          List<PoLine> lines = new ArrayList<>(compPO.getPoLines().size());
          List<CompletableFuture<Void>> futures = new ArrayList<>();
          for (int i = 0; i < compPO.getPoLines().size(); i++) {
            PoLine compPOL = compPO.getPoLines().get(i);
            compPOL.setPurchaseOrderId(poId);
            compPOL.setPoLineNumber(poNumber + "-" + (i + 1));

            futures.add(createPoLine(compPOL)
              .thenAccept(lines::add));
          }

          VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[futures.size()]))
            .thenAccept(v -> {
              compPO.setPoLines(lines);
              compPO.setAdjustment(HelperUtils.calculateAdjustment(lines));
              future.complete(compPO);
            })
            .exceptionally(e -> {
              future.completeExceptionally(e.getCause());
              return null;
            });
        })
        .exceptionally(e -> {
          future.completeExceptionally(e.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("Exception calling POST /purchase_order", e);
      future.completeExceptionally(e);
    }
    return future;
  }

  public CompletableFuture<PoLine> createPoLine(PoLine compPOL) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    JsonObject line = JsonObject.mapFrom(compPOL);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    //TODO handle alerts, claims, fund_distribution, reporting_codes, source
    line.remove("alerts");
    line.remove("claims");
    line.remove("fund_distribution");
    line.remove("reporting_codes");
    line.remove("source");
    line.remove("renewal"); 
    line.remove("license"); 
    
    subObjFuts.add(createAdjustment(compPOL, line, compPOL.getAdjustment()));
    subObjFuts.add(createCost(compPOL, line, compPOL.getCost()));
    subObjFuts.add(createDetails(compPOL, line, compPOL.getDetails()));
    subObjFuts.add(createEresource(compPOL, line, compPOL.getEresource()));
    subObjFuts.add(createLocation(compPOL, line, compPOL.getLocation()));
    subObjFuts.add(createPhysical(compPOL, line, compPOL.getPhysical()));
    subObjFuts.add(createVendorDetail(compPOL, line, compPOL.getVendorDetail()));

    CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[subObjFuts.size()]))
      .thenAccept(v -> {
        try {
          Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
          httpClient.request(HttpMethod.POST, polBuf, "/po_line", okapiHeaders)
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
    return createSubObjIfPresent(line, adjustment, "adjustment", "/adjustment")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("adjustment");
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
    return createSubObjIfPresent(line, cost, "cost", "/cost")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("cost");
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
    return createSubObjIfPresent(line, details, "details", "/details")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("details");
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
    return createSubObjIfPresent(line, eresource, "eresource", "/eresource")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("eresource");
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
    return createSubObjIfPresent(line, location, "location", "/location")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("location");
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
    return createSubObjIfPresent(line, physical, "physical", "/physical")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("physical");
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
    return createSubObjIfPresent(line, vendor, "vendor_detail", "/vendor_detail")
      .thenAccept(id -> {
        if (id == null) {
          line.remove("vendor_detail");
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
      httpClient.request(HttpMethod.POST, obj.toBuffer(), url, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(body -> {
          String id = JsonObject.mapFrom(body).getString("id");
          pol.put(field, id);
          future.complete(id);
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

  public CompletableFuture<CompositePurchaseOrder> applyFunds(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    future.complete(compPO);
    return future;
  }

  public CompletableFuture<CompositePurchaseOrder> updateInventory(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    future.complete(compPO);
    return future;
  }

  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception placing order", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = ((HttpException) t).getMessage();
      switch (code) {
      case 400:
        result = Future.succeededFuture(PostOrdersResponse.withPlainBadRequest(message));
        break;
      case 500:
        result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(message));
        break;
      case 401:
        result = Future.succeededFuture(PostOrdersResponse.withPlainUnauthorized(message));
        break;
      default:
        result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(message));
      }
    } else {
      result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

  public static String generatePoNumber() {
    return (Long.toHexString(System.currentTimeMillis() - PO_NUMBER_EPOCH) +
        Long.toHexString(System.nanoTime() % 100))
          .toUpperCase();
  }

}
