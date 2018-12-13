package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.VendorDetail;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

public class PostOrderLineHelper extends AbstractOrderLineHelper {
  public static final String ADJUSTMENT = "adjustment";

  PostOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx);
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
    return createSubObjIfPresent(line, adjustment, ADJUSTMENT, "/adjustment")
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
