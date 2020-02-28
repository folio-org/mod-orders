package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.crud.CrudObjectResolver;
import org.folio.rest.impl.crud.objects.GenericCrudObject;


import javax.ws.rs.core.Response;
import java.util.Map;


public class BaseCrudHelper<T, E> extends AbstractHelper {

  private GenericCrudObject<T> crudObject;
  private String urlPrefix;
  private Class<T> clazz;
  private Class<E> collectionClazz;

  public BaseCrudHelper(Class<T> clazz, Class<E> collectionClazz, String urlPrefix, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
    this.crudObject = CrudObjectResolver.getCrudObject(clazz);
    this.clazz = clazz;
    this.collectionClazz = collectionClazz;
    this.urlPrefix = urlPrefix;
  }

  public void get(String query, int offset, int limit, Handler<AsyncResult<Response>> asyncResultHandler) {
    String endpoint = resourcesPath(crudObject.getName()) + String.format(SEARCH_PARAMS, limit, offset, buildQuery(query, logger), lang);
    HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(json -> VertxCompletableFuture.supplyBlockingAsync(ctx, () -> json.mapTo(collectionClazz)))
      .thenAccept(objects -> asyncResultHandler.handle(succeededFuture(this.buildOkResponse(objects))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, this, fail));
  }

  public void create(T entity, Handler<AsyncResult<Response>> asyncResultHandler) {
    createRecordInStorage(JsonObject.mapFrom(entity), resourcesPath(crudObject.getName()))
      .thenApply(id -> crudObject.withId(entity, id))
      .thenAccept(obj -> asyncResultHandler.handle(
        succeededFuture(buildResponseWithLocation(String.format(urlPrefix, crudObject.getId(obj)), obj))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, this, fail));
  }

  public void update(String id, T entity, Handler<AsyncResult<Response>> asyncResultHandler) {
    // Set id if this is available only in path
    if (isEmpty(crudObject.getId(entity))) {
      crudObject.setId(entity, id);
    } else if (!id.equals(crudObject.getId(entity))) {
      this.addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(this.buildErrorResponse(422)));
      return;
    }
    HelperUtils.handlePutRequest(resourceByIdPath(crudObject.getName(), crudObject.getId(entity)), JsonObject.mapFrom(entity), httpClient, ctx, okapiHeaders, logger)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(this.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, this, fail));
  }

  public void getById(String id, Handler<AsyncResult<Response>> asyncResultHandler) {
    HelperUtils.handleGetRequest(resourceByIdPath(crudObject.getName(), id), httpClient, ctx, okapiHeaders,logger)
      .thenApply(json -> json.mapTo(clazz))
      .thenAccept(title -> asyncResultHandler.handle(succeededFuture(this.buildOkResponse(title))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, this, fail));
  }

  public void delete(String id, Handler<AsyncResult<Response>> asyncResultHandler) {
    HelperUtils.handleDeleteRequest(resourceByIdPath(crudObject.getName(), id), httpClient, ctx, okapiHeaders, logger)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(this.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, this, fail));
  }
}
