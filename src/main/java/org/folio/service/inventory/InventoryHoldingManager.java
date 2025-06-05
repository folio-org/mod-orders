package org.folio.service.inventory;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.orders.utils.StreamUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestConstants.NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_BY_ID_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.PARTIALLY_RETURNED_COLLECTION;
import static org.folio.service.inventory.InventoryUtils.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

public class InventoryHoldingManager {
  private static final Logger logger = LogManager.getLogger(InventoryHoldingManager.class);

  public static final String ID = "id";
  public static final String HOLDING_INSTANCE_ID = "instanceId";
  public static final String HOLDING_PERMANENT_LOCATION_ID = "permanentLocationId";
  public static final String HOLDING_SOURCE = "sourceId";

  public static final String HOLDINGS_RECORDS_BY_ID_ENDPOINT = "holdingsRecordsById";

  private static final String TENANT_SPECIFIC_KEY_FORMAT = "%s.%s.%s";
  public static final String HOLDINGS_LOOKUP_QUERY = "instanceId==%s and permanentLocationId==%s";

  private final RestClient restClient;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final InventoryCache inventoryCache;

  public InventoryHoldingManager(RestClient restClient,
                                 ConfigurationEntriesCache configurationEntriesCache,
                                 InventoryCache inventoryCache) {
    this.restClient = restClient;
    this.configurationEntriesCache = configurationEntriesCache;
    this.inventoryCache = inventoryCache;
  }

  public Future<String> getOrCreateHoldingsRecord(String instanceId, Location location, RequestContext requestContext) {
    if (Objects.nonNull(location.getHoldingId())) {
      return getFromCacheOrCreateHolding(location.getHoldingId(), requestContext);
    } else {
      return createHoldingAndReturnId(instanceId, location.getLocationId(), requestContext);
    }
  }

  public Future<String> getFromCacheOrCreateHolding(String holdingId, RequestContext requestContext) {
    Context ctx = requestContext.getContext();
    String tenantId = TenantTool.tenantId(requestContext.getHeaders());

    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holdingId);

    Future<String> holdingIdFuture;
    var holdingIdKey = String.format(TENANT_SPECIFIC_KEY_FORMAT, tenantId, "getFromCacheOrCreateHolding", holdingId);
    String holdingIdCached = ctx.get(holdingIdKey);

    if (Objects.nonNull(holdingIdCached)) {
      holdingIdFuture = Future.succeededFuture(holdingIdCached);
    } else {
      holdingIdFuture = restClient.getAsJsonObject(requestEntry, requestContext)
        .onSuccess(id -> ctx.put(holdingIdKey, id))
        .map(holdingJson -> {
          var id = HelperUtils.extractId(holdingJson);
          ctx.put(holdingIdKey, id);
          return id;
        });
    }

    return holdingIdFuture.recover(throwable -> {
      handleHoldingsError(holdingId, throwable);
      return null;
    });
  }

  public Future<JsonObject> getOrCreateHoldingsJsonRecord(Eresource eresource, String instanceId, Location location, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(location.getHoldingId())) {
      String holdingId = location.getHoldingId();
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);
      return restClient.getAsJsonObject(requestEntry, requestContext)
        .recover(throwable -> {
          handleHoldingsError(holdingId, throwable);
          return null;
        });
    } else if (Eresource.CreateInventory.NONE == eresource.getCreateInventory() || Eresource.CreateInventory.INSTANCE == eresource.getCreateInventory()) {
      if (location.getQuantityPhysical() != null && location.getQuantityPhysical() > 0) {
        return createHolding(instanceId, location.getLocationId(), requestContext);
      }
    } else {
      return createHolding(instanceId, location.getLocationId(), requestContext);
    }
    return Future.succeededFuture();
  }

  private static void handleHoldingsError(String holdingId, Throwable throwable) {
    if (throwable instanceof HttpException httpException && httpException.getCode() == 404) {
      String msg = String.format(HOLDINGS_BY_ID_NOT_FOUND.getDescription(), holdingId);
      Error error = new Error().withCode(HOLDINGS_BY_ID_NOT_FOUND.getCode()).withMessage(msg);
      throw new HttpException(NOT_FOUND, error);
    } else {
      throw new CompletionException(throwable.getCause());
    }
  }

  public Future<List<JsonObject>> getHoldingsByIds(List<String> holdingIds, RequestContext requestContext) {
    return getHoldingsByIdsWithoutVerification(holdingIds, requestContext)
      .map(holdings -> {
        if (holdings.size() == holdingIds.size()) {
          return holdings;
        }
        List<Parameter> parameters = holdingIds.stream()
          .filter(id -> holdings.stream()
            .noneMatch(holding -> holding.getString(ID).equals(id)))
          .map(id -> new Parameter().withValue(id).withKey("holdings"))
          .toList();
        throw new HttpException(404, PARTIALLY_RETURNED_COLLECTION, parameters);
      });
  }

  private Future<List<JsonObject>> getHoldingsByIdsWithoutVerification(List<String> holdingIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(holdingIds), MAX_IDS_FOR_GET_RQ_15)
        .map(ids -> fetchHoldingsByHoldingIds(ids, requestContext)).toList()
    )
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .toList());
  }

  private Future<List<JsonObject>> fetchHoldingsByHoldingIds(List<String> holdingIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(holdingIds);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(MAX_IDS_FOR_GET_RQ_15);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(jsonHoldings -> jsonHoldings.getJsonArray(HOLDINGS_RECORDS)
        .stream()
        .map(JsonObject.class::cast)
        .toList());
  }

  public Future<JsonObject> getHoldingById(String holdingId, boolean skipNotFoundException, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);

      return restClient.getAsJsonObject(requestEntry, skipNotFoundException, requestContext);
    }
    return Future.succeededFuture(new JsonObject());
  }

  public Future<List<JsonObject>> getHoldingRecords(String instanceId, List<String> locationIds, RequestContext requestContext) {
    List<Future<JsonObject>> futures = new ArrayList<>();
    locationIds.forEach(locationId -> futures.add(getFirstHoldingRecord(instanceId, locationId, requestContext)));
    return collectResultsOnSuccess(futures).map(holdings -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = holdings.stream().map(holding -> holding.getString(ID)).collect(Collectors.joining(","));
        logger.debug("Holding ids: {}", deletedIds);
      }
      return holdings.stream().filter(Objects::nonNull).toList();
    });
  }

  public Future<JsonObject> getFirstHoldingRecord(String instanceId, String locationId, RequestContext requestContext) {
    String query = String.format(HOLDINGS_LOOKUP_QUERY, instanceId, locationId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(holdings -> {
        if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
          return Future.succeededFuture(getFirstObjectFromResponse(holdings, HOLDINGS_RECORDS));
        }
        return Future.succeededFuture();
      });
  }

  private Future<JsonObject> createHolding(String instanceId, String locationId, RequestContext requestContext) {
    return InventoryUtils.getSourceId(configurationEntriesCache, inventoryCache, requestContext)
      .map(sourceId -> {
        JsonObject holdingsRecJson = new JsonObject();
        holdingsRecJson.put(HOLDING_INSTANCE_ID, instanceId);
        holdingsRecJson.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
        holdingsRecJson.put(HOLDING_SOURCE, sourceId);
        logger.info("createHolding:: Creating a new holding, instanceId: {}, locationId: {}", instanceId, locationId);
        return holdingsRecJson;
      })
      .compose(holdingsRecJson -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS));
        return restClient.postJsonObject(requestEntry, holdingsRecJson, requestContext);
      });
  }

  public Future<String> createHoldingAndReturnId(String instanceId, String locationId, RequestContext requestContext) {
    return createHolding(instanceId, locationId, requestContext)
      .map(holding -> holding.getString(ID));
  }

  public Future<Void> updateInstanceForHoldingRecords(List<JsonObject> holdingRecords, String instanceId, RequestContext requestContext) {
    if (isNotEmpty(holdingRecords)) {
      holdingRecords.forEach(holding -> holding.put(HOLDING_INSTANCE_ID, instanceId));
      return updateHoldingRecords(holdingRecords, requestContext);
    } else {
      return Future.succeededFuture();
    }
  }

  private Future<Void> updateHoldingRecords(List<JsonObject> holdingRecords, RequestContext requestContext) {
    return GenericCompositeFuture.join(holdingRecords.stream()
        .map(holdingRecord -> updateHolding(holdingRecord, requestContext))
        .toList())
      .mapEmpty();
  }

  private Future<Void> updateHolding(JsonObject holding, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holding.getString(ID));
    return restClient.put(requestEntry, holding, requestContext);
  }

  public Future<String> getOrCreateHoldingRecordByInstanceAndLocation(String instanceId, Location location, RequestContext requestContext) {
    if (Objects.isNull(location.getLocationId())) {
      return getHoldingById(location.getHoldingId(), true, requestContext)
        .compose(holding -> {
          if (Objects.isNull(holding)) {
            return Future.succeededFuture();
          }
          String locationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
          return getFirstHoldingRecord(instanceId, locationId, requestContext)
            .compose(jsonHolding -> {
              if (Objects.nonNull(jsonHolding)) {
                String holdingId = jsonHolding.getString(ID);
                return Future.succeededFuture(holdingId);
              }
              return createHoldingAndReturnId(instanceId, locationId, requestContext);
            });
        });
    } else {
      return getFirstHoldingRecord(instanceId, location.getLocationId(), requestContext)
        .compose(jsonHolding -> {
          if (Objects.nonNull(jsonHolding)) {
            String holdingId = jsonHolding.getString(ID);
            return Future.succeededFuture(holdingId);
          }
          return createHoldingAndReturnId(instanceId, location.getLocationId(), requestContext);
        });
    }
  }

  public Future<String> createHolding(String newInstanceId, Location location, RequestContext requestContext) {
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    logger.info("createHolding:: Going to create new holding for new instanceId: {} in tenant: {}",
      newInstanceId, tenantId);
    if (Objects.isNull(location.getLocationId())) {
      logger.info("createHolding:: Fetching permanentLocationId for holdingId: {}", location.getHoldingId());
      return getHoldingById(location.getHoldingId(), true, requestContext)
        .compose(holding -> {
          String locationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
          return createHoldingAndReturnId(newInstanceId, locationId, requestContext);
        });
    } else {
      return createHoldingAndReturnId(newInstanceId, location.getLocationId(), requestContext);
    }
  }

  public Future<Void> deleteHoldingById(String holdingId, boolean skipNotFoundException, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);
      return restClient.delete(requestEntry, skipNotFoundException, requestContext);
    }
    return Future.succeededFuture();
  }

  public Map<String, Future<List<JsonObject>>> getHoldingsByLocationTenants(PoLine poLine, RequestContext requestContext) {
    String currentTenantId = TenantTool.tenantId(requestContext.getHeaders());
    Map<String, List<String>> holdingsByTenant = poLine.getLocations()
      .stream()
      .filter(location -> location.getHoldingId() != null)
      .collect(Collectors.groupingBy(
        location -> ObjectUtils.defaultIfNull(location.getTenantId(), currentTenantId),
        Collectors.mapping(Location::getHoldingId, Collectors.toList())
      ));

    return holdingsByTenant.entrySet()
      .stream()
      .collect(Collectors.toMap(Map.Entry::getKey, entry -> getHoldings(entry.getKey(), entry.getValue(), requestContext)));
  }

  private Future<List<JsonObject>> getHoldings(String tenantId, List<String> holdingIds, RequestContext requestContext) {

    if (holdingIds.isEmpty()) {
      return Future.succeededFuture(List.of());
    }

    return getHoldingsByIds(holdingIds, RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId))
      .onFailure(failure -> {
        logger.error("Couldn't retrieve holdings", failure);
        var param = new Parameter().withKey("holdingIds").withValue(holdingIds.toString());
        var cause = new Parameter().withKey("cause").withValue(failure.getMessage());
        throw new HttpException(404, HOLDINGS_BY_ID_NOT_FOUND, List.of(param, cause));
      });
  }

  public Future<List<String>> getLocationIdsFromHoldings(List<Location> locations, RequestContext requestContext) {
    var holdingLocations = StreamUtils.filter(locations, location -> location.getHoldingId() != null);
    if (holdingLocations.isEmpty()) {
      return Future.succeededFuture(List.of());
    }
    Map<String, List<String>> holdingIdsByTenant = StreamUtils.groupBy(
      holdingLocations,
      Location::getTenantId,
      Location::getHoldingId
    );
    return HelperUtils.combineResultListsOnSuccess(
      holdingIdsByTenant.entrySet()
        .stream()
        .map(entry -> {
          var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, entry.getKey());
          return getHoldingsByIdsWithoutVerification(entry.getValue(), locationContext);
        })
        .toList()
    ).map(holdings -> StreamUtils.map(holdings, holding -> holding.getString(HOLDING_PERMANENT_LOCATION_ID)));
  }

}
