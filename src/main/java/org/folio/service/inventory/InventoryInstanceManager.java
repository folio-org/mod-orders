package org.folio.service.inventory;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.models.consortium.SharingInstance;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.SharingInstanceService;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.orders.utils.HelperUtils.isProductIdsExist;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.service.inventory.InventoryUtils.INSTANCES;
import static org.folio.service.inventory.InventoryUtils.INSTANCE_RECORDS_BY_ID_ENDPOINT;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

public class InventoryInstanceManager {
  private static final Logger logger = LogManager.getLogger(InventoryInstanceManager.class);

  public static final String ID = "id";
  public static final String SOURCE_FOLIO = "FOLIO";
  public static final String INSTANCE_SOURCE = "source";
  public static final String INSTANCE_TITLE = "title";
  public static final String INSTANCE_EDITIONS = "editions";
  public static final String INSTANCE_STATUS_ID = "statusId";
  public static final String INSTANCE_TYPE_ID = "instanceTypeId";
  public static final String INSTANCE_PUBLISHER = "publisher";
  public static final String INSTANCE_CONTRIBUTORS = "contributors";
  public static final String INSTANCE_DATE_OF_PUBLICATION = "dateOfPublication";
  public static final String INSTANCE_PUBLICATION = "publication";
  public static final String INSTANCE_IDENTIFIER_TYPE_ID = "identifierTypeId";
  public static final String INSTANCE_IDENTIFIERS = "identifiers";
  public static final String INSTANCE_IDENTIFIER_TYPE_VALUE = "value";
  public static final String CONTRIBUTOR_NAME = "name";
  public static final String CONTRIBUTOR_NAME_TYPE_ID = "contributorNameTypeId";
  public static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  public static final String INSTANCE_STATUSES = "instanceStatuses";
  public static final String INSTANCE_TYPES = "instanceTypes";

  private final RestClient restClient;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final InventoryCache inventoryCache;
  private final InventoryService inventoryService;
  private final SharingInstanceService sharingInstanceService;
  private final ConsortiumConfigurationService consortiumConfigurationService;

  public InventoryInstanceManager(RestClient restClient,
                                  ConfigurationEntriesCache configurationEntriesCache,
                                  InventoryCache inventoryCache,
                                  InventoryService inventoryService,
                                  SharingInstanceService sharingInstanceService,
                                  ConsortiumConfigurationService consortiumConfigurationService) {
    this.restClient = restClient;
    this.configurationEntriesCache = configurationEntriesCache;
    this.inventoryCache = inventoryCache;
    this.inventoryService = inventoryService;
    this.sharingInstanceService = sharingInstanceService;
    this.consortiumConfigurationService = consortiumConfigurationService;
  }

  Future<JsonObject> searchInstancesByProducts(List<ProductId> productIds, RequestContext requestContext) {
    String query = productIds.stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = inventoryService.buildInventoryLookupEndpoint(INSTANCES, encodeQuery(query));
    return restClient.getAsJsonObject(endpoint, false, requestContext);
  }

  JsonObject buildInstanceRecordJsonObject(Title title, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, title.getTitle());

    if (title.getEdition() != null) {
      instance.put(INSTANCE_EDITIONS, new JsonArray(singletonList(title.getEdition())));
    }
    instance.put(INSTANCE_STATUS_ID, lookupObj.getString(INSTANCE_STATUSES));
    instance.put(INSTANCE_TYPE_ID, lookupObj.getString(INSTANCE_TYPES));

    if (title.getPublisher() != null || title.getPublishedDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put(INSTANCE_PUBLISHER, title.getPublisher());
      publication.put(INSTANCE_DATE_OF_PUBLICATION, title.getPublishedDate());
      instance.put(INSTANCE_PUBLICATION, new JsonArray(singletonList(publication)));
    }

    List<Contributor> titleContributors = title.getContributors();
    if (isNotEmpty(titleContributors)) {
      List<JsonObject> contributors = titleContributors.stream().map(compPolContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, compPolContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, compPolContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    List<ProductId> productIds = title.getProductIds();
    if (CollectionUtils.isNotEmpty(productIds)) {
      List<JsonObject> identifiers =
        productIds.stream()
          .map(pId -> {
            JsonObject identifier = new JsonObject();
            identifier.put(INSTANCE_IDENTIFIER_TYPE_ID, pId.getProductIdType());
            identifier.put(INSTANCE_IDENTIFIER_TYPE_VALUE, pId.getProductId());
            return identifier;
          })
          .collect(toList());
      instance.put(INSTANCE_IDENTIFIERS, new JsonArray(identifiers));
    }
    return instance;
  }

  public Future<JsonObject> getInstanceById(String instanceId, boolean skipNotFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(instanceId);
    return restClient.getAsJsonObject(requestEntry, skipNotFoundException, requestContext);
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param compPOL PO line to retrieve Instance Record Id for
   * @return future with Instance Id
   */
  public Future<String> getInstanceRecord(CompositePoLine compPOL, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!isProductIdsExist(compPOL) || isInstanceMatchingDisabled) {
      return createInstanceRecord(compPOL, requestContext);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return Future.succeededFuture(extractId(getFirstObjectFromResponse(instances, INSTANCES)));
        }
        return createInstanceRecord(compPOL, requestContext);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param compPOL PO line to create Instance Record for
   * @return id of newly created Instance Record
   */
  private Future<String> createInstanceRecord(CompositePoLine compPOL, RequestContext requestContext) {
    logger.debug("Start processing instance record");
    JsonObject lookupObj = new JsonObject();
    Future<Void> instanceTypeFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> statusFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(compPOL.getContributors(), requestContext);

    return CompositeFuture.join(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .map(v -> buildInstanceRecordJsonObject(compPOL, lookupObj))
      .compose(instanceRecJson -> {
        logger.debug("Instance record to save : {}", instanceRecJson);
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
        return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
      });
  }

  private JsonObject buildInstanceRecordJsonObject(CompositePoLine compPOL, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, compPOL.getTitleOrPackage());

    if (compPOL.getEdition() != null) {
      instance.put(INSTANCE_EDITIONS, new JsonArray(singletonList(compPOL.getEdition())));
    }
    instance.put(INSTANCE_STATUS_ID, lookupObj.getString(INSTANCE_STATUSES));
    instance.put(INSTANCE_TYPE_ID, lookupObj.getString(INSTANCE_TYPES));

    if (compPOL.getPublisher() != null || compPOL.getPublicationDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put(INSTANCE_PUBLISHER, compPOL.getPublisher());
      publication.put(INSTANCE_DATE_OF_PUBLICATION, compPOL.getPublicationDate());
      instance.put(INSTANCE_PUBLICATION, new JsonArray(singletonList(publication)));
    }

    if (isNotEmpty(compPOL.getContributors())) {
      List<JsonObject> contributors = compPOL.getContributors().stream().map(compPolContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, compPolContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, compPolContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    if (isProductIdsExist(compPOL)) {
      List<JsonObject> identifiers =
        compPOL.getDetails()
          .getProductIds()
          .stream()
          .map(pId -> {
            JsonObject identifier = new JsonObject();
            identifier.put(INSTANCE_IDENTIFIER_TYPE_ID, pId.getProductIdType());
            identifier.put(INSTANCE_IDENTIFIER_TYPE_VALUE, pId.getProductId());
            return identifier;
          })
          .collect(toList());
      instance.put(INSTANCE_IDENTIFIERS, new JsonArray(identifiers));
    }
    return instance;
  }

  private String buildProductIdQuery(ProductId productId) {
    return String.format("(identifiers =/@identifierTypeId=\"%s\"/@value \"%s\")",
      productId.getProductIdType(), productId.getProductId());
  }

  public Future<Void> verifyContributorNameTypesExist(List<Contributor> contributors, RequestContext requestContext) {
    List<String> ids = contributors.stream()
      .map(contributor -> contributor.getContributorNameTypeId().toLowerCase())
      .distinct()
      .collect(toList());

    return getContributorNameTypes(ids, requestContext)
      .map(contributorNameTypes -> {
        List<String> retrievedIds = contributorNameTypes.stream()
          .map(o -> o.getString(ID).toLowerCase())
          .toList();
        if (retrievedIds.size() != ids.size()) {
          ids.removeAll(retrievedIds);
          throw new HttpException(500, InventoryUtils.buildErrorWithParameter(String.join(", ", ids), MISSING_CONTRIBUTOR_NAME_TYPE));
        }
        return null;
      });
  }

  private Future<List<JsonObject>> getContributorNameTypes(List<String> ids, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(ids, MAX_IDS_FOR_GET_RQ_15)
      .map(idChunk -> getContributorNameTypeByIds(idChunk, requestContext))
      .toList())
      .map(lists -> StreamEx.of(lists).toFlatList(contributorNameTypes -> contributorNameTypes));
  }

  private Future<List<JsonObject>> getContributorNameTypeByIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(CONTRIBUTOR_NAME_TYPES))
      .withQuery(query).withOffset(0).withLimit(ids.size());
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(entries -> entries.getJsonArray(CONTRIBUTOR_NAME_TYPES).stream()
        .map(JsonObject::mapFrom)
        .collect(Collectors.toList())
      )
      .recover(e -> {
        logger.error("The issue happened getting contributor name types", e);
        throw new CompletionException(e.getCause());
      });
  }

  public Future<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
    return getOrCreateInstanceRecord(title, false, requestContext);
  }

  public Future<String> getOrCreateInstanceRecord(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(title.getProductIds()) || isInstanceMatchingDisabled) {
      return createInstanceRecord(title, requestContext);
    }

    return searchInstancesByProducts(title.getProductIds(), requestContext)
      .compose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          String instanceId = getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
          return Future.succeededFuture(instanceId);
        }
        return createInstanceRecord(title, requestContext);
      });
  }

  public Future<String> createInstanceRecord(Title title, RequestContext requestContext) {
    JsonObject lookupObj = new JsonObject();
    Future<Void> instanceTypeFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> statusFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(title.getContributors(), requestContext);

    return CompositeFuture.join(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .map(cf -> buildInstanceRecordJsonObject(title, lookupObj))
      .compose(instanceJson -> createInstance(instanceJson, requestContext));
  }

  private Future<String> createInstance(JsonObject instanceRecJson, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
    return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
  }

  public Future<CompositePoLine> openOrderHandleInstance(CompositePoLine compPOL, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (compPOL.getInstanceId() != null) {
          return consortiumConfiguration.isEmpty() ? Future.succeededFuture(compPOL.getInstanceId())
            : shareInstanceAmongTenantsIfNeeded(compPOL.getInstanceId(), consortiumConfiguration.get(), compPOL.getLocations(), requestContext);
        } else {
          return getInstanceRecord(compPOL, isInstanceMatchingDisabled, requestContext)
            .compose(instanceId -> consortiumConfiguration.isEmpty() ? Future.succeededFuture(instanceId)
              : shareInstanceAmongTenantsIfNeeded(instanceId, consortiumConfiguration.get(), compPOL.getLocations(), requestContext));
        }
      }).map(compPOL::withInstanceId);
  }

  public Future<SharingInstance> createShadowInstanceIfNeeded(String instanceId, String targetTenantId, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          logger.debug("createShadowInstanceIfNeeded:: Skipping creating shadow instance for non ECS mode.");
          return Future.succeededFuture();
        }
        if (StringUtils.isBlank(instanceId)) {
          logger.info("createShadowInstanceIfNeeded:: Provided instanceId is blank, skip creating of shadow instance.");
          return Future.succeededFuture();
        }
        return getInstanceById(instanceId, true, requestContext)
          .compose(instance -> {
            if (Objects.nonNull(instance) && !instance.isEmpty()) {
              logger.info("createShadowInstanceIfNeeded:: Shadow instance already exists, skipping...");
              return Future.succeededFuture();
            }
            logger.info("createShadowInstanceIfNeeded:: Creating shadow instance with instanceId: {}", instanceId);
            return sharingInstanceService.createShadowInstance(instanceId, targetTenantId, consortiumConfiguration.get(), requestContext);
          });
      });
  }

  private Future<String> shareInstanceAmongTenantsIfNeeded(String instanceId, ConsortiumConfiguration consortiumConfiguration,
                                                           List<Location> locations, RequestContext requestContext) {
    return findTenantsWithUnsharedInstance(instanceId, locations, requestContext)
      .map(tenantIds -> tenantIds.stream().map(targetTenantId -> sharingInstanceService.createShadowInstance(instanceId,
        targetTenantId, consortiumConfiguration, requestContext)).toList())
      .compose(HelperUtils::collectResultsOnSuccess)
      .map(sharingInstances -> instanceId);
  }

  private Future<List<String>> findTenantsWithUnsharedInstance(String instanceId, List<Location> locations, RequestContext requestContext) {
    List<Future<Optional<String>>> tenantIdFutures = locations.stream()
      .map(Location::getTenantId)
      .distinct()
      .filter(Objects::nonNull)
      .map(tenantId -> RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId))
      .map(clonedRequestContext -> getInstanceById(instanceId, false, clonedRequestContext)
        .map(instance -> Optional.<String>empty())
        .recover(throwable -> throwable instanceof HttpException httpException && httpException.getCode() == 404 ?
          Future.succeededFuture(Optional.of(TenantTool.tenantId(clonedRequestContext.getHeaders()))) : Future.failedFuture(throwable)))
      .toList();
    return collectResultsOnSuccess(tenantIdFutures)
      .map(tenantIds -> tenantIds.stream().flatMap(Optional::stream).toList());
  }

}
