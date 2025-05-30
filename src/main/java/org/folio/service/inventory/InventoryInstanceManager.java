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
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.SharingInstanceService;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.orders.utils.HelperUtils.isProductIdsExist;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
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
  public static final String INSTANCE_DISCOVERY_SUPPRESS = "discoverySuppress";

  private final RestClient restClient;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final InventoryCache inventoryCache;
  private final SharingInstanceService sharingInstanceService;
  private final ConsortiumConfigurationService consortiumConfigurationService;

  public InventoryInstanceManager(RestClient restClient,
                                  ConfigurationEntriesCache configurationEntriesCache,
                                  InventoryCache inventoryCache,
                                  SharingInstanceService sharingInstanceService,
                                  ConsortiumConfigurationService consortiumConfigurationService) {
    this.restClient = restClient;
    this.configurationEntriesCache = configurationEntriesCache;
    this.inventoryCache = inventoryCache;
    this.sharingInstanceService = sharingInstanceService;
    this.consortiumConfigurationService = consortiumConfigurationService;
  }

  Future<JsonObject> searchInstancesByProducts(List<ProductId> productIds, RequestContext requestContext) {
    String query = productIds.stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES))
      .withQuery(query).withOffset(0).withLimit(1);
    return restClient.getAsJsonObject(requestEntry, requestContext);
  }

  JsonObject buildInstanceRecordJsonObject(Title title, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, title.getTitle());
    instance.put(INSTANCE_DISCOVERY_SUPPRESS, false);

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
      List<JsonObject> contributors = titleContributors.stream().map(polContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, polContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, polContributor.getContributor());
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

  Future<JsonObject> getInstanceById(String instanceId, boolean skipNotFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(instanceId);
    return restClient.getAsJsonObject(requestEntry, skipNotFoundException, requestContext);
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param poLine PO line to retrieve Instance Record Id for
   * @return future with Instance Id
   */
  private Future<String> getInstanceRecord(PoLine poLine, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (poLine.getInstanceId() != null) {
      return Future.succeededFuture(poLine.getInstanceId());
    }
    // proceed with new Instance Record creation if no productId is provided
    if (!isProductIdsExist(poLine) || isInstanceMatchingDisabled) {
      return createInstanceRecord(poLine, requestContext);
    }

    String query = poLine.getDetails().getProductIds().stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return Future.succeededFuture(extractId(getFirstObjectFromResponse(instances, INSTANCES)));
        }
        return createInstanceRecord(poLine, requestContext);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param poLine PO line to create Instance Record for
   * @return id of newly created Instance Record
   */
  private Future<String> createInstanceRecord(PoLine poLine, RequestContext requestContext) {
    logger.debug("Start processing instance record");
    JsonObject lookupObj = new JsonObject();
    Future<Void> instanceTypeFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> statusFuture = InventoryUtils.getEntryId(configurationEntriesCache, inventoryCache, INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(poLine.getContributors(), requestContext);

    return CompositeFuture.join(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .map(v -> buildInstanceRecordJsonObject(poLine, lookupObj))
      .compose(instanceRecJson -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
        return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
      });
  }

  private JsonObject buildInstanceRecordJsonObject(PoLine poLine, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, poLine.getTitleOrPackage());
    instance.put(INSTANCE_DISCOVERY_SUPPRESS, false);

    if (poLine.getEdition() != null) {
      instance.put(INSTANCE_EDITIONS, new JsonArray(singletonList(poLine.getEdition())));
    }
    instance.put(INSTANCE_STATUS_ID, lookupObj.getString(INSTANCE_STATUSES));
    instance.put(INSTANCE_TYPE_ID, lookupObj.getString(INSTANCE_TYPES));

    if (poLine.getPublisher() != null || poLine.getPublicationDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put(INSTANCE_PUBLISHER, poLine.getPublisher());
      publication.put(INSTANCE_DATE_OF_PUBLICATION, poLine.getPublicationDate());
      instance.put(INSTANCE_PUBLICATION, new JsonArray(singletonList(publication)));
    }

    if (isNotEmpty(poLine.getContributors())) {
      List<JsonObject> contributors = poLine.getContributors().stream().map(polContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, polContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, polContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    if (isProductIdsExist(poLine)) {
      List<JsonObject> identifiers =
        poLine.getDetails()
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

    return GenericCompositeFuture.join(List.of(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture))
      .map(cf -> buildInstanceRecordJsonObject(title, lookupObj))
      .compose(instanceJson -> createInstance(instanceJson, requestContext));
  }

  private Future<String> createInstance(JsonObject instanceRecJson, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
    return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
  }

  public Future<PoLine> openOrderHandleInstance(PoLine poLine, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(configuration ->
        getInstanceRecord(poLine, isInstanceMatchingDisabled, requestContext)
          .compose(instanceId -> configuration
            .map(consortiumConfiguration -> shareInstanceAmongTenantsIfNeeded(instanceId, consortiumConfiguration, poLine.getLocations(), requestContext))
            .orElse(Future.succeededFuture(instanceId))))
      .map(poLine::withInstanceId);
  }

  private Future<String> shareInstanceAmongTenantsIfNeeded(String instanceId, ConsortiumConfiguration consortiumConfiguration,
                                                           List<Location> locations, RequestContext requestContext) {
    return getTenantIdsWithoutShadowInstances(instanceId, consortiumConfiguration, locations, requestContext)
      .map(tenantIds -> tenantIds.stream()
        .map(tenantId -> createShadowInstanceIfNeeded(instanceId, consortiumConfiguration, createContextWithNewTenantId(requestContext, tenantId)))
        .toList())
      .compose(HelperUtils::collectResultsOnSuccess)
      .map(sharingInstances -> instanceId);
  }

  private Future<Collection<String>> getTenantIdsWithoutShadowInstances(String instanceId, ConsortiumConfiguration consortiumConfiguration,
                                                                        List<Location> locations, RequestContext requestContext) {
    return sharingInstanceService.getSharingInstances(instanceId, consortiumConfiguration, requestContext)
      .map(instancesCollection -> {
        String centralTenantId = consortiumConfiguration.centralTenantId();
        List<String> tenantIdsWithSharingInstances = instancesCollection.getSharingInstances().stream()
          .flatMap(sharing ->
            Stream.of(sharing.targetTenantId(), sharing.sourceTenantId()))
          .filter(tenantId -> !tenantId.equals(centralTenantId))
          .distinct()
          .toList();
        List<String> locationTenantIds = locations.stream()
          .map(Location::getTenantId)
          .filter(StringUtils::isNotBlank)
          .filter(tenantId -> !tenantId.equals(centralTenantId))
          .distinct()
          .toList();
        Collection<String> tenantIdsToShare = CollectionUtils.subtract(locationTenantIds, tenantIdsWithSharingInstances);
        logger.info("List of tenants where shadow instances should be created: {} for instanceId: {}", tenantIdsToShare, instanceId);
        return tenantIdsToShare;
      });
  }

  public Future<SharingInstance> createShadowInstanceIfNeeded(String instanceId, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> createShadowInstanceIfNeeded(instanceId, consortiumConfiguration.orElse(null), requestContext));
  }

  private Future<SharingInstance> createShadowInstanceIfNeeded(String instanceId, ConsortiumConfiguration consortiumConfiguration, RequestContext requestContext) {
    if (consortiumConfiguration == null) {
      logger.debug("createShadowInstanceIfNeeded:: Skipping creating shadow instance for non ECS mode.");
      return Future.succeededFuture();
    }
    if (StringUtils.isBlank(instanceId)) {
      logger.info("createShadowInstanceIfNeeded:: Provided instanceId is blank, skip creating of shadow instance.");
      return Future.succeededFuture();
    }
    var targetTenant = TenantTool.tenantId(requestContext.getHeaders());
    logger.info("createShadowInstanceIfNeeded:: Getting instance: {} from tenant: {}", instanceId, targetTenant);
    return getInstanceById(instanceId, true, requestContext)
      .compose(instance -> {
        if (Objects.nonNull(instance) && !instance.isEmpty()) {
          logger.info("createShadowInstanceIfNeeded:: Shadow instance {} already exists in tenant: {}, skipping...", instanceId, targetTenant);
          return Future.succeededFuture();
        }
        logger.info("createShadowInstanceIfNeeded:: Creating shadow instance with instanceId: {} in tenant: {}", instanceId, targetTenant);
        return sharingInstanceService.createShadowInstance(instanceId, consortiumConfiguration, requestContext);
      });
  }

}
