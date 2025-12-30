package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.DecodeException;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import org.apache.http.HttpStatus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.AcquisitionMethod;
import org.folio.AcquisitionsUnit;
import org.folio.ContributorNameType;
import org.folio.Contributornametypes;
import org.folio.ExpenseClass;
import org.folio.ExpenseClassCollection;
import org.folio.Fund;
import org.folio.FundCollection;
import org.folio.IdentifierType;
import org.folio.Identifiertypes;
import org.folio.Location;
import org.folio.Locations;
import org.folio.Materialtypes;
import org.folio.Mtype;
import org.folio.Organization;
import org.folio.OrganizationCollection;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.rest.util.RestUtil;
import org.folio.service.AcquisitionMethodsService;
import org.folio.service.AcquisitionsUnitsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import static java.lang.String.format;
import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.QueryUtils.encodeQuery;
import static org.folio.rest.RestConstants.ID;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

/**
 * The class responsible for caching {@link MappingParameters}
 */
@Component
public class MappingParametersCache {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String ORGANIZATIONS = "/organizations/organizations";
  private static final String SORT_BY_ID_QUERY = "(cql.allRecords=1) sortBy id";
  private static final String LOCATIONS_RESPONSE_PARAM = "locations";
  private static final String MATERIALS_TYPES_RESPONSE_PARAM = "mtypes";
  private static final String CONTRIBUTOR_NAME_TYPES_RESPONSE_PARAM = "contributorNameTypes";
  private static final String FUNDS_RESPONSE_PARAM = "funds";
  private static final String EXPENSE_CLASSES_RESPONSE_PARAM = "expenseClasses";
  private static final String IDENTIFIER_TYPES_RESPONSE_PARAM = "identifierTypes";
  private static final String TENANT_CONFIGURATION_ADDRESSES_URL = "/configurations/entries?query="
    + URLEncoder.encode("(module==TENANT and configName==tenant.addresses)", StandardCharsets.UTF_8);
  private static final String CONFIGS_VALUE_RESPONSE = "configs";
  private static final String VALUE_RESPONSE = "value";
  public static final String ERROR_LOADING_CACHE_MESSAGE = "Error loading cache for mapping parameter: '%s', tenantId: '%s', status code: %s, response message: %s";

  private final int settingsLimit;

  private final AsyncCache<String, MappingParameters> cache;
  private final RestClient restClient;
  private final AcquisitionsUnitsService acquisitionsUnitsService;
  private final AcquisitionMethodsService acquisitionMethodsService;

  @Autowired
  public MappingParametersCache(Vertx vertx, RestClient restClient,
    AcquisitionsUnitsService acquisitionsUnitsService,
    AcquisitionMethodsService acquisitionMethodsService,
    @Value("${orders.cache.mapping.parameters.settings.limit:5000}") int settingsLimit,
    @Value("${orders.cache.mapping.parameters.expiration.seconds:3600}") long cacheExpirationTime) {
    LOGGER.info("MappingParametersCache:: settings limit: '{}', expiration time: '{}'",
      settingsLimit, cacheExpirationTime);
    cache = buildAsyncCache(vertx, cacheExpirationTime);
    this.restClient = restClient;
    this.acquisitionsUnitsService = acquisitionsUnitsService;
    this.acquisitionMethodsService = acquisitionMethodsService;
    this.settingsLimit = settingsLimit;
  }

  /**
   * Retrieves {@link MappingParameters} from cache by tenantId
   *
   * @param params {@link OkapiConnectionParams} connection params
   * @return CompletableFuture with {@link MappingParameters} object
   */
  public Future<MappingParameters> get(OkapiConnectionParams params) {
    try {
      return Future.fromCompletionStage(cache.get(params.getTenantId(), (key, executor) ->
        loadMappingParameters(params).toCompletionStage().toCompletableFuture()));
    } catch (Exception e) {
      LOGGER.warn("get:: Error loading organizations from cache, tenantId: '{}'", params.getTenantId(), e);
      return Future.failedFuture(e);
    }
  }

  /**
   * Generates {@link MappingParameters}
   *
   * @param params {@link OkapiConnectionParams} connection params
   * @return CompletableFuture with {@link MappingParameters} object
   */
  private Future<MappingParameters> loadMappingParameters(OkapiConnectionParams params) {
    String tenantId = params.getTenantId();
    LOGGER.debug("loadMappingParameters:: Trying to load mapping parameters '{}' for cache, okapi url: {}, tenantId: {}",
      tenantId, params.getOkapiUrl(), params.getTenantId());

    var locationsFuture = getLocations(params);
    var materialTypesFuture = getMaterialTypes(params);
    var contributorNameTypesFuture = getContributorNameTypes(params);
    var organizationsFuture = getOrganizations(params);
    var fundsFuture = getFunds(params);
    var expenseClassesFuture = getExpenseClasses(params);
    var identifierTypesFuture = getIdentifierTypes(params);
    var acquisitionsUnitsFuture = getAcquisitionsUnits(params);
    var acquisitionsMethodsFuture = getAcquisitionMethods(params);
    var tenantConfigurationAddressesFuture = getTenantConfigurationAddresses(params);

    return Future.join(Arrays.asList(locationsFuture, materialTypesFuture, contributorNameTypesFuture,
        organizationsFuture, fundsFuture, identifierTypesFuture, expenseClassesFuture, acquisitionsUnitsFuture,
        acquisitionsMethodsFuture, tenantConfigurationAddressesFuture))
      .map(v -> new MappingParameters()
        .withLocations(locationsFuture.result())
        .withMaterialTypes(materialTypesFuture.result())
        .withContributorNameTypes(contributorNameTypesFuture.result())
        .withOrganizations(organizationsFuture.result())
        .withFunds(fundsFuture.result())
        .withExpenseClasses(expenseClassesFuture.result())
        .withIdentifierTypes(identifierTypesFuture.result())
        .withAcquisitionsUnits(acquisitionsUnitsFuture.result())
        .withAcquisitionMethods(acquisitionsMethodsFuture.result())
        .withTenantConfigurationAddresses(tenantConfigurationAddressesFuture.result()));
  }

  private Future<List<Organization>> getOrganizations(OkapiConnectionParams params) {
    String tenantId = params.getTenantId();
    LOGGER.debug("getOrganizations:: Trying to load organizations '{}' for cache, okapi url: {}, tenantId: {}",
      tenantId, params.getOkapiUrl(), params.getTenantId());

    return RestUtil.doRequest(params, getOrganizationsSortedLimitPath(settingsLimit), HttpMethod.GET, null)
      .compose(httpResponse -> {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          OrganizationCollection orgCollection = Json.decodeValue(httpResponse.getJson().encode(), OrganizationCollection.class);
          LOGGER.debug("getOrganizations:: The first chunk of organizations was loaded for cache, tenantId '{}', organizations '{}', totalRecords '{}'",
            tenantId, orgCollection.getOrganizations().size(), orgCollection.getTotalRecords());
          if (orgCollection.getTotalRecords() > settingsLimit) {
            return getRemainingOrganizations(params, orgCollection);
          }

          return Future.succeededFuture(orgCollection.getOrganizations());
        } else {
          String message = format(ERROR_LOADING_CACHE_MESSAGE, "Organizations", tenantId, httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn("getOrganizations:: {}", message);
          return Future.failedFuture(new CacheLoadingException(message));
        }
      });
  }

  private Future<List<Organization>> getRemainingOrganizations(OkapiConnectionParams params,
                                                               OrganizationCollection organizationCollection) {
    return collectResultsOnSuccess(getOrganizationCollectionFutures(params.getHeaders(), organizationCollection.getTotalRecords()))
      .map(orgCollections -> orgCollections.stream()
        .flatMap(orgCollection -> orgCollection.getOrganizations().stream())
        .toList())
      .compose(organizations -> {
        LOGGER.debug("getRemainingOrganizations:: Organizations were loaded for cache, tenantId '{}', organizations '{}'",
          params.getTenantId(), organizations.size());
        organizationCollection.getOrganizations().addAll(organizations);
        return Future.succeededFuture(organizationCollection.getOrganizations());
      });
  }

  private List<Future<OrganizationCollection>> getOrganizationCollectionFutures(Map<String, String> headers, Integer totalRecords) {
    final int maxChunkSize = totalRecords / settingsLimit;
    int offset = settingsLimit;
    List<Future<OrganizationCollection>> organizationCollectionFutures = new ArrayList<>(maxChunkSize);
    RequestEntry requestEntry = new RequestEntry(ORGANIZATIONS).withLimit(settingsLimit).withQuery(SORT_BY_ID_QUERY);
    for (int i = 0; i < maxChunkSize; i++) {
      Future<OrganizationCollection> future = restClient.get(requestEntry.withOffset(offset), OrganizationCollection.class,
        new RequestContext(Vertx.currentContext(), headers));
      organizationCollectionFutures.add(future);
      offset += settingsLimit;
    }
    return organizationCollectionFutures;
  }

  private Future<List<Mtype>> getMaterialTypes(OkapiConnectionParams params) {
    String materialTypesUrl = "/material-types?limit=" + settingsLimit;
    return loadData(params, materialTypesUrl, MATERIALS_TYPES_RESPONSE_PARAM,
      response -> response.mapTo(Materialtypes.class).getMtypes(), "MaterialTypes");
  }

  private Future<List<Location>> getLocations(OkapiConnectionParams params) {
    String locationsUrl = "/locations?limit=" + settingsLimit;
    return loadData(params, locationsUrl, LOCATIONS_RESPONSE_PARAM,
      response -> response.mapTo(Locations.class).getLocations(), "Locations");
  }

  private Future<List<ContributorNameType>> getContributorNameTypes(OkapiConnectionParams params) {
    String contributorNameTypesUrl = "/contributor-name-types?limit=" + settingsLimit;
    return loadData(params, contributorNameTypesUrl, CONTRIBUTOR_NAME_TYPES_RESPONSE_PARAM,
      response -> response.mapTo(Contributornametypes.class).getContributorNameTypes(), "ContributorNameTypes");
  }

  private Future<List<Fund>> getFunds(OkapiConnectionParams params) {
    String fundsUrl = "/finance/funds?limit=" + settingsLimit;
    return loadData(params, fundsUrl, FUNDS_RESPONSE_PARAM,
      response -> response.mapTo(FundCollection.class).getFunds(), "Funds");
  }

  private Future<List<ExpenseClass>> getExpenseClasses(OkapiConnectionParams params) {
    String fundsUrl = "/finance/expense-classes?limit=" + settingsLimit;
    return loadData(params, fundsUrl, EXPENSE_CLASSES_RESPONSE_PARAM,
      response -> response.mapTo(ExpenseClassCollection.class).getExpenseClasses(), "ExpenseClasses");
  }

  private Future<List<IdentifierType>> getIdentifierTypes(OkapiConnectionParams params) {
    String identifierTypesUrl = "/identifier-types?limit=" + settingsLimit;
    return loadData(params, identifierTypesUrl, IDENTIFIER_TYPES_RESPONSE_PARAM,
      response -> response.mapTo(Identifiertypes.class).getIdentifierTypes(), "IdentifierTypes");
  }

  private Future<List<AcquisitionsUnit>> getAcquisitionsUnits(OkapiConnectionParams params) {
    return acquisitionsUnitsService.getAcquisitionsUnits(null, 0, settingsLimit, new RequestContext(Vertx.currentContext(), params.getHeaders()))
      .map(unitsCollection -> unitsCollection.getAcquisitionsUnits()
        .stream().map(unit -> mapTo(unit, AcquisitionsUnit.class)).toList());
  }

  private Future<List<AcquisitionMethod>> getAcquisitionMethods(OkapiConnectionParams params) {
    return acquisitionMethodsService.getAcquisitionMethods(settingsLimit, 0, null, new RequestContext(Vertx.currentContext(), params.getHeaders()))
      .map(methodsCollection -> methodsCollection.getAcquisitionMethods()
        .stream().map(method -> mapTo(method, AcquisitionMethod.class)).toList());
  }

  private static <T> T mapTo(Object unit, Class<T> objectClass) {
    return Json.decodeValue(Json.encode(unit), objectClass);
  }

  private Future<List<String>> getTenantConfigurationAddresses(OkapiConnectionParams params) {
    return RestUtil.doRequest(params, TENANT_CONFIGURATION_ADDRESSES_URL, HttpMethod.GET, null).compose(httpResponse -> {
      if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
        JsonObject response = httpResponse.getJson();
        if (ifConfigResponseIsValid(response)) {
          List<String> addresses = response.getJsonArray(CONFIGS_VALUE_RESPONSE).stream()
            .map(config -> {
              JsonObject configJson = (JsonObject) config;
              String configValue = configJson.getString(VALUE_RESPONSE);
              if (configValue != null) {
                try {
                  return new JsonObject(configValue).put(ID, configJson.getString(ID)).toString();
                } catch (DecodeException decodeException) {
                  LOGGER.warn("getTenantConfigurationAddresses:: Cannot parse json configuration value {}", configValue);
                  return null;
                }
              }
              return null;
            })
            .filter(Objects::nonNull)
            .toList();
          return Future.succeededFuture(addresses);
        }
        return Future.succeededFuture(Collections.emptyList());
      } else {
        String message = format(ERROR_LOADING_CACHE_MESSAGE, "TenantConfigurationAddresses", params.getTenantId(),
          httpResponse.getResponse().statusCode(), httpResponse.getBody());
        LOGGER.warn("getTenantConfigurationAddresses:: {}", message);
        return Future.failedFuture(new CacheLoadingException(message));
      }
    });
  }

  private <T> Future<List<T>> loadData(OkapiConnectionParams params, String requestUrl, String dataCollectionField,
                                       Function<JsonObject, List<T>> dataExtractor, String parameterName) {
    return RestUtil.doRequest(params, requestUrl, HttpMethod.GET, null).compose(httpResponse -> {
      try {
        if (httpResponse.getResponse().statusCode() == HttpStatus.SC_OK) {
          JsonObject response = httpResponse.getJson();
          if (response != null && response.containsKey(dataCollectionField)) {
            return Future.succeededFuture(dataExtractor.apply(response));
          }
          return Future.succeededFuture(Collections.emptyList());
        } else {
          String message = format(ERROR_LOADING_CACHE_MESSAGE, parameterName, params.getTenantId(),
            httpResponse.getResponse().statusCode(), httpResponse.getBody());
          LOGGER.warn("loadData:: {}", message);
          return Future.failedFuture(new CacheLoadingException(message));
        }
      } catch (Exception e) {
        LOGGER.warn("loadData:: Failed to load {}", dataCollectionField, e);
        return Future.failedFuture(new CacheLoadingException(e.getMessage()));
      }
    });
  }

  private boolean ifConfigResponseIsValid(JsonObject response) {
    return response != null && response.containsKey(CONFIGS_VALUE_RESPONSE)
      && response.getJsonArray(CONFIGS_VALUE_RESPONSE) != null
      && !response.getJsonArray(CONFIGS_VALUE_RESPONSE).isEmpty();
  }

  private String getOrganizationsSortedLimitPath(int limit) {
    return format("%s?limit=%d&query=", ORGANIZATIONS, limit) + encodeQuery(SORT_BY_ID_QUERY);
  }
}
