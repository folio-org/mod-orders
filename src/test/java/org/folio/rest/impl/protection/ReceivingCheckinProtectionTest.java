package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ORDERS_CHECKIN_ENDPOINT;
import static org.folio.TestConstants.ORDERS_RECEIVING_ENDPOINT;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getRandomId;
import static org.folio.TestUtils.getTitle;
import static org.folio.TestUtils.getToBeCheckedIn;
import static org.folio.TestUtils.getToBeReceived;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.Every.everyItem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import io.vertx.core.json.JsonObject;


public class ReceivingCheckinProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LogManager.getLogger();


  private static final String ORDER_WITH_PROTECTED_UNITS_ID = getRandomId();
  private static final String ORDER_WITH_NOT_PROTECTED_UNITS_ID = getRandomId();

  private static final String RANDOM_PO_LINE_ID_1 = getRandomId();
  private static final String RANDOM_PO_LINE_ID_2 = "84fedaa4-ae4d-4d9b-9a93-7cf9058c67e5";
  private static final String RANDOM_TITLE_ID_2 = "bc763ccc-6f2f-4868-9349-2e8066bebe46";
  private static final String RANDOM_PIECE_ID_2 = "f3316376-2ed8-4285-9ec3-6a1169083b8f";
  private static final String PROCESSED_SUCCESSFULLY = "processedSuccessfully";
  private static final String PROCESSED_WITH_ERROR = "processedWithError";
  private static final String EXPECTED_FLOW_PO_LINE_ID = "9b68e7c2-8818-4387-a405-6567232c1c6f";
  private static final String EXPECTED_FLOW_PIECE_ID_1 = "5e317dc2-deeb-4429-b2a1-91e5cd0fd5f7";
  private static final String EXPECTED_FLOW_PIECE_ID_2 = "71d9322b-5cdd-45d8-ad45-c7f3044802e7";

  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  // After changing the check for acq-units from orders to titles, we now use the TitlesService#getTitlesByPieceIds method,
  // which calls AcquisitionsUnitsService#buildAcqUnitsCqlExprToSearchRecords. As a result, our search for acq-units has increased.
  private enum Entities {
    RECEIVING(ORDERS_RECEIVING_ENDPOINT) {
      @Override
      public String getJsonRequest(List<String> units) {
        return JsonObject.mapFrom(getReceivingCollection(units)).encode();
      }
    },
    CHECK_IN(ORDERS_CHECKIN_ENDPOINT) {
      @Override
      public String getJsonRequest(List<String> units) {
        return JsonObject.mapFrom(getCheckInCollection(units)).encode();
      }
    };

    Entities(String endpoint) {
      this.endpoint = endpoint;
    }

    private String endpoint;

    public String getEndpoint() {
      return endpoint;
    }

    public abstract String getJsonRequest(List<String> units);
  }

  @ParameterizedTest
  @EnumSource(value = Entities.class)
  void testFlowWithNonExistedUnits(Entities entity) {
    logger.info("=== Test check-in/receiving flow - non-existing units ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(NON_EXISTENT_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyRestrictedCase(results);
    validateNumberOfRequests(2, 1);
  }

  @ParameterizedTest
  @EnumSource(value = Entities.class)
  void testFlowWithAllowedUnits(Entities entity) {
    logger.info("=== Test check-in/receiving flow - not-protecting units ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(NOT_PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyAllowedCase(results);
    validateNumberOfRequests(3, 2);
  }

  @ParameterizedTest
  @EnumSource(value = Entities.class)
  void testFlowWithRestrictedUnitsAndAllowedUser(Entities entity) {
    logger.info("=== Test check-in/receiving flow - protecting units and allowed user ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyAllowedCase(results);
    validateNumberOfRequests(3, 3);
  }

  @ParameterizedTest
  @EnumSource(value = Entities.class)
  void testCheckInWithProtectedUnitsAndForbiddenUser(Entities entity) {
    logger.info("=== Test check-in/receiving flow - protecting units and forbidden user ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyRestrictedCase(results);
    validateNumberOfRequests(2, 2);
  }

  @Test
  void testReceivingCompositeFlow() {
    CompositePoLine expectedFlowPoLine = getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(EXPECTED_FLOW_PO_LINE_ID);
    CompositePoLine randomPoLine1 = getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_1);
    CompositePoLine randomPoLine2 = getMinimalContentCompositePoLine(ORDER_WITH_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_2);
    List.of(expectedFlowPoLine, randomPoLine1, randomPoLine2).forEach(line -> addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line)));

    Title expectedFlowTitle = getTitle(expectedFlowPoLine).withAcqUnitIds(NOT_PROTECTED_UNITS);
    Title randomTitle1 = getTitle(randomPoLine1).withAcqUnitIds(NOT_PROTECTED_UNITS);
    Title randomTitle2 = getTitle(randomPoLine2).withId(RANDOM_TITLE_ID_2).withAcqUnitIds(PROTECTED_UNITS);
    List.of(expectedFlowTitle, randomTitle1, randomTitle2).forEach(title -> addMockEntry(TITLES, title));

    ReceivingCollection toBeReceivedRq = new ReceivingCollection();

    List<ToBeReceived> toBeReceivedList = new ArrayList<>();
    toBeReceivedList.add(getToBeReceived(EXPECTED_FLOW_PO_LINE_ID, EXPECTED_FLOW_PIECE_ID_1));
    toBeReceivedList.add(getToBeReceived(RANDOM_PO_LINE_ID_1, getRandomId()));
    toBeReceivedList.add(getToBeReceived(RANDOM_PO_LINE_ID_2, RANDOM_PIECE_ID_2));
    toBeReceivedList.add(getToBeReceived(EXPECTED_FLOW_PO_LINE_ID, getRandomId()));

    toBeReceivedRq.setToBeReceived(toBeReceivedList);
    toBeReceivedRq.setTotalRecords(toBeReceivedList.size());

    ReceivingResults results = verifyPostResponse(Entities.RECEIVING.getEndpoint(), JsonObject.mapFrom(toBeReceivedRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    List<ProcessingStatus> result = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(ReceivingItemResult::getProcessingStatus))
      .filter(s -> Objects.isNull(s.getError()))
      .collect(Collectors.toList());

    assertThat(result, hasSize(1));

    List<Error> errors = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(e -> e.getProcessingStatus().getError()))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());

    assertThat(errors, hasSize(toBeReceivedList.size() - 1));
    assertThat(errors.stream().filter(e -> e.getCode().equals(PIECE_NOT_FOUND.getCode())).count(), is(2L));
    assertThat(errors.stream().filter(e -> e.getCode().equals(USER_HAS_NO_PERMISSIONS.getCode())).count(), is(1L));
  }

  @Test
  void testCheckInCompositeFlow() {
    CompositePoLine expectedFlowPoLine = getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(EXPECTED_FLOW_PO_LINE_ID);
    CompositePoLine randomPoLine1 = getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_1);
    CompositePoLine randomPoLine2 = getMinimalContentCompositePoLine(ORDER_WITH_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_2);
    List.of(expectedFlowPoLine, randomPoLine1, randomPoLine2).forEach(line -> addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line)));

    Title expectedFlowTitle = getTitle(expectedFlowPoLine).withAcqUnitIds(NOT_PROTECTED_UNITS);
    Title randomTitle1 = getTitle(randomPoLine1).withAcqUnitIds(NOT_PROTECTED_UNITS);
    Title randomTitle2 = getTitle(randomPoLine2).withId(RANDOM_TITLE_ID_2).withAcqUnitIds(PROTECTED_UNITS);
    List.of(expectedFlowTitle, randomTitle1, randomTitle2).forEach(title -> addMockEntry(TITLES, title));

    CheckinCollection toBeCheckedInRq = new CheckinCollection();

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(getToBeCheckedIn(EXPECTED_FLOW_PO_LINE_ID, EXPECTED_FLOW_PIECE_ID_1));
    toBeCheckedInList.add(getToBeCheckedIn(RANDOM_PO_LINE_ID_1, getRandomId()));
    toBeCheckedInList.add(getToBeCheckedIn(RANDOM_PO_LINE_ID_2, RANDOM_PIECE_ID_2));
    toBeCheckedInList.add(getToBeCheckedIn(EXPECTED_FLOW_PO_LINE_ID, getRandomId()));

    toBeCheckedInRq.setToBeCheckedIn(toBeCheckedInList);
    toBeCheckedInRq.setTotalRecords(toBeCheckedInList.size());

    ReceivingResults results = verifyPostResponse(Entities.CHECK_IN.getEndpoint(), JsonObject.mapFrom(toBeCheckedInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    List<ProcessingStatus> result = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(ReceivingItemResult::getProcessingStatus))
      .filter(s -> Objects.isNull(s.getError()))
      .collect(Collectors.toList());

    assertThat(result, hasSize(1));

    List<Error> errors = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(e -> e.getProcessingStatus().getError()))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());

    assertThat(errors, hasSize(toBeCheckedInList.size() - 1));
    assertThat(errors.stream().filter(e -> e.getCode().equals(PIECE_NOT_FOUND.getCode())).count(), is(2L));
    assertThat(errors.stream().filter(e -> e.getCode().equals(USER_HAS_NO_PERMISSIONS.getCode())).count(), is(1L));
  }

  private static CheckinCollection getCheckInCollection(List<String> units) {

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder().withAcqUnitIds(units).withId(getRandomId());
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));

    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId()).withId(EXPECTED_FLOW_PO_LINE_ID);
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    Title title = getTitle(poLine).withAcqUnitIds(units);
    addMockEntry(TITLES, title);

    CheckinCollection toBeCheckedInRq = new CheckinCollection();

    List<ToBeCheckedIn> toBeCheckedInList
      = new ArrayList<>(Arrays.asList(getToBeCheckedIn(poLine.getId(), EXPECTED_FLOW_PIECE_ID_1),
      getToBeCheckedIn(poLine.getId(), EXPECTED_FLOW_PIECE_ID_2)));

    toBeCheckedInRq.setToBeCheckedIn(toBeCheckedInList);
    toBeCheckedInRq.setTotalRecords(toBeCheckedInList.size());

    return toBeCheckedInRq;
  }

  private static ReceivingCollection getReceivingCollection(List<String> units) {

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder().withAcqUnitIds(units).withId(getRandomId());
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));

    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId()).withId(EXPECTED_FLOW_PO_LINE_ID);
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    Title title = getTitle(poLine).withAcqUnitIds(units);
    addMockEntry(TITLES, title);

    ReceivingCollection toBeReceivedRq = new ReceivingCollection();

    List<ToBeReceived> toBeReceivedList
      = new ArrayList<>(Arrays.asList(getToBeReceived(poLine.getId(), EXPECTED_FLOW_PIECE_ID_1),
      getToBeReceived(poLine.getId(), EXPECTED_FLOW_PIECE_ID_2)));

    toBeReceivedRq.setToBeReceived(toBeReceivedList);
    toBeReceivedRq.setTotalRecords(toBeReceivedList.size());

    return toBeReceivedRq;
  }

  private void verifyAllowedCase(ReceivingResults results) {
    assertThat(results.getReceivingResults(), allOf(
      everyItem(hasProperty(PROCESSED_SUCCESSFULLY, is(1))),
      everyItem(hasProperty(PROCESSED_WITH_ERROR, is(0)))
    ));

    List<Error> errors = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(e -> e.getProcessingStatus().getError()))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());

    assertThat(errors, empty());
  }

  private void verifyRestrictedCase(ReceivingResults results) {
    assertThat(results.getReceivingResults(), allOf(
      everyItem(hasProperty(PROCESSED_SUCCESSFULLY, is(0))),
      everyItem(hasProperty(PROCESSED_WITH_ERROR, is(1)))
    ));

    List<Error> errors = results.getReceivingResults().stream()
      .flatMap(r -> r.getReceivingItemResults().stream()
        .map(e -> e.getProcessingStatus().getError()))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());

    assertThat(errors, hasSize(2));
    assertThat(errors, allOf(
      everyItem(hasProperty("code", equalTo(USER_HAS_NO_PERMISSIONS.getCode()))),
      everyItem(hasProperty("message", equalTo(USER_HAS_NO_PERMISSIONS.getDescription())))
    ));
  }
}
