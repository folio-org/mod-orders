package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
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
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.folio.HttpStatus;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;

@RunWith(JUnitParamsRunner.class)
public class ReceivingCheckinProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PiecesProtectionTest.class);


  private static final String ORDER_WITH_PROTECTED_UNITS_ID = getRandomId();
  private static final String ORDER_WITH_NOT_PROTECTED_UNITS_ID = getRandomId();

  private static final String RANDOM_PO_LINE_ID_1 = getRandomId();
  private static final String RANDOM_PO_LINE_ID_2 = getRandomId();
  private static final String PROCESSED_SUCCESSFULLY = "processedSuccessfully";
  private static final String PROCESSED_WITH_ERROR = "processedWithError";
  private static final String EXPECTED_FLOW_PO_LINE_ID = "9b68e7c2-8818-4387-a405-6567232c1c6f";
  private static final String EXPECTED_FLOW_PIECE_ID_1 = "5e317dc2-deeb-4429-b2a1-91e5cd0fd5f7";
  private static final String EXPECTED_FLOW_PIECE_ID_2 = "71d9322b-5cdd-45d8-ad45-c7f3044802e7";

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

  @Test
  @Parameters(source = Entities.class)
  public void testFlowWithNonExistedUnits(Entities entity) {
    logger.info("=== Test check-in/receiving flow - non-existing units ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(NON_EXISTENT_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyRestrictedCase(results);
    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters(source = Entities.class)
  public void testFlowWithAllowedUnits(Entities entity) {
    logger.info("=== Test check-in/receiving flow - not-protecting units ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(NOT_PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyAllowedCase(results);
    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters(source = Entities.class)
  public void testFlowWithRestrictedUnitsAndAllowedUser(Entities entity) {
    logger.info("=== Test check-in/receiving flow - protecting units and allowed user ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyAllowedCase(results);
    validateNumberOfRequests(1, 1);
  }

  @Test
  @Parameters(source = Entities.class)
  public void testCheckInWithProtectedUnitsAndForbiddenUser(Entities entity) {
    logger.info("=== Test check-in/receiving flow - protecting units and forbidden user ===");

    ReceivingResults results = verifyPostResponse(entity.getEndpoint(), entity.getJsonRequest(PROTECTED_UNITS),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt()).as(ReceivingResults.class);

    verifyRestrictedCase(results);
    validateNumberOfRequests(1, 1);
  }

  @Test
  public void testReceivingCompositeFlow() {

    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(getMinimalContentCompositePurchaseOrder().withAcqUnitIds(NOT_PROTECTED_UNITS).withId(ORDER_WITH_NOT_PROTECTED_UNITS_ID)));
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(getMinimalContentCompositePurchaseOrder().withAcqUnitIds(PROTECTED_UNITS).withId(ORDER_WITH_PROTECTED_UNITS_ID)));
    List<CompositePoLine> poLines = new ArrayList<>();
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(EXPECTED_FLOW_PO_LINE_ID));
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_1));
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_2));
    poLines.forEach(line -> addMockEntry(PO_LINES, JsonObject.mapFrom(line)));

    MockServer.addMockTitles(poLines);
    ReceivingCollection toBeReceivedRq = new ReceivingCollection();

    List<ToBeReceived> toBeReceivedList = new ArrayList<>();
    toBeReceivedList.add(getToBeReceived(EXPECTED_FLOW_PO_LINE_ID, EXPECTED_FLOW_PIECE_ID_1));
    toBeReceivedList.add(getToBeReceived(RANDOM_PO_LINE_ID_1, getRandomId()));
    toBeReceivedList.add(getToBeReceived(RANDOM_PO_LINE_ID_2, getRandomId()));
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
  public void testCheckInCompositeFlow() {

    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(getMinimalContentCompositePurchaseOrder().withAcqUnitIds(NOT_PROTECTED_UNITS).withId(ORDER_WITH_NOT_PROTECTED_UNITS_ID)));
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(getMinimalContentCompositePurchaseOrder().withAcqUnitIds(PROTECTED_UNITS).withId(ORDER_WITH_PROTECTED_UNITS_ID)));

    List<CompositePoLine> poLines = new ArrayList<>();
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(EXPECTED_FLOW_PO_LINE_ID));
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_NOT_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_1));
    poLines.add(getMinimalContentCompositePoLine(ORDER_WITH_PROTECTED_UNITS_ID).withId(RANDOM_PO_LINE_ID_2));
    poLines.forEach(line -> addMockEntry(PO_LINES, JsonObject.mapFrom(line)));

    MockServer.addMockTitles(poLines);

    CheckinCollection toBeCheckedInRq = new CheckinCollection();

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(getToBeCheckedIn(EXPECTED_FLOW_PO_LINE_ID, EXPECTED_FLOW_PIECE_ID_1));
    toBeCheckedInList.add(getToBeCheckedIn(RANDOM_PO_LINE_ID_1, getRandomId()));
    toBeCheckedInList.add(getToBeCheckedIn(RANDOM_PO_LINE_ID_2, getRandomId()));
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
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId()).withId(EXPECTED_FLOW_PO_LINE_ID);
    addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));

    MockServer.addMockTitles(Collections.singletonList(poLine));

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
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId()).withId(EXPECTED_FLOW_PO_LINE_ID);
    addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));

    MockServer.addMockTitles(Collections.singletonList(poLine));
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
