package org.folio;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.TestConstants.ID;
import static org.folio.TestConstants.ITEM_ID;
import static org.folio.TestConstants.LOCATION_ID;
import static org.folio.TestConstants.MIN_PO_ID;
import static org.folio.TestConstants.MIN_PO_LINE_ID;
import static org.folio.TestConstants.PIECE_ID;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.MockServer.serverRqRs;
import static org.folio.rest.impl.TitlesApiTest.SAMPLE_TITLE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeReceived;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

public final class TestUtils {

  private static final Logger logger = LogManager.getLogger();

  private TestUtils() {}

  public static String getMockData(String path) throws IOException {

    try (InputStream resourceAsStream = TestConstants.class.getClassLoader().getResourceAsStream(path)) {
      if (resourceAsStream != null) {
        return IOUtils.toString(resourceAsStream, StandardCharsets.UTF_8);
      } else {
        StringBuilder sb = new StringBuilder();
        try (Stream<String> lines = Files.lines(Paths.get(path))) {
          lines.forEach(sb::append);
        }
        return sb.toString();
      }
    }
  }

  public static JsonObject getMockAsJson(String path, String id) {
    return getMockAsJson(String.format("%s%s.json", path, id));
  }

  public static JsonObject getMockAsJson(String fullPath) {
    try {
      return new JsonObject(getMockData(fullPath));
    } catch (IOException e) {
      fail(e.getMessage());
    }
    return new JsonObject();
  }

  public static Date convertLocalDateTimeToDate(LocalDateTime dateToConvert) {
    return Date
      .from(dateToConvert.atZone(ZoneId.systemDefault())
        .toInstant());
  }

  public static Object[] getModifiedProtectedFields(Error error) {
    return Optional.of(error.getAdditionalProperties()
            .get("protectedAndModifiedFields"))
            .map(obj -> (List<?>) obj)
            .get()
            .toArray();
  }

    public static void checkVertxContextCompletion(VertxTestContext context) throws Throwable {
      assertTrue(context.awaitCompletion(30, SECONDS));
      if (context.failed()) {
        throw context.causeOfFailure();
      }
    }

  public static Piece getMinimalContentPiece(String poLineId) {
    return new Piece()
      .withId(PIECE_ID)
      .withReceivingStatus(Piece.ReceivingStatus.RECEIVED)
      .withFormat(Piece.Format.PHYSICAL)
      .withItemId(ITEM_ID)
      .withTitleId(SAMPLE_TITLE_ID)
      .withReceiptDate(new Date())
      .withPoLineId(poLineId);
  }

  public static String getInstanceId(PoLine poline) {
    return Optional.ofNullable(serverRqRs.get(TITLES, HttpMethod.PUT)).orElseGet(Collections::emptyList).stream()
      .map(title -> title.mapTo(Title.class))
      .filter(title -> poline.getId().equals(title.getPoLineId()))
      .map(Title::getInstanceId)
      .findFirst().orElse(null);
  }

  public static void validatePoLineCreationErrorForNonPendingOrder(String errorCode, Errors errors, int externalAPICalls) {
    assertEquals(1, errors.getErrors().size());
    assertEquals(errorCode, errors.getErrors().get(0).getCode());
    // Assert that only PO Lines limit (count of existing Lines) , GET PO and ISBN validation requests made
    assertEquals(externalAPICalls, MockServer.serverRqRs.rowKeySet().size());
    assertEquals(1, MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET).size());
    assertEquals(1, getPoLineSearches().size());
  }

  public static void verifyLocationQuantity(Location location, CompositePoLine.OrderFormat orderFormat) {
    switch (orderFormat) {
      case P_E_MIX:
        assertEquals(location.getQuantityPhysical() + location.getQuantityElectronic(), location.getQuantity().intValue());
        break;
      case ELECTRONIC_RESOURCE:
        assertEquals(location.getQuantityElectronic(), location.getQuantity());
        break;
      case PHYSICAL_RESOURCE:
      case OTHER:
        assertEquals(location.getQuantityPhysical(), location.getQuantity());
        break;
    }
  }

  public static Title getTitle(CompositePoLine poLine) {
    return new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(poLine.getId())
      .withTitle(poLine.getTitleOrPackage())
      .withInstanceId(poLine.getInstanceId())
      .withProductIds(Optional.ofNullable(poLine.getDetails()).orElseGet(Details::new).getProductIds());
  }

  public static CompositePoLine getMinimalContentCompositePoLine() {
    return getMinimalContentCompositePoLine(MIN_PO_ID);
  }

  public static CompositePoLine getMinimalContentCompositePoLine(String orderId) {
    return new CompositePoLine().withSource(CompositePoLine.Source.EDI)
      .withId(MIN_PO_LINE_ID)
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withAcquisitionMethod(CompositePoLine.AcquisitionMethod.PURCHASE)
      .withPhysical(new Physical().withMaterialType("2d1398ae-e1aa-4c7c-b9c9-15adf8cf6425"))
      .withCost(new Cost().withCurrency("EUR").withQuantityPhysical(1).withListUnitPrice(10.0))
      .withLocations(Collections.singletonList(new Location().withLocationId("2a00b0be-1447-42a1-a112-124450991899").withQuantityPhysical(1).withQuantity(1)))
      .withTitleOrPackage("Title")
      .withPurchaseOrderId(orderId);
  }

  public static CompositePurchaseOrder getMinimalContentCompositePurchaseOrder() {
    return new CompositePurchaseOrder()
      .withId(MIN_PO_ID)
      .withPoNumber("TestNumber")
      .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME)
      .withVendor("7d232b43-bf9a-4301-a0ce-9e076298632e");
  }

  public static String encodePrettily(Object entity) {
    return JsonObject.mapFrom(entity).encodePrettily();
  }

  public static List<CheckInPiece> getCheckInPieces(CheckInPiece... checkInPieces) {
    return Arrays.asList(checkInPieces);
  }

  public static List<ReceivedItem> getReceivedItems(ReceivedItem... receivedItems) {
    return Arrays.asList(receivedItems);
  }

  public static CheckInPiece getCheckInPiece(String id) {
    return new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.IN_PROCESS).withLocationId(LOCATION_ID).withId(id);
  }

  public static ReceivedItem getReceivedItem(String pieceId) {
    return new ReceivedItem().withItemStatus(ReceivedItem.ItemStatus.IN_PROCESS).withLocationId(LOCATION_ID).withPieceId(pieceId);
  }

  public static ToBeCheckedIn getToBeCheckedIn(String poLineId, String pieceId) {
    return new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(getCheckInPieces(getCheckInPiece(pieceId)));
  }

  public static ToBeReceived getToBeReceived(String poLineId, String pieceId) {
    return new ToBeReceived()
      .withPoLineId(poLineId)
      .withReceivedItems(getReceivedItems(getReceivedItem(pieceId)));
  }

  public static String getRandomId() {
    return UUID.randomUUID().toString();
  }

  public static void validateSavedPoLines() {
    getPoLineUpdates()
      .forEach(poline -> {
        logger.info("validate poline {}", poline.getString(ID));
        poline.mapTo(PoLine.class);
      });
  }
}
