package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.CONFIGS;
import static org.folio.orders.utils.HelperUtils.CONFIG_NAME;
import static org.folio.orders.utils.HelperUtils.CONFIG_VALUE;
import static org.folio.orders.utils.HelperUtils.calculateExpectedQuantityOfPiecesWithoutItemCreation;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculatePiecesQuantityWithoutLocation;
import static org.folio.orders.utils.HelperUtils.calculateTotalQuantity;
import static org.folio.orders.utils.HelperUtils.getElectronicCostQuantity;
import static org.folio.orders.utils.HelperUtils.getPhysicalCostQuantity;
import static org.folio.orders.utils.HelperUtils.groupLocationsById;
import static org.folio.orders.utils.HelperUtils.isHoldingCreationRequiredForLocation;
import static org.folio.rest.impl.ApiTestBase.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.rest.impl.ApiTestBase.getInstanceId;
import static org.folio.rest.impl.InventoryHelper.CONFIG_NAME_INSTANCE_STATUS_CODE;
import static org.folio.rest.impl.InventoryHelper.CONFIG_NAME_INSTANCE_TYPE_CODE;
import static org.folio.rest.impl.InventoryHelper.CONFIG_NAME_LOAN_TYPE_NAME;
import static org.folio.rest.impl.InventoryHelper.CONTRIBUTOR_NAME;
import static org.folio.rest.impl.InventoryHelper.CONTRIBUTOR_NAME_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_INSTANCE_STATUS_CODE;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_INSTANCE_TYPE_CODE;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_LOAN_TYPE_NAME;
import static org.folio.rest.impl.InventoryHelper.HOLDING_INSTANCE_ID;
import static org.folio.rest.impl.InventoryHelper.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_CONTRIBUTORS;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_DATE_OF_PUBLICATION;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIERS;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIER_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIER_TYPE_VALUE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_PUBLICATION;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_PUBLISHER;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_SOURCE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_STATUSES;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_STATUS_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TITLE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TYPES;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.rest.impl.InventoryHelper.ITEM_MATERIAL_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.ITEM_PERMANENT_LOAN_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.rest.impl.InventoryHelper.ITEM_STATUS;
import static org.folio.rest.impl.InventoryHelper.ITEM_STATUS_NAME;
import static org.folio.rest.impl.InventoryHelper.LOAN_TYPES;
import static org.folio.rest.impl.MockServer.CONFIG_MOCK_PATH;
import static org.folio.rest.impl.MockServer.INSTANCE_STATUSES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.INSTANCE_TYPES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.LOAN_TYPES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getCreatedInstances;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedPieces;
import static org.folio.rest.impl.MockServer.getHoldingsSearches;
import static org.folio.rest.impl.MockServer.getInstancesSearches;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReceivedItem;

import io.restassured.http.Header;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

class InventoryInteractionTestHelper {

  private static final Logger logger = LoggerFactory.getLogger(InventoryInteractionTestHelper.class);

  static void verifyInstanceLinksForUpdatedOrder(CompositePurchaseOrder reqData) {
    List<JsonObject> polUpdates = getPoLineUpdates();
    assertNotNull(polUpdates);
    for (CompositePoLine compLine : reqData.getCompositePoLines()) {
      int itemsQuantity = calculateInventoryItemsQuantity(compLine);
      if (itemsQuantity > 0) {
        boolean instanceLinked = false;
        for (JsonObject jsonObj : polUpdates) {
          jsonObj.remove("instanceId");
          PoLine line = jsonObj.mapTo(PoLine.class);
          if (StringUtils.equals(line.getId(), compLine.getId()) && StringUtils.isNotEmpty(getInstanceId(line))) {
            instanceLinked = true;
            // Populate instance id in the req data for further validation
            compLine.setInstanceId(getInstanceId(line));
            break;
          }
        }

        assertTrue("The PO Line must contain instance id", instanceLinked);
      }
    }
  }

  static void verifyInventoryInteraction(CompositePurchaseOrder reqData, int createdInstancesCount) {
    verifyInventoryInteraction(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, reqData, createdInstancesCount);
  }

  static void verifyInventoryInteraction(Header tenant, CompositePurchaseOrder reqData, int createdInstancesCount) {
    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(true);

    List<JsonObject> createdInstances = getCreatedInstances();
    List<JsonObject> createdHoldings = getCreatedHoldings();
    List<JsonObject> createdPieces = getCreatedPieces();

    assertEquals(createdInstancesCount, createdInstances.size());

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();

    verifyPiecesQuantityForSuccessCase(reqData.getCompositePoLines(), createdPieces);

    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      verifyInstanceCreated(tenant, createdInstances, pol);
      verifyHoldingsCreated(createdHoldings, pol);
      verifyItemsCreated(tenant, items, pol);
      verifyPiecesCreated(items, reqData.getCompositePoLines(), createdPieces);
    }
  }

  static void verifyInventoryInteraction(boolean checkItemsCreated) {
    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = getInstancesSearches();
    List<JsonObject> holdingsSearches = getHoldingsSearches();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> piecesSearches = getPieceSearches();
    assertNotNull(instancesSearches);
    logger.debug("--------------------------- Instances found -------------------------------\n" + new JsonArray(instancesSearches).encodePrettily());
    assertNotNull(holdingsSearches);
    logger.debug("--------------------------- Holdings found -------------------------------\n" + new JsonArray(holdingsSearches).encodePrettily());
    assertNotNull(itemsSearches);
    logger.debug("--------------------------- Items found -------------------------------\n" + new JsonArray(itemsSearches).encodePrettily());
    assertNotNull(piecesSearches);
    logger.debug("--------------------------- Pieces found -------------------------------\n" + new JsonArray(piecesSearches).encodePrettily());

    // Check that creation of the new instances and items was done
    List<JsonObject> createdInstances = getCreatedInstances();
    List<JsonObject> createdHoldings = getCreatedHoldings();
    List<JsonObject> createdItems = getCreatedItems();
    List<JsonObject> createdPieces = getCreatedPieces();
    assertNotNull(createdInstances);
    logger.debug("--------------------------- Instances created -------------------------------\n" + new JsonArray(createdInstances).encodePrettily());
    assertNotNull(createdHoldings);
    logger.debug("--------------------------- Holdings created -------------------------------\n" + new JsonArray(createdHoldings).encodePrettily());
    if (checkItemsCreated) {
      assertNotNull(createdItems);
      logger.debug("--------------------------- Items created -------------------------------\n" + new JsonArray(createdItems).encodePrettily());
    }
    assertNotNull(createdPieces);
    logger.debug("--------------------------- Pieces created -------------------------------\n" + new JsonArray(createdPieces).encodePrettily());
  }

  static void verifyPiecesQuantityForSuccessCase(List<CompositePoLine> poLines, List<JsonObject> createdPieces) {
    int totalQuantity = 0;
    for (CompositePoLine poLine : poLines) {
      if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) continue;
      totalQuantity += calculateTotalQuantity(poLine);
    }
    assertEquals(totalQuantity, createdPieces.size());
  }

  static List<JsonObject> joinExistingAndNewItems() {
    List<JsonObject> items = new ArrayList<>(CollectionUtils.emptyIfNull(getCreatedItems()));
    getItemsSearches().forEach(json -> {
      JsonArray existingItems = json.getJsonArray("items");
      if (existingItems != null) {
        existingItems.forEach(item -> items.add((JsonObject) item));
      }
    });
    return items;
  }

  private static void verifyInstanceCreated(Header tenant, List<JsonObject> inventoryInstances, CompositePoLine pol) {
    boolean verified = false;
    for (JsonObject instance : inventoryInstances) {
      if (pol.getTitleOrPackage().equals(instance.getString("title"))) {
        verifyInstanceRecordRequest(tenant, instance, pol);
        verified = true;
        break;
      }
    }
    if (pol.getCheckinItems() != null && pol.getCheckinItems()) return;

    int expectedItemsQuantity = calculateInventoryItemsQuantity(pol);
    if (!verified && expectedItemsQuantity > 0) {
      fail("No matching instance for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }

    if ((!verified && StringUtils.isNotEmpty(pol.getInstanceId()) || (verified && expectedItemsQuantity == 0))) {
      fail("No instance expected for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }
  }

  static void verifyPiecesCreated(List<JsonObject> inventoryItems, List<CompositePoLine> compositePoLines, List<JsonObject> pieceJsons) {
    // Collect all item id's
    List<String> itemIds = inventoryItems.stream()
      .map(item -> item.getString(ID))
      .collect(Collectors.toList());
    List<Piece> pieces = pieceJsons
      .stream()
      .map(pieceObj -> pieceObj.mapTo(Piece.class))
      .collect(Collectors.toList());

    // Verify quantity of created pieces
    int totalForAllPoLines = 0;
    for (CompositePoLine poLine : compositePoLines) {
      List<Location> locations = poLine.getLocations().stream()
        .filter(location -> isHoldingCreationRequiredForLocation(poLine, location))
        .collect(Collectors.toList());

      // Prepare data first

      // Calculated quantities
      int expectedElQty = 0;
      int expectedPhysQty = 0;
      int expectedOthQty = 0;
      if (poLine.getCheckinItems() == null || !poLine.getCheckinItems()) {
        if (poLine.getOrderFormat() == CompositePoLine.OrderFormat.OTHER) {
          expectedOthQty += getPhysicalCostQuantity(poLine);//calculatePiecesQuantity(Piece.Format.OTHER, locations);
        } else {
          expectedPhysQty += getPhysicalCostQuantity(poLine);//calculatePiecesQuantity(Piece.Format.PHYSICAL, locations);
        }
        expectedElQty = getElectronicCostQuantity(poLine);//calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations);
      }

      int expectedWithItemQty = calculateExpectedQuantityOfPiecesWithoutItemCreation(poLine, locations);
      int expectedWithoutItemQty = calculateInventoryItemsQuantity(poLine, locations);
      int expectedWithoutLocation = calculatePiecesQuantityWithoutLocation(poLine).values().stream().mapToInt(Integer::intValue).sum();

      // Prepare pieces for PO Line
      List<Piece> piecesByPoLine = pieces
        .stream()
        .filter(piece -> piece.getPoLineId().equals(poLine.getId()))
        .collect(Collectors.toList());

      // Get all PO Line's locations' ids
      List<String> locationIds = locations
        .stream()
        .map(Location::getLocationId)
        .distinct()
        .collect(Collectors.toList());

      int expectedTotal = expectedWithItemQty + expectedWithoutItemQty + expectedWithoutLocation;
      // Make sure that quantities by piece type and by item presence are the same
      assertThat(expectedPhysQty + expectedElQty + expectedOthQty, is(expectedTotal));

      assertThat(piecesByPoLine, hasSize(expectedTotal));

      // Verify each piece individually
      piecesByPoLine.forEach(piece -> {
        // Check if itemId in inventoryItems match itemId in piece record
        if (piece.getLocationId() != null) {
          assertThat(locationIds, hasItem(piece.getLocationId()));
        }
        assertThat(piece.getReceivingStatus(), equalTo(Piece.ReceivingStatus.EXPECTED));
        if (piece.getItemId() != null) {
          assertThat(itemIds, hasItem(piece.getItemId()));
        }
        assertThat(piece.getFormat(), notNullValue());
      });

      totalForAllPoLines += expectedTotal;
    }

    // Make sure that none of pieces missed
    assertThat(pieceJsons, hasSize(totalForAllPoLines));
  }

  private static void verifyHoldingsCreated(List<JsonObject> holdings, CompositePoLine pol) {
    Map<String, List<Location>> groupedLocations = groupLocationsById(pol);

    long actualQty = 0;
    for (JsonObject holding : holdings) {
      if (groupedLocations.keySet().contains(holding.getString(HOLDING_PERMANENT_LOCATION_ID))
        && StringUtils.equals(pol.getInstanceId(), holding.getString(HOLDING_INSTANCE_ID))) {
        actualQty++;
      }
    }

    int itemsQuantity = calculateInventoryItemsQuantity(pol);
    if (itemsQuantity == 0) {
      assertEquals("No holdings expected", 0, actualQty);
    } else {
      long expectedQty = groupedLocations
        .values()
        .stream()
        .mapToInt(locations -> calculateInventoryItemsQuantity(pol, locations))
        .count();
      assertEquals("Quantity of holdings does not match to expected", expectedQty, actualQty);
    }
  }

  private static void verifyItemsCreated(Header tenant, List<JsonObject> inventoryItems, CompositePoLine pol) {
    Map<Piece.Format, Integer> expectedItemsPerResourceType = HelperUtils.calculatePiecesWithItemIdQuantity(pol,
      pol.getLocations());

    Map<String, List<JsonObject>> itemsByMaterial = inventoryItems.stream()
      .filter(item -> pol.getId().equals(item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER)))
      .collect(Collectors.groupingBy(item -> item.getString(ITEM_MATERIAL_TYPE_ID)));

    expectedItemsPerResourceType.forEach((resourceType, quantity) -> {
      if (quantity < 1) {
        return;
      }
      if (resourceType.equals(Piece.Format.ELECTRONIC)) {
        assertThat(quantity, is(itemsByMaterial.get(pol.getEresource().getMaterialType()).size()));
        itemsByMaterial.get(pol.getEresource().getMaterialType())
          .forEach(item -> verifyItemRecordRequest(tenant, item, pol.getEresource().getMaterialType()));
      } else {
        assertThat(quantity, is(itemsByMaterial.get(pol.getPhysical().getMaterialType()).size()));
        itemsByMaterial.get(pol.getPhysical().getMaterialType())
          .forEach(item -> verifyItemRecordRequest(tenant, item, pol.getPhysical().getMaterialType()));

      }
    });

    long actualQuantity = itemsByMaterial.values()
      .stream()
      .mapToInt(List::size)
      .sum();

    int expectedQuantity = calculateInventoryItemsQuantity(pol);
    if (expectedQuantity != actualQuantity) {
      fail(String.format("Actual items quantity is %d but expected %d", actualQuantity, expectedQuantity));
    }
  }

  private static void verifyInstanceRecordRequest(Header tenant, JsonObject instance, CompositePoLine line) {
    assertThat(instance.getString(INSTANCE_TITLE), equalTo(line.getTitleOrPackage()));
    assertThat(instance.getString(INSTANCE_SOURCE), equalTo("FOLIO"));
    assertThat(instance.getString(INSTANCE_STATUS_ID), equalTo(getInstanceStatusId(tenant)));
    assertThat(instance.getString(INSTANCE_TYPE_ID), equalTo(getInstanceTypeId(tenant)));

    JsonObject publication = instance.getJsonArray(INSTANCE_PUBLICATION).getJsonObject(0);
    assertThat(publication.getString(INSTANCE_PUBLISHER), equalTo(line.getPublisher()));
    assertThat(publication.getString(INSTANCE_DATE_OF_PUBLICATION), equalTo(line.getPublicationDate()));

    if (line.getDetails().getProductIds().size() > 0) {
      assertThat(instance.getJsonArray(INSTANCE_IDENTIFIERS).getJsonObject(0).getString(INSTANCE_IDENTIFIER_TYPE_ID), equalTo("8261054f-be78-422d-bd51-4ed9f33c3422"));
      assertThat(instance.getJsonArray(INSTANCE_IDENTIFIERS).getJsonObject(0).getString(INSTANCE_IDENTIFIER_TYPE_VALUE), equalTo(line.getDetails().getProductIds().get(0).getProductId()));
    }
    Object[] actual = Optional.ofNullable(instance.getJsonArray(INSTANCE_CONTRIBUTORS)).orElse(new JsonArray()).stream()
      .map(o -> (JsonObject) o)
      .collect(Collectors.toMap(c -> c.getString(CONTRIBUTOR_NAME), c -> c.getString(CONTRIBUTOR_NAME_TYPE_ID)))
      .entrySet().toArray();
    Object[] expected = line.getContributors().stream()
      .collect(Collectors.toMap(Contributor::getContributor, Contributor::getContributorNameTypeId))
      .entrySet().toArray();
    assertArrayEquals(expected, actual);
  }

  private static void verifyItemRecordRequest(Header tenant, JsonObject item, String material) {
    assertThat(item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER), not(isEmptyOrNullString()));
    assertThat(material, is(item.getString(ITEM_MATERIAL_TYPE_ID)));
    assertThat(item.getString(ITEM_HOLDINGS_RECORD_ID), not(isEmptyOrNullString()));
    assertThat(item.getString(ITEM_PERMANENT_LOAN_TYPE_ID), equalTo(getLoanTypeId(tenant)));
    assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
    assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ON_ORDER.value()));
  }

  private static String getInstanceStatusId(Header tenant) {
    try {
      String code = getConfigValue(tenant, CONFIG_NAME_INSTANCE_STATUS_CODE, DEFAULT_INSTANCE_STATUS_CODE);
      JsonArray types = new JsonObject(ApiTestBase.getMockData(INSTANCE_STATUSES_MOCK_DATA_PATH + "types.json"))
        .getJsonArray(INSTANCE_STATUSES);
      return getIdByKeyValue("code", code, types);
    } catch (IOException e) {
      return null;
    }
  }

  private static String getInstanceTypeId(Header tenant) {
    try {
      String code = getConfigValue(tenant, CONFIG_NAME_INSTANCE_TYPE_CODE, DEFAULT_INSTANCE_TYPE_CODE);
      JsonArray types = new JsonObject(ApiTestBase.getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "types.json"))
        .getJsonArray(INSTANCE_TYPES);
      return getIdByKeyValue("code", code, types);
    } catch (IOException e) {
      return null;
    }
  }

  private static String getLoanTypeId(Header tenant) {
    try {
      String name = getConfigValue(tenant, CONFIG_NAME_LOAN_TYPE_NAME, DEFAULT_LOAN_TYPE_NAME);
      JsonArray types = new JsonObject(ApiTestBase.getMockData(LOAN_TYPES_MOCK_DATA_PATH + "types.json"))
        .getJsonArray(LOAN_TYPES);
      return getIdByKeyValue("name", name, types);
    } catch (IOException e) {
      return null;
    }
  }

  private static String getIdByKeyValue(String key, String value, JsonArray types) {
    return types.stream()
      .map(o -> (JsonObject) o)
      .filter(config -> value.equals(config.getString(key)))
      .map(config -> config.getString(ID))
      .findFirst()
      .orElse(null);
  }

  private static String getConfigValue(Header tenant, String configName, String defaultValue) throws IOException {
    JsonObject configs = new JsonObject(ApiTestBase.getMockData(String.format(CONFIG_MOCK_PATH, tenant.getValue())));
    return configs.getJsonArray(CONFIGS)
      .stream()
      .map(o -> (JsonObject) o)
      .filter(config -> configName.equals(config.getString(CONFIG_NAME)))
      .map(config -> config.getString(CONFIG_VALUE))
      .findFirst()
      .orElse(defaultValue);
  }

}
