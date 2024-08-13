package org.folio.service.inventory;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import java.io.IOException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.utils.TenantTool;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.HashMap;
import java.util.Map;
import org.mockito.MockitoAnnotations;

import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PIECES_JSON;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ACCESSION_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_LEVEL_CALL_NUMBER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

@ExtendWith(VertxExtension.class)
public class InventoryUtilsTest {

  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(Vertx.vertx().getOrCreateContext(), okapiHeadersMock);
  }

  @Test
  void testUpdateItemWithPieceFields() {
    // given
    Piece piece = new Piece();
    piece.setEnumeration("enumeration");
    piece.setCopyNumber("copy number");
    piece.setChronology("chronology");
    piece.setBarcode("barcode");
    piece.setAccessionNumber("accession number");
    piece.setCallNumber("call number");
    piece.setDiscoverySuppress(true);

    String oldValue = "old value";
    JsonObject item = new JsonObject(new HashMap<>(Map.of(
      ITEM_ENUMERATION, oldValue,
      COPY_NUMBER, oldValue,
      ITEM_CHRONOLOGY, oldValue,
      ITEM_BARCODE, oldValue,
      ITEM_ACCESSION_NUMBER, oldValue,
      ITEM_LEVEL_CALL_NUMBER, oldValue,
      ITEM_DISCOVERY_SUPPRESS, false
    )));

    // when
    InventoryUtils.updateItemWithPieceFields(item, piece);

    // then
    assertEquals(piece.getDisplaySummary(), item.getString(ITEM_DISPLAY_SUMMARY));
    assertEquals(piece.getEnumeration(), item.getString(ITEM_ENUMERATION));
    assertEquals(piece.getCopyNumber(), item.getString(COPY_NUMBER));
    assertEquals(piece.getChronology(), item.getString(ITEM_CHRONOLOGY));
    assertEquals(piece.getBarcode(), item.getString(ITEM_BARCODE));
    assertEquals(piece.getAccessionNumber(), item.getString(ITEM_ACCESSION_NUMBER));
    assertEquals(piece.getCallNumber(), item.getString(ITEM_LEVEL_CALL_NUMBER));
    assertEquals(piece.getDiscoverySuppress(), item.getBoolean(ITEM_DISCOVERY_SUPPRESS));
  }

  @Test
  void testUpdateItemWithPieceFields_notOverwrite() {
    // given
    Piece piece = new Piece();

    String oldValue = "old value";
    JsonObject item = new JsonObject(new HashMap<>(Map.of(
      ITEM_ENUMERATION, oldValue,
      COPY_NUMBER, oldValue,
      ITEM_CHRONOLOGY, oldValue,
      ITEM_BARCODE, oldValue,
      ITEM_ACCESSION_NUMBER, oldValue,
      ITEM_LEVEL_CALL_NUMBER, oldValue,
      ITEM_DISCOVERY_SUPPRESS, false
    )));

    // when
    InventoryUtils.updateItemWithPieceFields(item, piece);

    // then
    assertEquals(oldValue, item.getString(ITEM_ENUMERATION));
    assertEquals(oldValue, item.getString(COPY_NUMBER));
    assertEquals(oldValue, item.getString(ITEM_CHRONOLOGY));
    assertEquals(oldValue, item.getString(ITEM_BARCODE));
    assertEquals(oldValue, item.getString(ITEM_ACCESSION_NUMBER));
    assertEquals(oldValue, item.getString(ITEM_LEVEL_CALL_NUMBER));
    assertFalse(item.getBoolean(ITEM_DISCOVERY_SUPPRESS));
  }


  @Test
  void testConstructItemRecreateConfigSrcWithReceivingTenantId() throws IOException {
    var exceptedSrcTenantId = "university";

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);

    assertEquals(exceptedSrcTenantId, srcConfig.tenantId());
    assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigSrcWithNullReceivingTenantId() throws IOException {
    var exceptedSrcTenantId = TenantTool.tenantId(requestContext.getHeaders());

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);

    Assertions.assertNull(srcConfig.tenantId());
    Assertions.assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigDstWithReceivingTenantId() throws IOException {
    var exceptedDstTenantId = "college";

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(exceptedDstTenantId);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, false);

    Assertions.assertEquals(exceptedDstTenantId, dstConfig.tenantId());
    Assertions.assertEquals(exceptedDstTenantId, TenantTool.tenantId(dstConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigDstWithNullReceivingTenantId() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, false);

    Assertions.assertNull(dstConfig.tenantId());
    Assertions.assertNull(dstConfig.context());
  }

  @Test
  void testAllowItemRecreateTrue() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId("college");

    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesRequest.getReceivingTenantId(), requestContext, false);

    Assertions.assertTrue(InventoryUtils.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithSameTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId("university");

    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesRequest.getReceivingTenantId(), requestContext, false);

    Assertions.assertFalse(InventoryUtils.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithNullSrcTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);

    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesRequest.getReceivingTenantId(), requestContext, false);

    Assertions.assertFalse(InventoryUtils.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithNullDstTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class);

    var srcConfig = InventoryUtils.constructItemRecreateConfig(piecesStorage.getReceivingTenantId(), requestContext, true);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(piecesRequest.getReceivingTenantId(), requestContext, false);

    Assertions.assertFalse(InventoryUtils.allowItemRecreate(srcConfig, dstConfig));
  }
}
