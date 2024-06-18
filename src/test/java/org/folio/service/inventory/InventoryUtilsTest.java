package org.folio.service.inventory;

import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.HashMap;
import java.util.Map;

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

}
