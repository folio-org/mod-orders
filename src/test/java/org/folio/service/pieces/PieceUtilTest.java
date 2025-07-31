package org.folio.service.pieces;

import org.folio.CopilotGenerated;
import org.folio.models.pieces.PieceBatchStatusUpdateHolder;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Location;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@CopilotGenerated(model = "Claude Sonnet 4", partiallyGenerated = true)
public class PieceUtilTest {

  @Test
  void testUpdatePieceStatusWithOldAndNewStatus() {
    Piece piece = new Piece().withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    Piece.ReceivingStatus newStatus = Piece.ReceivingStatus.RECEIVED;

    boolean statusChanged = PieceUtil.updatePieceStatus(piece, piece.getReceivingStatus(), newStatus);

    assertTrue(statusChanged);
    assertSame(newStatus, piece.getReceivingStatus());
    assertNotNull(piece.getStatusUpdatedDate());
  }

  @Test
  void testUpdatePieceStatusWithSameOldAndNewStatus() {
    Piece piece = new Piece().withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    Piece.ReceivingStatus newStatus = Piece.ReceivingStatus.EXPECTED;

    boolean statusChanged = PieceUtil.updatePieceStatus(piece, piece.getReceivingStatus(), newStatus);

    assertFalse(statusChanged);
    assertSame(newStatus, piece.getReceivingStatus());
  }

  @Test
  void testUpdatePieceStatusWithHolder() {
    Piece piece = new Piece().withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    PieceBatchStatusUpdateHolder holder = new PieceBatchStatusUpdateHolder(Piece.ReceivingStatus.RECEIVED,
      30, "internal", "external", List.of(piece), UUID.randomUUID().toString());

    boolean statusChanged = PieceUtil.updatePieceStatus(piece, holder);

    assertTrue(statusChanged);
    assertSame(piece.getReceivingStatus(), holder.getReceivingStatus());
    assertSame(piece.getClaimingInterval(), holder.getClaimingInterval());
    assertEquals(piece.getInternalNote(), holder.getInternalNote());
    assertEquals(piece.getExternalNote(), holder.getExternalNote());
    assertNotNull(piece.getStatusUpdatedDate());
  }

  @Test
  void testUpdatePieceStatusWithHolderSameStatus() {
    Piece piece = new Piece().withReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    PieceBatchStatusUpdateHolder holder = new PieceBatchStatusUpdateHolder(Piece.ReceivingStatus.EXPECTED,
      30, "internal", "external", List.of(piece), UUID.randomUUID().toString());

    boolean statusChanged = PieceUtil.updatePieceStatus(piece, holder);

    assertFalse(statusChanged);
    assertSame(piece.getReceivingStatus(), holder.getReceivingStatus());
    assertSame(piece.getClaimingInterval(), holder.getClaimingInterval());
    assertEquals(piece.getInternalNote(), holder.getInternalNote());
    assertEquals(piece.getExternalNote(), holder.getExternalNote());
  }

  @ParameterizedTest
  @EnumSource(value = Piece.Format.class, names = { "PHYSICAL", "OTHER" })
  void returnsExpectedReceiptDateWhenPieceFormatIsPhysicalOrOtherAndPhysicalIsPresent(Piece.Format format) {
    PoLine poLine = new PoLine().withPhysical(new Physical().withExpectedReceiptDate(new Date()));

    Date result = PieceUtil.getExpectedReceiptDate(format, poLine);

    assertEquals(poLine.getPhysical().getExpectedReceiptDate(), result);
  }

  @Test
  void returnsNullWhenPieceFormatIsPhysicalAndPhysicalIsNull() {
    PoLine poLine = new PoLine();

    Date result = PieceUtil.getExpectedReceiptDate(Piece.Format.PHYSICAL, poLine);

    assertNull(result);
  }

  @Test
  void returnsNullWhenPieceFormatIsNotPhysical() {
    PoLine poLine = new PoLine().withPhysical(new Physical().withExpectedReceiptDate(new Date()));

    Date result = PieceUtil.getExpectedReceiptDate(Piece.Format.ELECTRONIC, poLine);

    assertNull(result);
  }

  @Test
  void testGetPiecesLocationsWithEmptyList() {
    List<Piece> pieces = List.of();

    List<Location> result = PieceUtil.getPiecesLocations(pieces);

    assertTrue(result.isEmpty());
  }

  @Test
  void testGetPiecesLocationsWithNonEmptyList() {
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String tenantId = "test-tenant";

    Piece piece = new Piece()
      .withHoldingId(holdingId)
      .withLocationId(locationId)
      .withReceivingTenantId(tenantId);

    List<Piece> pieces = List.of(piece);

    List<Location> result = PieceUtil.getPiecesLocations(pieces);

    assertEquals(1, result.size());
    Location location = result.getFirst();
    assertEquals(holdingId, location.getHoldingId());
    assertEquals(locationId, location.getLocationId());
    assertEquals(tenantId, location.getTenantId());
  }

}
