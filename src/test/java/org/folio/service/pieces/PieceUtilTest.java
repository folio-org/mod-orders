package org.folio.service.pieces;

import org.folio.CopilotGenerated;
import org.folio.models.pieces.PieceBatchStatusUpdateHolder;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@CopilotGenerated(partiallyGenerated = true)
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
}
