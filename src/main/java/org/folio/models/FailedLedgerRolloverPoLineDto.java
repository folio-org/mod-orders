package org.folio.models;

import java.util.UUID;

public record FailedLedgerRolloverPoLineDto(UUID id,
                                            UUID rolloverId,
                                            UUID ledgerId,
                                            UUID poLineId,
                                            String requestBody,
                                            String responseBody,
                                            int statusCode,
                                            String workflow_status) {
}
