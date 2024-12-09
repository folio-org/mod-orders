package org.folio.models.claiming;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ClaimingError {
  CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY("Cannot send claims, piece ids are empty"),
  CANNOT_RETRIEVE_CONFIG_ENTRIES("Cannot retrieve config entries"),
  CANNOT_GROUP_PIECES_BY_VENDOR_MESSAGE("Cannot group pieces by vendor"),
  CANNOT_CREATE_JOBS_AND_UPDATE_PIECES("Cannot create jobs and update pieces"),
  CANNOT_FIND_A_PIECE_BY_ID("Cannot find a piece by '%s' id"),
  CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS("Cannot find pieces with LATE status to process"),
  CANNOT_COMPLETE_REQ("Cannot complete request to an optional module mod-data-export-spring is unreachable");

  private  final String value;
}
