package org.folio.service.orders;

import org.folio.rest.core.exceptions.ErrorCodes;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class BaseValidationService {
  /**
   * Checks if claiming interval is greater than 0 if claiming is active.
   *
   * @param claimingActive is claiming active
   * @param claimingInterval claiming interval
   * @return list of error codes
   */
  protected List<ErrorCodes> checkClaimingConfig(Boolean claimingActive, Integer claimingInterval) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (Boolean.TRUE.equals(claimingActive) && (Objects.isNull(claimingInterval) || claimingInterval <= 0)) {
      errors.add(ErrorCodes.CLAIMING_CONFIG_INVALID);
    }
    return errors;
  }
}
