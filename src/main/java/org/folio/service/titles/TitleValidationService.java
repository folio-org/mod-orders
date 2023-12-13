package org.folio.service.titles;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.orders.BaseValidationService;

import java.util.List;

public class TitleValidationService extends BaseValidationService {
  /**
   * Validates title.
   *
   * @param title the incoming title
   * @return list of errors or empty list in case if title is valid
   */
  public List<Error> validateTitle(Title title) {
    List<ErrorCodes> errorCodes = checkClaimingConfig(title.getClaimingActive(), title.getClaimingInterval());
    return convertErrorCodesToErrors(errorCodes);
  }

  private List<Error> convertErrorCodesToErrors(List<ErrorCodes> errorCodes) {
    return errorCodes.stream().map(ErrorCodes::toError).toList();
  }
}
