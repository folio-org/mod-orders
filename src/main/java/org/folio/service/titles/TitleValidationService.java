package org.folio.service.titles;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.orders.BaseValidationService;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class TitleValidationService extends BaseValidationService {
  // Pattern matches the JSON schema definition in acq-models/mod-orders-storage/schemas/po_line.json
  private static final Pattern POL_NUMBER_PATTERN = Pattern.compile("^[a-zA-Z0-9]{1,22}-[0-9]{1,3}$");

  /**
   * Validates title.
   *
   * @param title the incoming title
   * @return list of errors or empty list in case if title is valid
   */
  public List<Error> validateTitle(Title title) {
    List<ErrorCodes> errorCodes = new ArrayList<>(checkClaimingConfig(title.getClaimingActive(), title.getClaimingInterval()));
    errorCodes.addAll(validatePoLineNumber(title.getPoLineNumber()));
    return convertErrorCodesToErrors(errorCodes);
  }

  private List<ErrorCodes> validatePoLineNumber(String poLineNumber) {
    List<ErrorCodes> errors = new ArrayList<>();

    if (StringUtils.isNotEmpty(poLineNumber) && !POL_NUMBER_PATTERN.matcher(poLineNumber).matches()) {
      errors.add(ErrorCodes.POL_NUMBER_INVALID_OR_TOO_LONG);
    }

    return errors;
  }

  private List<Error> convertErrorCodesToErrors(List<ErrorCodes> errorCodes) {
    return errorCodes.stream().map(ErrorCodes::toError).toList();
  }
}
