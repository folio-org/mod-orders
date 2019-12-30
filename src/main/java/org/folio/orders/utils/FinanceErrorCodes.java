package org.folio.orders.utils;

import org.folio.rest.jaxrs.model.Error;

public enum FinanceErrorCodes {
  BUDGET_NOT_FOUND_FOR_TRANSACTION("budgetNotFoundForTransaction", "Budget not found for transaction"),
  LEDGER_NOT_FOUND_FOR_TRANSACTION("ledgerNotFoundForTransaction", "Ledger not found for transaction"),
  BUDGET_IS_INACTIVE("budgetIsInactive", "Cannot create encumbrance from the not active budget"),
  FUND_CANNOT_BE_PAID("fundCannotBePaid", "Fund cannot be paid due to restrictions");

  private final String code;
  private final String description;

  FinanceErrorCodes(String code, String description) {
    this.code = code;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public String getCode() {
    return code;
  }

  @Override
  public String toString() {
    return code + ": " + description;
  }

  public Error toError() {
    return new Error().withCode(code).withMessage(description);
  }
}
