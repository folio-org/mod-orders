package org.folio.service.finance.expenceclass;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.ID;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_EXPENSE_CLASS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.INACTIVE_EXPENSE_CLASS;
import static org.folio.service.finance.transaction.EncumbranceService.EXPENSE_CLASS_NAME;
import static org.folio.service.finance.transaction.EncumbranceService.FUND_CODE;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.BudgetExpenseClass;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;

public class ExpenseClassValidationService {

  private final BudgetExpenseClassService budgetExpenseClassService;
  private final ExpenseClassService expenseClassService;

  public ExpenseClassValidationService(BudgetExpenseClassService budgetExpenseClassService,
      ExpenseClassService expenseClassService) {
    this.budgetExpenseClassService = budgetExpenseClassService;
    this.expenseClassService = expenseClassService;
  }

  public Future<Void> validateExpenseClassesForOpenedOrder(CompositePurchaseOrder compOrder,
      List<PoLine> poLines, RequestContext requestContext) {
    if (compOrder.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.OPEN) {
      return validateExpenseClasses(poLines, true, requestContext);
    }
    return Future.succeededFuture();
  }

  public Future<Void> validateExpenseClasses(List<PoLine> poLines,
      boolean isActiveExpenseClassCheckRequired, RequestContext requestContext) {
    Map<FundDistribution, String> expenseClassesByFundId = poLines.stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream())
      .distinct()
      .filter(fundDistribution -> Objects.nonNull(fundDistribution.getExpenseClassId()))
      .collect(toMap(Function.identity(), FundDistribution::getExpenseClassId));

    return GenericCompositeFuture.join(expenseClassesByFundId.entrySet()
      .stream()
      .map(expenseClassByFundId -> checkExpenseClassIsActiveByFundDistribution(expenseClassByFundId, isActiveExpenseClassCheckRequired, requestContext))
      .collect(toList()))
      .mapEmpty();
  }

  public Future<Void> checkExpenseClassIsActiveByFundDistribution(
      Map.Entry<FundDistribution, String> expenseClassByFundId,
      boolean isActiveExpenseClassCheckRequired, RequestContext requestContext) {
    String query = String.format("budget.fundId==%s and budget.budgetStatus==Active", expenseClassByFundId.getKey()
      .getFundId());
    return budgetExpenseClassService.getBudgetExpenseClasses(query, 0, Integer.MAX_VALUE, requestContext)
      .compose(budgetExpenseClasses -> {
        var budgetExpenseClassIdsList = budgetExpenseClasses.getBudgetExpenseClasses()
          .stream()
          .map(BudgetExpenseClass::getExpenseClassId)
          .collect(toList());

        if (budgetExpenseClassIdsList.contains(expenseClassByFundId.getValue())) {
          var hasInactiveExpenseClass = budgetExpenseClasses.getBudgetExpenseClasses()
            .stream()
            .filter(budgetExpenseClass -> expenseClassByFundId.getValue()
              .contains(budgetExpenseClass.getExpenseClassId()))
            .anyMatch(expenseClass -> BudgetExpenseClass.Status.INACTIVE.equals(expenseClass.getStatus()));

          if (isActiveExpenseClassCheckRequired && hasInactiveExpenseClass) {
            return getFundIdExpenseClassIdParameters(expenseClassByFundId, requestContext).map(parameters -> {
              throw new HttpException(400, INACTIVE_EXPENSE_CLASS.toError()
                .withParameters(parameters));
            });
          }

        } else {
          return getFundIdExpenseClassIdParameters(expenseClassByFundId, requestContext).map(parameters -> {
            throw new HttpException(400, BUDGET_EXPENSE_CLASS_NOT_FOUND.toError()
              .withParameters(parameters));
          });
        }
        return Future.succeededFuture();
      });
  }

  private Future<List<Parameter>> getFundIdExpenseClassIdParameters(Map.Entry<FundDistribution, String> expenseClassByFundId,
      RequestContext requestContext) {
    String query = ID + "==" + expenseClassByFundId.getValue();
    List<Parameter> parameters = new ArrayList<>();
    parameters.add(new Parameter().withKey(FUND_CODE)
      .withValue(expenseClassByFundId.getKey()
        .getCode()));

    return expenseClassService.getExpenseClasses(query, 0, Integer.MAX_VALUE, requestContext)
      .map(expenseClasses -> {
        expenseClasses.getExpenseClasses()
          .forEach(exc -> parameters.add(new Parameter().withKey(EXPENSE_CLASS_NAME)
            .withValue(exc.getName())));
        return parameters;
      });
  }
}
