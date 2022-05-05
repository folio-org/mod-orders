package org.folio.service.orders.flows.update.open;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;

public class OpenCompositeOrderFlowValidator {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderFlowValidator.class);

  private final ExpenseClassValidationService expenseClassValidationService;
  private final PieceStorageService pieceStorageService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final CompositePoLineValidationService compositePoLineValidationService;

  public OpenCompositeOrderFlowValidator(ExpenseClassValidationService expenseClassValidationService,
    PieceStorageService pieceStorageService, EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
    CompositePoLineValidationService compositePoLineValidationService) {
    this.expenseClassValidationService = expenseClassValidationService;
    this.pieceStorageService = pieceStorageService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.compositePoLineValidationService = compositePoLineValidationService;
  }

  public CompletableFuture<Void> validate(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                                          RequestContext requestContext) {
    return expenseClassValidationService.validateExpenseClasses(compPO.getCompositePoLines(), true, requestContext)
      .thenCompose(v -> checkLocationsAndPiecesConsistency(compPO.getCompositePoLines(), requestContext))
      .thenAccept(v -> FundDistributionUtils.validateFundDistributionTotal(compPO.getCompositePoLines()))
      .thenApply(v -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN))
      .thenCompose(strategy -> strategy.prepareProcessEncumbrancesAndValidate(compPO, poFromStorage, requestContext))
      .thenAccept(v -> validateMaterialTypes(compPO));
  }

  public CompletableFuture<Void> checkLocationsAndPiecesConsistency(List<CompositePoLine> poLines, RequestContext requestContext) {
    logger.debug("checkLocationsAndPiecesConsistency start");
    List<CompositePoLine> linesWithIdWithoutManualPieceReceived = poLines.stream().filter(
        compositePoLine -> StringUtils.isNotEmpty(compositePoLine.getId()) && Boolean.FALSE.equals(compositePoLine.getCheckinItems()))
      .collect(Collectors.toList());
    List<String> lineIds = linesWithIdWithoutManualPieceReceived.stream().map(CompositePoLine::getId).collect(toList());
    return pieceStorageService.getPiecesByLineIdsByChunks(lineIds, requestContext)
      .thenApply(pieces -> new PieceCollection().withPieces(pieces).withTotalRecords(pieces.size()))
      .thenAccept(pieces -> verifyLocationsAndPiecesConsistency(linesWithIdWithoutManualPieceReceived, pieces));
  }

  private CompositePurchaseOrder validateMaterialTypes(CompositePurchaseOrder purchaseOrder){
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      List<Error> errors = compositePoLineValidationService.checkMaterialsAvailability(purchaseOrder.getCompositePoLines());
      if (!errors.isEmpty()) {
        throw new HttpException(422, errors.get(0));
      }
    }
    return purchaseOrder;
  }
}
