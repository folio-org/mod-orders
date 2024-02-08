package org.folio.service.orders.flows.update.open;

import static org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.finance.FundService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;

public class OpenCompositeOrderFlowValidator {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderFlowValidator.class);

  private final FundService fundService;
  private final ExpenseClassValidationService expenseClassValidationService;
  private final PieceStorageService pieceStorageService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final CompositePoLineValidationService compositePoLineValidationService;

  public OpenCompositeOrderFlowValidator(FundService fundService,
                                         ExpenseClassValidationService expenseClassValidationService,
                                         PieceStorageService pieceStorageService,
                                         EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                         CompositePoLineValidationService compositePoLineValidationService) {
    this.fundService = fundService;
    this.expenseClassValidationService = expenseClassValidationService;
    this.pieceStorageService = pieceStorageService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.compositePoLineValidationService = compositePoLineValidationService;
  }

  public Future<Void> validate(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                               RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();

    Future<Void> validateMaterialTypesFuture = Future.succeededFuture()
      .map(v -> {
        validateMaterialTypes(compPO);
        return null;
      });

    Future<Void> validateEncumbrancesFuture = Future.succeededFuture()
      .compose(v ->
        encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN)
          .prepareProcessEncumbrancesAndValidate(compPO, poFromStorage, requestContext)
          .mapEmpty()
      );

    futures.add(expenseClassValidationService.validateExpenseClasses(compPO.getCompositePoLines(), true, requestContext));
    futures.add(checkLocationsAndPiecesConsistency(compPO.getCompositePoLines(), requestContext));
    futures.add(validateFundDistributionTotal(compPO.getCompositePoLines()));
    futures.add(validateMaterialTypesFuture);
    futures.add(validateEncumbrancesFuture);

    return GenericCompositeFuture.join(futures)
      .mapEmpty();
  }

  private Future<Void> validateFundDistributionTotal(List<CompositePoLine> compositePoLines) {
    return Future.succeededFuture().map(v -> {
      FundDistributionUtils.validateFundDistributionTotal(compositePoLines);
      return null;
    });
  }

  public Future<Void> checkLocationsAndPiecesConsistency(List<CompositePoLine> poLines, RequestContext requestContext) {
    logger.debug("checkLocationsAndPiecesConsistency start");
    List<CompositePoLine> linesWithIdWithoutManualPieceReceived = poLines.stream().filter(
        compositePoLine -> StringUtils.isNotEmpty(compositePoLine.getId()) && Boolean.FALSE.equals(compositePoLine.getCheckinItems()))
      .collect(Collectors.toList());
    List<String> lineIds = linesWithIdWithoutManualPieceReceived.stream().map(CompositePoLine::getId).toList();
    return pieceStorageService.getPiecesByLineIdsByChunks(lineIds, requestContext)
      .map(pieces -> new PieceCollection().withPieces(pieces).withTotalRecords(pieces.size()))
      .map(pieces -> {
        verifyLocationsAndPiecesConsistency(linesWithIdWithoutManualPieceReceived, pieces);
        return null;
      });
  }

  private void validateMaterialTypes(CompositePurchaseOrder purchaseOrder) {
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      List<Error> errors = compositePoLineValidationService.checkMaterialsAvailability(purchaseOrder.getCompositePoLines());
      if (!errors.isEmpty()) {
        throw new HttpException(422, errors.get(0));
      }
    }
  }

  public Future<Void> checkFundLocationRestrictions(List<CompositePoLine> poLines, RequestContext requestContext) {
    logger.debug("checkFundLocationRestrictions start");
    List<Future<Void>> checkFunds = poLines
      .stream()
      .filter(poLine -> CollectionUtils.isNotEmpty(poLine.getLocations()))
      .filter(poLine -> CollectionUtils.isNotEmpty(poLine.getFundDistribution()))
      .map(poLine -> {
        List<String> fundIdList = poLine.getFundDistribution().stream()
          .map(FundDistribution::getFundId)
          .toList();
        return fundService.getFunds(fundIdList, requestContext)
          .compose(funds -> validateLocationRestrictions(poLine, funds));
      }).toList();
    return GenericCompositeFuture.join(checkFunds).mapEmpty();
  }

  private Future<Void> validateLocationRestrictions(CompositePoLine poLine, List<Fund> funds) {
    // TODO: will be removed in scope of https://folio-org.atlassian.net/browse/MODORDERS-981
    // as we'll obtain funds once before the processing and check on emptiness
    if (CollectionUtils.isEmpty(funds)) {
      logger.info("No funds found for PO Line {}, skipping fund-location restrictions check.", poLine.getId());
      return Future.succeededFuture();
    }

    List<String> restrictedLocations = poLine.getLocations()
      .stream()
      .map(Location::getLocationId)
      .filter(locId -> isRestrictedByAllFunds(funds, locId))
      .toList();

    if (restrictedLocations.isEmpty()) {
      return Future.succeededFuture();
    }

    String poLineId = poLine.getId();
    logger.error("For POL {} locations {} are restricted to be used by all funds", poLineId, restrictedLocations);
    List<Parameter> parameters = List.of(
      new Parameter().withKey("poLineId").withValue(poLineId),
      new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
      new Parameter().withKey("restrictedLocations").withValue(restrictedLocations.toString())
    );
    return Future.failedFuture(new HttpException(422, ErrorCodes.FUND_LOCATION_RESTRICTION_VIOLATION, parameters));
  }

  private boolean isRestrictedByAllFunds(List<Fund> funds, String locationId) {
    return funds
      .stream()
      .allMatch(fund -> Boolean.TRUE.equals(fund.getRestrictByLocations()) && !fund.getLocationIds().contains(locationId));
  }

}
