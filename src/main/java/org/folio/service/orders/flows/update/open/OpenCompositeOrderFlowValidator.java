package org.folio.service.orders.flows.update.open;

import static org.folio.orders.utils.validators.LocationsAndPiecesConsistencyValidator.verifyLocationsAndPiecesConsistency;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
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
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.finance.FundService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;

public class OpenCompositeOrderFlowValidator {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderFlowValidator.class);

  private final FundService fundService;
  private final ExpenseClassValidationService expenseClassValidationService;
  private final PieceStorageService pieceStorageService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final CompositePoLineValidationService compositePoLineValidationService;
  private final InventoryHoldingManager inventoryHoldingManager;

  public OpenCompositeOrderFlowValidator(FundService fundService,
                                         ExpenseClassValidationService expenseClassValidationService,
                                         PieceStorageService pieceStorageService,
                                         EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                         CompositePoLineValidationService compositePoLineValidationService, InventoryHoldingManager inventoryHoldingManager) {
    this.fundService = fundService;
    this.expenseClassValidationService = expenseClassValidationService;
    this.pieceStorageService = pieceStorageService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.compositePoLineValidationService = compositePoLineValidationService;
    this.inventoryHoldingManager = inventoryHoldingManager;
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
          .compose(funds -> validateLocationRestrictions(poLine, funds, requestContext));
      }).toList();
    return GenericCompositeFuture.join(checkFunds).mapEmpty();
  }

  private Future<Void> validateLocationRestrictions(CompositePoLine poLine, List<Fund> funds, RequestContext requestContext) {
    logger.debug("validateLocationRestrictions:: Validating location restrictions for poLine '{}' that has '{}' fund(s)", poLine.getId(), funds.size());
    // TODO: will be removed in scope of https://folio-org.atlassian.net/browse/MODORDERS-981
    // as we'll obtain funds once before the processing and check on emptiness
    if (CollectionUtils.isEmpty(funds)) {
      logger.info("No funds found for PO Line {}, skipping fund-location restrictions check.", poLine.getId());
      return Future.succeededFuture();
    }

    Set<Fund> restrictedFunds = funds.stream().filter(Fund::getRestrictByLocations).collect(Collectors.toSet());

    if (restrictedFunds.isEmpty()) {
      logger.info("validateLocationRestrictions:: No funds are restricted by locations");
      return Future.succeededFuture();
    }

    return extractRestrictedLocationIds(poLine, restrictedFunds, requestContext)
      .compose(restrictedLocations -> {
        if (restrictedLocations.isEmpty()) {
          logger.info("validateLocationRestrictions:: No restricted locations found for poLineId '{}'", poLine.getId());
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
      });
  }


  /**
   * The method checking fund location against valid locations and holding locations in POL to identify restricted locations
   * <br> if there is one, will be stored to put in error as parameter.
   * <br> otherwise, order can be opened
   *
   * @param poLine          poLine of order that requested to be open
   * @param restrictedFunds restricted funds of poLine
   * @param requestContext  requestContext
   * @return Restricted locations
   */
  private Future<Set<String>> extractRestrictedLocationIds(CompositePoLine poLine, Set<Fund> restrictedFunds, RequestContext requestContext) {

    String currentTenantId = TenantTool.tenantId(requestContext.getHeaders());

    var locationsForCurrentTenant = getCurrentTenantLocations(poLine, currentTenantId);
    var futures = inventoryHoldingManager.getHoldingsFutures(poLine, requestContext);

    return GenericCompositeFuture.all(new ArrayList<>(futures.values())).map(ar -> {
      var locationsFromHoldings = futures.entrySet().stream()
        .map(OpenCompositeOrderFlowValidator::mapHoldingsToLocations)
        .flatMap(List::stream)
        .toList();

      logger.info("extractRestrictedLocations:: '{}' restrictedFund(s) is being checked against locationsForCurrentTenant: {} and locationsFromHoldings: {}",
        restrictedFunds.size(), locationsForCurrentTenant, locationsFromHoldings);

      var allowedLocations = CollectionUtils.union(locationsForCurrentTenant, locationsFromHoldings);

      Set<String> restrictedLocationsIds = new HashSet<>();
      restrictedFunds.forEach(fund -> extractRestrictedFundLocations(fund, currentTenantId, allowedLocations, restrictedLocationsIds));
      return restrictedLocationsIds;
    });
  }

  private static List<String> getCurrentTenantLocations(CompositePoLine poLine, String currentTenantId) {
    return poLine.getLocations()
      .stream()
      .map(Location::getLocationId)
      .filter(Objects::nonNull)
      .map(location -> getTenantLocation(currentTenantId, location))
      .toList();
  }

  private static List<String> mapHoldingsToLocations(Map.Entry<String, Future<List<JsonObject>>> entry) {
    return entry.getValue().result()
      .stream()
      .map(holding -> getTenantLocation(entry.getKey(), holding.getString(HOLDING_PERMANENT_LOCATION_ID)))
      .toList();
  }

  private void extractRestrictedFundLocations(Fund fund, String currentTenantId,
      Collection<String> allowedLocations, Set<String> restrictedLocationsIds) {
    List<String> fundLocations = fund.getLocations()
      .stream()
      .map(location -> getTenantLocation(ObjectUtils.defaultIfNull(location.getTenantId(), currentTenantId), location.getLocationId()))
      .toList();
    if (fundLocations.stream().anyMatch(allowedLocations::contains)) {
      logger.info("extractRestrictedLocationIds:: Fund '{}' has valid location (at least one)", fund.getId());
    } else {
      restrictedLocationsIds.addAll(fundLocations);
      logger.info("extractRestrictedLocationIds:: restrictedFundLocationIds: {} for fund: {}", restrictedLocationsIds, fund.getId());
    }
  }

  private static String getTenantLocation(String tenantId, String locationId) {
    return tenantId + "." + locationId;
  }

}
