package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_BUSINESS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.ValidateFundDistributionsRequest;
import org.folio.rest.jaxrs.resource.OrdersOrderLines;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.orders.PoLineValidationService;
import org.folio.service.orders.lines.update.OrderLinePatchOperationService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class PoLineAPI extends BaseApi implements OrdersOrderLines {
  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private PurchaseOrderLineHelper helper;
  @Autowired
  private CommonSettingsCache commonSettingsCache;
  @Autowired
  private PoLineValidationService poLineValidationService;
  @Autowired
  private OrderLinePatchOperationService orderLinePatchOperationService;

  public PoLineAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersOrderLines(String totalRecords, int offset, int limit, String query, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    helper.getOrderLines(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(lines -> asyncResultHandler.handle(succeededFuture(buildOkResponse(lines))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersOrderLines(PoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    commonSettingsCache.loadSettings(requestContext)
      .compose(tenantConfig -> helper.createPoLine(poLine, tenantConfig, requestContext))
      .onSuccess(pol -> {
        String okapiUrl = okapiHeaders.get(OKAPI_URL);
        String url = resourceByIdPath(PO_LINES_BUSINESS, poLine.getId());
        asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiUrl, url, poLine)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getOrdersOrderLinesById(String lineId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("Started Invocation of POLine Request with id = {}", lineId);
    helper.getPoLine(lineId, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(poLine -> asyncResultHandler.handle(succeededFuture(buildOkResponse(poLine))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteOrdersOrderLinesById(String lineId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    helper.deleteLine(lineId, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, PoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("Handling PUT Order Line operation...");
    // Set id if this is available only in path
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(lineId);
    }

    // First validate content of the PO Line and proceed only if all is okay
    List<Error> errors = new ArrayList<>();

    if (!lineId.equals(poLine.getId())) {
      errors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    }
    commonSettingsCache.loadSettings(requestContext)
      .compose(tenantConfig -> helper.setTenantDefaultCreateInventoryValues(poLine, tenantConfig))
      .compose(v -> poLineValidationService.validatePoLine(poLine, requestContext))
      .map(errors::addAll)
      .onSuccess(b -> {
        if (!errors.isEmpty()) {
          PutOrdersOrderLinesByIdResponse response = PutOrdersOrderLinesByIdResponse
            .respond422WithApplicationJson(new Errors().withErrors(errors));
          asyncResultHandler.handle(succeededFuture(response));
          return;
        }
        helper.updateOrderLine(poLine, requestContext)
          .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
          .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void patchOrdersOrderLinesById(String lineId,  PatchOrderLineRequest request,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);

    orderLinePatchOperationService.patch(lineId, request, requestContext)
        .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
        .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  public void putOrdersOrderLinesFundDistributionsValidate(ValidateFundDistributionsRequest request, Map<String, String> okapiHeaders,
                                                           Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      Cost cost = request.getCost();
      cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
      FundDistributionUtils.validateFundDistributionForPoLine(cost, request.getFundDistribution());
      asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
    } catch (HttpException e) {
      handleErrorResponse(asyncResultHandler, e);
    }
  }
}
