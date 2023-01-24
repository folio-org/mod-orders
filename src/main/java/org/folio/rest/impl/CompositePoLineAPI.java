package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
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
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.ValidateFundDistributionsRequest;
import org.folio.rest.jaxrs.resource.OrdersOrderLines;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.lines.update.OrderLinePatchOperationService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class CompositePoLineAPI extends BaseApi implements OrdersOrderLines {
  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private PurchaseOrderLineHelper helper;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;
  @Autowired
  private CompositePoLineValidationService compositePoLineValidationService;
  @Autowired
  private OrderLinePatchOperationService orderLinePatchOperationService;

  public CompositePoLineAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersOrderLines(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    helper.getOrderLines(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(lines -> asyncResultHandler.handle(succeededFuture(buildOkResponse(lines))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersOrderLines(String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
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
  public void getOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Started Invocation of POLine Request with id = {}", lineId);
    helper.getCompositePoLine(lineId, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(poLine -> {
        if (logger.isInfoEnabled()) {
          logger.info("Received PO Line Response: {}", JsonObject.mapFrom(poLine)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(poLine)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    helper.deleteLine(lineId, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Handling PUT Order Line operation...");
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
    configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> helper.setTenantDefaultCreateInventoryValues(poLine, tenantConfig))
      .compose(v -> compositePoLineValidationService.validatePoLine(poLine, requestContext))
      .onSuccess(errors::addAll)
      .onSuccess(empty -> {
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
