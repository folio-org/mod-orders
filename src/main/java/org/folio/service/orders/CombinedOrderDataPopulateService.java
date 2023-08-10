package org.folio.service.orders;

import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;

public class CombinedOrderDataPopulateService implements CompositeOrderDynamicDataPopulateService {

  private static final Logger logger = LogManager.getLogger(CombinedOrderDataPopulateService.class);

  private final CompositeOrderRetrieveHolderBuilder holderBuilder;
  private final Set<CompositeOrderDynamicDataPopulateService> populateServices;

  public CombinedOrderDataPopulateService(CompositeOrderRetrieveHolderBuilder holderBuilder,
                                          Set<CompositeOrderDynamicDataPopulateService> populateServices) {
    this.holderBuilder = holderBuilder;
    this.populateServices = populateServices;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder retrieveHolder, RequestContext requestContext) {
    logger.info("[ORDERS_AUDIT] CombinedOrderDataPopulateService populate");
    return holderBuilder.withCurrentFiscalYear(retrieveHolder, requestContext)
      .compose(holder -> populateAllDynamicData(holder, requestContext));
  }

  private Future<CompositeOrderRetrieveHolder> populateAllDynamicData(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    logger.info("[ORDERS_AUDIT] CombinedOrderDataPopulateService populateAllDynamicData");
    return GenericCompositeFuture.join(populateServices.stream()
      .map(service -> {
        logger.info("[ORDERS_AUDIT] populate service call");
        return service.populate(holder, requestContext);
      })
      .collect(Collectors.toList()))
      .map(aVoid -> holder);
  }
}
