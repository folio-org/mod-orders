package org.folio.service.orders.lines.update;

import io.vertx.core.Context;
import org.apache.commons.lang.NotImplementedException;
import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.orders.lines.update.instance.WithoutHoldingOrderLineUpdateInstanceStrategy;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.junit.jupiter.api.Assertions.*;

public class OrderLineUpdateInstanceHandlerTest {
  @Autowired
  private OrderLineUpdateInstanceHandler orderLineUpdateInstanceHandler;

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(OrderLineUpdateInstanceHandlerTest.ContextConfiguration.class);
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  public void shouldThrowNotImplementedExceptionForMIXOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
        withId(orderLineId).
        withOrderFormat(PoLine.OrderFormat.P_E_MIX)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF);

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    assertThrows(NotImplementedException.class, () ->
      orderLineUpdateInstanceHandler.handle(orderLineUpdateInstanceHolder, requestContext));

  }

  @Test
  public void shouldThrowNotImplementedExceptionForPhysicalOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF);

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    assertThrows(NotImplementedException.class, () ->
        orderLineUpdateInstanceHandler.handle(orderLineUpdateInstanceHolder, requestContext));

  }

  @Test
  public void shouldThrowNotImplementedExceptionForEresourceOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
        .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF);

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    assertThrows(NotImplementedException.class, () ->
        orderLineUpdateInstanceHandler.handle(orderLineUpdateInstanceHolder, requestContext));

  }

  @Test
  public void shouldThrowNotImplementedExceptionForOtherOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.OTHER)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF);

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    assertThrows(NotImplementedException.class, () ->
        orderLineUpdateInstanceHandler.handle(orderLineUpdateInstanceHolder, requestContext));

  }

  static class ContextConfiguration {
    @Bean RestClient restClient() {
      return new RestClient();
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService(RestClient restClient) {
      return new PurchaseOrderLineService(restClient);
    }

    @Bean OrderLinePatchOperationService orderLinePatchOperationService(
        OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
        PurchaseOrderLineService purchaseOrderLineService) {
      return new OrderLinePatchOperationService(orderLinePatchOperationHandlerResolver, purchaseOrderLineService);
    }

    @Bean PatchOperationHandler orderLineUpdateInstanceHandler(
        OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver) {
      return new OrderLineUpdateInstanceHandler(updateInstanceStrategyResolver);
    }

    @Bean OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver(
        PatchOperationHandler orderLineUpdateInstanceHandler) {
      Map<PatchOrderLineRequest.Operation, PatchOperationHandler> handlers = new HashMap<>();
      handlers.put(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF, orderLineUpdateInstanceHandler);
      return new OrderLinePatchOperationHandlerResolver(handlers);
    }

    @Bean OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy() {
      return new WithHoldingOrderLineUpdateInstanceStrategy();
    }

    @Bean OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy() {
      return new WithoutHoldingOrderLineUpdateInstanceStrategy();
    }

    @Bean OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver(OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy,
        OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy) {
      Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies = new HashMap<>();

      strategies.put(CreateInventoryType.INSTANCE_HOLDING_ITEM, withHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.INSTANCE_HOLDING, withHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.INSTANCE, withoutHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.NONE, withoutHoldingOrderLineUpdateInstanceStrategy);

      return new OrderLineUpdateInstanceStrategyResolver(strategies);
    }
  }
}