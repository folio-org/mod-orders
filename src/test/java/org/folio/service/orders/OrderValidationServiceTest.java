package org.folio.service.orders;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_ONGOING;
import static org.folio.rest.core.exceptions.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.rest.core.exceptions.ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.OrderType.ONE_TIME;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.OrderType.ONGOING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

public class OrderValidationServiceTest {
  static final String LISTED_PRINT_MONOGRAPH_PATH = "po_listed_print_monograph.json";
  @InjectMocks
  private OrderValidationService orderValidationService;
  @Mock
  private CompositePoLineValidationService compositePoLineValidationService;
  @Mock
  private PurchaseOrderLineHelper purchaseOrderLineHelper;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private ConfigurationEntriesCache configurationEntriesCache;
  @Mock
  private RequestContext requestContext;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);

    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).setTenantDefaultCreateInventoryValues(any(CompositePoLine.class), any(JsonObject.class));
    doReturn(succeededFuture(List.of()))
      .when(compositePoLineValidationService).validatePoLine(any(CompositePoLine.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineService).validateAndNormalizeISBN(anyList(), eq(requestContext));
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Test validateOrderForPost with Ongoing field")
  void testValidateOrderForPostWithOngoingField() {
    // Given
    JsonObject tenantConfig = new JsonObject();
    CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
    compPO.setOrderType(ONE_TIME);
    compPO.setOngoing(new Ongoing());

    // When
    Future<List<Error>> future = orderValidationService.validateOrderForPost(compPO, tenantConfig, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getCode(), is(ONGOING_NOT_ALLOWED.getCode()));
  }

  @Test
  @DisplayName("Test validateOrderForPost without Ongoing field")
  void testValidateOrderForPostWithoutOngoingField() {
    // Given
    JsonObject tenantConfig = new JsonObject();
    CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
    compPO.setOrderType(ONGOING);

    // When
    Future<List<Error>> future = orderValidationService.validateOrderForPost(compPO, tenantConfig, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getCode(), is(MISSING_ONGOING.getCode()));
  }

  @Test
  @DisplayName("Test validateOrderForPut with Ongoing field")
  void testValidateOrderForPutWithOngoingField() {
    // Given
    JsonObject tenantConfig = new JsonObject();
    CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
    compPO.setOrderType(ONE_TIME);
    compPO.setOngoing(new Ongoing());

    doReturn(succeededFuture(tenantConfig))
      .when(configurationEntriesCache).loadConfiguration(eq(ORDER_CONFIG_MODULE_NAME), eq(requestContext));

    // When
    Future<List<Error>> future = orderValidationService.validateOrderForPut(compPO.getId(), compPO, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getCode(), is(ONGOING_NOT_ALLOWED.getCode()));
  }

  @Test
  @DisplayName("Test validateOrderForPut without Ongoing field")
  void testValidateOrderForPutWithoutOngoingField() {
    // Given
    JsonObject tenantConfig = new JsonObject();
    CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
    compPO.setOrderType(ONGOING);

    doReturn(succeededFuture(tenantConfig))
      .when(configurationEntriesCache).loadConfiguration(eq(ORDER_CONFIG_MODULE_NAME), eq(requestContext));

    // When
    Future<List<Error>> future = orderValidationService.validateOrderForPut(compPO.getId(), compPO, requestContext);

    // Then
    if (future.failed()) {
      fail(future.cause());
    }
    List<Error> errors = future.result();
    assertThat(errors, hasSize(1));
    assertThat(errors.get(0).getCode(), is(MISSING_ONGOING.getCode()));
  }

}
