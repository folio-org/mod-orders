package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class OpenToPendingEncumbranceStrategyTest {

    @InjectMocks
    private OpenToPendingEncumbranceStrategy openToPendingEncumbranceStrategy;
    @Mock
    private EncumbranceService encumbranceService;
    @Mock
    private TransactionService transactionService;
    @Mock
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
    @Mock
    private RequestContext requestContext;
    private Map<String, String> okapiHeadersMock;
    private Context ctxMock;
    private static boolean runningOnOwn;

    @BeforeAll
    static void before() throws InterruptedException, ExecutionException, TimeoutException {
      if (isVerticleNotDeployed()) {
        ApiTestSuite.before();
        runningOnOwn = true;
      }
      initSpringContext(ApplicationConfig.class);
    }

    @BeforeEach
    public void initMocks() {
      MockitoAnnotations.openMocks(this);
      ctxMock = getFirstContextFromVertx(getVertx());
      okapiHeadersMock = new HashMap<>();
      okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
      okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
      okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
      okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
      requestContext = new RequestContext(ctxMock, okapiHeadersMock);
    }

    @Test
    void  testShouldSetEncumbrancesToPending() {
      // Given
      CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
      Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);

      doReturn(succeededFuture(Collections.singletonList(encumbrance))).when(encumbranceService).getOrderEncumbrancesForCurrentFiscalYear(any(), any());

      doReturn(succeededFuture(null)).when(encumbranceService).updateEncumbrances(any(), any());

      List<EncumbranceRelationsHolder> encumbranceRelationsHolders = new ArrayList<>();
      encumbranceRelationsHolders.add(new EncumbranceRelationsHolder()
        .withOldEncumbrance(encumbrance)
        .withCurrentFiscalYearId(UUID.randomUUID().toString()));

      doReturn(new ArrayList<EncumbranceRelationsHolder>()).when(encumbranceRelationsHoldersBuilder).buildBaseHolders(any());
      doReturn(succeededFuture(new ArrayList<EncumbranceRelationsHolder>())).when(encumbranceRelationsHoldersBuilder).withFinances(any(), any());
      doReturn(succeededFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).withExistingTransactions(any(), any(), any());
      doReturn(succeededFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).prepareEncumbranceRelationsHolder(any(), any(), any());

      Map<String, List<PoLine>> mapFiscalYearsWithPoLines = new HashMap<>();
      String fiscalYearId = UUID.randomUUID().toString();
      mapFiscalYearsWithPoLines.put(fiscalYearId, Arrays.asList(new PoLine().withId(UUID.randomUUID().toString())));
      CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
      orderFromStorage.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

      doReturn(succeededFuture(mapFiscalYearsWithPoLines)).when(encumbranceRelationsHoldersBuilder).retrieveMapFiscalYearsWithPoLines(eq(order), eq(orderFromStorage), eq(requestContext));
      String compositePoLineId = UUID.randomUUID().toString();
      List<PoLine> poLines = Arrays.asList(new PoLine().withId(compositePoLineId));
      List<String> poLineIds = Arrays.asList(compositePoLineId);
      List<Transaction> allTransactions = Arrays.asList(encumbrance);
      doReturn(succeededFuture(Arrays.asList(encumbrance))).when(transactionService).getTransactionsByPoLinesIds(eq(poLineIds), eq(fiscalYearId), eq(requestContext));
      doReturn(succeededFuture(Arrays.asList(encumbrance))).when(encumbranceService).getCurrentPoLinesEncumbrances(eq(poLines), eq(fiscalYearId), eq(requestContext));
      doReturn(succeededFuture(allTransactions)).when(encumbranceService).getEncumbrancesByPoLinesFromCurrentFy(eq(mapFiscalYearsWithPoLines), eq(requestContext));

      // When
      openToPendingEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext).result();

      // Then
      assertEquals(0d, encumbrance.getAmount(), 0.0);
      assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
      assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());
      assertEquals(Encumbrance.OrderStatus.PENDING, encumbrance.getEncumbrance().getOrderStatus());
    }
}
