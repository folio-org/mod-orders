package org.folio.service.finance;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.RestClientTest.X_OKAPI_TENANT;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Error;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class FundServiceTest {
  @InjectMocks
  private FundService fundService;
  @Mock
  private RestClient restClient;
  private Map<String, String> okapiHeadersMock;
  private RequestContext requestContext;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(Vertx.vertx().getOrCreateContext(), okapiHeadersMock);
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testShouldRetrieveFundById(VertxTestContext vertxTestContext) {
    //Given
    String ledgerId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    CompositeFund compositeFund = new CompositeFund().withFund(new Fund().withId(fundId).withLedgerId(ledgerId));

    doReturn(succeededFuture(compositeFund)).when(restClient).get(any(RequestEntry.class), eq(CompositeFund.class), eq(requestContext));
    //When
    var future = fundService.retrieveFundById(fundId,requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        assertThat(result.result(), equalTo(compositeFund.getFund()));

        ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).get(argumentCaptor.capture(), eq(CompositeFund.class), eq(requestContext));
        RequestEntry requestEntry = argumentCaptor.getValue();

        assertEquals("/finance/funds/" + fundId, requestEntry.buildEndpoint());
        vertxTestContext.completeNow();
      });


  }

  @Test
  void testShouldRetrieveFundsByLedgerId(VertxTestContext vertxTestContext) {
    //Given
    String ledgerId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    List<Fund> funds = List.of(new Fund().withId(fundId).withLedgerId(ledgerId));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = URLEncoder.encode("ledgerId==" + ledgerId, StandardCharsets.UTF_8);
    doReturn(succeededFuture(fundCollection)).when(restClient).get(any(RequestEntry.class), eq(FundCollection.class),  eq(requestContext));
    //When
    var future = fundService.getFundsByLedgerId(ledgerId, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).get(argumentCaptor.capture(),eq(FundCollection.class), eq(requestContext));
        RequestEntry requestEntry = argumentCaptor.getValue();

        assertEquals(query, requestEntry.getQueryParams().get("query"));
        assertEquals(0, requestEntry.getQueryParams().get("offset"));
        assertEquals(Integer.MAX_VALUE, requestEntry.getQueryParams().get("limit"));
        assertThat(result.result(), equalTo(fundCollection.getFunds()));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testShouldRetrieveFundsByFundIds(VertxTestContext vertxTestContext) throws UnsupportedEncodingException {
    //Given
    String fundId1 = UUID.randomUUID().toString();
    String fundId2= UUID.randomUUID().toString();
    List<String> fundIds = List.of(fundId1, fundId2);
    List<Fund> funds = List.of(new Fund().withId(fundId1), new Fund().withId(fundId2));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = URLEncoder.encode("id==(" + fundId1 + " or " +  fundId2 +")", StandardCharsets.UTF_8);
    doReturn(succeededFuture(fundCollection)).when(restClient).get(any(RequestEntry.class), any(),  any());
    //When
    var future = fundService.getAllFunds(fundIds, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).get(argumentCaptor.capture(), eq(FundCollection.class), any());

        RequestEntry requestEntry = argumentCaptor.getValue();
        assertEquals(query, requestEntry.getQueryParams().get("query"));
        assertEquals(0, requestEntry.getQueryParams().get("offset"));
        assertEquals(MAX_IDS_FOR_GET_RQ_15, requestEntry.getQueryParams().get("limit"));
        assertThat(result.result(), equalTo(fundCollection.getFunds()));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testShouldThrowHttpExceptionAsCauseIfFundNotFound(VertxTestContext vertxTestContext) {
    //Given
    String fundId = UUID.randomUUID().toString();

    Error expError = new Error().withCode(FUNDS_NOT_FOUND.getCode()).withMessage(String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
    doReturn(failedFuture(new HttpException(404, expError))).when(restClient).get(any(RequestEntry.class), eq(CompositeFund.class), eq(requestContext));
    //When
    var future = fundService.retrieveFundById(fundId, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(thrown -> {
        HttpException actHttpException = (HttpException) thrown.cause();
        Error actError = actHttpException.getError();
        assertEquals(404, actHttpException.getCode());
        assertEquals(actError.getMessage(), String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
        //Then
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldThrowHttpExceptionAsCauseIfFundNotFoundWithGetAllFunds(VertxTestContext vertxTestContext) {
    // Given
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    List<String> fundIds = List.of(fundId1, fundId2);
    Fund fund1 = new Fund().withId(fundId1);
    FundCollection fundCollection = new FundCollection().withFunds(List.of(fund1));

    doReturn(succeededFuture(fundCollection)).when(restClient).get(any(RequestEntry.class), eq(FundCollection.class),
      eq(requestContext));

    // When
    var future = fundService.getAllFunds(fundIds, requestContext);

    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(thrown -> {
        HttpException httpException = (HttpException) thrown.cause();
        assertEquals(404, httpException.getCode());
        assertEquals(httpException.getError().getCode(), FUNDS_NOT_FOUND.getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldThrowNotHttpExceptionIfFundNotFound(VertxTestContext vertxTestContext) {
    //Given
    String fundId = UUID.randomUUID().toString();
    doReturn(failedFuture(new RuntimeException())).when(restClient).get(any(RequestEntry.class), eq(CompositeFund.class), eq(requestContext));
    //When
    var future = fundService.retrieveFundById(fundId, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(thrown ->{
        assertEquals(RuntimeException.class, thrown.cause().getCause().getClass());
        //Then
        verify(restClient).get(any(RequestEntry.class), eq(CompositeFund.class), eq(requestContext));
        vertxTestContext.completeNow();
      });

  }
}
