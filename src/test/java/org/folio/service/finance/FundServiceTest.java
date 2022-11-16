package org.folio.service.finance;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletionException;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Error;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class FundServiceTest {
  @InjectMocks
  private FundService fundService;
  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldRetrieveFundById() {
    //Given
    String ledgerId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    CompositeFund compositeFund = new CompositeFund().withFund(new Fund().withId(fundId).withLedgerId(ledgerId));

    doReturn(succeededFuture(compositeFund)).when(restClient).get(any(), eq(requestContext), eq(CompositeFund.class));
    //When
    Fund actFund = fundService.retrieveFundById(fundId,requestContext).result();
    //Then
    assertThat(actFund, equalTo(compositeFund.getFund()));

    ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
    verify(restClient).get(argumentCaptor.capture(), eq(requestContext), eq(CompositeFund.class));
    RequestEntry requestEntry = argumentCaptor.getValue();

    assertEquals("/finance/funds/" + fundId, requestEntry.buildEndpoint());

  }

  @Test
  void testShouldRetrieveFundsByLedgerId() throws UnsupportedEncodingException {
    //Given
    String ledgerId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    List<Fund> funds = List.of(new Fund().withId(fundId).withLedgerId(ledgerId));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = URLEncoder.encode("ledgerId==" + ledgerId, StandardCharsets.UTF_8.toString());
    doReturn(succeededFuture(fundCollection)).when(restClient).get(any(),  eq(requestContext), eq(FundCollection.class));
    //When
    List<Fund> actFund = fundService.getFundsByLedgerId(ledgerId, requestContext).result();
    //Then
    ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
    verify(restClient).get(argumentCaptor.capture(), eq(requestContext), eq(FundCollection.class));
    RequestEntry requestEntry = argumentCaptor.getValue();

    assertEquals(query, requestEntry.getQueryParams().get("query"));
    assertEquals(0, requestEntry.getQueryParams().get("offset"));
    assertEquals(Integer.MAX_VALUE, requestEntry.getQueryParams().get("limit"));
    assertThat(actFund, equalTo(fundCollection.getFunds()));
  }

  @Test
  void testShouldRetrieveFundsByFundIds() throws UnsupportedEncodingException {
    //Given
    String fundId1 = UUID.randomUUID().toString();
    String fundId2= UUID.randomUUID().toString();
    List<String> fundIds = List.of(fundId1, fundId2);
    List<Fund> funds = List.of(new Fund().withId(fundId1), new Fund().withId(fundId2));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = URLEncoder.encode("id==(" + fundId1 + " or " +  fundId2 +")", StandardCharsets.UTF_8.toString());
    doReturn(succeededFuture(fundCollection)).when(restClient).get(any(), any(),  any());
    //When
    List<Fund> actFund = fundService.getAllFunds(fundIds, requestContext).result();
    //Then
    ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
    verify(restClient).get(argumentCaptor.capture(), any(), eq(FundCollection.class));

    RequestEntry requestEntry = argumentCaptor.getValue();
    assertEquals(query, requestEntry.getQueryParams().get("query"));
    assertEquals(0, requestEntry.getQueryParams().get("offset"));
    assertEquals(MAX_IDS_FOR_GET_RQ, requestEntry.getQueryParams().get("limit"));
    assertThat(actFund, equalTo(fundCollection.getFunds()));
  }

  @Test
  void testShouldThrowHttpExceptionAsCauseIfFundNotFound() {
    //Given
    String fundId = UUID.randomUUID().toString();

    Error expError = new Error().withCode(FUNDS_NOT_FOUND.getCode()).withMessage(String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
    doThrow(new CompletionException(new HttpException(404, expError))).when(restClient).get(any(), eq(requestContext), eq(CompositeFund.class));
    //When
    CompletionException thrown = assertThrows(
      CompletionException.class,
      () -> fundService.retrieveFundById(fundId, requestContext).result(), "Expected exception"
    );
    HttpException actHttpException = (HttpException)thrown.getCause();
    Error actError = actHttpException.getError();
    assertEquals(actError.getCode(), expError.getCode());
    assertEquals(actError.getMessage(), String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
    assertEquals(404, actHttpException.getCode());
    //Then
    verify(restClient).get(any(), eq(requestContext), eq(CompositeFund.class));
  }

  @Test
  void testShouldThrowNotHttpExceptionIfFundNotFound() {
    //Given
    String fundId = UUID.randomUUID().toString();
    doThrow(new CompletionException(new RuntimeException())).when(restClient).get(any(), eq(requestContext), eq(CompositeFund.class));
    //When
    CompletionException thrown = assertThrows(
      CompletionException.class,
      () -> fundService.retrieveFundById(fundId,requestContext).result(),"Expected exception"
    );
    assertEquals(RuntimeException.class, thrown.getCause().getClass());
    //Then
    verify(restClient).get(any(), eq(requestContext), eq(CompositeFund.class));
  }
}
