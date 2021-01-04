package org.folio.service.finance;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletionException;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class FundServiceTest {
  @InjectMocks
  private FundService fundService;
  @Mock
  private RestClient fundStorageRestClient;

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

    doReturn(completedFuture(compositeFund)).when(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
    //When
    Fund actFund = fundService.retrieveFundById(fundId,requestContext).join();
    //Then
    assertThat(actFund, equalTo(compositeFund.getFund()));
    verify(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
  }

  @Test
  void testShouldRetrieveFundsByLedgerId() {
    //Given
    String ledgerId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    List<Fund> funds = List.of(new Fund().withId(fundId).withLedgerId(ledgerId));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = "ledgerId==" + ledgerId;
    doReturn(completedFuture(fundCollection)).when(fundStorageRestClient).get(query, 0, Integer.MAX_VALUE,  requestContext, FundCollection.class);
    //When
    List<Fund> actFund = fundService.getFundsByLedgerId(ledgerId, requestContext).join();
    //Then
    verify(fundStorageRestClient).get(eq(query), eq(0), eq(Integer.MAX_VALUE), eq(requestContext), eq(FundCollection.class));
    assertThat(actFund, equalTo(fundCollection.getFunds()));
  }

  @Test
  void testShouldRetrieveFundsByFundIds() {
    //Given
    String fundId1 = UUID.randomUUID().toString();
    String fundId2= UUID.randomUUID().toString();
    List<String> fundIds = List.of(fundId1, fundId2);
    List<Fund> funds = List.of(new Fund().withId(fundId1), new Fund().withId(fundId2));
    FundCollection fundCollection = new FundCollection().withFunds(funds).withTotalRecords(1);

    String query = "id==(" + fundId1 + " or " +  fundId2 +")";
    doReturn(completedFuture(fundCollection)).when(fundStorageRestClient).get(anyString(), anyInt(), anyInt(), any(),  any());
    //When
    List<Fund> actFund = fundService.getFunds(fundIds, requestContext).join();
    //Then
    verify(fundStorageRestClient).get(eq(query), eq(0), eq(MAX_IDS_FOR_GET_RQ), eq(requestContext), eq(FundCollection.class));
    assertThat(actFund, equalTo(fundCollection.getFunds()));
  }

  @Test
  void testShouldThrowHttpExceptionAsCauseIfFundNotFound() {
    //Given
    String fundId = UUID.randomUUID().toString();

    Error expError = new Error().withCode(FUNDS_NOT_FOUND.getCode()).withMessage(String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
    doThrow(new CompletionException(new HttpException(404, expError))).when(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
    //When
    CompletionException thrown = assertThrows(
      CompletionException.class,
      () -> fundService.retrieveFundById(fundId,requestContext).join(),      "Expected exception"
    );
    HttpException actHttpException = (HttpException)thrown.getCause();
    Error actError = actHttpException.getError();
    assertEquals(actError.getCode(), expError.getCode());
    assertEquals(actError.getMessage(), String.format(FUNDS_NOT_FOUND.getDescription(), fundId));
    assertEquals(404, actHttpException.getCode());
    //Then
    verify(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
  }

  @Test
  void testShouldThrowNotHttpExceptionIfFundNotFound() {
    //Given
    String fundId = UUID.randomUUID().toString();
    doThrow(new CompletionException(new RuntimeException())).when(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
    //When
    CompletionException thrown = assertThrows(
      CompletionException.class,
      () -> fundService.retrieveFundById(fundId,requestContext).join(),"Expected exception"
    );
    assertEquals(RuntimeException.class, thrown.getCause().getClass());
    //Then
    verify(fundStorageRestClient).getById(fundId, requestContext, CompositeFund.class);
  }
}
