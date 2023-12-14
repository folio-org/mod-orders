package org.folio.service.orders.flows.update.open;

import static org.folio.rest.core.exceptions.ErrorCodes.FUND_LOCATION_RESTRICTION_VIOLATION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.finance.FundService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderFlowValidatorTest {

  @Mock
  private RequestContext requestContext;
  @Mock
  private FundService fundService;

  @InjectMocks
  private OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  public void testCheckFundLocationRestrictions(VertxTestContext vertxTestContext) {

    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L2", "L3");
    CompositePoLine poLine = new CompositePoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    Mockito.when(fundService.getFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L4")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2", "L3"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("fundId").withValue("F2"),
          new Parameter().withKey("fundCode").withValue("FC"),
          new Parameter().withKey("restrictedLocations").withValue("[L2, L3]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

}
