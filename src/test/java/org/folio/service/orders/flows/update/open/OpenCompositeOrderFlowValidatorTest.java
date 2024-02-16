package org.folio.service.orders.flows.update.open;

import static org.folio.rest.core.exceptions.ErrorCodes.FUND_LOCATION_RESTRICTION_VIOLATION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
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

  /* Positive cases */
  @Test
  public void testCheckFundLocationRestrictions1(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L7");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions2(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions3(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L7");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions4(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions5(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L2");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions6(VertxTestContext vertxTestContext) {
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1", "L3")),
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions7(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L2", "L3");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of()),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.checkFundLocationRestrictions(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  /* Error cases */
  @Test
  public void testCheckFundLocationRestrictions8(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L2");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions9(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L2");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions10(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L2");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions11(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L3");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L1, L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions12(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L3");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L1", "L3")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L1, L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions13(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L3");
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
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocationIds(List.of()),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocationIds(List.of("L2"))
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
          new Parameter().withKey("restrictedLocations").withValue("[L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

}
