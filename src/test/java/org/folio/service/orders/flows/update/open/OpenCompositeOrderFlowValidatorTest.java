package org.folio.service.orders.flows.update.open;

import static org.folio.rest.core.exceptions.ErrorCodes.FUND_LOCATION_RESTRICTION_VIOLATION;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.finance.FundService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderFlowValidatorTest {

  @Mock
  private RequestContext requestContext;
  @Mock
  private FundService fundService;
  @Mock
  private InventoryHoldingManager inventoryHoldingManager;

  @InjectMocks
  private OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  /* Wrote tests based on the table in the ticket https://folio-org.atlassian.net/browse/MODORDERS-1020 */
  @Test
  public void testCheckFundLocationRestrictions1(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      );

    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

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
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

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
    List<String> locationIds = List.of("L2");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions4(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L7");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(createLocations("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(false).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

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
    List<String> locationIds = List.of("L1");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(createLocations("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

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
    List<String> locationIds = List.of("L2");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(createLocations("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions7(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions8(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L2, folio_shared.L1]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions9(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1", "L3")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions10(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L2");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions11(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L2", "L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(List.of()),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions12(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(false).withLocations(List.of()),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("poLineId").withValue(poLine.getId()),
          new Parameter().withKey("poLineNumber").withValue(poLine.getPoLineNumber()),
          new Parameter().withKey("restrictedLocations").withValue("[folio_shared.L2]")
        );
        assertEquals(FUND_LOCATION_RESTRICTION_VIOLATION.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictions13(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L1", "L2", "L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1", "L3")),
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictionsWhenFundHasAtLeastOneValidLocation(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    List<String> locationIds = List.of("L3");
    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(
        locationIds.stream().map(id -> new Location().withLocationId(id)).toList()
      );
    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1", "L3"))
      ))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictionsWithLocationAndHoldings(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    String locationId = "L1";
    String holdingId = "297d5fbe-7994-43ae-b51d-65be761dff8b";
    JsonObject holding = JsonObject.of("permanentLocationId", "L2");

    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(List.of(
          new Location().withLocationId(locationId),
          new Location().withHoldingId(holdingId)
        )
      );

    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );
    when(inventoryHoldingManager.getHoldingsByLocationTenants(poLine, requestContext)).thenReturn(
      Map.of("folio_shared", Future.succeededFuture(List.of(holding)))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  public void testCheckFundLocationRestrictionsThrowErrorWhenHoldingNotFound(VertxTestContext vertxTestContext) {
    // given
    List<String> fundIds = List.of("F1", "F2");
    String locationId = "L1";
    String holdingId = "297d5fbe-7994-43ae-b51d-65be761dff8b";

    PoLine poLine = new PoLine()
      .withId("ID")
      .withPoLineNumber("number")
      .withFundDistribution(
        fundIds.stream().map(id -> new FundDistribution().withFundId(id)).toList()
      )
      .withLocations(List.of(
          new Location().withLocationId(locationId),
          new Location().withHoldingId(holdingId)
        )
      );

    when(fundService.getAllFunds(fundIds, requestContext)).thenReturn(
      Future.succeededFuture(List.of(
        new Fund().withId("F1").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L2")),
        new Fund().withId("F2").withCode("FC").withRestrictByLocations(true).withLocations(createLocations("L1"))
      ))
    );
    when(inventoryHoldingManager.getHoldingsByLocationTenants(poLine, requestContext)).thenReturn(
      Map.of("", Future.failedFuture(new HttpException(404, "Not found")))
    );

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(404, exception.getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  @CopilotGenerated(model = "Claude Sonnet 4")
  public void testPopulateMissingFundCodes_ShouldPopulateMissingCodes(VertxTestContext vertxTestContext) {
    // given
    String fundId1 = "fund-id-1";
    String fundId2 = "fund-id-2";
    String fundCode1 = "FUND-CODE-1";
    String fundCode2 = "FUND-CODE-2";

    List<FundDistribution> fundDistributions = List.of(
      new FundDistribution().withFundId(fundId1), // Missing code
      new FundDistribution().withFundId(fundId2).withCode(fundCode2) // Already has correct code
    );

    PoLine poLine = new PoLine()
      .withId("test-po-line-id")
      .withPoLineNumber("test-po-line-number")
      .withFundDistribution(fundDistributions)
      .withLocations(List.of(new Location().withLocationId("L1")));

    List<Fund> funds = List.of(
      new Fund().withId(fundId1).withCode(fundCode1).withRestrictByLocations(false),
      new Fund().withId(fundId2).withCode(fundCode2).withRestrictByLocations(false)
    );

    when(fundService.getAllFunds(List.of(fundId1, fundId2), requestContext))
      .thenReturn(Future.succeededFuture(funds));

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        // Verify that missing fund code was populated
        assertEquals(fundCode1, poLine.getFundDistribution().get(0).getCode());
        // Verify that existing correct fund code was preserved
        assertEquals(fundCode2, poLine.getFundDistribution().get(1).getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  @CopilotGenerated(model = "Claude Sonnet 4")
  public void testPopulateMissingFundCodes_ShouldNotPopulateWhenFundNotFound(VertxTestContext vertxTestContext) {
    // given
    String fundId1 = "fund-id-1";
    String fundId2 = "fund-id-2";
    String fundCode1 = "FUND-CODE-1";

    List<FundDistribution> fundDistributions = List.of(
      new FundDistribution().withFundId(fundId1), // Missing code
      new FundDistribution().withFundId(fundId2)  // Missing code, fund not found
    );

    PoLine poLine = new PoLine()
      .withId("test-po-line-id")
      .withPoLineNumber("test-po-line-number")
      .withFundDistribution(fundDistributions)
      .withLocations(List.of(new Location().withLocationId("L1")));

    // Only fund1 is returned, fund2 is missing
    List<Fund> funds = List.of(
      new Fund().withId(fundId1).withCode(fundCode1).withRestrictByLocations(false)
    );

    when(fundService.getAllFunds(List.of(fundId1, fundId2), requestContext))
      .thenReturn(Future.succeededFuture(funds));

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        // Verify that found fund code was populated
        assertEquals(fundCode1, poLine.getFundDistribution().get(0).getCode());
        // Verify that missing fund code remains null
        assertNull(poLine.getFundDistribution().get(1).getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  @CopilotGenerated(model = "Claude Sonnet 4")
  public void testPopulateMissingFundCodes_ShouldNotPopulateWhenFundServiceReturnsPartialResults(VertxTestContext vertxTestContext) {
    // given
    String fundId1 = "fund-id-1";
    String fundId2 = "fund-id-2";
    String fundId3 = "fund-id-3";
    String fundCode1 = "FUND-CODE-1";
    String fundCode3 = "FUND-CODE-3";

    List<FundDistribution> fundDistributions = List.of(
      new FundDistribution().withFundId(fundId1), // Missing code, fund will be found
      new FundDistribution().withFundId(fundId2), // Missing code, fund will NOT be found
      new FundDistribution().withFundId(fundId3)  // Missing code, fund will be found
    );

    PoLine poLine = new PoLine()
      .withId("test-po-line-id")
      .withPoLineNumber("test-po-line-number")
      .withFundDistribution(fundDistributions)
      .withLocations(List.of(new Location().withLocationId("L1")));

    // Service returns only fund1 and fund3, fund2 is missing from response
    List<Fund> funds = List.of(
      new Fund().withId(fundId1).withCode(fundCode1).withRestrictByLocations(false),
      new Fund().withId(fundId3).withCode(fundCode3).withRestrictByLocations(false)
    );

    when(fundService.getAllFunds(List.of(fundId1, fundId2, fundId3), requestContext))
      .thenReturn(Future.succeededFuture(funds));

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        // Verify that found fund codes were populated
        assertEquals(fundCode1, poLine.getFundDistribution().get(0).getCode());
        assertEquals(fundCode3, poLine.getFundDistribution().get(2).getCode());
        // Verify that missing fund code remains null (fund2 not found in service response)
        assertNull(poLine.getFundDistribution().get(1).getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  @CopilotGenerated(model = "Claude Sonnet 4")
  public void testPopulateMissingFundCodes_ShouldThrowErrorWhenFundCodeMismatch(VertxTestContext vertxTestContext) {
    // given
    String fundId1 = "fund-id-1";
    String correctFundCode = "CORRECT-FUND-CODE";
    String incorrectFundCode = "WRONG-FUND-CODE";

    List<FundDistribution> fundDistributions = List.of(
      new FundDistribution().withFundId(fundId1).withCode(incorrectFundCode) // Wrong code provided
    );

    PoLine poLine = new PoLine()
      .withId("test-po-line-id")
      .withPoLineNumber("test-po-line-number")
      .withFundDistribution(fundDistributions)
      .withLocations(List.of(new Location().withLocationId("L1")));

    List<Fund> funds = List.of(
      new Fund().withId(fundId1).withCode(correctFundCode).withRestrictByLocations(false)
    );

    when(fundService.getAllFunds(List.of(fundId1), requestContext))
      .thenReturn(Future.succeededFuture(funds));

    // when
    Future<Void> future = openCompositeOrderFlowValidator.validateFundsAndPopulateCodes(List.of(poLine), requestContext);

    // then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());

        // Verify error parameters contain the mismatch details
        List<Parameter> parameters = exception.getError().getParameters();
        assertTrue(parameters.stream().anyMatch(p -> "fundId".equals(p.getKey()) && fundId1.equals(p.getValue())));
        assertTrue(parameters.stream().anyMatch(p -> "expectedFundCode".equals(p.getKey()) && correctFundCode.equals(p.getValue())));
        assertTrue(parameters.stream().anyMatch(p -> "providedFundCode".equals(p.getKey()) && incorrectFundCode.equals(p.getValue())));
        assertTrue(parameters.stream().anyMatch(p -> "poLineId".equals(p.getKey()) && "test-po-line-id".equals(p.getValue())));

        vertxTestContext.completeNow();
      });
  }

  private static List<org.folio.rest.acq.model.finance.Location> createLocations(String... ids) {
    return Arrays.stream(ids).map(id -> {
      org.folio.rest.acq.model.finance.Location location = new org.folio.rest.acq.model.finance.Location();
      location.setLocationId(id);
      return location;
    }).toList();
  }

}
