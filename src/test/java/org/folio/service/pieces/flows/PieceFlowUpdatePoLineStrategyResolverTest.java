package org.folio.service.pieces.flows;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_CREATE_FLOW;
import static org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_DELETE_FLOW;
import static org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_UPDATE_FLOW;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

public class PieceFlowUpdatePoLineStrategyResolverTest {
  @Autowired
  private PieceFlowUpdatePoLineStrategyResolver resolver;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  void shouldNotReturnStrategyForCreatePieceFlowAndAnyPoWorkflowWherePoLineIsPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(true).withPieceFlowType(PIECE_CREATE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(false, strategy.isPresent());
  }

  @Test
  void shouldNotReturnStrategyForDeletePieceFlowAndAnyPoWorkflowWherePoLineIsPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(true).withPieceFlowType(PIECE_DELETE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(false, strategy.isPresent());
  }

  @Test
  void shouldReturnStrategyForCreatePieceFlowAndPendingPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.PENDING)
                                                                  .withPieceFlowType(PIECE_CREATE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(true, strategy.isPresent());
    assertEquals(PieceFlowUpdatePoLineStrategies.ADD, strategy.get());
  }

  @Test
  void shouldReturnStrategyForDeletePieceFlowAndPendingPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.PENDING)
                                                                  .withPieceFlowType(PIECE_DELETE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(true, strategy.isPresent());
    assertEquals(PieceFlowUpdatePoLineStrategies.DELETE, strategy.get());
  }

  @Test
  void shouldReturnStrategyForCreatePieceFlowAndOpenPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.OPEN)
      .withPieceFlowType(PIECE_CREATE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(true, strategy.isPresent());
    assertEquals(PieceFlowUpdatePoLineStrategies.ADD, strategy.get());
  }

  @Test
  void shouldReturnStrategyForDeletePieceFlowAndOpenPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.OPEN)
      .withPieceFlowType(PIECE_DELETE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(true, strategy.isPresent());
    assertEquals(PieceFlowUpdatePoLineStrategies.DELETE, strategy.get());
  }

  @Test
  void shouldNotReturnStrategyForUpdatePieceFlowAndOpenPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.OPEN)
      .withPieceFlowType(PIECE_UPDATE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(false, strategy.isPresent());
  }

  @Test
  void shouldNotReturnStrategyForUpdatePieceFlowAndPendingPoWorkflowWherePoLineIsNotPackage() {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(false).withOrderWorkFlowStatus(WorkflowStatus.PENDING)
      .withPieceFlowType(PIECE_UPDATE_FLOW);
    Optional<PieceFlowUpdatePoLineStrategy> strategy = resolver.resolve(key);

    assertEquals(false, strategy.isPresent());
  }
}
