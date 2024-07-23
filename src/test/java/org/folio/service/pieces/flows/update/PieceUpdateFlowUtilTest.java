package org.folio.service.pieces.flows.update;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PIECES_JSON;
import static org.junit.jupiter.api.Assertions.assertEquals;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.utils.TenantTool;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

class PieceUpdateFlowUtilTest {

  @Mock
  private Map<String, String> okapiHeadersMock;
  private final Context ctx = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);

    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();

      runningOnOwn = true;
    }
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
  void testConstructItemRecreateConfigSrcWithReceivingTenantId() throws IOException {
    var exceptedSrcTenantId = "university";

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);

    assertEquals(exceptedSrcTenantId, srcConfig.tenantId());
    assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigSrcWithNullReceivingTenantId() throws IOException {
    var exceptedSrcTenantId = TenantTool.tenantId(requestContext.getHeaders());

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);

    Assertions.assertNull(srcConfig.tenantId());
    Assertions.assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigDstWithReceivingTenantId() throws IOException {
    var exceptedDstTenantId = "college";

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(exceptedDstTenantId);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, false);

    Assertions.assertEquals(exceptedDstTenantId, dstConfig.tenantId());
    Assertions.assertEquals(exceptedDstTenantId, TenantTool.tenantId(dstConfig.context().getHeaders()));
  }

  @Test
  void testConstructItemRecreateConfigDstWithNullReceivingTenantId() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, false);

    Assertions.assertNull(dstConfig.tenantId());
    Assertions.assertNull(dstConfig.context());
  }

  @Test
  void testAllowItemRecreateTrue() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId("college");

    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesRequest, requestContext, false);

    Assertions.assertTrue(PieceUpdateFlowUtil.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithSameTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId("university");

    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesRequest, requestContext, false);

    Assertions.assertFalse(PieceUpdateFlowUtil.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithNullSrcTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);

    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesRequest, requestContext, false);

    Assertions.assertFalse(PieceUpdateFlowUtil.allowItemRecreate(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreateFalseWithNullDstTenant() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class).withReceivingTenantId(null);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class);

    var srcConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesStorage, requestContext, true);
    var dstConfig = PieceUpdateFlowUtil.constructItemRecreateConfig(piecesRequest, requestContext, false);

    Assertions.assertFalse(PieceUpdateFlowUtil.allowItemRecreate(srcConfig, dstConfig));
  }
}
