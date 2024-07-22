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
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PO_LINE_JSON;
import static org.junit.jupiter.api.Assertions.assertEquals;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
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
  void testCreateItemRecreateSrcConfigWithLocationAndWithUniversityTenantId() throws IOException {
    var exceptedSrcTenantId = "university";

    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);

    assertEquals(exceptedSrcTenantId, srcConfig.tenantId());
    assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testCreateItemRecreateSrcConfigWithoutLocation() throws IOException {
    var exceptedSrcTenantId = TenantTool.tenantId(requestContext.getHeaders());

    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    poLine.setLocations(List.of());
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);

    Assertions.assertNull(srcConfig.tenantId());
    Assertions.assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testCreateItemRecreateSrcConfigWithLocationAndWithoutTenantId() throws IOException {
    var exceptedSrcTenantId = TenantTool.tenantId(requestContext.getHeaders());

    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    poLine.getLocations().get(0).setTenantId(null);
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);

    Assertions.assertNull(srcConfig.tenantId());
    Assertions.assertEquals(exceptedSrcTenantId, TenantTool.tenantId(srcConfig.context().getHeaders()));
  }

  @Test
  void testCreateEscItemRecreateDstConfigWithPieceReceivingTenantId() throws IOException {
    var exceptedDstTenantId = "college";

    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    var dstConfig = PieceUpdateFlowUtil.createItemRecreateDstConfig(piece, requestContext);

    Assertions.assertEquals(exceptedDstTenantId, dstConfig.tenantId());
    Assertions.assertEquals(exceptedDstTenantId, TenantTool.tenantId(dstConfig.context().getHeaders()));
  }

  @Test
  void testCreateEscItemRecreateDstConfigWithoutPieceReceivingTenantId() throws IOException {
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    piece.setReceivingTenantId(null);
    var dstConfig = PieceUpdateFlowUtil.createItemRecreateDstConfig(piece, requestContext);

    Assertions.assertNull(dstConfig.tenantId());
    Assertions.assertNull(dstConfig.context());
  }

  @Test
  void testAllowItemRecreationTrue() throws IOException {
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);
    var dstConfig = PieceUpdateFlowUtil.createItemRecreateDstConfig(piece, requestContext);

    Assertions.assertTrue(PieceUpdateFlowUtil.allowItemRecreation(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreationFalseSameTenant() throws IOException {
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    piece.setReceivingTenantId(poLine.getLocations().get(0).getTenantId());
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);
    var dstConfig = PieceUpdateFlowUtil.createItemRecreateDstConfig(piece, requestContext);

    Assertions.assertFalse(PieceUpdateFlowUtil.allowItemRecreation(srcConfig, dstConfig));
  }

  @Test
  void testAllowItemRecreationFalseNullTenant() throws IOException {
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var poLine = new JsonObject(poLineMock).mapTo(CompositePoLine.class);
    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    piece.setReceivingTenantId(null);
    var srcConfig = PieceUpdateFlowUtil.createItemRecreateSrcConfig(poLine, requestContext);
    var dstConfig = PieceUpdateFlowUtil.createItemRecreateDstConfig(piece, requestContext);

    Assertions.assertFalse(PieceUpdateFlowUtil.allowItemRecreation(srcConfig, dstConfig));
  }
}
