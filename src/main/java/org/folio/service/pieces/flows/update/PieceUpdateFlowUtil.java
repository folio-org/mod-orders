package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.Objects;
import lombok.experimental.UtilityClass;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;

@UtilityClass
public class PieceUpdateFlowUtil {

  static ItemRecreateConfig constructItemRecreateConfig(Piece piece, RequestContext requestContext, boolean reuseInitialRequestContext) {
    if (Objects.isNull(piece.getReceivingTenantId())) {
      return new ItemRecreateConfig(null, reuseInitialRequestContext ? requestContext : null);
    }

    var tenantId = piece.getReceivingTenantId();
    return new ItemRecreateConfig(tenantId, createContextWithNewTenantId(requestContext, tenantId));
  }

  static boolean allowItemRecreate(ItemRecreateConfig srcConfig, ItemRecreateConfig dstConfig) {
    return Objects.nonNull(srcConfig.tenantId()) && Objects.nonNull(dstConfig.tenantId())
      && !srcConfig.tenantId().equals(dstConfig.tenantId());
  }

  public record ItemRecreateConfig(String tenantId, RequestContext context) {}
}
