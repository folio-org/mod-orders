package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.Objects;
import lombok.experimental.UtilityClass;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;

@UtilityClass
public class PieceUpdateFlowUtil {

  static ItemRecreateConfig constructItemRecreateSrcConfig(CompositePoLine poLine, RequestContext requestContext) {
    var locations = poLine.getLocations();
    if (locations.isEmpty()) {
      return new ItemRecreateConfig(null,  requestContext);
    }

    var location = locations.get(0);
    if (Objects.isNull(location.getTenantId())) {
      return new ItemRecreateConfig(null, requestContext);
    }

    var srcTenantId = location.getTenantId();
    return new ItemRecreateConfig(srcTenantId, createContextWithNewTenantId(requestContext, srcTenantId));
  }

  static ItemRecreateConfig constructItemRecreateDstConfig(Piece pieceToUpdate, RequestContext requestContext) {
    if (Objects.isNull(pieceToUpdate.getReceivingTenantId())) {
      return new ItemRecreateConfig(null, null);
    }

    var dstTenantId = pieceToUpdate.getReceivingTenantId();
    return new ItemRecreateConfig(dstTenantId, createContextWithNewTenantId(requestContext, dstTenantId));
  }

  static boolean allowItemRecreate(ItemRecreateConfig srcConfig, ItemRecreateConfig dstConfig) {
    return Objects.nonNull(srcConfig.tenantId()) && Objects.nonNull(dstConfig.tenantId())
      && !srcConfig.tenantId().equals(dstConfig.tenantId());
  }

  public record ItemRecreateConfig(String tenantId, RequestContext context) {}
}
