package org.folio.rest.impl;

import static org.folio.rest.tools.utils.TenantTool.tenantId;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import java.util.Map;
import javax.ws.rs.core.Response;
import org.folio.kafka.services.KafkaAdminClientService;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.service.OrdersKafkaTopicService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

public class ModTenantApi extends TenantAPI {

  @Autowired
  private OrdersKafkaTopicService ordersKafkaTopicService;

  public ModTenantApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void postTenant(TenantAttributes tenantAttributes, Map<String, String> headers,
                         Handler<AsyncResult<Response>> handler, Context context) {
    super.postTenant(tenantAttributes, headers, ar -> {
        if (ar.succeeded()) {
          Vertx vertx = context.owner();
          var tenantId = tenantId(headers);
          var kafkaAdminClientService = new KafkaAdminClientService(vertx);
          kafkaAdminClientService.createKafkaTopics(ordersKafkaTopicService.createTopicObjects(), tenantId);
          handler.handle(Future.succeededFuture(ar.result()));
        } else {
          handler.handle(Future.failedFuture(ar.cause()));
        }
      }, context);
  }
}
