package org.folio.rest.impl.crud;

import java.util.UUID;

import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.Suffix;

import io.vertx.core.json.JsonObject;

public enum CrudTestEntities {
  REASON_FOR_CLOSURE("/orders/configuration/reasons-for-closure") {
    @Override
    public JsonObject getTestSample() {
      ReasonForClosure reason = new ReasonForClosure().withId(UUID.randomUUID()
        .toString())
        .withReason("Test reason")
        .withSource(ReasonForClosure.Source.USER);
      return JsonObject.mapFrom(reason);
    }
  },
  PREFIX("/orders/configuration/prefixes") {
    @Override
    public JsonObject getTestSample() {
      Prefix prefix = new Prefix().withId(UUID.randomUUID()
        .toString())
        .withName("Test prefix")
        .withDescription("Test prefix description");
      return JsonObject.mapFrom(prefix);
    }
  },
  SUFFIX("/orders/configuration/suffixes") {
    @Override
    public JsonObject getTestSample() {
      Suffix suffix = new Suffix().withId(UUID.randomUUID()
        .toString())
        .withName("Test suffix")
        .withDescription("Test suffix description");
      return JsonObject.mapFrom(suffix);
    }
  };

  CrudTestEntities(String endpoint) {
    this.endpoint = endpoint;
  }

  private String endpoint;

  public String getEndpoint() {
    return endpoint;
  }

  public abstract JsonObject getTestSample();

}
