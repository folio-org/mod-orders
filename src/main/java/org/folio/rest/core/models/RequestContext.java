package org.folio.rest.core.models;

import java.util.Collections;
import java.util.Map;

import io.vertx.core.Context;

public class RequestContext {
  private Context context;
  private Map<String, String> headers;

  public RequestContext(Context context, Map<String, String> headers) {
    this.context = context;
    this.headers = headers;
  }

  public void withContext(Context context) {
    this.context = context;
  }

  public void withHeaders(Map<String, String> headers) {
    this.headers = headers;
  }

  public Context getContext() {
    return context;
  }

  public Map<String, String> getHeaders() {
    return Collections.unmodifiableMap(headers);
  }
}
