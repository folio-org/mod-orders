package org.folio.service.dataexport;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import static org.folio.models.claiming.IntegrationDetailField.CONFIGS;
import static org.folio.models.claiming.IntegrationDetailField.CONFIG_NAME;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;

@Log4j2
@RequiredArgsConstructor
public class ExportConfigsRetriever {

  private final RestClient restClient;

  public Future<JsonObject> getExportConfigs(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(exportConfigs -> {
        var result = new JsonObject();
        var configs = exportConfigs.getJsonArray(CONFIGS.getValue());
        for (int i = 0; i < configs.size(); i++) {
          var exportConfig = configs.getJsonObject(i);
          result.put(getConfigName(exportConfig), exportConfig);
        }
        return result;
      });
  }

  private static String getConfigName(JsonObject exportConfig) {
    return exportConfig.getJsonObject(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue())
      .getJsonObject(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue())
      .getString(CONFIG_NAME.getValue());
  }

}
