package org.folio.service.settings;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.settings.util.SettingKey;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

@ExtendWith(VertxExtension.class)
public class SettingsRetrieverTest {

  @Mock
  private RestClient restClientMock;

  @Mock
  private RequestContext requestContext;

  private SettingsRetriever settingsRetriever;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    settingsRetriever = new SettingsRetriever(restClientMock);
  }

  @ParameterizedTest
  @MethodSource("testGetSettingByKeyParamProvider")
  void testGetSettingByKey(SettingCollection settingCollection, VertxTestContext vertxTestContext) {
    var settings = settingCollection.getSettings();
    var setting = settings.isEmpty() ? null : settings.get(0);
    doReturn(Future.succeededFuture(JsonObject.mapFrom(settingCollection)))
      .when(restClientMock).getAsJsonObject(any(), eq(requestContext));

    var future = settingsRetriever.getSettingByKey(SettingKey.CENTRAL_ORDERING_ENABLED, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        assertEquals(setting, result.result().orElse(null));
        vertxTestContext.completeNow();
      });
  }

  private static Stream<Arguments> testGetSettingByKeyParamProvider() {
    return Stream.of(
      Arguments.of(createSettingCollection(SettingKey.CENTRAL_ORDERING_ENABLED, "true")),
      Arguments.of(createSettingCollection(SettingKey.CENTRAL_ORDERING_ENABLED, "false")),
      Arguments.of(createSettingCollection(SettingKey.CENTRAL_ORDERING_ENABLED, null)),
      Arguments.of(new SettingCollection().withSettings(List.of()).withTotalRecords(0))
    );
  }

  private static SettingCollection createSettingCollection(SettingKey settingKey, String value) {
    return new SettingCollection()
      .withSettings(List.of(new Setting().withKey(settingKey.getName()).withValue(value)))
      .withTotalRecords(1);
  }

}
