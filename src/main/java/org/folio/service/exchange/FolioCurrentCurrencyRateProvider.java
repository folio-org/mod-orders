package org.folio.service.exchange;

import io.vertx.core.json.JsonObject;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.javamoney.moneta.convert.ExchangeRateBuilder;
import org.javamoney.moneta.spi.AbstractRateProvider;
import org.javamoney.moneta.spi.DefaultNumberValue;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;
import java.math.BigDecimal;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.folio.service.exchange.FolioExchangeRateType.FOLIO_CURRENT;

@Log4j2
public class FolioCurrentCurrencyRateProvider extends AbstractRateProvider {

  private static final ProviderContext CONTEXT = ProviderContextBuilder.of(FOLIO_CURRENT.get(), RateType.REALTIME)
    .set("providerDescription", FOLIO_CURRENT.getDescription())
    .build();
  private static final HttpClient httpClient= HttpClient.newHttpClient();

  private static final String FOLIO_DIGIT_FRACTION = "folio.digit.fraction";
  private static final String GET_LATEST_EXCHANGE_RATE_URL = "https://api.currencyapi.com/v3/latest?apikey=%s&base_currency=%s&currencies=%s";
  private static final String CONTENT_TYPE = "Content-Type";
  private static final String APPLICATION_JSON_UTF_8 = "text/application-json;charset=UTF-8";
  private static final String apiKey = System.getenv("API_KEY");

  public FolioCurrentCurrencyRateProvider() {
    super(CONTEXT);
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery query) {
    log.info("getExchangeRate:: Using {} exchange rate provider", FOLIO_CURRENT.get());
    var fromCurrencyCode = query.getBaseCurrency();
    var toCurrencyCode = query.getCurrency();

    var exchangeRate = getExchangeRateFromApi(fromCurrencyCode.getCurrencyCode(), toCurrencyCode.getCurrencyCode());
    log.info("getExchangeRate:: Exchange rate {} -> {}: {}", fromCurrencyCode, toCurrencyCode, exchangeRate);
    var builder = new ExchangeRateBuilder(this.getExchangeContext(FOLIO_DIGIT_FRACTION));
    builder.setBase(fromCurrencyCode);
    builder.setTerm(toCurrencyCode);
    builder.setFactor(DefaultNumberValue.of(exchangeRate));

    return builder.build();
  }

  @SneakyThrows
  private BigDecimal getExchangeRateFromApi(String fromCurrencyCode, String toCurrencyCode) {
    var httpRequest = HttpRequest.newBuilder()
      .uri(new URI(String.format(GET_LATEST_EXCHANGE_RATE_URL, apiKey, fromCurrencyCode, toCurrencyCode)))
      .headers(CONTENT_TYPE, APPLICATION_JSON_UTF_8)
      .GET().build();

    var httpResponse = httpClient.send(httpRequest, HttpResponse.BodyHandlers.ofString());
    log.info("getExchangeRateFromApi:: Status code: {}, body: {}", httpResponse.statusCode() ,httpResponse.body());

    var value = new JsonObject(httpResponse.body())
      .getJsonObject(CurrencyApiResponseField.DATA.getValue())
      .getJsonObject(toCurrencyCode)
      .getString(CurrencyApiResponseField.VALUE.getValue());

    return new BigDecimal(value);
  }
}
