package org.folio.orders.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.CompletionException;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.tools.client.Response;

import io.vertx.core.json.JsonObject;

public class HelperUtils {

  private HelperUtils() {

  }

  public static String getMockData(String path) throws IOException {
    try {
      return IOUtils.toString(HelperUtils.class.getClassLoader().getResourceAsStream(path));
    } catch (NullPointerException e) {
      StringBuilder sb = new StringBuilder();
      try (Stream<String> lines = Files.lines(Paths.get(path))) {
        lines.forEach(sb::append);
      }
      return sb.toString();
    }
  }
  
  public static JsonObject verifyAndExtractBody(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
          new HttpException(response.getCode(), response.getError().getString("errorMessage")));
    }

    return response.getBody();
  }

}
