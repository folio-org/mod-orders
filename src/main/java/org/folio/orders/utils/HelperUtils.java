package org.folio.orders.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class HelperUtils {

  private HelperUtils() {

  }

  public static String getMockData(String path) throws IOException {
    StringBuilder sb = new StringBuilder();
    try (Stream<String> lines = Files.lines(Paths.get(path))) {
      lines.forEach(sb::append);
    }
    return sb.toString();
  }

}
