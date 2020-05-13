package org.folio.utils;

import org.folio.orders.rest.exceptions.HttpException;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

public class HttpExceptionCodeMatcher extends TypeSafeMatcher<HttpException> {

  private int foundErrorCode;
  private final int expectedErrorCode;

  public static HttpExceptionCodeMatcher hasCode(int item) {
    return new HttpExceptionCodeMatcher(item);
  }

  private HttpExceptionCodeMatcher(int expectedErrorCode) {
    this.expectedErrorCode = expectedErrorCode;
  }

  @Override
  protected boolean matchesSafely(HttpException item) {
    foundErrorCode = item.getCode();
    return foundErrorCode == expectedErrorCode;
  }

  @Override
  public void describeTo(Description description) {
    description.appendValue(foundErrorCode)
      .appendText(" was not found instead of ")
      .appendValue(expectedErrorCode);
  }
}
