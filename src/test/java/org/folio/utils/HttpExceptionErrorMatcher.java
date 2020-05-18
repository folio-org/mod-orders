package org.folio.utils;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

public class HttpExceptionErrorMatcher extends TypeSafeMatcher<HttpException> {

  private Error foundError;
  private final Error expectedError;

  public static HttpExceptionErrorMatcher hasError(Error error) {
    return new HttpExceptionErrorMatcher(error);
  }

  private HttpExceptionErrorMatcher(Error error) {
    this.expectedError = error;
  }

  @Override
  protected boolean matchesSafely(HttpException item) {
    foundError = item.getError();
    return foundError.equals(expectedError);
  }

  @Override
  public void describeTo(Description description) {
    description.appendValue(foundError)
      .appendText(" was not found instead of ")
      .appendValue(expectedError);
  }
}
