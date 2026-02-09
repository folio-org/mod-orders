package org.folio.service.titles;

import static org.folio.rest.core.exceptions.ErrorCodes.POL_NUMBER_INVALID_OR_TOO_LONG;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Title;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Title Validation Service Unit Tests")
class TitleValidationServiceTest {

  private TitleValidationService titleValidationService;

  @BeforeEach
  void setUp() {
    titleValidationService = new TitleValidationService();
  }

  @Test
  @DisplayName("Should pass validation for valid POL number")
  void testValidPoLineNumber() {
    Title title = new Title().withPoLineNumber("ABC123-1");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }

  @Test
  @DisplayName("Should pass validation for POL number with 22 character prefix")
  void testPoLineNumberWith22CharPrefix() {
    Title title = new Title().withPoLineNumber("1234567890123456789012-999");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }

  @Test
  @DisplayName("Should fail validation when POL number prefix exceeds 22 characters")
  void testPoLineNumberPrefixTooLong() {
    // Given - 23 character prefix
    Title title = new Title().withPoLineNumber("12345678901234567890123-1");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, hasSize(1));
    assertEquals(POL_NUMBER_INVALID_OR_TOO_LONG.toError().getCode(), errors.get(0).getCode());
    assertEquals("POL number is invalid or bigger than 26 symbols", errors.get(0).getMessage());
  }

  @Test
  @DisplayName("Should fail validation for POL number with invalid format (no dash)")
  void testPoLineNumberNoDash() {
    Title title = new Title().withPoLineNumber("ABC1231");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, hasSize(1));
    assertEquals(POL_NUMBER_INVALID_OR_TOO_LONG.toError().getCode(), errors.get(0).getCode());
  }

  @Test
  @DisplayName("Should fail validation for POL number with special characters")
  void testPoLineNumberWithSpecialCharacters() {
    Title title = new Title().withPoLineNumber("ABC!@#-1");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, hasSize(1));
    assertEquals(POL_NUMBER_INVALID_OR_TOO_LONG.toError().getCode(), errors.get(0).getCode());
  }

  @Test
  @DisplayName("Should fail validation when POL suffix is non-numeric")
  void testPoLineNumberNonNumericSuffix() {
    Title title = new Title().withPoLineNumber("ABC123-ABC");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, hasSize(1));
    assertEquals(POL_NUMBER_INVALID_OR_TOO_LONG.toError().getCode(), errors.get(0).getCode());
  }

  @Test
  @DisplayName("Should fail validation when POL suffix exceeds 3 digits")
  void testPoLineNumberSuffixTooLong() {
    Title title = new Title().withPoLineNumber("ABC123-1234");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, hasSize(1));
    assertEquals(POL_NUMBER_INVALID_OR_TOO_LONG.toError().getCode(), errors.get(0).getCode());
  }

  @Test
  @DisplayName("Should pass validation for null POL number")
  void testNullPoLineNumber() {
    Title title = new Title().withPoLineNumber(null);
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }

  @Test
  @DisplayName("Should pass validation for empty POL number")
  void testEmptyPoLineNumber() {
    Title title = new Title().withPoLineNumber("");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }

  @Test
  @DisplayName("Should pass validation for POL number with minimum length")
  void testPoLineNumberMinimumLength() {
    Title title = new Title().withPoLineNumber("A-1");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }

  @Test
  @DisplayName("Should pass validation for POL number with alphanumeric prefix")
  void testPoLineNumberAlphanumericPrefix() {
    Title title = new Title().withPoLineNumber("Mon123Ogr456-999");
    List<Error> errors = titleValidationService.validateTitle(title);
    assertThat(errors, is(empty()));
  }
}
