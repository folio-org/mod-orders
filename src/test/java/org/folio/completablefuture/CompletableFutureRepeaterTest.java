package org.folio.completablefuture;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.completablefuture.VertxFutureRepeater.repeat;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Iterator;
import java.util.List;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import io.vertx.core.Future;

class CompletableFutureRepeaterTest {

  private final static Future<String> a = succeededFuture("a");
  private final static Future<String> b = succeededFuture("b");
  private static class Fail1 extends RuntimeException {
  };
  private static class Fail2 extends RuntimeException {
  };
  private final static Future<String> fail1 = failedFuture(new Fail1());
  private final static Future<String> fail2 = failedFuture(new Fail2());

  @SafeVarargs
  private Supplier<Future<String>> task(Future<String> ...completableFutures) {
    Iterator<Future<String>> iterator = List.of(completableFutures).iterator();
    return iterator::next;
  }

  private void assertThrowsCause(Class<?> clazz, Executable executable) {
    assertThat(assertThrows(Throwable.class, executable).getCause(), is(instanceOf(clazz)));
  }

  @Test
  void fail() {
    assertThrowsCause(Fail1.class, () -> repeat(1, task(fail1, a, b)).result());
    assertThrowsCause(Fail2.class, () -> repeat(2, task(fail1, fail2, a, b)).result());
  }

  @Test
  void success() {
    assertThat(repeat(1, task(a, b)).result(), is("a"));
    assertThat(repeat(2, task(a, b)).result(), is("a"));
  }

  @Test
  void failSuccess() {
    assertThat(repeat(2, task(fail1, a, b)).result(), is("a"));
    assertThat(repeat(3, task(fail1, a, b)).result(), is("a"));
    assertThat(repeat(3, task(fail1, fail2, a, b)).result(), is("a"));
    assertThat(repeat(4, task(fail1, fail2, a, b)).result(), is("a"));
  }
}
