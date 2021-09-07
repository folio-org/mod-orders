package org.folio.completablefuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.concurrent.CompletableFuture.failedFuture;
import static org.folio.completablefuture.CompletableFutureRepeater.repeat;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

class CompletableFutureRepeaterTest {

  private final static CompletableFuture<String> a = completedFuture("a");
  private final static CompletableFuture<String> b = completedFuture("b");
  private static class Fail1 extends RuntimeException {
  };
  private static class Fail2 extends RuntimeException {
  };
  private final static CompletableFuture<String> fail1 = failedFuture(new Fail1());
  private final static CompletableFuture<String> fail2 = failedFuture(new Fail2());

  @SafeVarargs
  private Supplier<CompletableFuture<String>> task(CompletableFuture<String> ...completableFutures) {
    Iterator<CompletableFuture<String>> iterator = List.of(completableFutures).iterator();
    return () -> iterator.next();
  }

  private void assertThrowsCause(Class<?> clazz, Executable executable) {
    assertThat(assertThrows(Throwable.class, executable).getCause(), is(instanceOf(clazz)));
  }

  @Test
  void fail() {
    assertThrowsCause(Fail1.class, () -> repeat(1, task(fail1, a, b)).getNow(""));
    assertThrowsCause(Fail2.class, () -> repeat(2, task(fail1, fail2, a, b)).getNow(""));
  }

  @Test
  void success() {
    assertThat(repeat(1, task(a, b)).getNow(""), is("a"));
    assertThat(repeat(2, task(a, b)).getNow(""), is("a"));
  }

  @Test
  void failSuccess() {
    assertThat(repeat(2, task(fail1, a, b)).getNow(""), is("a"));
    assertThat(repeat(3, task(fail1, a, b)).getNow(""), is("a"));
    assertThat(repeat(3, task(fail1, fail2, a, b)).getNow(""), is("a"));
    assertThat(repeat(4, task(fail1, fail2, a, b)).getNow(""), is("a"));
  }
}
