package org.folio.utils.iterators;

import io.vertx.core.Future;

import java.util.function.Function;

import static io.vertx.core.Future.succeededFuture;

/**
 * A FutureIterator returning results from applying a function to an input future iterator.
 * Skips the entry if the result of the function is null.
 */
public class AppliedFutureIterator<T,R> extends FutureIterator<R> {
  private final FutureIterator<T> inputIterator;
  private final Function<T, Future<R>> f;

  public AppliedFutureIterator(FutureIterator<T> inputIterator, Function<T, Future<R>> f) {
    this.inputIterator = inputIterator;
    this.f = f;
  }

  public Future<R> internalNext() {
    return inputIterator.next()
      .compose(nextInput -> {
        if (nextInput == null) {
          return succeededFuture(null);
        }
        return f.apply(nextInput)
          .compose(result -> {
            if (result == null) {
              return internalNext();
            }
            return succeededFuture(result);
          });
      });
  }
}
