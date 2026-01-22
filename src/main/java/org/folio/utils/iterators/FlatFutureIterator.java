package org.folio.utils.iterators;

import io.vertx.core.Future;

import static io.vertx.core.Future.succeededFuture;

/**
 * Used to flatten an iterator of iterators.
 */
public class FlatFutureIterator<T> extends FutureIterator<T> {

  private final FutureIterator<FutureIterator<T>> inputIterator;
  private FutureIterator<T> currentIterator = null;

  public FlatFutureIterator(FutureIterator<FutureIterator<T>> inputIterator) {
    this.inputIterator = inputIterator;
  }

  public Future<T> internalNext() {
    if (currentIterator == null) {
      return nextFromNewIterator();
    }
    return nextFromCurrentIterator();
  }

  private Future<T> nextFromCurrentIterator() {
    return currentIterator.next()
      .compose(result -> {
        if (result == null) {
          return nextFromNewIterator();
        }
        return succeededFuture(result);
      });
  }

  private Future<T> nextFromNewIterator() {
    return inputIterator.next()
      .compose(nextIterator -> {
        if (nextIterator == null) {
          return succeededFuture(null);
        }
        currentIterator = nextIterator;
        return nextFromCurrentIterator();
    });
  }
}
