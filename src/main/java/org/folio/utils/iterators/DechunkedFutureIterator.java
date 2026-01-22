package org.folio.utils.iterators;

import io.vertx.core.Future;

import java.util.ArrayList;
import java.util.List;

import static io.vertx.core.Future.succeededFuture;

/**
 * Iterator converting a chunked iterator into a single item one.
 */
public class DechunkedFutureIterator<T> extends FutureIterator<T> {
  private final FutureIterator<List<T>> inputIterator;
  private ArrayList<T> buffer = new ArrayList<>();

  public DechunkedFutureIterator(FutureIterator<List<T>> inputIterator) {
    this.inputIterator = inputIterator;
  }

  public Future<T> internalNext() {
    if (!buffer.isEmpty()) {
      return succeededFuture(buffer.removeFirst());
    }
    return inputIterator.next()
      .compose(nextInput -> {
        if (nextInput == null) {
          return succeededFuture(null);
        } else {
          buffer = new ArrayList<>(nextInput);
          return internalNext();
        }
      });
  }
}
