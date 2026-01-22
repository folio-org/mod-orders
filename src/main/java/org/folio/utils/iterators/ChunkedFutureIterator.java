package org.folio.utils.iterators;

import io.vertx.core.Future;

import java.util.ArrayList;
import java.util.List;

import static io.vertx.core.Future.succeededFuture;

/**
 * A future iterator taking elements from an input iterator and returning them by chunks.
 */
public class ChunkedFutureIterator<T> extends FutureIterator<List<T>> {
  private final FutureIterator<T> inputIterator;
  private final int chunkSize;
  private boolean done = false;

  public ChunkedFutureIterator(FutureIterator<T> inputIterator, int chunkSize) {
    if (chunkSize <= 0) {
      throw new FutureIteratorException("ChunkedFutureIterator: chunkSize must be strictly positive");
    }
    this.inputIterator = inputIterator;
    this.chunkSize = chunkSize;
  }

  public Future<List<T>> internalNext() {
    if (done) {
      return succeededFuture(null);
    }
    List<T> batch = new ArrayList<>();
    Future<Void> f = succeededFuture(null);
    for (int i = 0; i < chunkSize; i++) {
      f = f.compose(v -> {
        if (done) {
          return succeededFuture(null);
        }
        return inputIterator.next()
          .map(nextInput -> {
            if (nextInput == null) {
              done = true;
            } else {
              batch.add(nextInput);
            }
            return null;
          });
      });
    }
    return f.map(cont -> {
      if (batch.isEmpty()) {
        return null;
      }
      return batch;
    });
  }
}
