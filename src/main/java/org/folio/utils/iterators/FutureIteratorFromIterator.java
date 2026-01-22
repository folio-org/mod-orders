package org.folio.utils.iterators;

import io.vertx.core.Future;

import java.util.Iterator;

import static io.vertx.core.Future.succeededFuture;

/**
 * A future iterator created from a normal iterator.
 */
public class FutureIteratorFromIterator<T> extends FutureIterator<T> {
  Iterator<T> inputIterator;

  public FutureIteratorFromIterator(Iterator<T> inputIterator) {
    this.inputIterator = inputIterator;
  }

  public Future<T> internalNext() {
    if (inputIterator.hasNext()) {
      return succeededFuture(inputIterator.next());
    }
    return succeededFuture(null);
  }
}
