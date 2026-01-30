package org.folio.utils.iterators;

import io.vertx.core.Future;

import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

import static io.vertx.core.Future.succeededFuture;

/**
 * An iterator over future results.
 * It does not require to know the number of items in advance.
 * Items are not loaded into memory.
 */
public abstract class FutureIterator<T> {

  private Future<T> futureNext = null;

  /**
   * Returns the next item or null if there are none, in a Future.
   * Optimized by starting to load the next item after this one just before returning the result.
   */
  public Future<T> next() {
    Future<T> nextF;
    if (futureNext != null) {
      nextF = futureNext;
    } else {
      nextF = internalNext();
    }
    return nextF.map(next -> {
      if (next == null) {
        futureNext = null;
      } else {
        futureNext = internalNext();
      }
      return next;
    });
  }

  /**
   * Method for subclasses to implement, returning the next item in a Future.
   */
  protected abstract Future<T> internalNext();

  /**
   * Apply the function to all items in the future iterator.
   */
  public <R> Future<R> applyToAll(Function<T, Future<R>> f) {
    // NOTE: should we use Future.await() here to avoid recursion ?
    return next()
      .compose(nextInput -> {
        if (nextInput == null) {
          return succeededFuture();
        }
        return f.apply(nextInput)
          .compose(v -> applyToAll(f));
      });
  }

  // Convenience methods
  // It would have been nice to have non-static methods here, but dechunk() and flatten() would not work with generics

  public static <T,R> FutureIterator<R> applyFunction(FutureIterator<T> inputIterator, Function<T, Future<R>> f) {
    return new AppliedFutureIterator<>(inputIterator, f);
  }

  public static <T> FutureIterator<List<T>> chunk(FutureIterator<T> inputIterator, int chunkSize) {
    return new ChunkedFutureIterator<>(inputIterator, chunkSize);
  }

  public static <T> FutureIterator<T> dechunk(FutureIterator<List<T>> inputIterator) {
    return new DechunkedFutureIterator<>(inputIterator);
  }

  public static <T> FutureIterator<T> flatten(FutureIterator<FutureIterator<T>> inputIterator) {
    return new FlatFutureIterator<>(inputIterator);
  }

  public static <T> FutureIterator<T> fromIterator(Iterator<T> inputIterator) {
    return new FutureIteratorFromIterator<>(inputIterator);
  }

  public static <T> FutureIterator<List<T>> getByChunks(String baseQuery, int chunkSize, Function<String,
      Future<List<T>>> getFunction) {
    return new GetByChunksFutureIterator<>(baseQuery, chunkSize, getFunction);
  }
}
