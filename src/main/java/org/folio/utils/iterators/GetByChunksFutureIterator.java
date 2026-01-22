package org.folio.utils.iterators;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.List;
import java.util.function.Function;

import static io.vertx.core.Future.succeededFuture;

/**
 * Retrieves records by chunks.
 * This is useful when a potentially large number of records can match a query, and they should not all be loaded in memory.
 * The function should return results by chunks using the same chunk size as the iterator one.
 */
public class GetByChunksFutureIterator<T> extends FutureIterator<List<T>> {

  private final String baseQuery;
  private final int chunkSize;
  private final Function<String, Future<List<T>>> getFunction;
  private String lastId = null;
  private boolean done = false;

  public GetByChunksFutureIterator(String baseQuery, int chunkSize, Function<String, Future<List<T>>> getFunction) {
    if (chunkSize <= 0) {
      throw new RuntimeException("GetByChunksFutureIterator: chunkSize must be strictly positive");
    }
    this.baseQuery = baseQuery;
    this.chunkSize = chunkSize;
    this.getFunction = getFunction;
  }

  public Future<List<T>> internalNext() {
    // See https://github.com/folio-org/raml-module-builder#implement-chunked-bulk-download
    if (done) {
      return succeededFuture(null);
    }
    String query = baseQuery;
    if (lastId == null) {
      query += " and cql.allRecords=1";
    } else {
      query += " and id > " + lastId;
    }
    query += " sortBy id";
    return getFunction.apply(query)
      .map(chunk -> {
        if (chunk.isEmpty()) {
          done = true;
          return null;
        }
        if (chunk.size() < chunkSize) {
          done = true;
        }
        lastId = JsonObject.mapFrom(chunk.getLast()).getString("id");
        return chunk;
      });
  }
}
