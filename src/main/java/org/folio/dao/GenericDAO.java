package org.folio.dao;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.jaxrs.model.Entity;

import io.vertx.core.Context;

/**
 * Generic data access object
 * @param <T> type of the entity
 * @param <E> type of the collection of T entities
 */
public interface GenericDAO<T extends Entity, E> {

  /**
   * Searches for T entities
   *
   * @param query   query string to filter records based on matching criteria in fields
   * @param limit   maximum number of results to return
   * @param offset  starting index in a list of results
   * @return CompletableFuture with E, a collection of T entities
   */
  CompletableFuture<E> get(String query, int offset, int limit, Context context, Map<String, String> headers);

  /**
   * Searches for T entity by id
   *
   * @param id  config entity id
   * @return    CompletableFuture with T entity
   */
  CompletableFuture<T> getById(String id, Context context, Map<String, String> headers);

  /**
   * Saves T entity
   *
   * @param entity  entity to save
   * @return CompletableFuture with created T entity
   */
  CompletableFuture<T> save(T entity, Context context, Map<String, String> headers);

  /**
   * Updates T entity
   *
   * @param id      entity id
   * @param entity  entity to update
   * @return CompletableFuture
   */
  CompletableFuture<Void> update(String id, T entity, Context context, Map<String, String> headers);

  /**
   * Delete entity by id
   *
   * @param id  entity id
   * @return  CompletableFuture
   */
  CompletableFuture<Void> delete(String id, Context context, Map<String, String> headers);

}
