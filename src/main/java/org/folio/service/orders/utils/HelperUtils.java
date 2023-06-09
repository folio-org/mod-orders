package org.folio.service.orders.utils;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertxconcurrent.Semaphore;
import org.apache.commons.lang3.StringUtils;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.ProductId;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;

public class HelperUtils {

  private HelperUtils() {}

  public static <I, O> Future<List<O>> executeWithSemaphores(Collection<I> collection,
                                                             FunctionReturningFuture<I, O> f, RequestContext requestContext) {
    if (collection.isEmpty())
      return Future.succeededFuture(List.of());
    return requestContext.getContext().<List<Future<O>>>executeBlocking(promise -> {
      Semaphore semaphore = new Semaphore(SEMAPHORE_MAX_ACTIVE_THREADS, Vertx.currentContext().owner());
      List<Future<O>> futures = new ArrayList<>();
      for (I item : collection) {
        semaphore.acquire(() -> {
          Future<O> future = f.apply(item)
            .onComplete(asyncResult -> semaphore.release());
          futures.add(future);
          if (futures.size() == collection.size()) {
            promise.complete(futures);
          }
        });
      }
    }).compose(HelperUtils::collectResultsOnSuccess);
  }

  /**
   * Wait for all requests completion and collect all resulting objects. In case any failed, complete resulting future with the exception
   * @param futures list of futures and each produces resulting object on completion
   * @param <T> resulting objects type
   * @return Future with resulting objects
   */
  public static <T> Future<List<T>> collectResultsOnSuccess(List<Future<T>> futures) {
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(CompositeFuture::list);
  }

  public interface FunctionReturningFuture<I, O> {
    Future<O> apply(I item);
  }

  public static Set<String> buildSetOfProductIdsFromCompositePoLines(List<CompositePoLine> compositePoLines, String isbnTypeId) {
    List<ProductId> productIds = compositePoLines.stream()
      .flatMap(pol -> pol.getDetails().getProductIds().stream())
      .collect(Collectors.toList());
    return buildSetOfProductIds(productIds, isbnTypeId);
  }

  public static Set<String> buildSetOfProductIds(List<ProductId> productIds, String isbnTypeId) {
    return productIds.stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .map(ProductId::getProductId)
      .collect(Collectors.toSet());
  }

  public static boolean isISBN(String isbnTypeId, ProductId productId) {
    return Objects.equals(productId.getProductIdType(), isbnTypeId);
  }

  public static void removeISBNDuplicates(CompositePoLine compPOL, String isbnTypeId) {
    List<ProductId> productIds = removeISBNDuplicates(compPOL.getDetails().getProductIds(), isbnTypeId);
    compPOL.getDetails().setProductIds(productIds);
  }

  public static List<ProductId> removeISBNDuplicates(List<ProductId> productIds, String isbnTypeId) {
    List<ProductId> notISBNs = getNonISBNProductIds(productIds, isbnTypeId);
    List<ProductId> isbns = getDeduplicatedISBNs(productIds, isbnTypeId);
    isbns.addAll(notISBNs);
    return isbns;
  }

  public static  String extractProductId(String value) {
    if(value == null || !value.contains(" ")) {
      return value;
    }
    return value.substring(0, value.indexOf(" "));
  }

  public static  String extractQualifier(String value) {
    if(value == null || !value.contains(" ")) {
      return null;
    }
    return value.substring(value.indexOf(" ") + 1);
  }

  private static Predicate<ProductId> isUniqueISBN(List<ProductId> productIds) {
    return productId -> productIds.size() == 1 || StringUtils.isNotEmpty(productId.getQualifier());
  }

  private static List<ProductId> getNonISBNProductIds(List<ProductId> productIds, String isbnTypeId) {
    return productIds.stream()
      .filter(productId -> !isISBN(isbnTypeId, productId))
      .collect(toList());
  }

  private static List<ProductId> getDeduplicatedISBNs(List<ProductId> productIds, String isbnTypeId) {
    Map<String, List<ProductId>> uniqueISBNProductIds = productIds.stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .distinct()
      .collect(groupingBy(ProductId::getProductId));

    return uniqueISBNProductIds.values().stream()
      .flatMap(uniqueProductIds -> uniqueProductIds.stream()
        .filter(isUniqueISBN(uniqueProductIds)))
      .collect(toList());
  }
}
