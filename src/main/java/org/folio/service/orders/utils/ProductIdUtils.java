package org.folio.service.orders.utils;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.ProductId;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static org.apache.commons.lang3.StringUtils.SPACE;
import static org.folio.rest.core.exceptions.ErrorCodes.ISBN_NOT_VALID;

public class ProductIdUtils {

  private ProductIdUtils() {}

  public static Set<String> buildSetOfProductIdsFromCompositePoLines(List<CompositePoLine> compositePoLines, String isbnTypeId) {
    List<ProductId> productIds = compositePoLines.stream()
      .flatMap(pol -> pol.getDetails().getProductIds().stream())
      .toList();
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

  public static boolean isISBNValidationException(Throwable throwable) {
    return throwable instanceof HttpException httpException && ISBN_NOT_VALID.getCode().equals(httpException.getError().getCode());
  }

  public static void removeISBNDuplicates(CompositePoLine compPOL, String isbnTypeId) {
    List<ProductId> productIds = removeISBNDuplicates(compPOL.getDetails().getProductIds(), isbnTypeId);
    compPOL.getDetails().setProductIds(productIds);
  }

  public static List<ProductId> removeISBNDuplicates(List<ProductId> productIds, String isbnTypeId) {
    List<ProductId> notISBNs = getNonISBNProductIds(productIds, isbnTypeId);
    List<ProductId> isbns = getDeduplicatedISBNs(productIds, isbnTypeId);
    return ListUtils.union(isbns, notISBNs);
  }

  public static  String extractProductId(String value) {
    return StringUtils.substringBefore(value, SPACE);
  }

  public static  String extractQualifier(String value) {
    return StringUtils.substringAfter(value, SPACE);
  }

  private static Predicate<ProductId> isUniqueISBN(List<ProductId> productIds) {
    return productId -> productIds.size() == 1 || StringUtils.isNotEmpty(productId.getQualifier());
  }

  private static List<ProductId> getNonISBNProductIds(List<ProductId> productIds, String isbnTypeId) {
    return productIds.stream()
      .filter(productId -> !isISBN(isbnTypeId, productId))
      .toList();
  }

  private static List<ProductId> getDeduplicatedISBNs(List<ProductId> productIds, String isbnTypeId) {
    Map<String, List<ProductId>> uniqueISBNProductIds = productIds.stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .distinct()
      .collect(groupingBy(ProductId::getProductId));

    return uniqueISBNProductIds.values().stream()
      .flatMap(uniqueProductIds -> uniqueProductIds.stream()
        .filter(isUniqueISBN(uniqueProductIds)))
      .toList();
  }
}
