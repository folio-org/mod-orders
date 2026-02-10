package org.folio.orders.utils;

import lombok.experimental.UtilityClass;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Utility class allowing to perform stream operations in a more convenient way
 */
@UtilityClass
public class StreamUtils {

  public static <T> T find(Collection<T> collection, Predicate<T> predicate) {
    for (T t : collection) {
      if (predicate.test(t)) {
        return t;
      }
    }
    return null;
  }

  public static <T> List<T> filter(Collection<T> collection, Predicate<T> predicate) {
    return collection.stream().filter(predicate).toList();
  }

  public static <K, V> List<V> map(Collection<K> collection, Function<K, V> mapper) {
    return collection.stream().map(mapper).toList();
  }

  public static <K> List<K> flatMap(Collection<? extends Collection<K>> collection) {
    return collection.stream().flatMap(Collection::stream).toList();
  }

  public static <K, V> Set<V> mapToSet(Collection<K> collection, Function<K, V> mapper) {
    return collection.stream().map(mapper).collect(Collectors.toSet());
  }

  public static <T, C extends Collection<T>> List<T> flatten(Collection<C> collection) {
    return collection.stream().flatMap(Collection::stream).toList();
  }

  public static <T, K> Map<K, T> listToMap(Collection<T> collection, Function<T, K> toKey) {
    return listToMap(collection, toKey, Function.identity());
  }

  public static <T, K, V> Map<K, V> listToMap(Collection<T> collection, Function<T, K> toKey, Function<T, V> toValue) {
    return collection.stream().collect(Collectors.toMap(toKey, toValue, (a, b) -> a));
  }

  public static <T, K> Map<K, List<T>> groupBy(Collection<T> collection, Function<T, K> toKey) {
    return groupBy(collection, toKey, Function.identity());
  }

  public static <T, K, V> Map<K, List<V>> groupBy(Collection<T> collection, Function<T, K> toKey, Function<T, V> toValue) {
    Map<K, List<V>> map = new HashMap<>();
    for (T t : collection) {
      K key = toKey.apply(t);
      List<V> sublist = map.computeIfAbsent(key, k -> new ArrayList<>());
      sublist.add(toValue.apply(t));
    }
    return map;
  }

}
