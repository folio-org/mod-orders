package org.folio.orders.utils;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class StreamUtilsTest {

  @Test
  void testFind() {
    List<String> list = List.of("1", "2", "3");

    assertNull(StreamUtils.find(list, s -> s.equals("4")));

    assertEquals("2", StreamUtils.find(list, s -> s.equals("2")));
  }

  @Test
  void testFilter() {
    List<String> list = List.of("1", "2", "3");

    assertTrue(StreamUtils.filter(list, s -> s.equals("4")).isEmpty());

    assertEquals(List.of("1", "3"), StreamUtils.filter(list, s -> s.matches("[13]")));
  }

  @Test
  void testMap() {
    List<String> list = List.of("1", "22", "333");

    List<Integer> lengthList = StreamUtils.map(list, String::length);
    assertEquals(List.of(1, 2, 3), lengthList);
  }

  @Test
  void testMapToSet() {
    List<String> list = List.of("1", "2", "3", "1", "2", "3");

    assertEquals(Set.of("1", "2", "3"), StreamUtils.mapToSet(list, s -> s));
  }

  @Test
  void testListToMapKey() {
    List<String> list = List.of("1", "22", "3");

    Map<Integer, String> map = StreamUtils.listToMap(list, String::length);
    assertEquals(2, map.size());
    assertEquals("1", map.get(1));
    assertEquals("22", map.get(2));
  }

  @Test
  void testListToMapKeyValue() {
    List<String> list = List.of("1", "22", "3");

    Map<Integer, String> map = StreamUtils.listToMap(list, String::length, s -> s + "x");
    assertEquals(2, map.size());
    assertEquals("1x", map.get(1));
    assertEquals("22x", map.get(2));
  }

  @Test
  void testGroupBy() {
    List<String> list = List.of("1", "22", "3");

    Map<Integer, List<String>> map = StreamUtils.groupBy(list, String::length);
    assertEquals(2, map.size());
    assertEquals(List.of("1", "3"), map.get(1));
    assertEquals(List.of("22"), map.get(2));
  }

}
