package org.folio.utils.iterators;

import io.vertx.core.Future;
import org.apache.commons.collections4.ListUtils;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.utils.iterators.AppliedFutureIterator;
import org.folio.utils.iterators.ChunkedFutureIterator;
import org.folio.utils.iterators.DechunkedFutureIterator;
import org.folio.utils.iterators.FlatFutureIterator;
import org.folio.utils.iterators.FutureIterator;
import org.folio.utils.iterators.FutureIteratorFromIterator;
import org.folio.utils.iterators.GetByChunksFutureIterator;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

import static io.vertx.core.Future.succeededFuture;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class FutureIteratorTest {

  @Test
  void futureIteratorFromIteratorTest() {
    List<Integer> numbers = IntStream.rangeClosed(0, 9)
      .boxed()
      .toList();
    FutureIterator<Integer> numberIterator = new FutureIteratorFromIterator<>(numbers.listIterator());
    for (int i = 0; i < 10; i++) {
      Integer number = numberIterator.next().result();
      assertEquals(i, number);
    }
    assertNull(numberIterator.next().result());
  }

  @Test
  void chunkedFutureIteratorTest() {
    List<Integer> numbers = IntStream.rangeClosed(0, 24)
      .boxed()
      .toList();
    FutureIterator<Integer> numberIterator = new FutureIteratorFromIterator<>(numbers.listIterator());
    ChunkedFutureIterator<Integer> chunkedIterator = new ChunkedFutureIterator<>(numberIterator, 10);
    List<List<Integer>> expected = ListUtils.partition(numbers, 10);
    for (int i = 0; i < 3; i++) {
      List<Integer> chunk = chunkedIterator.next().result();
      assertEquals(expected.get(i), chunk);
    }
    assertNull(chunkedIterator.next().result());
  }

  @Test
  void dechunkedFutureIteratorTest() {
    List<Integer> numbers = IntStream.rangeClosed(0, 24)
      .boxed()
      .toList();
    List<List<Integer>> listOfLists = ListUtils.partition(numbers, 10);
    FutureIterator<List<Integer>> chunkedIterator = new FutureIteratorFromIterator<>(listOfLists.listIterator());
    FutureIterator<Integer> dechunkedIterator = new DechunkedFutureIterator<>(chunkedIterator);
    for (int i = 0; i <= 24; i++) {
      Integer next = dechunkedIterator.next().result();
      assertEquals(i, next);
    }
    assertNull(dechunkedIterator.next().result());
  }

  @Test
  void appliedFutureIteratorTest() {
    ArrayList<Integer> numbers = new ArrayList<>(IntStream.rangeClosed(0, 9)
      .boxed()
      .toList());
    numbers.addFirst(999); // add a number we will skip
    FutureIterator<Integer> numberIterator = new FutureIteratorFromIterator<>(numbers.listIterator());
    FutureIterator<Integer> appliedIterator = new AppliedFutureIterator<>(numberIterator,
      number -> {
        if (number == 999) {
          return succeededFuture(null);
        }
        return succeededFuture(number + 1);
      });
    for (int i = 0; i < 10; i++) {
      Integer number = appliedIterator.next().result();
      assertEquals(i + 1, number);
    }
    assertNull(appliedIterator.next().result());
  }

  @Test
  void flatFutureIteratorTest() {
    List<Integer> numbers = IntStream.rangeClosed(0, 9)
      .boxed()
      .toList();
    FutureIterator<Integer> numberIterator = new FutureIteratorFromIterator<>(numbers.listIterator());
    FutureIterator<FutureIterator<Integer>> iteratorOfIterators = new AppliedFutureIterator<>(numberIterator,
      number -> succeededFuture(new FutureIteratorFromIterator<>(numbers.listIterator())));
    FutureIterator<Integer> flatIterator = new FlatFutureIterator<>(iteratorOfIterators);
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 10; j++) {
        Integer next = flatIterator.next().result();
        assertEquals(j, next);
      }
    }
    assertNull(flatIterator.next().result());
  }

  @Test
  void getByChunksFutureIteratorTest() {
    PoLine poLine1 = new PoLine()
      .withId("id1");
    PoLine poLine2 = new PoLine()
      .withId("id2");
    PoLine poLine3 = new PoLine()
      .withId("id3");
    List<PoLine> poLineList1 = List.of(poLine1, poLine2);
    List<PoLine> poLineList2 = List.of(poLine3);
    List<String> queries = new ArrayList<>();
    Function<String, Future<List<PoLine>>> getFunction = query -> {
      queries.add(query);
      return succeededFuture(query.contains("allRecords") ? poLineList1 : poLineList2);
    };
    GetByChunksFutureIterator<PoLine> futureIterator = new GetByChunksFutureIterator<>("baseQuery", 2, getFunction);
    List<PoLine> chunk1 = futureIterator.next().result();
    assertEquals(poLineList1, chunk1);
    List<PoLine> chunk2 = futureIterator.next().result();
    assertEquals(poLineList2, chunk2);
    assertEquals(2, queries.size());
    assertEquals("baseQuery and cql.allRecords=1 sortBy id", queries.getFirst());
    assertEquals("baseQuery and id > id2 sortBy id", queries.get(1));
  }

  @Test
  void combinedFutureIteratorTest() {
    List<Integer> numbers = IntStream.rangeClosed(0, 99)
      .boxed()
      .toList();
    FutureIterator<Integer> numberIterator = new FutureIteratorFromIterator<>(numbers.listIterator());
    ChunkedFutureIterator<Integer> chunkedBy10 = new ChunkedFutureIterator<>(numberIterator, 10);
    FutureIterator<List<Integer>> chunkedBy10Plus1 = new AppliedFutureIterator<>(chunkedBy10,
      chunk -> succeededFuture(chunk.stream().map(i -> i + 1).toList()));
    FutureIterator<Integer> deChunkedIterator = new DechunkedFutureIterator<>(chunkedBy10Plus1);
    ChunkedFutureIterator<Integer> rechunkedIterator = new ChunkedFutureIterator<>(deChunkedIterator, 20);
    int current = 1;
    for (int i = 0; i < 5; i++) {
      List<Integer> chunk = rechunkedIterator.next().result();
      assertEquals(20, chunk.size());
      for (int j = 0; j < 20; j++) {
        assertEquals(current, chunk.get(j));
        current++;
      }
    }
    assertNull(rechunkedIterator.next().result());
  }
}
