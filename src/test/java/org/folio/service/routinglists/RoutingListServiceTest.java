package org.folio.service.routinglists;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.service.orders.PurchaseOrderLineService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.List;

import static org.folio.TestUtils.getLocationPhysicalCopies;
import static org.folio.TestUtils.getMinimalContentPoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.ROUTING_LISTS_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

@ExtendWith(VertxExtension.class)
public class RoutingListServiceTest {

  private static final String ROUTING_LIST_SAMPLE = ROUTING_LISTS_MOCK_DATA_PATH + "routing-list.json";

  private static final String PO_LINE_UUID = "0009662b-8b80-4001-b704-ca10971f222d";

  private PoLine samplePoLine;
  private RoutingList sampleRoutingList;

  @Mock
  private RestClient restClient;

  @Mock
  private PurchaseOrderLineService poLineService;

  private RoutingListsStorageService routingListService;

  @BeforeEach
  void before() {
    sampleRoutingList = getMockAsJson(ROUTING_LIST_SAMPLE).mapTo(RoutingList.class);
    samplePoLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(getLocationPhysicalCopies(1));

    routingListService = spy(new RoutingListsStorageService(poLineService, restClient));
  }

  @Test
  void testCreateRoutingList() {
    doReturn(getRoutingListCollection(1)).when(restClient).get(any(RequestEntry.class), RoutingList.class, any());
    doReturn(Future.succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());
    doReturn(Future.succeededFuture(List.of())).when(routingListService).getRoutingLists(any(), any(), any(), any());

    routingListService.createRoutingList(sampleRoutingList, null);
  }

  @Test
  void testCreateRoutingListWithPOLineLimitReached() {
    doReturn(getRoutingListCollection(1)).when(restClient).get(any(RequestEntry.class), RoutingList.class, any());
    doReturn(Future.succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());
    doReturn(Future.succeededFuture(List.of(sampleRoutingList))).when(routingListService).getRoutingLists(any(), any(), any(), any());

    assertThrows(HttpException.class, () -> routingListService.createRoutingList(sampleRoutingList, null));
  }

  @Test
  void testCreateRoutingListWithPOLineInvalidOrderFormat() {
    samplePoLine.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    doReturn(getRoutingListCollection(0)).when(restClient).get(any(RequestEntry.class), RoutingList.class, any());
    doReturn(Future.succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());
    doReturn(Future.succeededFuture(List.of())).when(routingListService).getRoutingLists(any(), any(), any(), any());

    assertThrows(HttpException.class, () -> routingListService.createRoutingList(sampleRoutingList, null));
  }

  private RoutingListCollection getRoutingListCollection(int n) {
    List<RoutingList> lists = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      lists.add(sampleRoutingList);
    }
    return new RoutingListCollection()
      .withRoutingLists(lists)
      .withTotalRecords(n);
  }

}
