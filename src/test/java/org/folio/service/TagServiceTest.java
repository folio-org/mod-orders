package org.folio.service;

import static io.vertx.core.Future.succeededFuture;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;

@ExtendWith(VertxExtension.class)
public class TagServiceTest {
  @InjectMocks
  TagService tagService;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContextMock;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void createTagsIfMissing(VertxTestContext vertxTestContext) {
    String sampleTag = "CAseSenSeTivE";
    Tag postTagResponse = new Tag().withLabel(sampleTag);
    TagCollection emptyTagCollection = new TagCollection()
      .withTags(new ArrayList<>())
      .withTotalRecords(0);

    doReturn(succeededFuture(emptyTagCollection)).when(restClient).get(any(RequestEntry.class), eq(TagCollection.class), any(RequestContext.class));
    doReturn(succeededFuture(postTagResponse)).when(restClient).post(any(RequestEntry.class), any(),  eq(Tag.class), any());

    Future<Void> future = tagService.createTagsIfMissing(Collections.singleton(sampleTag), requestContextMock);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        Assertions.assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
