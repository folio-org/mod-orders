package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.TAGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

public class TagService {

  private static final String TAG_ENDPOINT = resourcesPath(TAGS);

  private final RestClient restClient;

  public TagService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<Void> createTagsIfMissing(Set<String> tagLabels, RequestContext requestContext) {
    List<String> tagList = new ArrayList<>(tagLabels);
    String query = QueryUtils.convertTagListToCqlQuery(tagList, "label", true);

    return getTags(query, 0, Integer.MAX_VALUE, requestContext)
      .map(existingTagsCollection -> {
        List<String> existingTags = existingTagsCollection.getTags().stream()
          .map(Tag::getLabel)
          .collect(Collectors.toList());

        if (!existingTags.isEmpty()) {
          return CollectionUtils.removeAll(tagList, existingTags);
        }
        return tagList;
      })
      .compose(tagsForCreate -> {
        List<Future<Tag>> futures = new ArrayList<>();
        tagsForCreate.forEach(tag -> futures.add(createTag(tag, requestContext)));

        return Future.join(futures).mapEmpty();
      });
  }

  public Future<TagCollection> getTags(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(TAG_ENDPOINT).withQuery(query)
      .withLimit(limit)
      .withOffset(offset);
    return restClient.get(requestEntry, TagCollection.class, requestContext);
  }

  public Future<Tag> createTag(String tagName, RequestContext requestContext) {
    Tag tag = new Tag().withLabel(tagName);
    RequestEntry requestEntry = new RequestEntry(TAG_ENDPOINT);
    return restClient.post(requestEntry, tag, Tag.class, requestContext);
  }

}
