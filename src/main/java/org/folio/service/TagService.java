package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.TAGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.acq.model.tag.TagCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class TagService {

  private static final String TAG_ENDPOINT = resourcesPath(TAGS);

  private final RestClient restClient;

  public TagService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<Void> createTagsIfMissing(Set<String> tagLabels, RequestContext requestContext) {
    // MODORDERS-470 - new tags are all lower-case and no spaces
    List<String> tagList = tagLabels.stream()
      .map(tag -> StringUtils.deleteWhitespace(tag).toLowerCase())
      .collect(Collectors.toList());

    String query = HelperUtils.convertFieldListToCqlQuery(tagList, "label", true);

    return getTags(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(existingTagsCollection -> {
        List<String> existingTags = existingTagsCollection.getTags().stream()
          .map(Tag::getLabel)
          .collect(Collectors.toList());

        if (!existingTags.isEmpty()) {
          return CollectionUtils.removeAll(tagList, existingTags);
        }
        return tagList;
      })
      .thenCompose(tagsForCreate -> {
        List<CompletableFuture<Tag>> futures = new ArrayList<>();
        tagsForCreate.forEach(tag -> futures.add(createTag(tag, requestContext)));

        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
      });
  }

  public CompletableFuture<TagCollection> getTags(String query, int offset, int limit,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(TAG_ENDPOINT).withQuery(query)
      .withLimit(limit)
      .withOffset(offset);
    return restClient.get(requestEntry, requestContext, TagCollection.class);
  }

  public CompletableFuture<Tag> createTag(String tagName, RequestContext requestContext) {
    Tag tag = new Tag().withLabel(tagName);
    RequestEntry requestEntry = new RequestEntry(TAG_ENDPOINT);
    return restClient.post(requestEntry, tag, requestContext, Tag.class);
  }

}
