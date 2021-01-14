package org.folio.rest.core.models;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.glassfish.jersey.uri.UriTemplate;
import scala.Int;

public class RequestEntry {
    private String baseEndpoint;
    private Map<String, String> pathParams = new HashMap<>();
    private Map<String, Object> queryParams = new HashMap<>();

    public RequestEntry(String baseEndpoint) {
        this.baseEndpoint = baseEndpoint;
    }

    public RequestEntry withPathParameter(String key, String value) {
        pathParams.put(key, value);
        return this;
    }

    public RequestEntry withQueryParameter(String key, Object value) {
        queryParams.put(key, value);
        return this;
    }

    public RequestEntry withId(String id) {
        pathParams.put("id", id);
        return this;
    }

    public RequestEntry withQuery(String query) {
        queryParams.put("query", query);
        return this;
    }

    public RequestEntry withLimit(Integer limit) {
        queryParams.put("limit", limit);
        return this;
    }

    public RequestEntry withOffset(Integer offset) {
        queryParams.put("offset", offset);
        return this;
    }

    public String getBaseEndpoint() {
        return baseEndpoint;
    }

    public void setBaseEndpoint(String baseEndpoint) {
        this.baseEndpoint = baseEndpoint;
    }

    public Map<String, String> getPathParams() {
        return pathParams;
    }

    public void setPathParams(Map<String, String> pathParams) {
        this.pathParams = pathParams;
    }

    public Map<String, Object> getQueryParams() {
        return queryParams;
    }

    public void setQueryParams(Map<String, Object> queryParams) {
        this.queryParams = queryParams;
    }
    public String buildEndpoint() {
        UriTemplate uriTemplate = new UriTemplate(baseEndpoint);
        String endpoint = uriTemplate.createURI(pathParams);
        return endpoint + addQueryParams();
    }

    private String addQueryParams() {
        if (queryParams.isEmpty()) {
            return "";
        }
        return queryParams.entrySet().stream().map(entry -> entry.getKey() + "=" + entry.getValue().toString()).collect(Collectors.joining("&", "?", ""));
    }
}
