package org.folio.models;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import org.folio.rest.jaxrs.model.RoutingList;

public class TemplateProcessingRequest {
  @JsonProperty
  private UUID templateId;
  @JsonProperty
  private String lang;
  @JsonProperty
  private String outputFormat;
  @JsonProperty
  private Context context;

  public UUID getTemplateId() {
    return templateId;
  }

  public String getLang() {
    return lang;
  }

  public String getOutputFormat() {
    return outputFormat;
  }

  public Context getContext() {
    return context;
  }

  public TemplateProcessingRequest withTemplateId(UUID templateId) {
    this.templateId = templateId;
    return this;
  }

  public TemplateProcessingRequest withLang(String lang) {
    this.lang = lang;
    return this;
  }

  public TemplateProcessingRequest withOutputFormat(String outputFormat) {
    this.outputFormat = outputFormat;
    return this;
  }

  public TemplateProcessingRequest withContext(Context context) {
    this.context = context;
    return this;
  }

  public static class Context {
    @JsonProperty
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private RoutingList routingList;
    @JsonProperty
    private String title;
    @JsonProperty
    private List<User> users;

    public RoutingList getRoutingList() {
      return routingList;
    }

    public String getTitle() {
      return title;
    }

    public List<User> getUsers() {
      return users;
    }

    public Context withRoutingList(RoutingList routingList) {
      this.routingList = routingList;
      return this;
    }

    public Context withTitle(String title) {
      this.title = title;
      return this;
    }

    public Context withUsers(List<User> users) {
      this.users = users;
      return this;
    }
  }

  public static class User {
    @JsonProperty
    private String lastName;
    @JsonProperty
    private String firstName;
    @JsonProperty
    private String routingAddress;

    public String getLastName() {
      return lastName;
    }

    public String getFirstName() {
      return firstName;
    }

    public String getRoutingAddress() {
      return routingAddress;
    }

    public User withLastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public User withFirstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public User withRoutingAddress(String routingAddress) {
      this.routingAddress = routingAddress;
      return this;
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof TemplateProcessingRequest that)) return false;
    return Objects.equals(templateId, that.templateId)
      && Objects.equals(lang, that.lang)
      && Objects.equals(outputFormat, that.outputFormat)
      && Objects.equals(context, that.context);
  }

  @Override
  public int hashCode() {
    return Objects.hash(templateId, lang, outputFormat, context);
  }
}

