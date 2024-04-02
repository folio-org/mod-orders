package org.folio.models;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;

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

  public TemplateProcessingRequest setTemplateId(UUID templateId) {
    this.templateId = templateId;
    return this;
  }

  public TemplateProcessingRequest setLang(String lang) {
    this.lang = lang;
    return this;
  }

  public TemplateProcessingRequest setOutputFormat(String outputFormat) {
    this.outputFormat = outputFormat;
    return this;
  }

  public TemplateProcessingRequest setContext(Context context) {
    this.context = context;
    return this;
  }

  public static class Context {
    @JsonProperty
    private RoutingList routingList;
    @JsonProperty
    private List<User> users;

    public RoutingList getRoutingList() {
      return routingList;
    }

    public List<User> getUsers() {
      return users;
    }

    public Context setRoutingList(RoutingList routingList) {
      this.routingList = routingList;
      return this;
    }

    public Context setUsers(List<User> users) {
      this.users = users;
      return this;
    }
  }

  public static class RoutingList {
    @JsonProperty
    private String name;
    @JsonProperty
    private String note;

    public String getName() {
      return name;
    }

    public String getNote() {
      return note;
    }

    public RoutingList setName(String name) {
      this.name = name;
      return this;
    }

    public RoutingList setNote(String note) {
      this.note = note;
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

    public User setLastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public User setFirstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public User setRoutingAddress(String routingAddress) {
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

