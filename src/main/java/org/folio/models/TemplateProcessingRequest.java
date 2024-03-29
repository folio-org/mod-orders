package org.folio.models;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import lombok.Getter;

@Getter
public class TemplateProcessingRequest {
  private UUID templateId;
  private String lang;
  private String outputFormat;
  private Context context;

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

  @Getter
  public static class Context {
    private List<User> users;
    private List<Item> items;

    public Context withUsers(List<User> users) {
      this.users = users;
      return this;
    }

  }

  @Getter
  public static class User {
    private String name;
    public User withName(String name) {
      this.name = name;
      return this;
    }
  }

  @Getter
  public static class Item {
    private String name;

    public Item setName(String name) {
      this.name = name;
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

