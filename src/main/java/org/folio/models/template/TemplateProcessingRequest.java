package org.folio.models.template;

import java.util.List;
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
}

