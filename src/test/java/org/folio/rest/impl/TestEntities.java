package org.folio.rest.impl;

import static org.folio.rest.impl.ApiTestBase.getMockData;
import static org.folio.orders.utils.HelperUtils.ID;

import java.io.IOException;
import java.util.function.Supplier;

import org.folio.orders.utils.HelperUtils;

import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.resource.OrdersTitles;
import org.folio.rest.tools.parser.JsonPathParser;

import io.vertx.core.json.JsonObject;

public enum TestEntities {
  TITLE("budgets", getEndpoint(OrdersTitles.class), Title.class, "mockdata/titles/titles.json", "titles[0]", "name", "Updated name", 1);


  TestEntities(String name, String endpoint, Class clazz, String pathToSamples, String jsonPathToSample, String updatedFieldName,
               Object updatedFieldValue, int collectionQuantity) {
    this.name = name;
    this.endpoint = endpoint;
    this.clazz = clazz;
    this.jsonPathToObject = jsonPathToSample;
    this.pathToFileWithData = pathToSamples;
    this.updatedFieldName = updatedFieldName;
    this.updatedFieldValue = updatedFieldValue;
    this.collectionQuantity = collectionQuantity;
  }

  private final String name;
  private int collectionQuantity;
  private String endpoint;
  private String jsonPathToObject;
  private String pathToFileWithData;
  private String updatedFieldName;
  private String ignoreProperties;
  private Object updatedFieldValue;
  private Class clazz;

  public String getEndpoint() {
    return endpoint;
  }

  public String getEndpointWithDefaultId() {
    return endpoint + "/" + getMockObject().getString(ID);
  }

  public String getEndpointWithId(String id) {
    return endpoint + "/" + id;
  }

  public String getUpdatedFieldName() {
    return updatedFieldName;
  }

  public Object getUpdatedFieldValue() {
    return updatedFieldValue;
  }

  public int getCollectionQuantity() {
    return collectionQuantity;
  }

  public Class<?> getClazz() {
    return clazz;
  }

  public String getPathToFileWithData() {
    return pathToFileWithData;
  }

  public JsonObject getMockObject() {
    return getMockObject(jsonPathToObject);
  }

  public JsonObject getMockObject(String jsonPath) {
    Supplier<JsonObject> jsonFromFile = () -> {
      try {
        return new JsonObject(getMockData(pathToFileWithData));
      } catch (IOException e) {
        return null;
      }
    };
    return (JsonObject) new JsonPathParser(jsonFromFile.get()).getValueAt(jsonPath);
  }

  private static String getEndpoint(Class<?> clazz) {
    return HelperUtils.getEndpoint(clazz);
  }

  public String getName() {
    return name;
  }

  public String getIgnoreProperties() {
    return ignoreProperties;
  }

  public void setIgnoreProperties(String ignoreProperties) {
    this.ignoreProperties = ignoreProperties;
  }
}
