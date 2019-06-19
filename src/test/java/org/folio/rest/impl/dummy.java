package org.folio.rest.impl;

import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.lang.reflect.Field;
import org.apache.commons.lang.reflect.FieldUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;

public class dummy {

  public static void main(String[] args) throws IllegalAccessException, IOException {
    CompositePoLine jo = new JsonObject(ApiTestBase.getMockData("mockdata/compositeLines/0009662b-8b80-4001-b704-ca10971f175d.json")).mapTo(CompositePoLine.class);
    System.err.println(jo);
    //System.err.println(FieldUtils.readDeclaredField(jo, "details.subscriptionFrom",true));
    System.err.println(FieldUtils.getDeclaredField(CompositePoLine.class, "details",true));
    System.err.println(FieldUtils.readDeclaredField(FieldUtils.readDeclaredField(jo, "details",true),"productIds",true));

  }



}
