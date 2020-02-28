package org.folio.rest.impl.crud;

import org.folio.rest.impl.crud.objects.GenericCrudObject;
import org.folio.rest.impl.crud.objects.PrefixCrudObject;
import org.folio.rest.impl.crud.objects.ReasonForClosureCrudObject;
import org.folio.rest.impl.crud.objects.SuffixCrudObject;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.Suffix;

import java.util.HashMap;
import java.util.Map;

public class CrudObjectResolver {

  private static Map<Class, GenericCrudObject> objects = new HashMap<>();

  static {
    objects.put(ReasonForClosure.class, new ReasonForClosureCrudObject());
    objects.put(Prefix.class, new PrefixCrudObject());
    objects.put(Suffix.class, new SuffixCrudObject());
  }

  private CrudObjectResolver() {
  }

  public static <T> GenericCrudObject<T> getCrudObject(Class<T> clazz) {
    return objects.get(clazz);
  }
}
