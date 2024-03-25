package org.folio.orders.utils;

import io.vertx.core.json.JsonArray;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.core.models.RequestContext;

import java.util.List;
import java.util.Set;

public class PermissionsUtil {

  private static final String PERMISSION_ORDER_APPROVE = "orders.item.approve";
  private static final String PERMISSION_ORDER_UNOPEN = "orders.item.unopen";
  private static final String PERMISSION_ORDER_REOPEN = "orders.item.reopen";
  private static final String EMPTY_ARRAY = "[]";

  public static final String OKAPI_HEADER_PERMISSIONS = "X-Okapi-Permissions";

  private PermissionsUtil() {}

  public static boolean userHasDesiredPermission(AcqDesiredPermissions acqPerm, RequestContext requestContext) {
    return getProvidedPermissions(requestContext).contains(acqPerm.getPermission());
  }

  public static boolean userDoesNotHaveDesiredPermission(AcqDesiredPermissions acqPerm, RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(acqPerm.getPermission());
  }

  public static boolean isManagePermissionRequired(Set<String> newAcqUnits, Set<String> acqUnitsFromStorage) {
    return !CollectionUtils.isEqualCollection(newAcqUnits, acqUnitsFromStorage);
  }

  private static List<String> getProvidedPermissions(RequestContext requestContext) {
    return new JsonArray(requestContext.getHeaders().getOrDefault(OKAPI_HEADER_PERMISSIONS, EMPTY_ARRAY)).stream()
      .map(Object::toString)
      .toList();
  }

  public static boolean userDoesNotHaveApprovePermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_APPROVE);
  }

  public static boolean userDoesNotHaveUnopenPermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_UNOPEN);
  }

  public static boolean userDoesNotHaveReopenPermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_REOPEN);
  }
}
