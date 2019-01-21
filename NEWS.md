## 3.0.0 - Unreleased
 * [MODORDERS-124](https://issues.folio.org/browse/MODORDERS-124) - Redefined existing order/lines endpoints

## 2.0.1 - Released

The sole purpose of this release is to bring the interface versions in the RAML file inline with those in the module descriptor.

 * [MODORDERS-131](https://issues.folio.org/browse/MODORDERS-131)

## 2.0.0 - Released

This primary focus of this release was to implement backend logic necessary for ui-orders to manage (Create, Read, Update, Delete) purchase orders and purchase order lines.

NOTE: This was originally slated to be v1.1.0, but given the amount of changes made to API, schemas and interface permissions, the major version number was bumped.

NOTE: Validation is enabled for all endpoints in this release.

### Stories
 * [MODORDERS-95](https://issues.folio.org/browse/MODORDERS-95) - Upgrade to RMB v23.3.0 to enable path parameters pattern validation
 * [MODORDERS-94](https://issues.folio.org/browse/MODORDERS-94) - Improved implementation of PUT /orders/id
 * [MODORDERS-92](https://issues.folio.org/browse/MODORDERS-92) - Add the ability to specify a reason for closure
 * [MODORDERS-91](https://issues.folio.org/browse/MODORDERS-91) - Limit number of po_lines per purchase_order
 * [MODORDERS-90](https://issues.folio.org/browse/MODORDERS-90) - Update receiving and check-in schemas
 * [MODORDERS-89](https://issues.folio.org/browse/MODORDERS-89) - New API to validate PO Number
 * [MODORDERS-88](https://issues.folio.org/browse/MODORDERS-88) - Honor 'po_number' if passed in during order placement
 * [MODORDERS-85](https://issues.folio.org/browse/MODORDERS-85) - Move 'renewals' to purchase_order
 * [MODORDERS-83](https://issues.folio.org/browse/MODORDERS-83) - Set the 'created_on' field automatically
 * [MODORDERS-79](https://issues.folio.org/browse/MODORDERS-79) - Implemented GET order line logic
 * [MODORDERS-78](https://issues.folio.org/browse/MODORDERS-78) - Implemented GET order line logic
 * [MODORDERS-77](https://issues.folio.org/browse/MODORDERS-77) - Implemented PUT order line logic
 * [MODORDERS-76](https://issues.folio.org/browse/MODORDERS-76) - Implemented POST order line logic
 * [MODORDERS-70](https://issues.folio.org/browse/MODORDERS-70) - Various po_line schema updates
 * [MODORDERS-69](https://issues.folio.org/browse/MODORDERS-69) - Create instance record in inventory when order transitions to OPEN status
 * [MODORDERS-32](https://issues.folio.org/browse/MODORDERS-32) & [MODORDERS-33](https://issues.folio.org/browse/MODORDERS-33) - Create po_line sub-objects on order creation

### Bug Fixes
 * [MODORDERS-120](https://issues.folio.org/browse/MODORDERS-120)
 * [MODORDERS-116](https://issues.folio.org/browse/MODORDERS-116)
 * [MODORDERS-114](https://issues.folio.org/browse/MODORDERS-114)
 * [MODORDERS-113](https://issues.folio.org/browse/MODORDERS-113)
 * [MODORDERS-112](https://issues.folio.org/browse/MODORDERS-112)
 * [MODORDERS-102](https://issues.folio.org/browse/MODORDERS-102)


## 1.0.2 - Released
 * [MODORDERS-79](https://issues.folio.org/browse/MODORDERS-79) - Implemented delete order line logic
 * [MODORDERS-75](https://issues.folio.org/browse/MODORDERS-75) - Define order line endpoints
 * [MODORDERS-62](https://issues.folio.org/browse/MODORDERS-62) - Migrate to RAML1.0 and RMB 23
 * [MODORDERS-51](https://issues.folio.org/browse/MODORDERS-51)
 * [MODORDERS-50](https://issues.folio.org/browse/MODORDERS-50)
 * [MODORDERS-49](https://issues.folio.org/browse/MODORDERS-49)
 * [MODORDERS-48](https://issues.folio.org/browse/MODORDERS-48)
 * [MODORDERS-47](https://issues.folio.org/browse/MODORDERS-47)
 * [MODORDERS-40](https://issues.folio.org/browse/MODORDERS-40)
 * [MODORDERS-45](https://issues.folio.org/browse/MODORDERS-45) 
 * [MODORDERS-37](https://issues.folio.org/browse/MODORDERS-37)
 * [MODORDERS-34](https://issues.folio.org/browse/MODORDERS-34)
 * [MODORDERS-29](https://issues.folio.org/browse/MODORDERS-29)
 * [MODORDERS-28](https://issues.folio.org/browse/MODORDERS-28)
 * [MODORDERS-27](https://issues.folio.org/browse/MODORDERS-27)
 * [MODORDERS-25](https://issues.folio.org/browse/MODORDERS-25)
 * [MODORDERS-24](https://issues.folio.org/browse/MODORDERS-24)
 * [MODORDERS-23](https://issues.folio.org/browse/MODORDERS-23)
 * [MODORDERS-18](https://issues.folio.org/browse/MODORDERS-18)
 * [MODORDERS-17](https://issues.folio.org/browse/MODORDERS-17)
 * [MODORDERS-16](https://issues.folio.org/browse/MODORDERS-16)
 * [MODORDERS-15](https://issues.folio.org/browse/MODORDERS-15)
 * [MODORDERS-14](https://issues.folio.org/browse/MODORDERS-14)
 * Initial commit
