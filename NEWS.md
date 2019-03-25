## 5.0.0 - Unreleased

## 4.0.0 - Released
The primary focus of this release was to implement backend logic for the receiving and preparation for the check-in flows.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v3.0.0...v4.0.0)

### Stories
 * [MODORDERS-193](https://issues.folio.org/browse/MODORDERS-193) Sum total quantity on PO retrieval
 * [MODORDERS-185](https://issues.folio.org/browse/MODORDERS-185) Checkin: Implement `POST /orders/peices` endpoint
 * [MODORDERS-184](https://issues.folio.org/browse/MODORDERS-184) Receiving history: updates to support check-in flow
 * [MODORDERS-183](https://issues.folio.org/browse/MODORDERS-183) Checkin: create instance record on Order transition to Open
 * [MODORDERS-181](https://issues.folio.org/browse/MODORDERS-181) Updates to PO: removal of `adjustment` and `totalEstimatedPrice` calculation
 * [MODORDERS-180](https://issues.folio.org/browse/MODORDERS-180) Updates to PO Line: updates to `cost` and removal of `adjustment`
 * [MODORDERS-175](https://issues.folio.org/browse/MODORDERS-175) Add ability to move a received piece back to `Expected`
 * [MODORDERS-171](https://issues.folio.org/browse/MODORDERS-171) Receiving history: additional data for receiving flow
 * [MODORDERS-170](https://issues.folio.org/browse/MODORDERS-170) Prevent ordering from Inactive vendors/access provider
 * [MODORDERS-168](https://issues.folio.org/browse/MODORDERS-168) Refactor PO Line model - business logic
 * [MODORDERS-166](https://issues.folio.org/browse/MODORDERS-166) Add `locationId` to the piece schema
 * [MODORDERS-165](https://issues.folio.org/browse/MODORDERS-165) Add `locationId` to receiving_history
 * [MODORDERS-163](https://issues.folio.org/browse/MODORDERS-163) Implement receiving flow for P/E mix
 * [MODORDERS-162](https://issues.folio.org/browse/MODORDERS-162) Implement receiving flow for E-only
 * [MODORDERS-159](https://issues.folio.org/browse/MODORDERS-159) PO Line: receipt status
 * [MODORDERS-158](https://issues.folio.org/browse/MODORDERS-158) Refactor PO Line model - schema changes
 * [MODORDERS-141](https://issues.folio.org/browse/MODORDERS-141) Refactor business logic helpers structure to improve code maintainability
 * [MODORDERS-103](https://issues.folio.org/browse/MODORDERS-103) Implement receiving flow for physical-only

### Bug Fixes
 * [MODORDERS-197](https://issues.folio.org/browse/MODORDERS-197) Allow 0.0 for unit price in Cost for Orders
 * [MODORDERS-188](https://issues.folio.org/browse/MODORDERS-188) Cryptic error returned when trying to add a POLine to an Open/Closed PO

## 3.0.0 - Released
The primary focus of this release was to implement backend logic necessary for ui-orders to manage purchase orders and purchase order lines, integration between orders and inventory apps and preparation for the receiving flows.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v2.0.1...v3.0.0)

### Stories
 * [MODORDERS-154](https://issues.folio.org/browse/MODORDERS-154) - PO Line's `location` property is changed to `locations` i.e. from single object to array of objects
 * [MODORDERS-149](https://issues.folio.org/browse/MODORDERS-149) - Add PO Line's identifier to item record in the inventory
 * [MODORDERS-148](https://issues.folio.org/browse/MODORDERS-148) - Populate PO `dateOrdered` field field when Order is opened
 * [MODORDERS-146](https://issues.folio.org/browse/MODORDERS-146) - Return application/json (error.json schema) for all errors
 * [MODORDERS-142](https://issues.folio.org/browse/MODORDERS-142) - Implemented `GET /orders/order-lines` endpoint
 * [MODORDERS-134](https://issues.folio.org/browse/MODORDERS-134) - Assign PO Line's id to its sub-objects
 * [MODORDERS-129](https://issues.folio.org/browse/MODORDERS-129) - PO Lines: share status of corresponding PO
 * [MODORDERS-126](https://issues.folio.org/browse/MODORDERS-126) - Implemented `GET /orders/composite-orders` endpoint
 * [MODORDERS-124](https://issues.folio.org/browse/MODORDERS-124) - Redefined existing order/lines endpoints
 * [MODORDERS-121](https://issues.folio.org/browse/MODORDERS-121) - Create Instance Record in inventory when Order's status is changed to `Open`
 * [MODORDERS-117](https://issues.folio.org/browse/MODORDERS-117) - Business Logic: handle `Create Item` flag for e-Resources
 * [MODORDERS-105](https://issues.folio.org/browse/MODORDERS-105) - Implemented `GET /orders/receiving-history` endpoint
 * [MODORDERS-100](https://issues.folio.org/browse/MODORDERS-100) - Create Piece Records in Orders Storage for items quantity ordered on Order Placement
 * [MODORDERS-99](https://issues.folio.org/browse/MODORDERS-99) - Purchase Order Limit: Set system default to 1
 * [MODORDERS-96](https://issues.folio.org/browse/MODORDERS-96) - Supporting PO number prefix and suffix
 * [MODORDERS-93](https://issues.folio.org/browse/MODORDERS-93) - Assign system generated PO Line number when creating a new PO line
 * [MODORDERS-87](https://issues.folio.org/browse/MODORDERS-87) - Implemented `GET /orders/po-number` endpoint
 * [MODORDERS-72](https://issues.folio.org/browse/MODORDERS-72) - Define receiving endpoints: `/orders/receive`, `/orders/check-in` and `/orders/receiving-history`
 * [MODORDERS-67](https://issues.folio.org/browse/MODORDERS-67) - Create Item Record in inventory for physical/electronic items quantity
 * [MODORDERS-66](https://issues.folio.org/browse/MODORDERS-66) - Create Holding Record in inventory for titles ordered that are not currently represented in inventory by a Holding

### Bug Fixes
 * [MODORDERS-153](https://issues.folio.org/browse/MODORDERS-153) - PO Line's id is absent from sub-objects in response when creating new PO Line
 * [MODORDERS-145](https://issues.folio.org/browse/MODORDERS-145) - Unable to create new Purchase Order with PO Line

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
