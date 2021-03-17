## 12.1.0 - Unreleased

## 12.0.0 - Released

The focus of this release was to update RMB, support ledger fiscal year rollover and re-encumber logic. 
Also performance improvement was done for composite order API. 

[Full Changelog](https://github.com/folio-org/mod-invoice/compare/v11.1.3...v12.0.0)

### Technical tasks
* [MODORDERS-467](https://issues.folio.org/browse/MODORDERS-467) - mod-invoice: Update RMB

### Stories
* [MODORDERS-480](https://issues.folio.org/browse/MODORDERS-480) - Create encumbrances based on fundDistribution percentage value during re-encumber
* [MODORDERS-479](https://issues.folio.org/browse/MODORDERS-479) - Update fundDistributions during order rollover
* [MODORDERS-471](https://issues.folio.org/browse/MODORDERS-471) - Ensure that Allowable Encumbrance and Allowable Expenditure restrictions are based on "Total Funding"
* [MODORDERS-470](https://issues.folio.org/browse/MODORDERS-470) - Create new tags centrally when they are added from incoming POL data
* [MODORDERS-465](https://issues.folio.org/browse/MODORDERS-465) - Implement order rollover status logic for get by id
* [MODORDERS-464](https://issues.folio.org/browse/MODORDERS-464) - Implement rollover orders logic
* [MODORDERS-463](https://issues.folio.org/browse/MODORDERS-463) - Change encumbrance orderStatus when workFlowStatus is changed
* [MODORDERS-459](https://issues.folio.org/browse/MODORDERS-459) - Update POL status of "Ongoing" order to Ongoing when order is Open
* [MODORDERS-454](https://issues.folio.org/browse/MODORDERS-454) - User should NOT be able to delete an order or order line that is related to an invoice
* [MODORDERS-443](https://issues.folio.org/browse/MODORDERS-443) - Update logic for supporting manually exchange rate for purchase order line
* [MODORDERS-439](https://issues.folio.org/browse/MODORDERS-439) - Implement re-encumber orders logic
* [MODORDERS-438](https://issues.folio.org/browse/MODORDERS-438) - Define re-encumber and rollover orders business API
* [MODORDERS-391](https://issues.folio.org/browse/MODORDERS-491) - Release all encumbrances when order is closed

### Bug Fixes
* [MODORDERS-484](https://issues.folio.org/browse/MODORDERS-484) - Memory Leaks - concurrentHashMap and Vertx Internal timers not releasing heap memory in POST /composite-orders
* [MODORDERS-478](https://issues.folio.org/browse/MODORDERS-478) - Total encumbered for PO showing more than one year of encumbrances
* [MODORDERS-476](https://issues.folio.org/browse/MODORDERS-476) - Investigate invoice approval error
* [MODORDERS-472](https://issues.folio.org/browse/MODORDERS-472) - Potentially incorrect number of units/price display in order cost details

## 11.1.3 - Released
The primary focus of this release was to fix memory leaks upon loading large composite order
[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.1.2...v11.1.3)

### Bug Fixes
* [MODORDERS-484](https://issues.folio.org/browse/MODORDERS-484) - Memory Leaks - concurrentHashMap and Vertx Internal timers not releasing heap memory in POST /composite-orders


## 11.1.2 - Released
The primary focus of this release was to fix Adding certain prefix and suffixes prevents user from being able to save and currency issue for totalEstimatedPrice 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.1.1...v11.1.2)

  ### Bug Fixes
 * [MODORDERS-447](https://issues.folio.org/browse/MODORDERS-447) - totalEstimatedPrice field should be converted if polines in different currency
 * [MODORDSTOR-197](https://issues.folio.org/browse/MODORDSTOR-197) - Adding certain prefix and suffixes prevents user from being able to save

## 11.1.1 - Released
The focus of this release was to fix issue with different finance data and operations

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.1.0...v11.1.1)

### Bug Fixes
 * [MODORDERS-448](https://issues.folio.org/browse/MODORDERS-448) Encumbrance not deleted when order/order lines are deleted
 * [MODORDERS-447](https://issues.folio.org/browse/MODORDERS-447) totalEstimatedPrice field should be converted if polines in different currency
 * [MODORDERS-446](https://issues.folio.org/browse/MODORDERS-446) exchange rate not applied to encumbrance after Reopen order
 * [MODORDERS-437](https://issues.folio.org/browse/MODORDERS-437) Error must be returned in case of budget expense class mismatch
 * [MODORDERS-436](https://issues.folio.org/browse/MODORDERS-436) exchange rate not applied to encumbrance when POL in foreign currency.
 * [MODORDERS-435](https://issues.folio.org/browse/MODORDERS-435) unable to open order that references Funds from different ledgers
 * [MODORDERS-430](https://issues.folio.org/browse/MODORDERS-430) Can't Unopen order - error response 2
 * [MODORDERS-429](https://issues.folio.org/browse/MODORDERS-429) Can't Unopen order - error response
 
 
## 11.1.0 - Released
The focus of this release was to migrate to JDK 11 and RMB v31.1.1. Release also contains several bug fixes for open order.  

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.0.6...v11.1.0)

### Stories
 * [MODORDERS-419](https://issues.folio.org/browse/MODORDERS-419) Update RMB v31.1.1
 * [MODORDERS-414](https://issues.folio.org/browse/MODORDERS-414) Migrate mod-orders to JDK 11
 * [MODORDERS-406](https://issues.folio.org/browse/MODORDERS-406) Prevent the creation of an encumbrance for an inactive expense class
 * [MODORDERS-364](https://issues.folio.org/browse/MODORDERS-364) Encumbrances - tag inheritance
	
### Bug Fixes
 * [MODORDERS-432](https://issues.folio.org/browse/MODORDERS-432) Payment must be possible if invoice line contains multiple fund distribution with same fund and different expense classes
 * [MODORDERS-431](https://issues.folio.org/browse/MODORDERS-431) Piece quantity not changed, when location changed in Open order
 * [MODORDERS-424](https://issues.folio.org/browse/MODORDERS-424) Encumbrance not updated when cost or quantity of POL edited
 * [MODORDERS-423](https://issues.folio.org/browse/MODORDERS-423) Unable to open purchase order when POL currency different from system
 * [MODORDERS-420](https://issues.folio.org/browse/MODORDERS-420) Update opened order line to inactive expense class should return inactiveExpenseClass error

## 11.0.6 - Released
The focus of this release was to fix issue with updating order line for opened order

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.0.5...v11.0.6)

### Bug Fixes
 * [MODORDERS-416](https://issues.folio.org/browse/MODORDERS-416) One or more purchase order line record(s) cannot be updated during Pay of invoice
 

## 11.0.5 - Released
The focus of this release was to fix issue related with updating of order lines for open order

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.0.3...v11.0.5)

### Bug Fixes
 * [MODORDERS-411](https://issues.folio.org/browse/MODORDERS-411) Number of order transactions have to be greater than 0 upon poLine update
 
## 11.0.3 - Released
The focus of this release was to fix editing opened orders and associated order lines

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.0.1...v11.0.3)

### Bug Fixes
 * [MODORDERS-410](https://issues.folio.org/browse/MODORDERS-410) Cannot open order because systems thinks there is no current budget
 * [MODORDERS-408](https://issues.folio.org/browse/MODORDERS-408) Cannot edit opened poline with fundDistribution type=Amount


## 11.0.1 - Released 07-07-2020

The focus of this release was to fix editing opened orders and associated order lines

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.0.0...v11.0.1)

### Stories
 * [MODORDERS-402](https://issues.folio.org/browse/MODORDERS-402) Update encumbrances upon POL editing for Opened order

### Bug Fixes
 * [MODORDERS-407](https://issues.folio.org/browse/MODORDERS-407) Cannot create POL due to POL limit bug
 * [MODORDERS-400](https://issues.folio.org/browse/MODORDERS-400) Adding pieces, when the quantity increased for an order line
 * [MODORDERS-378](https://issues.folio.org/browse/MODORDERS-378) Automatically close order when fully paid and fully received

## 11.0.0 - Released
The main focus of this release was to implement new unOpen and reOpen order flow. Also RMB was updated to 30.0.0.
Now "title" and "package" order lines are supported.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v10.0.4...v11.0.0)

### Stories
 * [MODORDERS-397](https://issues.folio.org/browse/MODORDERS-397) Edit lowering quantity of items or changing location of quantities when edit POL for opened order
 * [MODORDERS-394](https://issues.folio.org/browse/MODORDERS-394) Update to RMB v30	
 * [MODORDERS-390](https://issues.folio.org/browse/MODORDERS-390) Populate titleId during piece creation
 * [MODORDERS-389](https://issues.folio.org/browse/MODORDERS-389) Unable to remove prefixes/suffixes that are currently used by an existing orders
 * [MODORDERS-388](https://issues.folio.org/browse/MODORDERS-388) Securing APIs by default
 * [MODORDERS-359](https://issues.folio.org/browse/MODORDERS-359) Support "title" and "package" order lines
 * [MODORDERS-356](https://issues.folio.org/browse/MODORDERS-356) Find/Create instance/holding/item upon piece creation and update assoc. title record
 * [MODORDERS-337](https://issues.folio.org/browse/MODORDERS-337) Check for "Orders: Unopen purchase orders" permission if order is un-opened
 * [MODORDERS-338](https://issues.folio.org/browse/MODORDERS-338) Adjust encumbrances when Order is re-opened
 * [MODORDERS-336](https://issues.folio.org/browse/MODORDERS-336) Allow to edit Vendor and Bill to on OPEN order
 * [MODORDERS-335](https://issues.folio.org/browse/MODORDERS-335) Edit quantity when Order is re-opened
 * [MODORDERS-281](https://issues.folio.org/browse/MODORDERS-281) Need the ability to change poNumber prefix/suffix on Pending orders
 * [MODORDERS-269](https://issues.folio.org/browse/MODORDERS-269) Create link to encumbrance in fundDistro, if the encumbrance is successfully created	

## 10.0.4 - Released

The main focus of this bugfix release was to fix bugs related to receiving and instances  

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v10.0.3...v10.0.4)

### Bug Fixes
 * [MODORDERS-386](https://issues.folio.org/browse/MODORDERS-386) - Create title with populated instanceId upon creation of poLine
 * [MODORDERS-384](https://issues.folio.org/browse/MODORDERS-384) - Item status not set to order closed
 * [MODORDERS-383](https://issues.folio.org/browse/MODORDERS-383) - Can not receive piece when item has been deleted
 * [MODORDERS-380](https://issues.folio.org/browse/MODORDERS-380) - Can not receive when order is closed
 * [MODORDERS-379](https://issues.folio.org/browse/MODORDERS-379) - Title in POL view in order search not hyperlinked when order creates instance
 
## 10.0.3 - Released

Bugfix release to fix dependency interface version

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v10.0.2...v10.0.3)

## 10.0.2 - Released

Bugfix release to make field for Piece optional 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v10.0.1...v10.0.2)

### Bug Fixes
 * [MODORDERS-376](https://issues.folio.org/browse/MODORDERS-376) - Make field "Piece.receiptDate" optional

## 10.0.1 - Released

Bugfix release to fix issues with PO Line update and inventory interaction during receiving and order opening

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v10.0.0...v10.0.1)

### Stories
 * [MODORDERS-360](https://issues.folio.org/browse/MODORDERS-360) - Create holding when receiving a piece and location is changed
 
### Bug Fixes
 * [MODORDERS-371](https://issues.folio.org/browse/MODORDERS-371) - Extra pieces are created for "PO Line" with type for "P/E Mix"
 * [MODORDERS-368](https://issues.folio.org/browse/MODORDERS-368) - Can't update POL with a specified instanceId
 * [MODORDERS-345](https://issues.folio.org/browse/MODORDERS-345) - Fund distribution totals must add upto 100%
 * [MODORDERS-176](https://issues.folio.org/browse/MODORDERS-176) - Failure in inventory interaction, fails the order

## 10.0.0 - Released

The main focus of this release was to introduce new APIs and business logic **orders reasons for closure, prefixes, suffixes, titles**.
The following APIs were  identified from the orders API without changing URLs: **receive, pieces, po number and order lines**.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v9.0.0...v10.0.0)

### Stories
 * [MODORDERS-354](https://issues.folio.org/browse/MODORDERS-354) - Divide the interface into smaller ones
 * [MODORDERS-353](https://issues.folio.org/browse/MODORDERS-353) - Migrate to new major version of item-storage, inventory, circulation
 * [MODORDERS-351](https://issues.folio.org/browse/MODORDERS-351) - Validate renewal/subscription information
 * [MODORDERS-350](https://issues.folio.org/browse/MODORDERS-350) - Update piece schema (receipt date)
 * [MODORDERS-349](https://issues.folio.org/browse/MODORDERS-349) - Make title schema changes to all APIs
 * [MODORDERS-348](https://issues.folio.org/browse/MODORDERS-348) - CRUD APIs for titles
 * [MODORDERS-346](https://issues.folio.org/browse/MODORDERS-346) - Schema changes for check-in and Receiving Packages
 * [MODORDERS-342](https://issues.folio.org/browse/MODORDERS-342) - Check enough money in budgets while creating encumbrances
 * [MODORDERS-341](https://issues.folio.org/browse/MODORDERS-341) - Set item status to "Order closed"
 * [MODORDERS-340](https://issues.folio.org/browse/MODORDERS-340) - Add call number while receiving and checkin
 * [MODORDERS-328](https://issues.folio.org/browse/MODORDERS-328) - Migrate orders settings from mod-configuration
 * [MODORDERS-315](https://issues.folio.org/browse/MODORDERS-315) - Default to "In process" for item status during receiving/check-in
 
### Bug Fixes
 * [MODORDERS-264](https://issues.folio.org/browse/MODORDERS-264) - Order creates instance when it's not supposed to

## 9.0.0 - Released
The primary focus of this release was to re-integration with encumbrance API, implementation of Order-Templates API and business logic improvements.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v8.0.2...v9.0.0)

### Stories
 * [MODORDERS-339](https://issues.folio.org/browse/MODORDERS-339) - Use JVM features to manage container memory
 * [MODORDERS-334](https://issues.folio.org/browse/MODORDERS-334) - Update purchase order number pattern
 * [MODORDERS-316](https://issues.folio.org/browse/MODORDERS-316) - Prevent duplicate ISBNs
 * [MODORDERS-310](https://issues.folio.org/browse/MODORDERS-310) - Update mod-orders to use the new version of the schema with qualifier field
 * [MODORDERS-307](https://issues.folio.org/browse/MODORDERS-307) - Fund Distribution schema changes
 * [MODORDERS-303](https://issues.folio.org/browse/MODORDERS-303) - Perform API tests in separate tenant
 * [MODORDERS-302](https://issues.folio.org/browse/MODORDERS-302) - Create order-templates API
 * [MODORDERS-299](https://issues.folio.org/browse/MODORDERS-299) - Re-integrate with encumbrance API
 
### Bug Fixes
 * [MODORDERS-343](https://issues.folio.org/browse/MODORDERS-343) - Cannot open order that is not connected to an instance

## 8.0.2 - Released
Bugfix release to honor createInventory irrespective of Receipt Status

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v8.0.1...v8.0.2)

### Bug Fixes
 * [MODORDERS-304](https://issues.folio.org/browse/MODORDERS-304) - `Receipt Not Required` doesn't consider CreateInventory Settings
 
## 8.0.1 - Released
Bugfix release to honor createInventory irrespective of Receipt Status

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v8.0.0...v8.0.1)

### Bug Fixes
 * [MODORDERS-304](https://issues.folio.org/browse/MODORDERS-304) - `Receipt Not Required` doesn't consider CreateInventory Settings

## 8.0.0 - Released
The primary focus of this release was to implement Teams-based operations restriction logic and business logic improvements.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v7.0.1...v8.0.0)

### Stories
 * [MODORDERS-298](https://issues.folio.org/browse/MODORDERS-298) - Temporarily disable interaction with encumbrance APIs
 * [MODORDERS-294](https://issues.folio.org/browse/MODORDERS-294) - Disallow "isDeleted" acq units to be assigned to orders
 * [MODORDERS-293](https://issues.folio.org/browse/MODORDERS-293) - Acquisition units - soft delete
 * [MODORDERS-291](https://issues.folio.org/browse/MODORDERS-291) - Add approvedBy and approvalDate fields to purchase_order/composite_purchase_order
 * [MODORDERS-290](https://issues.folio.org/browse/MODORDERS-290) - Enforce new setting: approval required to open orders
 * [MODORDERS-286](https://issues.folio.org/browse/MODORDERS-286) - Restrict receive/checkin based on acquisitions units
 * [MODORDERS-283](https://issues.folio.org/browse/MODORDERS-283) - Validate/Normalize ISBNs upon order creation/update
 * [MODORDERS-280](https://issues.folio.org/browse/MODORDERS-280) - Orders App: Ability to assign tags to a Purchase Order (PO) and Purchase Order Line (POL)
 * [MODORDERS-256](https://issues.folio.org/browse/MODORDERS-256) - Restrict search/view of PO, POL, Piece records based upon acquisitions unit
 * [MODORDERS-255](https://issues.folio.org/browse/MODORDERS-255) - Restrict deletion of PO, POL, Piece records based upon acquisitions unit
 * [MODORDERS-254](https://issues.folio.org/browse/MODORDERS-254) - Restrict updates of PO, POL, Piece records based upon acquisitions unit
 * [MODORDERS-251](https://issues.folio.org/browse/MODORDERS-251) - Restrict creation of PO, POL, Piece records based upon acquisitions unit

## 7.0.1 - Released
This release includes bug fix for issue related to order's closing.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v7.0.0...v7.0.1)

### Bug Fixes
 * [MODORDERS-284](https://issues.folio.org/browse/MODORDERS-284) - `closeReason` is protected, so unable to close Order

## 7.0.0 - Released
The primary focus of this release was to implement CRUD API for managing Teams (Units, Assignments, Memberships) and business logic improvements.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v6.0.0...v7.0.0)

### Stories
 * [MODORDERS-277](https://issues.folio.org/browse/MODORDERS-277) - Update inventory interface version
 * [MODORDERS-275](https://issues.folio.org/browse/MODORDERS-275) - Make poLine.source an enum
 * [MODORDERS-267](https://issues.folio.org/browse/MODORDERS-267) - Add purchaseOrder.template field
 * [MODORDERS-266](https://issues.folio.org/browse/MODORDERS-266) - Implement basic CRUD for `/orders/acquisitions-unit-assignments`
 * [MODORDERS-265](https://issues.folio.org/browse/MODORDERS-265) - Restrict which fields can be edited once worflowStatus = Open
 * [MODORDERS-250](https://issues.folio.org/browse/MODORDERS-250) - Implement basic CRUD for `/acquisitions-units/memberships`
 * [MODORDERS-249](https://issues.folio.org/browse/MODORDERS-249) - Implement basic CRUD for `/acquisitions-units/units`
 * [MODORDERS-247](https://issues.folio.org/browse/MODORDERS-247) - Make contributor-name-type configurable
 * [MODORDERS-243](https://issues.folio.org/browse/MODORDERS-243) - Create encumbrance upon order transition to "Open"
 * [MODORDERS-173](https://issues.folio.org/browse/MODORDERS-173) - ensure receiptStatus consistency between piece and poLine

## 6.0.0 - Released
The main focus of this release was to implement the search and filtering of order-lines, improve receiving/check-in flows. 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v5.0.0...v6.0.0)

### Stories
 * [MODORDERS-257](https://issues.folio.org/browse/MODORDERS-257) - Implement PUT and DELETE /orders/pieces/id
 * [MODORDERS-253](https://issues.folio.org/browse/MODORDERS-253) - Purchase order: bill to and ship to addresses
 * [MODORDERS-252](https://issues.folio.org/browse/MODORDERS-252) - Make product id type to be uuid
 * [MODORDERS-246](https://issues.folio.org/browse/MODORDERS-246) - Create a script to generate and load performance test data
 * [MODORDERS-242](https://issues.folio.org/browse/MODORDERS-242) - PoLine FundDistribution schema updates
 * [MODORDERS-241](https://issues.folio.org/browse/MODORDERS-241) - Make loan-type configurable
 * [MODORDERS-240](https://issues.folio.org/browse/MODORDERS-240) - Make instance-status configurable
 * [MODORDERS-239](https://issues.folio.org/browse/MODORDERS-239) - Make instance-type configurable
 * [MODORDERS-237](https://issues.folio.org/browse/MODORDERS-237) - Move "Owner" field to PO level
 * [MODORDERS-234](https://issues.folio.org/browse/MODORDERS-234) - Update PO status based on receipt/payment status - Part 2
 * [MODORDERS-233](https://issues.folio.org/browse/MODORDERS-233) - Add tenantId to cache keys
 * [MODORDERS-229](https://issues.folio.org/browse/MODORDERS-229) - Sort PO Lines array by POLNumber in composite PO GET by id response
 * [MODORDERS-228](https://issues.folio.org/browse/MODORDERS-228) - Search & Filter: Call GET /orders-storage/order-lines
 * [MODORDERS-221](https://issues.folio.org/browse/MODORDERS-221) - Ability to change locations in receiving/check-in flows
 
### Bug Fixes
 * [MODORDERS-244](https://issues.folio.org/browse/MODORDERS-244) - Missing module permission for getting contributor-name-types
 * [MODORDERS-235](https://issues.folio.org/browse/MODORDERS-235) - Instances being duplicated when ordering from existing instance
 * [MODORDERS-232](https://issues.folio.org/browse/MODORDERS-232) - Inappropriate error response on missing loan-type
 * [MODORDERS-231](https://issues.folio.org/browse/MODORDERS-231) - Inappropriate error response on missing instance-type
 * [MODORDERS-230](https://issues.folio.org/browse/MODORDERS-230) - Inappropriate error response on missing instance-status

## 5.0.0 - Released
The primary focus of this release was to implement check-in flow and accommodate increased flexibility in inventory integration. Also mod-organizations storage is used now instead of deprecated mod-vendors.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v4.0.0...v5.0.0)

### Stories
 * [MODORDERS-227](https://issues.folio.org/browse/MODORDERS-227) - Search & Filter purchase orders by query
 * [MODORDERS-225](https://issues.folio.org/browse/MODORDERS-225) - Switch to mod-organizations-storage
 * [MODORDERS-223](https://issues.folio.org/browse/MODORDERS-223) - Use Json API for loading module schemas
 * [MODORDERS-222](https://issues.folio.org/browse/MODORDERS-222) - Order Line has inactive Access Provider
 * [MODORDERS-218](https://issues.folio.org/browse/MODORDERS-218) - Automatically adjust order status based on poLine's paymentStatus and receivingStatus
 * [MODORDERS-212](https://issues.folio.org/browse/MODORDERS-212) - Split up unit tests
 * [MODORDERS-211](https://issues.folio.org/browse/MODORDERS-211) - Order Status: Align POL Payment and Receipt status with PO Status when opening order
 * [MODORDERS-210](https://issues.folio.org/browse/MODORDERS-210) - Make locationId as not required in Check-in flow
 * [MODORDERS-199](https://issues.folio.org/browse/MODORDERS-199) - VendorHelper refactoring in accordance with the general helpers structure
 * [MODORDERS-195](https://issues.folio.org/browse/MODORDERS-195) - Need the ability to specify materialType for physical and eresource
 * [MODORDERS-194](https://issues.folio.org/browse/MODORDERS-194) - Adjustments to piece and receiving-history regarding piece vs order type
 * [MODORDERS-179](https://issues.folio.org/browse/MODORDERS-179) - Allow tenant-specific defaults for inventory integration
 * [MODORDERS-178](https://issues.folio.org/browse/MODORDERS-178) - Increased flexibility in inventory integration
 * [MODORDERS-164](https://issues.folio.org/browse/MODORDERS-164) - Implement receiving flow for Other
 * [MODORDERS-136](https://issues.folio.org/browse/MODORDERS-136) - Define required fields in the schemas
 * [MODORDERS-104](https://issues.folio.org/browse/MODORDERS-104) - Implement check-in flow

### Bug Fixes
 * [MODORDERS-209](https://issues.folio.org/browse/MODORDERS-209) - Order placement hangs if create_inventory is not set.
 * [MODORDERS-208](https://issues.folio.org/browse/MODORDERS-208) - Vendor Id isn't required but should be
 * [MODORDERS-206](https://issues.folio.org/browse/MODORDERS-206) - Missing modulePermission in POST /orders/composite-orders
 * [MODORDERS-204](https://issues.folio.org/browse/MODORDERS-204) - When creating a brief order instance record in Inventory, data on Contributors are not populated
 * [MODORDERS-201](https://issues.folio.org/browse/MODORDERS-201) - Incorrect PO total estimated cost in some cases
 * [MODORDERS-189](https://issues.folio.org/browse/MODORDERS-189) - No correlation between location and cost quantity

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
