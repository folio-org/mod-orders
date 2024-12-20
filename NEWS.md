## 13.0.0 - Unreleased
* [MODORDERS-1215](https://issues.folio.org/browse/MODORDERS-1215) Assign to system user permissions required for data import

## 12.9.0 - Released (Ramsons R2 2024)
The primary focus of this release was to enhance multi-tenant functionality, improve piece management, and implement various inventory and order processing features.

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.8.0...v12.9.0)

### New APIs versions
* Requires `holdings-storage v8.0`

### Stories
* [MODORDERS-1194](https://issues.folio.org/browse/MODORDERS-1194) - Update libraries of dependant acq modules to the latest versions
* [MODORDERS-1191](https://issues.folio.org/browse/MODORDERS-1191) - Update total expended amount calculation for Purchase order
* [MODORDERS-1190](https://issues.folio.org/browse/MODORDERS-1190) - Fund distribution should not be changed in invoice line when fund was changed in related open POL
* [MODORDERS-1188](https://issues.folio.org/browse/MODORDERS-1188) - Apply restriction to edit Locations based on user affiliations
* [MODORDERS-1183](https://issues.folio.org/browse/MODORDERS-1183) - Introduce new error code for missed affiliations
* [MODORDERS-1182](https://issues.folio.org/browse/MODORDERS-1182) - Bump pieces interfaces minor version to 3.1 after new endpoint was added
* [MODORDERS-1180](https://issues.folio.org/browse/MODORDERS-1180) - Populate discoverySuppress as false by default when creating Instance from an Order
* [MODORDERS-1174](https://issues.folio.org/browse/MODORDERS-1174) - Filter pieces based on user affiliations
* [MODORDERS-1170](https://issues.folio.org/browse/MODORDERS-1170) - Orders app: Order not auto-closing as complete despite not receiving/payment requirements
* [MODORDERS-1166](https://issues.folio.org/browse/MODORDERS-1166) - Add bindItemTenantId to Piece schema
* [MODORDERS-1163](https://issues.folio.org/browse/MODORDERS-1163) - Edited location should displayed in related ongoing / cancelled PO line after receiving piece from full screen form
* [MODORDERS-1161](https://issues.folio.org/browse/MODORDERS-1161) - Error is not displayed when user goes to receiving title details with pieces from tenant where user has no affiliation
* [MODORDERS-1160](https://issues.folio.org/browse/MODORDERS-1160) - Unable to edit PO line when details field is empty
* [MODORDERS-1151](https://issues.folio.org/browse/MODORDERS-1151) - Central ordering - Opening and editing POs with location-restricted funds (FE)
* [MODORDERS-1146](https://issues.folio.org/browse/MODORDERS-1146) - Add link to original item for piece that is bound
* [MODORDERS-1145](https://issues.folio.org/browse/MODORDERS-1145) - Add Instance Title to Context for template request
* [MODORDERS-1144](https://issues.folio.org/browse/MODORDERS-1144) - Bound piece having related item with barcode can be removed from "Bound pieces data" accordion
* [MODORDERS-1138](https://issues.folio.org/browse/MODORDERS-1138) - Implement endpoint to fetch Circulation requests for pieces
* [MODORDERS-1133](https://issues.folio.org/browse/MODORDERS-1133) - Add support of creating holding in member tenants for Receiving
* [MODORDERS-1131](https://issues.folio.org/browse/MODORDERS-1131) - Fix piece, item, title relationship in bind piece features
* [MODORDERS-1129](https://issues.folio.org/browse/MODORDERS-1129) - Order with newly created title (free-text) and location from member tenant can be opened
* [MODORDERS-1122](https://issues.folio.org/browse/MODORDERS-1122) - Add logic to create item in any tenant for binding
* [MODORDERS-1120](https://issues.folio.org/browse/MODORDERS-1120) - Support populating searchLocationId for correct tenant during order opening
* [MODORDERS-1118](https://issues.folio.org/browse/MODORDERS-1118) - Fix and optimize getting finance data to process encumbrances
* [MODORDERS-1115](https://issues.folio.org/browse/MODORDERS-1115) - Extend bind pieces endpoint with functionality when items are associated
* [MODORDERS-1114](https://issues.folio.org/browse/MODORDERS-1114) - Enhance bind-pieces endpoint with flag to transfer circulation requests
* [MODORDERS-1108](https://issues.folio.org/browse/MODORDERS-1108) - Update PiecesAPI CRUD methods to process inventory in desired tenant
* [MODORDERS-1105](https://issues.folio.org/browse/MODORDERS-1105) - Update logic to handle item status "On order" "Order closed" in correct tenant
* [MODORDERS-1104](https://issues.folio.org/browse/MODORDERS-1104) - Integrate with Transfer API for moving circulation requests to new item
* [MODORDERS-1103](https://issues.folio.org/browse/MODORDERS-1103) - Update UnOpenCompositeOrderManager::deleteHoldings to use tenantId parameter
* [MODORDERS-1096](https://issues.folio.org/browse/MODORDERS-1096) - Add logic to update related item in desired tenant during piece receiving
* [MODORDERS-1095](https://issues.folio.org/browse/MODORDERS-1095) - Split InventoryManager god class into subclasses
* [MODORDERS-1084](https://issues.folio.org/browse/MODORDERS-1084) - Create Inventory objects in appropriate tenant from location during UnOpen order
* [MODORDERS-1081](https://issues.folio.org/browse/MODORDERS-1081) - Create instance when user adds receiving title to package order line
* [MODORDERS-1072](https://issues.folio.org/browse/MODORDERS-1072) - Add "Display to public" to the Piece schema with validation logic
* [MODORDERS-1048](https://issues.folio.org/browse/MODORDERS-1048) - Create Inventory objects in appropriate tenant from location for Change Instance Connection
* [MODORDERS-1047](https://issues.folio.org/browse/MODORDERS-1047) - Create Inventory objects in appropriate tenant from location during Open order
* [MODORDERS-1046](https://issues.folio.org/browse/MODORDERS-1046) - Retrieve holdings from member tenants for Restrict by Locations feature
* [MODORDERS-1026](https://issues.folio.org/browse/MODORDERS-1026) - Implement API to execute mod-template-engine request
* [MODORDERS-1015](https://issues.folio.org/browse/MODORDERS-1015) - Implement endpoint to bind multiple pieces together(no items associated)
* [MODORDERS-1014](https://issues.folio.org/browse/MODORDERS-1014) - Add "Bindery active" flag in POL with validation
* [MODORDERS-1006](https://issues.folio.org/browse/MODORDERS-1006) - Create Routing Lists API

### Bug Fixes
* [MODORDERS-1195](https://issues.folio.org/browse/MODORDERS-1195) - Closed purchase order line associated with a deleted piece record cannot be saved
* [MODORDERS-1152](https://issues.folio.org/browse/MODORDERS-1152) - Fix additional fields in EDIFACT order syntax
* [MODORDERS-1149](https://issues.folio.org/browse/MODORDERS-1149) - 500 response when changing the instance connection with "Create new" option
* [MODORDERS-1140](https://issues.folio.org/browse/MODORDERS-1140) - Item status is "On order" when add piece for cancelled order
* [MODORDERS-1130](https://issues.folio.org/browse/MODORDERS-1130) - ECS | Order can not be opened if contains three locations from different tenants
* [MODORDERS-1102](https://folio-org.atlassian.net/browse/MODORDERS-1102) - Order lines search results do not display fund code for orders created by data import
* [MODORDERS-1090](https://folio-org.atlassian.net/browse/MODORDERS-1090) - Quantity = 0 for electronic format orders

### Dependencies
* Bump `raml` from `35.2.0` to `35.3.0`
* Added `folio-module-descriptor-validator` version `1.0.0`


## 12.8.0 - Released (Quesnelia R1 2024)
This release focused on fixing several bugs as well as implement new features and upgrading dependent libraries

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.7.0...v12.8.0)

### New APIs versions

* Requires `orders-storage.pieces v5.0`
* Requires `orders-storage.receiving-history v4.0`
* Requires `orders-storage.po-line-number v2.0`
* Requires `orders-storage.order-invoice-relationships v1.0`
* Requires `finance.fiscal-years v2.0`
* Requires `finance-storage.ledgers v4.0`
* Provides `orders.receiving v2.0`
* Provides `orders.pieces v3.0`

### Stories

* [MODORDERS-1052](https://folio-org.atlassian.net/browse/MODORDERS-1052) - Upgrade RAML Module Builder for Quesnelia
* [MODORDERS-1044](https://folio-org.atlassian.net/browse/MODORDERS-1044) - Do not update orderStatus for past fiscal years
* [MODORDERS-1043](https://folio-org.atlassian.net/browse/MODORDERS-1043) - Remove pending payment encumbrance links when encumbrances are deleted
* [MODORDERS-1036](https://folio-org.atlassian.net/browse/MODORDERS-1036) - Include already existing holdings to validation process
* [MODORDERS-1032](https://folio-org.atlassian.net/browse/MODORDERS-1032) - Protect Holdings Receiving Histrory with Title acq units
* [MODORDERS-1031](https://folio-org.atlassian.net/browse/MODORDERS-1031) - Create Kafka topics instead of relying on auto create in mod-orders
* [MODORDERS-1028](https://folio-org.atlassian.net/browse/MODORDERS-1028) - Add specific error code of barcode uniqueness error error during receiving
* [MODORDERS-1027](https://folio-org.atlassian.net/browse/MODORDERS-1027) - Add logic to update POL receipt status when moving pieces between Unreceivable and Expected
* [MODORDERS-1022](https://folio-org.atlassian.net/browse/MODORDERS-1022) - Add receipt Date to the CheckIng Piece schema
* [MODORDERS-1020](https://folio-org.atlassian.net/browse/MODORDERS-1020) - Adjust Restricted Location + Fund validation rules
* [MODORDERS-1010](https://folio-org.atlassian.net/browse/MODORDERS-1010) - Change "caption" to "Display summary" for checkin and receiving collections
* [MODORDERS-1005](https://folio-org.atlassian.net/browse/MODORDERS-1005) - Check acq units from Title when receiving/unreceicing pieces
* [MODORDERS-1000](https://folio-org.atlassian.net/browse/MODORDERS-1000) - Populate Item "Display summary" field with Piece "Display summary" field value
* [MODORDERS-989](https://folio-org.atlassian.net/browse/MODORDERS-989) - Implement batch endpoint to move multiple pieces to Expected status
* [MODORDERS-985](https://folio-org.atlassian.net/browse/MODORDERS-985) - Add internal note and external note in Piece schema
* [MODORDERS-983](https://folio-org.atlassian.net/browse/MODORDERS-983) - Receiving a piece on a closed/cancelled PO will reopen the PO
* [MODORDERS-980](https://folio-org.atlassian.net/browse/MODORDERS-980) - Update dto schema with recent changes for holdingsItems
* [MODORDERS-974](https://folio-org.atlassian.net/browse/MODORDERS-974) - Add validation for claimingActive, claimingInterval fields
* [MODORDERS-972](https://folio-org.atlassian.net/browse/MODORDERS-972) - Protect Title acq units management with permissions
* [MODORDERS-970](https://folio-org.atlassian.net/browse/MODORDERS-970) - Do not overwrite Item fields when piece fields are null/empty
* [MODORDERS-969](https://folio-org.atlassian.net/browse/MODORDERS-969) - Opening and editing POs with location-restricted funds
* [MODORDERS-963](https://folio-org.atlassian.net/browse/MODORDERS-963) - Update PUT piece to check acq unit from title instead of order
* [MODORDERS-962](https://folio-org.atlassian.net/browse/MODORDERS-962) - Update Get titles query to check acq units from title instead of purchase order
* [MODORDERS-892](https://folio-org.atlassian.net/browse/MODORDERS-892) - Accumulate all transactions in holder to make only single call to mod-finance
* [MODORDERS-886](https://folio-org.atlassian.net/browse/MODORDERS-886) - The unclear error message is displayed  when a user is trying to save the order template with already existing name
* [MODORDERS-846](https://folio-org.atlassian.net/browse/MODORDERS-846) - Add validation back for deleting encumbrance when expended > 0
* [MODORDSTOR-360](https://folio-org.atlassian.net/browse/MODORDSTOR-360) - Inherit acqUnitIds from Purchase Order when creating new Title
* [MODDATAIMP-957](https://folio-org.atlassian.net/browse/MODDATAIMP-957) - Test, make adjustments and merge PRs for removing initial saving of records in SRS

### Bug Fixes

* [MODORDERS-1055](https://folio-org.atlassian.net/browse/MODORDERS-1055) - orderStatus is not always updated when reopening an order
* [MODORDERS-1053](https://folio-org.atlassian.net/browse/MODORDERS-1053) - Missing interface dependencies in module descriptor
* [MODORDERS-1049](https://folio-org.atlassian.net/browse/MODORDERS-1049) - Rollback inventory if open fails to create encumbrances
* [MODORDERS-1039](https://folio-org.atlassian.net/browse/MODORDERS-1039) - Encumbrance expense class not updated after a change to the fund distribution
* [MODORDERS-1030](https://folio-org.atlassian.net/browse/MODORDERS-1030) - "Receipt status" is changed to "Fully received" when all piece statuses for selected title were changed to "Claim delayed" and "Claim sent"
* [MODORDERS-1024](https://folio-org.atlassian.net/browse/MODORDERS-1024) - Error message appears when open unopened (duplicated) P/E mix order with same location both for physical and electronic resources
* [MODORDERS-986](https://folio-org.atlassian.net/browse/MODORDERS-986) - Error thrown during change instance connection
* [MODORDERS-984](https://folio-org.atlassian.net/browse/MODORDERS-984) - Reciept status remains "Fully received" after unreceiving piece
* [MODORDERS-967](https://folio-org.atlassian.net/browse/MODORDERS-967) - Defect in Order Receipt Status Update for 'Ongoing' Orders
* [MODORDERS-669](https://folio-org.atlassian.net/browse/MODORDERS-669) - Cannot delete a piece in receiving
* [MODDICORE-373](https://folio-org.atlassian.net/browse/MODDICORE-373) - The import of file is completed with errors

### Tech debts

* [MODORDERS-852](https://folio-org.atlassian.net/browse/MODORDERS-852) - Some unit tests are not executed

### Dependencies

* Bump `raml` from `35.0.1` to `35.2.0`
* Bump `vertx` from `4.3.4` to `4.5.4`
* Bump `data-import-processing-core` from `4.1.0` to `4.2.0`

## 12.7.0 - Released (Poppy R2 2023)

This release focused on fixing several bugs in rollover and implement new feature relate to POL or other parts 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.6.0...v12.7.0)

### Stories

* [MODORDERS-927](https://issues.folio.org/browse/MODORDERS-927) - Implement interception and update of invalid ISBNs during the POL update process
* [MODORDERS-912](https://issues.folio.org/browse/MODORDERS-912) - Add uniqueness check for retrieving transactions
* [MODORDERS-907](https://issues.folio.org/browse/MODORDERS-647) - Use new orders-storage endpoint to update order lines in batch during rollover
* [MODORDERS-905](https://issues.folio.org/browse/MODORDERS-905) - Persist rollover failures per poLine
* [MODORDERS-904](https://issues.folio.org/browse/MODORDERS-904) - Added filtering by fundDistribution encumbrance presence
* [MODORDERS-903](https://issues.folio.org/browse/MODORDERS-903) - Updated composite order conversion
* [MODORDERS-899](https://issues.folio.org/browse/MODORDERS-899) - Update to Java 17 mod-orders
* [MODORDERS-887](https://issues.folio.org/browse/MODORDERS-887) - Changing interface version
* [MODORDERS-882](https://issues.folio.org/browse/MODORDERS-882) - Update the error message displayed when importer is not a member of the specified acq unit
* [MODORDERS-881](https://issues.folio.org/browse/MODORDERS-881) - Populate locations and material type for POLs when multiple Holdings/Items were created
* [MODORDERS-879](https://issues.folio.org/browse/MODORDERS-879) - Prevent creating an order using 2 different fiscal years with clear error message
* [MODORDERS-878](https://issues.folio.org/browse/MODORDERS-878) - Adjust prefix and suffix for generated poNumber
* [MODORDERS-876](https://issues.folio.org/browse/MODORDERS-876) - Adjust material type and locations from existing holdings and item
* [MODORDERS-871](https://issues.folio.org/browse/MODORDERS-871) - Finding holdings record during import open P/E mix orderLine
* [MODORDERS-865](https://issues.folio.org/browse/MODORDERS-865) - Rewrite the orders rollover interaction in an asynchronous way
* [MODORDERS-860](https://issues.folio.org/browse/MODORDERS-860) - Adjust POL "create inventory" field override
* [MODORDERS-859](https://issues.folio.org/browse/MODORDERS-859) - Release encumbrance when orderStatus is set to CLOSED
* [MODORDERS-859](https://issues.folio.org/browse/MODORDERS-859) - Receiving: added permission for transaction summaries
* [MODORDERS-858](https://issues.folio.org/browse/MODORDERS-858) - Increase a memory to Snapshots
* [MODORDERS-855](https://issues.folio.org/browse/MODORDERS-855) - fixed vertx semaphores
* [MODORDERS-828](https://issues.folio.org/browse/MODORDERS-825) - Update dependent raml-util
* [MODORDERS-786](https://issues.folio.org/browse/MODORDERS-786) - Upgrade di-utils to v1.11.0
* [MODORDERS-405](https://issues.folio.org/browse/MODORDERS-405) - Composite orders web API updates budget objects with invalid date updated values
* [MODORDERS-316](https://issues.folio.org/browse/MODORDERS-316) - Using new proxy endpoints for ledger rollover errors
* [MODORDERS-307](https://issues.folio.org/browse/MODORDERS-307) - Update dependency
* [MODORDERS-162](https://issues.folio.org/browse/MODORDERS-162) - Updated models
* [MODFIN-323](https://issues.folio.org/browse/MODFIN-323) - Delete unnecessary code
* [UIOR-1102](https://issues.folio.org/browse/UIOR-1102) - Ability to edit Ongoing order information of Open orders

### Bug Fixes

* [MODORDERS-929](https://issues.folio.org/browse/MODORDERS-929) - An error appears after rollover when open order/order line having two fund distributions related to different ledgers
* [MODORDERS-928](https://issues.folio.org/browse/MODORDERS-928) - Can not delete acquisition methods with Order settings "All" permission
* [MODORDERS-920](https://issues.folio.org/browse/MODORDERS-920) - An error appears after rollover when open order/order line having two fund distributions related to different ledgers
* [MODORDERS-915](https://issues.folio.org/browse/MODORDERS-915) - NPE in OrderReEncumberService.java:245
* [MODORDERS-913](https://issues.folio.org/browse/MODORDERS-913) - Encumbrance became "Release" when changing fund distribution in order with related paid invoice having NOT checked "Release encumbrance" option
* [MODORDERS-914](https://issues.folio.org/browse/MODORDERS-914) - A user with order create permission can not add suffix and prefix to the order
* [MODORDERS-908](https://issues.folio.org/browse/MODORDERS-908) - Cannot load cache for mapping organizations when importing Orders (Orchid CSP 5 Clone)
* [MODORDERS-902](https://issues.folio.org/browse/MODORDERS-902) - Allow user to fully edit location details when receiving workflow = Independent order and receipt quantity.
* [MODORDERS-901](https://issues.folio.org/browse/MODORDERS-901) - zero division fix
* [MODORDERS-894](https://issues.folio.org/browse/MODORDERS-894) - Adding fund distribution line in the POL breaks the order record. Can no longer open.
* [MODORDERS-890](https://issues.folio.org/browse/MODORDERS-890) - When using POL "Change instance" the product ID can be made invalid blocking Invoice approve and pay 
* [MODORDERS-865](https://issues.folio.org/browse/MODORDERS-514) - fixed permissions and request routing
* [MODORDERS-863](https://issues.folio.org/browse/MODORDERS-863) - Error after importing open orders for the second time
* [MODORDERS-862](https://issues.folio.org/browse/MODORDERS-859) - Fixed receiving more than 15 pieces
* [MODORDERS-855](https://issues.folio.org/browse/MODORDERS-859) - fixed Future usage
* [MODORDERS-514](https://issues.folio.org/browse/MODORDERS-514) - Closing a composite order can fail silently when lines are included
* [MODORDERS-494](https://issues.folio.org/browse/MODORDERS-494) - Unexpected jump in POL number on 2 line PO
* [FAT-4999](https://issues.folio.org/browse/FAT-4999) - Added missing permission for check-in
* [FAT-4863](https://issues.folio.org/browse/FAT-4863) - fixed orders chunk counting
* [FAT-4844](https://issues.folio.org/browse/FAT-4844) - updated permissions

### Tech debt

* [MODORDERS-750](https://issues.folio.org/browse/MODORDERS-750) - Introduce Caffeine Cache for inventory entry types
* [MODORDERS-865](https://issues.folio.org/browse/MODORDERS-865) - added missed permission
* [MODORDERS-850](https://issues.folio.org/browse/MODORDERS-850) - Refactor inventoryManager in order to support caching

### Dependencies

* Bump `java version` from `11` to `17`
* Bump `data-import-utils` from `1.9.0` to `1.11.0`
* Bump `data-import-processing-core` from `4.0.1` to `4.1.0`

## 12.6.0 Orchid R1 2023 - Released
This release focused on replacing FolioVertxCompletableFuture usage refactoring, edit fund distributions with related invoices and orders import features

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.5.0...v12.6.0)

### Stories
* [MODORDERS-844](https://issues.folio.org/browse/MODORDERS-844) - Add cache to store organisations info for mapping
* [MODORDERS-843](https://issues.folio.org/browse/MODORDERS-843) - Adjust deduplication mechanism to store info by POLine id
* [MODORDERS-842](https://issues.folio.org/browse/MODORDERS-842) - Incorrect displaying initial amount encumbered after changing distribution percentage
* [MODORDERS-840](https://issues.folio.org/browse/MODORDERS-840) - Extend error handling logic if can't create OrderLine
* [MODORDERS-834](https://issues.folio.org/browse/MODORDERS-834) - Flow #2. Copy the amount awaiting payment and expended from the original encumbrance
* [MODORDERS-820](https://issues.folio.org/browse/MODORDERS-820) - Introduce "deleteHolding" param to orders put endpoint
* [MODORDERS-825](https://issues.folio.org/browse/MODORDERS-825) - Align the module with API breaking change
* [MODORDERS-823](https://issues.folio.org/browse/MODORDERS-823) - Implement logic that defines which event should be issued as a result of saving Pending Orders
* [MODORDERS-821](https://issues.folio.org/browse/MODORDERS-821) - Implement "Create new holdings for location" flag
* [MODORDERS-817](https://issues.folio.org/browse/MODORDERS-817) - Associate mapped POLines with single Order based on POLine limit value
* [MODORDERS-808](https://issues.folio.org/browse/MODORDERS-808) - Create "OrdersPostProcessingEventHandler" for mod-orders
* [MODORDERS-807](https://issues.folio.org/browse/MODORDERS-807) - Copy number does not get applied to item when creating item through receiving app
* [MODORDERS-805](https://issues.folio.org/browse/MODORDERS-805) - Replace FolioVertxCompletableFuture usage - merge
* [MODORDERS-804](https://issues.folio.org/browse/MODORDERS-804) - Remove validation for editing fund distribution when related invoice is approved or paid
* [MODORDERS-803](https://issues.folio.org/browse/MODORDERS-803) - Implement Edit Fund Destribution blocking action (FLOW #3)
* [MODORDERS-801](https://issues.folio.org/browse/MODORDERS-801) - Check and correct encumbrance ID links for invoices with Open or Reviewed status (FLOW #4)
* [MODORDERS-800](https://issues.folio.org/browse/MODORDERS-800) - Implement moving encumbered value from budget 1 to budget 2 (FLOW #2)
* [MODORDERS-789](https://issues.folio.org/browse/MODORDERS-789) - Creating large amounts of polines performance issue
* [MODORDERS-788](https://issues.folio.org/browse/MODORDERS-788) - Implement fetching related invoices on PUT PO Line method to be able to implement all futher flows
* [MODORDERS-784](https://issues.folio.org/browse/MODORDERS-784) - Logging improvement - Configuration
* [MODORDERS-783](https://issues.folio.org/browse/MODORDERS-783) - Implement Orders Export History API
* [MODORDERS-780](https://issues.folio.org/browse/MODORDERS-780) - Replace FolioVertxCompletableFuture usage
* [MODORDERS-775](https://issues.folio.org/browse/MODORDERS-775) - Add deduplication mechanism for Create Order event
* [MODORDERS-773](https://issues.folio.org/browse/MODORDERS-773) - Implement action profile handler for order creation
* [MODORDERS-724](https://issues.folio.org/browse/MODORDERS-724) - Replace use of deprecated class HttpClientFactory
* [MODORDERS-696](https://issues.folio.org/browse/MODORDERS-696) - Replace all utility methods for HTTP calls from HelperUtils
* [MODORDERS-647](https://issues.folio.org/browse/MODORDERS-647) - Implement Find or create logic for Holdings generated through the GOBI API

### Bug Fixes
* [MODORDERS-841](https://issues.folio.org/browse/MODORDERS-841) - Encumbrance current amount is calculated incorrectly after changing fund distribution when related paid invoice exists
* [MODORDERS-833](https://issues.folio.org/browse/MODORDERS-833) - Negative encumbrance values appear for expense classes after rollover
* [MODORDERS-827](https://issues.folio.org/browse/MODORDERS-827) - Order view is not restricted by Acquisition unit
* [MODORDERS-816](https://issues.folio.org/browse/MODORDERS-816) - A user with order create permission can not add suffix and prefix to the order
* [MODORDERS-810](https://issues.folio.org/browse/MODORDERS-810) - Invoice is unable to approve due to protected fields on productIds
* [MODORDERS-713](https://issues.folio.org/browse/MODORDERS-713) - Add prefix and suffix to PO number when create or update order


## 12.5.0 Nolana R3 2022 - Released
This release focused on upgrade RMB and module interface dependencies

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.4.0...v12.5.0)

### Stories
* [MODORDERS-778](https://issues.folio.org/browse/MODORDERS-778) - Temporary disable data-import verticle deployment
* [MODORDERS-776](https://issues.folio.org/browse/MODORDERS-776) - Upgrade RAML Module Builder
* [MODORDERS-761](https://issues.folio.org/browse/MODORDERS-761) - Support inventory 12.0 in ModuleDescriptor "requires"
* [MODORDERS-757](https://issues.folio.org/browse/MODORDERS-757) - Support holdings-storage 6.0, item-storage 10.0
* [MODORDERS-753](https://issues.folio.org/browse/MODORDERS-753) - Implement processing of DI Kafka events by mod-orders

### Bug Fixes
* [FOLIO-3604](https://issues.folio.org/browse/FOLIO-3604) - FolioVertxCompletableFuture copyright violation
* [MODORDERS-779](https://issues.folio.org/browse/MODORDERS-779) - Closed Order w/multiple lines & different/no funds will not re-open


## 12.4.0 Morning Glory R2 2022 - Released
This release focused on implementing edit instance connection logic of POL, updating POL when an order is closed with the "Cancelled" reason, defining new Business API for updating Instance Reference for Non-package order line, removing Renewal date and renewal interval requirement for opening order

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.3.0...v12.4.0)

### Stories
* [MODORDERS-725](https://issues.folio.org/browse/MODORDERS-725) - Temporary workaround for issue with x-okapi-tenant header
* [MODORDERS-710](https://issues.folio.org/browse/MODORDERS-710) - mod-orders: Upgrade RAML Module Builder
* [MODORDERS-706](https://issues.folio.org/browse/MODORDERS-706) - Create encumbrances if needed when reopening an order
* [MODORDERS-685](https://issues.folio.org/browse/MODORDERS-685) - Implement edit instance connection logic of POL - create inventory set to Instance or none
* [MODORDERS-684](https://issues.folio.org/browse/MODORDERS-684) - Implement edit instance connection logic of POL - create inventory set to Instance, holding or Instance, holding, item
* [MODORDERS-683](https://issues.folio.org/browse/MODORDERS-683) - Define new Business API for updating Instance Reference for Non-package order line
* [MODORDERS-682](https://issues.folio.org/browse/MODORDERS-682) - Remove Renewal date and renewal interval requirement for opening order
* [MODORDERS-664](https://issues.folio.org/browse/MODORDERS-664) - Release encumbrances when POL payment status is set to canceled and update POL statuses
* [MODORDERS-663](https://issues.folio.org/browse/MODORDERS-664) - Update PO workflow status based on cancelation of POLs.
* [MODORDERS-662](https://issues.folio.org/browse/MODORDERS-662) - Fix randomly failing tests
* [MODORDERS-660](https://issues.folio.org/browse/MODORDERS-660) - Improve validation to preventing creation order lines with wrong expense classes
* [MODORDERS-636](https://issues.folio.org/browse/MODORDERS-636) - Update po lines when an order is closed with the "Cancelled" reason

### Bug Fixes
* [MODORDERS-727](https://issues.folio.org/browse/MODORDERS-727) - Create encumbrances if needed when reopening an order
* [MODORDERS-722](https://issues.folio.org/browse/MODORDERS-722) - Holdings source gets updated to MARC during importing from bib - happens when holdings was created by Order app (Morning Glory)
* [MODORDERS-716](https://issues.folio.org/browse/MODORDERS-716) - Failing change instance connection from POL when Create inventory is "None"
* [MODORDERS-715](https://issues.folio.org/browse/MODORDERS-715) - Resolve issue with failing test
* [MODORDERS-712](https://issues.folio.org/browse/MODORDERS-712) - Encumbrances are created for closed orders during FYRO
* [MODORDERS-700](https://issues.folio.org/browse/MODORDERS-700) - Receive 'protected fields can’t be modified' error for eresource fields on PO line that is a Physical Resource
* [MODORDERS-699](https://issues.folio.org/browse/MODORDERS-699) - Encumbrances are not deleted after deleting Cancelled order
* [MODORDERS-697](https://issues.folio.org/browse/MODORDERS-697) - Instructions to vendor field in POL cannot be changed to blank after text has been added and saved
* [MODORDERS-694](https://issues.folio.org/browse/MODORDERS-694) - PendingToOpenEncumbranceStrategy is not thread-safe
* [MODORDERS-691](https://issues.folio.org/browse/MODORDERS-691) - Opening an Ongoing order changes resolved POL statuses
* [MODORDERS-681](https://issues.folio.org/browse/MODORDERS-681) - Errors when creating pieces in quick succession
* [MODORDERS-673](https://issues.folio.org/browse/MODORDERS-673) - After edit Encumbrance detail not shown on POL fund distribution
* [MODORDERS-658](https://issues.folio.org/browse/MODORDERS-658) - Protected fields can't be modified" on productIds field error when attempting to update unmodified order

## 12.3.0 - Released
This release focused on adding support of EDI export and fixing po line processing 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.1.0...v12.2.0)

### Stories
* [MODORDERS-634](https://issues.folio.org/browse/MODORDERS-634) - Support circulation interface v13
* [MODORDERS-620](https://issues.folio.org/browse/MODORDERS-620) - Create Acquisition method schema and API
* [MODORDERS-609](https://issues.folio.org/browse/MODORDERS-609) - Deleting an order line fails without mod-invoice
* [MODORDERS-605](https://issues.folio.org/browse/MODORDERS-605) - Return helpful and clear error code, when Acquisition unit name already exist
* [MODORDERS-599](https://issues.folio.org/browse/MODORDERS-599) - Update re-encumbered logic to support "initialEncumbered"
* [MODORDERS-569](https://issues.folio.org/browse/MODORDERS-569) - Implement logic for updating Item fields when Piece was changed
* [MODORDERS-557](https://issues.folio.org/browse/MODORDERS-557) - Support "Disable instance matching" flag at the open order flow

### Bug Fixes
* [MODORDERS-655](https://issues.folio.org/browse/MODORDERS-655) - Undefined permission 'finance.order-transaction-summaries.item.get'
* [MODORDERS-649](https://issues.folio.org/browse/MODORDERS-649) - Some fields of the piece are not populated when receiving/unreceiving
* [MODORDERS-645](https://issues.folio.org/browse/MODORDERS-645) - Invoice payment fails if order line fund distribution is changed before invoice approval
* [MODORDERS-638](https://issues.folio.org/browse/MODORDERS-638) - Invoice can be approved but not paid when expense class added at point of invoice
* [MODORDERS-635](https://issues.folio.org/browse/MODORDERS-635) - Title "Publication date" validation is different from that of the PO Line
* [MODORDERS-616](https://issues.folio.org/browse/MODORDERS-616) - New holdingId is not saved into piece when location is changed during receiving


## 12.2.0 - Released
This release focused on improvements in handling pieces 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.1.0...v12.2.0)

### Stories
* [MODORDERS-572](https://issues.folio.org/browse/MODORDERS-572) - Piece update/receive : support location/holding field adjustments
* [MODORDERS-565](https://issues.folio.org/browse/MODORDERS-565) - All transaction queries should use the fiscal year id
* [MODORDERS-538](https://issues.folio.org/browse/MODORDERS-538) - Update quantity of the Location in the POL if pieces updated/removed
* [MODORDERS-534](https://issues.folio.org/browse/MODORDERS-534) - Define and implement business API for retrieving pieces information
* [MODORDERS-533](https://issues.folio.org/browse/MODORDERS-533) - Remove unused InventoryManager.updateItemRecords
* [MODORDERS-517](https://issues.folio.org/browse/MODORDERS-520) - align dependency versions affected by Inventory's Optimistic Locking
* [MODORDERS-510](https://issues.folio.org/browse/MODORDERS-510) - Merge branch feature/UXPROD-1925 into master
* [MODORDERS-507](https://issues.folio.org/browse/MODORDERS-507) - Update UnOpen order logic with holding ids storing

### Bug Fixes
* [MODORDERS-576](https://issues.folio.org/browse/MODORDERS-576) - Holding is created for piece when Create inventory is None/Instance
* [MODORDERS-575](https://issues.folio.org/browse/MODORDERS-575) - No way to create piece for pending order with manual create piece = True
* [MODORDERS-573](https://issues.folio.org/browse/MODORDERS-573) - Update instance matching logic
* [MODORDERS-563](https://issues.folio.org/browse/MODORDERS-563) - Cannot delete PO Line if other PO Line on the same order is linked to an invoice line (#491) aleksandr2110* 10.09.2021, 14:57
* [MODORDERS-539](https://issues.folio.org/browse/MODORDERS-539) - Encumbrance not deleted when order Fund distribution is removed
* [MODORDERS-528](https://issues.folio.org/browse/MODORDERS-528) - Trying and failing to open a PO will still create records in Inventory
* [MODORDERS-404](https://issues.folio.org/browse/MODORDERS-404) - Orders composite crashes API crashes after loading around 10,000 open orders


## 12.1.0 - Released

This released focused on business logic updates for open orders. RMB version updated up to v33.0.0. Search and filtering speed increased after migrating to cross-index approach 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v12.0.3...v12.1.0)

### Stories
* [MODORDERS-520](https://issues.folio.org/browse/MODORDERS-520) - Remove protection from subscriptionTo and vendorAccount for Open order
* [MODORDERS-516](https://issues.folio.org/browse/MODORDERS-516) - Consider exchange rate when updating rollover adjustment amount in POL
* [MODORDERS-512](https://issues.folio.org/browse/MODORDERS-512) - Update circulation interface dependency to support v11
* [MODORDERS-511](https://issues.folio.org/browse/MODORDERS-511) - Do not allow changing the quantity or location of the POL, if the order already opened
* [MODORDERS-504](https://issues.folio.org/browse/MODORDERS-504) - Add GET PO lines cross index search support
* [MODORDERS-503](https://issues.folio.org/browse/MODORDERS-503) - Add GET orders cross index search support
* [MODORDERS-486](https://issues.folio.org/browse/MODORDERS-486) - mod-orders: Update RMB
* [MODORDERS-474](https://issues.folio.org/browse/MODORDERS-474) - Unrelease encumbrances when order is reopen
* [MODORDERS-224](https://issues.folio.org/browse/MODORDERS-224) - Validate that renewals is present in ongoing orders


### Bug Fixes
* [MODORDERS-519](https://issues.folio.org/browse/MODORDERS-519) - CANNOT receive pieces against package POL
* [MODORDERS-513](https://issues.folio.org/browse/MODORDERS-513) - Closing an order fails when all encumbrances already released
* [MODORDERS-502](https://issues.folio.org/browse/MODORDERS-502) - When reopening order system forces user to delete pieces and items
* [MODORDERS-499](https://issues.folio.org/browse/MODORDERS-499) - Renewals validation prevents opening of ongoing orders
* [MODORDERS-496](https://issues.folio.org/browse/MODORDERS-496) - Unable to open order with 2 POLs that have different currencies
* [MODORDERS-482](https://issues.folio.org/browse/MODORDERS-482) - Cannot split funds on a PO with an odd number of pennies as the cost


## 12.0.0 - Released

The focus of this release was to update RMB, support ledger fiscal year rollover and re-encumber logic. 
Also performance improvement was done for composite order API. 

[Full Changelog](https://github.com/folio-org/mod-orders/compare/v11.1.3...v12.0.0)

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
