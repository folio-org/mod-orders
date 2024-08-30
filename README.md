# mod-orders

Copyright (C) 2018-2023 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

This is the Orders business logic module.

## Additional information

### Additional Permissions Required
 * In case an acquisition unit has to be assigned to the Order it is required that user should have 
   `orders.acquisitions-units-assignments.assign` to create a purchase order
 * In case an acquisition unit has to be modified(added/deleted) in the order, the user should have
   `orders.acquisitions-units-assignments.manage` to update a purchase Order
 * If the setting "isApprovalRequired" is set to true,an order approval is required to `OPEN` an order and the user should have
   `orders.item.approve` permissions to `approve` an order

### Purchase Order logic
Upon receiving a request to create a PO, it does the following:
* Validate PO content (vendor, acquisition units, settings based validation like order lines limit and etc.); then
* Save a PO object in `Pending` status; then 
* Retrieve the `id` of the PO and pass that to the related PO lines; then
* Save the PO lines; then
* In case an order is in `Open` status and updates are required in the Inventory, it does the following:
  * Find or create instance record; then
  * Update PO lines with instance record identifier; then 
  * Find or create holding record; then
  * Find or create item record per unit of physical/electronic quantity identified in the order line record
  * Update a PO object; then 
* Retrieve the encumbrance transactions associated with each PO line based on fund distributions; then
* Make the appropriate encumbrance transactions against the associated funds

Upon receiving a request to update a PO, it does the following:
* validate PO content (vendor, acquisition units, settings based validation like order lines limit and etc.); then
* if provided PO doesn't contain PO Lines, just update the PO information via the `PUT /orders-storage/purchase-orders/<id>` endpoint
* if provided PO contain PO Lines:
  * Get the current list of PO Lines for this order; then
  * Compare the provided list of PO Lines with what's in the database:
    * if PO Line already exists, update it and any sub-objects
    * if PO Line exists in the database, but not in the request, delete this PO Line
    * if PO Line exists in the request but not in the database, create PO Line
* In case an order's status changes from `Pending` to `Open` and updates are required in the Inventory, it does the following:
  * Find or create instance record; then
  * Update PO lines with instance record identifier; then 
  * Find or create holding record; then
  * Find or create item record per unit of physical/electronic quantity identified in the order line record
  * Update a PO object with `Open` status 

Upon receiving a request to delete a PO, it does the following:
* Validate if PO is not restricted by acquisitions units or a user is a member of assigned units; then
* Retrieve PO lines by the `id` of the PO and delete them; then
* Delete PO

### Purchase Order Line logic
Upon receiving a request to create a PO Line, it does the following:
* Validate PO Line content (interrelated fields, acquisition units based validation, settings based validation like order lines limit and etc.); then
* Save a PO Line sub-objects in the storage; then 
* Save a PO Line content with references to the created sub-objects in the storage.

Upon receiving a request to update a PO Line, it does the following:
* Validate PO Line content (interrelated fields, acquisition units based validation and etc.); then
* Retrieve PO Line data from storage; then
* Validate that PO id of the PO Line from storage corresponds to order id in the path; then
* Depending on the content in the storage the sub-object updates are following:
  * In case the sub-object is available in the storage and the updated PO Line data also contains this, the `PUT` request is being sent to the storage to corresponding endpoint.  
    For example, to update existing `reportingCode` sub-object `PUT` request should be sent to [/orders-storage/reporting-codes/{id}](https://s3.amazonaws.com/foliodocs/api/mod-orders-storage/reporting-code.html#orders_storage_reporting_codes__id__put) endpoint.
  * In case the sub-object is available in the storage but the updated PO Line data does not contain this anymore, the `DELETE` request is being sent to the storage to corresponding endpoint.  
    For example, to remove existing `reportingCode` sub-object `DELETE` request should be sent to [/orders-storage/reporting-codes/{id}](https://s3.amazonaws.com/foliodocs/api/mod-orders-storage/reporting-code.html#orders_storage_reporting_codes__id__delete) endpoint.
  * In case the sub-object is not available in the storage but the updated PO Line data contains this, the `POST` request is being sent to the storage to corresponding endpoint.  
    For example, to add `reportingCode` sub-object `POST` request should be sent to [/orders-storage/reporting-codes](https://s3.amazonaws.com/foliodocs/api/mod-orders-storage/reporting-code.html#orders_storage_reporting-codess_post) endpoint.
* Final step is to store the updated PO Line content with references to the created/updated sub-objects. The `PUT` request is being sent to [/orders-storage/po-lines/{id}](https://s3.amazonaws.com/foliodocs/api/mod-orders-storage/po-line.html#orders_storage_po_lines__id__put) endpoint.

Note: the PO Line update might lead to Order's workflow status update (see [MODORDERS-218](https://issues.folio.org/browse/MODORDERS-218) for more details)

Upon receiving a request to delete a PO Line, it does the following:
* Validate if PO is not restricted by acquisitions units or a user is a member of assigned units; then
* Retrieve PO line by the `id`; then
* Validate that PO `id` of the PO Line corresponds to order id in the path; then
* Delete PO Line and its sub-objects

### Receiving logic 
Upon receiving a request to receive resources, it does the following:
* Retrieve piece records from the orders storage based on the [request body](https://github.com/folio-org/acq-models/blob/master/mod-orders/schemas/receivingCollection.json).
* For those pieces which have item id the process is like following:
  * Retrieve items from inventory.
  * Update item entities with barcode (if specified) and status
  * Send PUT requests to inventory to update items. In case any errors happen while updating items, the logic just collects them and does not stop entire process
* Update piece records with receiving information and send PUT requests to orders storage. If any errors happen, just collect them.  
  The following is expected to be update in piece:
  * `receivingStatus`: in case the `itemStatus` is `On order` the status is set to `Expected`, otherwise to `Received`
  * `locationId`: from the request
  * `receivedDate`: current date or `null` in case the `itemStatus` is `On order` 
* Retrieve all involved PO Lines from the storage and updates receipt status.
  * If all pieces of an order line have been received, the receipt status is `Fully Received`. 
  * If more than one, but less than the total number of pieces have been received, it is `Partially Received`.
  * If none received, it is `Awaiting Receipt`.
  * If the calculated status is the same as current one, no update is made
* Prepare [response](https://github.com/folio-org/acq-models/blob/master/mod-orders/schemas/receivingResults.json) to the client taking into account any error happened processing particular piece record.

Sample of the requests:
  * Request to receive pieces for 2 PO Lines
    ```json
    {
      "toBeReceived": [
        {
          "poLineId": "0804ddec-6545-404a-b54d-a693f505681d",
          "received": 1,
          "receivedItems": [
            {
              "barcode": "0987654111",
              "itemStatus": "Received",
              "locationId": "fcd64ce1-6995-48f0-840e-89ffa2288371",
              "pieceId": "cb9b0468-f2b4-4a13-b64c-662c4c9ec3ed"
            }
          ]
        },
        {
          "poLineId": "7f0c4975-885e-47d5-8d5a-793dffbba9b2",
          "received": 1,
          "receivedItems": [
            {
              "barcode": "0987654333",
              "itemStatus": "In transit",
              "locationId": "758258bc-ecc1-41b8-abca-f7b610822ffd",
              "pieceId": "20241b8c-9076-4cf5-817b-f2c1e2cb242f"
            }
          ]
        }
      ],
      "totalRecords": 2
    }
    ```
  * Request to move a received piece back to `Expected`
    ```json
    {
      "toBeReceived": [
        {
          "poLineId": "f217a5c2-2c56-4d05-9412-a96cfc8e52de",
          "received": 1,
          "receivedItems": [
            {
              "itemStatus": "On order",
              "pieceId": "56fbfde4-6335-4dd7-9a03-d100821f1d18"
            }
          ]
        }
      ],
      "totalRecords": 1
    }
    ```

Note: receiving might lead to Order's workflow status update (see [MODORDERS-218](https://issues.folio.org/browse/MODORDERS-218) for more details)

### Acquisitions units
CRUD APIs are available to manage acquisitions units and memberships (user-unit relations).  
In order to avoid reference integrity issues when deleting acquisition units that are assigned to records, the logic implements a "soft delete" approach.
* When client sends `DELETE /acquisitions-units/units/<id>`, the logic gets acquisitions unit by specified id and updates it setting `isDeleted` to `true`
* When client sends `GET /acquisitions-units/units?query=<cql>` and `<cql>` does not contain criteria by `isDeleted`, the logic will search for records with `isDeleted==false`.
* To get all the units regardless of `isDeleted` value, the request should be like `GET /acquisitions-units/units?query=isDeleted=* AND (<cql>)`

### Kafka Integration
during tenant init, mod-orders creates producer with numPartitions which can be customized with env vars below:
* DI_ORDER_CREATED_READY_FOR_POST_PROCESSING_PARTITIONS
* DI_ORDER_CREATED_PARTITIONS
* DI_PENDING_ORDER_CREATED_PARTITIONS
Default value is 1

### Issue tracker

See project [MODORDERS](https://issues.folio.org/browse/MODORDERS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### Other documentation

Other [modules](https://dev.folio.org/source-code/#server-side) are described,
with further FOLIO Developer documentation at
[dev.folio.org](https://dev.folio.org/)
