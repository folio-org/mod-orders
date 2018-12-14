# mod-orders

Copyright (C) 2018 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

This is the Orders business logic module.

## Additional information

### Purchase Order logic 
Upon receiving a request to create a PO, it does the following:
* Save a PO object; then 
* Retrieve the `id` of the PO and pass that to the related PO lines; then
* Save the PO lines; then
* Retrieve the fund transactions associated with each PO line; then
* Make the appropriate transactions against the associated funds

Upon receiving a request to update a PO, it does the following:
* Retrieve PO data from storage; then
* Delete PO Lines; then
* Delete PO; then
* Create PO following the steps described above

Upon receiving a request to delete a PO, it does the following:
* Retrieve PO lines by the `id` of the PO and delete them; then
* Delete PO

### Purchase Order Line logic
Upon receiving a request to create a PO Line, it does the following:
* Save a PO Line sub-objects in the storage; then 
* Save a PO Line to storage

Upon receiving a request to update a PO, it does the following:
* Retrieve PO Line data from storage; then
* Validate that PO id of the PO Line corresponds to order id in the path; then
* Delete PO Line from storage; then
* Create PO Line based on received content in the request

Upon receiving a request to delete a PO Line, it does the following:
* Retrieve PO line by the `id` of the PO and delete them; then
* Validate that PO id of the PO Line corresponds to order id in the path; then
* Delete PO

### Issue tracker

See project [MODORDERS](https://issues.folio.org/browse/MODORDERS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### Other documentation

Other [modules](https://dev.folio.org/source-code/#server-side) are described,
with further FOLIO Developer documentation at
[dev.folio.org](https://dev.folio.org/)
