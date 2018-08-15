# mod-orders

Copyright (C) 2018 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

This is the Orders business logic module.

## Additional information

 Upon receiving a request to create a PO, it does the following:

* Save a PO object; then 
* Retrieve the `id` of the PO and pass that to the related PO lines; then
* Save the PO lines; then
* Retrieve the fund transactions associated with each PO line; then
* Make the appropriate transactions against the associated funds

### Issue tracker

See project [MODORDERS](https://issues.folio.org/browse/MODORDERS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### Other documentation

Other [modules](https://dev.folio.org/source-code/#server-side) are described,
with further FOLIO Developer documentation at
[dev.folio.org](https://dev.folio.org/)
