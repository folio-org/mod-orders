-- Creates table to store association between record id and order id
CREATE TABLE IF NOT EXISTS po_lines_import_progress
(
  order_id           uuid    NOT NULL PRIMARY KEY,
  total_po_lines     integer NOT NULL,
  processed_po_lines integer
);
