-- Creates table to store progress of po lines processing during data import
CREATE TABLE IF NOT EXISTS po_lines_processing_progress
(
  order_id           uuid    NOT NULL PRIMARY KEY,
  total_po_lines     integer NOT NULL,
  processed_po_lines integer,
  creation_date      timestamp
);
