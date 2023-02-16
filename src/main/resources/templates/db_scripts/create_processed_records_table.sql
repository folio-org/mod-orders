-- Creates table to store record id for deduplication
CREATE TABLE IF NOT EXISTS processed_records (
  record_id uuid NOT NULL PRIMARY KEY,
  created_date timestamp
);
