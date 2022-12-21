-- Creates table to store association between record id and order id
CREATE TABLE IF NOT EXISTS records_orders (
  record_id uuid NOT NULL PRIMARY KEY,
  order_id uuid NOT NULL
);
