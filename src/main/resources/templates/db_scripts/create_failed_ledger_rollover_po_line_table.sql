-- Creates table to store failures during rollover
CREATE TABLE IF NOT EXISTS ${myuniversity}_${mymodule}.failed_ledger_rollover_po_line
(
  id uuid NOT NULL PRIMARY KEY,
  rollover_id uuid,
  ledger_id uuid,
  po_line_id uuid,
  request_body text,
  response_body text,
  status_code text,
  workflow_status text
);

CREATE INDEX IF NOT EXISTS failed_ledger_rollover_po_line_rollover_id_idx ON ${myuniversity}_${mymodule}.failed_ledger_rollover_po_line USING btree (rollover_id);
