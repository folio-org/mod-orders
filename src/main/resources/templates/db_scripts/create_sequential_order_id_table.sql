CREATE TABLE IF NOT EXISTS sequential_order_id (
  job_execution_id uuid NOT NULL,
  sequential_no integer NOT NULL,
  order_id uuid NOT NULL,
  saved_timestamp timestamp,
  CONSTRAINT sequential_order_pk_constraint PRIMARY KEY(job_execution_id, sequential_no)
);
