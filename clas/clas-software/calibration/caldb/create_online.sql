CREATE TABLE IF NOT EXISTS run_info (
  id int NOT NULL AUTO_INCREMENT PRIMARY KEY,
  time TIMESTAMP(14),
  run int NOT NULL,
  beam_energy float,
  torus_current float,
  mini_current float,
  fcup int,
  fcup_active int,
  fcup_live int,
  nevent int,
  KEY run_index (run)
);
