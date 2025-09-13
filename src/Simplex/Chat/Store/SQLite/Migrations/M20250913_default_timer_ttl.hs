{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250913_default_timer_ttl where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250913_default_timer_ttl :: Query
m20250913_default_timer_ttl =
  [sql|
ALTER TABLE users ADD COLUMN default_timer_ttl INTEGER NOT NULL DEFAULT 86400;
|]

down_m20250913_default_timer_ttl :: Query
down_m20250913_default_timer_ttl =
  [sql|
ALTER TABLE users DROP COLUMN default_timer_ttl;
|]
