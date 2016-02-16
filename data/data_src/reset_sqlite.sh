#!/bin/bash
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
set -eu

cd "${DIR}"
for script in drop_sqlite.sql sqlite_import sqlite_setup.sql
do
  < "prepare_sqlite/${script}" sqlite3  ../database.sqlite
done
