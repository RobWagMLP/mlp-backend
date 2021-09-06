create schema IF NOT exists utils;

CREATE EXTENSION IF NOT EXISTS pg_stat_statements with schema utils;
CREATE EXTENSION IF NOT EXISTS btree_gist         with schema utils;

ALTER DEFAULT PRIVILEGES IN SCHEMA public REVOKE EXECUTE ON FUNCTIONS from PUBLIC;