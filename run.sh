#!/bin/bash

psql -p 5432 -h localhost -U postgres -A -tc "SELECT 1 FROM pg_database     WHERE datname= 'mlp_store'"     |grep -q 1     || psql -p 5432 -h localhost -U postgres -A -a -tc "create database mlp_store"

sh dbdeploy.sh

rebar3 shell

