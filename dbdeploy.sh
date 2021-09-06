#!/bin/bash
ACTION="$1"
if [ -z "$ACTION" ]; then ACTION="migrate"; fi
flyway "$ACTION" \
 -locations="filesystem:database/inserts,filesystem:database/ddl,filesystem:database/sps" \
 -user=postgres \
 -url="jdbc:postgresql:mlp_store" \
 -password=postgres \
 -schemas=public \
 -group=true \
 -table=schema_version \
 -placeholders.client=ndpay \
 -placeholders.stage="local" \
 -outputFile=flyway.log
