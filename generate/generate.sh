#!/bin/bash
OPENAPI_GENERATOR_CLI=openapi-generator-cli
VERSION=5.1.1
SNAPSHOT=false
JAR_NAME=$OPENAPI_GENERATOR_CLI-$VERSION.jar

if [ ! -f $JAR_NAME ]
then
    if [ "$SNAPSHOT" = true ]; then
      MAIN_VERSION=${VERSION%%-*}  #remove all after the first minus
      wget https://oss.sonatype.org/content/repositories/snapshots/org/openapitools/openapi-generator-cli/$MAIN_VERSION-SNAPSHOT/$JAR_NAME -O $JAR_NAME 
    else  
      wget https://repo1.maven.org/maven2/org/openapitools/openapi-generator-cli/$VERSION/$JAR_NAME -O $JAR_NAME
    fi
fi

if [ "$1" = "-d" ]
then
        DEBUG_OPTIONS="-DdebugModels=true -DdebugOperations=true"
fi

#java -DdebugModels=true -DdebugOperations=true -jar swagger-codegen-cli.jar generate -l erlang-server -i priv/swagger.json -t ../../swagger_templates
java $DEBUG_OPTIONS -jar $JAR_NAME generate -g erlang-server -i ./openapi.json --additional-properties=packageName=mlp_api

