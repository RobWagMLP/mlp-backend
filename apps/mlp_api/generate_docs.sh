#!/bin/bash



#######################################
#abholen des verwendeten swagge codegen
#######################################
if [ ! -f swagger-codegen-cli.jar ]
then
  wget https://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.3.1/swagger-codegen-cli-2.3.1.jar -O  swagger-codegen-cli.jar
fi


#######################################
#code für core
#######################################
java -jar swagger-codegen-cli.jar generate -l html2 -i openapi.json  -o ./priv
