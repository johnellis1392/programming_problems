#!/bin/bash

# Create new maven project
mvn archetype:generate \
-DgroupId=ctci.practice \
-DartifactId=practice \
-DarchetypeArtifactId=maven-archetype-quickstart \
-DinteractiveMode=false
