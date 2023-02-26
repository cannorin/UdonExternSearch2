#!/bin/bash

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR/../

dotnet vpm check project ./project

rm project/Packages/com.vrchat.core.vpm-resolver/package.json
dotnet vpm add package com.vrchat.core.vpm-resolver -p project

rm project/Packages/com.vrchat.base/package.json
dotnet vpm add package com.vrchat.base -p project

rm project/Packages/com.vrchat.worlds/package.json
dotnet vpm add package com.vrchat.worlds -p project

rm project/Packages/com.vrchat.clientsim/package.json
dotnet vpm add package com.vrchat.clientsim -p project
