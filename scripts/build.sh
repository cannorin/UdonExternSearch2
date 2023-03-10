#!/bin/bash

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR/../

# Run extractor
dotnet build extractor/extractor.fsproj
dotnet run --project extractor/extractor.fsproj
cp udonInfo.json.gz webapp/static/assets/udonInfo.json.gz

# Build webapp
cd webapp
yarn
dotnet fable src && yarn webpack --mode production
