#!/bin/bash

# Run vpm to install the latest VRC SDK
dotnet tool restore
dotnet vpm add package com.vrchat.worlds -p project
