# Activate Unity
unity-editor -nographics -manualLicenseFile Unity.ulf -logfile

# Run vpm to install the latest VRC SDK
dotnet tool restore
cp vcc-settings.json /root/.local/share/VRChatCreatorCompanion/settings.json
dotnet vpm install templates
dotnet vpm new project World -p .
dotnet vpm add package com.vrchat.worlds -p project

# Build unity project
unity-editor -nographics -projectPath ./project -buildWindowsPlayer /dev/null -logfile

# Run extractor
dotnet build --project extractor/extractor.fsproj
dotnet run --project extractor/extractor.fsproj > /dev/null
