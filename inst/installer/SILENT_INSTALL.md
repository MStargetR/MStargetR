# MStargetR Silent Installation Guide

For enterprise or automated deployments, the MStargetR Windows installer
supports Inno Setup silent install flags.

## Basic Silent Install

```cmd
MStargetR_Setup.exe /VERYSILENT /SUPPRESSMSGBOXES /NORESTART
```

## Custom Install Directory

```cmd
MStargetR_Setup.exe /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /DIR="D:\MStargetR"
```

## Common Flags

| Flag                | Description                                   |
| ------------------- | --------------------------------------------- |
| `/VERYSILENT`       | No progress window, no user prompts           |
| `/SILENT`           | Show progress window but no user prompts      |
| `/SUPPRESSMSGBOXES` | Suppress message boxes during install         |
| `/NORESTART`        | Do not restart the machine after install      |
| `/DIR="path"`       | Override default install directory            |
| `/LOG="path"`       | Write install log to specified file           |
| `/NOICONS`          | Do not create Start Menu or desktop shortcuts |

## Prerequisites

Before running a silent install, ensure:

1. **R** (>= 4.1.0) is installed and on the system PATH.
2. **Docker Desktop** is installed (required for msConvertR and PeakForgeR).
3. The installer is run with administrator privileges if installing to
   `Program Files`.

The installer will attempt to install R if not found, but this step may
require user interaction. For fully unattended deployment, pre-install R
and Docker before running the MStargetR installer.

## Uninstall

```cmd
"C:\Program Files\MStargetR\unins000.exe" /VERYSILENT
```

## Exit Codes

| Code | Meaning                                        |
| ---- | ---------------------------------------------- |
| 0    | Success                                        |
| 1    | Setup failed to initialize                     |
| 2    | User cancelled (not applicable in silent mode) |
| 3    | Fatal error during preparation                 |
| 4    | Fatal error during install                     |
| 5    | User cancelled after warning                   |
| 6    | Setup was aborted by a debugger                |
