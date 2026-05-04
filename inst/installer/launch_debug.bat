@echo off
:: MStargetR Debug Launcher
:: =============================================================================
:: Prints detailed diagnostics, then tries to launch MStargetR.
:: Always pauses at end so you can read the output.

title MStargetR - Debug Launcher
setlocal EnableDelayedExpansion

echo ============================================
echo   MStargetR Debug Launcher
echo ============================================
echo.

:: ---- Step 1: Find R ----
echo [1/5] Searching for R...
echo.

set "RSCRIPT="

:: Check PATH
where Rscript >nul 2>&1
if %ERRORLEVEL% EQU 0 (
    for /f "delims=" %%r in ('where Rscript') do (
        echo   PATH: %%r
        if "!RSCRIPT!"=="" set "RSCRIPT=%%r"
    )
) else (
    echo   PATH: Rscript NOT found in PATH
)

:: Check Program Files
set "R_LOCATIONS=C:\Program Files\R"
if exist "%R_LOCATIONS%" (
    echo   Scanning %R_LOCATIONS%\...
    for /f "delims=" %%d in ('dir /b /o-n "%R_LOCATIONS%"') do (
        if exist "%R_LOCATIONS%\%%d\bin\Rscript.exe" (
            echo     Found: %R_LOCATIONS%\%%d\bin\Rscript.exe
            if "!RSCRIPT!"=="" set "RSCRIPT=%R_LOCATIONS%\%%d\bin\Rscript.exe"
        )
    )
) else (
    echo   %R_LOCATIONS% does not exist
)

:: Check LocalAppData
set "R_LOCAL=%LOCALAPPDATA%\Programs\R"
if exist "%R_LOCAL%" (
    echo   Scanning %R_LOCAL%\...
    for /f "delims=" %%d in ('dir /b /o-n "%R_LOCAL%"') do (
        if exist "%R_LOCAL%\%%d\bin\Rscript.exe" (
            echo     Found: %R_LOCAL%\%%d\bin\Rscript.exe
            if "!RSCRIPT!"=="" set "RSCRIPT=%R_LOCAL%\%%d\bin\Rscript.exe"
        )
    )
) else (
    echo   %R_LOCAL% does not exist
)

echo.
if "!RSCRIPT!"=="" (
    echo ERROR: No Rscript.exe found anywhere.
    echo Please install R from https://cran.r-project.org/
    echo.
    pause
    exit /b 1
)

echo   Using: !RSCRIPT!
echo.

:: ---- Step 2: R version ----
echo [2/5] R version info...
"!RSCRIPT!" --version 2>&1
echo.

:: ---- Step 3: Library paths ----
echo [3/5] R library paths...
"!RSCRIPT!" -e "writeLines(.libPaths())"
echo.

:: ---- Step 4: Check MStargetR and key packages ----
echo [4/5] Package availability...
"!RSCRIPT!" -e "pkgs <- c('MStargetR','shiny','bslib','DT','plotly','ggplot2'); for (p in pkgs) { ok <- requireNamespace(p, quietly=TRUE); cat(sprintf('  %-15s %s\n', p, ifelse(ok, 'INSTALLED', 'NOT FOUND'))) }"
echo.

:: ---- Step 5: Determine project directory and launch ----
echo [5/5] Attempting to launch MStargetR...
echo.

set "SCRIPT_DIR=%~dp0"
set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%") do set "INST_DIR=%%~dpI"
set "INST_DIR=%INST_DIR:~0,-1%"
for %%I in ("%INST_DIR%") do set "PROJECT_DIR=%%~dpI"
set "PROJECT_DIR=%PROJECT_DIR:~0,-1%"

echo   Script directory: %SCRIPT_DIR%
echo   Project directory: %PROJECT_DIR%

:: Check for bundled source (installed via Setup.exe)
if exist "%SCRIPT_DIR%\source\DESCRIPTION" (
    echo   Bundled source: FOUND at %SCRIPT_DIR%\source
) else (
    echo   Bundled source: not found
)

:: Check for dev source (development checkout)
if exist "%PROJECT_DIR%\DESCRIPTION" (
    echo   Dev source: FOUND at %PROJECT_DIR%
) else (
    echo   Dev source: not found
)
echo.

:: Try launching via launch_app.R script (avoids cmd.exe quoting issues)
set "LAUNCH_SCRIPT=%SCRIPT_DIR%\launch_app.R"
if exist "!LAUNCH_SCRIPT!" (
    echo   Using launch script: !LAUNCH_SCRIPT!
    echo.
    "!RSCRIPT!" "!LAUNCH_SCRIPT!" --script-dir "%SCRIPT_DIR%" --debug
) else (
    echo   launch_app.R not found, trying direct call...
    echo.
    "!RSCRIPT!" -e "MStargetR::launchMStargetR(launch.browser=TRUE, host='127.0.0.1')"
)

set "EXIT_CODE=%ERRORLEVEL%"
echo.
echo ============================================
if %EXIT_CODE% NEQ 0 (
    echo MStargetR exited with error code: %EXIT_CODE%
) else (
    echo MStargetR exited normally.
)
echo ============================================
echo.

endlocal
pause
