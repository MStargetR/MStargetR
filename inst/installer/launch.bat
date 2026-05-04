@echo off
:: MStargetR Launcher
:: =============================================================================
:: This script finds Rscript and launches the MStargetR Shiny GUI.
:: Works in both installed mode and development (source) mode.

title MStargetR - Starting...
setlocal EnableDelayedExpansion

set "RSCRIPT="

:: Try Rscript from PATH first
where Rscript >nul 2>&1
if %ERRORLEVEL% EQU 0 (
    for /f "delims=" %%r in ('where Rscript') do (
        set "RSCRIPT=%%r"
        echo Found R in PATH: !RSCRIPT!
        goto :found_r
    )
)

:: Check common R installation locations (sorted newest first)
set "R_LOCATIONS=C:\Program Files\R"
if exist "%R_LOCATIONS%" (
    for /f "delims=" %%d in ('dir /b /o-n "%R_LOCATIONS%"') do (
        if exist "%R_LOCATIONS%\%%d\bin\Rscript.exe" (
            set "RSCRIPT=%R_LOCATIONS%\%%d\bin\Rscript.exe"
            echo Found R at %R_LOCATIONS%\%%d
            goto :found_r
        )
    )
)

:: Check user-local R installation
set "R_LOCAL=%LOCALAPPDATA%\Programs\R"
if exist "%R_LOCAL%" (
    for /f "delims=" %%d in ('dir /b /o-n "%R_LOCAL%"') do (
        if exist "%R_LOCAL%\%%d\bin\Rscript.exe" (
            set "RSCRIPT=%R_LOCAL%\%%d\bin\Rscript.exe"
            echo Found R at %R_LOCAL%\%%d
            goto :found_r
        )
    )
)

:: R not found
echo.
echo ERROR: R is not installed or not found in the expected locations.
echo.
echo Searched:
echo   - System PATH
echo   - C:\Program Files\R\*\bin\Rscript.exe
echo   - %LOCALAPPDATA%\Programs\R\*\bin\Rscript.exe
echo.
echo Please install R from: https://cran.r-project.org/
echo After installing R, run this launcher again.
echo.
pause
exit /b 1

:found_r
echo.
echo ============================================
echo   MStargetR - Targeted LC-MS Preprocessing
echo ============================================
echo.
echo Starting MStargetR GUI...
echo Your browser will open automatically.
echo.
echo Do NOT close this window while using MStargetR.
echo Press Ctrl+C to stop the application.
echo.

:: Determine script directory and possible project root
set "SCRIPT_DIR=%~dp0"
set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

:: Check if install_deps.R is in the same directory (installed mode)
set "INSTALL_DEPS=%SCRIPT_DIR%\install_deps.R"

:: Launch via R script file (avoids cmd.exe quoting issues with inline R code)
set "LAUNCH_SCRIPT=%SCRIPT_DIR%\launch_app.R"
if exist "%LAUNCH_SCRIPT%" (
    "%RSCRIPT%" "%LAUNCH_SCRIPT%" --script-dir "%SCRIPT_DIR%"
) else (
    :: Fallback: try calling the exported function directly
    "%RSCRIPT%" -e "MStargetR::launchMStargetR(launch.browser=TRUE, host='127.0.0.1')"
)

set "EXIT_CODE=%ERRORLEVEL%"
echo.
if %EXIT_CODE% NEQ 0 (
    echo ============================================
    echo MStargetR exited with an error (code: %EXIT_CODE%).
    echo.
    echo Troubleshooting:
    echo   1. Run launch_debug.bat for detailed diagnostics
    echo   2. If packages are missing, run: "%RSCRIPT%" "%INSTALL_DEPS%"
    echo   3. Or in R run: remotes::install_github("MStargetR/MStargetR")
    echo ============================================
) else (
    echo MStargetR has stopped.
)
echo.
pause
endlocal
