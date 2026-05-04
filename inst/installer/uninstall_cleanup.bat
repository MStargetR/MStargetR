@echo off
:: MStargetR Uninstall Cleanup
:: =============================================================================
:: Removes MStargetR R package and cleans up application data.
:: Does NOT remove R, Docker, or Pandoc (shared system tools).

:: Remove MStargetR R package
:: Try PATH first, then common install locations
set "RSCRIPT="
where Rscript >nul 2>&1 && set "RSCRIPT=Rscript"
if not defined RSCRIPT (
    for /f "delims=" %%R in ('dir /b /o-n "C:\Program Files\R\R-*" 2^>nul') do (
        if exist "C:\Program Files\R\%%R\bin\Rscript.exe" (
            set "RSCRIPT=C:\Program Files\R\%%R\bin\Rscript.exe"
            goto :found_r
        )
    )
)
if not defined RSCRIPT (
    for /f "delims=" %%R in ('dir /b /o-n "%LOCALAPPDATA%\Programs\R\R-*" 2^>nul') do (
        if exist "%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe" (
            set "RSCRIPT=%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe"
            goto :found_r
        )
    )
)
:found_r
if defined RSCRIPT (
    "%RSCRIPT%" -e "try(remove.packages('MStargetR'), silent=TRUE)"
    echo MStargetR package removed from R library.
) else (
    echo R not found. MStargetR R package may still be installed.
    echo To remove manually, run in R: remove.packages('MStargetR')
)

:: Clean up MStargetR application data (logs, preferences)
if exist "%LOCALAPPDATA%\MStargetR" (
    rmdir /s /q "%LOCALAPPDATA%\MStargetR" 2>nul
    echo MStargetR application data removed.
)

:: Inform user about shared tools
echo.
echo Note: R, Docker, and Pandoc were NOT removed as they may be used by
echo other applications. To remove them manually:
echo   - R: Settings ^> Apps ^> R for Windows
echo   - Docker: Settings ^> Apps ^> Docker Desktop
echo   - Pandoc: Settings ^> Apps ^> Pandoc
