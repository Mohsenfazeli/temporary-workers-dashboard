@echo off
rem Double-click to start the dashboard in your browser (nothing to install).
rem Live IRCC data instead of the September 15, 2026 release: run_dashboard.bat live
rem Close this window to stop the dashboard.
setlocal
cd /d "%~dp0"
set "RSCRIPT=%~dp0R-portable\bin\Rscript.exe"
if exist "%RSCRIPT%" goto run
set "RSCRIPT="
for /f "delims=" %%i in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT for /d %%d in ("%ProgramFiles%\R\R-*") do set "RSCRIPT=%%d\bin\Rscript.exe"
if not defined RSCRIPT (
  echo R was not found. Use the offline Windows package, or install R from https://cran.r-project.org
  pause
  exit /b 1
)
:run
"%RSCRIPT%" --vanilla tools\run_local.R %*
if errorlevel 1 pause
