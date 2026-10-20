@echo off
rem Double-click to rebuild the data from source: downloads the two IRCC CSV files
rem into raw_profile\, rebuilds and checks them, and compares them with the shipped
rem release. The dashboard keeps showing the shipped release (data\ is not changed).
setlocal
cd /d "%~dp0"
set "RSCRIPT=%~dp0R-portable\bin\Rscript.exe"
if not exist "%RSCRIPT%" set "RSCRIPT=Rscript.exe"
if exist "%~dp0library" set "R_LIBS=%~dp0library"
if exist "%~dp0library" set "R_LIBS_USER=%~dp0library"
"%RSCRIPT%" --vanilla tools\build_cache_and_profile.R %*
pause
