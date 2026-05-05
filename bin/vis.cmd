@echo off
REM Vis CLI launcher for native Windows shells.
REM Runs from the repository root so :local/root dependencies resolve.

setlocal
set "BIN_DIR=%~dp0"
for %%I in ("%BIN_DIR%..") do set "REPO_ROOT=%%~fI"
cd /d "%REPO_ROOT%" || exit /b 1
clojure -M:vis %*
exit /b %ERRORLEVEL%
