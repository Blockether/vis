@echo off
REM Vis CLI launcher for native Windows shells.
REM Resolves deps from the repository root, but preserves the user's
REM invocation directory as JVM user.dir for git/environment context.

setlocal
set "INVOKE_CWD=%CD%"
set "BIN_DIR=%~dp0"
for %%I in ("%BIN_DIR%..") do set "REPO_ROOT=%%~fI"
cd /d "%REPO_ROOT%" || exit /b 1
clojure "-J-Duser.dir=%INVOKE_CWD%" -M:vis %*
exit /b %ERRORLEVEL%
