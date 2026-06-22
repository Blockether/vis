@echo off
REM Vis CLI launcher for native Windows shells.
REM Resolves deps from the repository root, but preserves the user's
REM invocation directory as JVM user.dir for git/environment context.

setlocal EnableDelayedExpansion
set "INVOKE_CWD=%CD%"
set "BIN_DIR=%~dp0"
for %%I in ("%BIN_DIR%..") do set "REPO_ROOT=%%~fI"

REM Self-compilation subcommands (run from the repo root via build.clj):
REM   vis native -> BOTH target\vis.exe (native) AND target\vis.jar (uberjar)
REM   vis uber   -> just the uberjar
if /I "%~1"=="native" (
  cd /d "%REPO_ROOT%" || exit /b 1
  clojure -T:build native
  exit /b %ERRORLEVEL%
)
if /I "%~1"=="uber" (
  cd /d "%REPO_ROOT%" || exit /b 1
  clojure -T:build uber
  exit /b %ERRORLEVEL%
)

REM Strip a leading --jvm flag (force the JVM distribution) and forward the rest.
set "FORCE_JVM=0"
set "ARGS=%*"
if /I "%~1"=="--jvm" (
  set "FORCE_JVM=1"
  set "ARGS=!ARGS:* =!"
)

REM Proxy to a prebuilt distribution if present: native binary > uberjar > source.
REM Self-contained artifacts run from the user's ORIGINAL cwd.
if "%FORCE_JVM%"=="0" if exist "%REPO_ROOT%\target\vis.exe" (
  "%REPO_ROOT%\target\vis.exe" %ARGS%
  exit /b %ERRORLEVEL%
)
if exist "%REPO_ROOT%\target\vis.jar" (
  java "-Duser.dir=%INVOKE_CWD%" -jar "%REPO_ROOT%\target\vis.jar" %ARGS%
  exit /b %ERRORLEVEL%
)

REM No prebuilt distribution -> run from live source.
cd /d "%REPO_ROOT%" || exit /b 1
clojure "-J-Duser.dir=%INVOKE_CWD%" -M:vis %ARGS%
exit /b %ERRORLEVEL%
