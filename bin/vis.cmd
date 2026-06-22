@echo off
REM Vis CLI launcher for native Windows shells.
REM Resolves deps from the repository root, but preserves the user's
REM invocation directory as JVM user.dir for git/environment context.

setlocal EnableDelayedExpansion
set "INVOKE_CWD=%CD%"
set "BIN_DIR=%~dp0"
for %%I in ("%BIN_DIR%..") do set "REPO_ROOT=%%~fI"

if not defined VIS_HOME set "VIS_HOME=%USERPROFILE%\.vis"
set "VIS_INSTALL=%VIS_HOME%\install"

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

REM Self-update (registry-backed managed install under %VIS_HOME%\install):
REM   vis update [<version>|latest] [--with-assets]   pull a NATIVE binary
REM   vis update <git-sha>                            switch to JVM source @ commit
if /I "%~1"=="update" (
  set "TGT=%~2"
  set "WA="
  if /I "%~2"=="--with-assets" ( set "WA=-with-assets" & set "TGT=%~3" )
  if /I "%~3"=="--with-assets" ( set "WA=-with-assets" )
  if not exist "%VIS_INSTALL%" mkdir "%VIS_INSTALL%"
  echo !TGT!| findstr /R "^[0-9a-fA-F][0-9a-fA-F]*$" >nul
  if !ERRORLEVEL!==0 (
    if not exist "%VIS_INSTALL%\src\.git" git clone --filter=blob:none https://github.com/Blockether/vis.git "%VIS_INSTALL%\src" || exit /b 1
    git -C "%VIS_INSTALL%\src" fetch --filter=blob:none origin || exit /b 1
    git -C "%VIS_INSTALL%\src" checkout --force !TGT! || exit /b 1
    > "%VIS_INSTALL%\mode" echo jvm-sha
    echo vis: now on JVM source @ !TGT!
    exit /b 0
  )
  if "!TGT!"=="" set "TGT=latest"
  set "ASSET=vis-windows-x64!WA!.exe"
  powershell -NoProfile -Command "$t='!TGT!'; $a='!ASSET!'; if($t -eq 'latest'){$u='https://api.github.com/repos/Blockether/vis/releases/latest'}else{$u='https://api.github.com/repos/Blockether/vis/releases/tags/'+$t}; $r=Invoke-RestMethod $u -Headers @{'User-Agent'='vis'}; $url=($r.assets ^| Where-Object name -eq $a).browser_download_url; if(-not $url){Write-Error ('no asset '+$a); exit 1}; Invoke-WebRequest $url -OutFile '%VIS_INSTALL%\native.exe'" || exit /b 1
  > "%VIS_INSTALL%\mode" echo native
  echo vis: updated to native !ASSET!
  exit /b 0
)

REM Strip a leading --jvm flag (force the JVM distribution) and forward the rest.
set "FORCE_JVM=0"
set "ARGS=%*"
if /I "%~1"=="--jvm" (
  set "FORCE_JVM=1"
  set "ARGS=!ARGS:* =!"
)

REM A managed install set up by `vis update` takes precedence over the repo build.
if exist "%VIS_INSTALL%\mode" (
  set /p MODE=<"%VIS_INSTALL%\mode"
  if /I "!MODE!"=="native" if "%FORCE_JVM%"=="0" if exist "%VIS_INSTALL%\native.exe" (
    "%VIS_INSTALL%\native.exe" %ARGS%
    exit /b %ERRORLEVEL%
  )
  if /I "!MODE!"=="jvm-sha" if exist "%VIS_INSTALL%\src" (
    cd /d "%VIS_INSTALL%\src"
    clojure "-J-Duser.dir=%INVOKE_CWD%" -M:vis %ARGS%
    exit /b %ERRORLEVEL%
  )
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
