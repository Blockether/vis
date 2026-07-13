@echo off
REM Vis CLI launcher for native Windows shells.
REM Resolves deps from the repository root, but preserves the user's
REM invocation directory as JVM user.dir for git/environment context.

setlocal EnableDelayedExpansion
set "INVOKE_CWD=%CD%"
set "BIN_DIR=%~dp0"
for %%I in ("%BIN_DIR%..") do set "REPO_ROOT=%%~fI"

REM ── Prefer a developer checkout at %USERPROFILE%\vis (mirrors bin/vis) ──────
REM A managed install points vis at %USERPROFILE%\.vis\sourcecode; if a dev
REM checkout exists at %USERPROFILE%\vis (or %VIS_DEV_CHECKOUT%) and we were
REM launched from a DIFFERENT repo root, hand off so your working-tree edits win
REM with no resync. VIS_NO_DEV_CHECKOUT breaks the re-exec loop / forces the
REM managed build.
if not defined VIS_DEV_CHECKOUT set "VIS_DEV_CHECKOUT=%USERPROFILE%\vis"
if not defined VIS_NO_DEV_CHECKOUT (
  if /I not "%VIS_DEV_CHECKOUT%"=="%REPO_ROOT%" (
    if exist "%VIS_DEV_CHECKOUT%\bin\vis.cmd" if exist "%VIS_DEV_CHECKOUT%\deps.edn" (
      set "VIS_NO_DEV_CHECKOUT=1"
      call "%VIS_DEV_CHECKOUT%\bin\vis.cmd" %*
      exit /b !ERRORLEVEL!
    )
  )
)

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
REM   vis update [--native]            (in a git checkout) update SOURCE via git
REM                                    pull; --native also rebuilds the binary
REM   vis update --rebuild           ... and BUILD the native from that source
REM   vis update --native            ... and DOWNLOAD the latest release native
REM   vis update <version>|latest   pull a NATIVE binary
REM   vis update <git-sha>            switch to JVM source @ commit
if /I "%~1"=="update" (
  set "TGT=%~2"
  set "REBUILD="
  set "FETCH_NATIVE="
  if /I "%~2"=="--rebuild" ( set "REBUILD=1" & set "TGT=" )
  if /I "%~3"=="--rebuild" ( set "REBUILD=1" )
  if /I "%~2"=="--native"  ( set "FETCH_NATIVE=1" & set "TGT=" )
  if /I "%~3"=="--native"  ( set "FETCH_NATIVE=1" )
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
  REM Bare `vis update` from a git checkout updates the SOURCE (git pull). It does
  REM NOT touch any binary unless --rebuild (build from source) or --native
  REM (download the latest release) is passed.
  if "!TGT!"=="" if exist "%REPO_ROOT%\.git" (
    echo vis: updating source at %REPO_ROOT% ^(git fetch + pull --ff-only^)
    git -C "%REPO_ROOT%" fetch --tags origin || exit /b 1
    git -C "%REPO_ROOT%" pull --ff-only || ( echo vis update: git pull --ff-only failed ^(local changes or diverged history^)>&2 & exit /b 1 )
    if defined REBUILD (
      pushd "%REPO_ROOT%" && clojure -T:build native & popd
      echo vis: source updated + native binary built from source.
      exit /b 0
    )
    if not defined FETCH_NATIVE (
      echo vis: source updated. Use "vis update --rebuild", "vis update --native", or run live source via "vis --source ...".
      exit /b 0
    )
    echo vis: source updated; fetching the latest release native binary...
    REM fall through to the release download below ^(TGT empty -^> latest^)
  )
  if "!TGT!"=="" set "TGT=latest"
  set "ASSET=vis-windows-x64.exe"
  powershell -NoProfile -Command "$t='!TGT!'; $a='!ASSET!'; if($t -eq 'latest'){$u='https://api.github.com/repos/Blockether/vis/releases/latest'}else{$u='https://api.github.com/repos/Blockether/vis/releases/tags/'+$t}; $r=Invoke-RestMethod $u -Headers @{'User-Agent'='vis'}; $url=($r.assets ^| Where-Object name -eq $a).browser_download_url; if(-not $url){Write-Error ('no asset '+$a); exit 1}; Invoke-WebRequest $url -OutFile '%VIS_INSTALL%\native.exe'" || exit /b 1
  > "%VIS_INSTALL%\mode" echo native
  echo vis: updated to native !ASSET!
  exit /b 0
)

REM Strip a leading build-override flag and forward the rest. They override the
REM auto precedence (native > live source; the uberjar is never auto-selected):
REM   --source  live source (clojure -M:vis)   --jar  uberjar   --native  binary
REM   --jvm     back-compat alias for --source
set "DIST="
set "ARGS=%*"
if /I "%~1"=="--source" ( set "DIST=source" & set "ARGS=!ARGS:* =!" )
if /I "%~1"=="--jvm"    ( set "DIST=source" & set "ARGS=!ARGS:* =!" )
if /I "%~1"=="--jar"    ( set "DIST=jar"    & set "ARGS=!ARGS:* =!" )
if /I "%~1"=="--native" ( set "DIST=native" & set "ARGS=!ARGS:* =!" )

REM Explicit override wins over everything.
if /I "%DIST%"=="jar" (
  if exist "%REPO_ROOT%\target\vis.jar" (
    java "-Duser.dir=%INVOKE_CWD%" -jar "%REPO_ROOT%\target\vis.jar" %ARGS%
    exit /b %ERRORLEVEL%
  )
  echo vis: --jar requested but target\vis.jar is missing ^(build with "vis uber"^); using live source. 1>&2
)
if /I "%DIST%"=="native" (
  if exist "%REPO_ROOT%\target\vis.exe" (
    "%REPO_ROOT%\target\vis.exe" %ARGS%
    exit /b %ERRORLEVEL%
  )
  if exist "%VIS_INSTALL%\native.exe" (
    "%VIS_INSTALL%\native.exe" %ARGS%
    exit /b %ERRORLEVEL%
  )
  echo vis: --native requested but no native binary found ^(build with "vis native"^); using live source. 1>&2
)

REM Auto precedence (no override): NATIVE if present, else LIVE SOURCE. The
REM uberjar is never auto-selected (it shadows working-tree edits) — only --jar.
REM Inside the repo that holds this launcher we run live source so a dev's edits
REM win (set VIS_PREBUILT=1 to use the build in-repo). A managed install
REM (`vis update`) still wins when you're outside the repo.
if not defined DIST (
  set "IN_REPO=0"
  if /I "!INVOKE_CWD:%REPO_ROOT%=!" NEQ "!INVOKE_CWD!" set "IN_REPO=1"
  if defined VIS_PREBUILT set "IN_REPO=0"
  if "!IN_REPO!"=="0" (
    if exist "%VIS_INSTALL%\mode" (
      set /p MODE=<"%VIS_INSTALL%\mode"
      if /I "!MODE!"=="native" if exist "%VIS_INSTALL%\native.exe" (
        "%VIS_INSTALL%\native.exe" %ARGS%
        exit /b %ERRORLEVEL%
      )
      if /I "!MODE!"=="jvm-sha" if exist "%VIS_INSTALL%\src" (
        cd /d "%VIS_INSTALL%\src"
        clojure "-J-Duser.dir=%INVOKE_CWD%" -M:vis %ARGS%
        exit /b %ERRORLEVEL%
      )
    )
    if exist "%REPO_ROOT%\target\vis.exe" (
      "%REPO_ROOT%\target\vis.exe" %ARGS%
      exit /b %ERRORLEVEL%
    )
  )
)

REM Live source.
cd /d "%REPO_ROOT%" || exit /b 1
clojure "-J-Duser.dir=%INVOKE_CWD%" -M:vis %ARGS%
exit /b %ERRORLEVEL%
