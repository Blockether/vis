param(
  [string]$Dest = $(if ($env:VIS_SOURCE_DIR) { $env:VIS_SOURCE_DIR } else { Join-Path $HOME ".vis\sourcecode" }),
  [string]$Repo = $(if ($env:VIS_REPO_URL) { $env:VIS_REPO_URL } else { "https://github.com/Blockether/vis.git" }),
  [string]$Branch = $(if ($env:VIS_REPO_BRANCH) { $env:VIS_REPO_BRANCH } else { "" }),
  [string]$LocalBinDir = $(if ($env:VIS_LOCAL_BIN_DIR) { $env:VIS_LOCAL_BIN_DIR } else { Join-Path $HOME ".local\bin" }),
  [switch]$Fork = $(if ($env:VIS_FORK -match '^(1|true|yes)$') { $true } else { $false }),
  [string]$ForkSource = $(if ($env:VIS_FORK_SOURCE) { $env:VIS_FORK_SOURCE } else { "Blockether/vis" }),
  [string]$ForkOwner = $(if ($env:VIS_FORK_OWNER) { $env:VIS_FORK_OWNER } else { "" }),
  [string]$ForkName = $(if ($env:VIS_FORK_NAME) { $env:VIS_FORK_NAME } else { "" }),
  [switch]$Help
)

$ErrorActionPreference = "Stop"

$VisCmd = Join-Path $LocalBinDir "vis.cmd"
$UpstreamRepo = ""

function Show-Usage {
  Write-Host @"
Usage: powershell -ExecutionPolicy Bypass -File bin\install-source.ps1 [-Dest PATH] [-Repo URL] [-Branch NAME] [-Fork] [-ForkSource OWNER/REPO] [-ForkOwner OWNER_OR_ORG] [-ForkName NAME]

Checks for Java, Clojure CLI, and git, then clones Vis to:
  $Dest

It also creates the launcher shim:
  $VisCmd

Environment overrides:
  VIS_SOURCE_DIR     checkout path (default: `$HOME\.vis\sourcecode)
  VIS_LOCAL_BIN_DIR  shim dir (default: `$HOME\.local\bin)
  VIS_REPO_URL       git remote (default: $Repo)
  VIS_REPO_BRANCH    branch/tag to clone or update
  VIS_FORK           true to create/clone a GitHub fork with gh
  VIS_FORK_SOURCE    source repo for -Fork (default: Blockether/vis)
  VIS_FORK_OWNER     fork owner/org for -Fork (default: authenticated user)
  VIS_FORK_NAME      fork repo name for -Fork (default: source repo name)
"@
}

if ($Help) {
  Show-Usage
  exit 0
}

function Test-Command($Name) {
  return $null -ne (Get-Command $Name -ErrorAction SilentlyContinue)
}

function ConvertTo-GitHubSlug($Raw) {
  $slug = $Raw
  $slug = $slug -replace '^https://github\.com/', ''
  $slug = $slug -replace '^http://github\.com/', ''
  $slug = $slug -replace '^git@github\.com:', ''
  $slug = $slug -replace '\.git$', ''
  $slug = $slug.TrimStart('/')
  if ($slug -notmatch '^[^/]+/[^/]+$') {
    throw "vis install: expected a GitHub repo as OWNER/REPO, got: $Raw"
  }
  return $slug
}

function Get-GitHubRepoUrl($Slug) {
  $protocol = ""
  if ($env:VIS_GH_PROTOCOL) {
    $protocol = $env:VIS_GH_PROTOCOL
  } else {
    $protocol = (& gh config get git_protocol -h github.com 2>$null).Trim()
  }

  if ($protocol -eq "ssh") {
    return "git@github.com:$Slug.git"
  }
  return "https://github.com/$Slug.git"
}

function Ensure-GitHubFork {
  & gh auth status -h github.com *> $null
  if ($LASTEXITCODE -ne 0) {
    throw "vis install: GitHub CLI is not authenticated. Run: gh auth login"
  }

  $SourceSlug = ConvertTo-GitHubSlug $ForkSource
  $Login = (& gh api user --jq .login).Trim()
  if ($LASTEXITCODE -ne 0 -or -not $Login) {
    throw "vis install: could not read authenticated GitHub user via gh"
  }

  $Owner = $(if ($ForkOwner) { $ForkOwner } else { $Login })
  $Name = $(if ($ForkName) { $ForkName } else { ($SourceSlug -split '/')[1] })
  $ForkSlug = "$Owner/$Name"

  & gh repo view $ForkSlug *> $null
  if ($LASTEXITCODE -eq 0) {
    Write-Host "vis install: using existing GitHub fork: $ForkSlug"
  } else {
    Write-Host "vis install: creating GitHub fork: $SourceSlug -> $ForkSlug"
    $GhArgs = @("repo", "fork", $SourceSlug)
    if ($ForkName) {
      $GhArgs += "--fork-name"
      $GhArgs += $ForkName
    }
    if ($ForkOwner -and $ForkOwner -ne $Login) {
      $GhArgs += "--org"
      $GhArgs += $ForkOwner
    }
    & gh @GhArgs
    if ($LASTEXITCODE -ne 0) {
      throw "vis install: gh repo fork failed"
    }
  }

  $script:Repo = Get-GitHubRepoUrl $ForkSlug
  $script:UpstreamRepo = Get-GitHubRepoUrl $SourceSlug
}

$missing = @()
$required = @("java", "clojure", "git")
if ($Fork) {
  $required += "gh"
}
foreach ($cmd in $required) {
  if (-not (Test-Command $cmd)) {
    $missing += $cmd
  }
}

if ($missing.Count -gt 0) {
  Write-Error @"
vis install: missing required command(s): $($missing -join ', ')

Install Java 21+, the official Clojure CLI, git, and GitHub CLI when using -Fork; then rerun this script.
Windows examples:
  winget install EclipseAdoptium.Temurin.21.JDK
  winget install Git.Git
  winget install GitHub.cli

Install Clojure CLI from:
  https://clojure.org/guides/install_clojure

Verify with:
  java -version
  clojure -Sdescribe
"@
}

& java -version *> $null
if ($LASTEXITCODE -ne 0) {
  throw "vis install: java exists but failed to run: java -version"
}

& clojure -Sdescribe *> $null
if ($LASTEXITCODE -ne 0) {
  throw "vis install: clojure exists but failed to run: clojure -Sdescribe. Install the official Clojure CLI from https://clojure.org/guides/install_clojure."
}

if ($Fork) {
  Ensure-GitHubFork
}

$Parent = Split-Path -Parent $Dest
if ($Parent) {
  New-Item -ItemType Directory -Force -Path $Parent | Out-Null
}

$GitDir = Join-Path $Dest ".git"
if ((Test-Path $Dest) -and -not (Test-Path $GitDir)) {
  throw "vis install: destination exists but is not a git checkout: $Dest. Move it aside or choose another -Dest."
}

if (-not (Test-Path $GitDir)) {
  Write-Host "vis install: cloning $Repo -> $Dest"
  if ($Branch) {
    & git clone --branch $Branch $Repo $Dest
  } else {
    & git clone $Repo $Dest
  }
  if ($LASTEXITCODE -ne 0) { throw "vis install: git clone failed" }
} else {
  Write-Host "vis install: updating existing checkout: $Dest"
  & git -C $Dest remote set-url origin $Repo
  if ($LASTEXITCODE -ne 0) { throw "vis install: git remote set-url failed" }
  & git -C $Dest fetch --prune origin
  if ($LASTEXITCODE -ne 0) { throw "vis install: git fetch failed" }
  if ($Branch) {
    & git -C $Dest checkout $Branch
    if ($LASTEXITCODE -ne 0) { throw "vis install: git checkout failed" }
    & git -C $Dest pull --ff-only origin $Branch
    if ($LASTEXITCODE -ne 0) { throw "vis install: git pull failed" }
  } else {
    $CurrentBranch = (& git -C $Dest branch --show-current).Trim()
    if ($CurrentBranch) {
      & git -C $Dest pull --ff-only origin $CurrentBranch
      if ($LASTEXITCODE -ne 0) { throw "vis install: git pull failed" }
    } else {
      Write-Warning "vis install: checkout is detached; fetched origin but did not switch branches."
    }
  }
}

if ($UpstreamRepo) {
  & git -C $Dest remote get-url upstream *> $null
  if ($LASTEXITCODE -eq 0) {
    & git -C $Dest remote set-url upstream $UpstreamRepo
    if ($LASTEXITCODE -ne 0) { throw "vis install: git remote set-url upstream failed" }
  } else {
    & git -C $Dest remote add upstream $UpstreamRepo
    if ($LASTEXITCODE -ne 0) { throw "vis install: git remote add upstream failed" }
  }
  Write-Host "vis install: upstream remote -> $UpstreamRepo"
}

New-Item -ItemType Directory -Force -Path $LocalBinDir | Out-Null
$RepoVisCmd = Join-Path $Dest "bin\vis.cmd"
$Marker = "Generated by Vis install-source.ps1"
$Shim = @"
@echo off
REM $Marker
"$RepoVisCmd" %*
exit /b %ERRORLEVEL%
"@

if ((Test-Path $VisCmd) -and -not ((Get-Content -Raw $VisCmd) -like "*$Marker*")) {
  Write-Warning "vis install: not replacing existing launcher: $VisCmd"
  Write-Warning "vis install: move it aside, or run directly: $RepoVisCmd"
} else {
  Set-Content -Path $VisCmd -Value $Shim -Encoding ASCII
  Write-Host "vis install: wrote $VisCmd -> $RepoVisCmd"
}

Write-Host ""
Write-Host "vis install: source checkout ready:"
Write-Host "  $Dest"
Write-Host ""
Write-Host "Try:"
Write-Host "  $VisCmd help"
Write-Host "  $VisCmd doctor"
Write-Host ""
Write-Host "Add Vis to PATH if your shell does not already include `$HOME\.local\bin:"
Write-Host "  setx PATH `"$LocalBinDir;%PATH%`""
