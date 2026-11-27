#!/usr/bin/env bash
# Install or update a self-hosted factory on one Linux machine with systemd.
#
#   VIS_BIN=/opt/vis/bin FACTORY_HOST=0.0.0.0 deploy/install.sh
#
# A second run with the same values changes nothing. The script keeps the settings of
# the last run in <root>/env, so a later run needs only the values that change.
#
#   VIS_BIN               The directory of a Vis release: vis-agent-native and vis-agent-python. Necessary.
#   FACTORY_ROOT          The directory of the factory. Default: /opt/factory.
#   FACTORY_HOST, FACTORY_PORT   The address where the factory listens. Default: 127.0.0.1 and 8090.
#   FACTORY_GATEWAY_PORT  The loopback port of the Vis gateway of the factory. Default: 8091.
#   FACTORY_PUBLIC_URL    The address that people use, when a proxy is in front.
#   FACTORY_NAME          The name on the pages. Default: Factory.
#   FACTORY_DATABASE      sqlite:///<file>, postgres://... or d1://<account>/<database>. Default: SQLite in <root>/data.
#   FACTORY_PROVIDER      The provider of a run whose workflow names a model.
#   VIS_SOURCE            The Git repository of Vis. Default: https://github.com/Blockether/vis.
#   VIS_WEB_DIR           A build of Vis web. Default: the script builds it from VIS_SOURCE with Node.js.
#
# The files in <root>:
#   app/      the code of the factory          site/     its Python packages
#   home/     the home of the Vis gateway      data/     the database and the logs
#   project/  the Git repository where each session and each run works
#   workflows/  the Vis project of the workflow code; the database keeps each version
#   env       the settings                     secret.env   the token of the factory
set -euo pipefail
umask 077

die() { echo "install: $*" >&2; exit 1; }
say() { echo "install: $*"; }

APP="$(cd -P "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="${FACTORY_ROOT:-/opt/factory}"
# The memory extension: the commit of the release 0.7.3.
OPTMEM_SOURCE=https://github.com/Blockether/vis-optmem
OPTMEM_REVISION=0a4083c23299bc715d94ca79bf379184b5e6fbc4

# A value of this run, else the value of the last run, else the default. An empty value
# of this run removes the value of the last run.
setting() {
  local value=""
  if [[ -n "${!1+set}" ]]; then
    value="${!1}"
  elif [[ -f "$ROOT/env" ]]; then
    value="$(sed -n "s/^$1=//p" "$ROOT/env" | tail -n 1)"
  fi
  printf '%s' "${value:-$2}"
}

[[ "$(id -u)" == 0 ]] || die "run as root: the script writes systemd units"
for tool in systemctl git curl; do command -v "$tool" >/dev/null || die "'$tool' is necessary"; done

VIS_BIN="$(setting VIS_BIN "")"
[[ -n "$VIS_BIN" ]] || die "set VIS_BIN to the directory of a Vis release"
ENGINE="$VIS_BIN/vis-agent-native"
PYTHON="$VIS_BIN/vis-agent-python/python/bin/python3"
[[ -x "$ENGINE" && -x "$PYTHON" ]] || die "$VIS_BIN is not a Vis release: no vis-agent-native or no vis-agent-python"
read -r VERSION COMMIT _ < "$ENGINE.build" || die "no build stamp $ENGINE.build"

HOST="$(setting FACTORY_HOST 127.0.0.1)"
PORT="$(setting FACTORY_PORT 8090)"
GATEWAY_PORT="$(setting FACTORY_GATEWAY_PORT 8091)"
PUBLIC_URL="$(setting FACTORY_PUBLIC_URL "")"
NAME="$(setting FACTORY_NAME Factory)"
DATABASE="$(setting FACTORY_DATABASE "sqlite:///$ROOT/data/factory.db")"
PROVIDER="$(setting FACTORY_PROVIDER "")"
VIS_SOURCE="$(setting VIS_SOURCE https://github.com/Blockether/vis)"
WEB="$(setting VIS_WEB_DIR "")"
# The address of the factory for the processes on this machine.
case "$HOST" in
  0.0.0.0 | ::) LOCAL_URL="http://127.0.0.1:$PORT" ;;
  *:*) LOCAL_URL="http://[$HOST]:$PORT" ;;
  *) LOCAL_URL="http://$HOST:$PORT" ;;
esac

mkdir -p "$ROOT" "$ROOT/data" "$ROOT/home/.vis/extensions"
cat > "$ROOT/env.new" <<EOF
VIS_BIN=$VIS_BIN
VIS_SOURCE=$VIS_SOURCE
VIS_WEB_DIR=$WEB
FACTORY_HOST=$HOST
FACTORY_PORT=$PORT
FACTORY_GATEWAY_PORT=$GATEWAY_PORT
FACTORY_PUBLIC_URL=$PUBLIC_URL
FACTORY_URL=$LOCAL_URL
FACTORY_NAME=$NAME
FACTORY_DATABASE=$DATABASE
FACTORY_PROVIDER=$PROVIDER
FACTORY_HOME=$ROOT/data
FACTORY_PROJECT=$ROOT/project
FACTORY_WORKFLOWS=$ROOT/workflows
VIS_GATEWAY_URL=http://127.0.0.1:$GATEWAY_PORT
VIS_GATEWAY_TOKEN_FILE=$ROOT/home/.vis/gateway.token
EOF
mv "$ROOT/env.new" "$ROOT/env"
# The token stays the same on each run. A person signs in with it.
[[ -s "$ROOT/secret.env" ]] || echo "FACTORY_TOKEN=$("$PYTHON" -c 'import secrets; print(secrets.token_urlsafe(32))')" > "$ROOT/secret.env"

# -- the Vis source of the commit of the engine: the SDK and Vis web come from it -----
[[ -d "$ROOT/vis/.git" ]] || git init --quiet "$ROOT/vis"
if [[ "$(git -C "$ROOT/vis" rev-parse --verify --quiet HEAD || true)" != "$COMMIT" ]]; then
  say "fetching Vis $VERSION $COMMIT"
  git -C "$ROOT/vis" fetch --quiet --depth 1 "$VIS_SOURCE" "$COMMIT"
  git -C "$ROOT/vis" checkout --quiet --detach --force FETCH_HEAD
fi

# -- the Python packages, for the Python of the release -------------------------------
# The packages of pyproject.toml that do not come from Vis, with the packages of one extra.
needs() {
  "$PYTHON" - "$APP/pyproject.toml" "$1" <<'PY'
import sys
import tomllib

project = tomllib.load(open(sys.argv[1], "rb"))["project"]
needs = project["dependencies"] + (project["optional-dependencies"][sys.argv[2]] if sys.argv[2] else [])
print("\n".join(need for need in needs if not need.startswith(("vis-agent", "vis-optmem"))))
PY
}
case "$DATABASE" in postgres*) extra=postgres ;; *) extra="" ;; esac
stamp="$COMMIT $OPTMEM_REVISION $extra $(sha256sum < "$APP/pyproject.toml")"
if [[ "$(cat "$ROOT/site/.stamp" 2>/dev/null || true)" != "$stamp" ]]; then
  say "installing the Python packages"
  mapfile -t needs < <(needs "$extra")
  rm -rf "$ROOT/site.new"
  "$PYTHON" -m pip install --quiet --no-cache-dir --disable-pip-version-check --root-user-action ignore --target "$ROOT/site.new" \
    "${needs[@]}" "$ROOT/vis/packages/vis-agent" "vis-optmem @ git+$OPTMEM_SOURCE@$OPTMEM_REVISION"
  echo "$stamp" > "$ROOT/site.new/.stamp"
  rm -rf "$ROOT/site" && mv "$ROOT/site.new" "$ROOT/site"
fi

# -- Vis web: the chats and the sessions ----------------------------------------------
if [[ -z "$WEB" ]]; then
  WEB="$ROOT/web"
  if [[ "$(cat "$WEB/.stamp" 2>/dev/null || true)" != "$COMMIT" ]]; then
    for tool in node npm; do command -v "$tool" >/dev/null || die "'$tool' builds Vis web; install Node.js or set VIS_WEB_DIR"; done
    say "building Vis web"
    (cd "$ROOT/vis/apps/vis-companion" && npm ci --silent && npm run --silent build:web) > "$ROOT/data/web-build.log" 2>&1 ||
      die "the build of Vis web failed; see $ROOT/data/web-build.log"
    rm -rf "$ROOT/web.new" && cp -R "$ROOT/vis/apps/vis-companion/dist-web" "$ROOT/web.new"
    echo "$COMMIT" > "$ROOT/web.new/.stamp"
    rm -rf "$ROOT/web" && mv "$ROOT/web.new" "$ROOT/web"
  fi
fi
[[ -f "$WEB/index.html" ]] || die "$WEB has no index.html"

# -- the code of the factory ----------------------------------------------------------
if [[ "$APP" != "$ROOT/app" ]]; then
  rm -rf "$ROOT/app.new" && mkdir "$ROOT/app.new"
  cp -R "$APP/factory" "$APP/web" "$APP/workflows" "$APP/extension" "$APP/pyproject.toml" "$ROOT/app.new/"
  rm -rf "$ROOT/app" && mv "$ROOT/app.new" "$ROOT/app"
fi

# -- the two Vis projects: the work of the factory, and the code of its workflows ------
mkdir -p "$ROOT/project" "$ROOT/workflows"
# The factory gives the workflow project its files: the database keeps the workflow code.
for project in project workflows; do
  [[ -f "$ROOT/$project/vis.yml" ]] || cat > "$ROOT/$project/vis.yml" <<EOF
# Each session of the factory reads and writes the memories of the factory.
environment:
  FACTORY_URL: {literal: "$LOCAL_URL"}
  FACTORY_TOKEN: {env: FACTORY_TOKEN}
  MEMORY_STORE: {literal: "vis_optmem.remote:RemoteStore"}
  MEMORY_STORE_CONFIG: {literal: '{"url": "$LOCAL_URL/factory/api/memory", "memory": "personal"}'}
  MEMORY_TEAM_STORE: {literal: "vis_optmem.remote:RemoteStore"}
  MEMORY_TEAM_STORE_CONFIG: {literal: '{"url": "$LOCAL_URL/factory/api/memory", "memory": "team"}'}
  MEMORY_STORE_TOKEN: {env: FACTORY_TOKEN}
EOF
done
if [[ ! -d "$ROOT/project/.git" ]]; then
  git init --quiet "$ROOT/project"
  git -C "$ROOT/project" add --all
  git -C "$ROOT/project" -c user.name=Factory -c user.email=factory@blockether.com commit --quiet --message "chore: start the factory project"
fi

# -- the Vis gateway of the factory ---------------------------------------------------
# The extension imports the package `factory`. An entry with that name hides the package,
# so the entry has another name, and the package is an import root of its own. The
# entry names its packages in a PEP 723 block; the script adds the import root to it.
rm -rf "$ROOT/tools.new" && mkdir "$ROOT/tools.new" && cp -R "$ROOT/app/factory" "$ROOT/tools.new/"
rm -rf "$ROOT/tools" && mv "$ROOT/tools.new" "$ROOT/tools"
entry="$ROOT/home/.vis/extensions/factory_tools.py"
"$PYTHON" - "$ROOT/app/extension/factory.py" "$ROOT/tools" > "$entry.new" <<'PY'
import sys

text = open(sys.argv[1]).read()
end = "\n# ///\n"
assert text.count(end) == 1, "extension/factory.py needs one PEP 723 block"
print(text.replace(end, f'\n# [tool.vis]\n# source_paths = ["{sys.argv[2]}"]{end}'), end="")
PY
mv "$entry.new" "$entry"
# The gateway and a person own this file after the first run: a provider goes here.
[[ -f "$ROOT/home/.vis/config.yml" ]] || cat > "$ROOT/home/.vis/config.yml" <<EOF
extensions:
  vis-optmem:
    source: $OPTMEM_SOURCE
    revision: $OPTMEM_REVISION
EOF
certificates=/etc/ssl/certs/ca-certificates.crt
gateway_environment() {
  echo "HOME=$ROOT/home"
  echo "VIS_HOME=$ROOT/home/.vis"
  echo "PATH=$VIS_BIN:/usr/local/bin:/usr/bin:/bin"
  echo "VIS_PYTHON_NATIVE_PATH=$VIS_BIN/vis-agent-python/libvispython.so"
  echo "VIS_WEB_DIR=$WEB"
  if [[ -f "$certificates" ]]; then echo "SSL_CERT_FILE=$certificates"; fi
}
say "installing the extensions"
mapfile -t gateway_values < <(gateway_environment)
env -i "${gateway_values[@]}" "$ENGINE" "-Duser.home=$ROOT/home" extension sync --global --trust

cat > /etc/systemd/system/factory-gateway.service <<EOF
[Unit]
Description=Vis gateway of the factory $NAME
After=network-online.target
Wants=network-online.target

[Service]
WorkingDirectory=$ROOT/project
$(gateway_environment | sed 's/^/Environment=/')
EnvironmentFile=$ROOT/secret.env
ExecStart=$ENGINE -Duser.home=$ROOT/home gateway start --host 127.0.0.1 --port $GATEWAY_PORT --token-file $ROOT/home/.vis/gateway.token --require-token
Restart=always
RestartSec=5
# The gateway exits with 143 on SIGTERM.
SuccessExitStatus=143
UMask=0077
LimitNOFILE=65536
StandardOutput=append:$ROOT/data/gateway.log
StandardError=append:$ROOT/data/gateway.log

[Install]
WantedBy=multi-user.target
EOF
cat > /etc/systemd/system/factory.service <<EOF
[Unit]
Description=Factory $NAME
After=factory-gateway.service
Wants=factory-gateway.service

[Service]
WorkingDirectory=$ROOT/project
Environment=HOME=$ROOT/home
Environment=PATH=$VIS_BIN:/usr/local/bin:/usr/bin:/bin
Environment=PYTHONPATH=$ROOT/app:$ROOT/site
Environment=PYTHONUNBUFFERED=1
EnvironmentFile=$ROOT/env
EnvironmentFile=$ROOT/secret.env
ExecStart=$PYTHON -m factory
Restart=always
RestartSec=5
UMask=0077
StandardOutput=append:$ROOT/data/factory.log
StandardError=append:$ROOT/data/factory.log

[Install]
WantedBy=multi-user.target
EOF
systemctl daemon-reload
systemctl enable --quiet factory-gateway.service factory.service
systemctl restart factory-gateway.service factory.service

# -- the check ------------------------------------------------------------------------
for _ in $(seq 60); do
  state="$(curl --silent --max-time 3 --globoff --header "Authorization: Bearer $(sed -n 's/^FACTORY_TOKEN=//p' "$ROOT/secret.env")" "$LOCAL_URL/factory/api/status" || true)"
  case "$state" in *'"ok": true'* | *'"ok":true'*) break ;; esac
  sleep 2
done
case "$state" in
  *'"ok": true'* | *'"ok":true'*) say "the factory runs on Vis $VERSION $COMMIT: ${PUBLIC_URL:-$LOCAL_URL}/factory" ;;
  *) die "the factory did not start; see $ROOT/data/factory.log and $ROOT/data/gateway.log" ;;
esac
say "the token for the sign-in is in $ROOT/secret.env"
