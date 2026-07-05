#!/usr/bin/env bash
# Build the explicit Vis install artifact consumed by the Harbor agent.
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -P "$here/../../.." && pwd)"
out="$repo_root/target/bench/vis-agent.tar.gz"
stage="$(mktemp -d)"
cleanup() { rm -rf "$stage"; }
trap cleanup EXIT

mkdir -p "$(dirname "$out")" "$stage/vis-agent"

native="${VIS_BENCH_NATIVE:-}"
if [[ -n "$native" && ! -x "$native" ]]; then
  echo "VIS_BENCH_NATIVE is not executable: $native" >&2
  exit 2
fi
for candidate in "$native" "$repo_root/target/vis" "$repo_root/target/vis/vis" "$repo_root/target/native/vis"; do
  [[ -n "$candidate" ]] || continue
  if [[ -x "$candidate" && ! -d "$candidate" ]]; then
    native="$candidate"
    break
  fi
done

allow_source="${VIS_BENCH_ALLOW_SOURCE:-0}"
if [[ -z "$native" && "$allow_source" != "1" ]]; then
  cat >&2 <<EOF
No native Vis binary found for the Harbor artifact.

Build or provide a Linux native image first, then rerun:
  VIS_BENCH_NATIVE=/path/to/linux/vis $here/package_vis.sh

Set VIS_BENCH_ALLOW_SOURCE=1 only for local debugging. Harbor task containers do
not include Clojure, so source-mode artifacts fail during installed-agent setup.
EOF
  exit 2
fi

cat > "$stage/vis-agent/install.sh" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
prefix="${1:-/opt/vis-agent}"
mkdir -p "$prefix"
cd "$(dirname "${BASH_SOURCE[0]}")"
if [[ -x native/vis ]]; then
  install -m 0755 native/vis "$prefix/vis"
elif [[ -f vis-source.tar.gz ]]; then
  mkdir -p "$prefix/source"
  tar -xzf vis-source.tar.gz -C "$prefix/source" --strip-components=1
  chmod +x "$prefix/source/bin/vis"
  cat > "$prefix/vis" <<EOF
#!/usr/bin/env bash
exec "$prefix/source/bin/vis" "\$@"
EOF
  chmod +x "$prefix/vis"
else
  echo "vis-agent artifact is missing native/vis and vis-source.tar.gz" >&2
  exit 2
fi
if [[ -w /usr/local/bin ]]; then
  ln -sf "$prefix/vis" /usr/local/bin/vis
fi
"$prefix/vis" --version
SH
chmod +x "$stage/vis-agent/install.sh"

if [[ -n "$native" ]]; then
  native_file="$(file -b "$native" 2>/dev/null || true)"
  if [[ "$native_file" != *ELF* && "${VIS_BENCH_ALLOW_NON_LINUX_NATIVE:-0}" != "1" ]]; then
    cat >&2 <<EOF
VIS_BENCH_NATIVE must point to a Linux native image for Harbor.

Selected: $native
file(1):  $native_file

Build the native image on Linux, or set VIS_BENCH_ALLOW_NON_LINUX_NATIVE=1 only
for local packaging tests that will not run inside Harbor Docker containers.
EOF
    exit 2
  fi
  mkdir -p "$stage/vis-agent/native"
  cp "$native" "$stage/vis-agent/native/vis"
  chmod +x "$stage/vis-agent/native/vis"
  mode="native"
  native_sha256="$(python3 -c 'import hashlib, sys; h=hashlib.sha256(); p=sys.argv[1]; f=open(p, "rb"); [h.update(b) for b in iter(lambda: f.read(1024 * 1024), b"")]; print(h.hexdigest())' "$native")"
else
  mode="source"
  native_file=""
  native_sha256=""
  tar -C "$repo_root" \
    --exclude='.git' \
    --exclude='.clj-kondo/.cache' \
    --exclude='target' \
    --exclude='.cpcache' \
    --exclude='dev/benches/senior_swe_bench/results' \
    -czf "$stage/vis-agent/vis-source.tar.gz" .
fi

vis_revision="$(git -C "$repo_root" rev-parse HEAD 2>/dev/null || echo unknown)"
source_status_count="$(git -C "$repo_root" status --porcelain 2>/dev/null | wc -l | tr -d ' ')"
source_dirty="false"
if [[ "$source_status_count" != "0" ]]; then
  source_dirty="true"
fi

cat > "$stage/vis-agent/metadata.json" <<EOF
{
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "mode": "$mode",
  "native_file": $(python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$native_file"),
  "native_sha256": $(python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$native_sha256"),
  "vis_revision": "$vis_revision",
  "source_dirty": $source_dirty,
  "source_status_count": $source_status_count,
  "source": "vis/dev/benches/senior_swe_bench/package_vis.sh"
}
EOF

COPYFILE_DISABLE=1 tar --no-xattrs -C "$stage" -czf "$out" vis-agent 2>/dev/null \
  || COPYFILE_DISABLE=1 tar -C "$stage" -czf "$out" vis-agent
echo "$out"
