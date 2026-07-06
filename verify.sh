#!/usr/bin/env bash
# verify.sh - pre-commit verification for vis
#
# Modes:
#   ./verify.sh                 # --full (default): every gate
#   ./verify.sh --quick | -q    # format + lint only (~10s)
#   ./verify.sh --graal         # just the GraalVM safety check
#   ./verify.sh --strict        # graal step demands ZERO warnings (no ratchet)
#   ./verify.sh --update-baseline
#                               # snapshot current graal warning count as baseline
#
# Each gate writes:
#   .verification/<step>.log    - stdout + stderr
#   .verification/<step>.code   - exit code (0 / N / "skip")
#   .verification/summary.log   - final report
#
# `.verification/` is gitignored. The graal warning baseline lives in
# `.verification-baseline/graal-warnings.count` (tracked) so the ratchet
# is shared across machines and CI.
#
# A failing gate stops the run. Fix the issue and re-run.

set -uo pipefail

cd "$(dirname "$0")"

VERIFY_DIR=".verification"
BASELINE_DIR=".verification-baseline"
GRAAL_BASELINE_FILE="$BASELINE_DIR/graal-warnings.count"

rm -rf "$VERIFY_DIR"
mkdir -p "$VERIFY_DIR" "$BASELINE_DIR"

TOTAL=0
PASSED=0
SKIPPED=0
FAILED_STEP=""

# --- Colors (disabled when piped) ---
if [ -t 1 ]; then
  RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
  BLUE='\033[0;34m'; BOLD='\033[1m'; NC='\033[0m'
else
  RED=''; GREEN=''; YELLOW=''; BLUE=''; BOLD=''; NC=''
fi

# --- Step runner: captures output, writes log+code, returns 1 on failure. ---
step() {
  local name="$1" desc="$2"
  shift 2
  TOTAL=$((TOTAL + 1))
  printf "${BLUE}[%02d]${NC} %-45s " "$TOTAL" "$desc"

  if "$@" > "$VERIFY_DIR/$name.log" 2>&1; then
    echo "0" > "$VERIFY_DIR/$name.code"
    PASSED=$((PASSED + 1))
    printf "${GREEN}PASS${NC}\n"
    return 0
  else
    local code=$?
    echo "$code" > "$VERIFY_DIR/$name.code"
    FAILED_STEP="$name"
    printf "${RED}FAIL${NC} (exit $code)\n"
    printf "  ${RED}see: $VERIFY_DIR/$name.log${NC}\n"
    echo ""
    tail -25 "$VERIFY_DIR/$name.log" | sed 's/^/    /'
    echo ""
    return 1
  fi
}

skip() {
  local name="$1" desc="$2" reason="$3"
  TOTAL=$((TOTAL + 1))
  SKIPPED=$((SKIPPED + 1))
  PASSED=$((PASSED + 1))
  printf "${BLUE}[%02d]${NC} %-45s ${YELLOW}SKIP${NC} (%s)\n" "$TOTAL" "$desc" "$reason"
  echo "skip" > "$VERIFY_DIR/$name.code"
  echo "Skipped: $reason" > "$VERIFY_DIR/$name.log"
}

summary() {
  local failed=$((TOTAL - PASSED))
  echo ""
  if [ $failed -eq 0 ]; then
    printf "${GREEN}${BOLD}All %d steps passed" "$TOTAL"
    [ $SKIPPED -gt 0 ] && printf " (%d skipped)" "$SKIPPED"
    printf "${NC}\n"
  else
    printf "${RED}${BOLD}Failed at step: %s (%d of %d passed)${NC}\n" "$FAILED_STEP" "$PASSED" "$TOTAL"
    printf "Fix the issue, then re-run: ${BOLD}./verify.sh${NC}\n"
  fi
  echo ""

  {
    echo "verify.sh - $(date -u '+%Y-%m-%dT%H:%M:%SZ')"
    echo "total=$TOTAL passed=$PASSED failed=$failed skipped=$SKIPPED"
    [ -n "$FAILED_STEP" ] && echo "stopped_at=$FAILED_STEP"
    echo ""
    for f in "$VERIFY_DIR"/*.code; do
      [ -f "$f" ] || continue
      local sname
      sname=$(basename "$f" .code)
      [ "$sname" = "summary" ] && continue
      printf "  %-25s %s\n" "$sname" "$(cat "$f")"
    done
  } > "$VERIFY_DIR/summary.log"

  [ $failed -eq 0 ]
}

# =============================================================================
# Gate implementations
# =============================================================================

# --- Format: codestyle (zprint) check via the :format alias. Config lives in
#     `.zprint.edn`; failing tells the dev to run the fix command. ---
_format() {
  local format_paths=(src extensions build.clj)
  [ -d test ] && format_paths+=(test)
  clojure -M:format check "${format_paths[@]}" || {
    echo ""
    echo "FAILED: codestyle found formatting issues."
    echo "Fix with:  clojure -M:format fix ${format_paths[*]}"
    return 1
  }
  echo "codestyle: clean"
}

# --- Lint: clj-kondo across every package src tree. ---
# Errors and warnings fail the build; info-level diagnostics are advisory.
_lint() {
  if ! command -v clj-kondo > /dev/null; then
    echo "clj-kondo not found - install with: brew install borkdude/brew/clj-kondo"
    return 1
  fi
  local lint_paths=(src)
  [ -d test ] && lint_paths+=(test)
  # `extensions/<category>/<pkg>/src` is two levels deep.
  for d in extensions/*/*/src extensions/*/*/test; do
    [ -d "$d" ] && lint_paths+=("$d")
  done
  local output code=0
  output=$(clj-kondo --lint "${lint_paths[@]}" 2>&1) || code=$?
  echo "$output"
  # clj-kondo exits non-zero on any finding. Treat error/warning lines as fatal,
  # info-only as advisory.
  local errors warnings
  errors=$(echo "$output"   | grep -cE ":[0-9]+:[0-9]+: error:"   || true)
  warnings=$(echo "$output" | grep -cE ":[0-9]+:[0-9]+: warning:" || true)
  if [ "$errors" -gt 0 ] || [ "$warnings" -gt 0 ]; then
    echo ""
    echo "FAILED: clj-kondo found $errors error(s), $warnings warning(s)."
    return 1
  fi
  echo "clj-kondo: errors=0 warnings=0 (info-level diagnostics may exist; advisory only)"
  return 0
}

# --- GraalVM safety: walks `src/` and every extension src tree, loads
#     each .clj with *warn-on-reflection* + *unchecked-math*
#     :warn-on-boxed, and counts warnings emitted from project source
#     paths.
#
# `benchmark/` is excluded - dev/research code, not on the default
# classpath, not shipped as a runtime jar.
#
# By default this is a RATCHET: fails only if the count grows beyond the
# baseline. Pass --strict to demand zero. ---
GRAAL_STRICT="${GRAAL_STRICT:-false}"

_graal_safety() {
  local out
  out=$(mktemp)
  local err
  err=$(mktemp)

  # Run the compiler walk. We use plain `clojure -M -e` (no :vis alias)
  # to keep :main-opts out of the way. Root `deps.edn` carries the merged
  # library deps; every classpath plug-in lives in an alias (`:vis`,
  # `:test`, `:dev`). Inject all of them via `-Sdeps` so the walker's
  # classpath matches the production runtime (`bin/vis`) and
  # `load-file` can resolve every `:require`.
  clojure \
    -Sdeps '{:deps {com.blockether/vis-common-meta             {:local/root "extensions/common/vis-common-meta"}
                    com.blockether/vis-common-environment      {:local/root "extensions/common/vis-common-environment"}
                    com.blockether/vis-common-editing          {:local/root "extensions/common/vis-common-editing"}
                    com.blockether/vis-persistance-sqlite      {:local/root "extensions/persistance/vis-persistance-sqlite"}
                    com.blockether/vis-provider-github-copilot {:local/root "extensions/providers/vis-provider-github-copilot"}
                    com.blockether/vis-provider-openai-codex   {:local/root "extensions/providers/vis-provider-openai-codex"}
                    com.blockether/vis-channel-telegram        {:local/root "extensions/channels/vis-channel-telegram"}
                    com.blockether/vis-channel-tui             {:local/root "extensions/channels/vis-channel-tui"}
                    com.blockether/vis-foundation-exa                     {:local/root "extensions/common/vis-foundation-exa"}}}' \
    -M -e '
    (set! *warn-on-reflection* true)
    (set! *unchecked-math* :warn-on-boxed)
    (let [;; Root `src/` carries the merged library; classpath plug-ins
          ;; live at `extensions/<category>/<pkg>/src/`. The benchmark
          ;; harness (`benchmarks/`) is intentionally NOT walked.
          root-src (clojure.java.io/file "src")
          ext-srcs (->> (.listFiles (clojure.java.io/file "extensions"))
                     (filter (fn [^java.io.File d]
                               (and (some? d) (.isDirectory d))))
                     (mapcat (fn [^java.io.File category]
                               (.listFiles category)))
                     (filter (fn [^java.io.File d]
                               (and (some? d) (.isDirectory d))))
                     (map (fn [^java.io.File d] (clojure.java.io/file d "src"))))
          pkg-srcs (->> (concat [root-src] ext-srcs)
                     (filter (fn [^java.io.File f] (.exists f))))
          clj-files (mapcat (fn [^java.io.File r]
                              (filter (fn [^java.io.File f]
                                        (and (.isFile f)
                                          (let [n (.getName f)]
                                            (or (.endsWith n ".clj")
                                              (.endsWith n ".cljc")))))
                                (file-seq r)))
                      pkg-srcs)]
      (doseq [^java.io.File f clj-files]
        (try (load-file (.getPath f))
          (catch Throwable e
            (binding [*out* *err*]
              (println "LOAD-ERROR" (.getPath f) "-" (.getMessage e)))))))' \
    > "$out" 2> "$err" || true

  # Filter to project paths only (exclude warnings from third-party jars,
  # whose paths are relative inside the jar, e.g. "clojure+/util.clj").
  local filtered
  # Match warnings from project source paths only (skip third-party
  # jar paths). Three shapes:
  #   /Users/.../vis/src/...                                 (vis-runtime, root)
  #   /Users/.../vis/packages/<pkg>/src/...                  (carved-out core packages)
  #   /Users/.../vis/extensions/<category>/<pkg>/src/...     (every classpath plug-in)
  filtered=$(grep -E "Reflection warning|Boxed math warning" "$err" \
    | grep -E "/vis/(src|extensions/[^/]+/[^/]+/src)/" \
    | sort -u)
  # Load errors are emitted with a leading `LOAD-ERROR ` token; preserve
  # them in $err for the dump above. Track them too so we can flag them.
  # (Already done via `grep -c "^LOAD-ERROR" $err` later in this fn.)
  local refl_count boxed_count load_errs total
  refl_count=$( echo "$filtered" | grep -c "Reflection warning" || true)
  boxed_count=$(echo "$filtered" | grep -c "Boxed math warning" || true)
  load_errs=$(  grep -c "^LOAD-ERROR" "$err" || true)
  total=$((refl_count + boxed_count))

  echo "GraalVM safety walk:"
  echo "  reflection warnings: $refl_count"
  echo "  boxed-math warnings: $boxed_count"
  echo "  total: $total"
  echo "  load errors: $load_errs"
  echo ""

  if [ -n "$filtered" ]; then
    echo "Per-package breakdown (reflection):"
    echo "$filtered" | grep "Reflection warning" \
      | sed -E -e 's#.*/extensions/[^/]+/([^/]+)/src.*#\1#' \
               -e 's#.*/packages/([^/]+)/src.*#\1#' \
               -e 's#.*/vis/src/.*#vis-runtime#' \
      | sort | uniq -c | sort -rn \
      | sed 's/^/  /'
    echo ""
    echo "Per-package breakdown (boxed-math):"
    echo "$filtered" | grep "Boxed math warning" \
      | sed -E -e 's#.*/extensions/[^/]+/([^/]+)/src.*#\1#' \
               -e 's#.*/packages/([^/]+)/src.*#\1#' \
               -e 's#.*/vis/src/.*#vis-runtime#' \
      | sort | uniq -c | sort -rn \
      | sed 's/^/  /'
    echo ""
    echo "Full filtered output: $err"
    echo "First 20 offenders:"
    echo "$filtered" | head -20 | sed 's/^/  /'
  fi

  rm -f "$out"

  if [ "$load_errs" -gt 0 ]; then
    echo ""
    echo "FAILED: $load_errs file(s) failed to compile (see $err)."
    return 1
  fi

  # Strict mode: any warning is fatal.
  if [ "$GRAAL_STRICT" = "true" ]; then
    if [ "$total" -gt 0 ]; then
      echo ""
      echo "FAILED (--strict): $total reflection/boxed-math warning(s)."
      return 1
    fi
    echo "GraalVM strict: 0 warnings"
    return 0
  fi

  # Ratchet mode: compare against baseline.
  local baseline
  if [ -f "$GRAAL_BASELINE_FILE" ]; then
    baseline=$(cat "$GRAAL_BASELINE_FILE")
  else
    baseline=""
  fi

  if [ -z "$baseline" ]; then
    echo "No baseline found at $GRAAL_BASELINE_FILE."
    echo "Run with --update-baseline to snapshot the current count ($total)."
    echo "Treating this run as PASS (no baseline to ratchet against)."
    return 0
  fi

  if [ "$total" -gt "$baseline" ]; then
    echo ""
    echo "FAILED: warning count grew $baseline -> $total (regression of $((total - baseline)))."
    echo "Either fix the new warnings or, if intentional, run:"
    echo "  ./verify.sh --update-baseline"
    return 1
  fi

  if [ "$total" -lt "$baseline" ]; then
    echo "Improvement: $baseline -> $total (-$((baseline - total))). Consider running:"
    echo "  ./verify.sh --update-baseline"
    echo "to lock in the new lower bound."
  else
    echo "Ratchet: $total <= baseline $baseline (no regression)"
  fi
  return 0
}

# --- Tests: aggregate suite via :test alias. ---
_test() {
  clojure -M:test || return 1
}

# --- Smoke test: bin/vis prints the help tree. Doesn't touch the user DB. ---
_smoke() {
  local out
  out=$(./bin/vis 2>&1)
  printf '%s\n' "$out"
  printf '%s\n' "$out" | grep -q "persistent sandboxed Recursive Language Model" || {
    echo "FAILED: bin/vis didn't print the expected help banner."
    return 1
  }
  echo "smoke: bin/vis OK"
}

# --- Git hygiene: trailing whitespace, conflict markers. ---
_git_check() {
  git diff --check HEAD -- . ':!*.png' ':!*.jpg' ':!*.gif' || return 1
  echo "git diff --check: clean"
}

# --- Secret scan: scan diff against base branch for common API-key patterns. ---
_secret_scan() {
  local base
  base=$(git merge-base HEAD origin/main 2>/dev/null \
       || git merge-base HEAD main 2>/dev/null \
       || echo "")
  if [ -z "$base" ]; then
    echo "No base branch found - skipping secret scan"
    return 0
  fi
  # Patterns: OpenAI sk_*, Linear lin_api_*, NVIDIA nvapi-*, Google AIzaSy*,
  # GitHub PAT ghp_*, generic password = "xxxxxxxx".
  local hits
  hits=$(git diff "$base"..HEAD \
    | grep -iE "(sk_[A-Za-z0-9]{20}|lin_api_[A-Za-z0-9]{20}|nvapi-[A-Za-z0-9]{20}|AIzaSy[A-Za-z0-9_-]{20}|ghp_[A-Za-z0-9]{20}|password\s*=\s*[\"'][^\"']{8,})" \
    || true)
  if [ -n "$hits" ]; then
    echo "FAILED: potential secrets in diff:"
    echo "$hits"
    return 1
  fi
  echo "Secret scan: clean"
}

# =============================================================================
# Modes
# =============================================================================

verify_quick() {
  printf "\n${BOLD}Quick verification (format + lint)${NC}\n\n"
  step "format" "Format check (codestyle)"    _format || return 1
  step "lint"   "Lint (clj-kondo)"           _lint   || return 1
  summary
}

verify_graal_only() {
  printf "\n${BOLD}GraalVM safety check only${NC}\n\n"
  step "graal" "GraalVM safety (reflection / boxed math)" _graal_safety || return 1
  summary
}

verify_full() {
  printf "\n${BOLD}Full verification${NC}\n\n"
  step "format"   "Format check (codestyle)"                     _format        || return 1
  step "lint"     "Lint (clj-kondo)"                            _lint          || return 1
  step "graal"    "GraalVM safety (reflection / boxed math)"    _graal_safety  || return 1
  step "test"     "Tests (clojure -M:test)"                     _test          || return 1
  step "smoke"    "Smoke (bin/vis help)"                        _smoke         || return 1
  step "git"      "Git hygiene (diff --check)"                  _git_check     || return 1
  step "secrets"  "Secret scan"                                 _secret_scan   || return 1
  summary
}

update_baseline() {
  printf "\n${BOLD}Updating GraalVM warning baseline${NC}\n\n"
  step "graal" "GraalVM safety (counting only)" _graal_safety || true
  # The step ran with the OLD baseline; re-extract the count from its log.
  local total
  total=$(grep -E "^  total: " "$VERIFY_DIR/graal.log" | head -1 | awk '{print $2}')
  if [ -z "$total" ]; then
    echo "Could not extract total count. See $VERIFY_DIR/graal.log"
    exit 1
  fi
  echo "$total" > "$GRAAL_BASELINE_FILE"
  echo ""
  echo "Baseline updated: $GRAAL_BASELINE_FILE = $total"
  echo "Commit this file to lock in the new bound."
}

# =============================================================================
# Entry point
# =============================================================================

MODE="${1:-full}"

case "$MODE" in
  --quick|-q)         verify_quick ;;
  --graal)            verify_graal_only ;;
  --strict)           GRAAL_STRICT=true verify_full ;;
  --update-baseline)  update_baseline ;;
  --full|-f|*)        verify_full ;;
esac
