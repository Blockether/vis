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
  local err
  err=$(mktemp)

  # Compiler walk: load EVERY project .clj/.cljc with reflection + boxed-math
  # warnings enabled, then count what escapes.
  #
  # Classpath: use the project's OWN `deps.edn` (plain `clojure -M -e`, no :vis
  # alias so :main-opts stay out of the way). deps.edn already declares every
  # extension as a `:local/root` dep, so the walker's classpath matches the
  # production runtime automatically. Do NOT re-declare the extensions via
  # `-Sdeps`: that hand-maintained list ROTS the instant an extension is added,
  # renamed, or moved, and a bad `:local/root` makes `clojure` abort building
  # the classpath BEFORE the walk runs -- which (swallowed by `|| true`) yields
  # an empty stderr and a silent FALSE PASS. (That is exactly the bug this
  # replaces.) `benchmark/` is intentionally NOT walked.
  clojure -M -e '
    (set! *warn-on-reflection* true)
    (set! *unchecked-math* :warn-on-boxed)
    (let [root-src (clojure.java.io/file "src")
          ext-srcs (->> (.listFiles (clojure.java.io/file "extensions"))
                     (filter (fn [^java.io.File d] (and (some? d) (.isDirectory d))))
                     (mapcat (fn [^java.io.File category] (.listFiles category)))
                     (filter (fn [^java.io.File d] (and (some? d) (.isDirectory d))))
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
      (binding [*out* *err*] (println "WALKED" (count clj-files) "files"))
      (doseq [^java.io.File f clj-files]
        (try (load-file (.getPath f))
          (catch Throwable e
            (binding [*out* *err*]
              (println "LOAD-ERROR" (.getPath f) "-" (.getMessage e)))))))' \
    > /dev/null 2> "$err" || true

  # Guard against the false-pass class of bug: if the walk did not actually
  # load files (classpath error, crash before the loop), fail loudly instead
  # of reporting a clean zero.
  local walked
  walked=$(grep -E "^WALKED [0-9]+ files" "$err" | head -1 | awk '{print $2}')
  if [ -z "$walked" ] || [ "$walked" -lt 100 ]; then
    echo "FAILED: compiler walk did not run (walked='${walked:-none}')."
    echo "The classpath or walker crashed before loading source. See $err:"
    head -20 "$err" | sed 's/^/  /'
    return 1
  fi

  # Keep only PROJECT warnings and dedup. Every project path -- absolute
  # (/.../vis/src/...) or classpath-relative (com/blockether/vis/...) -- contains
  # the `com/blockether/vis/` namespace root; third-party jars (cljfmt, svar,
  # anomaly, ...) do not. A file required transitively AND then load-file'd emits
  # the same warning twice under two path spellings, so normalize both to the
  # `com/blockether/vis/` form before `sort -u`.
  local filtered
  filtered=$(grep -E "Reflection warning|Boxed math warning" "$err" \
    | grep "com/blockether/vis/" \
    | sed -E 's#(Reflection warning|Boxed math warning), .*(com/blockether/vis/)#\1, \2#' \
    | sort -u)

  local refl_count boxed_count load_errs
  refl_count=$( echo "$filtered" | grep -c "Reflection warning" || true)
  boxed_count=$(echo "$filtered" | grep -c "Boxed math warning" || true)
  load_errs=$(  grep -c "^LOAD-ERROR" "$err" || true)

  echo "GraalVM safety walk: loaded $walked source files"
  echo "  reflection warnings (project): $refl_count"
  echo "  boxed-math warnings (project): $boxed_count"
  echo "  load errors: $load_errs"
  echo ""

  if [ -n "$filtered" ]; then
    echo "Per-package breakdown (reflection):"
    echo "$filtered" | grep "Reflection warning" \
      | sed -E 's#Reflection warning, com/blockether/vis/(ext/[^/]+|internal).*#\1#' \
      | sort | uniq -c | sort -rn | sed 's/^/  /'
    echo ""
  fi

  if [ "$load_errs" -gt 0 ]; then
    echo "FAILED: $load_errs file(s) failed to compile (see $err)."
    grep "^LOAD-ERROR" "$err" | head -20 | sed 's/^/  /'
    return 1
  fi

  # --- Reflection: HARD gate ------------------------------------------------
  # Every reflective call is a GraalVM native-image hazard (needs an explicit
  # reachability-metadata entry or it fails at run time). The project must carry
  # ZERO. Fix each with a type hint; there is no ratchet here.
  if [ "$refl_count" -gt 0 ]; then
    echo "Reflection offenders (fix with type hints):"
    echo "$filtered" | grep "Reflection warning" | head -50 | sed 's/^/  /'
    echo ""
    echo "FAILED: $refl_count project reflection warning(s). Reflection must be ZERO."
    echo "Full walk output: $err"
    return 1
  fi

  # --- Boxed math: advisory ratchet ----------------------------------------
  # Boxed arithmetic is a perf smell, not a correctness/native hazard, and the
  # repo carries a large historical backlog. Ratchet it (can only shrink);
  # --strict demands zero.
  if [ "$GRAAL_STRICT" = "true" ]; then
    if [ "$boxed_count" -gt 0 ]; then
      echo "FAILED (--strict): $boxed_count boxed-math warning(s)."
      return 1
    fi
    echo "GraalVM strict: 0 reflection, 0 boxed-math"
    return 0
  fi

  local baseline
  if [ -f "$GRAAL_BASELINE_FILE" ]; then
    baseline=$(cat "$GRAAL_BASELINE_FILE")
  else
    baseline=""
  fi

  if [ -z "$baseline" ]; then
    echo "No boxed-math baseline at $GRAAL_BASELINE_FILE."
    echo "Reflection is 0 (good). Run --update-baseline to snapshot boxed-math ($boxed_count)."
    return 0
  fi

  if [ "$boxed_count" -gt "$baseline" ]; then
    echo "FAILED: boxed-math grew $baseline -> $boxed_count (regression of $((boxed_count - baseline)))."
    echo "Either fix the new boxed math or, if intentional, run:"
    echo "  ./verify.sh --update-baseline"
    return 1
  fi

  if [ "$boxed_count" -lt "$baseline" ]; then
    echo "Improvement: boxed-math $baseline -> $boxed_count (-$((baseline - boxed_count))). Consider:"
    echo "  ./verify.sh --update-baseline"
  else
    echo "Ratchet: boxed-math $boxed_count <= baseline $baseline (no regression); reflection 0."
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
  # The step ran with the OLD baseline; re-extract the boxed-math count from
  # its log (reflection is a hard-zero gate and is NOT baselined).
  local total
  total=$(grep -E "^  boxed-math warnings \(project\): " "$VERIFY_DIR/graal.log" | head -1 | awk '{print $NF}')
  if [ -z "$total" ]; then
    echo "Could not extract boxed-math count. See $VERIFY_DIR/graal.log"
    exit 1
  fi
  echo "$total" > "$GRAAL_BASELINE_FILE"
  echo ""
  echo "Boxed-math baseline updated: $GRAAL_BASELINE_FILE = $total"
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
