# PROBES.md — Vis probe & optimization runbook

Empirical methodology used to harden Vis tooling. Read this before
running any new investigation; replicate the patterns instead of
inventing your own.

Companion docs:
- `TODO.md` — open work items (T10-T15)
- `AGENTS.md` — agent rules (verify discipline, test pairing)
- `target/probe/logs/*.log` — every probe leaves a full trace here

---

## 1. Probe methodology (the whole loop)

The single loop we re-used for every tool sweep:

```
research → audit → probe → analyse → fix → re-probe → commit
```

1. **Research** (Exa web search). How do industry harnesses do this
   tool? Claude Code, Cline, Roo Code, Aider, Codex, opencode are
   the reference points. Capture:
   - default parameter values (line limit, head_limit, depth)
   - output modes (content / files-only / counts)
   - feature flags (regex, context lines, fuzzy)
   - per-line truncation policy
   - known pain points (search GitHub issues)

2. **Audit**. Compare Vis surface against the harness shortlist.
   List the deltas as a numbered findings table. Rank by severity:
   - 🔥 — feature missing where every other harness has it
   - ⚠ — present but materially worse (e.g. hardcoded limit)
   - ℹ — minor convention drift

3. **Probe**. Design ≥10 scenarios that exercise each finding.
   Each scenario follows the same prompt template:

       Task: <concrete edit task>

       Self-analyze (3-5 lines):
       - How many <tool> calls did you make?
       - Did any fail (which :reason)?
       - Was <feature> helpful / unnecessary?
       - One concrete improvement suggestion.

   Run with `--full-trace-stream` so the trace lands in
   `target/probe/logs/<probe>.log`.

4. **Analyse**. Parse the log for:
   - `iter: $(grep -c 'response parsed' LOG)`
   - per-tool calls: `grep -cE 'tool-start.*:op :v/<tool>' LOG`
   - failures: `grep -cE ':no-match|:nth-out-of-range|:anchor-not-found|:stale|:file-not-found' LOG`
   - the model's verbatim `Self-analyze` section in `◆ answer`.

5. **Fix**. One cohesive patch. Update prompt + tool + tests in the
   same commit. AGENTS.md rule: every src ns gets a paired test ns;
   add tests in the same iteration.

6. **Re-probe**. Re-run the original probe. Compare iter count,
   failures, model self-report. If improved → commit. If worse →
   revert + reconsider.

7. **Commit**. Verbose commit message that captures: what the probe
   found, what the model self-reported, what changed, the before/after
   numbers. Future sessions read commits, not logs.

---

## 2. The actual sweeps we ran (chronological)

### v/patch (S1-S10, T1-T9)

Initial 10 scenarios in `target/probe/logs/s{1..10}.log`. Surfaced 9
follow-up items:

| Tag | Probe finding | Resolution |
|---|---|---|
| T1 | structured :failures lost in `;; ! data` trailer | engine lifts ex-data |
| T2 | `:passes` / `:indent-delta` alarm dropped | summary now carries them, line counters removed |
| T3 | over-verify loop (prompt contradiction) | Stale-read rule |
| T4 | per-edit line numbers | resolved by `:diff` `@@ -N,X +M,Y @@` |
| T5 | `:nth :all` discoverability | bulk-rename idiom in prompt |
| T6 | EOF append idiom | Codex envelope example in prompt |
| T7 | line-number anchor | rejected — `:nth` covers it |
| T8 | `v/rg :replace` mode | rejected — conflates concerns |
| T9 | `v/write` for full-file rewrite | added; envelope mode dropped |

All re-probed; logs `s*_v*.log` carry the after-shots.

### v/cat (C1-C10)

`target/probe/logs/cat-c{1..10}.log`. Bumped:

- default 400 → 2000 lines (industry parity)
- byte cap 64KB → 256KB
- per-line trunc 2000 chars (`:long-line-truncations` alarm)
- dropped `(path n)` + `(path offset n)` arities — `:range start end`
  is the single positional slicer
- added `:range start end` inclusive arity

### v/rg (R1-R10)

`target/probe/logs/rg-r{1..10}.log` + `*-v2.log` for re-probes:

- default limit 50 → 250
- `:before` / `:after` / `:context N` lines
- `:files-only?` / `:counts?` output modes
- `:regex?` opt-in (java.util.regex)
- per-line trunc 500 chars

### v/ls (L1-L6)

`target/probe/logs/ls-l{1..6}.log`. Replaced nested `:children`
tree with flat `:entries` vec; `:files-only?` / `:dirs-only?` /
`:limit 500` / depth bump 5 → 10.

### End-to-end (E2E A / B / C)

`target/probe/logs/e2e-{A,B,C}*.log`. Realistic refactor tasks that
exercise the full tool surface in one run.

- **A** — Audit logging refactor. Adds parameters, creates new
  helper, bumps version, deletes legacy file. Touches Python + JS +
  JSON.
- **B** — Security audit + remediation. v/rg with `:context 2`
  finds FIXME-tagged SQL injection; v/patch batches the fix; v/write
  generates `docs/SECURITY_AUDIT.md`.
- **C** — Module reorganization. v/rg `:regex? :files-only?` finds
  classes; v/move per file; v/patch updates imports; v/write
  `__init__.py`; v/delete-if-exists cleans `services_old/`.

Corpora live under `target/probe/e2e-{A,B,C}/` and are recreated
fresh each run via the bash setup blocks in the original transcript
or via `/tmp/run-c.sh`-style helpers.

---

## 3. Cross-model comparison (Claude vs GLM-5.1)

Run the same E2E with two providers:

```bash
# Claude (current default)
bin/vis --full-trace-stream "$(cat /tmp/vis-prompt.txt)" > log-claude.log 2>&1

# GLM-5.1 on Z.ai coding plan
bin/vis --provider zai-coding-plan --model glm-5.1 \
        --full-trace-stream "$(cat /tmp/vis-prompt.txt)" \
        > log-glm.log 2>&1
```

Findings (E2E A/B/C, n=3 each):
- GLM-5.1 ≈ 50% more iter than Claude on heavy tasks
- GLM-5.1 occasional semantic bugs (api.js camelCase mismatch,
  services_old/ path assumption) — Claude got both right
- Tool reliability identical (zero failures both)

GLM-5.1 self-diagnosis prompt template (when the model makes a
mistake, ask it to explain):

    Below is a bug you introduced. For (1)(2)(3):
    (1) What specifically went wrong?
    (2) Why did you do it — task ambiguity, prompt gap, model bias?
    (3) One concrete improvement to the system prompt or tool surface.

That self-analysis produced the Path-target prompt rule (the
`services_old/` fix).

---

## 4. Cavemanize the prompt

Industry common: long, verbose, prose-heavy system prompts (Claude
Code is ~5K tokens). Vis prompts had drifted into the same shape.
Cavemanize = compress prose into symbolic schemas + fragments while
keeping the structure intact.

### Style guide

- Replace `shape: {:turn N :iter M …}` ⇒ `:= {:turn :iter …}`
- Replace `status ∈ #{:todo :doing :done :cancelled}` ⇒ inline
  `:status∈#{:todo :doing :done :cancelled}` (the `∈` is good
  signal for model)
- `map of key to value` ⇒ `{K ↦ {…}}`
- enumerated bullet list of warnings ⇒ one-paragraph list with
  parenthetical examples
- 3-line restatement ⇒ 1-line fragment + tag (`ANSWER`, `DEFS`,
  `SANDBOX`)
- Drop articles where harmless. Keep `:` schema operators and `⇒`
  transitions.

### Before / after (CORE_SYSTEM_PROMPT)

| Metric | Before | After | Δ |
|---|---|---|---|
| chars | 11,561 | 5,575 | -52 % |
| lines | 194 | 123 | -37 % |
| tokens | ~2,890 | ~1,394 | **-1,496 tok per turn** |

No semantic changes — every engine fn signature, CTX subtree shape,
warning category, trailer pipeline behavior, done discipline, sandbox
guard still present. Just compressed.

### What we caveman-ed

| Section | Treatment |
|---|---|
| VOCAB | already symbolic, condensed labels |
| CTX subtrees | shape blocks become `:= {...}` lines |
| ENGINE FNS | signature list, drop prose explanation; details lazy via `(introspect-symbol-doc 'sym)` |
| BEHAVIORS | warning enumeration → one paragraph with examples |
| TRAILER | shape blocks symbolic; pipeline becomes 3-line `⇒` chain |
| ANSWER / DEFS / SANDBOX | each becomes 1-2 line label-tagged fragment |

### What we DIDN'T touch

The editing-prompt (`available-editing-prompt`) — T10 queued, similar
treatment. About 700 tokens still on the table.

---

## 5. svar reasoning_content knobs

Empirical preserved-thinking sweep on GLM-5.1 / Z.ai coding plan
(E2E-C, n=3):

| Mode | Mean iter | Mean thinking chars | Correctness |
|---|---|---|---|
| `:all` (default) | 6.67 | 5,624 | 3/3 ✅ |
| `:none` | 7.00 | 6,622 | 3/3 ✅ |
| `last-1` | 14 (single run) | — | broken |
| `last-2` | 10 | — | broken |
| `last-3` | 8 | — | broken |

Conclusions (cross-validated against ZAI docs +
[opencode#6708](https://github.com/anomalyco/opencode/issues/6708)):

1. **Full echo is the contract** — ZAI's preserved-thinking explicitly
   requires unmodified, ordered `reasoning_content`. Partial echo is
   named harmful in the official docs.
2. **`SVAR_DISABLE_REASONING_ECHO=1`** kept as a debug escape hatch
   only. Production violates the contract.
3. **`SVAR_REASONING_ECHO_LAST_N=K` was removed** — empirically and
   officially bad.
4. **Don't prune mid-turn**. Opencode community: "context prune
   operation jumps in the middle of strong logical deduction chain
   will cause serious issue."
5. **Vis already does it right** — mid-turn auto-pin is verbatim,
   `:trailer-drop` / `:trailer-summarize` fire only at `(done …)`
   (see `ctx_engine.clj:867` mid-turn, `:961` at done).

---

## 6. Quick recipes

### Probe a single tool

```bash
# Fresh corpus per probe
rm -rf target/probe/<corpus>
mkdir -p target/probe/<corpus>
# … set up fixture files …

# Prompt template
cat > /tmp/vis-prompt.txt <<'PROMPT'
Task: <single concrete edit>

Self-analyze (max N lines):
- How many v/<tool> calls did you make?
- Did <feature> help?
- One concrete improvement?
PROMPT

# Run
timeout 240 bin/vis --full-trace-stream "$(cat /tmp/vis-prompt.txt)" \
    > target/probe/logs/<name>.log 2>&1

# Quick metrics
log=target/probe/logs/<name>.log
echo "iter: $(grep -c 'response parsed' $log)"
echo "v/cat: $(grep -cE 'tool-start.*:op :v/cat' $log)"
echo "patch failures: $(grep -cE ':no-match|:nth-out-of-range|:anchor-not-found|:stale|:file-not-found' $log)"
grep -A 15 "◆ answer" $log | tail -20
```

### A/B with different model

```bash
# Same prompt, two providers
for prov in "" "--provider zai-coding-plan --model glm-5.1"; do
  # … reset corpus …
  bin/vis $prov --full-trace-stream "$(cat /tmp/vis-prompt.txt)" \
      > target/probe/logs/<name>-${prov//[ -\/]/_}.log 2>&1
done
```

### A/B with svar knob

```bash
# Run A: with echo
unset SVAR_DISABLE_REASONING_ECHO
bin/vis --provider zai-coding-plan --model glm-5.1 \
        --full-trace-stream "$PROMPT" > log-with.log

# Run B: without echo
export SVAR_DISABLE_REASONING_ECHO=1
bin/vis --provider zai-coding-plan --model glm-5.1 \
        --full-trace-stream "$PROMPT" > log-without.log
unset SVAR_DISABLE_REASONING_ECHO
```

### N-run statistical comparison

Single-shot probes are too noisy. For real comparison run **3 trials
per condition** and report mean ± range:

```bash
for cond in A B; do
  for trial in 1 2 3; do
    # set env per condition
    # reset corpus
    bin/vis ... > target/probe/logs/<name>-${cond}-T${trial}.log
  done
done
```

---

## 7. Verification discipline

After any prompt or tool surface change, before commit:

```bash
./verify.sh --quick      # format + lint (~10 s)
./verify.sh              # full gate (~2 min)
```

AGENTS.md: docs-only changes skip verify. Behavioral changes need:
- formal cljfmt pass
- clj-kondo zero new warnings
- full test suite green (`clojure -M:test`)
- smoke (`bin/vis help`)

If verify fails because a prompt-pinning test still expects the old
copy, update the test to assert the new shape — don't revert the
prompt.

---

## 8. What we deliberately did NOT pursue

| Idea | Why not |
|---|---|
| Tool-level metrics dashboard | Probe-driven flow already surfaces bottlenecks faster |
| Multi-agent file locking | Out of scope; pull --rebase + frequent push works |
| `SVAR_REASONING_ECHO_LAST_N` | Empirically + officially harmful |
| Engine: loosen tool+done same-iter rule | User explicit "TEGO NIE ROBIMY" |
| `v/rg :replace` mode | Conflates observation with mutation |
| `v/write` line-number range edits | `v/patch :range` covers it |
| Whole-file rewrite escape hatch | `v/write` solves the same need cleanly |

---

## 9. Where everything lives

| Concern | File |
|---|---|
| Probe logs | `target/probe/logs/*.log` |
| Corpora | `target/probe/{rg-corpus,e2e-A,e2e-B,e2e-C,*}/` |
| Editing-tool source | `extensions/common/vis-foundation-core/src/com/blockether/vis/ext/foundation_core/editing/{core,patch}.clj` |
| Engine prompt | `src/com/blockether/vis/internal/prompt.clj` |
| svar request body / echo | `~/svar/src/clj/com/blockether/svar/internal/llm.clj` |
| svar reasoning style table | `~/svar/src/clj/com/blockether/svar/internal/router.clj` |
| Open follow-ups | `TODO.md` |
| Test pairs | `*/test/.../*_test.clj` (one per src ns) |

---

## 10. Open questions parking

When findings emerge mid-probe that aren't immediately actionable,
park them as bullets in this section so they don't get lost:

- Long-context (>100K) GLM tool-call-leaks-into-thinking bug (opencode
  #6708). Not our bug; surface as known issue if a Vis session ever
  exceeds 100K with GLM.
- `:turn-level thinking` per-turn enable/disable (GLM-4.7 feature).
  Vis doesn't expose this — single global `:reasoning-level :balanced`
  per session. Could be useful for splitting probe loops where some
  iters need thinking and some don't.
- Provider-specific `:ext/prompt` overrides (T12). Architecturally
  clean but Vis does not support per-provider system-prompt tail
  today. Worth a small extension hook if T11/T12 sweep show real
  value.

Park new ones at the bottom; promote to T-tagged items in TODO.md
when actionable.
