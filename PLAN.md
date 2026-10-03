# Simplified English for Vis prompts, guidance and docs

Write every prompt, rule set and guide in Simplified Technical English: same rules, clearer text, fewer tokens.

## Context

Karpathy recommends ASD-STE100 for model output (quoted by Theo, x.com/theo/status/2106088065781612941).
The attached overview sets these limits:

- Write one instruction in each sentence. Use the imperative and the active voice.
- Use a maximum of 20 words in an instruction and 25 words in a description.
- Use a maximum of 6 sentences in a paragraph, with one topic in each paragraph.
- Use a maximum of 3 words in a noun cluster.
- Use one word for one thing. Use simple words: "use", not "utilize"; "before", not "prior to".
- Use vertical lists for complex text. In a warning, give the command first, then the risk.

The core prompt tells the model to write in Simplified English, but the prompt itself does not.
The same is true for the built-in rules, the extension prompts, the tool descriptions, the
AGENTS.md files and parts of the docs. Long text costs tokens on every request.

Base commits: vis 3af0912a0, vis-lang-interface af10c5f (v2.6.1), vis-lang-clojure e41abb4
(v1.9.1), vis-lang-python 49cbd97 (v1.6.1), spel 8a6fef0b401 (vis-spel/v0.1.11).

Owners of the text:

- Core prompt: `src/com/blockether/vis/internal/context/prompt.clj` and the process rule in
  `python/env.clj`.
- Built-in rules: `foundation/introspection.clj`, `council/core.clj`, `foundation/drafts.clj`,
  the agents prompt, `foundation/mcp/core.clj` and `foundation/harness/core.clj`.
- Tool declaration of `python_execution`, and the shim docstrings in `resources/vis-shims/`.
- Project extensions: `.vis/extensions/gh.py` and `.vis/extensions/uplink.py`.
- Sibling extensions: the shared toolchain prompt in vis-lang-interface, vis-lang-clojure,
  vis-lang-python and spel `extensions/vis-spel`.
- Guidance: `AGENTS.md` in vis and in each sibling repository that this task changes.
- Docs: `resources/vis-docs/` (29 pages, about 68,000 words).

Rejected alternatives:

- Add only one more rule that asks for Simplified English. The prompt stays long and hard to read.
- Rewrite all 68,000 words of docs by hand. The docs contract already limits sentences, so a scan
  for unapproved words and long sentences finds the remaining problems faster.
- Use the full STE dictionary. It would rename API names; these stay as technical names.

## 1. Baseline and rule set

Rationale: a size claim needs a measured baseline. One rule set keeps all rewrites the same.

Data: the rendered text of each prompt part at the base commits; Svar token counts with the
tokenizer of the context breakdown.

Acceptance criteria: a baseline table with characters, words and tokens for each part. The vis
AGENTS.md has a short Simplified English rule set for prompts, guidance and docs.

Unknowns: none.

## 2. Core prompt

Rationale: the model reads the core prompt on every request.

Data: `CORE_SYSTEM_PROMPT`, the planning rules, the autonomous CLI rules, the project
instructions header, the sandbox block and the process rule.

Acceptance criteria: every rule stays, with the same meaning. Each sentence follows the rule set.
The token count decreases. `prompt_test` and the affected suites pass.

Unknowns: which tests pin the old wording.

## 3. Built-in rules, tool declaration and shims

Rationale: these parts load with the core prompt or answer `doc()`.

Data: the introspection, Council, drafts, agents, MCP and harness prompts; the `python_execution`
description; `attach.py` and `ls.py` docstrings and their generated apropos resources.

Acceptance criteria: same rules, shorter text; the affected tests and the apropos resource test
pass.

Unknowns: none.

## 4. Project and sibling extensions

Rationale: extension prompts load in every session that enables them.

Data: `gh.py`, `uplink.py`, vis-lang-interface, vis-lang-clojure, vis-lang-python and vis-spel.

Acceptance criteria: each repository passes its tests, format and lint. Each changed extension
has a new version, tag and GitHub Release, and Extension Center lists it. `vis.yml` pins the
new versions.

Unknowns: the publish path for vis-spel.

## 5. AGENTS.md files

Rationale: AGENTS.md is a rule set for agents; it loads on every request in its repository.

Data: vis AGENTS.md and the AGENTS.md of each changed sibling repository.

Acceptance criteria: each rule is a short imperative sentence in a clear group. No rule is lost.

Unknowns: none.

## 6. Docs

Rationale: `doc()` serves the same pages to the model and to people.

Data: `resources/vis-docs/*.md`; a scan for unapproved words, long sentences and passive voice.

Acceptance criteria: the scan finds no unapproved words, and the docs canon test passes.
Anchors, API names and examples do not change.

Unknowns: the number of findings.

## 7. Verification and release

Rationale: shorter rules must keep the same agent behavior.

Data: affected Lazytest and pytest suites, format and lint, the e2e scenarios on Mikrus with a
native image built from the pushed commit.

Acceptance criteria: all affected checks pass. The e2e scenarios pass, or each failure has a
fix that is pushed and tested again. The final reply has a before and after table. There is no
Vis product release.

Unknowns: e2e provider limits on Mikrus.

## Plan state

- Phase 1 is done: the base texts and counts are recorded.
- Phase 2 is done: core prompt, sandbox block, process rule, planning, CLI and project rules.
- Phase 3 is done: tool declaration, introspection, Council, drafts, agents, MCP and harness.
- Phase 4 is done for vis-lang-interface 2.6.2, vis-lang-clojure 1.9.2, vis-lang-python 1.6.2,
  `gh.py` and `uplink.py`. `vis.yml` pins the new versions.
- Phase 4 blocker: vis-spel 0.1.12 waits for green spel CI. example.org removed its `h1`, so 22
  spel tests fail on Linux and macOS. They need local fixture pages instead of the live site.
- Phase 5 is done: vis AGENTS.md has a Simplified English rule set. Four sibling files changed.
- Phase 6 is done: six hard words are fixed, and `test-prose/simpler-words` enforces the rule.
- Phase 7 is done. The full local suite passes (6327 cases), and clj format and lint are clean.
- CI passes the main suite on Linux and macOS. The standalone TUI suite fails one test, "session
  picker coalesces wheel floods and moves selection". It failed before this work too (83dba5bc8).
  CI then skips Beta Native, so the Mikrus e2e runs use the JVM source gateway.
- Mikrus e2e at b39cc06cd: 24 of 39 scenarios pass. A 60-second usage query timeout of the JVM
  gateway fails 5 of them. Mikrus tool setup fails 3. 4 give the correct result with one model
  code error. 3 fail on behavior; `ls-source-root-discovery` then passed 3 of 3.
- A/B on `extension-contract-discovery`, counting runs clean apart from the usage timeout: base
  6 of 7, rewrite 4 of 8 with 3 `dict(record)` errors, final 3 of 5 with none.
  `extension-schema-discovery` fails 3 of 3 at the base and after the rewrite.
- f04ae9b1c restores "Skip discovery.". 6f8f3f921 adds the `dataclasses.asdict(r)` rule and sets
  the core prompt limit to 11 650 characters.
- The `attach` and `ls` shim docs did not change. `apropos_resource_test` applies the same prose
  rules to them, and it passes.
