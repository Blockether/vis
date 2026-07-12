# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

## [v0.1.3] - 2026-07-12

### Changed
- feat(sessions): owner + session-group folders across sqlite/gateway/tui
- feat(companion): live SSE tool-card streaming + native iOS TestFlight scaffolding
- fix(gateway): decode workspace kebab keys at the client boundary so added filesystem roots show
- feat(gateway): client-managed daemon self-reap + settings/models API
- refactor(companion): drop the clojure extension, rewrite the RN app
- style(channel-web): use --primary-fg for text on filled primary buttons
- fix(editing): treat non-positive expected_mtime as no staleness guard
- feat(language): full TypeScript/JavaScript/JSX/TSX support
- test(channel-tui): repair stale state-test setups against current code
- style(tui,web): bolden dialog titles and warm modal chrome
- fix(channel-tui): drop the attaching turn from the queued mirror
- chore(companion): trim web dependencies
- fix(gateway): route provider diagnostics through daemon
- feat(tui): colour the footer git/draft chips like sibling buttons
- feat(tui): async magit network verbs + C-x g chord on the footer git button
- bench: tolerate EDN sets and tags in preflight config parser
- test(python): close matplotlib contexts
- fix(gateway): extend native startup timeout
- feat(tui): magit dialog WIP, hint-bar fitting + F4 log fetch under a timeout
- fix(repl): make nREPL start truly synchronous + health-aware resources
- fix(tui): bound clipboard helpers with a hard deadline; table wrap via shared lanterna word-wrap
- fix(tui): stop the mid-stream scroll bounce on macOS trackpads
- fix(tui): wrap markdown table cells inside their columns
- fix(tui): sync session titles live across processes during streaming
- fix(workspace): fresh drafts can never delete HEAD files
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge
- fix(gateway): synchronize queued turns across channels
- feat(gateway): canonical wire transcripts + turn traces across channels
- fix(editing): make rg scan phase and parallel sub-loops cancellable
- fix(editing): stop runaway rg CPU on cancelled gather
- fix(gateway): probe entry timeout
- chore(format): reformat foundation editing and language-surface
- fix(gateway): release listen socket before resource reap and exit daemon on stop
- feat(language-typescript-bun): refuse monorepo-root REPL with app-dir hint
- feat(gateway): kill session background resources on TUI close
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.10
- fix(language-surface): advertise dir/timeout_ms on repl_eval, dir/filter on run_tests
- feat(gateway): route interactive clients through daemon
- feat(language-typescript-bun): managed Bun REPL + bun test language pack
- release: update version files for v0.1.2, bump to next dev version

### Package changes

#### com.blockether/vis
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- fix(gateway): decode workspace kebab keys at the client boundary so added filesystem roots show (404f4c91)
- feat(gateway): client-managed daemon self-reap + settings/models API (58cbac17)
- refactor(companion): drop the clojure extension, rewrite the RN app (b483c962)
- fix(editing): treat non-positive expected_mtime as no staleness guard (a97de2d7)
- feat(language): full TypeScript/JavaScript/JSX/TSX support (93272651)
- fix(gateway): route provider diagnostics through daemon (10021653)
- bench: tolerate EDN sets and tags in preflight config parser (4ebe1e57)
- test(python): close matplotlib contexts (7203812e)
- fix(gateway): extend native startup timeout (aacd1e6c)
- fix(repl): make nREPL start truly synchronous + health-aware resources (bb1ce93e)
- fix(workspace): fresh drafts can never delete HEAD files (d9e743a6)
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge (a881e23e)
- fix(gateway): synchronize queued turns across channels (19d1721a)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- fix(editing): make rg scan phase and parallel sub-loops cancellable (16bed7e8)
- fix(editing): stop runaway rg CPU on cancelled gather (5163a878)
- fix(gateway): probe entry timeout (98934a52)
- chore(format): reformat foundation editing and language-surface (12f933ae)
- fix(gateway): release listen socket before resource reap and exit daemon on stop (80106799)
- feat(gateway): kill session background resources on TUI close (614ecf21)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.10 (4758ddc8)
- fix(language-surface): advertise dir/timeout_ms on repl_eval, dir/filter on run_tests (ba850596)
- feat(gateway): route interactive clients through daemon (c23d8035)
- feat(language-typescript-bun): managed Bun REPL + bun test language pack (41b8d217)
- release: update version files for v0.1.2, bump to next dev version (5503af84)

#### com.blockether/vis-channel-tui
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- test(channel-tui): repair stale state-test setups against current code (1b5c2a3f)
- style(tui,web): bolden dialog titles and warm modal chrome (63ea851b)
- fix(channel-tui): drop the attaching turn from the queued mirror (7b02d0db)
- fix(gateway): route provider diagnostics through daemon (10021653)
- feat(tui): colour the footer git/draft chips like sibling buttons (f50e23dc)
- feat(tui): async magit network verbs + C-x g chord on the footer git button (1c3aa843)
- feat(tui): magit dialog WIP, hint-bar fitting + F4 log fetch under a timeout (893fe04f)
- fix(tui): bound clipboard helpers with a hard deadline; table wrap via shared lanterna word-wrap (bdd63e32)
- fix(tui): stop the mid-stream scroll bounce on macOS trackpads (5c004898)
- fix(tui): wrap markdown table cells inside their columns (4fa8daf6)
- fix(tui): sync session titles live across processes during streaming (6ff45bfd)
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge (a881e23e)
- fix(gateway): synchronize queued turns across channels (19d1721a)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-channel-web
- style(channel-web): use --primary-fg for text on filled primary buttons (0d4262fc)
- style(tui,web): bolden dialog titles and warm modal chrome (63ea851b)
- fix(gateway): route provider diagnostics through daemon (10021653)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-foundation-harness
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-language-clojure
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- fix(repl): make nREPL start truly synchronous + health-aware resources (bb1ce93e)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-language-typescript-bun
- feat(language): full TypeScript/JavaScript/JSX/TSX support (93272651)
- feat(language-typescript-bun): refuse monorepo-root REPL with app-dir hint (4722c8e9)
- feat(language-typescript-bun): managed Bun REPL + bun test language pack (41b8d217)

#### com.blockether/vis-persistance-sqlite
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)

#### com.blockether/vis-provider-standard
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-provider-zai
- feat(gateway): route interactive clients through daemon (c23d8035)



## [v0.1.2] - 2026-07-10

### Changed
- fix(release): auto-publish extension packages
- fix(editing): treat blank paths entry as "search everything"
- release: update version files for v0.1.1, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(release): auto-publish extension packages (1408366a)
- fix(editing): treat blank paths entry as "search everything" (484fa5d9)
- release: update version files for v0.1.1, bump to next dev version (9d0764d8)

#### com.blockether/vis-provider-github-copilot
- fix(release): auto-publish extension packages (1408366a)



## [v0.1.1] - 2026-07-10

### Changed
- fix(release): publish all vis monorepo packages
- feat(providers): surface svar 0.7.56 models
- fix(editing): coerce stringified array in rg include/query/paths
- feat(provider-github-copilot): allow claude-sonnet-5 in policy models
- fix(prompt): render every prior answer in full in resume block
- feat: improve vis tool rendering and resources
- Fix markdown fence and comment rendering
- docs(agents): note commit message style
- fix(editing): accept rg include shorthand
- fix(transcript): render nested markdown fences
- fix(editing): delete directory trees
- test: fix full suite regressions
- Update senior SWE benchmark tooling
- refactor(tui): reuse active turn cleanup helper
- Fix TUI workspace root sync
- fix(tui): reconcile stale in-flight state
- fix(tui): clear stale cancelling state
- fix(loop): close GraalPy context on environment disposal
- fix(self-docs): string-key vis_docs payloads
- fix(clojure-test-runner): empty selectors run everything, not error
- feat(attachments): session-level introspection lister (P1)
- feat(resources): live-tail + paging in background-log viewers
- feat(attachments): storage-offload rail — registry + pure decision + resolver
- feat(resources): view background logs in TUI + web
- fix(channel-web): space + chip styling for result summaries
- feat(attachments): session_fold collapses vision replay too
- fix(channel-web): harden renderProse against UI-spoofing HTML injection
- fix(web): strip vis-image fence in DB-restored history; drop comment profanity
- fix(tui): collapse same-file edit band to full path shown once
- feat(attachments): V4 unifies both rails into one session_attachment table
- refactor(attachments): self-describing handle ids kill the read-back fallback
- feat(attachments): unify read-back across tool + user attachments
- test(loop): update synth oracle for symbol_rename as positional native tool
- feat(attachments): introspection read-back API + misc workspace changes
- feat(editing): promote symbol_rename back to a native tool
- test(attachments): lock down gather->virtual-thread sink conveyance
- clj-ext: format only on :write, not patch/struct_patch
- feat(attachments): $VIS_OUTBOX filesystem tap + rename :images -> :attachments
- feat(attachments): vis_attach — generic producer rail for any artifact
- feat(attachments): V3 brings session_turn_attachment to V2 payload parity
- feat(attachments): wire iteration-attachment rail to both ends
- refactor(mpl): sink produced images at source, drop stdout-fence parsing
- feat(loop): replay generated figures to vision models on the wire
- feat(loop): capture matplotlib figure bytes into iteration attachments
- feat(persist): V2 session_iteration_attachment table + store/read
- tui(navigator): drop empty Modified column, rename Directory -> Dir


### Added
- GitHub Copilot **Enterprise** provider (`:github-copilot-enterprise`). The
  provider extension already shipped the enterprise base-url, provider id,
  label, and account type, but only registered `:individual` + `:business`,
  so Copilot Enterprise users could not select Claude Opus 4.8 / Sonnet 4.6 /
  Haiku 4.5 at all. Enterprise now registers alongside the other tiers and
  inherits the same curated catalog: dotted models.dev ids
  (`claude-opus-4.8`, `claude-sonnet-4.6`, `claude-haiku-4.5`) over the native
  Anthropic `/v1/messages` wire (never `/chat/completions`).
- Extension system with global registry, topo-sort, hot-reload
- `:ext/nudge-fn` for per-iteration system nudges from extensions
- `:ext/requires` for extension dependency declaration
- `:ext/version`, `:ext/author`, `:ext/license` metadata
- `register-global!`, `load-extension!`, `reload-extension!`
- `extensions/common/vis-foundation` package (read, list, grep, patch)
- mdBook documentation at https://blockether.github.io/vis/
- Iteration metadata stores active extensions (namespace + version)
- Apache-2.0 license

### Changed
- Default reasoning level: `:balanced` (was `:quick`)
- `create-env` -> `create-environment`
- `dispose-env!` -> `dispose-environment!`
- `vis!` -> `query!`
- `register-env-def!` removed (use extensions)
- Nudges moved from `loop/nudges.clj` to `loop/runtime/prompt.clj`
- `session/shared.clj` folded into `session/core.clj`

### Removed
- `var-diff` (dead code)
- `restore-var` references (never existed as callable tool)
- Scattered .md files (consolidated into `resources/docs/`)
- Built-in repetition `[system_nudge]` ("You repeated the same expression ...").
  `<journal>` + the dedup cache (`:cached? true`) already give the model
  enough signal to change strategy; the nudge was noise. Drops
  `repetition-warning`, `REPETITION_THRESHOLD`, the `call-counts-atom`
  plumbing, and the `:call-counts-atom` arg to `prompt/build-iteration-context`.

### Fixed
- GitHub Copilot Claude requests returning `404 page not found`. The token
  exchange's authoritative `endpoints.api` (and the account fallback hosts)
  are bare roots with no `/v1`, so `provider-token-base-url` handed svar a
  versionless base and Claude hit `{host}/messages` instead of
  `{host}/v1/messages`. The token's LLM base is now suffixed with `/v1` at
  exchange time (idempotent `ensure-api-version`) and reused from cache, while
  the model-policy call still targets the root host. Affects all Copilot tiers
  (individual/business/enterprise), since every account's token endpoint
  resolves to the same versionless host.
- `github-copilot-provider-id?` omitted `:github-copilot-enterprise`, so
  enterprise models were filtered out of the visible catalog mapping.

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.3...HEAD
[v0.1.1]: https://github.com/Blockether/vis/releases/tag/v0.1.1
[v0.1.2]: https://github.com/Blockether/vis/releases/tag/v0.1.2
[v0.1.3]: https://github.com/Blockether/vis/releases/tag/v0.1.3
