# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

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

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.0...HEAD
