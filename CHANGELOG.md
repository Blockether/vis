# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Added
- Extension system with global registry, topo-sort, hot-reload
- `:ext/nudge-fn` for per-iteration system nudges from extensions
- `:ext/requires` for extension dependency declaration
- `:ext/version`, `:ext/author`, `:ext/license` metadata
- `register-global!`, `load-extension!`, `reload-extension!`
- `extensions/editing` package (read, write, grep, list, patch)
- mdBook documentation at https://blockether.github.io/vis/
- Iteration metadata stores active extensions (namespace + version)
- Apache-2.0 license

### Changed
- Default reasoning level: `:balanced` (was `:quick`)
- `create-env` → `create-environment`
- `dispose-env!` → `dispose-environment!`
- `vis!` → `query!`
- `register-env-def!` removed (use extensions)
- Nudges moved from `loop/nudges.clj` to `loop/runtime/prompt.clj`
- `conversation/shared.clj` folded into `conversation/core.clj`

### Removed
- `var-diff` (dead code)
- `restore-var` references (never existed as callable tool)
- Scattered .md files (consolidated into `resources/docs/`)

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.0...HEAD
