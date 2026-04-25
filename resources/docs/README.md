# Vis Documentation

Built with [mdBook](https://rust-lang.github.io/mdBook/).

## Quick Start

```bash
cd docs
mdbook serve --open    # live-reload dev server at http://localhost:3000
mdbook build           # static build to docs/book/
```

## Structure

```
docs/
├── book.toml          mdBook config
├── theme/
│   └── custom.css     style overrides
└── src/
    ├── SUMMARY.md     table of contents (defines nav)
    ├── README.md      landing page
    ├── architecture/
    │   ├── overview.md
    │   ├── iteration-flow.md
    │   ├── directory-structure.md
    │   ├── state.md
    │   └── database.md
    ├── extensions/
    │   ├── overview.md
    │   ├── spec.md
    │   ├── hooks.md
    │   ├── environment.md
    │   └── nudges.md
    └── reference/
        ├── api.md
        ├── reasoning.md
        └── language.md
```

## Rules

1. **Every change to source code that affects architecture, environment shape,
   extension spec, iteration flow, or public API MUST be accompanied by an
   update to the relevant page under `docs/src/`.**
2. If docs and code diverge, code wins — fix the docs immediately.
3. In-source READMEs are pointers to these docs. Do not duplicate content.
