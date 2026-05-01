# vis-foundation

Package: `com.blockether/vis-foundation`. Source: `extensions/common/vis-foundation/`.

`vis-foundation` ships one SCI alias: `v/`. It bundles the agent-facing foundation surface: conversation-state introspection, file I/O helpers, Markdown answer builders, and environment awareness.

## Introspection

Conversation-state introspection deliberately has one data surface and one renderer:

```clojure
(v/inspect)       ; canonical data for the current conversation
(v/inspect cid)   ; canonical data for another conversation / unambiguous prefix
(v/report)        ; Markdown rendered from the same data
(v/report cid)    ; Markdown report for another conversation / prefix
```

`v/inspect` returns a map with the conversation summary, current turn, classified failures, diagnosis, fork/retry metadata, and full transcript data. `v/report` renders that same conversation record to Markdown for human handoff/export.

Extension discovery stays separate because it is about the loaded surface, not one conversation:

```clojure
(v/extensions)
(v/extension-docs ext-ref)
(v/extension-doc ext-ref name)
(v/extension-readme ext-ref)
```

The full reference is in the extension's canonical README. Read it from inside the agent with:

```clojure
(v/extension-readme 'v)
```

Or load it from disk at `extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn` under `[v :docs "README.md" :content]`.

## Answer construction in the same alias

The `vis-foundation` jar also exposes Markdown builders under `v/` for `(answer …)` bodies. Headings, lists, tables, code blocks, and link helpers (`v/link`, `v/image`, `v/file-link`, `v/anchor`) let answers cite source files, URLs, and in-document anchors uniformly across channels.

See [Markdown builders under `v/`](markdown.md) for the full builder reference.
