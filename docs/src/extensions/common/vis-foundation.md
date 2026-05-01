# Environment extension

Package: `com.blockether/vis-foundation`. Source:
`extensions/common/vis-foundation/`.

This extension owns the `<environment>` block that ships in the
system prompt and exposes the same data as a structured snapshot
inside `:code` under the `environment/` alias. The runtime no
longer hardcodes any environment text — drop the jar, drop the
block.

The full reference (sandbox surface, prompt format, boundaries,
guarantees) is inlined as the extension's canonical README. Read
it from inside the agent with:

```clojure
(v/extension-readme 'environment)
```

Or load it from disk at
`extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn`
under `[environment :docs "README.md" :content]`.

## Answer construction in the same alias

The `vis-foundation` jar now ships **one** SCI alias: `v/`.
Introspection, file I/O, environment awareness, and answer
construction all live under that single prefix.

- [Markdown builders under `v/`](markdown.md) — programmatic
  markdown builders for `(answer …)` bodies. Headings, lists,
  tables, code blocks, and — importantly — link helpers
  (`v/link`, `v/image`, `v/file-link`, `v/anchor`) so answers can
  cite source files, URLs, and in-document anchors uniformly across
  channels.
