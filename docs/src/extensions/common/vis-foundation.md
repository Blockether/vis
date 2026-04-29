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
(vis/extension-readme 'environment)
```

Or load it from disk at
`extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn`
under `[environment :docs "README.md" :content]`.

## Sibling extensions in this jar

The `vis-foundation` jar ships two SCI aliases. The `vis/` alias
covers introspection, file I/O, and environment awareness (above).
The second alias is for answer construction:

- [Markdown surface (`md/`)](markdown.md) — programmatic markdown
  builders for `(answer …)` bodies. Headings, lists, tables, code
  blocks, and — importantly — link helpers (`md/link`, `md/image`,
  `md/file-link`, `md/anchor`) so answers can cite source files,
  URLs, and in-document anchors uniformly across channels.
