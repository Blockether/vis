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
