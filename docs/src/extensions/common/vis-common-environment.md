# Environment extension

Package: `com.blockether/vis-common-environment`. Source:
`extensions/common/vis-common-environment/`.

This extension owns the `<environment>` block that ships in the
system prompt and exposes the same data as a structured snapshot
inside `:code` under the `environment/` alias. The runtime no
longer hardcodes any environment text — drop the jar, drop the
block.

The full reference (sandbox surface, prompt format, boundaries,
guarantees) is inlined as the extension's canonical README. Read
it from inside the agent with:

```clojure
(foundation/extension-readme 'environment)
```

Or load it from disk at
`extensions/common/vis-common-environment/resources/META-INF/vis-extension/vis.edn`
under `[environment :docs "README.md" :content]`.
