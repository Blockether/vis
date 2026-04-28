# Meta extension

Source of truth: the inlined `"README.md"` body in
[`extensions/common/vis-common-meta/resources/META-INF/vis-extension/vis.edn`](https://github.com/Blockether/vis/blob/main/extensions/common/vis-common-meta/resources/META-INF/vis-extension/vis.edn).
The book intentionally does NOT duplicate it (see `AGENTS.md` ▸ "Every
extension declares itself in vis.edn with an inline README").

Read the canonical doc one of three ways:

- From inside the agent: `(meta/extension-readme 'meta)` — returns
  the full Markdown body. Pair with `(meta/extensions)` /
  `(meta/extension-docs 'meta)` to scan the abstract index first.
- From the host shell: open the EDN file linked above; the README
  body is the value at `[meta :docs "README.md"]`.
- From a Clojure REPL with vis-loop on the classpath:

  ```clojure
  (require '[com.blockether.vis-extension.extension :as ext])
  (println (ext/extension-doc 'meta "README.md"))
  ```

## At a glance

`vis-common-meta` is the agent's read-only introspection extension. Bound
under the `meta` alias. The functions return plain Clojure maps and
vectors so the agent manipulates state structurally instead of
running raw `sqlite3`. Symbols include:

- `(meta/turn)`, `(meta/conversation)`, `(meta/conversations)` —
  current turn / single conversation / cross-channel listing.
- `(meta/var-history sym)`, `(meta/find-attempts pattern)` —
  programmatic history search.
- `(meta/failures)`, `(meta/diagnose)` — provider/code failure
  triage with classifications and next-action hints. First stop for
  stalled-turn debugging.
- `(meta/extensions)`, `(meta/extension-docs ref)`,
  `(meta/extension-doc ref name)`, `(meta/extension-readme ref)` —
  catalog of every loaded extension and the docs it ships, abstracts
  parsed from each doc's YAML frontmatter.
