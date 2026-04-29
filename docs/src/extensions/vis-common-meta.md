# Meta extension

Source of truth: the inlined `"README.md"` body in
[`extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn`](https://github.com/Blockether/vis/blob/main/extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn).
The book intentionally does NOT duplicate it (see `AGENTS.md` ▸ "Every
extension declares itself in vis.edn with an inline README").

Read the canonical doc one of three ways:

- From inside the agent: `(foundation/extension-readme 'foundation)` — returns
  the full Markdown body. Pair with `(foundation/extensions)` /
  `(foundation/extension-docs 'foundation)` to scan the abstract index first.
- From the host shell: open the EDN file linked above; the README
  body is the value at `[meta :docs "README.md"]`.
- From a Clojure REPL with `com.blockether/vis` on the classpath:

  ```clojure
  (require '[com.blockether.vis.core :as sdk])
  (println (sdk/extension-doc 'foundation "README.md"))
  ```

## At a glance

`vis-foundation` is the agent's read-only introspection extension. Bound
under the `foundation` alias. The functions return plain Clojure maps and
vectors so the agent manipulates state structurally instead of
running raw `sqlite3`. Symbols include:

- `(foundation/turn)`, `(foundation/conversation)`, `(foundation/conversations)` —
  current turn / single conversation / cross-channel listing.
- `(foundation/var-history sym)`, `(foundation/find-attempts pattern)` —
  programmatic history search.
- `(foundation/failures)`, `(foundation/diagnose)` — provider/code failure
  triage with classifications and next-action hints. First stop for
  stalled-turn debugging.
- `(foundation/extensions)`, `(foundation/extension-docs ref)`,
  `(foundation/extension-doc ref name)`, `(foundation/extension-readme ref)` —
  catalog of every loaded extension and the docs it ships. Each
  doc descriptor's `:description` and `:content` are inlined as
  plain EDN strings in the extension's `vis.edn` (no YAML
  frontmatter parsing).
