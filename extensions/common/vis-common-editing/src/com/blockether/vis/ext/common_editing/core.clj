(ns com.blockether.vis.ext.common-editing.core
  "Aggregator for the `vis-common-editing` extension.

   This namespace is the single classpath-discovery entry point: the
   unified `META-INF/vis-extension/vis.edn` resource lists THIS namespace, the
   extension loader `require`s it, and the `register-global!` call
   below wires every module (currently just `editing`) into the
   global registry as one extension named
   `com.blockether.vis.ext.common-editing.core`.

   To add a new module (e.g. `shell`, `text`):
     1. Create `com.blockether.vis.ext.common-editing.<module>`
        exposing a `*-symbols` vector and an optional `*-prompt`
        string. Do NOT call `register-global!` from the module.
     2. Require it here, conj its symbols onto `all-symbols` and
        merge its prompt into `combined-prompt`.

   Depends on `com.blockether/vis-runtime` and uses the extension-author
   facade `com.blockether.vis-sdk.core`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis-sdk.core :as ext]
   [com.blockether.vis.ext.common-editing.editing :as editing]))

(def all-symbols
  "Concatenation of every module's symbol vector. Order matters only
   for human-readability in `<var_index>`; the SCI sandbox indexes by
   name."
  (vec editing/editing-symbols))

(def combined-prompt
  "Module prompt fragments joined with a blank line so the LLM sees one
   coherent block per extension instead of N tiny disjointed snippets."
  (str/join "\n\n" [editing/editing-prompt]))

;; Parse-error rescue lives at the SYMBOL level (see
;; `rg-symbol` in editing.clj) so the iteration loop only
;; fires it when the broken source actually mentions the symbol it
;; repairs. No extension-wide hook is needed here.

(def extension
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.common-editing.core
     :ext/doc       "Common Vis operations: cat, ls, rg, patch."
     :ext/version   "0.4.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
     :ext/group     "filesystem"
     :ext/prompt    combined-prompt
     :ext/symbols   all-symbols}))

(ext/register-extension! extension)
