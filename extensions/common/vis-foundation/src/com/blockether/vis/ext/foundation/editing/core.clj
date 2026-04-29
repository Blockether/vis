(ns com.blockether.vis.ext.foundation.editing.core
  "Editing + filesystem aggregator. Two extensions registered from
   one classpath manifest:

     vis  (cat, ls, rg, edit, write)
     fs   (cwd, exists?, glob, parent, components, file-name,
           extension, expand-home, list-dir, relativize)

   Clojure-specific structured editing (z/zedit + the rewrite-clj
   zipper bound under z/) lives in vis-language-clojure under
   extensions/languages/clojure/."
  (:require
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.foundation.editing.fs :as fs-bindings]
   [com.blockether.vis.ext.foundation.editing.tools :as editing]))

(def editing-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.editing.tools
     :ext/doc       "Editing tools: cat, ls, rg, edit, write. Every return value is a Clojure map; no prose embedded in tool output."
     :ext/version   "0.7.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
     :ext/group     "filesystem"
     :ext/prompt    editing/editing-prompt
     :ext/symbols   editing/editing-symbols}))

(def fs-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.editing.fs
     :ext/doc       "babashka.fs path primitives bound under the fs/ alias."
     :ext/version   "0.7.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.fs :alias 'fs}
     :ext/group     "filesystem"
     :ext/prompt    fs-bindings/fs-prompt
     :ext/symbols   fs-bindings/fs-symbols}))

(sdk/register-extension! editing-extension)
(sdk/register-extension! fs-extension)
