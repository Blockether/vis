(ns com.blockether.vis.ext.common-editing.fs-bindings
  "Bindings for babashka.fs under the `fs/` alias.

   Picks a small, useful subset \u2014 the surface a Clojure programmer
   reaches for in path math: cwd, exists?, glob, parent, components,
   file-name, extension, expand-home, list-dir, relativize.

   No I/O conveniences here \u2014 reading file contents lives under the
   `vis/` alias (vis/cat, vis/ls, vis/rg). The fs/ alias is just plumbing."
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.core :as sdk]))

(defn- as-strings [coll]
  (mapv str coll))

(defn- fs-glob [root pat]
  (as-strings (fs/glob (str root) (str pat))))

(defn- fs-list-dir [path]
  (as-strings (fs/list-dir (str path))))

(defn- fs-components [path]
  (as-strings (fs/components (str path))))

(def fs-symbols
  [(sdk/symbol 'cwd #(str (fs/cwd))
     {:doc "Current working directory as a string."
      :arglists '([])
      :examples ["(fs/cwd)"]})
   (sdk/symbol 'exists? #(fs/exists? (str %))
     {:doc "True iff the path exists on disk."
      :arglists '([path])
      :examples ["(fs/exists? \"deps.edn\")"]})
   (sdk/symbol 'parent #(some-> (fs/parent (str %)) str)
     {:doc "Parent directory of `path` as a string, or nil."
      :arglists '([path])
      :examples ["(fs/parent \"src/main.clj\")"]})
   (sdk/symbol 'file-name #(str (fs/file-name (str %)))
     {:doc "Final path segment as a string."
      :arglists '([path])
      :examples ["(fs/file-name \"src/main.clj\")"]})
   (sdk/symbol 'extension #(some-> (fs/extension (str %)) str)
     {:doc "File extension (without dot) or nil."
      :arglists '([path])
      :examples ["(fs/extension \"main.clj\")"]})
   (sdk/symbol 'components fs-components
     {:doc "Vector of path components as strings."
      :arglists '([path])
      :examples ["(fs/components \"src/main.clj\")"]})
   (sdk/symbol 'expand-home #(str (fs/expand-home (str %)))
     {:doc "Expand a leading `~` to the user's home directory."
      :arglists '([path])
      :examples ["(fs/expand-home \"~/.vis\")"]})
   (sdk/symbol 'glob fs-glob
     {:doc "Glob a tree. Returns a vector of matching paths as strings."
      :arglists '([root pattern])
      :examples ["(fs/glob \".\" \"**/*.clj\")"]})
   (sdk/symbol 'list-dir fs-list-dir
     {:doc "Non-recursive directory listing as a vector of paths."
      :arglists '([dir])
      :examples ["(fs/list-dir \"src\")"]})
   (sdk/symbol 'relativize #(str (fs/relativize (str %1) (str %2)))
     {:doc "Relativize `to` against `from`."
      :arglists '([from to])
      :examples ["(fs/relativize \"/a\" \"/a/b/c\")"]})])

(def fs-prompt
  "FS PATH MATH. babashka.fs primitives under the `fs/` alias \u2014
ergonomic path operations that don't read or mutate files. Use the
`vis/` alias for cat / ls / rg / edit / write / zedit.")
