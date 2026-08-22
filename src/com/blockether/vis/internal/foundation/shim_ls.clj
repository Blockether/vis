(ns com.blockether.vis.internal.foundation.shim-ls
  "Built-in sandbox SHIM: `ls` — the DIRECTORY listing, as a Python call instead
   of a native tool.

   Mapping a tree is the cheapest question there is and the one a model asks
   most, so it must not cost a wire round trip: `ls(dir)` runs inside the
   `python_execution` block the model is already in, and what it answers is the
   ANSWER: one compact tree STRING, ready to print, not rows to re-render. As a
   native tool the same listing spent a whole tool result — plus a schema and a
   description in every request — to hand back a structure whose only consumer
   was a formatter. Structured rows cost the reader a second rendering step and
   cost the context every quoted brace; the tree is the shortest form that still
   says name, kind, size and shape.

   The walk itself stays on the HOST: `editing/list-directories` is fff's
   ignore-aware listing (`.gitignore`, `.ignore`, cache directories, the `vis.yml`
   overlay), an order of magnitude faster than a guest `os.scandir` recursion that
   would honour none of those rules. The bridge is one callable answering the
   `[ok result kind]` envelope every shim uses, because errors must cross the
   boundary as DATA — GraalPy does not route a host exception through Python
   `except`. The rows themselves cross as a JSON string rather than as the
   boundary's own `ForeignDict`s: a listing is data the caller SERIALIZES, and
   `json.dumps` is the one dict operation a foreign map refuses. Measured over a
   100 KB listing of this repo, the JSON hop costs 3.4 ms against 11.8 ms for
   `__vis_pyify__` over the proxies — the string is the CHEAPEST real dict.

   `:fs/access` is asked by `list-directories` itself, so an extension that hides
   a tree hides it from the listing exactly as it hides it from every read."
  (:require [charred.api :as json]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.editing.core :as editing]))

(def ^:private error-kinds
  "`ex-info` `:type` → the KIND the Python shim turns into a real exception, so a
   caller catches `NotADirectoryError` rather than parsing a sentence. Anything
   unmapped stays a `RuntimeError`."
  {:ext.foundation.editing/path-protected "denied"
   :ext.foundation.editing/ls-missing-path "missing"
   :ext.foundation.editing/ls-on-file "file"
   :ext.foundation.editing/invalid-ls-args "args"})

(defn- ls-bridge-bindings
  "Host callable the `ls` shim delegates to. `__vis_list_directories__` takes the
   JSON request (`{\"paths\": [...], \"depth\": n, \"is_hidden\": b}`) and answers
   `[true json-rows nil]`, or `[false message kind]` when the listing was refused,
   the path is missing, the path is a file, or the request is malformed."
  []
  {"__vis_list_directories__"
   (fn list-directories [args-json]
     (try [true
           (json/write-json-str (editing/list-directories extension/*current-environment*
                                                          (json/read-json (str args-json)))) nil]
          (catch Throwable t
            [false (str (or (ex-message t) t)) (get error-kinds (:type (ex-data t)))])))})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-ls"
     :ext/description
     (str "Sandbox `ls(paths, depth=1, is_hidden=False)` — the directory listing as a Python "
          "call: fff's ignore-aware walk, rendered as one compact printable tree string.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "ls"
       :shim/globals ["ls"]
       :shim/docs
       (str
         "`ls(paths, depth=1, is_hidden=False)` maps a tree from the host's ignore-aware walk as "
         "a compact STRING: a `path  Nd Nf` header, then one line per entry, directories first "
         "then alphabetical — a directory is `name/` (with its child count once `depth` expanded "
         "it), a file is `name  size` (`812`, `7.2k`, `2.1M`). `ls([dir, ...])` renders one "
         "blank-line separated section per directory, and a batch entry may be a per-path spec "
         "(`{\"path\": dir, \"depth\": 2}`). Dotfiles need `is_hidden=True`; gitignored "
         "entries are never listed; a file raises `NotADirectoryError`. A path is a `str` or a "
         "`pathlib.Path`.")
       :shim/bindings ls-bridge-bindings
       :shim/source "vis-shims/ls.py"}]}))

(vis/register-extension! vis-extension)
