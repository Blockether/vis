(ns com.blockether.vis.internal.foundation.shim-ls
  "Built-in sandbox SHIM: `ls` — the DIRECTORY listing available inside Python.

   Mapping a tree is the cheapest question there is and the one a model asks
   most, so `ls(dir)` runs inside the `python_execution` block already in flight.
   It answers with one compact tree STRING, ready to print. Structured rows would
   cost the reader a second rendering step and the context every quoted brace; the
   tree is the shortest form that still says name, kind, size and shape.

   The walk itself stays on the HOST: `editing/list-directories` is fff's
   ignore-aware listing (`.gitignore`, `.ignore`, cache directories, the `vis.yml`
   overlay), an order of magnitude faster than a guest `os.scandir` recursion that
   would honour none of those rules. The bridge is one callable answering the
   `[ok result kind]` envelope every shim uses, because errors must cross the
   boundary as DATA — CPython does not route a host exception through Python
   `except`. The rows themselves cross as a JSON string rather than as the
   boundary's own `ForeignDict`s: a listing is data the caller SERIALIZES, and
   `json.dumps` is the one dict operation a foreign map refuses. Measured over a
   100 KB listing of this repo, the JSON hop costs 3.4 ms against 11.8 ms for
   `__vis_pyify__` over the proxies — the string is the CHEAPEST real dict.

   `:fs/access` is asked by `list-directories` itself, so an extension that hides
   a tree hides it from the listing exactly as it hides it from every read."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.editing.core :as editing]))

(def ^:private error-kinds
  "`ex-info` `:type` → the KIND the Python shim turns into a real exception, so a
   caller catches `NotADirectoryError` rather than parsing a sentence. Anything
   unmapped stays a `RuntimeError`."
  {:ext.foundation.editing/path-protected "denied"
   :ext.foundation.editing/ls-missing-path "missing"
   :ext.foundation.editing/ls-on-file "file"
   :ext.foundation.editing/invalid-ls-args "args"})

(defn- listing-section
  [{:strs [path entries]}]
  (let [all
        (rest (tree-seq #(seq (get % "children")) #(get % "children") {"children" entries}))

        rows
        (take 12 all)

        dirs
        (count (filter #(= "dir" (get % "type")) entries))]

    {"headline" (activity-event/bounded-text (str/replace path #"[\p{Cntrl}\u2028\u2029]" " ") 400)
     "summary" (str dirs
                    " directories · " (- (count entries) dirs)
                    " files" (when (> (count all) 12)
                               (str " · showing 12 of " (count all) " entries")))
     "content" [{"type" "table"
                 "columns" ["Name" "Kind" "Bytes"]
                 "rows" (mapv (fn [entry]
                                (let [entry-path
                                      (str (get entry "path" (get entry "name")))

                                      name
                                      (if (.startsWith ^String entry-path (str path "/"))
                                        (subs entry-path (inc (count path)))
                                        entry-path)

                                      dir?
                                      (= "dir" (get entry "type"))]

                                  [(str (subs name 0 (min 64 (count name)))
                                        (when (> (count name) 64) "…")
                                        (when dir? "/")) (if dir? "Directory" "File")
                                   (if dir? "—" (str (get entry "size" 0)))]))
                              rows)}]}))

(defn- listing-presentation
  "One directory supplies the step header; batches keep distinct, spaced sections."
  [listings]
  (let [sections
        (mapv listing-section (take 4 listings))

        n
        (count listings)]

    (if (= n 1)
      (update (first sections) "headline" #(str "Listed " %))
      {"headline" (str "Listed " n " directories")
       "summary" (str (reduce + 0 (map #(count (get % "entries")) listings))
                      " entries"
                      (when (> n 4) (str " · showing 4 of " n " directories")))
       "content" []
       "sections" sections})))

(def ^:private listing-symbol
  {:ext.symbol/symbol 'ls
   :ext.symbol/tag :observation
   :ext.symbol/presenter :observation
   :ext.symbol/inject-env? true
   :ext.symbol/fn (fn [env args]
                    (extension/publish-activity! {"headline" "Listing directories"
                                                  "summary" ""
                                                  "content" [{"type" "progress"
                                                              "label" "Listing directories"}]})
                    (let [rows (editing/list-directories env args)]
                      (extension/publish-activity! (listing-presentation rows))
                      (extension/success {:result rows})))})

(defn- ls-bridge-bindings
  "Observe the host listing as one symbol invocation, retaining the shim's error envelope."
  []
  {"__vis_list_directories__" (fn list-directories [args-json]
                                (try [true
                                      (json/write-json-str (extension/invoke-symbol-wrapper
                                                             {:ext/name "foundation-shim-ls"}
                                                             listing-symbol
                                                             [(json/read-json (str args-json))]
                                                             extension/*current-environment*)) nil]
                                     (catch Throwable t
                                       [false (str (or (ex-message t) t))
                                        (get error-kinds (:type (ex-data t)))])))})

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

(defn register! [] (vis/register-extension! vis-extension))
