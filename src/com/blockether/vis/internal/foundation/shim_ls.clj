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
   would honour none of those rules. Rows cross as JSON so the shim renders native
   Python dicts, not foreign proxies. Failures use the standard host-tool boundary
   and the same declarative error hook as the editing tools.

   `:fs/access` is asked by `list-directories` itself, so an extension that hides
   a tree hides it from the listing exactly as it hides it from every read."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.editing.core :as editing]))

(defn- pattern-label
  [pattern]
  (str "Pattern: "
       (activity-event/bounded-text (str/replace pattern #"[\p{Cntrl}\u2028\u2029]" " ") 160)))

(defn- listing-section
  [{:strs [path entries pattern]} mixed?]
  (let [all
        (rest (tree-seq #(seq (get % "children")) #(get % "children") {"children" entries}))

        rows
        (take 12 all)

        dirs
        (count (filter #(= "dir" (get % "type")) entries))

        files
        (- (count entries) dirs)]

    {"headline" (activity-event/bounded-text (str/replace path #"[\p{Cntrl}\u2028\u2029]" " ") 400)
     "summary" (str (cond (some? pattern) (str (pattern-label pattern) " · ")
                          mixed? "Unfiltered · ")
                    dirs
                    (if (some? pattern)
                      (str " shown " (if (= dirs 1) "directory" "directories") " · ")
                      " directories · ")
                    files
                    (if (some? pattern) (str " matching " (if (= files 1) "file" "files")) " files")
                    (when (> (count all) 12) (str " · showing 12 of " (count all) " entries")))
     "content" (if (seq rows)
                 [{"type" "table"
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
                                rows)
                   "paths" (mapv (fn [entry]
                                   (let [entry-path (str (get entry "path" ""))]
                                     (if (or (= "dir" (get entry "type"))
                                             (not (str/starts-with? entry-path "/")))
                                       ""
                                       entry-path)))
                                 rows)}]
                 [])}))

(def ^:private max-listing-sections 16)

(def ^:private max-listing-section-bytes 30000)

(defn- listing-sections
  "Share a bounded Activity budget so later directory counts remain visible."
  [listings mixed?]
  (let [requested
        (vec (take max-listing-sections listings))

        allowance
        (quot (long max-listing-section-bytes) (long (max 1 (count requested))))]

    (mapv (fn [listing]
            (let [section (listing-section listing mixed?)]
              (if (<= (activity-event/utf8-bytes (json/write-json-str section)) allowance)
                section
                (-> section
                    (assoc "content" [])
                    (update "summary"
                            #(activity-event/bounded-text (str % " · entries omitted from Activity")
                                                          512))))))
          requested)))

(defn- listing-presentation
  "One directory supplies the step header; batches keep distinct, spaced sections."
  [listings]
  (let [patterns
        (mapv #(get % "pattern") listings)

        mixed?
        (> (count (distinct patterns)) 1)

        pattern
        (first patterns)

        sections
        (listing-sections listings mixed?)

        n
        (count listings)

        total
        (reduce + 0 (map #(count (get % "entries")) listings))]

    (if (= n 1)
      (let [section (first sections)]
        (assoc section
          "headline" "Listed directory"
          "summary" (activity-event/bounded-text
                      (if (some? pattern)
                        (str (get section "summary") " · " (get section "headline"))
                        (str (get section "headline") " · " (get section "summary")))
                      512)))
      {"headline" (str "Listed " n " directories")
       "summary"
       (str (cond mixed? "Filters differ by directory · "
                  (some? pattern) (str (pattern-label pattern) " · "))
            total
            (if (some some? patterns) (str " shown " (if (= total 1) "entry" "entries")) " entries")
            (when (> n (count sections))
              (str " · showing " (count sections) " of " n " directories")))
       "content" []
       "sections" sections})))

(def ^:private listing-symbol
  {:ext.symbol/symbol 'ls
   :ext.symbol/tag :observation
   :ext.symbol/presenter :observation
   :ext.symbol/activity {:headline "List directories" :show-start false}
   :ext.symbol/inject-env? true
   :ext.symbol/on-error-fn (editing/tool-failure-on-error :ls :dir)
   :ext.symbol/fn (fn [env args]
                    (let [rows
                          (editing/list-directories env args)

                          shown
                          (mapv (fn [row spec]
                                  (assoc row
                                    "pattern"
                                    (get (if (map? spec) spec {}) "pattern" (get args "pattern"))))
                                rows
                                (or (get args "paths") [(get args "path")]))]

                      (extension/publish-activity! (listing-presentation shown))
                      (extension/success {:result rows})))})

(defn- ls-bridge-bindings
  "Observe the listing through the standard host-tool boundary; return rows as JSON."
  []
  {"__vis_list_directories__" (fn list-directories [args-json]
                                (json/write-json-str (extension/invoke-symbol-wrapper
                                                       {:ext/name "foundation-shim-ls"}
                                                       listing-symbol
                                                       [(json/read-json (str args-json))]
                                                       extension/*current-environment*)))})

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
         "it), a file is `name size` (`812`, `7.2k`, `2.1M`), children indented two spaces "
         "per level. `ls([dir, ...])` renders one "
         "blank-line separated section per directory, and a batch entry may be a per-path spec "
         "(`{\"path\": dir, \"depth\": 2}`). Optional `pattern=None` leaves the listing unchanged; "
         "a string filters basenames by case-sensitive glob (`*`, `?`, `[abc]`, `{a,b}`), not regex, "
         "at each requested depth, retaining ancestors of matches. Per-path specs override it; "
         "None disables filtering. Example: `ls(dir, pattern='*snapshot*')`. Dotfiles need `is_hidden=True`; gitignored "
         "entries are never listed. Use confirmed directories: one missing, protected or "
         "non-directory path fails the batch with a host tool error. Read files with `cat`. "
         "A path is a `str` or a `pathlib.Path`.")
       :shim/bindings ls-bridge-bindings
       :shim/source "vis-shims/ls.py"}]}))

(defn register! [] (vis/register-extension! vis-extension))
