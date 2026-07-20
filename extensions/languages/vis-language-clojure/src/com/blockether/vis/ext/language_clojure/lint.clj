(ns com.blockether.vis.ext.language-clojure.lint
  "clj-kondo linting for the Vis language surface.

   Runs clj-kondo's programmatic API (`clj-kondo.core/run!`) — never shells out —
   over a code string (fed on stdin as `-`), explicit path(s), or the workspace's
   default source paths, and returns a uniform result map (STRING keys — crosses
   the strings-only boundary as a tool `:result`):
   `{\"op\" \"clj-lint\" \"error\" N \"warning\" N \"info\" N \"files\" N \"findings\" [...]}`
   where each finding is `{\"file\" \"row\" \"col\" \"level\" \"type\" \"message\" \"provider\"}`
   (clj-kondo findings carry `\"provider\" \"clj-kondo\"`)."
  (:require [clj-kondo.core :as clj-kondo]))

(defn- finding->map
  [f]
  {"file" (:filename f)
   "row" (:row f)
   "col" (:col f)
   "level" (some-> (:level f)
                   name)
   "type" (some-> (:type f)
                  name)
   "message" (:message f)
   "provider" "clj-kondo"})

(defn run-lint
  "Run clj-kondo over `lint-arg` (a vector of paths or `[\"-\"]` for stdin) and
   shape the result into the uniform lint map. `opts` is merged into the run
   config (e.g. `{:config {...}}`)."
  [lint-arg opts]
  (let [r
        (clj-kondo/run! (merge {:lint lint-arg} opts))

        s
        (:summary r)]

    {"op" "clj-lint"
     "error" (:error s)
     "warning" (:warning s)
     "info" (:info s)
     "files" (:files s)
     "findings" (mapv finding->map (:findings r))}))

(defn lint-code "Lint a raw code string via stdin." [code] (with-in-str code (run-lint ["-"] nil)))

(defn lint-paths
  "Lint one or more filesystem paths. With `config-dir` (a `.clj-kondo`
   directory) it is threaded to clj-kondo as `:config-dir`, so a NESTED project's
   config is honored instead of clj-kondo's process-CWD default resolution."
  ([paths] (run-lint (vec paths) nil))
  ([paths config-dir] (run-lint (vec paths) (when config-dir {:config-dir (str config-dir)}))))

(def empty-result
  "The zeroed lint result (no files, no findings) — identity for `merge-results`."
  {"op" "clj-lint" "error" 0 "warning" 0 "info" 0 "files" 0 "findings" []})

(defn merge-results
  "Combine per-config-dir `run-lint` result maps into one uniform map: summed
   counts, concatenated findings. Lets a grouped (monorepo) lint — one run per
   nearest `.clj-kondo` dir — report back as a single result."
  [results]
  (reduce (fn [a b]
            {"op" "clj-lint"
             "error" (+ (long (or (get a "error") 0)) (long (or (get b "error") 0)))
             "warning" (+ (long (or (get a "warning") 0)) (long (or (get b "warning") 0)))
             "info" (+ (long (or (get a "info") 0)) (long (or (get b "info") 0)))
             "files" (+ (long (or (get a "files") 0)) (long (or (get b "files") 0)))
             "findings" (into (vec (get a "findings")) (get b "findings"))})
          empty-result
          results))

(defn- fs-parent
  "The file's parent directory, or `\".\"` when it has none (e.g. `<stdin>` or a
   bare basename)."
  [file]
  (or (.getParent (java.io.File. ^String file)) "."))

(defn- fs-name
  "The file's basename — the directory prefix stripped off."
  [file]
  (.getName (java.io.File. ^String file)))

(defn group-by-dir
  "Regroup a flat `findings` vector into a directory-nested map that writes each
   file's directory ONCE:
   `{<dir> {<basename> {\"error\" [...] \"warning\" [...] \"info\" [...]}}}`.
   `<dir>` is the file's parent (`\".\"` when it has none, e.g. `<stdin>`) and the
   inner key is just the basename, so the long directory prefix isn't repeated
   per file — saving characters over a flat by-file map. Each finding keeps its
   full uniform shape (including its `\"provider\"`), so a single file's group can
   mix providers (clj-kondo + general). Levels with no findings are absent."
  [findings]
  (reduce-kv (fn [m dir fs]
               (assoc m
                 dir (reduce-kv (fn [g file gs]
                                  (assoc g
                                    (fs-name file)
                                    (reduce
                                      (fn [lvls f]
                                        (update lvls (or (get f "level") "info") (fnil conj []) f))
                                      {}
                                      gs)))
                                {}
                                (group-by #(get % "file") fs))))
             {}
             (group-by #(fs-parent (get % "file")) (vec findings))))
