(ns com.blockether.vis.ext.language-clojure.lint
  "clj-kondo linting for the Vis language surface.

   Runs clj-kondo's programmatic API (`clj-kondo.core/run!`) — never shells out —
   over a code string (fed on stdin as `-`), explicit path(s), or the workspace's
   default source paths, and returns a uniform result map:
   `{:op :clj-lint :error N :warning N :info N :files N :findings [...]}`
   where each finding is `{:file :row :col :level :type :message}`."
  (:require
   [clj-kondo.core :as clj-kondo]))

(defn- finding->map [f]
  {:file    (:filename f)
   :row     (:row f)
   :col     (:col f)
   :level   (some-> (:level f) name)
   :type    (some-> (:type f) name)
   :message (:message f)})

(defn run-lint
  "Run clj-kondo over `lint-arg` (a vector of paths or `[\"-\"]` for stdin) and
   shape the result into the uniform lint map. `opts` is merged into the run
   config (e.g. `{:config {...}}`)."
  [lint-arg opts]
  (let [r (clj-kondo/run! (merge {:lint lint-arg} opts))
        s (:summary r)]
    {:op       :clj-lint
     :error    (:error s)
     :warning  (:warning s)
     :info     (:info s)
     :files    (:files s)
     :findings (mapv finding->map (:findings r))}))

(defn lint-code
  "Lint a raw code string via stdin."
  [code]
  (with-in-str code (run-lint ["-"] nil)))

(defn lint-paths
  "Lint one or more filesystem paths."
  [paths]
  (run-lint (vec paths) nil))
