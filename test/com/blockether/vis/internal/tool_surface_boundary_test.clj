(ns com.blockether.vis.internal.tool-surface-boundary-test
  "The STRINGS-ONLY Clojure->Python boundary, checked across the WHOLE registered
   surface instead of one extension at a time.

   Issue #115 was never an `environment` bug: the envelope builder handed Python
   the engine's own keyword-keyed snapshot, and ANY tool that does the same dies
   the same way — `STRINGS-ONLY boundary violation: non-string-key :host at the
   TOP-LEVEL map key` — before the extension's own code is ever blamed. So this
   namespace enumerates the LIVE registry rather than a hand-written list: every
   registered extension, every symbol it contributes, every `:ext/ctx-fn`. A tool
   added tomorrow is covered the day it registers.

   Payloads travel the REAL funnel Python calls, `extension/invoke-symbol-wrapper`
   (before-fn -> fn -> after-fn, envelope unwrapped to `:result`), and are then
   judged by `env-python/boundary-view` — the no-context mirror of `->py`, which
   throws on a keyword/symbol key or value at any depth exactly like GraalPy's
   boundary does. A tool that legitimately FAILS here (no MCP server, no session
   DB) is not a boundary bug and is reported as such; only a boundary violation
   fails the test."
  (:require [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- extensions
  "Every extension after the ordered distribution manifest is initialized."
  []
  (manifest/initialize!)
  (extension/registered-extensions))

(defn- probe-env [] {:workspace/root (System/getProperty "user.dir")})

(defn- crossing
  "Run `thunk` and report how its value crosses the strings-only boundary:
   `[:ok view]`, `[:boundary-violation msg]` (the #115 failure) or
   `[:tool-failed msg]` (the tool itself refused — not a boundary bug)."
  [thunk]
  (try [:ok (env-python/boundary-view (thunk))]
       (catch clojure.lang.ExceptionInfo e
         (if (:vis/boundary-violation (ex-data e))
           [:boundary-violation (ex-message e)]
           [:tool-failed (ex-message e)]))
       (catch Throwable t [:tool-failed (ex-message t)])))

(defn- readable-symbols
  "`[ext sym-entry]` for every registered symbol that can be CALLED with no
   arguments and only observes (`:observation`), so probing one is a pure read.
   Mutations are deliberately excluded — this test never writes."
  []
  (into []
        (comp (mapcat (fn [ext]
                        (map (fn [e]
                               [ext e])
                             (extension/ext-symbols ext))))
              (filter (fn [[_ e]]
                        (and (= :observation (:ext.symbol/tag e))
                             (some #(zero? (count %)) (:ext.symbol/arglists e))))))
        (extensions)))

(defn- ctx-fns
  "`[id ctx-fn]` for every extension contributing to the sandbox `session` map —
   the other value `->py` converts, and one keyword key there kills the whole
   context bind rather than a single tool."
  []
  (into []
        (keep (fn [ext]
                (when-let [f (:ext/ctx-fn ext)]
                  [(:ext/id ext) f])))
        (extensions)))

;; Regression, issue #115: `repositories()`, `languages()`, `monorepo()` and
;; `main_agent_instructions()` all handed Python their RAW keyword-keyed engine
;; snapshot, so every one of them raised "STRINGS-ONLY boundary violation:
;; non-string-key :host at the TOP-LEVEL map key" instead of returning the
;; string-keyed dict their docstrings promised.
(defdescribe
  tool-surface-boundary-test
  (it "reaches a real registry, so a green run is never vacuous"
      (let [syms (readable-symbols)]
        (expect (<= 4 (count syms)) "the registry must expose at least the environment symbols")
        (expect (contains? (set (map (comp str :ext.symbol/symbol second) syms)) "repositories")
                "issue #115's own symbols must be among the probed ones")
        (expect (seq (ctx-fns)) "at least one extension contributes ctx")))
  (it "hands Python string-keyed payloads for every no-arg observation tool"
      (doseq [[ext e] (readable-symbols)]
        (let [sym (:ext.symbol/symbol e)
              [status v] (crossing #(extension/invoke-symbol-wrapper ext e [] (probe-env)))]

          (expect (not= :boundary-violation status) (str sym " -> " v))
          (when (and (= :ok status) (map? v))
            (expect (every? string? (keys v)) (str sym " top-level keys"))))))
  (it "hands Python a string-keyed `session` context from every extension"
      (doseq [[id f] (ctx-fns)]
        (let [[status v] (crossing #(f (probe-env)))]
          (expect (not= :boundary-violation status) (str id " ctx -> " v))
          (when (and (= :ok status) (map? v))
            (expect (every? string? (keys v)) (str id " ctx top-level keys")))))))
