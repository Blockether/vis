(ns com.blockether.vis.internal.env-journal-test
  "Phase 7 prep: journal-system-vars + journal-live-vars introspection helpers
   that produce the `[{:name :doc} …]` entries the new journal's
   discovery-surface renderers consume.

   The helpers walk a real SCI sandbox + a per-env LRU map (from the
   resolve-symbol* monkey-patch) and return ranked entries. Tests use
   `env/create-sci-context` to build a real sandbox so the helpers
   exercise the actual `:namespaces 'sandbox` shape."
  (:require
   [com.blockether.vis.internal.env :as env]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- fresh
  "Build a fresh SCI context. Returns the {:sci-ctx :initial-ns-keys ...}
   map env/create-sci-context produces; tests pull what they need."
  []
  (env/create-sci-context nil))

(defn- ns-obj [sci-ctx]
  (sci/find-ns sci-ctx 'sandbox))

(defdescribe journal-system-vars-test
  (it "surfaces UPPERCASE engine-managed vars with their docstrings"
    (let [{:keys [sci-ctx]} (fresh)]
      ;; create-sci-context already injects *1/*2/*3/*e under sandbox;
      ;; we add a USER_REQUEST-style system var via sci-update-binding! +
      ;; the sci-patches docstring contract.
      (sci/eval-string+ sci-ctx
        "(def USER_REQUEST \"current turn user request\" \"please summarize README\")"
        {:ns (ns-obj sci-ctx)})
      (let [entries (env/journal-system-vars sci-ctx)
            by-name (into {} (map (juxt :name identity)) entries)]
        (expect (vector? entries))
        (expect (some? (get by-name "USER_REQUEST")))
        (expect (= "current turn user request"
                  (:doc (get by-name "USER_REQUEST")))))))
  (it "returns an empty vec when no system vars are bound (test contexts)"
    ;; A bare SCI context (not via create-sci-context) has no system
    ;; vars; journal-system-vars returns [] not nil so the renderer can
    ;; safely call (seq …) on it.
    (let [bare (sci/init {:namespaces {'sandbox {}}})]
      (expect (= [] (env/journal-system-vars bare)))))
  (it "returns nil for nil sci-ctx (defensive)"
    (expect (nil? (env/journal-system-vars nil)))))

(defdescribe journal-live-vars-test
  (it "surfaces user-defined vars with their docstrings"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)]
      (sci/eval-string+ sci-ctx
        "(def big-handle \"README handle\" 42)
         (def hits \"TODO grep hits\" [])"
        {:ns (ns-obj sci-ctx)})
      (let [entries (env/journal-live-vars sci-ctx initial-ns-keys {} 1)
            by-name (into {} (map (juxt :name identity)) entries)]
        (expect (vector? entries))
        (expect (some? (get by-name "big-handle")))
        (expect (= "README handle" (:doc (get by-name "big-handle"))))
        (expect (some? (get by-name "hits")))
        (expect (= "TODO grep hits" (:doc (get by-name "hits")))))))
  (it "excludes system-vars from the live-vars surface"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)]
      (sci/eval-string+ sci-ctx
        "(def USER_REQUEST \"current request\" \"x\") (def my-var \"docstring\" 1)"
        {:ns (ns-obj sci-ctx)})
      (let [names (set (map :name (env/journal-live-vars sci-ctx initial-ns-keys {} 1)))]
        (expect (contains? names "my-var"))
        (expect (not (contains? names "USER_REQUEST"))))))
  (it "drops vars whose LRU stamp is older than JOURNAL_LRU_TURN_WINDOW"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)]
      (sci/eval-string+ sci-ctx
        "(def fresh-var \"recent\" 1) (def stale-var \"long ago\" 2)"
        {:ns (ns-obj sci-ctx)})
      ;; current-turn-pos = 15; window = 10
      ;;   fresh-var stamped at turn 14  -> 15-14=1 ≤ 10 → kept
      ;;   stale-var stamped at turn  3  -> 15-3=12 > 10 → hidden
      (let [lru {"fresh-var" 14 "stale-var" 3}
            names (set (map :name (env/journal-live-vars sci-ctx initial-ns-keys lru 15)))]
        (expect (contains? names "fresh-var"))
        (expect (not (contains? names "stale-var"))))))
  (it "vars with no LRU stamp are kept (LRU is informational, missing != stale)"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)]
      (sci/eval-string+ sci-ctx "(def unstamped \"never used\" 1)" {:ns (ns-obj sci-ctx)})
      (let [names (set (map :name (env/journal-live-vars sci-ctx initial-ns-keys {} 1)))]
        (expect (contains? names "unstamped")))))
  (it "honors JOURNAL_LIVE_VARS_CAP — older stamps drop first when above cap"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          n (+ env/JOURNAL_LIVE_VARS_CAP 5)]
      (doseq [i (range n)]
        (sci/eval-string+ sci-ctx
          (str "(def v" i " \"v" i "-doc\" " i ")")
          {:ns (ns-obj sci-ctx)}))
      (let [;; stamp every var freshly so eviction is by NAME-tiebreak,
            ;; not LRU staleness; we just want the cap behaviour
            lru (into {} (map (fn [i] [(str "v" i) 100])) (range n))
            entries (env/journal-live-vars sci-ctx initial-ns-keys lru 100)]
        (expect (= env/JOURNAL_LIVE_VARS_CAP (count entries))))))
  (it "returns nil for nil sci-ctx (defensive)"
    (expect (nil? (env/journal-live-vars nil nil {} 1)))))
