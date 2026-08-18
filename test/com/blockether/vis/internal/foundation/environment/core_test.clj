(ns com.blockether.vis.internal.foundation.environment.core-test
  (:require [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.foundation.environment.core :as env-core]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe environment-core-test
             (it "exports the expected environment symbol surface"
                 (let [syms (set (map :ext.symbol/symbol env-core/environment-symbols))]
                   (expect (not (contains? syms 'snapshot)))
                   (expect (not (contains? syms 'git)))
                   (expect (contains? syms 'repositories))
                   (expect (contains? syms 'languages))
                   (expect (contains? syms 'monorepo))
                   ;; `refresh!` is HOST-ONLY: dropping the env snapshot and
                   ;; rescanning the tree is the user's `/reload`, never
                   ;; something the model can call from `python_execution`.
                   (expect (not (contains? syms 'refresh!)))
                   (expect (not (contains? syms 'render)))
                   (expect (contains? syms 'main-agent-instructions))
                   (expect (not (contains? syms 'load-skill!)))
                   (expect (not (contains? syms 'load-skill)))
                   (expect (not (contains? syms 'reload-skills!)))
                   (expect (not (contains? syms 'scan-warnings)))
                   (expect (not (contains? syms 'reload-instructions!)))
                   (expect (not (contains? syms 'reload-extensions!)))))
             ;; `/reload` is the ONLY refresh path left: the hook registration is
             ;; what replaces the removed sandbox symbol, so a user who reshapes
             ;; the tree still gets a fresh scan without the model reloading.
             (it "registers its refresh as a `/reload` hook"
                 (expect (= ::env-core/environment-refresh @#'env-core/_environment-reload-hook)))
             (it "provides foundation environment info through ctx"
                 (let [ctx (env-core/environment-ctx {})]
                   (expect (contains? ctx :project))
                   (expect (contains? (:project ctx) :host))
                   (expect (contains? (:project ctx) :root)))))

(defn- symbol-view
  "Call the environment symbol named `sym` and return what Python would hold."
  [sym]
  (let [{:ext.symbol/keys [fn]} (first (filter #(= sym (:ext.symbol/symbol %))
                                               env-core/environment-symbols))]
    (env-python/boundary-view (:result (fn)))))

;; Regression, issue #115: every environment symbol handed Python its RAW
;; keyword-keyed snapshot, so `await refresh()` died with "STRINGS-ONLY
;; boundary violation: non-string-key :host at the TOP-LEVEL map key" — and so
;; did repositories(), languages(), monorepo() and main_agent_instructions().
(defdescribe environment-symbols-boundary-test
             (it "hands Python string-keyed payloads for every environment symbol"
                 (doseq [sym-map env-core/environment-symbols]
                   (let [sym (:ext.symbol/symbol sym-map)
                         envelope ((:ext.symbol/fn sym-map))
                         ;; `boundary-view` is the no-context mirror of the real
                         ;; Clojure->Python boundary: it THROWS on a keyword key or
                         ;; value at any depth, exactly like `->py` does in GraalPy.
                         view (env-python/boundary-view (:result envelope))]

                     (expect (:success? envelope) (str sym " envelope must succeed"))
                     (expect (map? view) (str sym " must return a dict"))
                     (expect (every? string? (keys view)) (str sym " top-level keys")))))
             (it "spells its keys the way the docstrings promise"
                 (let [languages
                       (symbol-view 'languages)

                       repositories
                       (symbol-view 'repositories)]

                   (expect (contains? languages "total_files"))
                   (expect (contains? languages "is_truncated"))
                   (expect (contains? repositories "count")))))
