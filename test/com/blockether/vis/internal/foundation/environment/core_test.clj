(ns com.blockether.vis.internal.foundation.environment.core-test
  (:require [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.foundation.environment.core :as env-core]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe environment-core-test
             (it "exports the expected environment symbol surface"
                 (let [syms (set (map :ext.symbol/symbol env-core/environment-symbols))]
                   (expect (not (contains? syms 'snapshot)))
                   (expect (not (contains? syms 'git)))
                   (expect (not (contains? syms 'repositories)))
                   (expect (not (contains? syms 'languages)))
                   (expect (not (contains? syms 'monorepo)))
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
                   (expect (not (contains? syms 'reload-extensions!))))))

;; Regression, issue #115: environment tools must encode engine data before
;; crossing the strings-only Python boundary.
(defdescribe environment-symbols-boundary-test
             (it "hands Python string-keyed payloads for every environment symbol"
                 (doseq [sym-map env-core/environment-symbols]
                   (let [sym (:ext.symbol/symbol sym-map)
                         envelope ((:ext.symbol/fn sym-map))
                         ;; `boundary-view` is the no-context mirror of the real
                         ;; Clojure->Python boundary: it THROWS on a keyword key or
                         ;; value at any depth, exactly like `->py` does in CPython.
                         view (env-python/boundary-view (:result envelope))]

                     (expect (:success? envelope) (str sym " envelope must succeed"))
                     (expect (map? view) (str sym " must return a dict"))
                     (expect (every? string? (keys view)) (str sym " top-level keys"))))))
