(ns com.blockether.vis.loop.redundancy-metric-test
  "Phase 2-m measurement: `canonical-expression-hash` collapses
   whitespace / equivalent-form differences so two textually-different
   but semantically-identical code blocks dedup correctly;
   `count-duplicates` builds the per-query seen-hashes set and
   reports how many of THIS iter's expressions are duplicates of
   prior SUCCESSFUL ones.

   These two helpers feed `:expression-redundancy-fraction` and
   `:dedup-saves` in iteration metadata. The metric is the data we
   need before deciding if Phase 2's auto-dedup short-circuit is
   worth the engineering."
  (:require
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [lazytest.core :refer [defdescribe it expect]]))

;; -----------------------------------------------------------------------------
;; canonical-expression-hash
;; -----------------------------------------------------------------------------

(defdescribe canonical-expression-hash-test
  (it "collapses whitespace differences"
    (expect (= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(grep   \"X\")"))))

  (it "collapses leading/trailing whitespace"
    (expect (= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "  (grep \"X\")\n"))))

  (it "produces different hashes for different forms"
    (expect (not= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(grep \"Y\")"))))

  (it "produces different hashes for forms with different head sym"
    (expect (not= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(read-file \"X\")"))))

  (it "falls back to raw-string hash on parse failure (never throws)"
    ;; A truncated form like \"(def\" is unparseable; the helper must
    ;; still return a stable hash.
    (let [a (iterate/canonical-expression-hash "(def")
          b (iterate/canonical-expression-hash "(def")
          c (iterate/canonical-expression-hash "(defn")]
      (expect (= a b))
      (expect (not= a c))))

  (it "treats nil / empty string as a stable hash, not a throw"
    (expect (string? (iterate/canonical-expression-hash "")))
    (expect (string? (iterate/canonical-expression-hash nil)))))

;; -----------------------------------------------------------------------------
;; count-duplicates
;; -----------------------------------------------------------------------------

(defdescribe count-duplicates-test
  (it "first iter has zero duplicates and seeds the seen-set"
    (let [seen (atom #{})
          [duplicates total] (iterate/count-duplicates seen
                               [{:code "(+ 1 2)"}
                                {:code "(grep \"X\")"}])]
      (expect (= 0 duplicates))
      (expect (= 2 total))
      (expect (= 2 (count @seen)))))

  (it "subsequent iter reports duplicates and grows the seen-set with new hashes only"
    (let [seen (atom #{})]
      (iterate/count-duplicates seen
        [{:code "(grep \"X\")"}
         {:code "(read-file \"a\")"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(grep \"X\")"}            ;; duplicate
                                  {:code "(grep \"Y\")"}             ;; new
                                  {:code "(read-file \"a\")"}])]      ;; duplicate
        (expect (= 2 duplicates))
        (expect (= 3 total))
        ;; Seen-set now has 3 distinct hashes: grep X, grep Y, read-file a.
        (expect (= 3 (count @seen))))))

  (it "errors are NOT recorded — retrying after failure is legitimate"
    ;; Iter 1 errored out; iter 2 retries the same call; iter 2's call
    ;; must NOT count as a duplicate.
    (let [seen (atom #{})]
      (iterate/count-duplicates seen
        [{:code "(grep \"X\")" :error "regex broken"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(grep \"X\")"}])]
        (expect (= 0 duplicates))
        (expect (= 1 total)))))

  (it "whitespace-equivalent retries dedup correctly"
    (let [seen (atom #{})]
      (iterate/count-duplicates seen [{:code "(+ 1 2)"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(+   1   2)"}])]
        (expect (= 1 duplicates))
        (expect (= 1 total)))))

  (it "handles an empty expressions vec gracefully"
    (let [seen (atom #{})
          [duplicates total] (iterate/count-duplicates seen [])]
      (expect (= 0 duplicates))
      (expect (= 0 total)))))

;; -----------------------------------------------------------------------------
;; dedup-cache short-circuit — the actual Phase 2 mechanism.
;; -----------------------------------------------------------------------------

(defdescribe dedup-cache-test
  (it "lookup returns nil when the cache is empty"
    (let [cache (atom {})]
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "lookup returns nil when expression is nil"
    (let [cache (atom {})]
      (expect (nil? (iterate/dedup-cache-lookup cache nil)))))

  (it "record! stores successful results, lookup hits afterwards"
    (let [cache (atom {})
          successful {:result :ok :stdout "" :stderr "" :execution-time-ms 7}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" successful "i3.1")
      (let [hit (iterate/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (some? hit))
        (expect (= :ok (:result hit)))
        (expect (= "i3.1" (:cached-from hit)))
        (expect (true? (:cached? hit)))
        (expect (= 0 (:execution-time-ms hit))))))

  (it "record! is a no-op for error results"
    (let [cache (atom {})
          err-result {:result nil :error "boom" :stdout "" :stderr ""}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" err-result "i3.1")
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! is a no-op for timeouts"
    (let [cache (atom {})
          timeout-result {:result nil :timeout? true :stdout "" :stderr ""}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" timeout-result "i3.1")
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! preserves the FIRST writer when racing"
    (let [cache (atom {})
          first-result  {:result :first :execution-time-ms 1}
          second-result {:result :second :execution-time-ms 2}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" first-result "i1.1")
      (iterate/dedup-cache-record! cache "(grep \"X\")" second-result "i5.2")
      (let [hit (iterate/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (= :first (:result hit)))
        (expect (= "i1.1" (:cached-from hit))))))

  (it "whitespace-equivalent forms hit the same cache entry"
    (let [cache (atom {})]
      (iterate/dedup-cache-record! cache "(grep \"X\")" {:result :ok} "i1.1")
      (expect (some? (iterate/dedup-cache-lookup cache "(grep    \"X\")"))))))
