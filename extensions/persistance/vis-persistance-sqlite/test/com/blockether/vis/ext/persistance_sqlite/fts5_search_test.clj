(ns com.blockether.vis.ext.persistance-sqlite.fts5-search-test
  "Real-DB coverage for the search-query DSL → FTS5 renderer behind
   `recall({\"match\": …})`.

   The query is BACKEND-NEUTRAL DATA (`{:all […]}`, `{:near {…}}`), not a
   string of engine operators — so the agent/caller never authors FTS5 syntax
   and a term full of code punctuation can never break the query. These run the
   ACTUAL SQLite FTS5 `search` index (in-memory store; the iteration `code`
   trigger indexes) through the same `persistance/db-search` facade `ctx-loop`
   calls, pinning the DSL semantics against a live engine.

   Tokenizer is `porter unicode61` (see V1__schema.sql) — fixtures use stable
   whole tokens (alpha/bravo/…)."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe expect it]]))

(h/use-mem-store!)

(defn- seed!
  "Store one session + turn and index each `code` string as its own iteration."
  [codes]
  (let [s   (h/store)
        cid (h/store-session! s {:channel :tui})
        qid (vis/db-store-session-turn! s {:parent-session-id cid
                                           :user-request "x" :status :running})]
    (doseq [c codes]
      (h/store-iteration! s {:session-turn-id qid :code c :result 1 :duration-ms 1}))
    s))

(defn- search [s q]
  (persistance/db-search s q {:owner-table "session_turn_iteration"
                              :field "code" :limit 50}))

(defn- n-hits [s q] (count (search s q)))

(defdescribe dsl-leaf-test
  (describe "bare string — implicit-AND convenience"
    (it "splits on whitespace and requires all words"
      (let [s (seed! ["alpha bravo charlie" "alpha delta"])]
        (expect (= 1 (n-hits s "alpha bravo")))
        (expect (= 2 (n-hits s "alpha")))
        (expect (= 0 (n-hits s "alpha zulu"))))))

  (describe "{:phrase …} — adjacent run only"
    (it "matches in-order adjacency, unlike implicit-AND"
      (let [s (seed! ["alpha bravo" "bravo alpha"])]
        (expect (= 1 (n-hits s {:phrase "alpha bravo"})))
        (expect (= 2 (n-hits s {:all ["alpha" "bravo"]}))))))

  (describe "{:prefix …}"
    (it "matches any token sharing the prefix"
      (let [s (seed! ["alphabet soup" "alpine route" "bravo"])]
        (expect (= 1 (n-hits s {:prefix "alpha"})))   ; alphabet
        (expect (= 2 (n-hits s {:prefix "alp"})))))))  ; alphabet + alpine

(defdescribe dsl-combinator-test
  (describe "{:all …} — AND"
    (it "requires every child"
      (let [s (seed! ["alpha bravo charlie" "alpha delta"])]
        (expect (= 1 (n-hits s {:all ["alpha" "bravo"]})))
        (expect (= 2 (n-hits s {:all ["alpha"]}))))))

  (describe "{:any …} — OR"
    (it "unions the children"
      (let [s (seed! ["alpha bravo" "charlie delta"])]
        (expect (= 2 (n-hits s {:any ["bravo" "delta"]})))
        (expect (= 1 (n-hits s {:any ["bravo" "zulu"]}))))))

  (describe "{:not …} as an :all child — negation"
    (it "excludes rows containing the negated term"
      (let [s (seed! ["alpha bravo" "alpha charlie"])]
        (expect (= 1 (n-hits s {:all ["alpha" {:not "bravo"}]})))
        (let [hit (first (search s {:all ["alpha" {:not "bravo"}]}))]
          (expect (clojure.string/includes? (:snippet hit) "charlie"))))))

  (describe "{:near {:terms … :within k}} — proximity"
    (it "matches within k tokens, not beyond"
      (let [s (seed! ["alpha one two three bravo"])]   ; 3 tokens between
        (expect (= 1 (n-hits s {:near {:terms ["alpha" "bravo"] :within 5}})))
        (expect (= 0 (n-hits s {:near {:terms ["alpha" "bravo"] :within 2}})))
        (expect (= 0 (n-hits s {:phrase "alpha bravo"}))))))   ; not adjacent

  (describe "nested composition"
    (it "{:all [{:any …} {:not …}]} composes recursively"
      (let [s (seed! ["alpha bravo" "alpha gamma" "alpha bravo delta"])]
        ;; (bravo OR gamma) NOT delta
        (expect (= 2 (n-hits s {:all [{:any ["bravo" "gamma"]} {:not "delta"}]})))))))

(defdescribe dsl-safety-test
  (describe "the DSL CANNOT be broken by punctuation — every term is escaped"
    (it "code-ish terms with quotes/parens never throw, just match literally"
      (let [s (seed! ["call fact_set( now" "plain"])]
        ;; would be a syntax error as a raw FTS5 string; as a DSL term it's quoted
        (expect (vector? (search s {:all ["fact_set("]})))
        (expect (= 1 (n-hits s {:phrase "fact_set("})))
        (expect (vector? (search s {:all ["alpha" "\"weird"]})))
        (expect (vector? (search s "messy (text) with \"quotes"))))))

  (describe "malformed DSL throws a clear error (a DSL logic bug, not text)"
    (it "a lone :not has no positive to subtract from"
      (let [s (seed! ["alpha"])]
        (expect (try (search s {:not "alpha"}) false (catch Exception _ true)))
        (expect (try (search s {:all [{:not "alpha"}]}) false (catch Exception _ true)))))
    (it "an unrecognized node is rejected"
      (let [s (seed! ["alpha"])]
        (expect (try (search s {:bogus "x"}) false (catch Exception _ true)))))))

(defdescribe error-indexing-test
  ;; Per-form FAILURES must be findable by recall search. They lived only
  ;; inside the Nippy `:forms` blob (invisible to FTS), so a model hunting
  ;; "why did the edit fail?" got [] back — the introspection hole from
  ;; session f5aba6d4. db-store-iteration! now indexes each errored form's
  ;; head line + op-error :message/:hint under field "errors".
  (describe "errored forms index under field \"errors\""
    (it "finds the failure by its error message text"
      (let [s   (h/store)
            cid (h/store-session! s {:channel :tui})
            qid (vis/db-store-session-turn! s {:parent-session-id cid
                                               :user-request "x" :status :running})]
        (h/store-iteration! s
          {:session-turn-id qid
           :code "clj_edit({\"path\": \"a.clj\"})"
           :forms [{:scope "t1/i1/f1"
                    :src "clj_edit({\"path\": \"a.clj\"})"
                    :error {:message "source file does not parse: Unexpected EOF"
                            :hint "repair the file first (clj_paren_repair), then retry"}}]
           :duration-ms 1})
        ;; reachable under the explicit errors field…
        (expect (= 1 (count (persistance/db-search s "unexpected EOF"
                              {:owner-table "session_turn_iteration"
                               :field "errors" :limit 10}))))
        ;; …and through the recall verb's multi-field filter
        (expect (= 1 (count (persistance/db-search s {:all ["clj_paren_repair"]}
                              {:owner-table "session_turn_iteration"
                               :field ["code" "errors"] :limit 10}))))))

    (it "a clean iteration indexes NO errors row"
      (let [s   (h/store)
            cid (h/store-session! s {:channel :tui})
            qid (vis/db-store-session-turn! s {:parent-session-id cid
                                               :user-request "x" :status :running})]
        (h/store-iteration! s {:session-turn-id qid :code "ok_code_token" :result 1 :duration-ms 1})
        (expect (= 0 (count (persistance/db-search s "ok_code_token"
                              {:owner-table "session_turn_iteration"
                               :field "errors" :limit 10}))))
        (expect (= 1 (count (persistance/db-search s "ok_code_token"
                              {:owner-table "session_turn_iteration"
                               :field ["code" "errors"] :limit 10})))))))

  (describe "db-search :field accepts a collection"
    (it "field IN (…) unions hits across fields without double-counting"
      (let [s   (h/store)
            cid (h/store-session! s {:channel :tui})
            qid (vis/db-store-session-turn! s {:parent-session-id cid
                                               :user-request "x" :status :running})]
        (h/store-iteration! s
          {:session-turn-id qid
           :code "shared_token in code"
           :forms [{:scope "t1/i1/f1" :src "boom()"
                    :error {:message "shared_token in error"}}]
           :duration-ms 1})
        (expect (= 2 (count (persistance/db-search s "shared_token"
                              {:owner-table "session_turn_iteration"
                               :field ["code" "errors"] :limit 10}))))))))
