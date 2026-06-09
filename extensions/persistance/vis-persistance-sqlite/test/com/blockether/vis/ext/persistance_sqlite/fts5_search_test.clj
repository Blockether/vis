(ns com.blockether.vis.ext.persistance-sqlite.fts5-search-test
  "Real-DB FTS5 coverage for the `recall({\"match\": …})` content-search path.

   These exercise the ACTUAL SQLite FTS5 `search` index (in-memory store, the
   iteration `code` trigger does the indexing) through the same
   `persistance/db-search` facade `ctx-loop` calls — so the query-language
   behavior the agent relies on (operators, prefix, phrase, NEAR) and the
   literal-vs-fts distinction are pinned against a live engine, not a mock.

   Tokenizer is `porter unicode61` (see V1__schema.sql) — stemming applies, so
   fixtures use stable whole tokens (alpha/bravo/…)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   ;; load the backend so `persistance/db-search` can dispatch (test-helpers
   ;; also pulls the registrar, kept explicit here for ns-in-isolation runs).
   [com.blockether.vis.ext.persistance-sqlite.registrar]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe expect it]]))

(h/use-mem-store!)

(defn- seed!
  "Store one session + turn and index each `code` string as its own iteration.
   Returns the store."
  [codes]
  (let [s   (h/store)
        cid (h/store-session! s {:channel :tui})
        qid (vis/db-store-session-turn! s {:parent-session-id cid
                                           :user-request "x" :status :running})]
    (doseq [c codes]
      (h/store-iteration! s {:session-turn-id qid :code c :result 1 :duration-ms 1}))
    s))

(defn- search
  "Run db-search over the iteration `code` field; default raw FTS5."
  ([s q] (search s q :fts))
  ([s q mode]
   (persistance/db-search s q {:owner-table "session_turn_iteration"
                               :field "code" :query-mode mode :limit 50})))

(defn- n-hits [s q] (count (search s q)))

(defdescribe fts5-operators-test
  (describe "implicit AND (space-separated bare terms)"
    (it "matches only rows containing ALL terms"
      (let [s (seed! ["alpha bravo charlie" "alpha delta"])]
        (expect (= 1 (n-hits s "alpha bravo")))   ; only the first row has both
        (expect (= 2 (n-hits s "alpha")))         ; both rows
        (expect (= 0 (n-hits s "alpha zulu")))))) ; zulu present nowhere

  (describe "OR"
    (it "unions rows matching either side"
      (let [s (seed! ["alpha bravo" "charlie delta"])]
        (expect (= 2 (n-hits s "bravo OR delta")))
        (expect (= 1 (n-hits s "bravo OR zulu"))))))

  (describe "NOT"
    (it "excludes rows containing the negated term"
      (let [s (seed! ["alpha bravo" "alpha charlie"])]
        (expect (= 1 (n-hits s "alpha NOT bravo")))
        (let [hit (first (search s "alpha NOT bravo"))]
          (expect (str/includes? (:snippet hit) "charlie"))))))

  (describe "prefix wildcard"
    (it "`alpha*` matches any token starting with alpha"
      (let [s (seed! ["alphabet soup" "alpine route" "bravo only"])]
        (expect (= 1 (n-hits s "alpha*")))   ; alphabet (alpine doesn't start 'alpha')
        (expect (= 2 (n-hits s "alp*"))))))  ; alphabet + alpine

  (describe "exact phrase (quoted)"
    (it "matches the in-order phrase only"
      (let [s (seed! ["alpha bravo" "bravo alpha"])]
        (expect (= 1 (n-hits s "\"alpha bravo\"")))
        (expect (= 2 (n-hits s "alpha bravo"))))))  ; bare = implicit-AND, order-free

  (describe "NEAR(group, N) — SQLite's proximity operator"
    (it "matches within N intervening tokens, not beyond"
      (let [s (seed! ["alpha one two three bravo"])]   ; 3 tokens between
        (expect (= 1 (n-hits s "NEAR(alpha bravo, 5)")))
        (expect (= 0 (n-hits s "NEAR(alpha bravo, 2)")))
        ;; the bare phrase "alpha bravo" needs adjacency → no match here
        (expect (= 0 (n-hits s "\"alpha bravo\"")))))))

(defdescribe fts5-vs-literal-test
  (describe "raw FTS5 vs literal-text mode on the SAME query"
    (it "operators are live in :fts but inert (matched verbatim) in :literal-text"
      (let [s (seed! ["alpha only" "bravo only"])]
        ;; :fts → OR unions both rows
        (expect (= 2 (count (search s "alpha OR bravo" :fts))))
        ;; :literal-text → the whole thing is ONE phrase "alpha OR bravo",
        ;; which appears in neither row
        (expect (= 0 (count (search s "alpha OR bravo" :literal-text))))))

    (it "punctuation-heavy code text is safe as a literal phrase"
      (let [s (seed! ["located in /vis/internal" "elsewhere"])]
        ;; literal mode quotes + escapes, so the slashes match verbatim
        (expect (= 1 (count (search s "located in /vis" :literal-text))))))))

(defdescribe fts5-malformed-test
  (describe "malformed FTS5 input (this is WHY ctx-loop wraps fts with a literal fallback)"
    (it "an unbalanced quote throws under raw :fts"
      (let [s (seed! ["alpha bravo"])]
        (expect (try (search s "alpha \"bravo" :fts) false
                  (catch Exception _ true)))))
    (it "the same query is safe under :literal-text (no throw)"
      (let [s (seed! ["alpha \"bravo\" gamma"])]
        ;; escaped to a literal phrase; returns a (possibly empty) vector, never throws
        (expect (vector? (search s "alpha \"bravo" :literal-text)))))
    (it "the Whoosh-style `NEAR/k` infix is NOT valid SQLite FTS5 (throws)"
      (let [s (seed! ["alpha bravo"])]
        (expect (try (search s "alpha NEAR/2 bravo" :fts) false
                  (catch Exception _ true)))))))
