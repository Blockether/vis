(ns com.blockether.vis.internal.compaction-verbs-test
  "Compaction/`fold_session` coverage at three layers:
     1. Raw-Python integration — drive `fold_session` THROUGH the GraalPy sandbox
        so the real argument marshalling (Python list/dict → `->clj`) and the
        visible return string are exercised end to end, not just reasoned about.
     2. Selector resolution — `expand-through` against a live iteration universe:
        every selector form (explicit list, bare-turn, `through`/`from`/`to`/
        `since`) plus the boundaries (cursor past the ends, empty windows,
        unknown scopes) where a fold quietly folds nothing.
     3. What the LLM actually SEES — `apply-summaries` over a trailer: which
        iterations collapse off the wire, where the single gist breadcrumb lands,
        and that a broader re-fold supersedes a finer one.
     4. Session-bag reflection — a landed fold surfaces INSIDE `session_utilization`
        as two string leaves: `folds` (stable gists, one structural delta per fold)
        and `now` (volatile position + budget + live, re-emitted each iteration),
        via `ctx-engine/folds-view` → `ctx-renderer/render-ctx-delta`."
  (:require [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.ctx-renderer :as cr]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.loop :as lp]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private compaction-verbs (var-get #'lp/compaction-verbs))

(def ^:private rebase-session-context! (var-get #'lp/rebase-session-context!))

(def ^:private apply-summaries (var-get #'lp/apply-summaries))

(def ^:private expand-through (var-get #'eng/expand-through))


(def ^:private irm (var-get #'lp/iteration-results-message))

(defn- with-verbs
  "Fresh ctx-atom + a GraalPy context with fold_session bound.
   Returns [ctx-atom eval-fn]; eval-fn runs Python and returns the result string."
  []
  (let [ca
        (atom {"session_turn" 99})

        ctx
        (:python-context (ep/create-python-context (compaction-verbs ca)))]

    [ca
     (fn [^String code]
       (.asString (.eval ^org.graalvm.polyglot.Context ctx "python" code)))]))

(defn- trailer
  "Build an apply-summaries trailer from `tN/iN` iteration ids: each becomes one
   iter-record whose lone form carries that scope (`tN/iN/f1`)."
  [& iters]
  (mapv (fn [pos scope]
          [pos {:forms-vec [{:scope (str scope "/f1") :stdout "x"}]}])
        (range 1 (inc (count iters)))
        iters))

;; ── layer 1: raw-Python integration ─────────────────────────────────────────

(defdescribe
  compaction-verbs-python-test
  (it "fold_session(list, gist): records a \"scopes\" intent + returns a visible confirmation"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session([\"t1/i2\", \"t1/i3\"], \"explored auth\")")]

        (expect
          (= [{"scopes" #{"t1/i2" "t1/i3"} "issued_turn" 99 "at_turn" 99 "gist" "explored auth"}]
             (get @ca "session_summaries")))
        (expect (re-find #"^folded " out))
        (expect (re-find #"explored auth" out))))
  (it "normalizes a provider's JSON-encoded string key before selecting scopes"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session('[\"t1/i2\", \"t1/i3\"]', \"encoded key\")")]

        (expect (=
                  [{"scopes" #{"t1/i2" "t1/i3"} "issued_turn" 99 "at_turn" 99 "gist" "encoded key"}]
                  (get @ca "session_summaries")))
        (expect (str/includes? out "folded t1/i2, t1/i3"))))
  ;; Regression, reported as "how come the latest fold saved 0 tokens?": a hyphen
  ;; range typed as ONE key (`"t2/i1-i56"`) used to be swallowed as a single
  ;; opaque scope id. It matched no wire step, the card still said `folded …`,
  ;; and the gist anchored to an id the session never had.
  (it "fold_session(\"tN/iA-iB\"): a hyphen RANGE folds the window, not one bogus scope id"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3" "t1/i4"])

            out
            (ev "fold_session(\"t1/i2-i4\", \"middle\")")]

        (expect (= [{"from" "t1/i2" "to" "t1/i4" "issued_turn" 99 "at_turn" 99 "gist" "middle"}]
                   (get @ca "session_summaries")))
        (expect (str/includes? out "folded t1/i2-t1/i4"))
        (expect (= #{"t1/i2" "t1/i3" "t1/i4"}
                   (get (first (expand-through (get @ca "session_summaries")
                                               ["t1/i1" "t1/i2" "t1/i3" "t1/i4"]))
                        "scopes")))))
  (it "fold_session(\"tN/iA..iB\"): `..` spells the same window as `-`"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3" "t1/i4"])

            _
            (ev "fold_session(\"t1/i2..i4\", \"middle\")")]

        (expect (= [{"from" "t1/i2" "to" "t1/i4" "issued_turn" 99 "at_turn" 99 "gist" "middle"}]
                   (get @ca "session_summaries")))))
  (it "fold_session(\"-tN/iM\"): an OPEN START folds every settled step through the cursor"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session(\"-t1/i5\", \"early reads\")")]

        (expect (= [{"through" "t1/i5" "issued_turn" 99 "at_turn" 99 "gist" "early reads"}]
                   (get @ca "session_summaries")))
        (expect (re-find #"through t1/i5" out))))
  ;; A foreign ProxyExecutable is positional-only, so the sandbox folds Python
  ;; **kwargs into ONE trailing dict for the DIRECT verbs and the verb unfolds it.
  ;; Both keyword spellings must bind exactly like the positional call.
  (it "fold_session(key, gist=…): a trailing KEYWORD gist binds like the positional one"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session(\"-t1/i5\", gist=\"early reads\")")]

        (expect (= [{"through" "t1/i5" "issued_turn" 99 "at_turn" 99 "gist" "early reads"}]
                   (get @ca "session_summaries")))
        (expect (re-find #"through t1/i5" out))
        (expect (re-find #"early reads" out))))
  (it "fold_session(key=…, gist=…): a fully KEYWORD call selects and summarizes"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session(key=[\"t1/i2\", \"t1/i3\"], gist=\"explored auth\")")]

        (expect
          (= [{"scopes" #{"t1/i2" "t1/i3"} "issued_turn" 99 "at_turn" 99 "gist" "explored auth"}]
             (get @ca "session_summaries")))
        (expect (re-find #"explored auth" out))))
  ;; The verb takes a KEY and a gist and nothing else: whatever else still spreads at
  ;; the top level is not a key, so it is REFUSED with the grammar instead of quietly
  ;; folding nothing.
  (it "fold_session(through=…): a leftover selector KEYWORD is refused with the grammar"
      (let [ca
            (atom {"session_turn" 99})

            sf
            (get (compaction-verbs ca) 'fold-session)

            ex
            (try (sf {"through" "t1/i5" "gist" "early reads"})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :vis/fold-session-key (:type (ex-data ex))))
        (expect (str/includes? (ex-message ex) "fold_session takes a key and a gist"))
        (expect (nil? (get @ca "session_summaries")))))
  (it "fold_session(\"tN/iM-\"): an OPEN END folds every settled step since the cursor"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session(\"t2/i1-\")")]

        (expect (= [{"since" "t2/i1" "issued_turn" 99 "at_turn" 99}] (get @ca "session_summaries")))
        (expect (re-find #"^folded since t2/i1" out))))
  (it
    "an OPEN END FREEZES to concrete scopes at fold time when a universe exists — no rolling swallow of future work"
    (let [[ca ev]
          (with-verbs)

          _
          (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3"])

          out
          (ev "fold_session(\"t1/i2-\", \"tail\")")

          [intent]
          (get @ca "session_summaries")]

      ;; frozen to the scopes present NOW — the raw `since` selector is gone
      (expect (= {"scopes" #{"t1/i2" "t1/i3"} "issued_turn" 99 "at_turn" 99 "gist" "tail"} intent))
      (expect (not (contains? intent "since")))
      (expect (re-find #"^folded since t1/i2" out))
      ;; and because it's frozen, a LATER-grown universe can't swallow new iters
      (expect (= #{"t1/i2" "t1/i3"}
                 (get (first (expand-through [intent] ["t1/i1" "t1/i2" "t1/i3" "t1/i4" "t1/i5"]))
                      "scopes")))))
  (it "an OPEN END that covered a whole turn keeps that turn's explicit intent when it freezes"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1"])

            _
            (ev "fold_session(\"t1/i2-\", \"open\")")

            [intent]
            (get @ca "session_summaries")]

        ;; frozen scopes PLUS the whole-turn intent the open range carried at
        ;; fold time (it covered ALL of t2) — dropping it on freeze would
        ;; resurrect t2's Q/A recap downstream.
        (expect
          (= {"scopes" #{"t1/i2" "t2/i1"} "issued_turn" 99 "at_turn" 99 "gist" "open" "turns" #{2}}
             intent))
        (expect (not (contains? intent "since")))
        (expect (= #{"t1/i2" "t2/i1"}
                   (get (first (expand-through [intent] ["t1/i1" "t1/i2" "t2/i1" "t2/i2"]))
                        "scopes")))))
  (it "a BOUNDED range stays RAW even with a universe — its ceiling already blocks new scopes"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3"])

            _
            (ev "fold_session(\"-t1/i2\", \"early\")")]

        (expect (= [{"through" "t1/i2" "issued_turn" 99 "at_turn" 99 "gist" "early"}]
                   (get @ca "session_summaries")))))
  (it "fold_session(\"tN/iA-tM\"): a range may cross turns and end on a whole turn"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"])

            out
            (ev "fold_session(\"t1/i3-t2\", \"across\")")]

        (expect (= [{"from" "t1/i3" "to" "t2" "issued_turn" 99 "at_turn" 99 "gist" "across"}]
                   (get @ca "session_summaries")))
        (expect (str/includes? out "folded t1/i3-t2"))
        (expect (= #{"t1/i3" "t2/i1" "t2/i2"}
                   (get (first (expand-through (get @ca "session_summaries")
                                               ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"]))
                        "scopes")))))
  (it "several tokens union in ONE call: explicit ids beside a range"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1" "t2/i2" "t2/i3"])

            out
            (ev "fold_session(\"t1/i1, t2/i1-i3\", \"mixed\")")]

        (expect (= [{"scopes" #{"t1/i1"}
                     "from" "t2/i1"
                     "to" "t2/i3"
                     "issued_turn" 99
                     "at_turn" 99
                     "gist" "mixed"}]
                   (get @ca "session_summaries")))
        (expect (str/includes? out "folded t1/i1, t2/i1-t2/i3"))))
  (it "a token that is not a step key is refused BY NAME, with the grammar"
      (let [ca
            (atom {"session_turn" 99})

            sf
            (get (compaction-verbs ca) 'fold-session)

            ex
            (try (sf "t1/i1, ancient-history" "typo") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :vis/fold-session-key (:type (ex-data ex))))
        (expect (str/includes? (ex-message ex) "not a step key: \"ancient-history\""))
        (expect (nil? (get @ca "session_summaries")))))
  ;; Regression, user report: `fold_session("t1/i61-i98,t3/i111-i135,t4", …)` was refused
  ;; with "one range per fold", so disjoint settled spans cost one call — and one repeated
  ;; gist — per span.
  (it "SEVERAL ranges in ONE call fold every window and record a single summary"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc
              "engine_iter_universe"
              ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t3/i1" "t3/i2" "t4/i1"])

            out
            (ev "fold_session(\"t1/i1-i2, t3/i1-i2, t4\", \"two spans and a whole turn\")")

            summaries
            (get @ca "session_summaries")]

        (expect (= 1 (count summaries)))
        (expect (= [{"from" "t1/i1" "to" "t1/i2"} {"from" "t3/i1" "to" "t3/i2"}]
                   (get (first summaries) "ranges")))
        (expect (= #{"t1/i1" "t1/i2" "t3/i1" "t3/i2" "t4/i1"}
                   (get (first (expand-through summaries (get @ca "engine_iter_universe")))
                        "scopes")))
        (expect (str/includes? out "folded"))))
  ;; Regression, same report: the fold that "saved 0 tokens" resolved to NOTHING
  ;; and was recorded anyway, so the ack claimed a fold the wire never made.
  (it "a key that matches NO settled step is refused instead of recording a silent no-op"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'fold-session)

            ex
            (try (sf "t1/i7-i9" "nothing there") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :vis/fold-session-unknown-key (:type (ex-data ex))))
        (expect (str/includes? (ex-message ex) "matches no settled step"))
        (expect (str/includes? (ex-message ex) "t1/i1 … t2/i1"))
        (expect (nil? (get @ca "session_summaries")))))
  (it "fold_session([\"t2\"]): a bare turn id records as a whole-turn scope token"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session([\"t2\"], \"whole turn 2\")")]

        (expect (= [{"scopes" #{"t2"} "issued_turn" 99 "at_turn" 99 "gist" "whole turn 2"}]
                   (get @ca "session_summaries")))
        (expect (re-find #"^folded t2 " out))))
  (it "fold_session WITHOUT a gist records a canonical gist-less collapse"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session([\"t1/i1\"])")]

        (expect (= [{"scopes" #{"t1/i1"} "issued_turn" 99 "at_turn" 99}]
                   (get @ca "session_summaries")))
        (expect (re-find #"^folded t1/i1" out))))
  (it "an empty/blank key is a no-op: records nothing, returns a hint"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "fold_session([])")]

        (expect (nil? (get @ca "session_summaries")))
        (expect (re-find #"nothing to fold" out))))
  (it
    "a non-string key — a stale selector structure — is refused by name with the grammar"
    (let [ca
          (atom {"session_turn" 99})

          sf
          (get (compaction-verbs ca) 'fold-session)

          ex
          (try (sf {"through" "t1/i5"} "stale shape") nil (catch clojure.lang.ExceptionInfo e e))]

      (expect (= :vis/fold-session-key (:type (ex-data ex))))
      (expect (str/includes? (ex-message ex) "fold_session takes a key and a gist"))
      (expect (str/includes? (ex-message ex) "\"t2/i1-i56\" a range"))
      (expect (nil? (get @ca "session_summaries")))))
  (it "blocks only the LIVE (unsettled) iteration of the current turn and any future turn"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'fold-session)]

        ;; `t2/i2` is the iteration being emitted right now (absent from the
        ;; settled universe); `t3/i1` is a future step. Both are off-limits.
        (doseq [[target blocked] [[["t2/i2"] #{"t2/i2"}] [["t3/i1"] #{"t3/i1"}]]]
          (let [ex (try (sf target "unsafe") nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (= :vis/fold-session-active-turn (:type (ex-data ex))))
            (expect (= 2 (:current-turn (ex-data ex))))
            (expect (= blocked (:blocked-scopes (ex-data ex))))
            (expect (str/includes? (ex-message ex) "live iteration"))
            (expect (str/includes? (ex-message ex) "COMPLETED steps"))))
        (expect (nil? (get @ca "session_summaries")))))
  (it "allows folding a SETTLED iteration of the current turn (finer-grained than the whole turn)"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'fold-session)]

        ;; `t2/i1` is a completed iteration of the current turn — foldable, even
        ;; while turn 2 is still being produced. A bare `t2` and a `through`
        ;; cursor resolve against the universe, so they too fold only settled
        ;; steps (never the live iteration).
        (expect (re-find #"^folded t2/i1" (sf ["t2/i1"] "settled")))
        (expect (re-find #"^folded t2\b" (sf ["t2"] "whole-so-far")))
        (expect (re-find #"^folded through t2/i1" (sf "-t2/i1" "upto")))
        (expect (seq (get @ca "session_summaries")))))
  (it "allows completed prior turns after the new turn has started"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'fold-session)]

        (expect (re-find #"^folded t1" (sf ["t1"] "done")))
        (expect (= [{"scopes" #{"t1"} "issued_turn" 2 "at_turn" 2 "gist" "done"}]
                   (get @ca "session_summaries")))))
  (it "fails closed when the current turn is unavailable"
      (let [sf
            (get (compaction-verbs (atom {})) 'fold-session)

            ex
            (try (sf ["t1/i1"] "unknown") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :vis/fold-session-turn-unknown (:type (ex-data ex)))))))

;; ── layer 1b: the KEY grammar (pure parse, no ctx) ──────────────────────────

(defdescribe
  fold-key-test
  (it "one step, one turn, and a form id folding its whole iteration"
      (expect (= {"scopes" #{"t2/i5"}} (:intent (eng/fold-key "t2/i5"))))
      (expect (= {"scopes" #{"t2"}} (:intent (eng/fold-key "t2"))))
      (expect (= {"scopes" #{"t2/i5"}} (:intent (eng/fold-key "t2/i5/f3")))))
  (it "a hyphen or `..` range, with the shared turn optional on the right"
      (doseq [t ["t2/i1-i56" "t2/i1..i56" "t2/i1-t2/i56" "t2/i1-56"]]
        (expect (= {"from" "t2/i1" "to" "t2/i56"} (:intent (eng/fold-key t)))))
      (expect (= "t2/i1-t2/i56" (:label (eng/fold-key "t2/i1-i56")))))
  (it "an omitted side opens the range"
      (expect (= {"through" "t2/i56"} (:intent (eng/fold-key "-t2/i56"))))
      (expect (= {"through" "t2/i56"} (:intent (eng/fold-key "..t2/i56"))))
      (expect (= {"since" "t2/i5"} (:intent (eng/fold-key "t2/i5-"))))
      (expect (= {"since" "t2/i5"} (:intent (eng/fold-key "t2/i5..")))))
  (it "commas, whitespace and lists all union into one intent"
      (expect (= {"scopes" #{"t1/i2" "t1/i3"}} (:intent (eng/fold-key "t1/i2, t1/i3"))))
      (expect (= {"scopes" #{"t1/i2" "t1/i3"}} (:intent (eng/fold-key "t1/i2 t1/i3"))))
      (expect (= {"scopes" #{"t1/i2" "t1/i3"}} (:intent (eng/fold-key ["t1/i2" "t1/i3"]))))
      (expect (= {"scopes" #{"t1"} "from" "t2/i1" "to" "t2/i3"}
                 (:intent (eng/fold-key ["t1" "t2/i1-i3"])))))
  (it "nothing named is nil — not an error"
      (expect (nil? (eng/fold-key nil)))
      (expect (nil? (eng/fold-key "")))
      (expect (nil? (eng/fold-key []))))
  (it "a token that is not a step key and a non-string key are refusals"
      (expect (str/includes? (:error (eng/fold-key "t2/i1-i56x")) "not a step key"))
      (expect (str/includes? (:error (eng/fold-key "nonsense")) "not a step key"))
      (expect (str/includes? (:error (eng/fold-key "-")) "not a step key"))
      ;; Regression, user report: the verb is `fold_session(key, gist)` and nothing else, so a
      ;; leftover selector structure is REFUSED with the grammar instead of folding nothing.
      (expect (str/includes? (:error (eng/fold-key {"through" "t1/i5"}))
                             "fold_session takes a key and a gist")))
  ;; Regression, user report: the same key was refused with "one range per fold" — one
  ;; intent used to carry one window, so a sweep spread across turns could not be one fold.
  (it "SEVERAL ranges union into ONE intent: a lone window flat, the rest under `ranges`"
      (expect (= {"scopes" #{"t4"}
                  "ranges" [{"from" "t1/i61" "to" "t1/i98"} {"from" "t3/i111" "to" "t3/i135"}]}
                 (:intent (eng/fold-key "t1/i61-i98,t3/i111-i135,t4"))))
      (expect (= "t4, t1/i61-t1/i98, t3/i111-t3/i135"
                 (:label (eng/fold-key "t1/i61-i98,t3/i111-i135,t4"))))
      (expect (= {"ranges" [{"through" "t1/i5"} {"since" "t3/i2"}]}
                 (:intent (eng/fold-key "-t1/i5, t3/i2-")))))
  (it "every refusal carries the whole grammar, so the caller reads it at once"
      (expect (str/includes? (:error (eng/fold-key "nonsense")) eng/fold-key-grammar))))


;; ── layer 2: selector resolution against a live universe ─────────────────────

(def ^:private universe ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"])

(defn- resolve1
  "Resolve ONE selector map against `universe` → its concrete scope set."
  [sel]
  (get (first (expand-through [sel] universe)) "scopes"))

(defdescribe expand-through-selectors-test
             (it "through: every universe scope AT OR BEFORE the cursor"
                 (expect (= #{"t1/i1" "t1/i2"} (resolve1 {"through" "t1/i2"}))))
             (it "from/to: an inclusive window across a turn boundary"
                 (expect (= #{"t1/i2" "t1/i3" "t2/i1"} (resolve1 {"from" "t1/i2" "to" "t2/i1"}))))
             (it "since: every universe scope AT OR AFTER the cursor"
                 (expect (= #{"t1/i3" "t2/i1" "t2/i2"} (resolve1 {"since" "t1/i3"}))))
             (it "open-ended from (no to) reaches the newest — same as since"
                 (expect (= #{"t2/i1" "t2/i2"} (resolve1 {"from" "t2/i1"}))))
             (it "open-ended to (no from) reaches from the start — same as through"
                 (expect (= #{"t1/i1" "t1/i2"} (resolve1 {"to" "t1/i2"}))))
             (it "a bare turn id expands to every iteration of that turn"
                 (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (resolve1 {"scopes" #{"t1"}}))))
             (it "explicit tN/iN ids pass verbatim, unioned with a selector"
                 (expect (= #{"t1/i1" "t2/i2"} (resolve1 {"scopes" #{"t1/i1"} "since" "t2/i2"}))))
             ;; A bare `tN` range cursor is a WHOLE-TURN boundary, not a silent
             ;; no-op — regression for `{"through" "tN"}` folding nothing.
             (it "through: a bare turn cursor covers all of that turn (and earlier)"
                 (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (resolve1 {"through" "t1"}))))
             (it "to: a bare turn cursor is an upper whole-turn boundary"
                 (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (resolve1 {"to" "t1"}))))
             (it "since: a bare turn cursor covers that turn onward"
                 (expect (= #{"t2/i1" "t2/i2"} (resolve1 {"since" "t2"}))))
             (it "from: a bare turn cursor is a lower whole-turn boundary"
                 (expect (= #{"t2/i1" "t2/i2"} (resolve1 {"from" "t2"}))))
             ;; ── boundaries ──
             (it "cursor PAST the newest folds the whole universe (through)"
                 (expect (= (set universe) (resolve1 {"through" "t9/i9"}))))
             (it "cursor BEFORE the oldest folds nothing (through)"
                 (expect (= #{} (resolve1 {"through" "t0/i0"}))))
             (it "an inverted window (from > to) folds nothing"
                 (expect (= #{} (resolve1 {"from" "t2/i2" "to" "t1/i1"}))))
             (it "a single-point window (from == to) folds exactly that scope"
                 (expect (= #{"t1/i3"} (resolve1 {"from" "t1/i3" "to" "t1/i3"}))))
             (it "a selector naming a turn absent from the universe folds nothing"
                 (expect (= #{} (resolve1 {"scopes" #{"t7"}}))))
             ;; Regression, user report: several windows in one key were refused, so
             ;; disjoint spans could not share one fold.
             (it "several windows resolve independently and UNION"
                 (expect (= #{"t1/i1" "t2/i2"}
                            (resolve1 {"ranges" [{"from" "t1/i1" "to" "t1/i1"}
                                                 {"since" "t2/i2"}]})))
                 (expect (= #{"t1/i1" "t1/i2" "t2/i2"}
                            (resolve1 {"scopes" #{"t1/i1"}
                                       "ranges" [{"through" "t1/i2"} {"since" "t2/i2"}]}))))
             (it "an intent with NO selector key passes through untouched"
                 (expect (= [{"drop" true}] (expand-through [{"drop" true}] universe)))))

(def ^:private supersede-summaries (var-get #'eng/supersede-summaries))

(defdescribe
  whole-turn-intent-test
  ;; `"turns"` records EXPLICIT whole-turn intent only. Downstream
  ;; (previous-turn-context) keys Q/A removal off it, so these pin the boundary
  ;; between "fold these iterations" and "fold that whole turn".
  (it "a bare tN records whole-turn intent even when the universe is empty"
      (let [out (first (expand-through [{"scopes" #{"t1"}}] []))]
        (expect (= #{1} (get out "turns")))))
  (it "a range selector spanning every iteration of a turn records that turn"
      (let [out (first (expand-through [{"through" "t2/i1"}] universe))]
        ;; t1 fully inside the window; t2 only partially (t2/i2 is outside).
        (expect (= #{1} (get out "turns")))))
  (it "a range selector past the newest records every universe turn"
      (let [out (first (expand-through [{"through" "t9/i9"}] universe))]
        (expect (= #{1 2} (get out "turns")))))
  (it "several windows record every turn any ONE of them fully covers"
      (let [out (first (expand-through [{"ranges" [{"from" "t1/i1" "to" "t1/i3"}
                                                   {"since" "t2/i1"}]}]
                                       universe))]
        (expect (= #{1 2} (get out "turns")))))
  (it "an ENUMERATED list covering every iteration records NO whole-turn intent"
      (let [out (first (expand-through [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"}}] universe))]
        (expect (nil? (get out "turns")))))
  (it "supersede merges a dropped summary's whole-turn intent into its surviving coverer"
      (let [resolved
            (expand-through [{"scopes" #{"t1"} "gist" "fine"} {"through" "t2/i2" "gist" "broad"}]
                            universe)

            out
            (supersede-summaries resolved)]

        (expect (= 1 (count out)))
        (expect (= "broad" (get (first out) "gist")))
        (expect (= #{1 2} (get (first out) "turns"))))))

;; ── layer 3: what the LLM sees (apply-summaries over a trailer) ───────────────

(defn- summary-forms
  [applied]
  (mapcat (fn [[_ rec]]
            (filter :summary? (:forms-vec rec)))
          applied))

(defdescribe
  apply-summaries-boundary-test
  (it "through cursor collapses every step at/before it; the tail survives"
      (let [tr
            (trailer "t1/i1" "t1/i2" "t1/i3")

            out
            (apply-summaries tr [{"through" "t1/i2" "at_turn" 1 "gist" "G"}])

            [[_ r1] [_ r2] [_ r3]]
            out

            sfs
            (summary-forms out)]

        (expect (:collapsed? r1))
        (expect (:collapsed? r2))
        (expect (not (:collapsed? r3))) ; the tail step is untouched
        (expect (= [] (:forms-vec r2))) ; collapsed body left the wire
        (expect (= 1 (count sfs)))      ; ONE breadcrumb
        (expect (= "G" (:summary-gist (first sfs))))
        ;; the breadcrumb is injected at the EARLIEST collapsed step (t1/i1)
        (expect (some :summary? (:forms-vec r1)))
        (expect (= ["t1/i1" "t1/i2"] (:summary-iters (first sfs))))))
  (it "a bare-turn fold collapses EVERY iteration of that turn"
      (let [tr
            (trailer "t1/i1" "t1/i2" "t1/i3")

            out
            (apply-summaries tr [{"scopes" #{"t1"} "at_turn" 1 "gist" "all of t1"}])]

        (expect (every? (fn [[_ r]]
                          (:collapsed? r))
                        out))
        (expect (= 1 (count (summary-forms out))))))
  (it "a from/to window collapses only the inclusive middle"
      (let [tr
            (trailer "t1/i1" "t1/i2" "t1/i3" "t1/i4")

            out
            (apply-summaries tr [{"from" "t1/i2" "to" "t1/i3" "at_turn" 1 "gist" "mid"}])]

        ;; endpoints of the window survive; only i2,i3 collapse
        (expect (= 2
                   (count (filter (fn [[_ r]]
                                    (:collapsed? r))
                                  out))))
        (expect (= 1 (count (summary-forms out))))))
  (it "a since cursor collapses that step through the newest"
      (let [tr
            (trailer "t1/i1" "t1/i2" "t1/i3")

            out
            (apply-summaries tr [{"since" "t1/i2" "at_turn" 1 "gist" "tail"}])

            [[_ r1] [_ r2] [_ r3]]
            out]

        (expect (not (:collapsed? r1)))
        (expect (:collapsed? r2))
        (expect (:collapsed? r3))))
  (it "a fold whose scopes miss the trailer entirely collapses nothing"
      (let [tr
            (trailer "t1/i1" "t1/i2")

            out
            (apply-summaries tr [{"through" "t0/i0" "at_turn" 1 "gist" "nope"}])]

        (expect (not-any? (fn [[_ r]]
                            (:collapsed? r))
                          out))
        (expect (empty? (summary-forms out)))))
  (it "empty summaries leave the trailer byte-for-byte"
      (let [tr (trailer "t1/i1" "t1/i2")]
        (expect (= (vec tr) (apply-summaries tr [])))))
  (it "a broader re-fold SUPERSEDES the finer one (one breadcrumb, broader gist)"
      (let [[ca ev] (with-verbs)]
        (ev "fold_session([\"t1/i2\", \"t1/i3\"], \"A\")")
        (ev "fold_session([\"t1/i2\", \"t1/i3\", \"t1/i4\"], \"B\")")
        (let [tr (trailer "t1/i2" "t1/i3" "t1/i4")
              out (apply-summaries tr (get @ca "session_summaries"))
              sfs (summary-forms out)]

          (expect (= 1 (count sfs)))
          (expect (= "B" (:summary-gist (first sfs))))
          (expect (every? (fn [[_ r]]
                            (:collapsed? r))
                          out)))))
  (it "a range re-fold supersedes an explicit finer fold of the same region"
      (let [[ca ev] (with-verbs)]
        (ev "fold_session([\"t1/i2\"], \"finer\")")
        (ev "fold_session(\"-t1/i3\", \"broad\")")
        (let [tr (trailer "t1/i1" "t1/i2" "t1/i3")
              out (apply-summaries tr (get @ca "session_summaries"))
              sfs (summary-forms out)]

          (expect (= 1 (count sfs)))
          (expect (= "broad" (:summary-gist (first sfs)))))))
  (it "FOLD OF FOLD: one whole-turn re-fold swallows TWO finer folds into one breadcrumb"
      (let [[ca ev] (with-verbs)]
        ;; two disjoint finer folds recorded first…
        (ev "fold_session([\"t1/i2\", \"t1/i3\"], \"fold A\")")
        (ev "fold_session([\"t1/i5\"], \"fold B\")")
        ;; …then the whole turn is re-folded — a fold OF those folds.
        (ev "fold_session([\"t1\"], \"meta: the whole turn\")")
        (let [tr (trailer "t1/i1" "t1/i2" "t1/i3" "t1/i4" "t1/i5")
              out (apply-summaries tr (get @ca "session_summaries"))
              sfs (summary-forms out)]

          ;; both finer breadcrumbs are superseded in durable state — only the
          ;; meta gist survives, and every iteration collapses off the wire.
          (expect (= 1 (count (get @ca "session_summaries"))))
          (expect (= 1 (count sfs)))
          (expect (= "meta: the whole turn" (:summary-gist (first sfs))))
          (expect (every? (fn [[_ r]]
                            (:collapsed? r))
                          out))))))

;; ── layer 5: session-bag reflection (the CTX delta) ──────────────────────────

(def ^:private folds-view (var-get #'eng/folds-view))

(defn- delta-map
  "The per-iteration ctx map (`ctx-renderer/ctx-delta-map`) for a raw ctx."
  [ctx]
  (cr/ctx-delta-map {:ctx ctx}))

(def ^:private base-ctx
  {"session_id" "s1"
   "session_turn" 4
   "session_scope" {"turn" 4 "iter" 3 "next_form" 1}
   "session_workspace" {"root" "/x"}
   "engine_utilization" {"saturation" 8}
   ;; the live wire's iteration universe, stamped by `stamp-iter-universe!`, so
   ;; `folds-view` resolves selectors + computes the still-live ledger.
   "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i5" "t3/i1"]})

(defdescribe
  fold-session-ctx-reflection-test
  ;; The fold GIST lives ONCE in the transcript breadcrumb (rendered where the step
  ;; collapsed, with its file:line anchors). The ONLY thing merged into
  ;; `"session_utilization"` is the tiny volatile `"now"` budget leaf (saved + live,
  ;; NO gists) — there is no `"folds"` leaf, so the heavy gist is never echoed.
  (it "folds-view resolves selectors into the single volatile `now` budget leaf"
      (let [uni
            ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"]

            out
            (folds-view [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"} "gist" "mapped"}] uni nil nil)]

        ;; no gist here (it rides the breadcrumb); just saved · live
        (expect (= {"now" "saved 3/5 (60%) · live t2/*"} out))))
  (it "stamped weights price the saved wire as `~<toks> tok` in the `now` label"
      (let [uni
            ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"]

            weights
            {"t1/i1" 4000 "t1/i2" 6000 "t1/i3" 2000 "t2/i1" 500 "t2/i2" 900}

            out
            (folds-view [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"} "gist" "mapped"}] uni weights nil)]

        ;; only the three folded scopes' weights are summed (4k+6k+2k = 12k)
        (expect (= {"now" "saved 3/5 (60%, ~12k tok) · live t2/*"} out))))
  (it "no weights (or none matching the folded scopes) omits the token clause"
      (let [uni ["t1/i1" "t1/i2" "t2/i1"]]
        ;; nil weights -> scope counts only
        (expect (= {"now" "saved 2/3 (67%) · live t2/*"}
                   (folds-view [{"through" "t1/i2"}] uni nil nil)))
        ;; weights present but none cover the folded scopes -> still no clause
        (expect (= {"now" "saved 2/3 (67%) · live t2/*"}
                   (folds-view [{"through" "t1/i2"}] uni {"t9/i9" 5000} nil)))))
  (it "the live per-call saturation leads the `now` label as a `context <U>%` clause"
      (let [uni
            ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i2"]

            weights
            {"t1/i1" 4000 "t1/i2" 6000 "t1/i3" 2000}]

        ;; util's saturation prepends `context 44%`; folds + tokens follow
        (expect (= {"now" "context 44% · saved 3/5 (60%, ~12k tok) · live t2/*"}
                   (folds-view [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"} "gist" "g"}]
                               uni
                               weights
                               {"saturation" 44})))
        ;; saturation of 0 still shows (0 is a real reading, not "missing")
        (expect (= {"now" "context 0% · saved 3/5 (60%) · live t2/*"}
                   (folds-view [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"} "gist" "g"}]
                               uni
                               nil
                               {"saturation" 0})))
        ;; no util -> no context clause (unchanged shape)
        (expect (= {"now" "saved 3/5 (60%) · live t2/*"}
                   (folds-view [{"scopes" #{"t1/i1" "t1/i2" "t1/i3"} "gist" "g"}] uni nil nil)))))
  (it "a through selector is RESOLVED against the wire when scoring `saved`"
      (let [uni
            ["t1/i1" "t1/i2" "t2/i1"]

            out
            (folds-view [{"through" "t1/i2"}] uni nil nil)]

        (expect (= {"now" "saved 2/3 (67%) · live t2/*"} out))))
  (it "a partial-turn fold leaves the unfolded gaps live in `now`"
      (let [uni
            ["t3/i1" "t3/i2" "t3/i3" "t3/i4" "t3/i5"]

            out
            (folds-view [{"scopes" #{"t3/i1" "t3/i2" "t3/i4"} "gist" "g"}] uni nil nil)]

        ;; the unfolded gaps show as live, run-compressed
        (expect (= {"now" "saved 3/5 (60%) · live t3/i3,i5"} out))))
  (it "a broader re-fold SUPERSEDES a finer one (whole universe folded -> nothing live)"
      (let [uni
            ["t1/i1" "t1/i2" "t1/i3"]

            out
            (folds-view [{"scopes" #{"t1/i1"} "gist" "fine"} {"scopes" #{"t1"} "gist" "meta"}]
                        uni
                        nil
                        nil)]

        ;; every turn folded -> no live section, no gist
        (expect (= {"now" "saved 3/3 (100%)"} out))))
  (it "a fold whose scopes scrolled OFF the wire never inflates `saved` (phantom guard)"
      ;; universe is 3 live iters; the fold references t1/i1 which was trimmed off the
      ;; trailer. `saved` must count only on-wire scopes -> 0/3, not a phantom 1/4.
      (let [uni
            ["t3/i1" "t3/i2" "t3/i3"]

            out
            (folds-view [{"scopes" #{"t1/i1"} "gist" "old"}] uni nil nil)]

        (expect (= {"now" "saved 0/3 (0%) · live t*"} out))))
  (it "with NO universe (resume / fresh seed) folds-view yields `{}` — breadcrumbs carry the gists"
      ;; before the first live send stamps the universe there is no budget to report;
      ;; the transcript breadcrumbs alone hold every fold's gist until the next send.
      (expect (= {}
                 (folds-view [{"scopes" #{"t1/i2" "t1/i1"} "gist" "mapped"} {"through" "t2/i5"}]
                             nil
                             nil
                             nil)))
      (expect (= {} (folds-view [{"scopes" #{"t1/i1"}}] nil nil nil))))
  (it "turn-weights price the removed Q/A recap of a whole-turn fold into the token clause"
      (let [uni ["t1/i1" "t1/i2" "t2/i1"]]
        ;; bare t1 fold: iteration weights (4k+2k) + t1's Q/A recap (6k) = 12k
        (expect (= {"now" "saved 2/3 (67%, ~12k tok) · live t2/*"}
                   (folds-view [{"scopes" #{"t1"} "gist" "g"}]
                               uni
                               {"t1/i1" 4000 "t1/i2" 2000}
                               nil
                               {1 6000})))
        ;; an enumerated fold carries NO whole-turn intent -> Q/A weight NOT added
        (expect (= {"now" "saved 2/3 (67%, ~6k tok) · live t2/*"}
                   (folds-view [{"scopes" #{"t1/i1" "t1/i2"} "gist" "g"}]
                               uni
                               {"t1/i1" 4000 "t1/i2" 2000}
                               nil
                               {1 6000})))
        ;; Q/A weight alone (no iteration weights) still yields the clause
        (expect (= {"now" "saved 2/3 (67%, ~6k tok) · live t2/*"}
                   (folds-view [{"scopes" #{"t1"} "gist" "g"}] uni nil nil {1 6000})))))
  (it "session-view merges only `now` INTO session_utilization — no top-level key, no `folds` leaf"
      (expect (not (contains? (eng/session-view base-ctx) "fold_sessions")))
      (let [util (get (eng/session-view (assoc base-ctx
                                          "session_summaries" [{"scopes" #{"t1/i1"} "gist" "g"}]))
                      "session_utilization")]
        (expect (not (contains? util "folds")))
        (expect (contains? util "now"))))
  (it "a landed fold emits a session[\"utilization\"][\"now\"] budget delta, NO gist echoed"
      (let [c1
            (assoc base-ctx "session_summaries" [{"scopes" #{"t1/i1" "t1/i2"} "gist" "mapped"}])

            d
            (cr/render-ctx-delta (delta-map base-ctx) (delta-map c1))]

        (expect (re-find #"session\[\"utilization\"\]\[\"now\"\] = " d))
        ;; the gist is NOT in the utilization delta — it rides only the breadcrumb
        (expect (not (re-find #"mapped" d)))
        (expect (not (re-find #"\[\"folds\"\]" d)))))
  (it "universe grows with NO new fold -> `now` re-emits, and there is never a `folds` leaf"
      (let [folded
            (assoc base-ctx "session_summaries" [{"scopes" #{"t1/i1" "t1/i2"} "gist" "mapped"}])

            grown
            (update folded "engine_iter_universe" conj "t3/i2")

            d
            (cr/render-ctx-delta (delta-map folded) (delta-map grown))]

        (expect (re-find #"session\[\"utilization\"\]\[\"now\"\]" d))
        (expect (not (re-find #"\[\"folds\"\]" d)))
        (expect (not (re-find #"mapped" d)))))
  (it "no summaries -> no now/folds subkeys in utilization, no delta"
      (expect (not (contains? (get (delta-map base-ctx) "utilization") "now")))
      (expect (not (contains? (get (delta-map base-ctx) "utilization") "folds")))
      (expect (nil? (cr/render-ctx-delta (delta-map base-ctx) (delta-map base-ctx)))))
  (it "the live bound session bag (project-ctx) carries the `now` budget inside utilization"
      (expect (contains? (get (cr/project-ctx (eng/session-view (assoc base-ctx
                                                                  "session_summaries"
                                                                  [{"scopes" #{"t1/i1"}
                                                                    "gist" "g"}])))
                              "utilization")
                         "now"))))

(defdescribe
  over-budget-hint-test
  ;; Pressure escalates before the soft ceiling and never silently expires while
  ;; the request remains dangerous. `stamp-utilization!` arms at 75% of the
  ;; operating budget; `over-budget-hint` chooses advisory/urgent/required copy.
  (let [required
        {"last_request_tokens" 210000
         "auto_compress_above" 200000
         "model_input_limit" 1000000
         "saturation" 21}

        urgent
        {"last_request_tokens" 180000
         "auto_compress_above" 200000
         "model_input_limit" 1000000
         "saturation" 18}

        advisory
        {"last_request_tokens" 150000
         "auto_compress_above" 200000
         "model_input_limit" 1000000
         "saturation" 15}

        under
        {"last_request_tokens" 140000
         "auto_compress_above" 200000
         "model_input_limit" 1000000
         "saturation" 14}

        stamp
        #'lp/stamp-utilization!]

    (it "uses a 200k default compaction budget"
        (expect (= 200000 eng/DEFAULT_PROMPT_BUDGET_TOKENS)))
    (it "starts advisory pressure at 75% of the operating budget"
        (let [hint (eng/over-budget-hint advisory 6 6)]
          (expect (str/includes? hint "FOLD SOON"))
          (expect (str/includes? hint "150k"))
          (expect (str/includes? hint "200k"))
          (expect (str/includes? hint "fold_session"))))
    (it "escalates at 90% and requires folding before more large tool calls"
        (let [hint (eng/over-budget-hint urgent 6 6)]
          (expect (str/includes? hint "FOLD NOW"))
          (expect (str/includes? hint "before another large tool call"))
          (expect (str/includes? hint "last completed scope"))))
    (it "is imperative above budget and prescribes one broad verified fold"
        (let [hint (eng/over-budget-hint required 6 6)]
          (doseq [text
                  ["ACTION REQUIRED" "210k" "200k" "Fold settled search/tool sweeps"
                   "one broad fold_session" "last completed scope"
                   "preserve decisions, edits, and verification" "preserve exact physical paths"
                   "If the edit is ready and the next patch fits available headroom, patch first"
                   "compact actionable checkpoint" "exact paths/symbols" "hypothesis"
                   "intended edit/test" "dirty files" "never bare or abbreviated filenames"
                   "confirm the receipt saved tokens"]]
            (expect (str/includes? hint text)))))
    (it "never silently expires while pressure remains"
        (expect (some? (eng/over-budget-hint required 6 6)))
        (expect (some? (eng/over-budget-hint required 60 6))))
    (it "is nil below 75%, when unarmed, or when the ceiling is missing"
        (expect (nil? (eng/over-budget-hint advisory 6 nil)))
        (expect (nil? (eng/over-budget-hint under 6 6)))
        (expect (nil? (eng/over-budget-hint {"last_request_tokens" 210000} 6 6))))
    (it "session-view surfaces the hint only while pressure is armed"
        (let [ctx {"session_id" "s" "session_turn" 7 "engine_utilization" advisory}]
          (expect (= (eng/over-budget-hint advisory 7 6)
                     (get-in (eng/session-view (assoc ctx "engine_overbudget_hint_turn" 6))
                             ["session_utilization" "hint"])))
          (expect (not (contains? (get (eng/session-view ctx) "session_utilization") "hint")))))
    (it "stamp-utilization! arms at 75%, holds, clears below, and re-arms"
        (let [a (atom {"session_turn" 5})]
          (stamp a advisory)
          (expect (= 5 (get @a "engine_overbudget_hint_turn")))
          (swap! a assoc "session_turn" 6)
          (stamp a required)
          (expect (= 5 (get @a "engine_overbudget_hint_turn")))
          (swap! a assoc "session_turn" 7)
          (stamp a under)
          (expect (nil? (get @a "engine_overbudget_hint_turn")))
          (swap! a assoc "session_turn" 9)
          (stamp a urgent)
          (expect (= 9 (get @a "engine_overbudget_hint_turn")))
          ;; A transient nil must not blank the last authoritative utilization.
          (stamp a nil)
          (expect (= urgent (get @a "engine_utilization")))))))

;; ── layer 6: the human-facing fold CARD (tokens saved + context level) ───────

(defn- priced-ctx
  "ctx-atom pre-stamped as a live send would be: an iteration universe, the
   per-scope ~token weights (`stamp-iter-universe!`), and the provider-measured
   utilization — everything the `fold_session` card prices its suffix from."
  []
  (atom {"session_turn" 3
         "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3" "t2/i1" "t2/i9"]
         "engine_iter_weights" {"t1/i1" 12000 "t1/i2" 3400 "t1/i3" 900 "t2/i1" 500}
         "engine_utilization" {"saturation" 44
                               "last_request_tokens" 42000
                               "auto_compress_above" 70000
                               "model_input_limit" 96000}}))

(defdescribe
  fold-session-card-test
  (it "a real fold crossing 200k rebases the snapshot and emits a full same-turn delta"
      (let [ctx
            (atom {"session_turn" 2
                   "engine_iter_universe" ["t1/i1" "t1/i2"]
                   "engine_iter_weights" {"t1/i1" 120000 "t1/i2" 80000}
                   "engine_utilization" {"auto_compress_above" 200000}})

            rebase
            (atom {:reclaimed-tokens 0 :pending? false})

            standing
            (atom {:block "old cached prefix" :baseline {"old" true}})

            current
            {"turn" 2 "workspace" {"root" "/repo"}}

            sf
            (get (compaction-verbs ctx rebase) 'fold-session)]

        ;; Invoke the same bound verb used by real Python/native fold_session calls.
        (sf ["t1/i1"] "first large fold")
        (expect (= {:reclaimed-tokens 120000 :pending? false} @rebase))
        (sf ["t1/i2"] "second large fold")
        (expect (= {:reclaimed-tokens 200000 :pending? true} @rebase))
        ;; Invoke the exact iteration-end transition used after that tool result.
        (let [delta (rebase-session-context! standing rebase current)]
          (expect (= {:reclaimed-tokens 0 :pending? false} @rebase))
          (expect (= current (:baseline @standing)))
          (expect (str/includes? (:block @standing) "session ="))
          (expect (str/includes? delta "session[\"turn\"] = 2"))
          (expect (str/includes? delta "session[\"workspace\"]")))))
  (it "a broader re-fold charges only scopes newly visible since the earlier fold"
      ;; Multiple `fold_session` calls can happen before the next provider projection
      ;; re-stamps `engine_iter_weights`. The second card and rebase ledger must not
      ;; charge the first fold's raw payload again merely because its old weight remains.
      (let [ca
            (atom {"session_turn" 3
                   "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3"]
                   "engine_iter_weights" {"t1/i1" 120000 "t1/i2" 80000 "t1/i3" 30000}
                   "engine_utilization" {"auto_compress_above" 200000}})

            rebase
            (atom {:reclaimed-tokens 0 :pending? false})

            sf
            (get (compaction-verbs ca rebase) 'fold-session)

            first-card
            (sf ["t1/i1"] "first")

            broader-card
            (sf "-t1/i3" "broader")]

        (expect (= "folded t1/i1 · saved ~120k tokens · ~60% of budget → first" first-card))
        ;; t1/i1 was already collapsed by `first-card`; only t1/i2 + t1/i3 are new.
        (expect (= "folded through t1/i3 · saved ~110k tokens · ~55% of budget → broader"
                   broader-card))
        (expect (= {:reclaimed-tokens 230000 :pending? true} @rebase))))
  (it "a broader whole-turn re-fold does not recharge an already removed Q/A recap"
      (let [ca
            (atom {"session_turn" 3
                   "engine_iter_universe" ["t1/i1" "t1/i2"]
                   "engine_iter_weights" {"t1/i1" 10000 "t1/i2" 20000}
                   "engine_turn_weights" {1 50000}
                   "engine_utilization" {"auto_compress_above" 100000}})

            rebase
            (atom {:reclaimed-tokens 0 :pending? false})

            sf
            (get (compaction-verbs ca rebase) 'fold-session)]

        (expect (= "folded t1 · saved ~80k tokens · ~80% of budget → first" (sf ["t1"] "first")))
        (expect (= "folded through t1/i2 · saved ~0 tokens → broader" (sf "-t1/i2" "broader")))
        (expect (= {:reclaimed-tokens 80000 :pending? false} @rebase))))
  ;; The verb RETURN string is the tool card the human sees. It is enriched with
  ;; how much wire the fold reclaims — in ~tokens (summed from `engine_iter_weights`)
  ;; AND as a fraction of the OPERATING ceiling (`auto_compress_above`, grown to the
  ;; handled context on a bigger task). This is the fold's OWN reduction, NOT an
  ;; absolute level: a projected level baselines on the growing
  ;; (issue #27's scary regression). A per-fold reduction can never mislead that way.
  ;; Alongside it the card ALSO surfaces the live window fullness as `context <U>%`,
  ;; taken straight from the provider's authoritative `saturation` — a separate,
  ;; absolute reading, omitted when no `saturation` is stamped. That stamp is the
  ;; PRE-fold one (nothing has been sent since), so whenever the fold reclaims wire
  ;; the clause renders the transition `<U>%→~<U'>%` with `U'` projected by
  ;; SUBTRACTING this fold's own reclaim from `last_request_tokens` — never the
  ;; frozen pre-fold number alone beside `saved ~Nk`.
  (it
    "an explicit scope card prices its ~tokens + the reduction as % of budget"
    (let [sf (get (compaction-verbs (priced-ctx)) 'fold-session)]
      (expect
        (=
          "folded t1/i1 · saved ~12k tokens · ~17% of budget · context 44%→~31% (42k→30k tokens) → big cat dump"
          (sf ["t1/i1"] "big cat dump")))))
  (it
    "a `through` selector sums the weight of EVERY scope it resolves"
    (let [sf (get (compaction-verbs (priced-ctx)) 'fold-session)]
      ;; through t1/i2 folds t1/i1 (12k) + t1/i2 (3.4k) = ~15k
      (expect
        (=
          "folded through t1/i2 · saved ~15k tokens · ~22% of budget · context 44%→~28% (42k→27k tokens) → traced"
          (sf "-t1/i2" "traced")))))
  (it "a gist-less fold still shows the tokens + reduction suffix"
      (let [sf (get (compaction-verbs (priced-ctx)) 'fold-session)]
        (expect
          (= "folded t1/i1 · saved ~12k tokens · ~17% of budget · context 44%→~31% (42k→30k tokens)"
             (sf ["t1/i1"])))))
  (it
    "a scope with NO stamped weight reclaims nothing, so the card reads an explicit saved ~0 alongside the live window fullness"
    (let [sf (get (compaction-verbs (priced-ctx)) 'fold-session)]
      ;; t2/i9 is ON the wire but absent from the weights map (created this
      ;; iteration, not yet priced) — a fold that frees no measured wire honestly
      ;; reports `saved ~0 tokens`, never a silently dropped clause, and the
      ;; absolute `context <U>%` (provider saturation) still tells the human where
      ;; the window stands.
      (expect (= "folded t2/i9 · saved ~0 tokens · context 44% (42k/96k tokens) → fresh"
                 (sf ["t2/i9"] "fresh")))))
  (it
    "a fold reaching over off-wire scopes says so and names the whole-turn shape that reclaims them"
    ;; Issue #88: every iteration of a NORMALLY COMPLETED turn is priced 0 by
    ;; `off-wire-seed?` — its raw results never replay, only the turn's Q/A recap
    ;; does, and ONLY a whole-turn token (`tN`) charges and removes that recap.
    ;; Folding 44 `tN/iM` ids and reading `saved ~946` looked like broken
    ;; accounting; the card now names the weightless share and the shape that
    ;; would actually reclaim it. The numbers themselves are unchanged.
    (let [mk (fn []
               (atom {"session_turn" 3
                      "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1" "t2/i2"]
                      ;; turn 1 completed normally: its iterations are off the wire
                      "engine_iter_weights" {"t1/i1" 0 "t1/i2" 0 "t2/i1" 8000 "t2/i2" 4000}
                      "engine_turn_weights" {1 5000}
                      "engine_utilization" {"saturation" 30
                                            "last_request_tokens" 30000
                                            "auto_compress_above" 60000
                                            "model_input_limit" 100000}}))]
      ;; The #88 shape: 3 enumerated iteration ids, 2 of them weightless.
      (expect
        (= (str "folded t1/i1, t1/i2, t2/i1 · saved ~8k tokens · ~13% of budget"
                " · context 30%→~22% (30k→22k tokens)"
                " · 2/3 scopes already off-wire — fold t1 to drop their recaps → enumerated")
           ((get (compaction-verbs (mk)) 'fold-session) ["t1/i1" "t1/i2" "t2/i1"] "enumerated")))
      ;; A fold that frees nothing at all still names the shape that would.
      (expect (= (str "folded t1/i1, t1/i2 · saved ~0 tokens · context 30% (30k/100k tokens)"
                      " · 2/2 scopes already off-wire — fold t1 to drop their recaps → nothing")
                 ((get (compaction-verbs (mk)) 'fold-session) ["t1/i1" "t1/i2"] "nothing")))
      ;; The whole-turn shape DOES charge and remove the recap, so it needs no nudge.
      (expect (= (str "folded t1 · saved ~5k tokens · ~8% of budget"
                      " · context 30%→~25% (30k→25k tokens) → whole turn")
                 ((get (compaction-verbs (mk)) 'fold-session) ["t1"] "whole turn")))
      ;; A `through` selector that fully covers a turn is promoted to that turn
      ;; (recap charged), so it is already the reclaiming shape — also no nudge.
      (expect (= (str "folded through t2/i1 · saved ~13k tokens · ~22% of budget"
                      " · context 30%→~17% (30k→17k tokens) → spanned")
                 ((get (compaction-verbs (mk)) 'fold-session) "-t2/i1" "spanned")))))
  (it
    "a later, bigger request can't inflate the card — the reduction is the fold's own"
    ;; The scary regression (fold → tool call → fold → % climbs): a projected
    ;; level subtracts cumulative-saved from the GROWING `last_request_tokens`, so
    ;; the second card would RISE. The per-fold reduction is immune — it prices
    ;; only the scope THIS fold reclaims, never the live request size.
    (let [ca
          (priced-ctx)

          sf
          (get (compaction-verbs ca) 'fold-session)

          card1
          (sf ["t1/i1"] "first")

          ;; one iteration passes: a big tool result lands, the request grows,
          ;; and t1/i1 is now collapsed on the wire so its weight drops to 0.
          _
          (swap! ca assoc
            "engine_iter_weights" {"t1/i1" 0 "t1/i2" 3400 "t1/i3" 900 "t2/i1" 500}
            "engine_utilization"
            {"last_request_tokens" 90000 "auto_compress_above" 70000 "model_input_limit" 96000})

          card2
          (sf ["t1/i2"] "second")]

      (expect
        (=
          "folded t1/i1 · saved ~12k tokens · ~17% of budget · context 44%→~31% (42k→30k tokens) → first"
          card1))
      ;; second fold reclaims only its own 3.4k regardless of the 90k request
      (expect (= "folded t1/i2 · saved ~3k tokens · ~4% of budget → second" card2))))
  (it
    "a bigger task prices against the GROWN operating ceiling (handled context), not the fixed budget"
    ;; auto-compress is a SOFT guardrail: `last_request_tokens` can float above it
    ;; before compaction fires. The `% of budget` denominator is
    ;; max(auto_compress_above, last_request_tokens), so it grows with the real
    ;; handled context and the fraction never overflows a ceiling already breached.
    (let [ca
          (priced-ctx)

          _
          (swap! ca assoc
            "engine_iter_weights" {"t1/i1" 60000 "t1/i2" 3400 "t1/i3" 900 "t2/i1" 500}
            "engine_utilization" {"saturation" 94
                                  "last_request_tokens" 90000
                                  "auto_compress_above" 70000
                                  "model_input_limit" 96000})

          sf
          (get (compaction-verbs ca) 'fold-session)]

      ;; ceiling = max(70000, 90000) = 90000; 60000/90000 = 67%
      (expect
        (=
          "folded t1/i1 · saved ~60k tokens · ~67% of budget · context 94%→~31% (90k→30k tokens) → bigger task"
          (sf ["t1/i1"] "bigger task")))))
  (it
    "the note ALSO lands in the persistent breadcrumb, not just the tool card"
    ;; regression: the saved-tokens + projected suffix must ride the durable
    ;; `# ⋯ folded …` label the human reads on scroll-back, NOT only the
    ;; transient tool-return confirmation.
    (let [ctx
          (priced-ctx)

          sf
          (get (compaction-verbs ctx) 'fold-session)

          _
          (sf ["t1/i1"] "big cat dump")

          trailer
          [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]

          out
          (apply-summaries trailer (get @ctx "session_summaries"))

          line
          (:content (irm (second (first out))))]

      (expect
        (=
          "# ⋯ folded t1/i1 · saved ~12k tokens · ~17% of budget · context 44%→~31% (42k→30k tokens) · big cat dump"
          line))))
  (it "a fold breadcrumb carries no recovery coordinate"
      ;; Regression guard for the `ntr` removal: `python_execution` PRINTS, so
      ;; nothing stores a result the breadcrumb could point at. A pointer here
      ;; would promise a recovery path that cannot exist.
      (let [tr
            [[1
              {:forms-vec [{:scope "t1/i1/f1"
                            :svar/tool-call-id "toolu_A"
                            :result "a"
                            :result-summary "2 files"}]}]
             [2 {:forms-vec [{:scope "t1/i2/f1" :svar/tool-call-id "toolu_B" :result "b"}]}]]

            folded
            (apply-summaries tr [{"scopes" #{"t1/i1" "t1/i2"} "at_turn" 1 "gist" "did it"}])

            dropped
            (apply-summaries tr [{"scopes" #{"t1/i1"} "at_turn" 1 "note" " · misread"}])

            line
            (:content (irm (second (first folded))))]

        (expect (= "# ⋯ folded t1/i1-i2 · did it" line))
        (expect (not (str/includes? line "ntr")))
        (expect (not (str/includes? line "# saved:")))
        (expect (= "# ⋯ dropped t1/i1 · misread" (:content (irm (second (first dropped))))))))
  (it "with NO stamped utilization the card degrades to the bare confirmation"
      (let [sf (get (compaction-verbs (atom {"session_turn" 2})) 'fold-session)]
        (expect (= "folded t1/i1 → g" (sf ["t1/i1"] "g")))))
  (it "a fold receipt carries no recovery pointer"
      ;; The receipt says what was reclaimed and what the gist was — never how to
      ;; re-read a folded result, because folding changes rendering, not storage,
      ;; and there is no per-result accessor left to name.
      (let [sf
            (get (compaction-verbs (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1"]}))
                 'fold-session)

            out
            (sf ["t1/i1"] "g")]

        (expect (= "folded t1/i1 → g" out))
        (expect (not (str/includes? out "ntr")))
        (expect (not (str/includes? out "more results"))))))
