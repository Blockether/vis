(ns com.blockether.vis.internal.compaction-verbs-test
  "Compaction/`session_fold` coverage at three layers:
     1. Raw-Python integration — drive `session_fold` THROUGH the GraalPy sandbox
        so the real argument marshalling (Python list/dict → `->clj`) and the
        visible return string are exercised end to end, not just reasoned about.
     2. Selector resolution — `expand-through` against a live iteration universe:
        every selector form (explicit list, bare-turn, `through`/`from`/`to`/
        `since`) plus the boundaries (cursor past the ends, empty windows,
        unknown scopes) where a fold quietly folds nothing.
     3. What the LLM actually SEES — `apply-summaries` over a trailer: which
        iterations collapse off the wire, where the single gist breadcrumb lands,
        and that a broader re-fold supersedes a finer one.
     4. Native tool_use surface — the injected `session_fold` call-shape
        synthesizes the SAME positional `session_fold(target, gist)` into the
        bound verb, so both surfaces share one definition.
     5. Session-bag reflection — a landed fold surfaces INSIDE `session_utilization`
        as two string leaves: `folds` (stable gists, one structural delta per fold)
        and `now` (volatile position + budget + live, re-emitted each iteration),
        via `ctx-engine/folds-view` → `ctx-renderer/render-ctx-delta`."
  (:require [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.ctx-renderer :as cr]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.loop :as lp]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private compaction-verbs (var-get #'lp/compaction-verbs))
(def ^:private apply-summaries (var-get #'lp/apply-summaries))
(def ^:private expand-through (var-get #'eng/expand-through))
(def ^:private session-fold-tool (var-get #'lp/session-fold-tool))
(def ^:private tool-call->python-source (var-get #'lp/tool-call->python-source))
(def ^:private irm (var-get #'lp/iteration-results-message))

(defn- with-verbs
  "Fresh ctx-atom + a GraalPy context with session_fold bound.
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
  (it "session_fold(list, gist): records a \"scopes\" intent + returns a visible confirmation"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"explored auth\")")]

        (expect (= [{"scopes" #{"t1/i2" "t1/i3"} "gist" "explored auth"}]
                   (get @ca "session_summaries")))
        (expect (re-find #"^folded " out))
        (expect (re-find #"explored auth" out))))
  (it "session_fold({\"through\": …}): the options DICT marshals to a \"through\" cursor"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold({\"through\": \"t1/i5\"}, \"early reads\")")]

        (expect (= [{"through" "t1/i5" "gist" "early reads"}] (get @ca "session_summaries")))
        (expect (re-find #"through t1/i5" out))))
  (it "session_fold({\"from\": …, \"to\": …}): a WINDOW dict marshals to from/to keys"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold({\"from\": \"t1/i2\", \"to\": \"t1/i4\"}, \"middle\")")]

        (expect (= [{"from" "t1/i2" "to" "t1/i4" "gist" "middle"}] (get @ca "session_summaries")))
        (expect (re-find #"window t1/i2\.\.t1/i4" out))))
  (it "session_fold({\"since\": …}): a SINCE dict marshals to a since cursor"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold({\"since\": \"t2/i1\"})")]

        (expect (= [{"since" "t2/i1"}] (get @ca "session_summaries")))
        (expect (re-find #"^folded since t2/i1" out))))
  (it
    "session_fold({\"since\": …}) FREEZES to concrete scopes at fold time when a universe exists — no rolling swallow of future work"
    (let [[ca ev]
          (with-verbs)

          _
          (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3"])

          out
          (ev "session_fold({\"since\": \"t1/i2\"}, \"tail\")")

          [intent]
          (get @ca "session_summaries")]

      ;; frozen to the scopes present NOW — the raw `since` selector is gone
      (expect (= {"scopes" #{"t1/i2" "t1/i3"} "gist" "tail"} intent))
      (expect (not (contains? intent "since")))
      (expect (re-find #"^folded since t1/i2" out))
      ;; and because it's frozen, a LATER-grown universe can't swallow new iters
      (expect (= #{"t1/i2" "t1/i3"}
                 (get (first (expand-through [intent] ["t1/i1" "t1/i2" "t1/i3" "t1/i4" "t1/i5"]))
                      "scopes")))))
  (it "session_fold({\"from\": …}) with NO \"to\" also freezes its open ceiling at fold time"
      (let [[ca ev]
            (with-verbs)

            _
            (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1"])

            _
            (ev "session_fold({\"from\": \"t1/i2\"}, \"open\")")

            [intent]
            (get @ca "session_summaries")]

        (expect (= {"scopes" #{"t1/i2" "t2/i1"} "gist" "open"} intent))
        (expect (not (contains? intent "from")))
        (expect (= #{"t1/i2" "t2/i1"}
                   (get (first (expand-through [intent] ["t1/i1" "t1/i2" "t2/i1" "t2/i2"]))
                        "scopes")))))
  (it
    "bounded selectors (through / from+to) stay RAW even with a universe — their ceiling already blocks new scopes"
    (let [[ca ev]
          (with-verbs)

          _
          (swap! ca assoc "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3"])

          _
          (ev "session_fold({\"through\": \"t1/i2\"}, \"early\")")]

      (expect (= [{"through" "t1/i2" "gist" "early"}] (get @ca "session_summaries")))))
  (it "session_fold([\"t2\"]): a bare turn id records as a whole-turn scope token"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold([\"t2\"], \"whole turn 2\")")]

        (expect (= [{"scopes" #{"t2"} "gist" "whole turn 2"}] (get @ca "session_summaries")))
        (expect (re-find #"^folded t2 " out))))
  (it "session_fold WITHOUT a gist records a gist-less collapse (replaces session_drop)"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold([\"t1/i1\"])")]

        (expect (= [{"scopes" #{"t1/i1"}}] (get @ca "session_summaries")))
        (expect (re-find #"^folded t1/i1" out))))
  (it "an empty/blank target is a no-op: records nothing, returns a hint"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold([])")]

        (expect (nil? (get @ca "session_summaries")))
        (expect (re-find #"nothing to fold" out))))
  (it "an options dict with NO recognized selector key is a no-op hint"
      (let [[ca ev]
            (with-verbs)

            out
            (ev "session_fold({\"bogus\": \"t1/i1\"})")]

        (expect (nil? (get @ca "session_summaries")))
        (expect (re-find #"nothing to fold" out))))
  (it "rejects current and future turns before recording any fold"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'session-fold)]

        (doseq [target [["t2/i1"] ["t2"] {"through" "t2/i1"} ["t3/i1"]]]
          (let [ex (try (sf target "unsafe") nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (= :vis/session-fold-active-turn (:type (ex-data ex))))
            (expect (= 2 (:current-turn (ex-data ex))))))
        (expect (nil? (get @ca "session_summaries")))))
  (it "allows completed prior turns after the new turn has started"
      (let [ca
            (atom {"session_turn" 2 "engine_iter_universe" ["t1/i1" "t1/i2" "t2/i1"]})

            sf
            (get (compaction-verbs ca) 'session-fold)]

        (expect (re-find #"^folded t1" (sf ["t1"] "done")))
        (expect (= [{"scopes" #{"t1"} "gist" "done"}] (get @ca "session_summaries")))))
  (it "fails closed when the current turn is unavailable"
      (let [sf
            (get (compaction-verbs (atom {})) 'session-fold)

            ex
            (try (sf ["t1/i1"] "unknown") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :vis/session-fold-turn-unknown (:type (ex-data ex)))))))

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
             (it "an intent with NO selector key passes through untouched"
                 (expect (= [{"drop" true}] (expand-through [{"drop" true}] universe)))))

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
            (apply-summaries tr [{"through" "t1/i2" "gist" "G"}])

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
            (apply-summaries tr [{"scopes" #{"t1"} "gist" "all of t1"}])]

        (expect (every? (fn [[_ r]]
                          (:collapsed? r))
                        out))
        (expect (= 1 (count (summary-forms out))))))
  (it "a from/to window collapses only the inclusive middle"
      (let [tr
            (trailer "t1/i1" "t1/i2" "t1/i3" "t1/i4")

            out
            (apply-summaries tr [{"from" "t1/i2" "to" "t1/i3" "gist" "mid"}])]

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
            (apply-summaries tr [{"since" "t1/i2" "gist" "tail"}])

            [[_ r1] [_ r2] [_ r3]]
            out]

        (expect (not (:collapsed? r1)))
        (expect (:collapsed? r2))
        (expect (:collapsed? r3))))
  (it "a fold whose scopes miss the trailer entirely collapses nothing"
      (let [tr
            (trailer "t1/i1" "t1/i2")

            out
            (apply-summaries tr [{"through" "t0/i0" "gist" "nope"}])]

        (expect (not-any? (fn [[_ r]]
                            (:collapsed? r))
                          out))
        (expect (empty? (summary-forms out)))))
  (it "empty summaries leave the trailer byte-for-byte"
      (let [tr (trailer "t1/i1" "t1/i2")]
        (expect (= (vec tr) (apply-summaries tr [])))))
  (it "a broader re-fold SUPERSEDES the finer one (one breadcrumb, broader gist)"
      (let [[ca ev] (with-verbs)]
        (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"A\")")
        (ev "session_fold([\"t1/i2\", \"t1/i3\", \"t1/i4\"], \"B\")")
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
        (ev "session_fold([\"t1/i2\"], \"finer\")")
        (ev "session_fold({\"through\": \"t1/i3\"}, \"broad\")")
        (let [tr (trailer "t1/i1" "t1/i2" "t1/i3")
              out (apply-summaries tr (get @ca "session_summaries"))
              sfs (summary-forms out)]

          (expect (= 1 (count sfs)))
          (expect (= "broad" (:summary-gist (first sfs)))))))
  (it "FOLD OF FOLD: one whole-turn re-fold swallows TWO finer folds into one breadcrumb"
      (let [[ca ev] (with-verbs)]
        ;; two disjoint finer folds recorded first…
        (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"fold A\")")
        (ev "session_fold([\"t1/i5\"], \"fold B\")")
        ;; …then the whole turn is re-folded — a fold OF those folds.
        (ev "session_fold([\"t1\"], \"meta: the whole turn\")")
        (let [tr (trailer "t1/i1" "t1/i2" "t1/i3" "t1/i4" "t1/i5")
              out (apply-summaries tr (get @ca "session_summaries"))
              sfs (summary-forms out)]

          ;; both finer breadcrumbs are superseded — only the meta gist survives,
          ;; and every iteration of the turn collapses off the wire.
          (expect (= 1 (count sfs)))
          (expect (= "meta: the whole turn" (:summary-gist (first sfs))))
          (expect (every? (fn [[_ r]]
                            (:collapsed? r))
                          out))))))

;; ── layer 4: native tool_use surface ─────────────────────────────────────────

(def ^:private native-shapes {"session_fold" {:pos ["target"] :opt-pos ["gist"]}})

(defdescribe
  session-fold-native-tool-test
  (it "the native schema advertises session_fold with a target property"
      (let [t (session-fold-tool)]
        (expect (= "session_fold" (:name t)))
        (expect (string? (:description t)))
        (expect (contains? (:properties (:schema t)) "target"))
        (expect (= ["target"] (:required (:schema t))))))
  (it "native dispatch synthesizes a POSITIONAL call for a list target + gist"
      (expect (= "session_fold([\"t1/i2\", \"t1/i3\"], \"G\")"
                 (tool-call->python-source native-shapes
                                           {:name "session_fold"
                                            :input {"target" ["t1/i2" "t1/i3"] "gist" "G"}}))))
  (it "native dispatch synthesizes a DICT selector target, gist omitted"
      (expect (= "session_fold({\"through\": \"t1/i2\"})"
                 (tool-call->python-source native-shapes
                                           {:name "session_fold"
                                            :input {"target" {"through" "t1/i2"}}}))))
  (it "the synthesized native source runs the SAME bound verb (records the intent)"
      (let [[ca ev]
            (with-verbs)

            src
            (tool-call->python-source native-shapes
                                      {:name "session_fold"
                                       :input {"target" ["t2/i4"] "gist" "native"}})]

        (ev src)
        (expect (= [{"scopes" #{"t2/i4"} "gist" "native"}] (get @ca "session_summaries"))))))

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
  session-fold-ctx-reflection-test
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
  (it "session-view merges only `now` INTO session_utilization — no top-level key, no `folds` leaf"
      (expect (not (contains? (eng/session-view base-ctx) "session_folds")))
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

;; ── layer 6: the human-facing fold CARD (tokens saved + context level) ───────

(defn- priced-ctx
  "ctx-atom pre-stamped as a live send would be: an iteration universe, the
   per-scope ~token weights (`stamp-iter-universe!`), and the provider-measured
   utilization — everything the `session_fold` card prices its suffix from."
  []
  (atom {"session_turn" 3
         "engine_iter_universe" ["t1/i1" "t1/i2" "t1/i3" "t2/i1"]
         "engine_iter_weights" {"t1/i1" 12000 "t1/i2" 3400 "t1/i3" 900 "t2/i1" 500}
         "engine_utilization"
         {"saturation" 44 "last_request_tokens" 42000 "model_input_limit" 96000}}))

(defdescribe
  session-fold-card-test
  ;; The verb RETURN string is the tool card the human sees. It is enriched with
  ;; how much wire the fold reclaims — in ~tokens (summed from `engine_iter_weights`)
  ;; AND as a fraction of `model_input_limit`. This is the fold's OWN reduction,
  ;; NOT an absolute level: a projected level baselines on the growing
  ;; `last_request_tokens`, so it RISES across iterations even when the fold helped
  ;; (issue #27's scary regression). A per-fold reduction can never mislead that way.
  ;; Alongside it the card ALSO surfaces the live window fullness as `context <U>%`,
  ;; taken straight from the provider's authoritative `saturation` — a separate,
  ;; absolute reading, omitted when no `saturation` is stamped.
  (it "an explicit scope card prices its ~tokens + the reduction as % of window"
      (let [sf (get (compaction-verbs (priced-ctx)) 'session-fold)]
        (expect (= "folded t1/i1 · saved ~12k tokens · ~13% of window · context 44% → big cat dump"
                   (sf ["t1/i1"] "big cat dump")))))
  (it "a `through` selector sums the weight of EVERY scope it resolves"
      (let [sf (get (compaction-verbs (priced-ctx)) 'session-fold)]
        ;; through t1/i2 folds t1/i1 (12k) + t1/i2 (3.4k) = ~15k
        (expect (=
                  "folded through t1/i2 · saved ~15k tokens · ~16% of window · context 44% → traced"
                  (sf {"through" "t1/i2"} "traced")))))
  (it "a gist-less fold still shows the tokens + reduction suffix"
      (let [sf (get (compaction-verbs (priced-ctx)) 'session-fold)]
        (expect (= "folded t1/i1 · saved ~12k tokens · ~13% of window · context 44%"
                   (sf ["t1/i1"])))))
  (it "a scope with NO stamped weight reclaims nothing, so the card omits the suffix"
      (let [sf (get (compaction-verbs (priced-ctx)) 'session-fold)]
        ;; t2/i9 is not in the weights map (created this iteration, unsent) — a fold
        ;; that frees no wire honestly shows no savings, not a phantom level.
        (expect (= "folded t2/i9 → fresh" (sf ["t2/i9"] "fresh")))))
  (it "a later, bigger request can't inflate the card — the reduction is the fold's own"
      ;; The scary regression (fold → tool call → fold → % climbs): a projected
      ;; level subtracts cumulative-saved from the GROWING `last_request_tokens`, so
      ;; the second card would RISE. The per-fold reduction is immune — it prices
      ;; only the scope THIS fold reclaims, never the live request size.
      (let [ca
            (priced-ctx)

            sf
            (get (compaction-verbs ca) 'session-fold)

            card1
            (sf ["t1/i1"] "first")

            ;; one iteration passes: a big tool result lands, the request grows,
            ;; and t1/i1 is now collapsed on the wire so its weight drops to 0.
            _
            (swap! ca assoc
              "engine_iter_weights" {"t1/i1" 0 "t1/i2" 3400 "t1/i3" 900 "t2/i1" 500}
              "engine_utilization" {"last_request_tokens" 90000 "model_input_limit" 96000})

            card2
            (sf ["t1/i2"] "second")]

        (expect (= "folded t1/i1 · saved ~12k tokens · ~13% of window · context 44% → first" card1))
        ;; second fold reclaims only its own 3.4k regardless of the 90k request
        (expect (= "folded t1/i2 · saved ~3k tokens · ~4% of window → second" card2))))
  (it "the note ALSO lands in the persistent breadcrumb, not just the tool card"
      ;; regression: the saved-tokens + projected suffix must ride the durable
      ;; `# ⋯ folded …` label the human reads on scroll-back, NOT only the
      ;; transient tool-return confirmation.
      (let [ctx
            (priced-ctx)

            sf
            (get (compaction-verbs ctx) 'session-fold)

            _
            (sf ["t1/i1"] "big cat dump")

            trailer
            [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]

            out
            (apply-summaries trailer (get @ctx "session_summaries"))

            line
            (:content (irm (second (first out))))]

        (expect
          (= "# ⋯ folded t1/i1 · saved ~12k tokens · ~13% of window · context 44% · big cat dump"
             line))))
  (it "with NO stamped utilization the card degrades to the bare confirmation"
      (let [sf (get (compaction-verbs (atom {"session_turn" 2})) 'session-fold)]
        (expect (= "folded t1/i1 → g" (sf ["t1/i1"] "g"))))))
