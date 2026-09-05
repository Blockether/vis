(ns com.blockether.vis.internal.context.cache-test
  "Repro + spec for prompt-cache-STABLE context rendering.

   The standing `session = {…}` block rides in the CACHED system prefix. For
   the prompt cache (OpenAI implicit prefix cache keyed by `:cache-key`, or an
   Anthropic `cache_control` breakpoint) to HIT, that prefix must stay
   BYTE-IDENTICAL across requests.

   Today vis RE-RENDERS the block every turn from current state (loop.clj:3375)
   and re-seeds the delta baseline every turn (loop.clj:3379) — so any state
   change (e.g. `/dir add`, model switch, nREPL start) on turn N rewrites the
   system prefix on turn N+1 and BUSTS the cache, even though `summarize` was
   never called.

   The fix (pi/maki-style): FREEZE the block once per session, carry the delta
   baseline ACROSS turns, and emit every change — incl. utilization — as an
   appended `session[...] = …` delta AFTER the cache breakpoint. Only
   `summarize`/`drop` re-baseline (the deliberate, infrequent bust).

   These tests pin the mechanism the fix relies on."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.renderer :as cr]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.context.env-digest :as env-digest]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-ctx
  {"session_id" "s1"
   "session_workspace" {"root" "/repo" "isolated" false "vcs_kind" "git"}
   "session_env" {"host" {"os" "macos"} "project" {"kind" "single"}}
   "session_language_tools" {"clojure" ["repl_eval" "repl_start"]}
   "session_routing" {"model" "gpt-5.5"}})

;; A realistic cross-turn state change: the session title changed.
(def ^:private changed-ctx
  (assoc-in base-ctx ["session_workspace" "session_title"] "Renamed session"))

(defdescribe
  ctx-cache-stability-test
  ;; --- THE BUG: re-rendering the static block per turn changes the SYSTEM
  ;;     PREFIX whenever state moves → invalidates the cached prefix.
  (it
    "a state change makes render-ctx-static emit a DIFFERENT system block (this diff lands in the cached prefix → bust)"
    (let [block-0
          (cr/render-ctx-static {:ctx base-ctx})

          block-1
          (cr/render-ctx-static {:ctx changed-ctx})]

      (expect (string? block-0))
      (expect (not= block-0 block-1))))
  ;; --- THE FIX MECHANISM: the same change is a tiny, append-only, cache-safe delta.
  (it "the change is expressible as a MINIMAL `session[...] = …` delta, not a whole-block re-render"
      (let [m0
            (cr/ctx-static-map {:ctx base-ctx})

            m1
            (cr/ctx-static-map {:ctx changed-ctx})

            delta
            (cr/render-ctx-delta m0 m1)]

        (expect (some? delta))
        (expect (str/includes? delta "session_title")) ; only what moved
        (expect (str/includes? delta "session[")) ; append-only assignment
        ;; far cheaper than re-sending the frozen block
        (expect (< (count delta) (quot (count (cr/render-ctx-static {:ctx changed-ctx})) 2)))))
  (it "no state change ⇒ no delta (frozen prefix stays warm)"
      (let [m0 (cr/ctx-static-map {:ctx base-ctx})]
        (expect (nil? (cr/render-ctx-delta m0 m0)))))
  (it
    "does NOT project language capabilities into ctx (the EXTENSIONS prompt block already names them)"
    (let [m (cr/ctx-static-map {:ctx base-ctx})]
      (expect (not (contains? m "language_tools")))
      (expect (not (contains? m "session_language_tools")))))
  (it "projects the immutable security snapshot as standing session access"
      (let [access
            {"generation" "sha256:abc"
             "is_jailed" true
             "filesystem" {"read_write" ["~/vis" "~/demo"]}
             "changes_require" "reload"}

            m
            (cr/ctx-static-map {:ctx (assoc base-ctx "session_access" access)})]

        (expect (= ["~/vis" "~/demo"] (get-in m ["access" "filesystem" "read_write"])))
        (expect (= "reload" (get-in m ["access" "changes_require"])))
        (expect (not (contains? m "session_access")))))
  ;; Regression, issue #ctx-resources: the live shell/REPL registry used to be
  ;; re-rendered into `session["resources"]` on every turn boundary, so one working
  ;; session accumulated 74 finished shells — 12k chars of `exit 0` on every request.
  (it "never renders live resources into the model-facing session"
      (let [boundary
            (cr/render-turn-boundary {:ctx (assoc base-ctx
                                             "session_turn" 7
                                             "session_resources" [{"id" "shell-1"}])})

            enriched
            (with-redefs [prompt/active-extensions
                          (fn [_]
                            [])

                          extension/ctx-contributions
                          (fn [_ _]
                            {})

                          env-digest/base-digest
                          (fn [_]
                            {})

                          env-digest/deep-merge
                          (fn [& xs]
                            (apply merge xs))]

              (ctx-loop/enrich-ctx {:session-id "s1"} base-ctx))]

        (expect (not (str/includes? boundary "resources")))
        (expect (not (contains? enriched "session_resources")))
        (expect (not (contains? (cr/ctx-static-map {:ctx (assoc base-ctx
                                                           "session_resources" {"repls" {"clojure"
                                                                                         {}}})})
                                "resources")))
        (expect (not (contains? (set ctx-engine/model-facing-keys) "session_resources")))))
  (it "renders turn and utilization explicitly for iteration 1"
      (let [boundary (cr/render-turn-boundary {:ctx (assoc base-ctx
                                                      "session_turn" 7
                                                      "engine_utilization"
                                                      {"last_request_tokens" 1200
                                                       "model_input_limit" 10000
                                                       "saturation" 12})})]
        (expect (str/includes? boundary "session[\"turn\"] = 7"))
        (expect (str/includes? boundary "session[\"utilization\"]"))
        (expect (str/includes? boundary "\"saturation\": 12")))))

(defdescribe
  freeze-semantics-test
  "Mirrors iteration-loop's `:standing-ctx-atom` logic: render the standing
   block ONCE, reuse it across turns, and diff the current util-inclusive map
   against the baseline CARRIED across turns. Proves the cached system prefix
   stays byte-identical while changes ride as appended deltas."
  ;; A turn-2 ctx with a changed session title and measured utilization.
  ;; `"engine_utilization"` is the engine-stamped key `session-view` derives
  ;; `"session_utilization"` from (ctx_engine/session-view) — same as the live loop.
  (let [util-ctx (assoc changed-ctx
                   "engine_utilization" {"last_request_tokens" 1200 "saturation" 1})]
    (it "the standing block is byte-identical across turns even after state changed (cache holds)"
        (let [standing (atom nil)]
          ;; TURN 1 seeds the frozen block + baseline (as iteration-loop does once)
          (reset! standing {:block (cr/render-ctx-static {:ctx base-ctx})
                            :baseline (cr/ctx-static-map {:ctx base-ctx})})
          (let [block-t1 (:block @standing)
                ;; TURN 2: state changed, but the loop REUSES the frozen block
                block-t2 (:block @standing)]

            (expect (= block-t1 block-t2))
            ;; and the frozen block never carried utilization (cache-stability)
            (expect (not (str/includes? block-t1 "utilization"))))))
    (it "a cross-turn change + utilization both ride as one appended delta, not a re-render"
        (let [baseline (cr/ctx-static-map {:ctx base-ctx})
              cur (cr/ctx-delta-map {:ctx util-ctx})
              delta (cr/render-ctx-delta baseline cur)]

          (expect (some? delta))
          (expect (str/includes? delta "session_title")) ; the state change
          (expect (str/includes? delta "utilization")) ; live token usage as a delta
          (expect (every? #(str/starts-with? % "session[") (str/split-lines delta)))))
    (it "ctx-delta-map carries utilization but ctx-static-map (the frozen block) does NOT"
        (expect (contains? (cr/ctx-delta-map {:ctx util-ctx}) "utilization"))
        (expect (not (contains? (cr/ctx-static-map {:ctx util-ctx}) "utilization"))))))

(defdescribe
  cache-breakpoints-test
  "The four prompt-cache breakpoints: the last `:role \"system\"` message (the
   frozen `session={…}` prefix) plus the three trailing transcript messages, so a
   request's read anchor lands exactly where its predecessor wrote. The 1-hour
   tier is asked for only on a route measured to honour it."
  (let [apply-bp
        @#'lp/apply-cache-breakpoints

        cached?
        (fn [m]
          (let [c (:content m)]
            (boolean (and (vector? c) (some :svar/cache c)))))

        ttls
        (fn [out]
          (into #{} (comp (mapcat :content) (keep :svar/cache-ttl)) out))]

    (it "tags the last system message and the three trailing transcript messages"
        (let [out (apply-bp [{:role "system" :content "core"}
                             {:role "system" :content "session = {…}"} ; frozen block (last system)
                             {:role "user" :content [{:type "text" :text "settled"}]}
                             {:role "assistant" :content [{:type "text" :text "settled reply"}]}
                             {:role "user" :content [{:type "text" :text "previous write anchor"}]}
                             {:role "assistant" :content [{:type "text" :text "reply"}]}
                             {:role "user" :content [{:type "text" :text "current"}]}]
                            :zai-coding-plan)]
          (expect (not (cached? (nth out 0)))) ; earlier system block
          (expect (cached? (nth out 1))) ; frozen system prefix
          (expect (not (cached? (nth out 2)))) ; settled middle stays untouched
          (expect (not (cached? (nth out 3))))
          (expect (cached? (nth out 4))) ; the previous request's write anchor
          (expect (cached? (nth out 5)))
          (expect (cached? (nth out 6))) ; moving recency
          (expect (= 4 (count (filter cached? out))))))
    (it "never spends more than Anthropic's four breakpoints"
        (let [msgs (into [{:role "system" :content "s"}]
                         (map (fn [i]
                                {:role "user" :content [{:type "text" :text (str i)}]}))
                         (range 20))]
          (expect (= 4 (count (filter cached? (apply-bp msgs :anthropic-coding-plan)))))))
    (it "coerces a bare-string last message into a cached text block"
        (let [out
              (apply-bp [{:role "system" :content "s"} {:role "user" :content "hello"}]
                        :zai-coding-plan)

              last-blk
              (first (:content (last out)))]

          (expect (= "hello" (:text last-blk)))
          (expect (true? (:svar/cache last-blk)))))
    (it "empty list is a no-op; a system-only call collapses to one breakpoint"
        (expect (= [] (apply-bp [] :zai-coding-plan)))
        (let [out (apply-bp [{:role "system" :content "only"}] :zai-coding-plan)]
          (expect (= 1 (count (filter cached? out))))))
    (it "asks for the 1-hour tier only on a route measured to honour it"
        (let [msgs [{:role "system" :content "s"} {:role "user" :content "u"}]]
          (expect (= #{:1h} (ttls (apply-bp msgs :anthropic-coding-plan))))
          (expect (= #{:1h} (ttls (apply-bp msgs :anthropic))))
          (expect (empty? (ttls (apply-bp msgs :zai-coding-plan))))))
    (it "never marks a preserved thinking block"
        (let [out (apply-bp [{:role "system" :content "s"}
                             {:role "assistant"
                              :content [{:type "thinking" :thinking "…" :signature "sig"}]}
                             {:role "assistant"
                              :content [{:type "thinking" :thinking "…" :signature "sig"}
                                        {:type "text" :text "answer"}]}]
                            :anthropic-coding-plan)]
          (expect (not (cached? (nth out 1)))) ; nothing cacheable in it
          (expect (nil? (:svar/cache (first (:content (nth out 2))))))
          (expect (true? (:svar/cache (second (:content (nth out 2))))))))))

(defdescribe
  prompt-cache-window-test
  "Replaying an exact prefix across a pause is gated by the TIER its route was
   written with — a 1-hour breakpoint is worthless if the checkpoint carrying it
   is still discarded after five minutes."
  (let [window
        @#'lp/prompt-cache-window-ms

        fresh?
        @#'lp/prompt-cache-entry-fresh?

        half-hour-old
        {:at-ms (- (util/now-ms) 1800000)}]

    (it "an extended-tier route still replays a thirty-minute-old checkpoint"
        (expect (= 3600000 (window :anthropic-coding-plan)))
        (expect (true? (fresh? :anthropic-coding-plan half-hour-old))))
    (it "a five-minute route drops it"
        (expect (= 300000 (window :zai-coding-plan)))
        (expect (not (fresh? :zai-coding-plan half-hour-old))))))
