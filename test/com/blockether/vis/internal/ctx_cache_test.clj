(ns com.blockether.vis.internal.ctx-cache-test
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
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-renderer :as cr]
   [com.blockether.vis.internal.loop :as lp]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-ctx
  {:session/id        "s1"
   :session/workspace {:workspace/root "/repo" :workspace/sandbox? false :vcs/kind :git}
   :session/env       {:host {:os "macos"} :project {:kind "single"}}
   :session/routing   {:model "gpt-5.5"}})

;; A realistic cross-turn state change: the user added a filesystem root.
(def ^:private changed-ctx
  (assoc-in base-ctx [:session/workspace :workspace/filesystem-roots] [{:dir "/repo/src"}]))

(defdescribe ctx-cache-stability-test
  ;; --- THE BUG: re-rendering the static block per turn changes the SYSTEM
  ;;     PREFIX whenever state moves → invalidates the cached prefix.
  (it "a state change makes render-ctx-static emit a DIFFERENT system block (this diff lands in the cached prefix → bust)"
    (let [block-0 (cr/render-ctx-static {:ctx base-ctx})
          block-1 (cr/render-ctx-static {:ctx changed-ctx})]
      (expect (string? block-0))
      (expect (not= block-0 block-1))))

  ;; --- THE FIX MECHANISM: the same change is a tiny, append-only, cache-safe delta.
  (it "the change is expressible as a MINIMAL `session[...] = …` delta, not a whole-block re-render"
    (let [m0    (cr/ctx-static-map {:ctx base-ctx})
          m1    (cr/ctx-static-map {:ctx changed-ctx})
          delta (cr/render-ctx-delta m0 m1)]
      (expect (some? delta))
      (expect (str/includes? delta "filesystem_roots"))   ; only what moved
      (expect (str/includes? delta "session["))          ; append-only assignment
      ;; far cheaper than re-sending the frozen block
      (expect (< (count delta)
                (quot (count (cr/render-ctx-static {:ctx changed-ctx})) 2)))))

  (it "no state change ⇒ no delta (frozen prefix stays warm)"
    (let [m0 (cr/ctx-static-map {:ctx base-ctx})]
      (expect (nil? (cr/render-ctx-delta m0 m0))))))

(defdescribe freeze-semantics-test
  "Mirrors iteration-loop's `:standing-ctx-atom` logic: render the standing
   block ONCE, reuse it across turns, and diff the current util-inclusive map
   against the baseline CARRIED across turns. Proves the cached system prefix
   stays byte-identical while changes ride as appended deltas."
  ;; A turn-2 ctx that BOTH gained a filesystem root AND measured utilization.
  ;; `:engine/utilization` is the engine-stamped key `session-view` derives
  ;; `:session/utilization` from (ctx_engine/session-view) — same as the live loop.
  (let [util-ctx (assoc changed-ctx :engine/utilization
                   {:last-request-tokens 1200 :saturation 1})]
    (it "the standing block is byte-identical across turns even after state changed (cache holds)"
      (let [standing (atom nil)]
        ;; TURN 1 seeds the frozen block + baseline (as iteration-loop does once)
        (reset! standing {:block    (cr/render-ctx-static {:ctx base-ctx})
                          :baseline (cr/ctx-static-map {:ctx base-ctx})})
        (let [block-t1 (:block @standing)
              ;; TURN 2: state changed, but the loop REUSES the frozen block
              block-t2 (:block @standing)]
          (expect (= block-t1 block-t2))
          ;; and the frozen block never carried utilization (cache-stability)
          (expect (not (str/includes? block-t1 "utilization"))))))

    (it "a cross-turn change + utilization both ride as one appended delta, not a re-render"
      (let [baseline (cr/ctx-static-map {:ctx base-ctx})
            cur      (cr/ctx-delta-map {:ctx util-ctx})
            delta    (cr/render-ctx-delta baseline cur)]
        (expect (some? delta))
        (expect (str/includes? delta "filesystem_roots"))   ; the state change
        (expect (str/includes? delta "utilization"))      ; live token usage as a delta
        (expect (every? #(str/starts-with? % "session[") (str/split-lines delta)))))

    (it "ctx-delta-map carries utilization but ctx-static-map (the frozen block) does NOT"
      (expect (contains? (cr/ctx-delta-map {:ctx util-ctx}) :utilization))
      (expect (not (contains? (cr/ctx-static-map {:ctx util-ctx}) :utilization))))))

(defdescribe cache-breakpoints-test
  "The two prompt-cache breakpoints: the last `:role \"system\"` message (the
   frozen `session={…}` prefix) and the LAST message overall (moving recency,
   caches the append-only transcript). pi/maki two-breakpoint pattern."
  (let [apply-bp @#'lp/apply-cache-breakpoints]
    (it "tags ONLY the last system message and the last message (not the middle)"
      (let [out (apply-bp [{:role "system" :content "core"}
                           {:role "system" :content "session = {…}"}     ; frozen block (last system)
                           {:role "user"   :content [{:type "text" :text "prior"}]}
                           {:role "user"   :content [{:type "text" :text "current"}]}])] ; last
        (expect (not (some :svar/cache (let [c (:content (nth out 0))] (if (string? c) nil c)))))
        (expect (some :svar/cache (:content (nth out 1))))   ; frozen system block
        (expect (not (some :svar/cache (:content (nth out 2))))) ; middle untouched
        (expect (some :svar/cache (:content (nth out 3))))))  ; moving recency

    (it "coerces a bare-string last message into a cached text block"
      (let [out (apply-bp [{:role "system" :content "s"} {:role "user" :content "hello"}])
            last-blk (first (:content (last out)))]
        (expect (= "hello" (:text last-blk)))
        (expect (true? (:svar/cache last-blk)))))

    (it "empty list is a no-op; single message gets one breakpoint (idempotent overlap)"
      (expect (= [] (apply-bp [])))
      (let [out (apply-bp [{:role "system" :content "only"}])]
        (expect (some :svar/cache (:content (first out))))))))
