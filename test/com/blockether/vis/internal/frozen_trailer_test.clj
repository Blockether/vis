(ns com.blockether.vis.internal.frozen-trailer-test
  "E1 frozen-trailer contract — the prefix-cache architecture:

   Trailer pins render ONCE as permanent `<results>` user messages,
   interleaved chronologically with the assistant replays, so the
   conversation grows APPEND-ONLY:

     [system, user_initial,
      <pre-turn pins>, asst_1, <results t/i1>, asst_2, <results t/i2>, …,
      <mutable context tail>]

   and the regenerated `<context>` tail carries ONLY the mutable ctx
   (tasks/facts/cursor/utilization/…). The properties under test:

     1. `render-trailer-pin` is DETERMINISTIC (same pin → same bytes —
        the property that makes frozen messages cacheable) and renders
        both form pins and summary pins, noise-stripped.
     2. `render-ctx-mutable` excludes `session_trailer`; `render-ctx`
        (the bound-dict twin) keeps it.
     3. `frozen-trailer-messages` assembly: ordering, interleaving,
        replay-compatibility filtering, summary-pin placement,
        unparseable-scope grouping — and the PREFIX-STABILITY property:
        iteration K's suffix is a byte-identical prefix of K+1's.
     4. `ensure-prompt-under-budget!` measures the frozen pin messages
        via `:extra-msgs-fn` and folds when they push the prompt over
        budget."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-renderer :as r]
   [com.blockether.vis.internal.safe-guards :as sg]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; loaded for the private fn vars below
(require 'com.blockether.vis.internal.loop)

(def ^:private ftm
  "frozen-trailer-messages [env trailer-iters target turn-position]"
  @#'com.blockether.vis.internal.loop/frozen-trailer-messages)

(def ^:private parse-pin-position
  @#'com.blockether.vis.internal.loop/parse-pin-position)

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- form-pin
  ([scope] (form-pin scope "print(1)" "ok"))
  ([scope src result]
   {:scope scope
    :forms [{:scope (str scope "/f1") :src src :result result}]}))

(defn- env-with-pins [pins]
  {:ctx-atom (atom (assoc (eng/empty-ctx "frozen-trailer-test")
                     :session/trailer (vec pins)))})

(def ^:private target {:provider :zai :model "glm-5"})

(defn- iter-entry
  "One trailer-iters entry [pos data] compatible with `target`."
  ([pos] (iter-entry pos (str "assistant reply " pos)))
  ([pos content]
   [pos {:assistant-message {:role "assistant" :content content}
         :llm-provider (:provider target)
         :llm-model (:model target)
         :preserved-thinking/replay? true}]))

(defn- assistant? [msg] (= "assistant" (:role msg)))

(defn- msg-text
  "WIRE-equivalent text of a message: string content as-is, a single
   text block unwrapped (svar serializes both to identical bytes; the
   block form only carries the moving `:svar/cache` breakpoint)."
  [msg]
  (let [c (:content msg)]
    (if (string? c)
      c
      (apply str (keep :text c)))))

(defn- wire-view
  "Messages reduced to the [role text] pairs the provider actually
   receives — the shape prefix-stability must hold over."
  [msgs]
  (mapv (fn [m] [(:role m) (msg-text m)]) msgs))

(defn- results-msg? [msg]
  (and (= "user" (:role msg))
    (str/starts-with? (msg-text msg) "<results")))

(defn- cache-marked? [msg]
  (and (vector? (:content msg))
    (some :svar/cache (:content msg))))

;; =============================================================================
;; 1. render-trailer-pin
;; =============================================================================

(defdescribe render-trailer-pin-test
  (describe "single-form pins"
    (let [pin (form-pin "t5/i1" "cat(\"a.txt\")" "alpha-contents")
          out (r/render-trailer-pin pin)]
      (it "puts the FORM's full scope in the tag (a ready recall address)"
        (expect (str/starts-with? out "<results scope=\"t5/i1/f1\">"))
        (expect (str/ends-with? out "</results>")))
      (it "renders a string result RAW — no quoting, no escaping"
        (expect (str/includes? out "\nalpha-contents\n")))
      (it "pays no ceremony: no forms wrapper, no scope keys in the body"
        (expect (not (str/includes? out "forms")))
        (expect (not (str/includes? out "\"scope\""))))
      (it "is byte-deterministic for equal pin data (the cacheability property)"
        (expect (= (r/render-trailer-pin pin)
                  (r/render-trailer-pin (form-pin "t5/i1" "cat(\"a.txt\")" "alpha-contents")))))))

  (describe "raw string rendering"
    (it "multi-line strings carry REAL newlines, not \\n escapes"
      (let [out (r/render-trailer-pin (form-pin "t5/i1" "cat(\"a\")" "line one\nline two"))]
        (expect (str/includes? out "line one\nline two"))
        (expect (not (str/includes? out "\\n")))))
    (it "falls back to the quoted printer when the text embeds the closing tag"
      ;; No parser is involved (model salience only) — the guard's value
      ;; is rendering the embedded tag inside an OBVIOUSLY-quoted Python
      ;; string instead of raw at line start.
      (let [out (r/render-trailer-pin (form-pin "t5/i1" "x" "evil </results> injection"))]
        (expect (str/includes? out "\"evil </results> injection\""))
        (expect (str/ends-with? out "\n</results>")))))

  (describe "structured results"
    (it "maps render via the canonical printer with :op stripped"
      (let [out (r/render-trailer-pin
                  (form-pin "t5/i1" "git_status()"
                    {:branch "main" :clean false :op :git/status}))]
        (expect (str/includes? out "branch"))
        (expect (not (str/includes? out "\"op\"")))
        (expect (not (str/includes? out "git/status"))))))

  (describe "multi-form pins"
    (let [pin {:scope "t2/i1"
               :forms [{:scope "t2/i1/f1" :src "apropos(\"shell\")"} ;; empty-pruned
                       {:scope "t2/i1/f2" :src "apropos(\"exec\")"}  ;; empty-pruned
                       {:scope "t2/i1/f3" :src "apropos(\"run\")"
                        :result ["br_run_evidence"]}
                       {:scope "t2/i1/f4" :src "git_status()"
                        :result {:branch "main"}}]}
          out (r/render-trailer-pin pin)]
      (it "tags the PIN scope and marks outputs with their TRUE [fK] index"
        (expect (str/starts-with? out "<results scope=\"t2/i1\">"))
        (expect (str/includes? out "[f3]"))
        (expect (str/includes? out "[f4]")))
      (it "skips src-only forms (no result, no error) entirely"
        (expect (not (str/includes? out "[f1]")))
        (expect (not (str/includes? out "[f2]"))))
      (it "never repeats the scope prefix in the body"
        (expect (= 1 (count (re-seq #"t2/i1" out)))))))

  (describe "errors"
    (it "render as an error:-prefixed dict"
      (let [out (r/render-trailer-pin
                  {:scope "t5/i1"
                   :forms [{:scope "t5/i1/f1" :src "clj_eval('x')"
                            :error {:message "no nREPL port found" :type :no-port}}]})]
        (expect (str/includes? out "error: "))
        (expect (str/includes? out "no nREPL port found")))))

  (describe "noise stripping"
    (it "drops the channel sink slice from the rendered pin"
      (let [pin {:scope "t5/i1"
                 :forms [{:scope "t5/i1/f1" :src "x" :result "r"
                          :channel [{:form "(x)" :position 0}]}]}
            out (r/render-trailer-pin pin)]
        (expect (not (str/includes? out "channel"))))))

  (describe "summary pins"
    (let [pin {:scope-start "t5/i1" :scope-end "t5/i3"
               :summary "folded: read three files"
               :born "t5/i4/f0" :vis/auto? true :vis/summary-source :engine-dummy}
          out (r/render-trailer-pin pin)]
      (it "renders the folded range as the scope attribute"
        (expect (str/includes? out "scope=\"t5/i1..t5/i3\" folded")))
      (it "carries the summary text RAW"
        (expect (str/includes? out "\nfolded: read three files\n")))
      (it "engine bookkeeping never ships (born / auto? / summary-source)"
        (expect (not (str/includes? out "born")))
        (expect (not (str/includes? out "auto")))
        (expect (not (str/includes? out "engine-dummy")))))))

;; =============================================================================
;; 2. render-ctx-mutable vs render-ctx
;; =============================================================================

(defdescribe render-ctx-mutable-test
  (let [ctx (-> (eng/empty-ctx "frozen-trailer-test")
              (assoc :session/turn 5)
              (assoc :session/scope {:turn 5 :iter 2 :next-form 1})
              (assoc-in [:session/tasks "k"] {:title "do it" :status :doing :born "t5/i1/f1"})
              (assoc :session/trailer [(form-pin "t5/i1" "cat(\"a\")" "SECRET-TRAILER-PAYLOAD")]))]
    (it "render-ctx-mutable excludes the trailer entirely"
      (let [out (r/render-ctx-mutable {:ctx ctx :warnings []})]
        (expect (not (str/includes? out "session_trailer")))
        (expect (not (str/includes? out "SECRET-TRAILER-PAYLOAD")))))
    (it "render-ctx-mutable still carries the mutable ctx (tasks)"
      (let [out (r/render-ctx-mutable {:ctx ctx :warnings []})]
        (expect (str/includes? out "session_tasks"))
        (expect (str/includes? out "do it"))))
    (it "render-ctx (the bound-dict twin) keeps the trailer"
      (let [out (r/render-ctx {:ctx ctx :warnings []})]
        (expect (str/includes? out "session_trailer"))
        (expect (str/includes? out "SECRET-TRAILER-PAYLOAD"))))
    (it "project-ctx honours :include-trailer? false"
      (let [view (eng/session-view ctx [])]
        (expect (contains? (r/project-ctx view) :session/trailer))
        (expect (not (contains? (r/project-ctx view {:include-trailer? false})
                       :session/trailer)))))))

;; =============================================================================
;; 2b. empty payloads never enter the trailer (model-form-envelope)
;; =============================================================================

(defdescribe empty-payload-prune-test
  (let [mfe eng/model-form-envelope]
    (it "drops a nil result but keeps the src"
      (let [out (mfe {:scope "t2/i1/f1" :src "clj_eval('x')" :result nil})]
        (expect (= "clj_eval('x')" (:src out)))
        (expect (not (contains? out :result)))))
    (it "drops empty collection results ([] and {})"
      (expect (not (contains? (mfe {:src "apropos(\"shell\")" :result []}) :result)))
      (expect (not (contains? (mfe {:src "probe()" :result {}}) :result))))
    (it "keeps real results — including falsy-but-meaningful ones"
      (expect (= "ok" (:result (mfe {:src "x" :result "ok"}))))
      (expect (= 0 (:result (mfe {:src "x" :result 0}))))
      (expect (= false (:result (mfe {:src "x" :result false})))))
    (it "drops an error that collapses to nothing"
      (expect (not (contains? (mfe {:src "x" :error {}}) :error))))
    (it "keeps a real error"
      (expect (= "no nREPL port found"
                (get-in (mfe {:src "x" :error {:message "no nREPL port found"}}
                          ) [:error :message]))))))

;; =============================================================================
;; 3. frozen-trailer-messages assembly
;; =============================================================================

(defdescribe parse-pin-position-test
  (it "parses form-pin scopes"
    (expect (= {:turn 5 :iter 2} (parse-pin-position {:scope "t5/i2"})))
    (expect (= {:turn 12 :iter 31} (parse-pin-position {:scope "t12/i31/f4"}))))
  (it "summary pins place at their scope-end"
    (expect (= {:turn 5 :iter 3}
              (parse-pin-position {:scope-start "t5/i1" :scope-end "t5/i3"}))))
  (it "returns nil on garbage"
    (expect (nil? (parse-pin-position {:scope "not-a-scope"})))
    (expect (nil? (parse-pin-position {})))))

(defdescribe frozen-assembly-test
  (describe "empty inputs"
    (it "no pins, no iters → empty pins and suffix"
      (expect (= {:pins [] :suffix []}
                (ftm (env-with-pins []) [] target 5)))))

  (describe "pins without replays"
    (let [{:keys [pins suffix]} (ftm (env-with-pins [(form-pin "t5/i1")
                                                     (form-pin "t5/i2")])
                                  [] target 5)]
      (it "every pin becomes a <results> user message, in order"
        (expect (= 2 (count pins)))
        (expect (every? results-msg? pins))
        (expect (str/includes? (msg-text (first pins)) "t5/i1"))
        (expect (str/includes? (msg-text (second pins)) "t5/i2")))
      (it "suffix equals the pin messages on the wire when nothing replays"
        (expect (= (wire-view pins) (wire-view suffix))))))

  (describe "interleaving with replays"
    (let [{:keys [pins suffix]} (ftm (env-with-pins [(form-pin "t5/i1")
                                                     (form-pin "t5/i2")])
                                  [(iter-entry 1) (iter-entry 2)] target 5)]
      (it "wire order is asst_1, results_1, asst_2, results_2"
        (expect (= 4 (count suffix)))
        (expect (assistant? (nth suffix 0)))
        (expect (results-msg? (nth suffix 1)))
        (expect (str/includes? (msg-text (nth suffix 1)) "t5/i1"))
        (expect (assistant? (nth suffix 2)))
        (expect (results-msg? (nth suffix 3)))
        (expect (str/includes? (msg-text (nth suffix 3)) "t5/i2")))
      (it ":pins carries ONLY the <results> messages (budget measurement set)"
        (expect (= 2 (count pins)))
        (expect (every? results-msg? pins)))))

  (describe "replay without a pin (all-silent iteration)"
    (it "the assistant replay still appends"
      (let [{:keys [suffix]} (ftm (env-with-pins []) [(iter-entry 1)] target 5)]
        (expect (= 1 (count suffix)))
        (expect (assistant? (first suffix))))))

  (describe "pre-turn pins"
    (let [{:keys [suffix]} (ftm (env-with-pins [(form-pin "t4/i3")
                                                (form-pin "t5/i1")])
                             [(iter-entry 1)] target 5)]
      (it "previous-turn pins render FIRST, before this turn's replay pairs"
        (expect (= 3 (count suffix)))
        (expect (results-msg? (nth suffix 0)))
        (expect (str/includes? (msg-text (nth suffix 0)) "t4/i3"))
        (expect (assistant? (nth suffix 1)))
        (expect (str/includes? (msg-text (nth suffix 2)) "t5/i1")))))

  (describe "replay compatibility"
    (it "an incompatible provider drops the replay but keeps the pin"
      (let [other [[1 {:assistant-message {:role "assistant" :content "x"}
                       :llm-provider :anthropic :llm-model "claude"
                       :preserved-thinking/replay? true}]]
            {:keys [suffix]} (ftm (env-with-pins [(form-pin "t5/i1")]) other target 5)]
        (expect (= 1 (count suffix)))
        (expect (results-msg? (first suffix)))))
    (it "a replay? false seed (cross-turn) is not replayed"
      (let [seed [[1 {:assistant-message {:role "assistant" :content "x"}
                      :llm-provider (:provider target) :llm-model (:model target)
                      :preserved-thinking/replay? false}]]
            {:keys [suffix]} (ftm (env-with-pins []) seed target 5)]
        (expect (= [] suffix)))))

  (describe "summary pins (post-fold)"
    (it "a summary replacing i1..i2 sits at its scope-end position"
      (let [{:keys [suffix]} (ftm (env-with-pins
                                    [{:scope-start "t5/i1" :scope-end "t5/i2"
                                      :summary "folded"}])
                               [(iter-entry 1) (iter-entry 2)] target 5)]
        ;; asst_1, asst_2, summary-results (placed at pos 2)
        (expect (= 3 (count suffix)))
        (expect (assistant? (nth suffix 0)))
        (expect (assistant? (nth suffix 1)))
        (expect (results-msg? (nth suffix 2)))
        (expect (str/includes? (msg-text (nth suffix 2)) "folded")))))

  (describe "unparseable scopes"
    (it "group with the pre-turn pins at the front (stable position)"
      (let [{:keys [suffix]} (ftm (env-with-pins [{:scope "garbage" :forms [{:src "x"}]}
                                                  (form-pin "t5/i1")])
                               [(iter-entry 1)] target 5)]
        (expect (= 3 (count suffix)))
        (expect (results-msg? (nth suffix 0)))
        (expect (assistant? (nth suffix 1))))))

  (describe "cache breakpoint (Anthropic prompt caching)"
    (let [{:keys [pins suffix]} (ftm (env-with-pins [(form-pin "t5/i1")
                                                     (form-pin "t5/i2")])
                                  [(iter-entry 1) (iter-entry 2)] target 5)]
      (it "the LAST <results> message carries the :svar/cache marker"
        (expect (cache-marked? (last suffix))))
      (it "earlier suffix messages are unmarked"
        (expect (not-any? cache-marked? (butlast suffix))))
      (it ":pins (the measurement set) never carries markers"
        (expect (not-any? cache-marked? pins)))))

  (describe "PREFIX STABILITY — the property the cache depends on"
    ;; Compared on the WIRE view ([role text] pairs): the moving
    ;; :svar/cache breakpoint reshapes the last pin's vis-level message,
    ;; but string content and a single text block serialize identically.
    (it "iteration K's suffix is a wire-identical prefix of K+1's"
      (let [pin1   (form-pin "t5/i1" "cat(\"a\")" "alpha")
            pin2   (form-pin "t5/i2" "patch(\"a\")" "patched")
            step1  (ftm (env-with-pins [pin1]) [(iter-entry 1)] target 5)
            step2  (ftm (env-with-pins [pin1 pin2]) [(iter-entry 1) (iter-entry 2)] target 5)
            s1     (wire-view (:suffix step1))
            s2     (wire-view (:suffix step2))]
        (expect (< (count s1) (count s2)))
        (expect (= s1 (subvec s2 0 (count s1))))))
    (it "holds across three simulated iterations"
      (let [pins   [(form-pin "t5/i1") (form-pin "t5/i2") (form-pin "t5/i3")]
            iters  [(iter-entry 1) (iter-entry 2) (iter-entry 3)]
            steps  (mapv (fn [k]
                           (wire-view
                             (:suffix (ftm (env-with-pins (subvec pins 0 k))
                                        (subvec iters 0 k) target 5))))
                     [1 2 3])]
        (doseq [[a b] (partition 2 1 steps)]
          (expect (= a (subvec b 0 (count a)))))))))

;; =============================================================================
;; 4. budget guard measures the frozen pins
;; =============================================================================

(defdescribe budget-guard-extra-msgs-test
  ;; Sizing rationale: the engine's fold target is
  ;;   trailer-tokens − ((measured-total − budget) + 4000 margin)
  ;; and the picker (W5 safety) refuses any fold that leaves <2 tail
  ;; pins. Equal-sized pins therefore can't fold under a tight budget
  ;; (the 2-pin floor exceeds the target). ONE oversized pin + small
  ;; ones makes the fold trivially achievable: dropping the giant
  ;; lands the remainder far under any positive target.
  ;; Each fat pin stays UNDER the 10k-token form-result clip
  ;; (bound-form-result would stub anything bigger and the rendered
  ;; message would measure tiny): ~4.5k tokens × 4 fats ≈ 18k measured
  ;; vs a 12k budget → target ≈ 8k → folds 3 pins, leaves 3 (≥ the
  ;; 2-pin W5 floor).
  (let [fat (fn [scope] (form-pin scope "probe()" (apply str (repeat 1500 "alpha beta gamma "))))
        pins (conj (mapv #(fat (str "t5/i" %)) (range 1 5))
               (form-pin "t5/i5") (form-pin "t5/i6"))
        mk-env (fn [] (env-with-pins pins))
        pins-of (fn [env] (:pins (ftm env [] target 5)))]
    (it "folds when the frozen pin messages push the prompt over budget"
      (let [env (mk-env)
            r (sg/ensure-prompt-under-budget! env
                {:render-fn     (constantly "<context>\n{}\n</context>")
                 :stable-msgs   [{:role "system" :content "s"}]
                 :extra-msgs-fn pins-of
                 :budget-tokens 12000
                 :born-scope    "t5/i7/f0"})]
        (expect (seq (:rounds r)))
        ;; the fold rewrote pins: a summary stub replaced the oldest batch
        (expect (some :summary (:session/trailer @(:ctx-atom env))))
        (expect (< (count (:session/trailer @(:ctx-atom env))) (count pins)))))
    (it "does not fold when the same prompt is measured WITHOUT the pins"
      (let [env (mk-env)
            r (sg/ensure-prompt-under-budget! env
                {:render-fn     (constantly "<context>\n{}\n</context>")
                 :stable-msgs   [{:role "system" :content "s"}]
                 :budget-tokens 12000
                 :born-scope    "t5/i7/f0"})]
        (expect (empty? (:rounds r)))
        (expect (false? (:over-budget? r)))
        (expect (= (count pins) (count (:session/trailer @(:ctx-atom env)))))))
    (it "a throwing extra-msgs-fn never breaks the guard (fails open)"
      (let [env (mk-env)
            r (sg/ensure-prompt-under-budget! env
                {:render-fn     (constantly "<context>\n{}\n</context>")
                 :stable-msgs   [{:role "system" :content "s"}]
                 :extra-msgs-fn (fn [_] (throw (ex-info "boom" {})))
                 :budget-tokens 12000
                 :born-scope    "t5/i7/f0"})]
        (expect (false? (:over-budget? r)))))))
