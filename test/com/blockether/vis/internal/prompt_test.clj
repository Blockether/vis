(ns com.blockether.vis.internal.prompt-test
  "Smoke coverage for `internal/prompt`.

   Why it exists: AGENTS.md's hard rule \u2014 every namespace must have a
   matching `_test.clj`. The richer end-to-end coverage of the system
   prompt sits in `sandbox_compose_test`; this file pins down the
   small pure helpers in `prompt.clj` so refactors that quietly break
   `<journal>`/`<var_index>` rendering or nudge plumbing get caught
   here instead of in a model trace.

   In particular, after the `repetition-warning` cull we want a
   regression that:
     1. `build-iteration-context` no longer accepts / requires a
        `:call-counts-atom` arg.
     2. The function never injects a `[system_nudge]` line on its own
        \u2014 only `:ext/nudge-fn` results land in the output."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private NO_EXTENSIONS [])

(defdescribe safe-pr-str-test
  (it "round-trips simple values"
    (expect (= "42" (prompt/safe-pr-str 42)))
    (expect (= "\"hi\"" (prompt/safe-pr-str "hi")))
    (expect (= "[1 2 3]" (prompt/safe-pr-str [1 2 3]))))

  (it "truncates oversized strings with a chars-remaining suffix"
    (let [big (apply str (repeat 10000 \x))
          out (prompt/safe-pr-str big {:max-chars 50})]
      (expect (re-find #" \u2026<\+\d+ chars>$" out))
      (expect (< (count out) 200))))

  (it "swallows unprintable values instead of throwing"
    (let [bomb (reify Object (toString [_] (throw (ex-info "boom" {}))))]
      (expect (string? (prompt/safe-pr-str bomb))))))

(defdescribe truncated-pr-str-test
  (it "returns [string false] for small values"
    (let [[s truncated?] (prompt/truncated-pr-str {:a 1})]
      (expect (string? s))
      (expect (false? truncated?))))

  (it "flags truncation for oversized values"
    (let [big (apply str (repeat (* 10 prompt/MAX_RESULT_DISPLAY_CHARS) \y))
          [_s truncated?] (prompt/truncated-pr-str big)]
      (expect (true? truncated?)))))

(defdescribe build-iteration-context-test
  (it "requires :active-extensions; throws otherwise"
    (let [thrown? (try
                    (prompt/build-iteration-context {} {})
                    false
                    (catch clojure.lang.ExceptionInfo e
                      (= :vis/missing-active-extensions
                        (:type (ex-data e)))))]
      (expect (true? thrown?))))

  (it "returns nil when there is nothing to render and no nudges fire"
    ;; Pre-bind a non-blank title so the always-on title nudge stays
    ;; quiet, and skip a journal seed: nothing to render -> nil.
    (expect (nil? (prompt/build-iteration-context
                    {:conversation-title-atom (atom "set")}
                    {:active-extensions NO_EXTENSIONS
                     :iteration         0}))))

  (it "renders <journal> for prior iteration blocks"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "set")}
                {:active-extensions   NO_EXTENSIONS
                 :blocks-by-iteration [[0 {:thinking nil
                                           :blocks   [{:code "(+ 1 2)" :result 3}]}]]
                 :iteration           0})]
      (expect (string? out))
      (expect (str/includes? out "<journal>"))
      (expect (str/includes? out "(+ 1 2)"))
      (expect (str/includes? out "i0.1"))))

  ;; Helpers ------------------------------------------------------------------
  ;;
  ;; New `:blocks-by-iteration` shape is `[[pos {:thinking :blocks}]]`
  ;; (the renderer was upgraded to surface per-iter thinking + per-form
  ;; comments). Old `[[pos blocks]]` shape would be silently treated
  ;; as `:thinking nil :blocks <vec-of-pairs>` — wrap explicitly.
  ;;
  ;; The title-nudge built-in always fires on a bare `{}` env (no
  ;; `:conversation-title-atom`, so the title is treated as blank).
  ;; Tests that probe nudges from OTHER paths use a `with-title`
  ;; helper that pre-binds a non-blank title.
  (letfn [(env-with-title []
            {:conversation-title-atom (atom "already set")})
          (->iter [pos blocks]
            [pos {:thinking nil :blocks blocks}])]

    (it "never injects a built-in repetition nudge, even on identical reruns"
      ;; Regression: the built-in `repetition-warning` was removed.
      ;; Repeating the same expression must NOT produce a
      ;; `[system_nudge]` line from the repetition path. Title-nudge
      ;; suppressed by pre-binding a non-blank title; we check for the
      ;; absence of any [system_nudge] line.
      (let [env    (env-with-title)
            blocks [{:code "(grep \"X\")" :result []}]
            out-1  (prompt/build-iteration-context
                     env {:active-extensions   NO_EXTENSIONS
                          :blocks-by-iteration [(->iter 0 blocks)]
                          :iteration           0})
            out-2  (prompt/build-iteration-context
                     env {:active-extensions   NO_EXTENSIONS
                          :blocks-by-iteration [(->iter 1 blocks)]
                          :iteration           1})]
        (expect (not (str/includes? (or out-1 "") "[system_nudge]")))
        (expect (not (str/includes? (or out-2 "") "[system_nudge]")))))

    (it "rejects the legacy :call-counts-atom arg silently (it is ignored, not required)"
      ;; Defensive: callers that still pass `:call-counts-atom` must
      ;; not blow up; the key is simply ignored.
      (let [out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   NO_EXTENSIONS
                   :call-counts-atom    (atom {})
                   :blocks-by-iteration [(->iter 0 [{:code "(+ 1 2)" :result 3}])]
                   :iteration           0})]
        (expect (string? out))
        (expect (not (str/includes? out "[system_nudge]")))))

    (it "appends extension nudges when :ext/nudge-fn returns a non-blank string"
      (let [ext (identity
                  {:ext/namespace 'fake.nudger
                   :ext/nudge-fn  (fn [_ctx] "[system_nudge] hi from fake.nudger")})
            ;; Always-on title nudge keeps the line count >= 1 already;
            ;; we just check the model's nudge gets concatenated.
            ext-only-ext {:ext/namespace 'fake.nudger
                          :ext/nudge-fn  (fn [_ctx] "[system_nudge] hi from fake.nudger")}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext-only-ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (str/includes? out "[system_nudge] hi from fake.nudger"))
        ;; Silence the unused alias warning -- present for future tests.
        (when (some? ext) :ok)))

    (it "swallows extension nudge-fn exceptions instead of bubbling"
      (let [ext {:ext/namespace 'fake.thrower
                 :ext/nudge-fn  (fn [_ctx] (throw (ex-info "boom" {})))}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (string? out))
        ;; Both built-in and extension nudges suppressed: title is
        ;; non-blank, iteration not on the refresh cadence,
        ;; extension nudge threw and was swallowed.
        (expect (not (str/includes? out "[system_nudge]")))))

    (it "fires the title-nudge when CONVERSATION_TITLE is blank"
      (let [env-blank-title {:conversation-title-atom (atom "")}
            out (prompt/build-iteration-context
                  env-blank-title
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (str/includes? out "[system_nudge]"))
        (expect (str/includes? out "CONVERSATION_TITLE is currently empty"))))

    (it "fires the title-refresh nudge every TITLE_REFRESH_NUDGE_PERIOD iterations"
      (let [period @(requiring-resolve 'com.blockether.vis.internal.prompt/TITLE_REFRESH_NUDGE_PERIOD)
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           period})]
        (expect (str/includes? out "[system_nudge]"))
        (expect (str/includes? out (str "You're " period " iterations")))))))

(defdescribe extensions-snapshot-test
  (it "includes full extension provenance plus symbols/docs"
    (let [ext (or (some #(when (= 'com.blockether.vis.core (:ext/namespace %)) %)
                    (extension/registered-extensions))
                (extension/extension {:ext/namespace 'com.blockether.vis.core
                                      :ext/doc       "vis core"}))
          [entry] (prompt/extensions-snapshot [ext])]
      (expect (= 'com.blockether.vis.core (:namespace entry)))
      (expect (contains? entry :source-paths))
      (expect (contains? entry :source-mtime-max))
      (expect (contains? entry :source-hash-sha256))
      (expect (contains? entry :symbols))
      (expect (contains? entry :docs)))))

(defdescribe core-system-prompt-test
  (it "front-loads the RLM control-flow contract"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "recursive language model (RLM)"))
      (expect (str/includes? p "read/eval/observe loop"))
      (expect (str/includes? p "terminal COMMIT"))
      (expect (str/includes? p "TURN_USER_REQUEST is fully satisfied"))
      (expect (str/includes? p "host automatically continues the SAME user turn"))
      (expect (str/includes? p "Not every iteration needs an answer"))
      (expect (str/includes? p "omitting `(answer ...)`; the runtime will loop you"))
      (expect (str/includes? p "UNDERSTAND"))
      (expect (str/includes? p "PLAN"))
      (expect (str/includes? p "EXPLORE"))
      (expect (str/includes? p "OBSERVE"))
      (expect (str/includes? p "ACT"))
      (expect (str/includes? p "VERIFY"))
      (expect (str/includes? p "ANSWER"))))

  (it "teaches functional state surfacing and gates"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "(def observation (v/rg"))
      (expect (str/includes? p "observation"))
      (expect (str/includes? p "turn-state"))
      (expect (str/includes? p ":gates"))
      (expect (str/includes? p "Close or block gates with observed evidence"))))

  (it "rejects exploration-only terminal progress answers"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "Exploration-only iterations must not call `(answer …)`"))
      (expect (str/includes? p "(answer \"scanned\") ; BAD"))
      (expect (not (str/includes? p "last 2 iters"))))))

(defdescribe assemble-initial-messages-test
  (it "places system, history, and trailing user content in order"
    (let [msgs (prompt/assemble-initial-messages
                 {:system-prompt        "SYS"
                  :history-messages     [{:role "user" :content "old"}]
                  :initial-user-content "now"})]
      (expect (= [{:role "system" :content "SYS"}
                  {:role "user" :content "old"}
                  {:role "user" :content "now"}]
                msgs))))

  (it "omits the system slot when no system prompt is supplied"
    (let [msgs (prompt/assemble-initial-messages
                 {:initial-user-content "hi"})]
      (expect (= [{:role "user" :content "hi"}] msgs)))))

(defdescribe trim-to-initial-history-test
  (it "keeps exactly the prefix of length n"
    (let [msgs [:a :b :c :d]]
      (expect (= [:a :b] (prompt/trim-to-initial-history msgs 2)))
      (expect (= []      (prompt/trim-to-initial-history msgs 0)))
      (expect (= [:a :b :c :d] (prompt/trim-to-initial-history msgs 4))))))
