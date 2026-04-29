(ns com.blockether.vis.internal.prompt-test
  "Smoke coverage for `internal/prompt`.

   Why it exists: AGENTS.md's hard rule \u2014 every namespace must have a
   matching `_test.clj`. The richer end-to-end coverage of the system
   prompt sits in `sandbox_compose_test`; this file pins down the
   small pure helpers in `prompt.clj` so refactors that quietly break
   `<recent>`/`<var_index>` rendering or nudge plumbing get caught
   here instead of in a model trace.

   In particular, after the `repetition-warning` cull we want a
   regression that:
     1. `build-iteration-context` no longer accepts / requires a
        `:call-counts-atom` arg.
     2. The function never injects a `[system_nudge]` line on its own
        \u2014 only `:ext/nudge-fn` results land in the output."
  (:require
   [clojure.string :as str]
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

  (it "returns nil when there is nothing to render"
    (expect (nil? (prompt/build-iteration-context
                    {} {:active-extensions NO_EXTENSIONS}))))

  (it "renders <recent> for prior iteration blocks"
    (let [out (prompt/build-iteration-context
                {}
                {:active-extensions   NO_EXTENSIONS
                 :blocks-by-iteration [[0 [{:code "(+ 1 2)" :result 3}]]]})]
      (expect (string? out))
      (expect (str/includes? out "<recent>"))
      (expect (str/includes? out "(+ 1 2)"))
      (expect (str/includes? out "i0.1"))))

  (it "never injects a built-in repetition nudge, even on identical reruns"
    ;; Regression: the built-in `repetition-warning` was removed.
    ;; Repeating the same expression must NOT produce a
    ;; `[system_nudge]` line from the runtime itself.
    (let [blocks [{:code "(grep \"X\")" :result []}]
          out-1  (prompt/build-iteration-context
                   {} {:active-extensions   NO_EXTENSIONS
                       :blocks-by-iteration [[0 blocks]]})
          out-2  (prompt/build-iteration-context
                   {} {:active-extensions   NO_EXTENSIONS
                       :blocks-by-iteration [[1 blocks]]})]
      (expect (not (str/includes? (or out-1 "") "[system_nudge]")))
      (expect (not (str/includes? (or out-2 "") "[system_nudge]")))))

  (it "rejects the legacy :call-counts-atom arg silently (it is ignored, not required)"
    ;; Defensive: callers that still pass `:call-counts-atom` must
    ;; not blow up; the key is simply ignored.
    (let [out (prompt/build-iteration-context
                {}
                {:active-extensions   NO_EXTENSIONS
                 :call-counts-atom    (atom {})
                 :blocks-by-iteration [[0 [{:code "(+ 1 2)" :result 3}]]]})]
      (expect (string? out))
      (expect (not (str/includes? out "[system_nudge]")))))

  (it "appends extension nudges when :ext/nudge-fn returns a non-blank string"
    (let [ext     {:ext/namespace 'fake.nudger
                   :ext/nudge-fn  (fn [_ctx] "[system_nudge] hi from fake.nudger")}
          out     (prompt/build-iteration-context
                    {} {:active-extensions   [ext]
                        :blocks-by-iteration [[0 [{:code "1" :result 1}]]]})]
      (expect (str/includes? out "[system_nudge] hi from fake.nudger"))))

  (it "swallows extension nudge-fn exceptions instead of bubbling"
    (let [ext {:ext/namespace 'fake.thrower
               :ext/nudge-fn  (fn [_ctx] (throw (ex-info "boom" {})))}
          out (prompt/build-iteration-context
                {} {:active-extensions   [ext]
                    :blocks-by-iteration [[0 [{:code "1" :result 1}]]]})]
      (expect (string? out))
      (expect (not (str/includes? out "[system_nudge]"))))))

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
