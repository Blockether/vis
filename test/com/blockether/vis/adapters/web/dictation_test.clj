(ns com.blockether.vis.adapters.web.dictation-test
  (:require
   [lazytest.core :refer [defdescribe expect it]]
   [com.blockether.vis.adapters.web.conversations :as web-conversations]
   [com.blockether.vis.adapters.web.dictation :as sut]
   [com.blockether.vis.loop.runtime.query.routing :as rlm-routing]))

;; ── helpers ────────────────────────────────────────────────────────────

(defn- with-stub-router
  "Stub `rlm-routing/ask!` so no provider is contacted. `capture` is an
   atom: each call appends the opts map; the next response is whatever
   `respond-fn` returns for that opts map. This is the canonical pattern
   for testing one-shot LLM helpers in this repo (mirrors how we'd test
   `generate-conversation-title` if we did)."
  [capture respond-fn f]
  (with-redefs [rlm-routing/ask!
                (fn [opts]
                  (swap! capture conj opts)
                  (respond-fn opts))]
    (f)))

(defn- with-cached-messages [conversation-id messages f]
  ;; Pre-seed the messages cache so `recent-context` returns deterministic
  ;; data without going to the DB or the env.
  (let [original @web-conversations/messages-cache]
    (try
      (reset! web-conversations/messages-cache
              (assoc original conversation-id messages))
      (f)
      (finally
        (reset! web-conversations/messages-cache original)))))

;; ── tests ──────────────────────────────────────────────────────────────

(defdescribe cleanup-dictation-test
  (it "returns the LLM-cleaned text on success"
    (let [calls (atom [])]
      (with-cached-messages "test-conv" []
        (fn []
          (with-stub-router
            calls
            (fn [_opts] {:result {:cleaned "Refactor the auth module."}
                        :tokens {} :cost {}})
            (fn []
              (let [out (sut/cleanup-dictation "test-conv" "refactor the off module")]
                (expect (= "Refactor the auth module." out)))))))))

  (it "returns the raw input unchanged when the LLM throws"
    (let [calls (atom [])]
      (with-cached-messages "test-conv" []
        (fn []
          (with-stub-router
            calls
            (fn [_opts] (throw (ex-info "provider down" {})))
            (fn []
              (let [out (sut/cleanup-dictation "test-conv" "raw transcript here")]
                (expect (= "raw transcript here" out)))))))))

  (it "returns the raw input when the LLM returns a blank cleaned field"
    (let [calls (atom [])]
      (with-cached-messages "test-conv" []
        (fn []
          (with-stub-router
            calls
            (fn [_opts] {:result {:cleaned "   "} :tokens {} :cost {}})
            (fn []
              (let [out (sut/cleanup-dictation "test-conv" "the original text")]
                (expect (= "the original text" out)))))))))

  (it "returns the raw input when the LLM returns nil for cleaned"
    (let [calls (atom [])]
      (with-cached-messages "test-conv" []
        (fn []
          (with-stub-router
            calls
            (fn [_opts] {:result {} :tokens {} :cost {}})
            (fn []
              (let [out (sut/cleanup-dictation "test-conv" "fallback please")]
                (expect (= "fallback please" out)))))))))

  (it "short-circuits on blank input without calling the LLM"
    (let [calls (atom [])]
      (with-stub-router
        calls
        (fn [_opts] {:result {:cleaned "should not be returned"} :tokens {} :cost {}})
        (fn []
          (expect (= "" (sut/cleanup-dictation "test-conv" "")))
          (expect (= "   " (sut/cleanup-dictation "test-conv" "   ")))
          (expect (= 0 (count @calls)))))))

  (it "builds a message vector containing system + context + dictated text"
    (let [calls (atom [])]
      (with-cached-messages "ctx-conv"
        [{:role :user :text "Working on the auth module today."}
         {:role :assistant :text "Got it — what should change?"}]
        (fn []
          (with-stub-router
            calls
            (fn [_opts] {:result {:cleaned "X"} :tokens {} :cost {}})
            (fn []
              (sut/cleanup-dictation "ctx-conv" "make off async")
              (let [opts (first @calls)
                    msgs (:messages opts)
                    roles (mapv :role msgs)
                    contents (mapv :content msgs)]
                ;; First is the system prompt.
                (expect (= "system" (first roles)))
                (expect (re-find #"clean up" (str (first contents))))
                ;; Conversation context preserved in order.
                (expect (some #(= "Working on the auth module today." %) contents))
                (expect (some #(= "Got it — what should change?" %) contents))
                ;; Final message is the dictated text.
                (expect (= "user" (last roles)))
                (expect (re-find #"make off async" (str (last contents))))
                ;; Routing prefers speed and asks for chat capability.
                (expect (= :speed (:prefer opts)))
                (expect (= #{:chat} (:capabilities opts)))
                ;; Spec asks for a single :cleaned string field.
                (expect (contains? (:spec opts) :cleaned)))))))))

  (it "tolerates a nil conversation-id by skipping context"
    (let [calls (atom [])]
      (with-stub-router
        calls
        (fn [_opts] {:result {:cleaned "ok"} :tokens {} :cost {}})
        (fn []
          (let [out (sut/cleanup-dictation nil "hello world")]
            (expect (= "ok" out))
            ;; Only the system prompt + the dictated user message — no
            ;; context messages were appended.
            (let [msgs (:messages (first @calls))]
              (expect (= 2 (count msgs)))
              (expect (= "system" (:role (first msgs))))
              (expect (= "user" (:role (second msgs))))))))))

  (it "drops blank messages from the conversation context"
    (let [calls (atom [])]
      (with-cached-messages "blank-conv"
        [{:role :user :text "real message"}
         {:role :assistant :text ""}
         {:role :assistant :text nil}
         {:role :user :text "   "}]
        (fn []
          (with-stub-router
            calls
            (fn [_opts] {:result {:cleaned "x"} :tokens {} :cost {}})
            (fn []
              (sut/cleanup-dictation "blank-conv" "do the thing")
              (let [contents (mapv :content (:messages (first @calls)))]
                (expect (some #(= "real message" %) contents))
                (expect (not (some #(= "" %) contents)))
                (expect (not (some nil? contents)))))))))))
