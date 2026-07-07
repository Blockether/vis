(ns com.blockether.vis.internal.provider-error-test
  "Provider-error presentation — the per-provider failure breakdown svar attaches
   to an `all-providers-exhausted` error, surfaced as a scannable summary + IR."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider-error :as perr]
            [com.blockether.vis.internal.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private exhausted-err
  "The shape vis builds from svar's `all-providers-exhausted` ex-info: the full
   ex-data lands under `:data` (see loop/format-exception)."
  {:message "All providers exhausted"
   :data {:type :svar.llm/all-providers-exhausted
          :attempts
          [{:provider "anthropic"
            :model "claude-opus-4"
            :status 429
            :reason :rate-limit
            :error "rate limited"}
           {:provider "openai" :model "gpt-5" :status 401 :reason :auth :error "unauthorized"}]}})

(defdescribe
  provider-error-attempts-test
  (it "reads the per-provider attempts from the ex-data"
      (expect (= 2 (count (perr/provider-error-attempts exhausted-err))))
      (expect (= "anthropic" (:provider (first (perr/provider-error-attempts exhausted-err))))))
  (it "one attempt formats as provider/model: status reason"
      (expect (= "anthropic/claude-opus-4: 429 rate-limit"
                 (perr/attempt->line (first (perr/provider-error-attempts exhausted-err))))))
  (it "the summary joins every attempt with ·"
      (expect (= "anthropic/claude-opus-4: 429 rate-limit · openai/gpt-5: 401 auth"
                 (perr/provider-error-attempts-summary exhausted-err))))
  (it "no attempts (older svar / non-routing failure) → empty + nil summary"
      (let [bare {:message "All providers exhausted"
                  :data {:type :svar.llm/all-providers-exhausted}}]
        (expect (empty? (perr/provider-error-attempts bare)))
        (expect (nil? (perr/provider-error-attempts-summary bare)))))
  (it "title for exhausted is the specific headline"
      (expect (= "All providers unavailable" (perr/provider-error-title exhausted-err))))
  (it "the IR embeds each provider's failure line + carries attempts on the data"
      (let [ir
            (perr/provider-error-ir exhausted-err)

            text
            (render/extract-text ir)]

        (expect (str/includes? text "Providers tried"))
        (expect (str/includes? text "anthropic/claude-opus-4: 429 rate-limit"))
        (expect (str/includes? text "openai/gpt-5: 401 auth"))
        (expect (= 2 (count (:attempts (get-in ir [1 :vis/provider-error-data])))))))
  (it "the bare `All providers exhausted` wrapper is NOT repeated as a fact row"
      ;; title + attempts already carry it — no redundant `Wrapper: …` line
      (expect (not-any? #(= "Wrapper" (first %)) (perr/provider-error-facts exhausted-err)))))

(def ^:private single-attempt-err
  "A pinned main turn (post cross-provider-fallback removal) fails on ONE provider,
   yet svar still wraps it as `all-providers-exhausted`. The presentation must NOT
   claim the whole fleet was tried."
  {:message "All providers exhausted"
   :data {:type :svar.llm/all-providers-exhausted
          :attempts [{:provider "zai-coding-plan"
                      :model "glm-5.2"
                      :status 500
                      :reason :transient-error
                      :error "boom"}]}})

(defdescribe single-provider-exhausted-test
  (it "one attempt does NOT claim the whole fleet was tried"
      (expect (= "Provider unavailable" (perr/provider-error-title single-attempt-err)))
      (let [ex (perr/provider-error-explanation single-attempt-err)]
        (expect (str/includes? ex "selected provider"))
        (expect (not (str/includes? ex "every provider"))))
      (expect (str/includes? (perr/provider-error-next-step single-attempt-err)
                             "switch provider/model")))
  (it "two+ attempts still read as the fleet-wide exhaustion"
      (expect (= "All providers unavailable" (perr/provider-error-title exhausted-err)))
      (expect (str/includes? (perr/provider-error-explanation exhausted-err)
                             "every provider"))))

(def ^:private provider-unavailable-err
  "svar >= 0.7.55 no longer wraps a one-provider turn as `all-providers-exhausted`:
   it throws `:svar.llm/provider-unavailable` with message `Provider unavailable`
   and the upstream transient's status preserved. Exactly the turn-1 screenshot."
  {:message "Provider unavailable"
   :data {:type :svar.llm/provider-unavailable
          :status 500
          :attempts [{:provider "zai-coding-plan"
                      :model "glm-5.2"
                      :status 500
                      :reason :transient-error
                      :error "boom"}]}})

(defdescribe provider-unavailable-message-test
  (it "the native single-provider message reads as one provider, not the fleet"
      (expect (= "Provider unavailable"
                 (perr/provider-error-title provider-unavailable-err)))
      (let [ex (perr/provider-error-explanation provider-unavailable-err)]
        (expect (str/includes? ex "selected provider"))
        (expect (not (str/includes? ex "every provider"))))
      (expect (str/includes? (perr/provider-error-next-step provider-unavailable-err)
                             "switch provider/model")))
  (it "the bare `Provider unavailable` wrapper is NOT repeated as a fact row"
      (expect (not-any? #(= "Wrapper" (first %))
                        (perr/provider-error-facts provider-unavailable-err)))))

(defdescribe transport-error-test
  ;; A socket that dies before any response byte arrives ("HTTP/1.1 header
  ;; parser received no bytes") is a network/transport failure, NOT a rejection
  ;; — nothing answered, so there is no HTTP status and the model never ran.
  (let [err {:message "HTTP/1.1 header parser received no bytes" :data {}}]
    (it "classifies a no-bytes wrapper failure as :transport, not :generic"
        (expect (= :transport (perr/provider-error-kind err))))
    (it "titles it as an unreachable provider"
        (expect (= "Could not reach provider" (perr/provider-error-title err))))
    (it "the explanation does NOT falsely claim the provider rejected the request"
        (let [ex (perr/provider-error-explanation err)]
          (expect (str/includes? ex "connection dropped"))
          (expect (not (str/includes? ex "rejected the request")))))
    (it "the next step tells the user to just retry"
        (expect (str/includes? (perr/provider-error-next-step err) "retry")))
    (it "a real HTTP status is NOT mistaken for a transport failure"
        (expect (= :generic
                   (perr/provider-error-kind
                    {:message "Exceptional status code: 400"
                     :data {:status 400
                            :body "{\"error\":{\"message\":\"bad\"}}"}}))))))

(defdescribe
  transport-throwable-test
  ;; `transport-throwable?` is the RETRY gate's classifier: a Throwable is
  ;; retry-safe iff it's a CONNECTION/transport failure (no response byte
  ;; received), regardless of whether a stream had started. It shares the
  ;; exact patterns `transport-error?` applies to the human message, so the
  ;; "just retry" advice and the actual retry can never disagree again (the
  ;; disagreement that made a failed turn NOT retry while telling the user
  ;; it would).
  (it "retries a socket that closed before any response byte"
      (expect (perr/transport-throwable? (ex-info "HTTP/1.1 header parser received no bytes" {}))))
  (it "walks the cause chain for a wrapped transport failure"
      (expect (perr/transport-throwable? (ex-info "provider call failed"
                                                  {}
                                                  (ex-info "header parser received no bytes" {})))))
  (it "retries a connection reset / DNS failure"
      (expect (perr/transport-throwable? (ex-info "java.net.SocketException: Connection reset" {})))
      (expect (perr/transport-throwable? (ex-info "java.net.UnknownHostException: api.host" {}))))
  (it "does NOT retry a real rejection — 429/400 carry an HTTP status"
      (expect (not (perr/transport-throwable? (ex-info "rate limited" {:status 429}))))
      (expect (not (perr/transport-throwable? (ex-info "bad request" {:status 400})))))
  (it "is nil-safe" (expect (not (perr/transport-throwable? nil)))))
