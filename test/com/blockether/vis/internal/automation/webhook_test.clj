(ns com.blockether.vis.internal.automation.webhook-test
  (:require [com.blockether.vis.internal.automation.webhook :as webhook]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.nio.charset StandardCharsets)
           (java.util HexFormat)))

(defn- utf8 ^bytes [^String text] (.getBytes text StandardCharsets/UTF_8))

(defn- hex [^bytes data] (.formatHex (HexFormat/of) data))

(def ^:private now 1614265330000)

(defn- verify
  [kind secret headers body]
  (webhook/verify kind secret {:headers headers :body (utf8 body) :now now :skew-seconds 300}))

(defdescribe
  verify-test
  (describe
    "github"
    ;; The test vector of the GitHub webhook documentation.
    (it "accepts the published signature"
        (expect (nil? (verify
                        "github"
                        "It's a Secret to Everybody"
                        {"x-hub-signature-256"
                         "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"}
                        "Hello, World!"))))
    (it "rejects another secret, another body and a missing header"
        (let [headers {"x-hub-signature-256"
                       "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"}]
          (expect (= "signature" (verify "github" "other" headers "Hello, World!")))
          (expect (= "signature" (verify "github" "It's a Secret to Everybody" headers "Hello")))
          (expect (= "signature"
                     (verify "github" "It's a Secret to Everybody" {} "Hello, World!"))))))
  (describe
    "standard"
    ;; The test vector of the Standard Webhooks specification.
    (let [secret
          "whsec_MfKQ9r8GKYqrTwjUPD8ILPZIo2LaLaSw"

          body
          "{\"test\": 2432232314}"

          headers
          {"webhook-id" "msg_p5jXN8AQM9LWM0D4loKWxJek"
           "webhook-timestamp" "1614265330"
           "webhook-signature" "v1,g0hM9SsE+OTPJTGt/tmIKtSyZlE3uFJELVlNIOLJ1OE="}]

      (it "signs and accepts the published vector"
          (expect (= "v1,g0hM9SsE+OTPJTGt/tmIKtSyZlE3uFJELVlNIOLJ1OE="
                     (webhook/standard-signature secret
                                                 "msg_p5jXN8AQM9LWM0D4loKWxJek"
                                                 "1614265330"
                                                 (utf8 body))))
          (expect (nil? (verify "standard" secret headers body))))
      (it "accepts one valid signature among several"
          (expect (nil? (verify "standard"
                                secret
                                (update headers "webhook-signature" #(str "v1,AAAA " %))
                                body))))
      (it "rejects an old timestamp and a changed body"
          (expect (= "timestamp"
                     (webhook/verify
                       "standard"
                       secret
                       {:headers headers :body (utf8 body) :now (+ now 301000) :skew-seconds 300})))
          (expect (= "signature" (verify "standard" secret headers "{\"test\": 1}"))))))
  (describe
    "generic"
    (let [secret
          "generic-secret"

          body
          "{\"a\":1}"

          signature
          (hex (webhook/hmac (utf8 secret) (utf8 (str "1614265330." body))))]

      (it "accepts the HMAC of the timestamp and the body"
          (expect (nil? (verify "generic"
                                secret
                                {"x-webhook-timestamp" "1614265330"
                                 "x-webhook-signature-v2" signature}
                                body))))
      (it "rejects a stale timestamp and a missing signature"
          (expect (= "timestamp"
                     (verify "generic"
                             secret
                             {"x-webhook-timestamp" "1614260000" "x-webhook-signature-v2" signature}
                             body)))
          (expect (= "signature"
                     (verify "generic" secret {"x-webhook-timestamp" "1614265330"} body))))))
  (describe "token"
            (it "accepts the token in a GitLab header or a bearer header"
                (expect (nil? (verify "token" "t0ken" {"x-gitlab-token" "t0ken"} "{}")))
                (expect (nil? (verify "token" "t0ken" {"authorization" "Bearer t0ken"} "{}"))))
            (it "rejects another token and an unknown scheme"
                (expect (= "signature" (verify "token" "t0ken" {"x-webhook-token" "other"} "{}")))
                (expect (= "signature"
                           (verify "unknown" "t0ken" {"x-webhook-token" "t0ken"} "{}"))))))

(def ^:private payload
  {"action" "opened"
   "pull_request" {"title" "Fix the parser" "base" {"ref" "main"} "labels" ["bug" "urgent"]}
   "commits" [{"id" "a1"} {"id" "b2"}]})

(defdescribe
  events-test
  (it "reads the event from a header or from the payload"
      (expect (= "pull_request" (webhook/event-name {"x-github-event" "pull_request"} payload)))
      (expect (= "invoice.paid" (webhook/event-name {} {"type" "invoice.paid"})))
      (expect (nil? (webhook/event-name {} nil))))
  (it "accepts every event without a list, else the event or event.action"
      (expect (webhook/event-accepted? [] "push" payload))
      (expect (webhook/event-accepted? ["pull_request"] "pull_request" payload))
      (expect (webhook/event-accepted? ["pull_request.opened"] "pull_request" payload))
      (expect (not (webhook/event-accepted? ["pull_request.closed"] "pull_request" payload)))
      (expect (not (webhook/event-accepted? ["push"] nil nil))))
  (it "uses the first delivery header that has a value"
      (expect (= "d-1" (webhook/delivery-id {"x-github-delivery" " d-1 " "webhook-id" "w-1"})))
      (expect (= "w-1" (webhook/delivery-id {"x-github-delivery" "" "webhook-id" "w-1"})))
      (expect (nil? (webhook/delivery-id {})))))

(defdescribe filters-test
             (it "reads dot paths through maps and vectors"
                 (expect (= "main" (webhook/value-at payload "pull_request.base.ref")))
                 (expect (= "b2" (webhook/value-at payload "commits.1.id")))
                 (expect (= ::webhook/missing (webhook/value-at payload "commits.5.id")))
                 (expect (= ::webhook/missing (webhook/value-at payload "pull_request.head"))))
             (it "passes only when every filter passes"
                 (expect (webhook/filters-pass? [] payload))
                 (expect (webhook/filters-pass? [{"field" "pull_request.base.ref" "equals" "main"}
                                                 {"field" "pull_request.title" "contains" "parser"}
                                                 {"field" "pull_request.labels" "contains" "bug"}
                                                 {"field" "action" "in" ["opened" "reopened"]}]
                                                payload))
                 (expect (not (webhook/filters-pass? [{"field" "pull_request.base.ref"
                                                       "equals" "main"}
                                                      {"field" "action" "in" ["closed"]}]
                                                     payload)))
                 (expect (not (webhook/filters-pass? [{"field" "missing" "equals" nil}] payload)))))

(defdescribe
  render-test
  (it
    "fills paths, keeps missing paths and adds the raw body"
    (expect
      (=
        "PR Fix the parser into main: {pull_request.head.ref} / {\"x\":1}"
        (webhook/render
          "PR {pull_request.title} into {pull_request.base.ref}: {pull_request.head.ref} / {__raw__}"
          payload
          "{\"x\":1}" 100))))
  (it "renders a structured value as JSON"
      (expect (= "[\"bug\",\"urgent\"]" (webhook/render "{pull_request.labels}" payload "" 100))))
  (it "clips each value at a character boundary"
      (expect (= "żó…" (webhook/render "{text}" {"text" "żółw"} "" 5))))
  (it "uses a replacement value as plain text"
      (expect (= "$1 \\n" (webhook/render "{text}" {"text" "$1 \\n"} "" 100)))))
