(ns com.blockether.vis.ext.provider-opencode-go.limits-test
  (:require [babashka.http-client :as http]
            [com.blockether.vis.ext.provider-opencode-go.limits :as go-limits]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private usage-payload
  "A 200 from `GET https://opencode.ai/zen/go/v1/usage`: one entry per metered
   window, an integer percent of that window's dollar budget and an ISO-8601
   reset instant. The dollar limits are not on the wire."
  {:usage {:rolling {:status "ok" :percent 37 :resetsAt "2026-08-14T12:00:00.000Z"}
           :weekly {:status "ok" :percent 12 :resetsAt "2026-08-17T00:00:00.000Z"}
           :monthly {:status "ok" :percent 0 :resetsAt "2026-09-01T00:00:00.000Z"}}})

(defn- epoch-ms [^String iso] (.toEpochMilli (java.time.Instant/parse iso)))

(defdescribe
  usage->dynamic-limits-test
  (it "normalizes the three metered windows shortest first"
      (let [report (go-limits/usage->dynamic-limits usage-payload)]
        (expect (= [:opencode-go-5h :opencode-go-7d :opencode-go-30d] (mapv :id (:limits report))))
        (expect (= ["OpenCode Go 5h quota (%)" "OpenCode Go 7d quota (%)"
                    "OpenCode Go 30d quota (%)"]
                   (mapv :label (:limits report))))
        (expect (nil? (:note report)))))
  (it "reports each window as a percentage tank so channels render \"% left\""
      (let [row (first (:limits (go-limits/usage->dynamic-limits usage-payload)))]
        (expect (= 37.0 (:used row)))
        (expect (= 100.0 (:limit row)))
        (expect (= 63.0 (:remaining row)))
        (expect (= :rate (:kind row)))
        (expect (= :account (:scope row)))
        (expect (= :exact (:precision row)))
        (expect (= :provider-api (:source row)))
        (expect (false? (:is-unlimited row)))))
  (it
    "keeps the window shape and the reset instant of each budget"
    (let [rows (:limits (go-limits/usage->dynamic-limits usage-payload))]
      (expect
        (= {:kind :rolling :unit :hour :size 5 :resets-at-ms (epoch-ms "2026-08-14T12:00:00.000Z")}
           (:window (nth rows 0))))
      (expect
        (= {:kind :calendar :unit :week :size 1 :resets-at-ms (epoch-ms "2026-08-17T00:00:00.000Z")}
           (:window (nth rows 1))))
      (expect
        (=
          {:kind :calendar :unit :month :size 1 :resets-at-ms (epoch-ms "2026-09-01T00:00:00.000Z")}
          (:window (nth rows 2))))))
  (it "flags an exhausted window on the row and on the report"
      (let
        [report
         (go-limits/usage->dynamic-limits
           (assoc-in usage-payload
             [:usage :rolling]
             {:status "rate-limited" :percent 100 :resetsAt "2026-08-14T12:00:00.000Z"}))

         row
         (first (:limits report))]

        (expect (= 100.0 (:used row)))
        (expect (= 0.0 (:remaining row)))
        (expect (string? (:note row)))
        (expect (re-find #"rolling" (:note report)))))
  (it "survives a window without a percent instead of inventing one"
      (let
        [report
         (go-limits/usage->dynamic-limits
           {:usage {:rolling {:status "ok" :resetsAt "2026-08-14T12:00:00.000Z"}}})

         row
         (first (:limits report))]

        (expect (= 1 (count (:limits report))))
        (expect (= :unknown (:precision row)))
        (expect (nil? (:used row)))
        (expect (nil? (:remaining row)))))
  (it "drops a window whose reset instant is unparseable rather than throwing"
      (let
        [report (go-limits/usage->dynamic-limits
                  {:usage {:weekly {:status "ok" :percent 5 :resetsAt "not-a-timestamp"}}})]
        (expect (= [:opencode-go-7d] (mapv :id (:limits report))))
        (expect (nil? (get-in report [:limits 0 :window :resets-at-ms])))))
  (it "returns an explanatory note when the payload carries no windows"
      (let [report (go-limits/usage->dynamic-limits {:usage {}})]
        (expect (= [] (:limits report)))
        (expect (string? (:note report))))))

(defdescribe
  fetch-usage-test
  (it
    "sends the Bearer header the endpoint requires and parses the body"
    (let [seen (atom nil)]
      (with-redefs-fn
        {#'http/get
         (fn [url opts]
           (reset! seen [url opts])
           {:status 200
            :body
            "{\"usage\":{\"rolling\":{\"status\":\"ok\",\"percent\":5,\"resetsAt\":\"2026-08-14T12:00:00.000Z\"}}}"})}
        (fn []
          (let [parsed (go-limits/fetch-usage! "test-key")]
            (expect (= 5 (get-in parsed [:usage :rolling :percent])))
            (expect (= "https://opencode.ai/zen/go/v1/usage" (first @seen)))
            (expect (= "Bearer test-key" (get-in (second @seen) [:headers "Authorization"]))))))))
  (it "throws with the HTTP status so 401 and 403 stay distinguishable"
      (with-redefs-fn {#'babashka.http-client/get (fn [_url _opts]
                                                    {:status 403 :body "{\"type\":\"error\"}"})}
        (fn []
          (expect (= 403
                     (try (go-limits/fetch-usage! "test-key")
                          nil
                          (catch clojure.lang.ExceptionInfo e (:status (ex-data e))))))))))
