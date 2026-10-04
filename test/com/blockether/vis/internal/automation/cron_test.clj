(ns com.blockether.vis.internal.automation.cron-test
  (:require [com.blockether.vis.internal.automation.cron :as cron]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.time Instant OffsetDateTime ZonedDateTime ZoneId)))

(defn- ms
  [text zone]
  (.toEpochMilli (.toInstant (ZonedDateTime/of (java.time.LocalDateTime/parse text)
                                               (ZoneId/of zone)))))

(defn- fire
  [expression zone from]
  (some-> (cron/next-fire (cron/parse expression) (ZoneId/of zone) (ms from zone))
          (java.time.Instant/ofEpochMilli)
          (.atZone (ZoneId/of zone))
          (.toLocalDateTime)
          str))

(defn- error-message
  [expression]
  (try (cron/parse expression) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defdescribe
  parse-test
  (describe "fields"
            (it "reads lists, ranges, steps and names"
                (let [spec (cron/parse "*/15 9-17/4 1,15 JAN-mar mon-FRI")]
                  (expect (= #{0 15 30 45} (:minutes spec)))
                  (expect (= #{9 13 17} (:hours spec)))
                  (expect (= #{1 15} (:days spec)))
                  (expect (= #{1 2 3} (:months spec)))
                  (expect (= #{1 2 3 4 5} (:weekdays spec)))))
            (it "reads a start with a step as a range to the field maximum"
                (expect (= #{50 55} (:minutes (cron/parse "50/5 * * * *")))))
            (it "treats day 7 as Sunday" (expect (= #{0} (:weekdays (cron/parse "0 0 * * 7"))))))
  (describe "macros"
            (it "expands the known macros"
                (expect (= (cron/parse "0 0 * * *") (cron/parse "@daily")))
                (expect (= (cron/parse "0 0 1 1 *") (cron/parse "@ANNUALLY")))))
  (describe "errors"
            (it "rejects bad expressions with a readable message"
                (expect (= "Cron expression needs five fields: minute hour day month weekday"
                           (error-message "* * * *")))
                (expect (= "Cron minute value is out of range: 60" (error-message "60 * * * *")))
                (expect (= "Cron hour range is reversed: 5-2" (error-message "0 5-2 * * *")))
                (expect (= "Cron macro is not known: @often" (error-message "@often")))
                (expect (= "Cron day of week value is not valid: XYZ"
                           (error-message "0 0 * * XYZ")))
                (expect (= "Cron minute step must be positive: */0" (error-message "*/0 * * * *")))
                (expect (= "Cron expression is empty" (error-message " "))))))

(defdescribe
  next-fire-test
  (describe "ordinary times"
            (it "answers the first time strictly after the start"
                (expect (= "2026-03-02T09:00" (fire "0 9 * * *" "UTC" "2026-03-01T09:00")))
                (expect (= "2026-03-01T09:15" (fire "*/15 * * * *" "UTC" "2026-03-01T09:00"))))
            (it "starts the first day at the local start time"
                (expect (= "2026-03-02T00:00" (fire "*/15 * * * *" "UTC" "2026-03-01T23:50")))
                (expect (= "2026-03-02T23:59" (fire "59 23 * * *" "UTC" "2026-03-01T23:59:30")))
                (expect (= "2026-03-01T23:59" (fire "59 23 * * *" "UTC" "2026-03-01T23:58:59")))))
  (describe "day rules"
            (it "matches either restricted day field"
                (expect (= "2026-03-02T00:00" (fire "0 0 13 * MON" "UTC" "2026-03-01T12:00")))
                (expect (= "2026-03-13T00:00" (fire "0 0 13 * MON" "UTC" "2026-03-09T12:00"))))
            (it "matches both fields when one field starts with a star"
                (expect (= "2026-03-02T00:00" (fire "0 0 * * MON" "UTC" "2026-03-01T12:00")))
                (expect (= "2026-06-01T00:00" (fire "0 0 */7 * MON" "UTC" "2026-03-01T12:00"))))
            (it "finds a rare date and answers nil for an impossible one"
                (expect (= "2028-02-29T00:00" (fire "0 0 29 2 *" "UTC" "2026-03-01T00:00")))
                (expect (nil? (fire "0 0 30 2 *" "UTC" "2026-03-01T00:00"))))))

(defdescribe
  daylight-saving-test
  (describe
    "a gap"
    (it "fires once at the end of the gap"
        (expect (= "2026-03-29T03:00" (fire "30 2 * * *" "Europe/Warsaw" "2026-03-29T00:00")))
        (expect (= "2026-03-29T03:00" (fire "*/20 2 * * *" "Europe/Warsaw" "2026-03-29T01:59")))
        (expect (= "2026-03-30T02:00" (fire "*/20 2 * * *" "Europe/Warsaw" "2026-03-29T03:00")))))
  (describe "an overlap"
            (it "fires only at the first occurrence"
                (let [zone
                      (ZoneId/of "Europe/Warsaw")

                      spec
                      (cron/parse "30 2 * * *")

                      first-fire
                      (cron/next-fire spec zone (ms "2026-10-25T00:00" "Europe/Warsaw"))

                      second-fire
                      (cron/next-fire spec zone first-fire)]

                  (expect (= "2026-10-25T02:30+02:00[Europe/Warsaw]"
                             (str (.atZone (java.time.Instant/ofEpochMilli first-fire) zone))))
                  (expect (= "2026-10-26T02:30"
                             (str (.toLocalDateTime (.atZone (java.time.Instant/ofEpochMilli
                                                               second-fire)
                                                             zone)))))))
            (it "starts after the second occurrence of a repeated hour"
                (let [zone
                      (ZoneId/of "Europe/Warsaw")

                      from
                      (.toEpochMilli (.toInstant (OffsetDateTime/parse "2026-10-25T02:40+01:00")))]

                  (expect (= "2026-10-25T03:00+01:00[Europe/Warsaw]"
                             (str (.atZone (Instant/ofEpochMilli
                                             (cron/next-fire (cron/parse "*/20 * * * *") zone from))
                                           zone))))))))

(defdescribe zone-test
             (it "rejects an unknown time zone"
                 (expect (= :invalid-timezone
                            (try (cron/zone "Mars/Olympus")
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
