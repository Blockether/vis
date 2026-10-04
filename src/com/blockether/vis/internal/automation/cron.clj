(ns com.blockether.vis.internal.automation.cron
  "Five-field cron expressions and their fire times in one time zone.

   A spec matches minute, hour, day of month, month and day of week. Day 0 and
   day 7 are Sunday. When both day fields are restricted, a day matches either
   field, as in Vixie cron. A local time in a daylight-saving gap fires at the
   end of the gap. A local time that occurs twice fires only at the first one."
  (:require [clojure.string :as str])
  (:import
    (java.time DateTimeException Instant LocalDate LocalDateTime LocalTime ZoneId ZonedDateTime)
    (java.time.zone ZoneOffsetTransition ZoneRules)))

(def ^:private macros
  {"@yearly" "0 0 1 1 *"
   "@annually" "0 0 1 1 *"
   "@monthly" "0 0 1 * *"
   "@weekly" "0 0 * * 0"
   "@daily" "0 0 * * *"
   "@midnight" "0 0 * * *"
   "@hourly" "0 * * * *"})

(def ^:private fields
  [{:key :minutes :label "minute" :min 0 :max 59} {:key :hours :label "hour" :min 0 :max 23}
   {:key :days :label "day of month" :min 1 :max 31}
   {:key :months
    :label "month"
    :min 1
    :max 12
    :names ["JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"]
    :first-name 1}
   {:key :weekdays
    :label "day of week"
    :min 0
    :max 7
    :names ["SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"]
    :first-name 0}])

(def ^:private search-days
  "Five years, so a rare valid date such as 29 February is still found."
  (* 5 366))

(defn- invalid [message expression] (ex-info message {:type :invalid-cron :expression expression}))

(defn- field-value
  [{:keys [label min max names first-name]} token expression]
  (let [upper
        (str/upper-case token)

        named
        (when names
          (let [index (.indexOf ^java.util.List names upper)]
            (when (<= 0 index) (+ (long first-name) index))))

        value
        (or named
            (when (re-matches #"\d{1,2}" token) (Long/parseLong token))
            (throw (invalid (str "Cron " label " value is not valid: " token) expression)))]

    (when-not (<= (long min) (long value) (long max))
      (throw (invalid (str "Cron " label " value is out of range: " token) expression)))
    (long value)))

(defn- field-values
  [{:keys [label min max] :as field} text expression]
  (when (str/blank? text) (throw (invalid (str "Cron " label " field is empty") expression)))
  (into (sorted-set)
        (mapcat
          (fn [item]
            (let [[range-text step-text & more]
                  (str/split item #"/" -1)

                  _
                  (when (or (seq more) (str/blank? range-text) (= "" step-text))
                    (throw (invalid (str "Cron " label " item is not valid: " item) expression)))

                  step
                  (if step-text
                    (if (re-matches #"\d{1,2}" step-text)
                      (Long/parseLong step-text)
                      (throw (invalid (str "Cron " label " step is not valid: " item) expression)))
                    1)

                  _
                  (when-not (pos? (long step))
                    (throw (invalid (str "Cron " label " step must be positive: " item)
                                    expression)))

                  [start end]
                  (cond (= "*" range-text) [min max]
                        (str/includes? range-text "-")
                        (let [[a b & extra] (str/split range-text #"-" -1)]
                          (when (or (seq extra) (str/blank? a) (str/blank? b))
                            (throw (invalid (str "Cron " label " range is not valid: " item)
                                            expression)))
                          [(field-value field a expression) (field-value field b expression)])
                        :else (let [value (field-value field range-text expression)]
                                [value (if step-text max value)]))]

              (when (> (long start) (long end))
                (throw (invalid (str "Cron " label " range is reversed: " item) expression)))
              (range start (inc (long end)) step))))
        (str/split text #"," -1)))

(defn parse
  "Parse a five-field expression or a macro such as `@daily`. Throws ex-info
   with `:type :invalid-cron` and a readable message."
  [expression]
  (let [source
        (str/trim (str expression))

        text
        (get macros (str/lower-case source) source)

        parts
        (str/split text #"\s+")]

    (when (str/blank? source) (throw (invalid "Cron expression is empty" expression)))
    (when (and (str/starts-with? source "@") (not (contains? macros (str/lower-case source))))
      (throw (invalid (str "Cron macro is not known: " source) expression)))
    (when-not (= 5 (count parts))
      (throw (invalid "Cron expression needs five fields: minute hour day month weekday"
                      expression)))
    (let [spec (into {}
                     (map (fn [field part]
                            [(:key field) (field-values field part expression)])
                          fields
                          parts))]
      (-> spec
          (update :weekdays
                  #(into (sorted-set)
                         (map (fn [day]
                                (mod (long day) 7)))
                         %))
          (assoc :is-day-restricted (not (str/starts-with? (nth parts 2) "*"))
                 :is-weekday-restricted (not (str/starts-with? (nth parts 4) "*")))))))

(defn zone
  "The ZoneId for `timezone`, or the system zone when it is blank. Throws ex-info
   with `:type :invalid-timezone` when the name is not known."
  ^ZoneId [timezone]
  (if (str/blank? timezone)
    (ZoneId/systemDefault)
    (try (ZoneId/of timezone)
         (catch DateTimeException _
           (throw (ex-info (str "Time zone is not known: " timezone)
                           {:type :invalid-timezone :timezone timezone}))))))

(defn- day-matches?
  [{:keys [days months weekdays is-day-restricted is-weekday-restricted]} ^LocalDate date]
  (let [weekday
        (mod (.getValue (.getDayOfWeek date)) 7)

        day?
        (contains? days (.getDayOfMonth date))

        weekday?
        (contains? weekdays weekday)]

    (and
      (contains? months (.getMonthValue date))
      (if (and is-day-restricted is-weekday-restricted) (or day? weekday?) (and day? weekday?)))))

(defn- instant-ms
  "Resolve one local time. A gap moves the time to the end of the gap; an overlap
   keeps the earlier offset."
  [^LocalDateTime local ^ZoneId zone-id]
  (let [^ZoneRules rules
        (.getRules zone-id)

        offsets
        (.getValidOffsets rules local)]

    (if (.isEmpty offsets)
      (let [^ZoneOffsetTransition transition (.getTransition rules local)]
        (.toEpochMilli (.getInstant transition)))
      (.toEpochMilli (.toInstant (ZonedDateTime/ofLocal local zone-id nil))))))

(defn next-fire
  "The first fire time strictly after `from-ms` in `zone-id`, in Unix
   milliseconds, or nil when none occurs in the next five years. The first day starts
   at the local time of `from-ms`, because an earlier local time never resolves to a
   later instant."
  [spec ^ZoneId zone-id from-ms]
  (let [local
        (.toLocalDateTime (ZonedDateTime/ofInstant (Instant/ofEpochMilli from-ms) zone-id))

        start
        (.toLocalDate local)

        from-hour
        (long (.getHour local))

        from-minute
        (long (.getMinute local))]

    (loop [offset 0]
      (when (< offset (long search-days))
        (let [date (.plusDays start offset)
              first-day? (zero? offset)]

          (or (when (day-matches? spec date)
                (some (fn [hour]
                        (some (fn [minute]
                                (let [at (instant-ms (LocalDateTime/of date
                                                                       (LocalTime/of (int hour)
                                                                                     (int minute)))
                                                     zone-id)]
                                  (when (> (long at) (long from-ms)) at)))
                              (if (and first-day? (== (long hour) from-hour))
                                (subseq (:minutes spec) >= from-minute)
                                (:minutes spec))))
                      (if first-day? (subseq (:hours spec) >= from-hour) (:hours spec))))
              (recur (inc offset))))))))
