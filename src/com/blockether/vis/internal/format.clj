(ns com.blockether.vis.internal.format
  "Format helpers — leaf module.

   Three small, dependency-light formatters used by the SDK facade,
   the TUI footer, the CLI status output, and the Telegram bot. Each
   one is a pure transform over basic Clojure / Java values.

     `format-date`     — `java.util.Date` to `dd-MM-yyyy HH:mm` (local TZ)
     `format-clojure`  — pretty-print a Clojure source string with zprint
     `format-duration` — millisecond duration to `2.3s`, `1m 15s`, etc.

   zprint is a hard dep; `format-clojure` calls it directly. The
   namespace is otherwise free of state — safe to require from any
   layer."
  (:require
   [clojure.string :as str]
   [zprint.core :as zprint])
  (:import
   [java.util Locale]))

(defn format-date
  "Format a `java.util.Date` as `dd-MM-yyyy HH:mm` in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))

(defn format-clojure
  "Pretty-print a Clojure source string with zprint. Falls back to
   the original string on any error."
  [code-str width]
  (try
    (let [formatted (zprint/zprint-str code-str width
                      {:parse-string? true :style :community})]
      (if (str/blank? formatted) code-str (str/trimr formatted)))
    (catch Exception _ code-str)))

(defn format-duration
  "Human-readable millisecond duration. e.g. `2.3s`, `1m 15s`. Always
   uses Locale/US so the decimal separator is a dot regardless of
   the JVM default locale. Coerces the input to long up-front because
   callers routinely pass a double from `(/ ns 1e6)`."
  [ms]
  (when (and ms (pos? ms))
    (let [ms (long ms)]
      (cond
        (< ms 1000)  (str ms "ms")
        (< ms 60000) (String/format Locale/US "%.1fs"
                       (into-array Object [(double (/ ms 1000.0))]))
        :else        (let [m (quot ms 60000)
                           s (quot (mod ms 60000) 1000)]
                       (str m "m " s "s"))))))
