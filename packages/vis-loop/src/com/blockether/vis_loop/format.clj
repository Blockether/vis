(ns com.blockether.vis-loop.format
  "Presentation helpers shared by every surface (TUI, CLI, Telegram):
   date / duration / Clojure-source pretty-printing.

   Lives in vis-loop because:
   - zprint is already a vis-loop dep (the SCI sandbox exposes it),
     so colocating `format-clojure` here avoids duplicating the dep
     into vis-extension.
   - vis-loop is the one package every surface already depends on,
     so vis-cli and channel extensions can both reach this without
     either depending on the other."
  (:require [borkdude.dynaload :as dl]
            [clojure.string :as str]))

;;; ── zprint (dynaload — lazy, cached) ─────────────────────────
;;
;; zprint is a hard dep of vis-loop but its load cost is non-trivial
;; (~hundreds of ms). Dynaload defers the load to first use AND caches
;; the resolved var, so subsequent `format-clojure` calls are a single
;; IFn invocation. The `:default` keeps `format-clojure` working as a
;; pass-through if a slim build ever excludes zprint.

(def ^:private zprint-str-fn
  (dl/dynaload 'zprint.core/zprint-str
    {:default (fn [s & _] s)}))

(defn format-date
  "Format a java.util.Date as dd-MM-yyyy HH:mm in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))

(defn format-clojure
  "Pretty-print a Clojure code string using zprint.
   Falls back to the original string on any error."
  [code-str width]
  (try
    (let [formatted (zprint-str-fn code-str width {:parse-string? true
                                                   :style :community})]
      (if (str/blank? formatted) code-str (str/trimr formatted)))
    (catch Exception _ code-str)))

(defn format-duration
  "Format millisecond duration as human-readable. e.g. '2.3s', '1m 15s'.

  Always uses `Locale/US` so the decimal separator is a dot regardless
  of the JVM default locale (otherwise pl_PL etc. would print '7,3s').
  Coerces `ms` to a long up-front because callers routinely pass a
  double (svar's `elapsed-since` divides by `1e6`), and `(quot 67000.0
  60000)` is `1.0` — which would render as '1.0m 7.0s' instead of
  '1m 7s'."
  [ms]
  (when (and ms (pos? ms))
    (let [ms (long ms)]
      (cond
        (< ms 1000)  (str ms "ms")
        (< ms 60000) (String/format java.util.Locale/US "%.1fs"
                       (into-array Object [(double (/ ms 1000.0))]))
        :else        (let [m (quot ms 60000)
                           s (quot (mod ms 60000) 1000)]
                       (str m "m " s "s"))))))
