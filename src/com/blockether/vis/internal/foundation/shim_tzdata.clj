(ns com.blockether.vis.internal.foundation.shim-tzdata
  "Built-in sandbox SHIM: `zoneinfo`, `pytz` and `dateutil` for the model's Python
   sandbox, backed by the JVM's `java.time` IANA time-zone database.

   GraalPy ships no writable filesystem, so the real `zoneinfo` / `pytz` /
   `tzdata` crash at import (`_tzpath` calls `getcwd`, which the denied FS refuses
   with an UN-catchable Java `SecurityException` that aborts the whole eval). This
   extension contributes a `:ext/sandbox-shims` entry that
   `env-python/build-agent-context` installs into every sandbox Context: host callables resolve zone offsets / DST / names via
   `java.time.ZoneId` (604+ zones, no data files), then a Python preamble publishes
   `zoneinfo`, `pytz`, `tzdata` and the `dateutil` package (`dateutil.tz`,
   `dateutil.parser`, `dateutil.relativedelta`) into `sys.modules` and staples them
   onto builtins. All tz math happens on the JVM; only small metadata vectors cross
   the strings-only boundary. Kills the whole class of tz-aware-`datetime` failures."
  (:require [com.blockether.vis.core :as vis])
  (:import [java.time LocalDateTime ZoneId ZoneOffset]
           [java.util Locale TimeZone]))

;; Host bridge: java.time IANA zone rules. The Python side holds only zone-id
;; strings + wall-clock [y m d H M S] vectors; every offset/DST/name lookup
;; happens here on the JVM, so no tz data files (and no getcwd) are ever needed.

(defn- zone-exists?
  "True when `key` names a resolvable java.time zone."
  [key]
  (try (ZoneId/of (str key)) true (catch Throwable _ false)))

(defn- i ^long [x] (long x))

(defn- tz-info
  "For wall-clock `[y m d H M S]` interpreted in zone `key`, return
   `[offset-seconds dst-seconds abbrev]`."
  [key ymdhms]
  (let [[y mo d H M S]
        (map i ymdhms)

        z
        (ZoneId/of (str key))

        rules
        (.getRules z)

        ldt
        (LocalDateTime/of (int y) (int mo) (int d) (int H) (int M) (int S))

        off
        (.getOffset rules ldt)

        inst
        (.toInstant (.atZone ldt z))

        dst
        (.getDaylightSavings rules inst)

        in-dst
        (pos? (.getSeconds dst))

        tzname
        (.getDisplayName (TimeZone/getTimeZone (str key)) in-dst TimeZone/SHORT Locale/US)]

    [(long (.getTotalSeconds off)) (long (.getSeconds dst)) (str tzname)]))

(defn- tz-fromutc
  "For wall-clock `[y m d H M S]` interpreted as UTC in zone `key`, return
   `[offset-seconds]` to add to reach that zone's local wall time."
  [key ymdhms]
  (let [[y mo d H M S]
        (map i ymdhms)

        z
        (ZoneId/of (str key))

        rules
        (.getRules z)

        ldt
        (LocalDateTime/of (int y) (int mo) (int d) (int H) (int M) (int S))

        inst
        (.toInstant ldt ZoneOffset/UTC)

        off
        (.getOffset rules inst)]

    [(long (.getTotalSeconds off))]))

(defn- tz-available
  "Sorted vector of every available IANA zone id."
  []
  (vec (sort (ZoneId/getAvailableZoneIds))))

(defn- tz-local
  "The JVM's default zone id (best-effort local zone)."
  []
  (str (.getId (ZoneId/systemDefault))))

(defn- tz-envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- tzdata-bridge-bindings
  "Host callables the tz shim delegates to (java.time-backed)."
  []
  {"__vis_tz_exists__" (fn [k]
                         (zone-exists? k))
   "__vis_tz_info__" (fn [k ymd]
                       (tz-envelope #(tz-info k ymd)))
   "__vis_tz_fromutc__" (fn [k ymd]
                          (tz-envelope #(tz-fromutc k ymd)))
   "__vis_tz_available__" (fn []
                            (tz-available))
   "__vis_tz_local__" (fn []
                        (tz-local))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-tzdata"
     :ext/description
     (str "Sandbox `zoneinfo`/`pytz`/`tzdata`/`dateutil` subset (tz, parser, relativedelta), "
          "backed by JVM `java.time` IANA data. Avoids denied-filesystem getcwd failures; no "
          "pip/wheel/data files.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims [{:shim/name "tzdata"
                          :shim/imports ["dateutil" "pytz" "tzdata" "zoneinfo"]
                          :shim/bindings tzdata-bridge-bindings
                          :shim/source "vis-shims/tzdata.py"}]}))

(defn register! [] (vis/register-extension! vis-extension))
