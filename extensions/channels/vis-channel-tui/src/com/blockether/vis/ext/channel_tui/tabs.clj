(ns com.blockether.vis.ext.channel-tui.tabs
  "Per-place tab-set persistence.

   The set of open tabs is the thing you want back when you return to a
   place: launch vis in a directory, restore the tabs you had there. The
   sessions themselves already persist in the DB (keyed by session id); this
   only records, per launch directory, WHICH sessions are open as tabs, their
   order, and which one was active.

   Stored as a small EDN sidecar under `~/.vis/tabs/<place>.edn` — no schema,
   no migration, entirely owned by this channel. Shape:

     {:active  \"<session-id>\"      ; the focused tab (or nil)
      :sessions [\"<session-id>\" …]} ; tab order, left to right"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- place-key
  "Stable filename for the current launch directory (`user.dir`): sanitized
   basename + an unsigned hash of the canonical path, so two same-named dirs
   in different locations never share a sidecar."
  []
  (let [dir
        (.getCanonicalPath (io/file (System/getProperty "user.dir")))

        base
        (-> (.getName (io/file dir))
            (str/replace #"[^A-Za-z0-9._-]+" "_"))

        base
        (if (str/blank? base) "root" base)

        h
        (Long/toUnsignedString (Integer/toUnsignedLong (hash dir)) 36)]

    (str base "-" h)))

(defn- tabs-file
  ^java.io.File []
  (io/file (System/getProperty "user.home") ".vis" "tabs" (str (place-key) ".edn")))

(defn save!
  "Persist `snapshot` ({:active sid :sessions [sid…]}) for this place. Best
   effort — IO failures are swallowed so a read-only home never breaks the TUI."
  [snapshot]
  (try (let [f (tabs-file)]
         (io/make-parents f)
         (spit f (pr-str snapshot)))
       (catch Throwable _ nil)))

(defn read-snapshot
  "Read this place's persisted tab snapshot, or nil when absent/unreadable.
   Returns only the well-formed shape: a map with a vector `:sessions`."
  []
  (try (let [f (tabs-file)]
         (when (.exists f)
           (let [m (edn/read-string (slurp f))]
             (when (and (map? m) (vector? (:sessions m))) m))))
       (catch Throwable _ nil)))
