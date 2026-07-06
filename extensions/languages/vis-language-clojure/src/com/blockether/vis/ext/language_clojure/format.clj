(ns com.blockether.vis.ext.language-clojure.format
  "Config-driven Clojure source formatter used by `clj/edit` for format-on-write
   and by the `format_code` language-surface verb.

   TWO backends live here, and the choice is TRANSPARENT to the language
   surface — callers just format; this namespace picks the formatter from the
   config files present around the target path:

     * zprint  — when a `.zprint.edn`/`.zprintrc` is found walking UP from the
                 path. The project's zprint options map is applied. This is the
                 canonical, reflowing formatter (matches the repo's `:format`
                 alias / codestyle).
     * cljfmt  — when only a `.cljfmt.edn`/`.cljfmt.clj` is found (no zprint), or
                 when neither config exists (cljfmt defaults). Conservative:
                 normalizes indentation + whitespace of MULTI-LINE forms but does
                 NOT reflow a one-liner into multiple lines.

   When BOTH configs are present, zprint WINS.

   Failure mode: if a backend refuses (parse error, unfamiliar reader macro,
   anything that throws), the formatter returns the original source unchanged.
   We never silently corrupt a file because the formatter choked."
  (:require [cljfmt.config :as cljfmt-config]
            [cljfmt.core :as cljfmt]
            [clojure.java.io :as io]
            [zprint.config :as zprint-config]
            [zprint.core :as zprint]))

(def ^:private config-cache
  "config-file canonical path -> {:mtime <long> :opts <map>}. Keeps the edit
   hook + format verb from re-reading + re-parsing a config file on every
   write. Shared by both backends (keyed on canonical path, so cljfmt and
   zprint configs never collide)."
  (atom {}))

(defn- cached-opts
  "Read+parse config `f` through `config-cache`, keyed on canonical path +
   mtime. `parse` turns the `io/file` into an opts map."
  [^java.io.File f parse]
  (let [stamp
        (.lastModified f)

        k
        (.getCanonicalPath f)

        hit
        (get @config-cache k)]

    (if (and hit (= (:mtime hit) stamp))
      (:opts hit)
      (let [opts (parse f)]
        (swap! config-cache assoc k {:mtime stamp :opts opts})
        opts))))

;; ── cljfmt backend ───────────────────────────────────────────────────────────

(defn format-string
  "Return `source` with cljfmt indentation/whitespace normalization, or
   `source` itself on any failure. `opts`, when supplied, is a cljfmt
   options map merged over cljfmt's defaults."
  ([^String source] (format-string source nil))
  ([^String source opts]
   (if-not (and (string? source) (seq source))
     source
     (try (if (seq opts) (cljfmt/reformat-string source opts) (cljfmt/reformat-string source))
          (catch Throwable _ source)))))

(defn cljfmt-opts-for
  "cljfmt options from the nearest `.cljfmt.edn`/`.cljfmt.clj` walking UP from
   `path` (a file OR directory path), so project-local indent rules (e.g. the
   lazytest `it`/`defdescribe` `[[:inner 0]]` overrides) are honored instead of
   cljfmt defaults. Returns nil when no config is found or it can't be read —
   callers then fall back to plain defaults. Cached per config-file + mtime."
  [path]
  (when (seq (str path))
    (try (when-let [cf (cljfmt-config/find-config-file (str path))]
           (cached-opts (io/file cf) cljfmt-config/read-config))
         (catch Throwable _ nil))))

;; ── zprint backend ───────────────────────────────────────────────────────────

(def ^:private zprint-config-names
  "Config filenames zprint recognizes, in priority order."
  [".zprint.edn" ".zprintrc"])

(defn zprint-config-file
  "The nearest zprint config file (`.zprint.edn`/`.zprintrc`) walking UP from
   `path` (a file OR directory path), or nil when none is found. This is the
   presence check that decides whether the zprint backend is used at all."
  ^java.io.File [path]
  (when (seq (str path))
    (try (loop [dir (let [f (.getAbsoluteFile (io/file (str path)))]
                      (if (.isDirectory f) f (.getParentFile f)))]
           (when dir
             (if-let [cf (some (fn [n]
                                 (let [c (io/file dir n)]
                                   (when (.isFile c) c)))
                               zprint-config-names)]
               cf
               (recur (.getParentFile dir)))))
         (catch Throwable _ nil))))

(defn zprint-opts-for
  "The zprint options map from the nearest `.zprint.edn`/`.zprintrc` walking UP
   from `path`, or nil when none is found or it can't be read (zprint then uses
   its built-in defaults). Read through zprint's OWN loader
   (`zprint.config/get-config-from-file`) so `:option-fn`/`:guided` forms in the
   config are sci-compiled into real functions — a plain `edn/read-string` would
   leave them as bare lists that zprint rejects. Cached per config-file + mtime."
  [path]
  (try (when-let [f (zprint-config-file path)]
         (cached-opts f
                      (fn [^java.io.File cf]
                        (let [[opts err] (zprint-config/get-config-from-file (.getCanonicalPath cf)
                                                                             true)]
                          (when err (throw (ex-info (str err) {:file (str cf)})))
                          opts))))
       (catch Throwable _ nil)))

(defn zprint-string
  "Return `source` reformatted by zprint using `opts` (the project's zprint
   options map, or nil for zprint defaults), or `source` itself on any
   failure."
  ([^String source] (zprint-string source nil))
  ([^String source opts]
   (if-not (and (string? source) (seq source))
     source
     (try (zprint/zprint-file-str source "vis" (or opts {})) (catch Throwable _ source)))))

;; ── transparent dispatch ─────────────────────────────────────────────────────

(defn format-source
  "Format Clojure `source`, choosing the backend from the config files present
   around `path`: zprint when a `.zprint.edn`/`.zprintrc` is found (its options
   applied), otherwise cljfmt (with the nearest `.cljfmt.edn` opts, or cljfmt
   defaults when neither config exists). zprint WINS when both configs are
   present. TRANSPARENT to callers — they just format; the magic of which
   formatter to run lives here. Returns `source` unchanged on any failure."
  ([source] (format-source source nil))
  ([source path]
   (if (zprint-config-file path)
     (zprint-string source (zprint-opts-for path))
     (format-string source (cljfmt-opts-for path)))))
