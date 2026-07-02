(ns com.blockether.vis.ext.language-clojure.format
  "cljfmt-backed indentation normalizer used by `clj/edit` for
   format-on-write.

   The root engine no longer reformats source (`internal.format` shows
   code verbatim — render is 'source as written'), so the editing path
   carries its own formatter. cljfmt normalizes indentation + whitespace
   of multi-line forms. NOTE: cljfmt does NOT reflow a one-liner into
   multiple lines — that is a deliberate non-goal of cljfmt — so the
   `clj/edit` tool doc requires the model to emit multi-line code; cljfmt
   then fixes up its indentation on write.

   Failure mode: if cljfmt refuses (parse error, unfamiliar reader macro,
   anything that throws), `format-string` returns the original source
   unchanged. We never silently corrupt a file because the formatter
   choked."
  (:require
   [cljfmt.config :as config]
   [cljfmt.core :as cljfmt]
   [clojure.java.io :as io]))

(defn format-string
  "Return `source` with cljfmt indentation/whitespace normalization, or
   `source` itself on any failure. `opts`, when supplied, is a cljfmt
   options map merged over cljfmt's defaults."
  ([^String source] (format-string source nil))
  ([^String source opts]
   (if-not (and (string? source) (seq source))
     source
     (try
       (if (seq opts)
         (cljfmt/reformat-string source opts)
         (cljfmt/reformat-string source))
       (catch Throwable _ source)))))

(def ^:private config-cache
  "config-file canonical path -> {:mtime <long> :opts <map>}. Keeps the edit
   hook from re-reading + re-parsing `.cljfmt.edn` on every write."
  (atom {}))

(defn cljfmt-opts-for
  "cljfmt options from the nearest `.cljfmt.edn`/`.cljfmt.clj` walking UP from
   `path` (a file OR directory path), so project-local indent rules (e.g. the
   lazytest `it`/`defdescribe` `[[:inner 0]]` overrides) are honored instead of
   cljfmt defaults. Returns nil when no config is found or it can't be read —
   callers then fall back to plain defaults. Cached per config-file + mtime."
  [path]
  (when (seq (str path))
    (try
      (when-let [cf (config/find-config-file (str path))]
        (let [f     (io/file cf)
              stamp (.lastModified f)
              k     (.getCanonicalPath f)
              hit   (get @config-cache k)]
          (if (and hit (= (:mtime hit) stamp))
            (:opts hit)
            (let [opts (config/read-config f)]
              (swap! config-cache assoc k {:mtime stamp :opts opts})
              opts))))
      (catch Throwable _ nil))))
