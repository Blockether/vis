(ns com.blockether.vis.ext.language-clojure.format
  "zprint-backed pretty printer used by `clj/edit` for format-on-write.

   Kept as a private seam so callers don't reach into zprint
   directly. The root `vis` project already pulls in `zprint/zprint`
   for its sandbox pretty-printer (see top-level deps.edn), so we
   inherit it transitively and don't redeclare the dep here.

   Failure mode: if zprint refuses (parse error, runaway expansion,
   anything that throws), `format-string` returns the original
   source unchanged. We never silently corrupt a file because the
   formatter choked."
  (:require
   [zprint.core :as zp]))

(def ^:private default-opts
  ;; Conservative defaults. `:width 100` matches the Vis house style;
  ;; `:parse-string-all? true` lets us format a top-level multi-form
  ;; string (rewrite-clj hands us subtree strings that aren't always
  ;; single forms).
  {:width 100
   :parse-string-all? true
   :parse {:interpose "\n"}})

(defn format-string
  "Return `source` pretty-printed by zprint, or `source` itself on
   any failure. `opts` merges into `default-opts`."
  ([^String source] (format-string source nil))
  ([^String source opts]
   (if-not (and (string? source) (seq source))
     source
     (try
       (zp/zprint-file-str source "vis-clj-edit" (merge default-opts opts))
       (catch Throwable _ source)))))
