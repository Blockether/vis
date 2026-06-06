(ns com.blockether.vis.ext.language-clojure.format
  "zprint-backed pretty printer used by `clj/edit` for format-on-write.

   Routes format-on-write through the root vis
   `internal.format/safe-zprint-file-str` so it shares the SAME
   process-wide zprint lock as the TUI render path. zprint keeps
   global in-flight state and throws an `Attempted to run zprint
   with type :structure ... type :zipper` re-entrancy error when a
   format-on-write races a render-side pretty-print;
   the shared gate serializes both surfaces. vis already pulls in
   `zprint/zprint` (top-level deps.edn), inherited transitively here.

   Failure mode: if zprint refuses (parse error, runaway expansion,
   anything that throws), `format-string` returns the original
   source unchanged. We never silently corrupt a file because the
   formatter choked."
  (:require
   [com.blockether.vis.internal.format :as vfmt]))

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
       (vfmt/safe-zprint-file-str source "vis-clj-edit" (merge default-opts opts))
       (catch Throwable _ source)))))
