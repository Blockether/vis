(ns com.blockether.vis.internal.language.clojure.format
  "Config-driven Clojure source formatter used by `clj/edit` for format-on-write
   and by the `format_code` language-surface verb.

   TWO backends live here, and the choice is TRANSPARENT to the language
   surface — callers just format; this namespace picks the formatter from the
   config files present around the target path:

     * zprint  — when a `.zprint.edn`/`.zprintrc` is found walking UP from the
                 path. The project's zprint options map is applied. This is the
                 canonical, reflowing formatter — for this repo it is THE
                 formatter, applied through the repo's own `.zprint.edn`.
     * cljfmt  — when only a `.cljfmt.edn`/`.cljfmt.clj` is found (no zprint), or
                 when neither config exists (cljfmt defaults). Conservative:
                 normalizes indentation + whitespace of MULTI-LINE forms but does
                 NOT reflow a one-liner into multiple lines.

   When BOTH configs are present, zprint WINS.

   Failure mode: if a backend refuses (parse error, unfamiliar reader macro,
   anything that throws), the formatter returns the original source unchanged.
   We never silently corrupt a file because the formatter choked."
  (:require [com.blockether.vis.internal.util :as util]
            [cljfmt.config :as cljfmt-config]
            [cljfmt.core :as cljfmt]
            [clojure.java.io :as io]
            [rewrite-clj.node :as node]
            [rewrite-clj.parser :as parser]
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

(defn- cljfmt-config-file
  "The nearest cljfmt config file walking UP from `path`, as a `java.io.File`,
   or nil. Split out of `cljfmt-opts-for` so the result cache can stamp its key
   with the config that actually governs this path."
  ^java.io.File [path]
  (when (seq (str path))
    (try (when-let [cf (cljfmt-config/find-config-file (str path))]
           (io/file cf))
         (catch Throwable _ nil))))

(defn cljfmt-opts-for
  "cljfmt options from the nearest `.cljfmt.edn`/`.cljfmt.clj` walking UP from
   `path` (a file OR directory path), so project-local indent rules (e.g. the
   lazytest `it`/`defdescribe` `[[:inner 0]]` overrides) are honored instead of
   cljfmt defaults. Returns nil when no config is found or it can't be read —
   callers then fall back to plain defaults. Cached per config-file + mtime."
  [path]
  (try (when-let [cf (cljfmt-config-file path)]
         (cached-opts cf cljfmt-config/read-config))
       (catch Throwable _ nil)))

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

;; ── top-level spacing ────────────────────────────────────────────────────────

(defn- gap-newlines
  "How many line breaks the whitespace run `ws` (rewrite-clj nodes between two
   top-level neighbours) spans. A comment node carries its own trailing newline,
   so `after-comment?` adds the one the run does not show."
  ^long [ws after-comment?]
  (+ (if after-comment? 1 0)
     (long (reduce + 0 (map #(count (filter #{\newline} (node/string %))) ws)))))

(defn- separator
  "The whitespace to put between top-level neighbours `prev` and `next`, given
   the run `ws` that sits there now, or nil to keep the run as written:

     * on the same line (a trailing `;; comment`, `#_` discards) — untouched;
     * comment above a form — stays attached; 2+ blank lines collapse to one;
     * form above a form or a comment — exactly one blank line."
  [prev ws]
  (let [after-comment?
        (node/comment? prev)

        newlines
        (gap-newlines ws after-comment?)]

    (cond (zero? newlines) nil
          after-comment? (when (> newlines 2) (node/newlines 1))
          :else (when (not= newlines 2) (node/newlines 2)))))

(defn normalize-top-level-spacing
  "`source` with exactly ONE blank line between top-level forms. A comment
   directly above a form stays attached to it, runs of blank lines collapse to
   one, and neighbours sharing a line are left alone. Whitespace INSIDE a form
   is never touched; returns `source` unchanged when it does not parse."
  ^String [^String source]
  (try (let [forms (parser/parse-string-all source)]
         (loop [[n & more] (node/children forms)
                prev nil
                ws []
                out (transient [])]

           (cond (nil? n) (node/string (node/replace-children forms
                                                              (persistent! (reduce conj! out ws))))
                 (node/whitespace? n) (recur more prev (conj ws n) out)
                 :else (let [run (if-some [sep (when prev (separator prev ws))]
                                   [sep]
                                   ws)]
                         (recur more n [] (conj! (reduce conj! out run) n))))))
       (catch Throwable _ source)))

;; ── transparent dispatch ─────────────────────────────────────────────────────
;; ── formatted-result cache ───────────────────────────────────────────────────

(def ^:private result-cache-limit
  "Hard cap on cached formatted sources. On overflow the whole map is dropped
   rather than evicting an LRU: the cache is a latency optimization, not a
   correctness input, and a rebuild costs one format per live file."
  512)

(def ^:private result-cache
  "[backend config-path config-mtime source-sha] -> formatted source.

   zprint/cljfmt are PURE functions of (source, opts), and `opts` is fully
   determined by the governing config file + its mtime — so this key is total:
   an edited file, an edited `.zprint.edn`, or a different backend all miss.
   Without it every `format_code` re-runs zprint's layout search over files it
   has already proven unchanged, which is seconds per call on deeply nested
   namespaces."
  (atom {}))

(defn- source-sha
  "SHA-256 of `s` as a URL-safe base64 string — the content half of the cache
   key. Hashing avoids pinning whole file bodies in the key."
  ^String [^String s]
  (.encodeToString (java.util.Base64/getUrlEncoder) (util/sha256 (util/utf8 s))))

(defn- result-key
  "Cache key for formatting `source` under `backend` governed by config file
   `cfg` (nil = the backend's built-in defaults)."
  [backend ^java.io.File cfg ^String source]
  [backend (when cfg (.getCanonicalPath cfg)) (when cfg (.lastModified cfg)) (count source)
   (source-sha source)])

(defn- cache-put!
  "Remember `out` for `k` and return `out`, dropping the map when it outgrows
   `result-cache-limit`."
  [k out]
  (swap! result-cache (fn [m]
                        (assoc (if (>= (count m) (long result-cache-limit)) {} m) k out)))
  out)

(defn clear-result-cache!
  "Forget every cached formatting. Only needed when something outside
   (source, config-file+mtime) changes the answer — i.e. in tests."
  []
  (reset! result-cache {}))

(defn format-source
  "Format Clojure `source`, choosing the backend from the config files present
   around `path`: zprint when a `.zprint.edn`/`.zprintrc` is found (its options
   applied), otherwise cljfmt (with the nearest `.cljfmt.edn` opts, or cljfmt
   defaults when neither config exists). zprint WINS when both configs are
   present. TRANSPARENT to callers — they just format; the magic of which
   formatter to run lives here. Either backend is followed by
   `normalize-top-level-spacing`, so a formatted file always carries exactly one
   blank line between top-level forms. Returns `source` unchanged on any failure.

   Memoized through `result-cache` on (backend, config file + mtime, source),
   so re-formatting content this JVM has already formatted is a hash, not a
   layout search. A computed result is ALSO seeded under its own key: a
   formatter is idempotent, so the output is its own fixed point, and the very
   common `format → write → format again` sequence (every re-run of an
   edit/format/lint block) then hits instead of paying a second layout search on
   content only just produced."
  ([source] (format-source source nil))
  ([source path]
   (if-not (and (string? source) (seq source))
     source
     (let [zcf
           (zprint-config-file path)

           backend
           (if zcf :zprint :cljfmt)

           cfg
           (or zcf (cljfmt-config-file path))

           k
           (result-key backend cfg source)]

       (if-some [hit (get @result-cache k)]
         hit
         (let [out (normalize-top-level-spacing (if zcf
                                                  (zprint-string source (zprint-opts-for path))
                                                  (format-string source (cljfmt-opts-for path))))]
           (cache-put! k out)
           ;; Fixed point: formatting `out` again yields `out`. Seeding it here
           ;; makes the next format of the file we just rewrote a cache hit.
           (when (not= out source) (cache-put! (result-key backend cfg out) out))
           out))))))

(defn formatter-for
  "Which backend `format-source` picks for `path`: `:zprint` when a
   `.zprint.edn`/`.zprintrc` is found walking UP, otherwise `:cljfmt`. Callers
   surface this so a format result NAMES the provider that actually ran — the
   dispatch in `format-source` is otherwise invisible."
  [path]
  (if (zprint-config-file path) :zprint :cljfmt))
