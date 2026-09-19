(ns com.blockether.vis.internal.foundation.editing.parse
  "Language detection and PARSE VERDICTS for the anchored `patch` gate.

   Two questions, both answered by the LANGUAGE SURFACES that extensions register:

     1. what language is this file, and does anything judge its syntax
        (`detect-language`, `guarded-language`)?
     2. where exactly does the new content fail to parse (`error-nodes`)?
     3. does that language offer a delimiter repair for a splice that would not
        parse (`repair`)?

   `patch` spends both: it re-checks what a write would produce and refuses an edit
   that introduces a syntax error the file did not already have, naming the line
   instead of a bare error count.

   Vis carries NO parser of its own. The only source of a verdict is the
   `:syntax-fn` a language surface declares under `:ext/language-tools`, and the
   file extensions such a surface claims are detected here too. A language nobody
   claims is UNGUARDED: its writes go through unchecked, exactly like prose. A
   surface that throws or breaks the `syntax_result` contract leaves its language
   unguarded for that call rather than blocking the write."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.surface :as contract-surface]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.extension.core :as extension]))

(def ^:private builtin-extension->language
  "The file extensions vis names on its own, before any extension registers: the
   Clojure family — EDN is a subset of the Clojure reader, so `deps.edn` and
   `manifest.edn` are Clojure here — and Python. Every other file type is named by
   the surface that claims it, and a surface only ADDS extensions: it never
   re-routes one of these."
  {"bb" "clojure"
   "clj" "clojure"
   "cljc" "clojure"
   "cljd" "clojure"
   "cljr" "clojure"
   "cljs" "clojure"
   "edn" "clojure"
   "py" "python"
   "pyi" "python"
   "pyw" "python"})

(defn- path-extension
  "Lower-cased extension of `path`'s final segment (no leading dot), or nil when
   the file name has none."
  [^String path]
  (let [name
        (str/replace path #"^.*[/\\]" "")

        dot
        (.lastIndexOf name ".")]

    (when (pos? dot) (str/lower-case (subs name (inc dot))))))

(defn- registered-surfaces
  "The language-tool entries active extensions declare, or nil when the registry
   cannot be read — nothing has registered yet, or a registration is mid-flight.
   Both mean no surface answers, and the language is unguarded."
  []
  (try (seq (mapcat :ext/language-tools (extension/registered-extensions)))
       (catch Throwable _ nil)))

(defn- surface-language
  "The language a surface entry serves, lower-cased, or nil. Registrations declare
   a STRING, exactly as the language-tool dispatcher reads them."
  [entry]
  (some-> (:language entry)
          str
          str/lower-case
          not-empty))

(defn- normalized-extension
  "A declared file extension spelled the way [[path-extension]] answers it:
   lower-cased, without a leading dot. nil when it is not a usable extension."
  [ext]
  (when-let [e (some-> ext
                       str
                       str/trim
                       str/lower-case
                       not-empty)]
    (not-empty (if (str/starts-with? e ".") (subs e 1) e))))

(defn- surface-extension->language
  "File extension -> language for every extension a registered surface CLAIMS.
   This is how a file type vis never heard of earns a name here; the surface that
   claims an extension owns it."
  []
  (reduce (fn [acc entry]
            (if-let [lang (surface-language entry)]
              (reduce (fn [m ext]
                        (if-let [e (normalized-extension ext)]
                          (assoc m e lang)
                          m))
                      acc
                      (:extensions entry))
              acc))
          {}
          (registered-surfaces)))

(defn detect-language
  "Language name for `path`, read from its file extension, or nil. The built-ins
   answer first, then the extensions a registered language surface claims.

   Detection alone says NOTHING about whether a syntax error is meaningful for the
   file: `.md`, `.txt` and `.csv` are simply not detected at all, and a detected
   language is only checked when a surface judges it. `guarded-language` is that
   policy boundary."
  [^String path]
  (when-let [ext (path-extension path)]
    (or (get builtin-extension->language ext) (get (surface-extension->language) ext))))

(defn- syntax-handler
  "The `:syntax-fn` a registered language surface declares for `lang`, or nil. It
   is called with `{:language :source}` and answers a `syntax_result` document —
   the verdict [[error-nodes]] reports. The first matching registration answers."
  [lang]
  (when-let [want (some-> lang
                          str
                          str/lower-case
                          not-empty)]
    (some (fn [entry]
            (let [f (:syntax-fn entry)]
              (when (and (ifn? f) (= want (surface-language entry))) f)))
          (registered-surfaces))))

(defn guarded-language
  "The detected language for `path` when a registered surface answers for its
   syntax, otherwise nil. This is the single policy boundary shared by `patch` and
   sandboxed Python writers; broad language detection alone must never gate prose.

   Vis parses nothing itself, so a language no surface claims is unguarded and its
   writes are not syntax-checked."
  [path]
  (let [lang (detect-language (str path))]
    (when (and lang (some? (syntax-handler lang))) lang)))

(defn- balance-handler
  "The `:balance-fn` a registered language surface declares for `lang`, or nil. It
   is called with the whole repair REQUEST and answers the surface's verdict; the
   first matching registration answers."
  [lang]
  (when-let [want (some-> lang
                          str
                          str/lower-case
                          not-empty)]
    (some (fn [entry]
            (let [f (:balance-fn entry)]
              (when (and (ifn? f) (= want (surface-language entry))) f)))
          (registered-surfaces))))

(defn repair
  "The delimiter repair a registered language surface offers for one spliced file.

   `request` names the language, the whole `:source` a write would produce, the
   `:original` text it replaced, the `:spans` of lines that write touched and an
   optional `:subject` for the refusal wording. Vis judges none of it: the pack
   decides whether a repair may be written and answers

     `{:ok? true :content <repaired source> :notes [note]}` — write this instead
     `{:ok? false :why <reason>}`                          — refuse, and say why

   or nil when the language has no pack, the pack found no repair, or its handler
   failed. Total: a broken splice is then refused exactly as it always was."
  [{:keys [language] :as request}]
  (when-let [f (balance-handler language)]
    (try (let [answer (f (assoc request :language (str language)))]
           (when (and (map? answer) (or (:ok? answer) (:why answer)))
             answer))
         (catch Throwable _ nil))))
(defn- normalized-finding
  "One handler finding as the row the gate reads — `:line`, `:col`, `:kind`,
   `:missing?` and `:text` — keeping every other field the surface reported. Keys
   arrive as Clojure keywords or as the snake_case strings a JSON-shaped handler
   answers, so [[wire/->engine]] is the one inbound spelling rule."
  [finding]
  (when (map? finding)
    (let [row
          (wire/->engine finding)

          kind
          (some-> (:kind row)
                  name
                  str/lower-case
                  not-empty)]

      (assoc row
        :line (long (:line row))
        :col (long (or (:col row) 0))
        :kind (or kind "parse")
        :missing? (boolean (or (:missing? row) (:is-missing row) (= "missing" kind)))
        :text (or (:text row) (:message row))))))

(defn- handler-findings
  "The rows a registered `:syntax-fn` answers for `source` under `lang`, or nil when
   nothing judges the language: no surface serves it, the handler threw, or its
   result does not satisfy the `syntax_result` contract. An empty vector is a
   VERDICT — the handler read the source and found it clean — so a caller must tell
   `[]` from nil."
  [lang ^String source]
  (when-let [f (syntax-handler lang)]
    (try (let [result (wire/->engine (contract-surface/check :syntax-fn
                                                             (f {:language lang :source source})))
               rows (into [] (keep normalized-finding) (:findings result))]

           (cond (:is-clean result) []
                 (seq rows) rows
                 ;; A verdict of "not clean" must never read as clean downstream: with no
                 ;; usable row the write is still refused, at the top of the file.
                 :else [{:line 1 :col 0 :kind "parse" :missing? false :text nil}]))
         (catch Throwable _ nil))))

(defn error-nodes
  "Every syntax fault the language surface for `lang` finds in `source`, in document
   order, as [{:line :col :kind :missing? :text} …] (1-based line, 0-based column)
   plus whatever else the surface reported. Public so an edit guard can turn a bare
   \"N syntax error(s)\" rejection into a LOCATED, actionable message — a surface may
   even NAME the delimiter it expected (`:kind` = `]`, `)`, …) and carry `:delimiter`
   / `:error-line` for a fault that opened earlier than the row it reports.

   Empty when the source is clean, when `lang` is nil, and when NOTHING judges the
   language: vis owns no parser, so an unclaimed language has no faults to report.
   A handler that throws or breaks the `syntax_result` contract reads the same way —
   a misbehaving extension must not wedge the editor shut."
  [lang ^String source]
  (or (handler-findings lang source) []))

(defn transition-verdict
  "Compare `original` and `candidate` under `lang`. Returns a plain-data verdict:
   `:clean`, `:still-broken`, or `:introduced-error`, with the surface's rows when
   relevant. A pre-existing broken file remains writable so a caller can repair it.
   Callers own policy beyond this verdict: `patch` may attempt an explicit delimiter
   repair while raw writers must either preserve the exact candidate or refuse it.

   `:unguarded` means NOTHING judged the write: there is no language, no surface
   serves it, or the one that does threw or broke the `syntax_result` contract. It
   is not a clean bill of health, and callers must not report it as one."
  [lang ^String original ^String candidate]
  (if-let [after (and lang (handler-findings lang candidate))]
    (if (empty? after)
      {:status :clean :language lang :before [] :after []}
      (let [before (or (handler-findings lang original) [])]
        {:status (if (seq before) :still-broken :introduced-error)
         :language lang
         :before before
         :after after}))
    {:status :unguarded :language nil :before [] :after []}))
