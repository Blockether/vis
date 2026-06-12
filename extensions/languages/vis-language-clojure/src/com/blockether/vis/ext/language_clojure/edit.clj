(ns com.blockether.vis.ext.language-clojure.edit
  "Structure-aware Clojure edits via rewrite-clj.

   Public entry: `(apply-edit! workspace-root opts)` where `opts` is:

     {:path      \"src/foo.clj\"        ; required
      :op        :replace | :insert-before | :insert-after | :add
                 | :replace-doc | :replace-sexp
                 ; :add inserts after :target, or appends a new top-level form
                 ; at EOF when no :target is given
                 ; :replace-doc swaps :target's docstring (inserts one if absent)
      :target    sym-name-string         ; defn/def name
                 | [sym-name dispatch]   ; defmethod
                 | {:within sym :match \"(...)\"}    ; for :replace-sexp
      :code      \"(defn bar [] ...)\"  ; new form text
                 ; for :replace-doc, :code is the docstring TEXT (a plain
                 ; string, not a quoted form)
      :is_format true                   ; default true; cljfmt-formats the
                                        ; EDITED/inserted form only, not the
                                        ; whole file}

   Why structure-aware: text-level `patch` is fragile on Clojure
   because docstrings, metadata, and whitespace make uniqueness
   anchors hard. Name-addressed edits cannot collide.

   The function NEVER writes a file unless the planned change is
   parse-clean (rewrite-clj must round-trip the resulting source).
   On any failure it returns a `:result` map with `:status :error`
   so the calling tool can surface the message to the model."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   (com.github.difflib DiffUtils UnifiedDiffUtils)))

;; ---------------------------------------------------------------------------
;; Locating top-level forms by name (+ defmethod dispatch)
;; ---------------------------------------------------------------------------

(def ^:private def-heads
  #{'def 'defonce 'defn 'defn- 'defmacro 'defmulti 'defmethod
    'defprotocol 'definterface 'defrecord 'deftype})

(defn- sexpr-safe [node]
  (try (n/sexpr node) (catch Throwable _ nil)))

(defn- form-matches-target?
  "True when `zloc` is a top-level def-family form named `target-name`
   (and, for `defmethod`, the dispatch matches `target-dispatch`)."
  [zloc target-name target-dispatch]
  (let [sx   (sexpr-safe (z/node zloc))
        head (when (sequential? sx) (first sx))
        nm   (when (sequential? sx) (nth sx 1 nil))]
    (and (contains? def-heads head)
      (= (str nm) (str target-name))
      (if target-dispatch
        ;; Caller specified a dispatch value -> only `defmethod`
        ;; forms can match, and their dispatch must equal the target.
        (and (= 'defmethod head)
          (= (pr-str (nth sx 2 nil)) (pr-str target-dispatch)))
        ;; No dispatch -> any def-family form with the same name
        ;; matches, except `defmethod` (those need an explicit
        ;; dispatch so we don't replace an arbitrary one).
        (not= 'defmethod head)))))

(defn- find-top-form
  "Move a zipper to the first top-level form matching `target`.
   Returns nil if not found."
  [zloc target-name target-dispatch]
  (loop [z zloc]
    (cond
      (nil? z) nil
      (form-matches-target? z target-name target-dispatch) z
      :else (recur (z/right z)))))

;; ---------------------------------------------------------------------------
;; Parse + render helpers
;; ---------------------------------------------------------------------------

(defn- parse-code
  "Parse a piece of replacement source into a rewrite-clj node,
   PRESERVING the caller's formatting (newlines + indentation). Returns
   `[node nil]` on success or `[nil error-message]` on failure.

   Uses rewrite-clj's parser rather than `(n/coerce (read-string code))`:
   the latter round-trips through a plain sexpr, which DISCARDS every bit
   of whitespace and collapses a multi-line form onto a single line.
   rewrite-clj's `z/of-string` keeps the source structure verbatim and
   still handles reader conditionals, tagged literals, and metadata."
  [^String code]
  (try
    (let [zloc (z/of-string code {:track-position? true})]
      (if (nil? zloc)
        [nil "empty code"]
        [(z/node zloc) nil]))
    (catch Throwable t
      [nil (str "parse error: " (.getMessage t))])))

(defn- root-string
  "Render the entire tree back to source. After we `z/replace` / move
   we always go up to the root before printing so we get the whole
   file, not the subtree rooted at the cursor."
  [zloc]
  (z/root-string zloc))

(defn- round-trip-clean?
  "Verify the new source still parses. Cheap defence against any
   shape that rewrite-clj can write but not re-read."
  [^String src]
  (try
    (z/of-string src)
    true
    (catch Throwable _ false)))

;; ---------------------------------------------------------------------------
;; Ops
;; ---------------------------------------------------------------------------

(defn- coerce-target
  "Normalize `:target` into `[name dispatch]`."
  [target]
  (cond
    (string? target)     [target nil]
    (symbol? target)     [(str target) nil]
    (sequential? target) (let [[nm dis] target] [(str nm) dis])
    (map? target)        [(some-> (:within target) str) (:dispatch target)]
    :else                [nil nil]))

(def ^:private diff-context-lines 3)
(def ^:private diff-max-lines 240)

(defn- unified-diff
  "Unified diff (`before` → `after`) as text, capped to `diff-max-lines`.
   Localized: `UnifiedDiffUtils` emits only the changed hunks plus
   `diff-context-lines` of context, so a one-defn edit shows just that defn —
   the same `git diff`-style view `v/patch` returns, so the channel renders a
   real diff instead of a byte-delta scalar."
  [^String before ^String after]
  (let [a     (vec (.split before "\n" -1))
        b     (vec (.split after "\n" -1))
        p     (DiffUtils/diff a b)
        lines (vec (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a p diff-context-lines))
        n     (count lines)]
    (if (<= n diff-max-lines)
      (str/join "\n" lines)
      (str (str/join "\n" (subvec lines 0 diff-max-lines))
        "\n… (" (- n diff-max-lines) " more diff lines)"))))

(defn- err [msg data]
  {:status :error :error msg :data data})

(defn- ok [path before-size after-size & {:as extra}]
  (merge {:status :ok
          :path   path
          :bytes  {:before before-size :after after-size}
          :delta  (- after-size before-size)}
    extra))

(defn- op-replace!
  [zloc target-name target-dispatch code]
  (let [[node err-msg] (parse-code code)]
    (if err-msg
      [nil err-msg]
      (if-let [found (find-top-form zloc target-name target-dispatch)]
        [(-> found (z/replace node)) nil]
        [nil (str "target not found: " target-name
               (when target-dispatch (str " " (pr-str target-dispatch))))]))))

(defn- op-insert!
  [zloc target-name target-dispatch code side]
  (let [[node err-msg] (parse-code code)]
    (if err-msg
      [nil err-msg]
      (if-let [found (find-top-form zloc target-name target-dispatch)]
        (let [inserter (case side
                         :before z/insert-left
                         :after  z/insert-right)]
          ;; Each insert lands IMMEDIATELY adjacent to `found`, so to get the
          ;; new form on its own line we insert the node first and the blank
          ;; line second: `found \n\n node` for :after, `node \n\n found` for
          ;; :before. (The old order jammed the form onto the target's line.)
          [(-> found (inserter node) (inserter (n/newlines 2)))
           nil])
        [nil (str "target not found: " target-name)]))))

(defn- op-append!
  "Append a new top-level form after the last top-level form (EOF append).
   Used by :add when no :target is given. Spacing is approximate;
   zprint normalizes it on write (`is_format` defaults true)."
  [zloc code]
  (let [[node err-msg] (parse-code code)]
    (if err-msg
      [nil err-msg]
      (let [last-z (z/rightmost zloc)]
        [(-> last-z
           (z/insert-right (n/spaces 1))
           (z/insert-right (n/newlines 2))
           (z/insert-right node))
         nil]))))

(defn- op-replace-doc!
  "Replace `target-name`'s docstring, or insert one if absent. `code` is the
   new docstring TEXT — a plain string, NOT a quoted form (built into a string
   node via pr-str so escaping is correct). The docstring is the string right
   after the name that still has a form after it; a lone trailing string (a
   `def` value, e.g. `(def x \"hi\")`) is left alone and a docstring is inserted
   before it."
  [zloc target-name target-dispatch code]
  (let [[doc-node perr] (parse-code (pr-str code))]
    (if perr
      [nil perr]
      (if-let [found (find-top-form zloc target-name target-dispatch)]
        (let [name-loc  (some-> found z/down z/right)        ; head -> name
              after     (some-> name-loc z/right)            ; node after name
              existing? (and after
                          (string? (sexpr-safe (z/node after)))
                          (some? (z/right after)))]          ; a string with a form after = docstring
          (cond
            (nil? name-loc) [nil (str "cannot locate a name in target form: " target-name)]
            existing?       [(z/replace after doc-node) nil]
            :else           [(-> name-loc (z/insert-right doc-node) (z/insert-right (n/spaces 1))) nil]))
        [nil (str "target not found: " target-name)]))))

(defn- op-replace-sexp!
  "Replace the first occurrence of `match-code` inside the body of
   `target-name`. We restrict the search to that top-level form so
   sexp swaps cannot escape into unrelated code."
  [zloc target-name match-code new-code]
  (let [[match-node merr] (parse-code match-code)
        [new-node nerr]   (parse-code new-code)]
    (cond
      merr [nil (str ":match " merr)]
      nerr [nil (str ":replace " nerr)]
      :else
      (if-let [found (find-top-form zloc target-name nil)]
        (let [match-sexpr (try (n/sexpr match-node) (catch Throwable _ ::no))
              ;; Depth-first walk over the WHOLE subtree of `found` so a match
              ;; nested arbitrarily deep (inside a let/cond/when/…) is found,
              ;; not just the two shallow levels the old loop reached. Bounded
              ;; to the subtree: we only descend (z/down) or step among
              ;; siblings (z/right) from the target's children — never z/up
              ;; into `found`'s own siblings.
              hit (when (not= ::no match-sexpr)
                    (letfn [(walk [z]
                              (cond
                                (nil? z) nil
                                (= match-sexpr (sexpr-safe (z/node z))) z
                                :else (or (walk (z/down z))
                                        (walk (z/right z)))))]
                      (walk (z/down found))))]
          (if hit
            [(z/replace hit new-node) nil]
            [nil "match sexp not found inside target form"]))
        [nil (str "target not found: " target-name)]))))

;; ---------------------------------------------------------------------------
;; Public entry
;; ---------------------------------------------------------------------------

(defn apply-edit!
  [workspace-root {:keys [path op target code is_format]
                   :or   {is_format true}
                   :as   opts}]
  ;; `op` arrives from the Python caller as a snake_case STRING
  ;; (e.g. "replace_sexp"); normalize to the kebab keyword the enum +
  ;; case dispatch below expect (:replace-sexp). Also accepts a bare
  ;; keyword for direct Clojure/test callers.
  (let [op (some-> op name (str/replace "_" "-") keyword)
        f  (if (.isAbsolute (io/file path))
             (io/file path)
             (io/file workspace-root path))]
    (cond
      (not (and (.exists f) (.isFile f)))
      (err (str "file not found: " (.getPath f)) {:opts opts})

      (not (#{:replace :insert-before :insert-after :add :replace-doc :replace-sexp} op))
      (err (str "invalid :op " (pr-str op))
        {:expected #{:replace :insert-before :insert-after :add :replace-doc :replace-sexp}})

      (and (not= :replace-sexp op)
        (or (not (string? code)) (str/blank? code)))
      (err ":code must be a non-blank string" {:opts opts})

      :else
      (let [src   (slurp f)
            zloc  (try (z/of-string src {:track-position? true})
                    (catch Throwable t
                      (throw (ex-info "source did not parse"
                               {:type :clj/edit-parse-failed
                                :path (.getPath f)
                                :cause (.getMessage t)}))))
            [tname tdispatch] (coerce-target target)
            code              (if (and is_format code (not= :replace-doc op))
                                (fmt/format-string code)
                                code)
            [zloc' err-msg]
            (case op
              :replace        (op-replace! zloc tname tdispatch code)
              :insert-before  (op-insert!  zloc tname tdispatch code :before)
              :insert-after   (op-insert!  zloc tname tdispatch code :after)
              :add            (if tname
                                (op-insert! zloc tname tdispatch code :after)
                                (op-append! zloc code))
              :replace-doc    (op-replace-doc! zloc tname tdispatch code)
              :replace-sexp   (op-replace-sexp! zloc tname (:match opts) code))]
        (cond
          err-msg
          (err err-msg {:opts opts})

          (nil? zloc')
          (err "edit produced no zipper" {:opts opts})

          :else
          (let [new-src   (root-string zloc')]
            (if-not (round-trip-clean? new-src)
              (err "edited source did not round-trip parse — refusing to write"
                {:opts opts})
              (do
                (spit f new-src)
                (ok (let [root (str (.getCanonicalPath (io/file workspace-root))
                    java.io.File/separator)
          full (.getCanonicalPath f)]
      (if (str/starts-with? full root)
        (subs full (count root))
        full))
    (count src) (count new-src)
    :edit-op op
    :target tname
    :diff (unified-diff src new-src))))))))))
