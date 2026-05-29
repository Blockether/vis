(ns com.blockether.vis.ext.foundation-git.render
  "Channel IR renderers for `git/*` tools.

   Engine contract for `:render-fn` / `:render-error-fn` (Phase 1 hard
   cut, mirrors every other extension):

     (fn [result] {:summary <ir-or-zones> :display <ir>})

   `result` is the raw map returned to SCI as `:result`. The MODEL
   sees the same map (via `pr-str` of the unwrapped SCI return) —
   these renderers ONLY shape the channel preview, never what the
   LLM reads.

   `:summary` is the single badge/op row. Where the result has a
   natural label + right-anchored metric (counts, +N/-M, line counts,
   sha) we emit a zone map `{:left <label-ir> :center? … :right? …}`;
   otherwise a single `[:p ...]` IR paragraph whose first `[:strong]`
   is the badge label. `:display` is the full expanded body these
   renderers historically returned — code blocks, numstat tables,
   per-file patches.

   Style: tiny IR vector builders (`[:ir]`, `[:p]`, `[:c]`, `[:code]`,
   `[:strong]`) and a single soft byte cap on free-form bodies.
   Bullet decoration is left to the channel: rows render as plain text
   inside `[:code {:lang \"text\"}]` blocks so previews stay
   copy-pasteable. Status badges use `[:strong]` so channels bold them
   (TUI / Telegram both honour the marker)."
  (:require
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; IR builders
;; ---------------------------------------------------------------------------

(defn- ir-code [s] [:c {} (str s)])
(defn- ir-strong [s] [:strong {} [:span {} (str s)]])
(defn- ir-code-block [lang body]
  [:code (cond-> {} lang (assoc :lang lang)) (str body)])
(defn- ir-inline [x] (if (vector? x) x [:span {} (str x)]))
(defn- ir-p [& parts]
  (into [:p {}] (map ir-inline (filter some? parts))))
(defn- ir-root [& blocks]
  (into [:ir {}] (filter some? blocks)))

(def ^:private preview-cap 32000)

(defn- cap [^String s]
  (cond
    (nil? s)             ""
    (<= (count s) preview-cap) s
    :else (str (subs s 0 (- preview-cap 64))
            "\n\n... (preview truncated; full payload in :result)")))

(defn- short-sha [s]
  (when (string? s) (subs s 0 (min 8 (count s)))))

(defn- pad-right [^String s n]
  (let [s (str s)
        l (count s)]
    (if (>= l n) s (str s (apply str (repeat (- n l) \space))))))

;; ---------------------------------------------------------------------------
;; git/status
;; ---------------------------------------------------------------------------

(defn render-status
  [{:keys [branch head clean? entries]}]
  (let [n     (count entries)
        label (if clean? "CLEAN" "DIRTY")]
    {:summary
     (cond-> {:left  (ir-strong label)
              :center (ir-code (or branch "?"))}
       head        (assoc :center
                     (ir-p (ir-code (or branch "?")) " @" (short-sha head)))
       (not clean?) (assoc :right (str n " entr" (if (= n 1) "y" "ies"))))
     :display
     (ir-root
       (ir-p (ir-strong label)
         "  " (ir-code (or branch "?"))
         (when head (str "  @" (short-sha head)))
         (when-not clean? (str "  " n " entr" (if (= n 1) "y" "ies"))))
       (when (seq entries)
         (ir-code-block "text"
           (cap (str/join "\n"
                  (map (fn [{:keys [status file]}]
                         (str status " " file))
                    entries))))))}))

;; ---------------------------------------------------------------------------
;; git/diff
;; ---------------------------------------------------------------------------

(defn- diff-row [{:keys [file + -]}]
  (str (pad-right (str "+" +) 6)
    (pad-right (str "-" -) 6)
    file))

(defn render-diff
  [{:keys [branch head kind from to path stat files porcelain]}]
  (let [nf    (:files stat)
        np    (:+ stat)
        nm    (:- stat)
        scope (or branch (name (or kind :workspace)))
        range (str (or (short-sha from) (str from)) ".."
                (or (short-sha to) (if to (str to) "WT")))
        ;; ONE file overview. The numstat carries the tracked changes (+/-);
        ;; porcelain entries the numstat didn't already cover (untracked files
        ;; have no +/- line) are appended so nothing is listed twice.
        tracked   (set (map :file files))
        untracked (remove #(contains? tracked (:file %)) (or porcelain []))
        overview  (str/join "\n"
                    (concat
                      (map diff-row files)
                      (map (fn [{:keys [status file]}] (str status " " file)) untracked)))
        ;; JGit's DiffFormatter already prefixes each file with its own
        ;; `diff --git a/… b/…` header, so the patches self-label — no
        ;; `── file ──` separator rows (those wrecked the layout). One block.
        patch-text (str/join "\n" (keep :patch files))]
    {:summary
     {:left   (ir-strong "DIFF")
      :center (ir-p (ir-code scope) " " range)
      :right  (str nf " file" (when (not= 1 nf) "s") "  +" np "  −" nm)}
     :display
     (apply ir-root
       (concat
         ;; Context NOT already in the summary header (path filter / resolved
         ;; HEAD sha). The DIFF label, scope, range and +/- counts live in the
         ;; summary — never repeat them here or the expanded row shows twice.
         (when (or path head)
           [(ir-p (str/join "  "
                    (remove nil?
                      [(when path (str "path=" path))
                       (when head (str "@" (short-sha head)))])))])
         (when (seq (str/trim overview))
           [(ir-code-block "text" (cap overview))])
         (when (seq (str/trim patch-text))
           [(ir-code-block "diff" (cap patch-text))])))}))

;; ---------------------------------------------------------------------------
;; git/log
;; ---------------------------------------------------------------------------

(defn- log-row [{:keys [short-sha author at subject]}]
  (str (pad-right short-sha 9)
    (pad-right (or author "?") 22)
    (or at "")
    "  " (or subject "")))

(defn render-log
  [{:keys [branch commits]}]
  (let [n (count commits)]
    {:summary
     {:left   (ir-strong "LOG")
      :center (ir-code (or branch "?"))
      :right  (str n " commit" (when (not= 1 n) "s"))}
     :display
     (ir-root
       (ir-p (ir-strong "LOG")
         "  " (ir-code (or branch "?"))
         "  " n " commit" (when (not= 1 n) "s"))
       (when (seq commits)
         (ir-code-block "text"
           (cap (str/join "\n" (map log-row commits))))))}))

;; ---------------------------------------------------------------------------
;; git/show
;; ---------------------------------------------------------------------------

(defn render-show
  [{:keys [short-sha sha author email at subject body files stat committer]}]
  (let [files (or files [])
        nf    (or (:files stat) (count files))
        np    (or (:+ stat) 0)
        nm    (or (:- stat) 0)
        sha*  (or short-sha (when sha (subs sha 0 (min 8 (count sha)))) "?")
        body* (str/join "\n" (map diff-row files))
        ;; Patches self-label via JGit's `diff --git a/… b/…` headers — one
        ;; diff block, no `── file ──` separator rows.
        patch-text (str/join "\n" (keep :patch files))]
    {:summary
     {:left   (ir-strong "SHOW")
      :center (ir-p (ir-code sha*) " " (or subject author "?"))
      :right  (str nf " file" (when (not= 1 nf) "s") "  +" np "  −" nm)}
     :display
     (apply ir-root
       (ir-p (ir-strong "SHOW")
         "  " (ir-code sha*)
         "  " (or author "?")
         (when email (str " <" email ">"))
         (when at (str "  " at)))
       (when subject (ir-p (ir-strong subject)))
       (when (and body (seq (str/trim body)))
         (ir-code-block "text" (cap body)))
       (ir-p (str nf " file" (when (not= 1 nf) "s")
               "  +" np "  −" nm
               (when (and committer (not= committer author))
                 (str "  committer=" committer))))
       (when (seq files)
         (ir-code-block "text" (cap body*)))
       (when (seq (str/trim patch-text))
         (ir-code-block "diff" (cap patch-text))))}))

;; ---------------------------------------------------------------------------
;; git/blame
;; ---------------------------------------------------------------------------

(defn- blame-row [{:keys [line short-sha author content]}]
  (str (format "%5d" (or line 0))
    "  " (pad-right (or short-sha "????????") 9)
    (pad-right (or author "?") 18)
    "  " (or content "")))

(defn render-blame
  [{:keys [path head total ignored-revs lines]}]
  {:summary
   {:left   (ir-strong "BLAME")
    :center (if head
              (ir-p (ir-code (or path "?")) " @" (short-sha head))
              (ir-code (or path "?")))
    :right  (str total " line" (when (not= 1 total) "s")
              (when (seq ignored-revs)
                (str "  ignored=" (count ignored-revs))))}
   :display
   (ir-root
     (ir-p (ir-strong "BLAME")
       "  " (ir-code (or path "?"))
       (when head (str "  @" (short-sha head)))
       "  " total " line" (when (not= 1 total) "s")
       (when (seq ignored-revs)
         (str "  ignored=" (count ignored-revs))))
     (when (seq lines)
       (ir-code-block "text"
         (cap (str/join "\n" (map blame-row lines))))))})

;; ---------------------------------------------------------------------------
;; git/merge-* ops
;; ---------------------------------------------------------------------------

(defn render-merge-status
  [{:keys [in-progress? branch head merge-head conflicts]}]
  (if-not in-progress?
    {:summary {:left (ir-strong "NO MERGE")
               :right "working tree is not mid-merge"}
     :display (ir-root (ir-p (ir-strong "NO MERGE") "  working tree is not mid-merge"))}
    (let [n (count conflicts)]
      {:summary
       {:left   (ir-strong "MERGING")
        :center (ir-code (or branch "?"))
        :right  (str n " conflict" (when (not= 1 n) "s"))}
       :display
       (ir-root
         (ir-p (ir-strong "MERGING")
           "  " (ir-code (or branch "?"))
           (when head (str "  HEAD=" (short-sha head)))
           (when merge-head (str "  MERGE_HEAD=" (short-sha merge-head))))
         (ir-p (str n " conflict" (when (not= 1 n) "s")
                 (when (zero? n) " — ready for (git/merge-continue!)")))
         (when (seq conflicts)
           (ir-code-block "text"
             (cap (str/join "\n"
                    (map (fn [{:keys [path state]}]
                           (str (or state "UU") " " path))
                      conflicts))))))})))

(defn render-merge-op
  "Generic single-path op renderer. Used by accept-ours / accept-theirs /
   mark-resolved — all of which return `{:path :op}`."
  [{:keys [path op]}]
  (let [label (str/upper-case (name (or op :op)))]
    {:summary {:left (ir-strong label)
               :right (ir-code (or path "?"))}
     :display (ir-root
                (ir-p (ir-strong label)
                  "  " (ir-code (or path "?"))))}))

(defn render-merge-continue
  [{:keys [result head message]}]
  {:summary
   {:left  (ir-strong "MERGED")
    :right (ir-code (or (short-sha head) "?"))}
   :display
   (ir-root
     (ir-p (ir-strong "MERGED")
       "  " (ir-code (or (short-sha head) "?"))
       (when message (str "  msg=\"" message "\""))
       (when (and result (not= :continued result))
         (str "  result=" (name result)))))})

(defn render-merge-abort
  [{:keys [result]}]
  {:summary {:left (ir-strong "ABORTED")
             :right "HEAD restored"}
   :display
   (ir-root
     (ir-p (ir-strong "ABORTED")
       "  HEAD restored"
       (when (and result (not= :aborted result))
         (str "  result=" (name result)))))})
