(ns com.blockether.vis.ext.foundation-git.render
  "Channel IR renderers for `git/*` tools.

   Engine contract for `:render-fn` / `:render-error-fn` (Phase 1 hard
   cut, mirrors every other extension):

     (fn [result] {:summary <ir-or-zones> :display <ir>})

   `result` is the raw map returned to Python as `:result`. The MODEL
   sees the same map (via `pr-str` of the unwrapped Python return) —
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

(def ^:private status-code-order ["A" "M" "D" "??" "UU"])

(defn render-status
  [{:keys [branch head changes]}]
  ;; clean ⇔ empty :changes — the tool result no longer carries a
  ;; derivable boolean (prompt-diet; the trailer rides every prompt).
  (let [clean? (empty? changes)
        n      (reduce + 0 (map count (vals changes)))
        label  (if clean? "CLEAN" "DIRTY")]
    {:summary
     (cond-> {:left  (ir-strong label)
              :center (ir-code (or branch "?"))}
       head        (assoc :center
                     (ir-p (ir-code (or branch "?")) " @" (short-sha head)))
       (not clean?) (assoc :right (str n " entr" (if (= n 1) "y" "ies"))))
     ;; Body ONLY — label / branch / @head / count are already on the summary
     ;; op-row; repeating them here renders the header twice. GROUPED BY status
     ;; code: each code stated once as a header, its files listed beneath.
     :display
     (ir-root
       (when (seq changes)
         (ir-code-block "text"
           (cap (str/join "\n"
                  (for [code  status-code-order
                        :let  [files (get changes code)]
                        :when (seq files)
                        line  (cons (str code)
                                (map #(str "  " %) files))]
                    line))))))}))

;; ---------------------------------------------------------------------------
;; git/diff
;; ---------------------------------------------------------------------------

(defn- diff-row [{:keys [file + -]}]
  (str (pad-right (str "+" +) 6)
    (pad-right (str "-" -) 6)
    file))

(defn render-diff
  [{:keys [branch head kind from to path stat files untracked]}]
  (let [nf    (:files stat)
        np    (:+ stat)
        nm    (:- stat)
        scope (or branch (name (or kind :workspace)))
        range (str (or (short-sha from) (str from)) ".."
                (or (short-sha to) (if to (str to) "WT")))
        ;; ONE file overview. The numstat carries the tracked changes (+/-);
        ;; the result's :untracked paths (already de-duped against numstat
        ;; by the tool) are appended so nothing is listed twice.
        overview  (str/join "\n"
                    (concat
                      (map diff-row files)
                      (map #(str "?? " %) untracked)))
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

(defn- fmt-at [at]
  (cond
    (nil? at) nil
    ;; Epoch millis -> human date. A non-number `at` (already-formatted
    ;; date string) passes through verbatim; never `(long ...)` a String.
    (number? at)
    (-> (java.time.Instant/ofEpochMilli (long at))
      (.atZone (java.time.ZoneId/systemDefault))
      (.format (java.time.format.DateTimeFormatter/ofPattern "MMM d HH:mm")))
    :else (str at)))

(defn- log-row [show-author? {:keys [short-sha author at subject]}]
  (str (pad-right short-sha 9)
    (pad-right (or (fmt-at at) "") 14)
    (when show-author? (pad-right (or author "?") 20))
    (or subject "")))

(defn render-log
  [{:keys [branch commits]}]
  (let [n        (count commits)
        authors  (distinct (keep :author commits))
        single?  (= 1 (count authors))]
    {:summary
     {:left   (ir-strong "LOG")
      :center (ir-code (or branch "?"))
      :right  (str n " commit" (when (not= 1 n) "s")
                ;; one author for the whole range -> name it once here
                ;; instead of repeating it on every row.
                (when single? (str " \u00b7 " (first authors))))}
     ;; Body ONLY — LOG / branch / count / lone-author are on the summary op-row.
     ;; Rows: sha + human date (+ author only when the range is multi-author).
     :display
     (ir-root
       (when (seq commits)
         (ir-code-block "text"
           (cap (str/join "\n" (map (partial log-row (not single?)) commits))))))}))

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
     ;; Body ONLY — SHOW, sha, subject and the +/- counts are on the summary
     ;; op-row. The display adds the author/date (and committer when it
     ;; differs), the commit message body, the file stat, and the patches.
     :display
     (apply ir-root
       (concat
         [(ir-p (str (or author "?")
                  (when email (str " <" email ">"))
                  (when at (str "  " at))
                  (when (and committer (not= committer author))
                    (str "  committer=" committer))))]
         (when (and body (seq (str/trim body)))
           [(ir-code-block "text" (cap body))])
         (when (seq files)
           [(ir-code-block "text" (cap body*))])
         (when (seq (str/trim patch-text))
           [(ir-code-block "diff" (cap patch-text))])))}))

;; ---------------------------------------------------------------------------
;; git/blame
;; ---------------------------------------------------------------------------

(defn- blame-row
  "One blame line in the `N│ <short-sha> <content>` style. The new result
   shape keeps only {:line :sha :content} per line — :sha is the SHORT sha
   that keys the commit legend, so the row references the legend instead of
   repeating author/email/at on every line."
  [{:keys [line sha content]}]
  (str (format "%5d" (or line 0))
    "│ " (pad-right (or sha "????????") 9)
    " " (or content "")))

(defn- legend-row
  "One commit-legend entry: `<short-sha>  <author>  <date>`. Stated ONCE per
   distinct commit; the per-line rows reference it by short-sha."
  [[sha {:keys [author at]}]]
  (str (pad-right (or sha "????????") 9)
    "  " (pad-right (or author "?") 20)
    (or (fmt-at at) "")))

(defn render-blame
  [{:keys [path head total ignored-revs commits lines]}]
  {:summary
   {:left   (ir-strong "BLAME")
    :center (if head
              (ir-p (ir-code (or path "?")) " @" (short-sha head))
              (ir-code (or path "?")))
    :right  (str total " line" (when (not= 1 total) "s")
              (when (seq ignored-revs)
                (str "  ignored=" (count ignored-revs))))}
   ;; Body ONLY — BLAME / path / @head / line count are on the summary op-row.
   ;; The commit legend is rendered ONCE (short-sha -> author/date); the
   ;; per-line rows below reference it by short-sha (the `N│ <sha> <content>`
   ;; style) instead of repeating commit identity on every line.
   :display
   (apply ir-root
     (concat
       (when (seq commits)
         [(ir-code-block "text"
            (cap (str/join "\n" (map legend-row (sort-by key commits)))))])
       (when (seq lines)
         [(ir-code-block "text"
            (cap (str/join "\n" (map blame-row lines))))])))})

;; ---------------------------------------------------------------------------
;; git/merge-* ops
;; ---------------------------------------------------------------------------

(defn render-merge-status
  [{:keys [in-progress? branch head merge-head conflicts]}]
  (if-not in-progress?
    ;; Nothing to expand — the summary op-row already says it all.
    {:summary {:left (ir-strong "NO MERGE")
               :right "working tree is not mid-merge"}
     :display (ir-root)}
    (let [n (count conflicts)]
      {:summary
       {:left   (ir-strong "MERGING")
        :center (ir-code (or branch "?"))
        :right  (str n " conflict" (when (not= 1 n) "s"))}
       ;; Body ONLY — MERGING / branch / conflict count are on the summary.
       :display
       (apply ir-root
         (concat
           (when (or head merge-head)
             [(ir-p (str/join "  "
                      (remove nil?
                        [(when head (str "HEAD=" (short-sha head)))
                         (when merge-head (str "MERGE_HEAD=" (short-sha merge-head)))])))])
           (when (zero? n)
             [(ir-p "ready for git_merge_continue()")])
           (when (seq conflicts)
             [(ir-code-block "text"
                (cap (str/join "\n"
                       (map (fn [{:keys [path state]}]
                              (str (or state "UU") " " path))
                         conflicts))))])))})))

(defn render-merge-op
  "Generic single-path op renderer. Used by accept-ours / accept-theirs /
   mark-resolved — all of which return `{:path :op}`."
  [{:keys [path op]}]
  (let [label (str/upper-case (name (or op :op)))]
    {:summary {:left (ir-strong label)
               :right (ir-code (or path "?"))}
     ;; Label + path are on the summary op-row; nothing more to expand.
     :display (ir-root)}))

(defn render-merge-continue
  [{:keys [result head message]}]
  {:summary
   {:left  (ir-strong "MERGED")
    :right (ir-code (or (short-sha head) "?"))}
   ;; Body ONLY — MERGED + sha are on the summary op-row.
   :display
   (apply ir-root
     (let [extra (str/join "  "
                   (remove nil?
                     [(when message (str "msg=\"" message "\""))
                      (when (and result (not= :continued result))
                        (str "result=" (name result)))]))]
       (when (seq extra) [(ir-p extra)])))})

(defn render-merge-abort
  [{:keys [result]}]
  {:summary {:left (ir-strong "ABORTED")
             :right "HEAD restored"}
   ;; Body ONLY — ABORTED + "HEAD restored" are on the summary op-row.
   :display
   (apply ir-root
     (when (and result (not= :aborted result))
       [(ir-p (str "result=" (name result)))]))}) (defn render-merge
                                                    "Preview for git_merge: MERGED / fast-forward / conflict badge plus the
   conflicting (or failing) paths when the merge did not land clean."
                                                    [{:keys [status merged? branch head merge-head conflicts failing-paths hint]}]
                                                    (let [conflict? (or (seq conflicts) (seq failing-paths))
                                                          label (cond merged? "MERGED"
                                                                  conflict? "MERGE CONFLICT"
                                                                  :else "MERGE")]
                                                      {:summary
                                                       {:left (ir-strong label)
                                                        :center (ir-code (or branch "?"))
                                                        :right (or (some-> head short-sha) (str status))}
                                                       :display
                                                       (apply ir-root
                                                         (concat
                                                           [(ir-p (str "status=" status
                                                                    (when merge-head (str "  merge-head=" (short-sha merge-head)))))]
                                                           (when (seq conflicts)
                                                             [(ir-code-block "text"
                                                                (cap (str/join "\n"
                                                                       (map (fn [{:keys [path state]}] (str (or state "UU") " " path))
                                                                         conflicts))))])
                                                           (when (seq failing-paths)
                                                             [(ir-code-block "text"
                                                                (cap (str/join "\n"
                                                                       (map (fn [[p r]] (str r " " p)) failing-paths))))])
                                                           (when hint [(ir-p hint)])))}))


