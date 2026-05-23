(ns com.blockether.vis.ext.foundation-git.render
  "Channel IR renderers for `git/*` tools.

   Engine contract for `:render-fn` (mirrors foundation-core/editing):

     (fn [result] [:ir {} <block> ...])

   `result` is the raw map returned to SCI as `:result`. The MODEL
   sees the same map (via `pr-str` of the unwrapped SCI return) —
   these renderers ONLY shape the channel preview, never what the
   LLM reads.

   Style follows `foundation-core/editing/core.clj`: tiny IR vector
   builders (`[:ir]`, `[:p]`, `[:c]`, `[:code]`, `[:strong]`) and a
   single soft byte cap on free-form bodies. Bullet decoration is
   left to the channel: we render rows as plain text inside
   `[:code {:lang \"text\"}]` blocks so previews stay copy-pasteable.

   Status badges use `[:strong]` so the channel can bold them
   (TUI / Telegram both honour the marker)."
  (:require
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; IR builders
;; ---------------------------------------------------------------------------

(defn- ir-code [s] [:c {} (str s)])
(defn- ir-strong [s] [:strong {} (str s)])
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
  (let [n (count entries)]
    (ir-root
      (ir-p (ir-strong (if clean? "CLEAN" "DIRTY"))
        "  " (ir-code (or branch "?"))
        (when head (str "  @" (short-sha head)))
        (when-not clean? (str "  " n " entr" (if (= n 1) "y" "ies"))))
      (when (seq entries)
        (ir-code-block "text"
          (cap (str/join "\n"
                 (map (fn [{:keys [status file]}]
                        (str status " " file))
                   entries))))))))

;; ---------------------------------------------------------------------------
;; git/diff
;; ---------------------------------------------------------------------------

(defn- diff-row [{:keys [file + -]}]
  (str (pad-right (str "+" +) 6)
    (pad-right (str "-" -) 6)
    file))

(defn render-diff
  [{:keys [branch head kind from to path stat files porcelain]}]
  (let [nf  (:files stat)
        np  (:+ stat)
        nm  (:- stat)
        body (str/join "\n" (map diff-row files))
        patches (->> files
                  (filter :patch)
                  (mapcat (fn [{:keys [file patch]}]
                            [[:p {} [:strong {} (str "── " file " ──")]]
                             (ir-code-block "diff" (cap patch))])))]
    (apply ir-root
      (ir-p (ir-strong "DIFF")
        "  " (ir-code (or branch (name (or kind :workspace))))
        "  " (or (short-sha from) (str from)) ".."
        (or (short-sha to) (if to (str to) "WT"))
        (when path (str "  path=" path))
        (when head (str "  @" (short-sha head))))
      (ir-p (str nf " file" (when (not= 1 nf) "s")
              "  +" np "  −" nm))
      (when (seq files)  (ir-code-block "text" (cap body)))
      (when (seq porcelain)
        (ir-code-block "text"
          (cap (str/join "\n"
                 (map (fn [{:keys [status file]}] (str status " " file))
                   porcelain)))))
      patches)))

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
    (ir-root
      (ir-p (ir-strong "LOG")
        "  " (ir-code (or branch "?"))
        "  " n " commit" (when (not= 1 n) "s"))
      (when (seq commits)
        (ir-code-block "text"
          (cap (str/join "\n" (map log-row commits))))))))

;; ---------------------------------------------------------------------------
;; git/show
;; ---------------------------------------------------------------------------

(defn render-show
  [{:keys [short-sha sha author email at subject body files stat committer]}]
  (let [files (or files [])
        nf    (or (:files stat) (count files))
        np    (or (:+ stat) 0)
        nm    (or (:- stat) 0)
        body* (str/join "\n" (map diff-row files))
        patches (->> files
                  (filter :patch)
                  (mapcat (fn [{:keys [file patch]}]
                            [(ir-p (ir-strong (str "── " file " ──")))
                             (ir-code-block "diff" (cap patch))])))]
    (apply ir-root
      (ir-p (ir-strong "SHOW")
        "  " (ir-code (or short-sha (when sha (subs sha 0 (min 8 (count sha)))) "?"))
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
      patches)))

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
  (ir-root
    (ir-p (ir-strong "BLAME")
      "  " (ir-code (or path "?"))
      (when head (str "  @" (short-sha head)))
      "  " total " line" (when (not= 1 total) "s")
      (when (seq ignored-revs)
        (str "  ignored=" (count ignored-revs))))
    (when (seq lines)
      (ir-code-block "text"
        (cap (str/join "\n" (map blame-row lines)))))))

;; ---------------------------------------------------------------------------
;; git/merge-* ops
;; ---------------------------------------------------------------------------

(defn render-merge-status
  [{:keys [in-progress? branch head merge-head conflicts]}]
  (if-not in-progress?
    (ir-root (ir-p (ir-strong "NO MERGE") "  working tree is not mid-merge"))
    (let [n (count conflicts)]
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
                     conflicts)))))))))

(defn render-merge-op
  "Generic single-path op renderer. Used by accept-ours / accept-theirs /
   mark-resolved — all of which return `{:path :op}`."
  [{:keys [path op]}]
  (ir-root
    (ir-p (ir-strong (str/upper-case (name (or op :op))))
      "  " (ir-code (or path "?")))))

(defn render-merge-continue
  [{:keys [result head message]}]
  (ir-root
    (ir-p (ir-strong "MERGED")
      "  " (ir-code (or (short-sha head) "?"))
      (when message (str "  msg=\"" message "\""))
      (when (and result (not= :continued result))
        (str "  result=" (name result))))))

(defn render-merge-abort
  [{:keys [result]}]
  (ir-root
    (ir-p (ir-strong "ABORTED")
      "  HEAD restored"
      (when (and result (not= :aborted result))
        (str "  result=" (name result))))))
