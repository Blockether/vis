(ns com.blockether.vis.ext.language-clojure.render
  "Channel IR renderers for `clj/*` tools.

   Engine contract for `:render-fn` (mirrors foundation-core):

     (fn [result] [:ir {} <block> ...])

   `result` is the raw map returned to SCI as `:result`. The MODEL
   sees that same map via `pr-str` of the SCI return value — these
   renderers ONLY shape the TUI / channel preview, never what the
   LLM reads.

   Style follows `extensions/common/vis-foundation-core/.../editing/core.clj`:
   tiny IR vector builders (`[:ir]`, `[:p]`, `[:c]`, `[:code]`) and a
   single `bounded-render-text` cap on free-form text bodies. Lists
   render as text code blocks so previews stay compact and scrollable."
  (:require
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; IR helpers (local; the foundation-core ones are private)
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

(def ^:private preview-cap
  "Soft byte ceiling on free-form preview bodies. Same intent as
   foundation-core's `bounded-render-text`: protect the TUI from
   pasting an entire 200 KB outline into a channel."
  32000)

(defn- cap [^String s]
  (cond
    (nil? s)             ""
    (<= (count s) preview-cap) s
    :else (str (subs s 0 (- preview-cap 64))
            "\n\n... (preview truncated; full payload in :result)")))

;; ---------------------------------------------------------------------------
;; clj/ports
;; ---------------------------------------------------------------------------

(defn render-ports
  "Preview for `(clj/ports)`.

   When the workspace has exactly ZERO or ONE nREPL port the badge
   line already carries the essential info (`default=7888`) and the
   per-source listing only adds the `.nrepl-port` path — noise the
   user has to scan past. Drop the body in that case so the row
   reads as a single badge headline, consistent with `TITLE` /
   `TASK` / `SPEC` recap rows.

   With TWO or more ports the listing disambiguates which file the
   default came from, so it's worth showing.

   The model still sees the full `:ports` vector via the SCI return
   value — this trim only shapes the channel preview."
  [{:keys [default ports]}]
  (let [n (count ports)]
    (cond-> (ir-root
              (ir-p (ir-strong "PORTS")
                "  " n " visible"
                (when default (str "  default=" default))))
      (> n 1)
      (conj (ir-code-block "text"
              (str/join "\n"
                (map (fn [{:keys [port source]}]
                       (str port "  " source))
                  ports)))))))

;; ---------------------------------------------------------------------------
;; clj/eval
;; ---------------------------------------------------------------------------

(defn render-eval
  [{:keys [value out err ns status ex root-ex ms port timed-out?]}]
  (let [bad?  (or timed-out? ex root-ex (contains? status "error"))
        badge (cond timed-out? "TIMEOUT"
                bad?       "ERROR"
                :else      "EVAL")]
    (ir-root
      (ir-p (ir-strong badge)
        "  :" port
        (when ns (str "  ns=" ns))
        (when (number? ms) (str "  " ms "ms")))
      (when value (ir-code-block "clojure" (cap value)))
      (when (and out (seq out))
        (ir-code-block "text" (str ":out\n" (cap out))))
      (when (and err (seq err))
        (ir-code-block "text" (str ":err\n" (cap err))))
      (when ex
        (ir-p (ir-strong "ex") "  " (ir-code (str ex))
          (when root-ex (str "  root=" root-ex)))))))

;; ---------------------------------------------------------------------------
;; clj/outline
;; ---------------------------------------------------------------------------

(defn- outline-row
  [{:keys [kind name line arglists doc private? dispatch]}]
  (str (format "%4d  " (or line 0))
    (subs (str kind) 1)                  ;; :defn -> "defn"
    (when private? "-")
    "  " name
    (when dispatch (str " " dispatch))
    (when (seq arglists)
      (str "  "
        (str/join " | "
          (map (fn [args] (str "[" (str/join " " args) "]")) arglists))))
    (when doc (str "  ; " doc))))

(defn render-outline
  [{:keys [path bytes ns counts forms total error]}]
  (ir-root
    (ir-p (ir-strong (if error "OUTLINE!" "OUTLINE"))
      "  " (ir-code (or path "?"))
      (cond
        error (str "  " error)
        :else (str "  " total " form" (when (not= total 1) "s")
                (when bytes (str "  " bytes "B")))))
    (when ns
      (ir-p "ns " (ir-code (:name ns))
        (when (:doc ns) (str "  ; " (:doc ns)))))
    (when (seq counts)
      (ir-p (str/join "  "
              (sort (map (fn [[k v]] (str v "×" (subs (str k) 1)))
                      counts)))))
    (when (seq forms)
      (ir-code-block "text"
        (cap (str/join "\n" (map outline-row forms)))))))

;; ---------------------------------------------------------------------------
;; clj/find
;; ---------------------------------------------------------------------------

(defn- find-row
  [{:keys [path line kind name dispatch doc]}]
  (str (format "%-6s " (subs (str kind) 1))
    name
    (when dispatch (str " " dispatch))
    "  " path ":" line
    (when doc (str "  ; " doc))))

(defn render-find
  [{:keys [matches scanned truncated? elapsed-ms]}]
  (let [n (count matches)]
    (ir-root
      (ir-p (ir-strong "FIND")
        "  " n " match" (when (not= n 1) "es")
        "  scanned=" scanned
        "  " elapsed-ms "ms"
        (when truncated? "  (truncated)"))
      (when (seq matches)
        (ir-code-block "text"
          (cap (str/join "\n" (map find-row matches))))))))

;; ---------------------------------------------------------------------------
;; clj/edit
;; ---------------------------------------------------------------------------

(defn render-edit
  [{:keys [status path op target error bytes delta]}]
  (cond
    (= :error status)
    (ir-root
      (ir-p (ir-strong "EDIT FAILED")
        "  " (ir-code (or error "unknown"))
        (when target (str "  target=" target))))

    :else
    (let [{:keys [before after]} (or bytes {})]
      (ir-root
        (ir-p (ir-strong "EDIT")
          "  " (ir-code (str op))
          "  " (ir-code (str target))
          "  → " (or path "?")
          (when (and before after)
            (str "  " before "B→" after "B"
              (when delta
                (str "  Δ=" (if (pos? delta) "+" "") delta)))))))))
