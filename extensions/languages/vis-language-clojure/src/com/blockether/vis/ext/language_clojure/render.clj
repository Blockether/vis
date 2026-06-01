(ns com.blockether.vis.ext.language-clojure.render
  "Channel renderers for `clj/*` tools.

   Engine contract for `:render-fn` (the `{:summary :display}` contract,
   `:com.blockether.vis.internal.extension/render-fn-result`):

     (fn [result] {:summary <summary> :display <ir>})

   `summary` is EITHER a zone map `{:left <ir-or-string> :center? … :right? …}`
   (used when the result has a natural label + right-anchored metric — counts,
   durations, byte deltas, ports) OR a single `[:p ...]`-bearing IR root whose
   first `[:strong ...]` is the badge label. `display` is the full canonical
   `[:ir ...]` body these renderers used to return (code blocks, listings).

   `result` is the raw map returned to SCI as `:result`. The MODEL sees that
   same map via `pr-str` — these renderers ONLY shape the TUI / channel
   preview, never what the LLM reads.

   IR builders come from the engine extension namespace so the emitted shape is
   exactly the canonical one the contract spec validates. Lists render as text
   code blocks so previews stay compact and scrollable."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]))

;; ---------------------------------------------------------------------------
;; IR helpers (canonical engine builders)
;; ---------------------------------------------------------------------------

(def ^:private ir-code extension/ir-code)
(def ^:private ir-strong extension/ir-strong)
(def ^:private ir-code-block extension/ir-code-block)
(def ^:private ir-p extension/ir-p)
(def ^:private ir-root extension/ir-root)

(def ^:private preview-cap
  "Soft byte ceiling on free-form preview bodies. Protect the TUI from
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

   Summary is a zone badge: `PORTS` label on the left, the visible count
   right-anchored, default port in the center when known. With ZERO or ONE
   port the display is just the badge headline (the count + default already
   carry everything — the per-source `.nrepl-port` path is noise). With TWO
   or more ports the display lists each port→source so the user can see which
   file the default came from.

   The model still sees the full `:ports` vector via the SCI return value —
   this trim only shapes the channel preview."
  [{:keys [default ports]}]
  (let [n (count ports)]
    {:summary (cond-> {:left  (ir-strong "PORTS")
                       :right (str n " visible")}
                default (assoc :center (ir-code (str "default=" default))))
     :display (when (> n 1)
                (ir-root
                  (ir-code-block "text"
                    (str/join "\n"
                      (map (fn [{:keys [port source]}]
                             (str port "  " source))
                        ports)))))}))

;; ---------------------------------------------------------------------------
;; clj/eval
;; ---------------------------------------------------------------------------

(defn render-eval
  "Preview for `(clj/eval …)`.

   Summary is a zone badge: `EVAL` / `TIMEOUT` / `ERROR` label, ns in the
   center, port + elapsed ms right-anchored. Display carries the value, any
   `:out` / `:err` capture and the exception class."
  [{:keys [value out err ns status ex root-ex ms port timed-out?]}]
  (let [bad?  (or timed-out? ex root-ex (contains? status "error"))
        badge (cond timed-out? "TIMEOUT"
                bad?       "ERROR"
                :else      "EVAL")
        right (str ":" port (when (number? ms) (str "  " ms "ms")))]
    {:summary (cond-> {:left  (ir-strong badge)
                       :right right}
                ns (assoc :center (ir-code (str "ns=" ns))))
     :display (ir-root
                (when value (ir-code-block "clojure" (cap value)))
                (when (and out (seq out))
                  (ir-code-block "text" (str ":out\n" (cap out))))
                (when (and err (seq err))
                  (ir-code-block "text" (str ":err\n" (cap err))))
                (when ex
                  (ir-p (ir-strong "ex") "  " (ir-code (str ex))
                    (when root-ex (str "  root=" root-ex)))))}))

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
  "Preview for `(clj/outline …)`.

   Summary is a zone badge: `OUTLINE` (or `OUTLINE!` on error) label, the
   file path in the center, the form count (or error string) right-anchored.
   Display carries the ns line, kind counts and the per-form listing."
  [{:keys [path bytes ns counts forms total error]}]
  {:summary (let [label (if error "OUTLINE!" "OUTLINE")]
              (cond-> {:left (ir-strong label)}
                path  (assoc :center (ir-code path))
                true  (assoc :right
                        (if error
                          (str error)
                          (str total " form" (when (not= total 1) "s")
                            (when bytes (str "  " bytes "B")))))))
   :display (ir-root
              (when ns
                (ir-p "ns " (ir-code (:name ns))
                  (when (:doc ns) (str "  ; " (:doc ns)))))
              (when (seq counts)
                (ir-p (str/join "  "
                        (sort (map (fn [[k v]] (str v "×" (subs (str k) 1)))
                                counts)))))
              (when (seq forms)
                (ir-code-block "text"
                  (cap (str/join "\n" (map outline-row forms))))))})

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
  "Preview for `(clj/find …)`.

   Summary is a zone badge: `FIND` label, the scanned/elapsed stats in the
   center, the match count right-anchored. Display carries the match listing."
  [{:keys [matches scanned truncated? elapsed-ms]}]
  (let [n (count matches)]
    {:summary {:left   (ir-strong "FIND")
               :center (ir-code (str "scanned=" scanned "  " elapsed-ms "ms"))
               :right  (str n " match" (when (not= n 1) "es")
                         (when truncated? " (truncated)"))}
     :display (ir-root
                (when (seq matches)
                  (ir-code-block "text"
                    (cap (str/join "\n" (map find-row matches))))))}))

;; ---------------------------------------------------------------------------
;; clj/edit
;; ---------------------------------------------------------------------------

(defn render-edit
  "Preview for `(clj/edit …)`.

   On success the summary is a zone badge: `EDIT` label, the edit-op + target
   in the center, the byte delta (`+N` / `-N`) right-anchored. On failure the
   summary is `EDIT FAILED` with the error string right-anchored. Display
   carries the full headline either way."
  [{:keys [status path edit-op target error bytes delta]}]
  (cond
    (= :error status)
    {:summary {:left  (ir-strong "EDIT FAILED")
               :right (ir-code (or error "unknown"))}
     :display (when target
                (ir-root
                  (ir-p "target " (ir-code (str target)))))}

    :else
    (let [{:keys [before after]} (or bytes {})]
      {:summary (cond-> {:left   (ir-strong "EDIT")
                         :center (ir-code (str edit-op "  " target))}
                  (and before after)
                  (assoc :right
                    (str before "B→" after "B"
                      (when delta
                        (str "  Δ=" (if (pos? delta) "+" "") delta)))))
       :display (when path
                  (ir-root
                    (ir-p "→ " (ir-code (str path)))))})))
