(ns com.blockether.vis.ext.language-clojure.render
  "Channel renderers for `clj/*` tools.

   Engine contract for `:render-fn` (the `{:summary :display}` contract,
   `:com.blockether.vis.internal.extension/render-fn-result`):

     (fn [result] {:summary <summary> :display <ir>})

   `summary` is EITHER a zone map `{:left <ir-or-string> :center … :right …}`
   (used when the result has a natural label + right-anchored metric — counts,
   durations, byte deltas, ports) OR a single `[:p ...]`-bearing IR root whose
   first `[:strong ...]` is the badge label. `display` is the full canonical
   `[:ir ...]` body these renderers return (code blocks, listings).

   `result` is the raw map returned to Python as `:result`. The MODEL sees that
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

(def ^:private cap extension/cap-preview)

(defn- strip-ansi
  "Strip ANSI escape sequences from a string before it lands in a preview code
   block, so test logs render as plain text in every channel (web + TUI) -
   including results persisted before the test-runner stripped at capture.
   nil-safe."
  [s]
  (when s
    (str/replace s #"\u001b\[[0-9;]*[A-Za-z]" "")))

;; ---------------------------------------------------------------------------
;; clj/ports
;; ---------------------------------------------------------------------------

(defn render-ports
  "Preview for `clj_ports()`.

   Summary is a zone badge: `PORTS` label on the left, the visible count
   right-anchored, default port in the center when known. With ZERO or ONE
   port the display is just the badge headline (the count + default already
   carry everything — the per-source `.nrepl-port` path is noise). With TWO
   or more ports the display lists each port→source so the user can see which
   file the default came from.

   The model still sees the full `:ports` vector via the Python return value —
   this trim only shapes the channel preview."
  [{:keys [default ports]}]
  (let [n (count ports)]
    {:summary (cond-> {:left  (ir-strong "PORTS")
                       :right (str n " visible")}
                default (assoc :center (ir-code (str "default=" default))))
     :display (ir-root
                (when (> n 1)
                  (ir-code-block "text"
                    (str/join "\n"
                      (map (fn [{:keys [port source]}]
                             (str port "  " source))
                        ports)))))}))

;; ---------------------------------------------------------------------------
;; clj/repl
;; ---------------------------------------------------------------------------

(defn render-repl
  "Preview for `clj_repl(…)`.

   Summary is a zone badge: `REPL` (+ a glyph for the action outcome) on the
   left, the result keyword (+ build tool) in the center, the visible port
   count right-anchored. Display carries any message and the port→source list."
  [{:keys [result managed ports tool port message]}]
  (let [badge   (case result
                  (:started :already-running) "REPL ↑"
                  :starting                   "REPL …"
                  :stopped                    "REPL ✕"
                  :no-launcher                "REPL ?"
                  "REPL")
        running? (boolean (:running managed))
        n        (count ports)
        center   (str (when result (subs (str result) 1))
                   (when-let [t (or tool (:tool managed))] (str " " (name t)))
                   (when port (str "  :" port))
                   (when (and (= result :status) running?) "  ●"))]
    {:summary (cond-> {:left  (ir-strong badge)
                       :right (str n " port" (when (not= n 1) "s"))}
                (seq center) (assoc :center (ir-code center)))
     :display (ir-root
                (when message (ir-p message))
                (when (seq ports)
                  (ir-code-block "text"
                    (str/join "\n"
                      (map (fn [{:keys [port source]}] (str port "  " source))
                        ports)))))}))

;; ---------------------------------------------------------------------------
;; clj/eval
;; ---------------------------------------------------------------------------

(defn render-eval
  "Preview for `clj_eval(...)`.

   Summary is a zone badge: `EVAL` / `TIMEOUT` / `ERROR` label, ns in the
   center, port + elapsed ms right-anchored. Display carries the value, any
   `:out` capture, and ONE consolidated error block: the `:err` text already
   names the exception (`Execution error (ExceptionInfo) at ...`), so the
   `ex` / `root-ex` classes are appended to the SAME block only when they add
   information the text does not carry - never a second bubble. A bare `nil`
   value on a failed eval is noise and is dropped."
  [{:keys [value out err ns status ex root-ex ms port timed_out]}]
  (let [bad?      (or timed_out ex root-ex (contains? status "error"))
        badge     (cond timed_out "TIMEOUT"
                    bad?       "ERROR"
                    :else      "EVAL")
        right     (str ":" port (when (number? ms) (str "  " ms "ms")))
        err-text  (when (and err (seq err)) (str/trimr err))
        ;; simple class name (`ExceptionInfo` out of `class clojure.lang.ExceptionInfo`)
        simple    (fn [c] (when c (last (str/split (str c) #"[. ]"))))
        ex-new?   (and ex (not (and err-text
                                 (str/includes? err-text (str (simple ex))))))
        root-new? (and root-ex (not= (str root-ex) (str ex))
                    (not (and err-text
                           (str/includes? err-text (str (simple root-ex))))))
        ex-line   (when (or ex-new? root-new?)
                    (str "ex " ex (when root-new? (str "  root=" root-ex))))
        err-blob  (when (or err-text ex-line)
                    (str/join "\n" (remove nil? [err-text ex-line])))]
    {:summary (cond-> {:left  (ir-strong badge)
                       :right right}
                ns (assoc :center (ir-code (str "ns=" ns))))
     :display (ir-root
                ;; `:wrap?` soft-folds a pathologically wide one-line value
                ;; (a wide map/vector/string) at the bubble edge instead of
                ;; letting it overflow; normal multi-line values stay verbatim.
                (when (and value (not (and bad? (= value "nil"))))
                  (ir-code-block "clojure" (cap value) {:wrap? true}))
                (when (and out (seq out))
                  (ir-code-block "text" (str ":out\n" (cap out))))
                (when err-blob
                  (ir-code-block "text" (str ":err\n" (cap err-blob)))))}))

;; ---------------------------------------------------------------------------
;; clj/edit
;; ---------------------------------------------------------------------------

(defn render-edit
  "Preview for `clj_edit(…)`.

   On success the summary is a zone badge: `EDIT` label, the edit-op + target
   in the center, the byte delta (`+N` / `-N`) right-anchored — and the display
   expands to the full unified diff, the same `git diff`-style view `v/patch`
   shows (not just a byte-delta scalar). On failure the summary is `EDIT FAILED`
   with the error string right-anchored."
  [{:keys [status path edit-op target error bytes delta diff]}]
  (cond
    (= :error status)
    {:summary {:left  (ir-strong "EDIT FAILED")
               :right (ir-code (or error "unknown"))}
     ;; The summary badge already reads `EDIT FAILED  <error>`; the expanded
     ;; body must NOT restate it (that printed the message twice). Carry only
     ;; the extra context — the target form — and nothing when there's no
     ;; target, so the row shows the failure exactly once.
     :display (ir-root
                (when target
                  (ir-p "target " (ir-code (str target)))))}

    :else
    (let [{:keys [before after]} (or bytes {})
          header (str (or edit-op "edit") " " (or path "?")
                   (when delta (str "  Δ=" (if (pos? delta) "+" "") delta)))]
      {:summary (cond-> {:left   (ir-strong "EDIT")
                         :center (ir-code (str edit-op "  " target))}
                  (and before after)
                  (assoc :right
                    (str before "B→" after "B"
                      (when delta
                        (str "  Δ=" (if (pos? delta) "+" "") delta)))))
       :display (cond-> (ir-root (ir-p (ir-code header)))
                  (seq diff) (conj (ir-code-block "diff" (cap diff))))})))

(defn render-paren-repair
  "Preview for `clj_paren_repair(…)`: a one-line badge stating whether the
   delimiters needed fixing — REPAIRED (parinfer changed it), OK (already
   balanced), or UNFIXABLE (parinfer could not produce a clean parse)."
  [{:keys [repaired? changed?]}]
  (let [label (cond (not repaired?) "CLOJURE PARENS UNFIXABLE"
                changed?        "CLOJURE PARENS REPAIRED"
                :else           "CLOJURE PARENS OK")]
    {:summary {:left (ir-strong label)}
     :display (ir-root (ir-p (ir-strong label)))}))

(defn render-format
  "Preview for `clj_format(…)`: a one-line badge — FORMATTED when cljfmt
   changed the source, OK when it was already clean."
  [{:keys [changed?]}]
  (let [label (if changed? "CLOJURE FORMATTED" "CLOJURE FORMAT OK")]
    {:summary {:left (ir-strong label)}
     :display (ir-root (ir-p (ir-strong label)))}))

(defn render-test "Preview for `clj_test(...)`. Green/red badge with pass/total; when selectors\n   filtered tests the right metric also shows `k skipped`; failures cite\n   file:line plus the captured run log (errors + output); the run :mode (repl or\n   cli) + framework ride in the center." [{:keys [mode framework ns total pass fail selected skipped failures errors exit output error note]}] (let [bad? (or (and (number? fail) (pos? fail)) (and (number? exit) (not (zero? exit))) error) badge (cond error "TEST ERROR" bad? "TEST FAIL" :else "TEST OK") right (cond (number? total) (str pass "/" total " pass" (when (and (number? fail) (pos? fail)) (str "  " fail " fail")) (when (and (number? skipped) (pos? skipped)) (str "  " skipped " skipped"))) (number? exit) (str "exit " exit) :else "") center (str (or framework mode "") (when ns (str "  " ns)) (when (and (number? selected) (number? skipped) (pos? skipped)) (str "  " selected " selected")))] {:summary (cond-> {:left (ir-strong badge), :right right} (seq center) (assoc :center (ir-code center))), :display (ir-root (when (seq failures) (ir-code-block "text" (cap (str/join "\n" (map (fn [f] (str (:type f) "  " (:test f) "  " (:file f) ":" (:line f) "\n    " (:message f))) failures))))) (when (and output (seq output)) (ir-code-block "text" (cap (strip-ansi output)))) (when note (ir-p note)) (when error (ir-p (ir-strong "error") "  " (str error))))}))


