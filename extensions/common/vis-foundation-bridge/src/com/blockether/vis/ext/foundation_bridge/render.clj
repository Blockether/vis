(ns com.blockether.vis.ext.foundation-bridge.render
  "Channel IR renderers for `br/*` tools.

   Engine contract for `:render-fn`:

     (fn [result] [:ir {} <block> ...])

   `result` is the unwrapped `:result` map. The MODEL sees the same
   map via `tool-result->public-value`; these renderers shape ONLY
   the channel/TUI preview.

   Style follows the rest of the foundation surface
   (`foundation-core/editing`, `foundation-git/render`,
   `language-clojure/render`): `[:strong]` badge headers, paragraph
   stats, optional `[:code {:lang \"text\"}]` body for free-form
   lists, soft byte cap on body."
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
    (nil? s)                   ""
    (<= (count s) preview-cap) s
    :else (str (subs s 0 (- preview-cap 64))
            "\n\n... (preview truncated; full payload in :result)")))

(defn- count-of [coll] (count (or coll [])))

;; ---------------------------------------------------------------------------
;; br/init
;; ---------------------------------------------------------------------------

(defn render-init
  [{:keys [configured? already-configured? workspace-root profile-path
           created updated message]}]
  (cond
    (and configured? already-configured?)
    (ir-root
      (ir-p (ir-strong "BRIDGE READY")
        "  " (ir-code (or profile-path "?"))
        (when workspace-root (str "  @ " workspace-root)))
      (when message (ir-p message)))

    configured?
    (let [nc (count-of created)
          nu (count-of updated)]
      (ir-root
        (ir-p (ir-strong "BRIDGE INIT")
          "  " (ir-code (or profile-path "?"))
          (when workspace-root (str "  @ " workspace-root))
          (when (pos? (+ nc nu))
            (str "  created=" nc "  updated=" nu)))
        (when (seq created)
          (ir-code-block "text"
            (cap (str "created:\n" (str/join "\n" (map #(str "  + " %) created))))))
        (when (seq updated)
          (ir-code-block "text"
            (cap (str "updated:\n" (str/join "\n" (map #(str "  ~ " %) updated))))))))

    :else
    (ir-root
      (ir-p (ir-strong "BRIDGE !INIT")
        (when workspace-root (str "  @ " workspace-root)))
      (when message (ir-p message)))))

;; ---------------------------------------------------------------------------
;; br/profile
;; ---------------------------------------------------------------------------

(defn render-profile
  [{:keys [configured? summary profile-path policy-path policy-loaded? message]}]
  (if-not configured?
    (ir-root
      (ir-p (ir-strong "NO PROFILE"))
      (when message (ir-p message)))
    (let [proj (:project summary)
          version (:version summary)
          name'   (:name summary)]
      (ir-root
        (ir-p (ir-strong "PROFILE")
          (when name' (str "  " name'))
          (when version (str "  v" version))
          (when proj (str "  proj=" proj))
          "  policy=" (if policy-loaded? "yes" "no"))
        (when profile-path (ir-p "profile: " (ir-code profile-path)))
        (when policy-path  (ir-p "policy: "  (ir-code policy-path)))))))

;; ---------------------------------------------------------------------------
;; br/check
;; ---------------------------------------------------------------------------

(defn- obligation-row
  [{:keys [kind subject artifact summary state]}]
  (str (format "%-9s " (or state "open"))
    (when kind (str "[" (name kind) "] "))
    (or subject artifact "?")
    (when summary (str "  ; " summary))))

(defn render-check
  [{:keys [configured? status status-summary issue-count
           required-obligations recommended-obligations
           evidence-receipts message]}]
  (cond
    (not configured?)
    (ir-root
      (ir-p (ir-strong "NO PROFILE"))
      (when message (ir-p message)))

    :else
    (let [n-req  (count-of required-obligations)
          n-rec  (count-of recommended-obligations)
          n-rec' (count-of evidence-receipts)
          ic     (or issue-count 0)
          badge  (cond
                   (zero? ic)         "BRIDGE OK"
                   (pos? ic)          "BRIDGE FAIL"
                   :else              "BRIDGE")]
      (ir-root
        (ir-p (ir-strong badge)
          (when status (str "  " (name status)))
          "  issues=" ic
          "  required=" n-req
          "  recommended=" n-rec
          "  receipts=" n-rec')
        (when-let [proj (:project status-summary)]
          (ir-p (str "project: " proj)))
        (when (seq required-obligations)
          (ir-code-block "text"
            (cap (str "required:\n"
                   (str/join "\n" (map #(str "  - " (obligation-row %))
                                    required-obligations))))))
        (when (seq recommended-obligations)
          (ir-code-block "text"
            (cap (str "recommended:\n"
                   (str/join "\n" (map #(str "  - " (obligation-row %))
                                    recommended-obligations))))))))))

;; ---------------------------------------------------------------------------
;; br/next
;; ---------------------------------------------------------------------------

(defn- action-line
  [{:keys [summary op required-evidence]}]
  (str (when op (str (:call op) "  "))
    (or summary "")
    (when (seq required-evidence)
      (str "  evidence=" (str/join "," required-evidence)))))

(defn render-next
  [{:keys [issue-count actions next-step status-summary configured? message]}]
  (cond
    (not configured?)
    (ir-root
      (ir-p (ir-strong "NO PROFILE"))
      (when message (ir-p message)))

    :else
    (let [n (count-of actions)
          ic (or issue-count 0)]
      (ir-root
        (ir-p (ir-strong (if (zero? ic) "BRIDGE OK" "NEXT"))
          "  " n " suggestion" (when (not= 1 n) "s")
          "  issues=" ic)
        (when next-step
          (ir-p "next: " (ir-code (get-in next-step [:op :call] ""))))
        (when (seq actions)
          (ir-code-block "text"
            (cap (str/join "\n" (map #(str "  → " (action-line %)) actions)))))
        (when-let [proj (:project status-summary)]
          (ir-p (str "project: " proj)))))))

;; ---------------------------------------------------------------------------
;; br/list-evidence
;; ---------------------------------------------------------------------------

(defn- evidence-row
  [{:keys [id description command timeout-seconds]}]
  (str (format "%-24s " (or id "?"))
    (when timeout-seconds (str "(" timeout-seconds "s) "))
    (or description command "")))

(defn render-list-evidence
  [{:keys [configured? commands profile-path message]}]
  (cond
    (not configured?)
    (ir-root
      (ir-p (ir-strong "NO PROFILE"))
      (when message (ir-p message)))

    :else
    (let [n (count-of commands)]
      (ir-root
        (ir-p (ir-strong "EVIDENCE")
          "  " n " command" (when (not= 1 n) "s")
          (when profile-path (str "  " profile-path)))
        (when (seq commands)
          (ir-code-block "text"
            (cap (str/join "\n" (map evidence-row commands)))))))))

;; ---------------------------------------------------------------------------
;; br/run-evidence
;; ---------------------------------------------------------------------------

(defn render-run-evidence
  [{:keys [profile-path result]}]
  (let [{:keys [id status duration-ms receipt-path stdout stderr exit-code]} (or result {})
        ok?   (= "passed" status)
        bad?  (= "failed" status)
        badge (cond ok? "EVIDENCE OK" bad? "EVIDENCE FAIL" :else "EVIDENCE")]
    (ir-root
      (ir-p (ir-strong badge)
        (when id (str "  " id))
        (when status (str "  " status))
        (when (number? duration-ms) (str "  " duration-ms "ms"))
        (when (number? exit-code)   (str "  exit=" exit-code)))
      (when receipt-path (ir-p "receipt: " (ir-code receipt-path)))
      (when profile-path (ir-p "profile: " (ir-code profile-path)))
      (when (and stdout (seq stdout))
        (ir-code-block "text" (str ":stdout\n" (cap stdout))))
      (when (and stderr (seq stderr))
        (ir-code-block "text" (str ":stderr\n" (cap stderr)))))))
