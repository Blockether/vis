(ns com.blockether.vis.ext.foundation-bridge.render
  "Channel IR renderers for `br/*` tools.

   Engine contract for `:render-fn`:

     (fn [result] {:summary <ir-or-zones> :display <ir>})

   `result` is the unwrapped `:result` map. The MODEL sees the same
   map via `tool-result->public-value`; these renderers shape ONLY
   the channel/TUI preview.

   `:summary` is the single badge/op row. Where a result has a
   natural label + right-anchored metric (counts, durations, line
   counts, sha) we use a zone map `{:left … :center? … :right?}`;
   otherwise a single `[:p …]` IR paragraph whose first `[:strong …]`
   is the label. `:display` is the full expanded IR body.

   Style follows the rest of the foundation surface
   (`foundation-core/editing`, `foundation-git/render`,
   `language-clojure/render`): `[:strong]` badge headers, paragraph
   stats, optional `[:code {:lang \"text\"}]` body for free-form
   lists, soft byte cap on body."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as ext]))

;; ---------------------------------------------------------------------------
;; IR builders (canonical helpers from the engine)
;; ---------------------------------------------------------------------------

(def ^:private ir-code ext/ir-code)
(def ^:private ir-strong ext/ir-strong)
(defn- ir-code-block
  ([body] (ext/ir-code-block "text" body))
  ([lang body] (ext/ir-code-block lang body)))
(def ^:private ir-p ext/ir-p)
(def ^:private ir-root ext/ir-root)

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
    {:summary {:left  (ir-strong "BRIDGE READY")
               :right (ir-code (or profile-path "?"))}
     :display (ir-root
                (ir-p (ir-strong "BRIDGE READY")
                  "  " (ir-code (or profile-path "?"))
                  (when workspace-root (str "  @ " workspace-root)))
                (when message (ir-p message)))}

    configured?
    (let [nc (count-of created)
          nu (count-of updated)]
      {:summary {:left  (ir-strong "BRIDGE INIT")
                 :right (str "+" nc " created  ~" nu " updated")}
       :display (ir-root
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
                      (cap (str "updated:\n" (str/join "\n" (map #(str "  ~ " %) updated)))))))})

    :else
    {:summary {:left (ir-strong "BRIDGE !INIT")}
     :display (ir-root
                (ir-p (ir-strong "BRIDGE !INIT")
                  (when workspace-root (str "  @ " workspace-root)))
                (when message (ir-p message)))}))

;; ---------------------------------------------------------------------------
;; br/profile
;; ---------------------------------------------------------------------------

(defn render-profile
  [{:keys [configured? summary profile-path policy-path policy-loaded? message]}]
  (if-not configured?
    {:summary {:left (ir-strong "NO PROFILE")}
     :display (ir-root
                (ir-p (ir-strong "NO PROFILE"))
                (when message (ir-p message)))}
    (let [proj (:project summary)
          version (:version summary)
          name'   (:name summary)]
      {:summary {:left   (ir-strong "PROFILE")
                 :center (ir-code (or name' proj "?"))
                 :right  (str (when version (str "v" version "  "))
                           "policy=" (if policy-loaded? "yes" "no"))}
       :display (ir-root
                  (ir-p (ir-strong "PROFILE")
                    (when name' (str "  " name'))
                    (when version (str "  v" version))
                    (when proj (str "  proj=" proj))
                    "  policy=" (if policy-loaded? "yes" "no"))
                  (when profile-path (ir-p "profile: " (ir-code profile-path)))
                  (when policy-path  (ir-p "policy: "  (ir-code policy-path))))})))

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
    {:summary {:left (ir-strong "NO PROFILE")}
     :display (ir-root
                (ir-p (ir-strong "NO PROFILE"))
                (when message (ir-p message)))}

    :else
    (let [n-req  (count-of required-obligations)
          n-rec  (count-of recommended-obligations)
          n-rec' (count-of evidence-receipts)
          ic     (or issue-count 0)
          badge  (cond
                   (zero? ic)         "BRIDGE OK"
                   (pos? ic)          "BRIDGE FAIL"
                   :else              "BRIDGE")]
      {:summary (cond-> {:left  (ir-strong badge)
                         :right (str "issues=" ic "  required=" n-req
                                  "  recommended=" n-rec "  receipts=" n-rec')}
                  status (assoc :center (ir-code (name status))))
       :display (ir-root
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
                                              recommended-obligations)))))))})))

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
    {:summary {:left (ir-strong "NO PROFILE")}
     :display (ir-root
                (ir-p (ir-strong "NO PROFILE"))
                (when message (ir-p message)))}

    :else
    (let [n  (count-of actions)
          ic (or issue-count 0)
          badge (if (zero? ic) "BRIDGE OK" "NEXT")]
      {:summary {:left   (ir-strong badge)
                 :center (ir-code (str n " suggestion" (when (not= 1 n) "s")))
                 :right  (str "issues=" ic)}
       :display (ir-root
                  (ir-p (ir-strong badge)
                    "  " n " suggestion" (when (not= 1 n) "s")
                    "  issues=" ic)
                  (when next-step
                    (ir-p "next: " (ir-code (get-in next-step [:op :call] ""))))
                  (when (seq actions)
                    (ir-code-block "text"
                      (cap (str/join "\n" (map #(str "  → " (action-line %)) actions)))))
                  (when-let [proj (:project status-summary)]
                    (ir-p (str "project: " proj))))})))

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
    {:summary {:left (ir-strong "NO PROFILE")}
     :display (ir-root
                (ir-p (ir-strong "NO PROFILE"))
                (when message (ir-p message)))}

    :else
    (let [n (count-of commands)]
      {:summary {:left  (ir-strong "EVIDENCE")
                 :right (str n " command" (when (not= 1 n) "s"))}
       :display (ir-root
                  (ir-p (ir-strong "EVIDENCE")
                    "  " n " command" (when (not= 1 n) "s")
                    (when profile-path (str "  " profile-path)))
                  (when (seq commands)
                    (ir-code-block "text"
                      (cap (str/join "\n" (map evidence-row commands))))))})))

;; ---------------------------------------------------------------------------
;; br/run-evidence
;; ---------------------------------------------------------------------------

(defn render-run-evidence
  [{:keys [profile-path result]}]
  (let [{:keys [id status duration-ms receipt-path stdout stderr exit-code]} (or result {})
        ok?   (= "passed" status)
        bad?  (= "failed" status)
        badge (cond ok? "EVIDENCE OK" bad? "EVIDENCE FAIL" :else "EVIDENCE")]
    {:summary (let [right (str/join "  "
                            (remove nil?
                              [(when status status)
                               (when (number? duration-ms) (str duration-ms "ms"))
                               (when (number? exit-code) (str "exit=" exit-code))]))]
                (cond-> {:left (ir-strong badge)}
                  id          (assoc :center (ir-code id))
                  (seq right) (assoc :right right)))
     :display (ir-root
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
                  (ir-code-block "text" (str ":stderr\n" (cap stderr)))))}))
