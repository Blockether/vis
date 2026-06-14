(ns com.blockether.vis.dev.provider-report
  "Render persisted provider requests for one session as an HTML audit report."
  (:require
   [charred.api :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.tokens :as tokens]))

(defn- parse-session-id
  [s]
  (cond
    (nil? s) nil
    (= ":latest" s) :latest
    (= "latest" s) :latest
    :else (java.util.UUID/fromString (str s))))

(defn- escape-html
  [s]
  (-> (str (or s ""))
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")
    (str/replace "'" "&#39;")))

(defn- fmt-int
  [n]
  (format "%,d" (long (or n 0))))

(defn- fmt-usd
  [n]
  (format "$%.6f" (double (or n 0.0))))

(defn- json-str
  [v]
  (try
    (json/write-json-str v :indent-str "  ")
    (catch Throwable _
      (pr-str v))))

(defn- content-text
  [content]
  (cond
    (string? content) content
    (sequential? content)
    (or (some (fn [part]
                (cond
                  (string? part) part
                  (map? part) (or (:text part) (:content part))))
          content)
      (json-str content))
    :else (json-str content)))

(defn- results-scope
  [text]
  (some-> (re-find #"<results(?: scope=\"([^\"]+)\")?" text) second))

(defn- current-user-message?
  [text]
  (str/includes? text ";; -- CURRENT-USER-MESSAGE --"))

(defn- results-ledger-zone
  [text before-current-user?]
  (cond
    (str/includes? text " folded>")
    :compaction-ledger

    (or (str/includes? text "graph_diff")
      (str/includes? text "resolved_evidence")
      (str/includes? text "transaction_mode"))
    :dag-ledger

    before-current-user?
    :frozen-ledger

    :else
    :current-turn-ledger))

(defn- inferred-zone
  [message-index zone-index role zone cache-class source content & [{:keys [scope]}]]
  (let [content (str content)]
    (cond-> {:message-index message-index
             :zone-index zone-index
             :role role
             :zone zone
             :zone-id (str "inferred/m" message-index "/z" zone-index "/" (name zone))
             :cache-class cache-class
             :source source
             :content content
             :char-count (count content)
             :byte-count (alength (.getBytes content "UTF-8"))
             :estimated-tokens (tokens/count-tokens content)
             :inferred? true}
      scope (assoc :scope scope))))

(defn- system-zone
  [text]
  (cond
    (str/includes? text ";; -- PROJECT-INSTRUCTIONS --") :project-instructions
    (str/includes? text ";; -- TURN-SYSTEM-CONTEXT --") :capability-system-context
    (str/includes? text ";; -- CLI-AUTONOMOUS --") :capability-system-context
    :else :stable-system))

(defn- marker-splits
  [text markers]
  (let [hits (->> markers
               (keep (fn [{:keys [needle] :as marker}]
                       (when-let [idx (str/index-of text needle)]
                         (assoc marker :idx idx))))
               (sort-by :idx)
               vec)]
    (mapv (fn [i]
            (let [{:keys [idx] :as hit} (nth hits i)
                  end (if (< (inc i) (count hits))
                        (:idx (nth hits (inc i)))
                        (count text))]
              (assoc hit :content (subs text idx end))))
      (range (count hits)))))

(defn- infer-user-zones
  [idx role text {:keys [current-user-message-index]}]
  (let [splits (marker-splits text
                 [{:needle ";; -- PREVIOUS-TURN-CONTEXT --"
                   :zone :previous-turn-context
                   :cache-class :turn-prefix
                   :source :prompt/previous-turn}
                  {:needle ";; -- CURRENT-USER-MESSAGE --"
                   :zone :current-user-request
                   :cache-class :turn-prefix
                   :source :prompt/current-user}])]
    (if (seq splits)
      (map-indexed (fn [zone-idx {:keys [zone cache-class source content]}]
                     (inferred-zone idx zone-idx role zone cache-class source content))
        splits)
      [(cond
         (str/starts-with? text "<results")
         (let [before-current-user? (and current-user-message-index
                                      (< idx current-user-message-index))
               zone (results-ledger-zone text before-current-user?)]
           (inferred-zone idx 0 role zone :append-only-prefix
             (case zone
               :compaction-ledger :ctx/compaction-ledger
               :dag-ledger :ctx/dag-ledger
               :frozen-ledger :ctx/frozen-ledger
               :current-turn-ledger :ctx/current-turn-ledger)
             text
             {:scope (results-scope text)}))

         (str/starts-with? text "<context")
         (inferred-zone idx 0 role :mutable-context :mutable-tail :ctx/render-mutable text)

         :else
         (inferred-zone idx 0 role :provider-extra :unknown :report/inferred text))])))

(defn- infer-zones
  [messages]
  (let [indexed (map-indexed vector (or messages []))
        current-user-message-index
        (some (fn [[idx {:keys [role content]}]]
                (when (and (= "user" (str role))
                        (current-user-message? (content-text content)))
                  idx))
          indexed)]
    (vec
      (mapcat
        (fn [[idx {:keys [role content]}]]
          (let [role (str role)
                text (content-text content)
                before-current-user? (and current-user-message-index
                                       (< idx current-user-message-index))]
            (cond
              (= role "system")
              [(inferred-zone idx 0 role (system-zone text) :stable-prefix :prompt/stable text)]

              (= role "assistant")
              [(inferred-zone idx 0 role
                 (if before-current-user? :frozen-ledger :current-turn-ledger)
                 :append-only-prefix :svar/preserved-thinking text)]

              (= role "user")
              (infer-user-zones idx role text
                {:current-user-message-index current-user-message-index})

              :else
              [(inferred-zone idx 0 role :provider-extra :unknown :report/inferred text)])))
        indexed))))

(defn- zones-for-iteration
  [iter]
  (if (seq (:provider-request-zones iter))
    {:mode :exact :zones (:provider-request-zones iter)}
    {:mode :inferred :zones (infer-zones (:llm-user-prompt iter))}))

(defn- token-row
  [x]
  (str "<tr>"
    "<td>" (fmt-int (:input-tokens x)) "</td>"
    "<td>" (fmt-int (:input-regular-tokens x)) "</td>"
    "<td>" (fmt-int (:input-cache-write-tokens x)) "</td>"
    "<td>" (fmt-int (:input-cache-read-tokens x)) "</td>"
    "<td>" (fmt-int (:output-tokens x)) "</td>"
    "<td>" (fmt-int (:output-reasoning-tokens x)) "</td>"
    "<td>" (fmt-usd (:cost-usd x)) "</td>"
    "</tr>"))

(defn- zone-summary-table
  [zones]
  (str "<table><thead><tr>"
    "<th>msg</th><th>zone</th><th>cache</th><th>role</th><th>scope</th>"
    "<th>chars</th><th>bytes</th><th>est tokens</th><th>sha256</th>"
    "</tr></thead><tbody>"
    (apply str
      (for [{:keys [message-index zone-index zone cache-class role scope char-count byte-count
                    estimated-tokens content-sha256]} zones]
        (str "<tr>"
          "<td>" message-index "." zone-index "</td>"
          "<td><code>" (escape-html (name zone)) "</code></td>"
          "<td><code>" (escape-html (name cache-class)) "</code></td>"
          "<td>" (escape-html role) "</td>"
          "<td><code>" (escape-html scope) "</code></td>"
          "<td>" (fmt-int char-count) "</td>"
          "<td>" (fmt-int byte-count) "</td>"
          "<td>" (if (some? estimated-tokens) (fmt-int estimated-tokens) "n/a") "</td>"
          "<td><code>" (escape-html (subs (str (or content-sha256 "")) 0 (min 12 (count (str content-sha256))))) "</code></td>"
          "</tr>")))
    "</tbody></table>"))

(defn- zone-blocks
  [zones]
  (apply str
    (for [{:keys [message-index zone-index zone cache-class role scope source content]} zones]
      (str "<details class=\"zone\" open>"
        "<summary>"
        "<strong>m" message-index ".z" zone-index "</strong> "
        "<code>" (escape-html (name zone)) "</code> "
        "<span>" (escape-html role) "</span> "
        "<span>" (escape-html (name cache-class)) "</span> "
        (when scope (str "<span>scope " (escape-html scope) "</span> "))
        (when source (str "<span>source " (escape-html (name source)) "</span>"))
        "</summary>"
        "<pre>" (escape-html content) "</pre>"
        "</details>"))))

(defn- message-blocks
  [messages]
  (apply str
    (for [[idx msg] (map-indexed vector (or messages []))]
      (str "<details class=\"message\">"
        "<summary><strong>message " idx "</strong> role <code>"
        (escape-html (:role msg))
        "</code></summary>"
        "<pre>" (escape-html (json-str msg)) "</pre>"
        "</details>"))))

(defn- render-iteration
  [iter]
  (let [{:keys [mode zones]} (zones-for-iteration iter)]
    (str "<section class=\"iteration\">"
      "<h3>Iteration " (:position iter) " <span>" (escape-html (name (:status iter))) "</span></h3>"
      "<p><strong>Provider/model:</strong> "
      (escape-html (str (or (:provider iter) (:llm-actual-provider iter) "?")))
      " / " (escape-html (or (:model iter) (:llm-actual-model iter) "?"))
      " · <strong>zone mode:</strong> <code>" (name mode) "</code>"
      (when (= mode :inferred)
        " <em>historical row: zone labels inferred, spend remains iteration-level only</em>")
      "</p>"
      "<table><thead><tr><th>input</th><th>regular</th><th>cache write</th><th>cache read</th><th>output</th><th>reasoning</th><th>cost</th></tr></thead><tbody>"
      (token-row iter)
      "</tbody></table>"
      "<h4>Zones</h4>"
      (zone-summary-table zones)
      (zone-blocks zones)
      "<h4>Exact persisted provider messages</h4>"
      (message-blocks (:llm-user-prompt iter))
      "</section>")))

(defn- render-turn
  [db _session-id turn]
  (let [iters (vis/db-list-session-turn-iterations db (:id turn))]
    (str "<section class=\"turn\">"
      "<h2>Turn " (:position turn) "</h2>"
      "<p><strong>Status:</strong> " (escape-html (name (:status turn)))
      " · <strong>Iterations:</strong> " (count iters)
      " · <strong>Cost:</strong> " (fmt-usd (:total-cost turn))
      "</p>"
      "<details open><summary>User request</summary><pre>"
      (escape-html (:user-request turn))
      "</pre></details>"
      "<table><thead><tr><th>input</th><th>regular</th><th>cache write</th><th>cache read</th><th>output</th><th>reasoning</th><th>cost</th></tr></thead><tbody>"
      (token-row (assoc turn :cost-usd (:total-cost turn)))
      "</tbody></table>"
      (apply str (map render-iteration iters))
      "</section>")))

(defn- totals
  [turns]
  (reduce
    (fn [acc t]
      (-> acc
        (update :input-tokens + (:input-tokens t 0))
        (update :input-regular-tokens + (:input-regular-tokens t 0))
        (update :input-cache-write-tokens + (:input-cache-write-tokens t 0))
        (update :input-cache-read-tokens + (:input-cache-read-tokens t 0))
        (update :output-tokens + (:output-tokens t 0))
        (update :output-reasoning-tokens + (:output-reasoning-tokens t 0))
        (update :cost-usd + (:total-cost t 0.0))))
    {:input-tokens 0 :input-regular-tokens 0 :input-cache-write-tokens 0
     :input-cache-read-tokens 0 :output-tokens 0 :output-reasoning-tokens 0
     :cost-usd 0.0}
    turns))

(defn render-session
  [session-id]
  (let [db (vis/db-info)
        resolved-id (or (vis/db-resolve-session-id db session-id)
                      (throw (ex-info "Session not found" {:session-id session-id})))
        session (vis/db-get-session db resolved-id)
        turns (vis/db-list-session-turns db resolved-id)
        total (totals turns)]
    (str "<!doctype html><html><head><meta charset=\"utf-8\">"
      "<title>Vis Provider Report " (escape-html resolved-id) "</title>"
      "<style>"
      "body{font-family:ui-sans-serif,system-ui,-apple-system,sans-serif;margin:24px;background:#f7f3eb;color:#171511}"
      "h1,h2,h3{font-family:Georgia,serif}section{border:1px solid #d7c7aa;background:#fffaf0;margin:18px 0;padding:16px;border-radius:12px}"
      "table{border-collapse:collapse;width:100%;margin:10px 0;background:white}th,td{border:1px solid #dfd2bd;padding:6px 8px;text-align:left;vertical-align:top}"
      "th{background:#efe2cc}pre{white-space:pre-wrap;word-break:break-word;background:#18140f;color:#f8ecd6;padding:12px;border-radius:8px;overflow:auto}"
      "code{background:#eadcc4;padding:1px 4px;border-radius:4px}.zone summary,.message summary{cursor:pointer;margin:8px 0}.zone summary span{margin-left:10px;color:#665b4a}"
      ".note{background:#fff3bf;border:1px solid #e2c95f;padding:10px;border-radius:8px}"
      "</style></head><body>"
      "<h1>Vis Provider Request Report</h1>"
      "<p><strong>Session:</strong> <code>" (escape-html resolved-id) "</code></p>"
      "<p><strong>Title:</strong> " (escape-html (:title session)) "</p>"
      "<p><strong>Provider/model:</strong> " (escape-html (str (:provider session))) " / "
      (escape-html (:model session)) "</p>"
      "<div class=\"note\">Real spend is provider-reported per iteration/turn. Zone token counts are exact only where provider rows explicitly contain provider token fields; otherwise zone tokens are Vis tokenizer estimates and costs are not apportioned.</div>"
      "<h2>Totals</h2><table><thead><tr><th>input</th><th>regular</th><th>cache write</th><th>cache read</th><th>output</th><th>reasoning</th><th>cost</th></tr></thead><tbody>"
      (token-row total)
      "</tbody></table>"
      (apply str (map #(render-turn db resolved-id %) turns))
      "</body></html>")))

(defn write-report!
  ([session-id] (write-report! session-id nil))
  ([session-id out]
   (let [resolved (parse-session-id session-id)
         db (vis/db-info)
         actual-id (vis/db-resolve-session-id db resolved)
         out-file (io/file (or out
                             (str "dev/provider-report-"
                               (subs (str actual-id) 0 8)
                               ".html")))
         html (render-session resolved)]
     (io/make-parents out-file)
     (spit out-file html)
     (.getCanonicalPath out-file))))

(defn -main
  [& [session-id out]]
  (when-not session-id
    (binding [*out* *err*]
      (println "usage: dev/provider-report SESSION_ID [OUT.html]"))
    (System/exit 2))
  (println (write-report! session-id out)))
