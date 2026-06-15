(ns com.blockether.vis.dev.provider-report
  "Render persisted provider requests for one session as an HTML audit report."
  (:require
   [charred.api :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.provider-zones :as provider-zones]))

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

(declare render-node)

(defn- attr-html
  [[k v]]
  (when (some? v)
    (str " " (name k) "=\"" (escape-html v) "\"")))

(defn- render-node
  [node]
  (cond
    (nil? node) ""

    (string? node) (escape-html node)

    (or (number? node) (keyword? node) (symbol? node) (uuid? node))
    (escape-html node)

    (and (vector? node) (= :raw (first node)))
    (apply str (rest node))

    (vector? node)
    (let [[tag maybe-attrs & children] node
          [attrs children] (if (map? maybe-attrs)
                             [maybe-attrs children]
                             [nil (cons maybe-attrs children)])]
      (str "<" (name tag) (apply str (keep attr-html attrs)) ">"
        (apply str (map render-node children))
        "</" (name tag) ">"))

    (sequential? node)
    (apply str (map render-node node))

    :else
    (escape-html node)))

(defn- html
  [& nodes]
  (apply str (map render-node nodes)))

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

(defn- zones-for-iteration
  [iter]
  (if (seq (:provider-request-zones iter))
    {:mode :exact :zones (:provider-request-zones iter)}
    {:mode :inferred
     :zones (provider-zones/provider-request-zones (:llm-user-prompt iter)
              {:inferred? true})}))

(defn- token-row
  [x]
  [:tr
   [:td (fmt-int (:input-tokens x))]
   [:td (fmt-int (:input-regular-tokens x))]
   [:td (fmt-int (:input-cache-write-tokens x))]
   [:td (fmt-int (:input-cache-read-tokens x))]
   [:td (fmt-int (:output-tokens x))]
   [:td (fmt-int (:output-reasoning-tokens x))]
   [:td (fmt-usd (:cost-usd x))]])

(def ^:private token-table-head
  [:thead
   [:tr
    [:th "input"]
    [:th "regular"]
    [:th "cache write"]
    [:th "cache read"]
    [:th "output"]
    [:th "reasoning"]
    [:th "cost"]]])

(defn- token-table
  [x]
  [:table token-table-head [:tbody (token-row x)]])

(defn- zone-summary-table
  [zones]
  [:table
   [:thead
    [:tr
     [:th "msg"]
     [:th "zone"]
     [:th "cache"]
     [:th "role"]
     [:th "scope"]
     [:th "chars"]
     [:th "bytes"]
     [:th "est tokens"]
     [:th "sha256"]]]
   [:tbody
    (for [{:keys [message-index zone-index zone cache-class role scope char-count byte-count
                  estimated-tokens content-sha256]} zones]
      [:tr
       [:td message-index "." zone-index]
       [:td [:code (name zone)]]
       [:td [:code (name cache-class)]]
       [:td role]
       [:td [:code scope]]
       [:td (fmt-int char-count)]
       [:td (fmt-int byte-count)]
       [:td (if (some? estimated-tokens) (fmt-int estimated-tokens) "n/a")]
       [:td [:code (subs (str (or content-sha256 ""))
                     0
                     (min 12 (count (str content-sha256))))]]])]])

(defn- zone-blocks
  [zones]
  (for [{:keys [message-index zone-index zone cache-class role scope source content]} zones]
    [:details {:class "zone" :open "open"}
     [:summary
      [:strong "m" message-index ".z" zone-index]
      " "
      [:code (name zone)]
      " "
      [:span role]
      " "
      [:span (name cache-class)]
      " "
      (when scope [:span "scope " scope])
      (when source [:span "source " (name source)])]
     [:pre content]]))

(defn- message-blocks
  [messages]
  (for [[idx msg] (map-indexed vector (or messages []))]
    [:details {:class "message"}
     [:summary
      [:strong "message " idx]
      " role "
      [:code (:role msg)]]
     [:pre (json-str msg)]]))

(defn- render-iteration
  [iter]
  (let [{:keys [mode zones]} (zones-for-iteration iter)]
    [:section {:class "iteration"}
     [:h3 "Iteration " (:position iter) " " [:span (name (:status iter))]]
     [:p
      [:strong "Provider/model:"]
      " "
      (str (or (:provider iter) (:llm-actual-provider iter) "?"))
      " / "
      (or (:model iter) (:llm-actual-model iter) "?")
      " · "
      [:strong "zone mode:"]
      " "
      [:code (name mode)]
      (when (= mode :inferred)
        [:em " historical row: zone labels inferred, spend remains iteration-level only"])]
     (token-table iter)
     [:h4 "Zones"]
     (zone-summary-table zones)
     (zone-blocks zones)
     [:h4 "Exact persisted provider messages"]
     (message-blocks (:llm-user-prompt iter))]))

(defn- render-turn
  [db _session-id turn]
  (let [iters (vis/db-list-session-turn-iterations db (:id turn))]
    [:section {:class "turn"}
     [:h2 "Turn " (:position turn)]
     [:p
      [:strong "Status:"]
      " " (name (:status turn))
      " · "
      [:strong "Iterations:"]
      " " (count iters)
      " · "
      [:strong "Cost:"]
      " " (fmt-usd (:total-cost turn))]
     [:details {:open "open"}
      [:summary "User request"]
      [:pre (:user-request turn)]]
     (token-table (assoc turn :cost-usd (:total-cost turn)))
     (map render-iteration iters)]))

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
      (html
        [:title "Vis Provider Report " resolved-id]
        [:style
         [:raw
          "body{font-family:ui-sans-serif,system-ui,-apple-system,sans-serif;margin:24px;background:#f7f3eb;color:#171511}"
          "h1,h2,h3{font-family:Georgia,serif}section{border:1px solid #d7c7aa;background:#fffaf0;margin:18px 0;padding:16px;border-radius:12px}"
          "table{border-collapse:collapse;width:100%;margin:10px 0;background:white}th,td{border:1px solid #dfd2bd;padding:6px 8px;text-align:left;vertical-align:top}"
          "th{background:#efe2cc}pre{white-space:pre-wrap;word-break:break-word;background:#18140f;color:#f8ecd6;padding:12px;border-radius:8px;overflow:auto}"
          "code{background:#eadcc4;padding:1px 4px;border-radius:4px}.zone summary,.message summary{cursor:pointer;margin:8px 0}.zone summary span{margin-left:10px;color:#665b4a}"
          ".note{background:#fff3bf;border:1px solid #e2c95f;padding:10px;border-radius:8px}"]])
      "</head><body>"
      (html
        [:h1 "Vis Provider Request Report"]
        [:p [:strong "Session:"] " " [:code resolved-id]]
        [:p [:strong "Title:"] " " (:title session)]
        [:p [:strong "Provider/model:"] " " (str (:provider session)) " / " (:model session)]
        [:div {:class "note"}
         "Real spend is provider-reported per iteration/turn. Zone token counts are exact only where provider rows explicitly contain provider token fields; otherwise zone tokens are Vis tokenizer estimates and costs are not apportioned."]
        [:h2 "Totals"]
        (token-table total)
        (map #(render-turn db resolved-id %) turns))
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
