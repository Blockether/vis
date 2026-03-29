(ns com.blockether.vis.web.presenter
  "HTML rendering — page layout, message bubbles, iterations, CSS, JS."
  (:require [com.blockether.vis.trace :as trace]
            [hiccup2.core :as h]
            [clojure.string :as str])
  (:import [java.util Locale]))

;;; ── Helpers ────────────────────────────────────────────────────────────

(defn- fmt-dur [ms]
  (cond (nil? ms) "" (< ms 1000) (str ms "ms")
        :else (String/format Locale/US "%.1fs" (into-array Object [(double (/ ms 1000.0))]))))

;;; ── Message rendering ──────────────────────────────────────────────────

(defn- exec-badge [code]
  (when code
    (let [t (str/trim code)
          b (if (str/starts-with? t "(") (subs t 1) t)]
      (first (str/split b #"[\s\)\(\"']" 2)))))

(defn- render-exec [{:keys [code result error]}]
  (let [clean     (trace/clean-result result)
        is-final? (and (map? result) (:rlm/final result))
        badge     (exec-badge code)]
    (cond
      ;; FINAL call — hide the call itself, the answer is shown by render-msg
      is-final? nil

      ;; Error — compact
      error
      [:div.exec.exec-errored
       (when badge [:span.exec-badge badge])
       [:div.exec-error (str error)]]

      ;; Normal execution (def, tool calls, etc.)
      :else
      [:div.exec
       (when badge [:span.exec-badge badge])
       [:div.exec-code code]
       [:div.exec-result
        (cond
          (nil? clean) "nil"
          (string? clean) clean
          :else [:pre.exec-data (pr-str clean)])]])))

(defn- render-iteration [{:keys [iteration thinking executions final?]}]
  [:div.iteration {:class (when final? "iteration-final")}
   [:div.iter-header (str "Iteration " (inc iteration)) (when final? [:span.final " FINAL"])]
   (when (and thinking (not (str/blank? thinking)))
     [:div.thinking thinking])
   (when (seq executions) (keep render-exec executions))])

(defn render-msg [idx {:keys [role text result]}]
  (case role
    :user [:div.msg.user-msg [:div.bubble.user-bubble text [:button.copy-btn {:onclick "copyMsg(this)"} [:i {:data-lucide "copy" :style "width:12px;height:12px"}]]]]
    :assistant
    [:div.msg.ai-msg
     [:div.bubble.ai-bubble
      (when-let [trace (:trace result)]
        (map render-iteration trace))
      (when-let [a (:answer result)]
        [:div.answer.md-content (let [v (if (map? a) (:result a) a)] (if (string? v) v (pr-str v)))])
      (let [{:keys [iterations duration-ms tokens cost]} result]
        [:div.meta
         (str/join " · "
                   (cond-> []
                     iterations (conj (str iterations " iter"))
                     duration-ms (conj (fmt-dur duration-ms))
                     (:input tokens) (conj (str (:input tokens) "↓ " (:output tokens) "↑"))
                     (:total-cost cost) (conj (String/format Locale/US "$%.4f"
                                                             (into-array Object [(double (:total-cost cost))])))))
         " "
         [:button.copy-btn {:data-msg (str idx) :onclick (if (:trace result) "copyTrace(this)" "copyMsg(this)")} [:i {:data-lucide "copy" :style "width:12px;height:12px"}]]])]]))


;;; ── CSS ────────────────────────────────────────────────────────────────

;;; ── CSS & JS loaded from resources/public/ ─────────────────────────────
;; Edit: resources/public/css/app.css and resources/public/js/app.js

;;; ── Page ────────────────────────────────────────────────────────────────

(def ^:const page-size 8)

(defn page [current-id sessions-list messages & [{:keys [offset]}]]
  (str
   "<!DOCTYPE html>"
   (h/html
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"}]
      [:title "vis"]
      [:link {:rel "stylesheet" :href "/css/app.css"}]
      [:link {:rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"}]
      [:script {:src "https://cdn.jsdelivr.net/npm/marked/marked.min.js"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]
      [:script {:src "https://unpkg.com/lucide@latest"}]]
     [:body
      (let [current-name (or (some #(when (= (:id %) current-id) (:name %)) sessions-list) "New Chat")]
        [:div.topbar
         [:div.topbar-title {:onclick "openSheet()"} current-name]])
      [:div#sheet.sheet
       [:div.sheet-bg {:onclick "closeSheet()"}]
       [:div.sheet-content
        [:div.sheet-handle]
        [:div.sheet-header
         [:span "Sessions"]
         [:a {:href "/new" :style "display:flex;align-items:center;gap:4px;text-decoration:none;color:var(--accent);font-size:14px;font-weight:600"} [:i {:data-lucide "plus" :style "width:16px;height:16px"}] "New"]]
        [:div.sheet-list
         (for [{:keys [id name]} sessions-list]
           [:div.sheet-item {:class (when (= id current-id) "active")}
            [:a.sheet-item-name {:href (str "/s/" id)} name]
            [:a.sheet-item-del {:href (str "/s/" id "/delete") :onclick "return confirm('Delete?')"}
             [:i {:data-lucide "trash-2" :style "width:16px;height:16px"}]]])]]]
      [:div#copy-modal.copy-modal {:onclick "closeCopyModal()"}
       [:div.copy-modal-content {:onclick "event.stopPropagation()"}
        [:div.copy-modal-header
         [:span "Trace"]
         [:div {:style "display:flex;gap:8px;align-items:center"}
          [:button.copy-modal-copy-btn {:onclick "copyModalText()" :style "background:var(--accent);color:var(--bg);border:none;padding:4px 12px;border-radius:8px;font-size:13px;font-weight:600;cursor:pointer"} "Copy"]
          [:button.copy-modal-close {:onclick "closeCopyModal()"} [:i {:data-lucide "x" :style "width:18px;height:18px"}]]]]
        [:div#copy-modal-text.copy-modal-text]]]
      [:div.main
       [:div#chat.chat {:data-total (str (count messages))
                        :data-showing (str (min (count messages) page-size))
                        :data-session current-id}
        (let [total     (count messages)
              show-from (max 0 (- total (or offset page-size)))
              visible   (subvec (vec messages) show-from)]
          [:div.chat-inner
           (if (empty? messages)
             [:div.empty ""]
             (list
              (when (pos? show-from)
                [:div#load-more.load-more {:data-from (str show-from)} "Loading..."])
              (map-indexed (fn [i msg] (render-msg (+ show-from i) msg)) visible)))])]
       [:div.input-bar
        [:form#form {:method "POST" :action (str "/s/" current-id)}
         [:input#input {:type "text" :name "q" :placeholder "Message..." :autofocus true :autocomplete "off"}]
         [:button#btn {:type "submit" :disabled true} [:i {:data-lucide "arrow-up" :style "width:20px;height:20px"}]]]]]
      [:script {:src "/js/app.js"}]]])))
