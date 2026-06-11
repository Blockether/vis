(ns com.blockether.vis.ext.channel-web.core
  "Web companion channel - the gateway's `/ui` chat instrument.

   Chat-first anatomy (the ChatGPT/Claude shape): the conversation is a
   centered document column - user messages as compact right-aligned
   pills, vis answers as flat typeset prose behind a small gold avatar,
   the execution machinery folded into a 'Work' disclosure, a floating
   pill composer pinned to the bottom (Enter sends, Shift+Enter breaks,
   auto-grows), a thin blurred sticky header, and CONTEXT - the same
   ctx mirror the model reads - as a quiet right rail.

   Pure Clojure SSR: hiccup renders HTML, HTMX does declarative swaps,
   the live feed is the htmx SSE extension consuming
   `/ui/session/:sid/stream` - a gateway SSE stream of named HTML
   fragments (activity/thinking/context) rendered server-side. Every
   script is VENDORED on the classpath and served from memory
   (htmx 2.0.10 + its SSE extension + ui.js + dev-reload.js) - a page
   never loads anything from outside vis.

   This namespace is the THIRD canonical-IR walker: the TUI walks IR
   into ANSI cells, Telegram walks it into its HTML subset, and
   `ir->hiccup` walks the same IR into DOM (GATEWAY.md §4.1 ALWAYS IR).

   AUTO-MOUNT: the extension declares a `:gateway.slot/http-routes`
   contribution; the gateway pulls it whenever it builds its handler
   (whiteboard pattern - no ordering requirement, see GATEWAY.md §10.1).

   Auth: none on the loopback default (`gateway-auth-required?` false);
   when the gate is on, POST /ui/auth exchanges the bearer token for an
   HttpOnly `vis_token` cookie that EventSource carries on SSE connect.

   Theme: vis-light - white surfaces, gold fills, amber accent text -
   CSS variables lifted 1:1 from `internal/theme.clj` light-palette."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [hiccup2.core :as h]
   [ring.core.protocols :as ring-protocols])
  (:import
   [java.io OutputStream]
   [java.nio.charset StandardCharsets]))

(def ^:private HEARTBEAT_MS 15000)

(def ^:private ui-load-stamp
  "Re-evaluated on every namespace (re)load — deliberately `def`, NOT
   `defonce`. The /ui/dev-reload stream watches this var: a REPL
   `:reload` of this namespace moves the stamp, connected browsers get
   a `reload` event and refresh themselves. Always on - the listener is
   one parked virtual thread per open page."
  (System/currentTimeMillis))

(def ^:private DEV_RELOAD_POLL_MS 2000)

;; Every script the pages load is VENDORED on the classpath under
;; resources/vis-channel-web/public/ and served by this channel from
;; memory — no CDN, no request ever leaves the host.
(def ^:private JS_ASSETS
  {"htmx.min.js"   "vis-channel-web/public/htmx.min.js"
   "htmx-sse.js"   "vis-channel-web/public/htmx-sse.js"
   "ui.js"         "vis-channel-web/public/ui.js"
   "dev-reload.js" "vis-channel-web/public/dev-reload.js"})

(def ^:private js-asset-cache
  "Asset name -> file content, read from the classpath ONCE and served
   from memory — no per-request resource IO on the hot path."
  (delay
    (into {}
      (keep (fn [[nm path]]
              (when-let [resource (io/resource path)]
                [nm (slurp resource)])))
      JS_ASSETS)))

(defn- js-asset-handler [request]
  (if-let [content (get @js-asset-cache (get-in request [:path-params :asset]))]
    {:status 200
     :headers {"Content-Type" "application/javascript; charset=utf-8"
               "Cache-Control" "no-cache"}
     :body content}
    {:status 404 :headers {"Content-Type" "text/plain"} :body "unknown asset"}))

;; =============================================================================
;; Canonical IR -> hiccup (the web IR walker)
;; =============================================================================

(def ^:private ir-tag->html
  "IR tags with a direct HTML counterpart. Anything else renders as a
   div carrying `ir-<tag>` so unknown/new IR never breaks the page."
  {:ir :div :p :p :span :span :strong :strong :b :strong :em :em :i :em
   :h1 :h1 :h2 :h2 :h3 :h3 :h4 :h4 :h5 :h5 :h6 :h6
   :ul :ul :ol :ol :li :li :blockquote :blockquote :hr :hr :br :br
   :table :table :thead :thead :tbody :tbody :tr :tr :td :td :th :th
   :del :del :s :del :a :a})

(defn ir->hiccup
  "Walk a canonical IR node (`[:tag {attrs}? & children]`, strings,
   seqs) into hiccup. Total: unknown tags degrade to classed divs,
   non-IR leaves degrade to strings. Never throws on model output."
  [node]
  (cond
    (string? node) node
    (nil? node) nil
    (number? node) (str node)
    (seq? node) (keep ir->hiccup node)

    (vector? node)
    (let [[tag second-el & rest-els] node
          attrs?   (map? second-el)
          attrs    (if attrs? second-el {})
          children (if attrs? rest-els (cons second-el rest-els))]
      (cond
        (= tag :code)
        [:code.ir-code (keep ir->hiccup children)]

        (or (= tag :pre) (= tag :code-block))
        [:pre.ir-pre
         [:code {:class (str "lang-" (name (or (:lang attrs) "txt")))}
          (keep ir->hiccup children)]]

        (= tag :a)
        (into [:a {:href (str (:href attrs)) :rel "noreferrer"}]
          (keep ir->hiccup children))

        :else
        (if-let [html-tag (ir-tag->html tag)]
          (into [html-tag] (keep ir->hiccup children))
          (into [:div {:class (str "ir-" (name tag))}]
            (keep ir->hiccup children)))))

    :else (str node)))

(defn- md->hiccup [markdown]
  (when-not (str/blank? (str markdown))
    (try
      (ir->hiccup (vis/markdown->ir (str markdown)))
      (catch Throwable _ [:pre.ir-pre (str markdown)]))))

(defn- html ^String [hiccup-form]
  (str (h/html hiccup-form)))

(defn- page ^String [title & body]
  (str "<!DOCTYPE html>"
    (h/html
      [:html
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:title (str title " · vis")]
        [:link {:rel "stylesheet" :href "/ui/app.css"}]
        ;; All vendored, all local — nothing loads from outside vis.
        [:script {:src "/ui/js/htmx.min.js" :defer true}]
        [:script {:src "/ui/js/htmx-sse.js" :defer true}]
        [:script {:src "/ui/js/ui.js" :defer true}]
        [:script {:src "/ui/js/dev-reload.js" :defer true}]]
       (into [:body] body)])))

;; =============================================================================
;; Defensive readers (context snapshot values may carry string OR kw
;; keys - the GraalPy boundary keeps snake_case strings verbatim)
;; =============================================================================

(defn- pick [m k]
  (when (map? m)
    (or (get m k) (get m (name k)) (get m (keyword (name k))))))

(defn- fact-entries [facts]
  (cond
    (map? facts) (sort-by (comp str key) facts)
    (sequential? facts) (map-indexed (fn [i f] [(or (pick f :key) (str "fact-" i)) f]) facts)
    :else nil))

;; =============================================================================
;; Fragments
;; =============================================================================

(defn- status-chip [status]
  [:span {:class (str "chip chip-" (or status "idle"))} (or status "idle")])

(defn- utilization-bar [utilization]
  (let [pct (or (pick utilization :pct-of-limit) (pick utilization :pct_of_limit))]
    (when (number? pct)
      [:div.util
       [:div.util-track [:div.util-fill {:style (str "width:" (min 100 (long pct)) "%")}]]
       [:span.util-label (str (long pct) "%")]])))

(defn- fact-card [[fact-key fact]]
  [:details.fact
   [:summary [:span.fact-key (str fact-key)]]
   [:div.fact-body
    (when-let [content (pick fact :content)]
      [:p.fact-content (str content)])
    (for [file (or (pick fact :files) [])
          :let [path (pick file :path)]
          :when path]
      [:div.fact-file
       [:span.fact-path (str path)]
       (for [region (or (pick file :regions) [])]
         [:div.fact-region
          (when-let [from-hash (pick region :from_hash)]
            [:span.fact-hash (str "@" from-hash)])
          (when-let [src (pick region :src)]
            [:pre.ir-pre [:code (str src)]])])])
    (when-not (or (pick fact :content) (seq (or (pick fact :files) [])))
      [:pre.ir-pre [:code (pr-str fact)]])]])

(def ^:private task-glyph
  {"done" "✓" "completed" "✓" "in_progress" "◐" "running" "◐"
   "candidate" "◇" "pending" "○"})

(defn- task-row [task]
  (let [status (str (or (pick task :status) "pending"))]
    [:li {:class (str "task task-" status)}
     [:span.task-glyph (get task-glyph status "○")]
     [:span.task-title (str (or (pick task :title) (pick task :id) (pr-str task)))]]))

(defn- context-panel
  "The right rail: CONTEXT - the same ctx mirror the model reads."
  [snapshot]
  [:div#context.context
   [:div.rail-head [:h2 "Context"] (utilization-bar (pick snapshot :session/utilization))]
   (let [tasks (pick snapshot :session/tasks)]
     [:section.rail-section
      [:h3 (str "Plan" (when (seq tasks) (str " · " (count tasks))))]
      (if (seq tasks)
        [:ul.tasks (map task-row tasks)]
        [:p.empty "no plan yet"])])
   (let [facts (fact-entries (pick snapshot :session/facts))]
     [:section.rail-section
      [:h3 (str "Facts" (when (seq facts) (str " · " (count facts))))]
      (if (seq facts)
        [:div.facts (map fact-card facts)]
        [:p.empty "no facts yet"])])
   (when-let [scope (pick snapshot :session/scope)]
     [:section.rail-section
      [:h3 "Scope"]
      [:pre.ir-pre [:code (str (or (pick scope :cursor) (pr-str scope)))]]])])

(defn- vis-avatar []
  [:div.avatar {:aria-hidden "true"} "v"])

(defn- turn-block [turn]
  (let [status (pick turn :status)]
    (list
      [:div.msg.msg-user [:div.msg-user-body [:p (str (pick turn :request))]]]
      (cond
        (pick turn :answer_md)
        [:div.msg.msg-vis
         (vis-avatar)
         [:div.msg-vis-body
          [:div.prose (md->hiccup (pick turn :answer_md))]
          [:div.msg-meta
           (status-chip status)
           (when-let [cost (pick (pick turn :cost) :total-cost)]
             [:span (format "$%.4f" (double cost))])
           (when-let [n (pick turn :iteration_count)]
             [:span (str n " iteration" (when (not= 1 n) "s"))])]]]

        (= "running" status)
        [:div.msg.msg-vis
         (vis-avatar)
         [:div.msg-vis-body
          [:div.dots [:span] [:span] [:span]]]]

        :else
        [:div.msg.msg-vis
         (vis-avatar)
         [:div.msg-vis-body
          [:p.empty (str "(" (or status "no answer") ")")]
          [:div.msg-meta (status-chip status)]]]))))

(defn- activity-item [kind & children]
  (html (into [:div {:class (str "act act-" kind)}] children)))

(defn- user-bubble-html [text]
  (html [:div.msg.msg-user [:div.msg-user-body [:p (str text)]]]))

(defn- vis-message-html
  "A full vis chat bubble from a terminal turn event — flies into the
   thread (`#live`), NOT the Work log."
  [event]
  (html
    [:div.msg.msg-vis
     (vis-avatar)
     [:div.msg-vis-body
      [:div.prose (md->hiccup (or (:answer_md event) (:error event) ""))]
      [:div.msg-meta
       (status-chip (:status event))
       (when-let [cost (pick (:cost event) :total-cost)]
         [:span (format "$%.4f" (double cost))])
       (when-let [n (:iteration_count event)]
         [:span (str n " iteration" (when (not= 1 n) "s"))])]]]))

;; =============================================================================
;; SSE: engine events -> named HTML fragments for htmx sse-swap
;; =============================================================================

(defn- code-snip [code]
  (let [s (str code)]
    (if (> (count s) 400) (str (subs s 0 400) " …") s)))

(defn- event->frames
  "One gateway event -> seq of `{:event name :html fragment}` for the
   htmx SSE extension (`sse-swap=\"<name>\"`)."
  [sid {:keys [type] :as event}]
  (case type
    ;; The thread gets BUBBLES (user message via the form response, the
    ;; answer via the `message` event); Work gets ONLY machinery.
    "turn.started"
    [{:event "thinking"
      :html (html (list [:div.dots [:span] [:span] [:span]]))}]

    "reasoning.delta"
    [{:event "thinking"
      :html (html [:span.act-dim (code-snip (:text event))])}]

    "block.started"
    [{:event "activity"
      :html (activity-item "code" [:pre.ir-pre [:code (code-snip (:code event))]])}]

    "block.output"
    [{:event "activity"
      :html (activity-item (if (:error event) "error" "result")
              (when-let [error (:error event)] [:pre.ir-pre.act-error [:code (code-snip error)]])
              (when-let [result (:result event)] [:pre.ir-pre [:code (code-snip result)]]))}]

    "iteration.completed"
    [{:event "activity"
      :html (activity-item "iter" [:span.act-dim "iteration done"])}]

    ("turn.completed" "turn.failed")
    (let [snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))]
      (cond-> [{:event "thinking" :html ""}
               {:event "message" :html (vis-message-html event)}]
        snapshot (conj {:event "context" :html (html (context-panel snapshot))})))

    nil))

(defn- write-frame! [^OutputStream out {:keys [event html]}]
  (let [frame (str "event: " event "\n"
                (->> (str/split-lines (str html))
                  (map #(str "data: " %))
                  (str/join "\n"))
                "\n\n")]
    (.write out (.getBytes frame StandardCharsets/UTF_8))))

(defn stream-handler
  "Live-only SSE stream of HTML fragments for the session page. Same
   replay-then-live monitor discipline as the JSON stream, but the
   cursor pins to `gateway-current-seq` so the page (which rendered
   current state) receives only what happens next."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (if-not (and sid (vis/gateway-soul sid))
      {:status 404 :headers {"Content-Type" "text/html"} :body "unknown session"}
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache"}
       :body
       (reify ring-protocols/StreamableResponseBody
         (write-body-to-stream [_ _ output-stream]
           (let [^OutputStream out output-stream
                 sub-id (str (java.util.UUID/randomUUID))
                 sink   (fn [event]
                          (locking out
                            (doseq [frame (event->frames sid event)]
                              (write-frame! out frame))
                            (.flush out)))]
             (try
               (locking out
                 (doseq [event (vis/gateway-subscribe! sid sub-id sink
                                 (vis/gateway-current-seq sid))]
                   (sink event)))
               (loop []
                 (Thread/sleep (long HEARTBEAT_MS))
                 (locking out
                   (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out))
                 (recur))
               (catch Throwable _ nil)
               (finally
                 (vis/gateway-unsubscribe! sid sub-id)
                 (try (.close out) (catch Throwable _ nil)))))))})))

(defn- dev-reload-handler
  "SSE stream backing the always-on auto-reload script. Emits the
   namespace load stamp on connect, then a `reload` event the moment
   the stamp moves (a REPL :reload of this ns) — the browser refreshes
   itself. The poll loop parks a virtual thread."
  [_request]
  (let [stamp-at-connect @#'ui-load-stamp]
    {:status 200
     :headers {"Content-Type" "text/event-stream"
               "Cache-Control" "no-cache"}
     :body
     (reify ring-protocols/StreamableResponseBody
       (write-body-to-stream [_ _ output-stream]
         (let [^OutputStream out output-stream]
           (try
             (.write out (.getBytes (str "event: stamp\ndata: " stamp-at-connect "\n\n")
                           StandardCharsets/UTF_8))
             (.flush out)
             (loop []
               (Thread/sleep (long DEV_RELOAD_POLL_MS))
               (if (not= stamp-at-connect @#'ui-load-stamp)
                 (do (.write out (.getBytes "event: reload\ndata: now\n\n"
                                   StandardCharsets/UTF_8))
                   (.flush out))
                 (do (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out)
                   (recur))))
             (catch Throwable _ nil)
             (finally
               (try (.close out) (catch Throwable _ nil)))))))}))

;; =============================================================================
;; Pages
;; =============================================================================

(defn- token-form-page [& [error]]
  (page "connect"
    [:main.auth
     [:div.auth-card
      [:h1 "vis"]
      [:p.tagline "see it think"]
      (when error [:p.auth-error error])
      [:form {:method "post" :action "/ui/auth"}
       [:input {:type "password" :name "token" :placeholder "gateway bearer token"
                :autofocus true :autocomplete "off"}]
       [:button.send-wide {:type "submit"} "Connect"]]
      [:p.auth-hint "the token lives at ~/.vis/gateway.token on the host"]]]))

(defn- sessions-page []
  (page "sessions"
    [:div.app
     [:header.bar
      [:div.bar-title [:span.wordmark "vis"]]]
     [:main.thread
      [:div.column.index
       [:h1.hello "What are we building?"]
       [:form.composer.newsession {:method "post" :action "/ui/sessions"}
        [:input {:type "text" :name "title" :placeholder "Name a new session…"
                 :autocomplete "off" :autofocus true}]
        [:button.send {:type "submit" :aria-label "Open"} "↑"]]
       (let [sessions (vis/gateway-list-sessions)]
         (when (seq sessions)
           [:div.sessions-block
            [:h3.block-label "Sessions"]
            [:ul.sessions
             (for [{:keys [id title status]} sessions]
               [:li
                [:a.session-row {:href (str "/ui/session/" id)}
                 [:span.session-title (or title "Untitled")]
                 [:span.session-spacer]
                 (status-chip status)
                 [:span.session-id (subs (str id) 0 8)]]])]]))]]]))

(defn- mic-icon []
  [:svg {:viewBox "0 0 24 24" :width "16" :height "16" :fill "currentColor"
         :aria-hidden "true"}
   [:path {:d "M12 14a3 3 0 0 0 3-3V5a3 3 0 0 0-6 0v6a3 3 0 0 0 3 3zm5-3a5 5 0 0 1-10 0H5a7 7 0 0 0 6 6.92V21h2v-3.08A7 7 0 0 0 19 11h-2z"}]])

(defn- sessions-sidebar
  "Left rail: the session drawer. The active session is highlighted; a
   running one carries a gold pulse dot."
  [active-sid]
  [:aside.sidebar
   [:form.newchat {:method "post" :action "/ui/sessions"}
    [:button.newchat-btn {:type "submit"} "+ New session"]]
   [:ul.side-sessions
    (for [{:keys [id title status]} (vis/gateway-list-sessions)]
      [:li
       [:a {:class (str "side-row" (when (= (str id) (str active-sid)) " active"))
            :href (str "/ui/session/" id)}
        [:span.side-title (or title "Untitled")]
        (when (= status "running") [:span.side-dot])]])]])

(defn- session-page [sid]
  (let [soul     (vis/gateway-soul sid)
        turns    (reverse (vis/gateway-list-turns sid))
        snapshot (try (vis/gateway-context-snapshot sid) (catch Throwable _ nil))]
    (page (or (:title soul) "session")
      [:div.app {:hx-ext "sse"
                 :sse-connect (str "/ui/session/" sid "/stream")}
       [:header.bar
        [:a.back {:href "/ui"} "←"]
        [:div.bar-title
         [:span.bar-name (or (:title soul) "Untitled")]
         (status-chip (:status soul))]
        [:span.session-id (subs (str sid) 0 8)]]
       [:div.layout
        (sessions-sidebar sid)
        ;; ONE center flex column holds the thread AND the composer dock,
        ;; so both center in the SAME box between the rails — the input
        ;; can never drift out of line with the chat column again.
        [:div.center
         [:main.thread
          [:div.column
           (if (seq turns)
             (map turn-block turns)
             [:div.hello-wrap
              [:h1.hello "What are we building?"]
              [:p.hello-sub "vis works in this workspace — ask for anything."]])
           ;; Live bubbles land here (user message from the form response,
           ;; the answer from the `message` SSE event). Work below holds
           ;; ONLY machinery: code, results, iteration ticks.
           [:div#live.live {:sse-swap "message" :hx-swap "beforeend"}]
           [:div#thinking.thinking {:sse-swap "thinking" :hx-swap "innerHTML"}]
           [:details.work
            [:summary "Work"]
            [:div#activity.activity {:sse-swap "activity" :hx-swap "beforeend"}]]
           [:div.thread-tail]]]
         [:div.dock
          [:form.composer {:hx-post (str "/ui/session/" sid "/turns")
                           :hx-target "#live" :hx-swap "beforeend"
                           "hx-on::after-request" "if(event.detail.successful) this.reset()"}
           [:textarea {:name "request" :rows 1
                       :placeholder "Ask vis…"}]
           [:button.mic {:type "button" :aria-label "Dictate"
                         :data-voice-url (str "/ui/session/" sid "/voice")}
            (mic-icon)]
           [:button.send {:type "submit" :aria-label "Send"} "↑"]]]]
        [:aside.rail {:sse-swap "context" :hx-swap "innerHTML"}
         (if snapshot
           (context-panel snapshot)
           [:div#context.context
            [:div.rail-head [:h2 "Context"]]
            [:p.empty "wakes on the first turn"]])]]])))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn- cookie-token [request]
  (get-in request [:cookies "vis_token" :value]))

(defn- ui-authed?
  "True when the gateway runs authless (the loopback default) or the
   request carries the gateway token as the browser cookie. Registered
   as the contribution's :request-authed-fn."
  [request ^String token]
  (or (not (vis/gateway-auth-required?))
    (= token (cookie-token request))))

(defn- index-handler
  "GET /ui - session list when authed (or authless), token form
   otherwise. This route is open; it never leaks data unauthenticated."
  [request token]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (if (ui-authed? request token)
           (sessions-page)
           (token-form-page))})

(defn- auth-handler
  "POST /ui/auth - exchange the bearer token for the HttpOnly cookie."
  [request token]
  (if (= token (some-> (get-in request [:form-params "token"]) str/trim))
    {:status 303
     :headers {"Location" "/ui"}
     :cookies {"vis_token" {:value token :http-only true
                            :same-site :lax :path "/"}}
     :body ""}
    {:status 401
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (token-form-page "that token does not match — check ~/.vis/gateway.token")}))

(defn- create-session-handler
  "POST /ui/sessions - create and bounce to the session page."
  [request]
  (let [title (let [t (str (get-in request [:form-params "title"]))]
                (when-not (str/blank? t) t))
        {:keys [id]} (vis/gateway-create-session! {:title title})]
    {:status 303 :headers {"Location" (str "/ui/session/" id)} :body ""}))

(defn- session-handler [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)]
    (if (and sid (vis/gateway-soul sid))
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (session-page sid)}
      {:status 303 :headers {"Location" "/ui"} :body ""})))

(defn- submit-turn-handler
  "POST /ui/session/:sid/turns (htmx form) - submit and return a small
   activity fragment; the live stream carries everything that follows."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        text (str (get-in request [:form-params "request"]))
        result (when sid (vis/gateway-submit-turn! sid {:request text}))]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (cond
             (:turn result)
             (user-bubble-html text)

             (= :turn-in-progress (:error result))
             (html [:div.msg.msg-vis (vis-avatar)
                    [:div.msg-vis-body
                     [:p.empty "a turn is already running — wait for it to finish"]]])

             :else
             (html [:div.msg.msg-vis (vis-avatar)
                    [:div.msg-vis-body
                     [:p.empty (str "rejected: " (or (:message result) "invalid request"))]]]))}))

(defn- json-text [m]
  (str "{" (str/join "," (for [[k v] m] (str (pr-str (name k)) ":" (pr-str (str v))))) "}"))

(defn- wav-file?
  "RIFF/WAVE magic + minimum header length. MANDATORY before handing a
   file to the ASR: sherpa-onnx's native WaveReader ABORTS THE WHOLE JVM
   on malformed input (observed live: Abort trap 6 on a garbage body) —
   an exception we can catch is not on offer, so we refuse early."
  [^java.io.File f]
  (and (>= (.length f) 44)
    (with-open [in (io/input-stream f)]
      (let [head (byte-array 12)]
        (and (= 12 (.read in head))
          (= "RIFF" (String. head 0 4 "US-ASCII"))
          (= "WAVE" (String. head 8 4 "US-ASCII")))))))

(defn- voice-handler
  "POST /ui/session/:sid/voice — body is a WAV blob recorded+encoded in
   the browser (ui.js). Transcribes through the LOCAL Parakeet model
   (vis-foundation-voice / sherpa-onnx; soft-resolved so a build without
   the voice extension answers 501 instead of failing to load). First
   use downloads the model — same behavior as TUI voice input."
  [request]
  (let [sid (some-> (get-in request [:path-params :sid]) parse-uuid)
        transcribe (try
                     (requiring-resolve
                       'com.blockether.vis.ext.foundation-voice.asr/transcribe-file!)
                     (catch Throwable _ nil))]
    (cond
      (not (and sid (vis/gateway-soul sid)))
      {:status 404 :headers {"Content-Type" "application/json"}
       :body (json-text {:error "unknown session"})}

      (nil? transcribe)
      {:status 501 :headers {"Content-Type" "application/json"}
       :body (json-text {:error "voice extension is not on the classpath"})}

      :else
      (let [tmp (java.io.File/createTempFile "vis-voice" ".wav")]
        (try
          (with-open [in ^java.io.InputStream (:body request)
                      out (io/output-stream tmp)]
            (io/copy in out))
          (if-not (wav-file? tmp)
            {:status 400 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:error "body must be a RIFF/WAVE audio file"})}
            {:status 200 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:text (str/trim (str (transcribe (str tmp))))})})
          (catch Throwable t
            {:status 400 :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json-text {:error (or (ex-message t) "transcription failed")})})
          (finally
            (.delete tmp)))))))

;; =============================================================================
;; CSS - the whole theme, one file, no inline styles.
;; vis-light tokens (internal/theme.clj light-palette):
;;   --bg :terminal-bg | --fg :text-fg | --panel2 :dialog-bg
;;   --code-bg :code-block-bg | --cream :turn-separator-bg
;;   --gold :header-tab-number-fg | --gold2 :turn-separator-fg
;;   --amber :code-result-fg | --amber-deep :warning-fg
;;   --warn-bg :warning-bg | --indigo :header-active-tab-bg
;;   --ok :status-ok | --err :status-bad
;; =============================================================================

(def ^:private APP_CSS
  "/* vis web companion - vis-light, chat-first */
/* ── reset (modern-normalize spirit): same rendering everywhere ──── */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
html{-webkit-text-size-adjust:100%;tab-size:4}
img,svg,video,canvas{display:block;max-width:100%}
input,button,textarea,select{font:inherit;color:inherit;letter-spacing:inherit}
button{background:none;border:0;cursor:pointer}
p,h1,h2,h3,h4,h5,h6{overflow-wrap:break-word}
ul[class],ol[class]{list-style:none}
a{text-decoration:none;color:inherit}
summary{list-style:none}summary::-webkit-details-marker{display:none}
:root{--bg:#ffffff;--panel2:#f8f8f8;--code-bg:#f0f3f8;--cream:#f8f4eb;
--line:#ececec;--line2:#dcdcdc;--fg:#1e1e1e;--dim:#8a8a8a;
--gold:#facc15;--gold2:#be9628;--amber:#a16207;--amber-deep:#503c00;--warn-bg:#fff5b4;
--indigo:#2563eb;--err:#dc3232;--ok:#28a03c;
--radius:16px;--shadow:0 1px 2px rgba(20,20,20,.05),0 6px 24px rgba(20,20,20,.07);
--mono:ui-monospace,SFMono-Regular,Menlo,monospace;
--serif:Charter,'Iowan Old Style',Georgia,serif}
html,body{height:100%}
body{background:var(--bg);color:var(--fg);
font:15px/1.6 -apple-system,BlinkMacSystemFont,'Segoe UI',Inter,sans-serif;
-webkit-font-smoothing:antialiased}
a{color:var(--indigo);text-decoration:none}
::selection{background:var(--warn-bg)}
.empty{color:var(--dim);font-style:italic}
@keyframes rise{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}
/* ── app shell ─────────────────────────────────────────────────── */
.app{height:100dvh;display:flex;flex-direction:column}
.bar{display:flex;align-items:center;gap:.8rem;padding:.6rem 1.1rem;
position:sticky;top:0;z-index:10;background:rgba(255,255,255,.82);
backdrop-filter:blur(12px) saturate(1.4);border-bottom:1px solid var(--line)}
.bar .back{font-size:1.05rem;color:var(--dim);padding:.1rem .45rem;border-radius:8px}
.bar .back:hover{background:var(--panel2);color:var(--fg)}
.bar-title{display:flex;align-items:center;gap:.6rem;min-width:0}
.bar-name{font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wordmark{font-weight:700;letter-spacing:.02em;border-bottom:3px solid var(--gold);padding-bottom:.05rem}
.session-id{margin-left:auto;font-family:var(--mono);font-size:.72rem;color:var(--dim)}
.layout{flex:1;display:flex;min-height:0}
/* center = thread + dock in ONE box between the rails, so the chat
   column and the composer share the exact same centering geometry */
.center{flex:1;display:flex;flex-direction:column;min-width:0}
/* ── thread ────────────────────────────────────────────────────── */
.thread{flex:1;overflow-y:auto;scroll-behavior:smooth;scrollbar-gutter:stable both-edges}
.column{max-width:46rem;margin:0 auto;padding:1.6rem 1.2rem 2.5rem;
display:flex;flex-direction:column;gap:1.3rem}
.hello-wrap{margin:16vh auto 0;text-align:center}
.hello{font-size:1.7rem;font-weight:650;letter-spacing:-.01em}
.hello-sub{color:var(--dim);margin-top:.5rem}
.msg{animation:rise .25s ease both}
.msg-user{display:flex;justify-content:flex-end}
.msg-user-body{background:var(--cream);border:1px solid #efe6cf;
border-radius:18px 18px 4px 18px;padding:.6rem 1rem;max-width:75%;
overflow-wrap:anywhere;box-shadow:0 1px 2px rgba(20,20,20,.04)}
.msg-vis{display:flex;gap:.85rem;align-items:flex-start}
.avatar{flex:none;width:28px;height:28px;border-radius:9px;background:var(--gold);
color:var(--amber-deep);font-weight:800;display:flex;align-items:center;
justify-content:center;font-size:.95rem;box-shadow:0 1px 2px rgba(20,20,20,.12);
user-select:none}
.msg-vis-body{min-width:0;flex:1;padding-top:.15rem}
.prose{font-family:var(--serif);font-size:1.02rem;line-height:1.7;
overflow-wrap:anywhere}
.prose p{margin:.45rem 0}
.prose ul,.prose ol{margin:.45rem 0 .45rem 1.35rem}
.prose li{margin:.2rem 0}
.prose h1,.prose h2,.prose h3{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
margin:1rem 0 .35rem;font-size:1.05rem;font-weight:650}
.prose blockquote{border-left:3px solid var(--gold);padding-left:.9rem;color:var(--dim);margin:.5rem 0}
.prose hr{border:0;border-top:1px solid var(--line);margin:.8rem 0}
.prose table{border-collapse:collapse;margin:.5rem 0;font-size:.9rem;font-family:-apple-system,sans-serif}
.prose td,.prose th{border:1px solid var(--line2);padding:.3rem .6rem}
.msg-meta{display:flex;gap:.7rem;align-items:center;margin-top:.5rem;
font-size:.72rem;color:var(--dim)}
/* typing dots */
.dots{display:inline-flex;gap:5px;padding:.4rem 0}
.dots span{width:7px;height:7px;border-radius:50%;background:var(--gold2);
animation:blink 1.2s infinite ease-in-out}
.dots span:nth-child(2){animation-delay:.18s}
.dots span:nth-child(3){animation-delay:.36s}
@keyframes blink{0%,80%,100%{opacity:.25;transform:scale(.85)}40%{opacity:1;transform:scale(1)}}
.thinking{min-height:1rem;font-size:.8rem;color:var(--dim);font-style:italic;overflow-wrap:anywhere}
/* work disclosure */
.work{border:1px solid var(--line);border-radius:12px;background:var(--panel2)}
.work summary{cursor:pointer;list-style:none;padding:.45rem .9rem;font-size:.74rem;
font-weight:600;text-transform:uppercase;letter-spacing:.08em;color:var(--dim);user-select:none}
.work summary::before{content:'▸ ';color:var(--gold2)}
.work[open] summary::before{content:'▾ '}
.activity{display:flex;flex-direction:column;gap:.4rem;padding:.2rem .9rem .8rem}
.act{border-left:2px solid var(--line2);padding:.15rem .7rem;font-size:.8rem;animation:rise .2s ease both}
.act-code{border-left-color:var(--indigo)}
.act-result{border-left-color:var(--gold)}
.act-error{border-left-color:var(--err)}.act .act-error{color:var(--err)}
.act-answer{border-left-color:var(--ok)}
.act-sent,.act-turn{border-left-color:var(--gold)}
/* chips */
.chip{font-size:.66rem;font-weight:650;text-transform:lowercase;letter-spacing:.03em;
border-radius:99px;padding:.1rem .55rem;background:var(--panel2);color:var(--dim)}
.chip-running{background:var(--warn-bg);color:var(--amber)}
.chip-completed,.chip-idle{background:#e9f7ec;color:var(--ok)}
.chip-failed,.chip-cancelled{background:#fdeaea;color:var(--err)}
.chip-suspended{background:#e8eefc;color:var(--indigo)}
/* code */
.ir-pre{background:var(--code-bg);border:1px solid var(--line);border-radius:10px;
padding:.6rem .8rem;overflow-x:auto;font-size:.8rem;margin:.4rem 0;line-height:1.5}
.ir-pre code,.ir-code{font-family:var(--mono)}
.ir-code{background:var(--code-bg);border-radius:5px;padding:.06rem .32rem;font-size:.86em;color:var(--amber)}
/* ── composer dock ─────────────────────────────────────────────── */
.dock{padding:.4rem 1.2rem 1.1rem;background:linear-gradient(to top,var(--bg) 65%,rgba(255,255,255,0))}
.composer{max-width:46rem;margin:0 auto;display:flex;align-items:flex-end;gap:.5rem;
background:var(--bg);border:1px solid var(--line2);border-radius:24px;
padding:.5rem .55rem .5rem 1.1rem;box-shadow:var(--shadow);transition:border-color .15s}
.composer:focus-within{border-color:var(--gold)}
.composer textarea{flex:1;border:0;outline:0;resize:none;background:transparent;
font:inherit;line-height:1.5;max-height:200px;padding:.25rem 0}
.composer input{flex:1;border:0;outline:0;background:transparent;font:inherit;padding:.25rem 0}
.send{flex:none;width:34px;height:34px;border-radius:50%;border:0;cursor:pointer;
background:var(--gold);color:var(--amber-deep);font-size:1.05rem;font-weight:800;
display:flex;align-items:center;justify-content:center;transition:filter .15s}
.send:hover{filter:brightness(1.07)}
/* ── sessions index ────────────────────────────────────────────── */
.index{padding-top:14vh;gap:1.6rem}
.index .hello{text-align:center}
.newsession{margin-top:.2rem}
.sessions-block{margin-top:1.6rem}
.block-label{font-size:.72rem;text-transform:uppercase;letter-spacing:.1em;color:var(--dim);margin-bottom:.6rem}
.sessions{list-style:none;display:flex;flex-direction:column;gap:.45rem}
.session-row{display:flex;align-items:center;gap:.7rem;background:var(--bg);
border:1px solid var(--line);border-radius:14px;padding:.75rem 1rem;color:var(--fg);
transition:border-color .12s,box-shadow .12s,transform .12s}
.session-row:hover{border-color:var(--gold);box-shadow:var(--shadow);transform:translateY(-1px)}
.session-title{font-weight:600}
.session-spacer{flex:1}
/* ── sessions sidebar (left) ───────────────────────────────────── */
.sidebar{width:15.5rem;flex:none;overflow-y:auto;border-right:1px solid var(--line);
padding:1rem .8rem;background:#fcfcfb;display:flex;flex-direction:column;gap:.9rem}
@media(max-width:60rem){.sidebar{display:none}}
.newchat-btn{width:100%;text-align:left;border:1px dashed var(--line2);border-radius:10px;
padding:.55rem .8rem;font-weight:600;font-size:.85rem;color:var(--amber);
transition:border-color .12s,background .12s}
.newchat-btn:hover{border-color:var(--gold);background:var(--warn-bg)}
.side-sessions{display:flex;flex-direction:column;gap:.15rem}
.side-row{display:flex;align-items:center;gap:.5rem;border-radius:9px;
padding:.45rem .7rem;font-size:.86rem;color:var(--fg);white-space:nowrap;
overflow:hidden;text-overflow:ellipsis;transition:background .1s}
.side-row:hover{background:var(--panel2)}
.side-row.active{background:var(--warn-bg);font-weight:600}
.side-title{overflow:hidden;text-overflow:ellipsis;flex:1}
.side-dot{flex:none;width:7px;height:7px;border-radius:50%;background:var(--gold2);
animation:blink 1.2s infinite ease-in-out}
/* live bubbles region */
.live{display:flex;flex-direction:column;gap:1.3rem}
.live:empty{display:none}
/* mic */
.mic{flex:none;width:34px;height:34px;border-radius:50%;display:flex;align-items:center;
justify-content:center;color:var(--dim);transition:color .15s,background .15s}
.mic:hover{background:var(--panel2);color:var(--amber)}
.mic.recording{background:var(--err);color:#fff;animation:pulse 1.2s infinite}
.mic.mic-error{color:var(--err)}
@keyframes pulse{0%,100%{box-shadow:0 0 0 0 rgba(220,50,50,.35)}50%{box-shadow:0 0 0 7px rgba(220,50,50,0)}}
/* ── context rail ──────────────────────────────────────────────── */
.rail{width:21rem;flex:none;overflow-y:auto;border-left:1px solid var(--line);
padding:1.2rem 1.1rem;background:#fcfcfb}
@media(max-width:68rem){.rail{display:none}}
.rail-head{display:flex;align-items:center;justify-content:space-between;gap:.8rem;margin-bottom:.9rem}
.rail-head h2{font-size:.78rem;text-transform:uppercase;letter-spacing:.12em;color:var(--gold2)}
.rail-section{border-top:1px solid var(--line);padding:.85rem 0}
.rail-section h3{font-size:.7rem;text-transform:uppercase;letter-spacing:.1em;color:var(--dim);margin-bottom:.5rem}
.util{display:flex;align-items:center;gap:.5rem;min-width:7rem}
.util-track{flex:1;height:5px;border-radius:3px;background:var(--panel2);overflow:hidden}
.util-fill{height:100%;background:linear-gradient(90deg,var(--gold),var(--gold2))}
.util-label{font-size:.68rem;color:var(--dim);white-space:nowrap}
.tasks{list-style:none;display:flex;flex-direction:column;gap:.4rem}
.task{display:flex;gap:.55rem;align-items:baseline;font-size:.84rem}
.task-glyph{color:var(--dim)}
.task-done .task-glyph,.task-completed .task-glyph{color:var(--ok)}
.task-candidate .task-glyph{color:var(--indigo)}
.task-in_progress .task-glyph,.task-running .task-glyph{color:var(--amber)}
.facts{display:flex;flex-direction:column;gap:.45rem}
.fact{background:var(--bg);border:1px solid var(--line);border-radius:10px;padding:.45rem .65rem}
.fact summary{cursor:pointer;list-style:none}
.fact-key{font-family:var(--mono);font-size:.78rem;color:var(--amber);
background:var(--warn-bg);border-radius:5px;padding:.04rem .4rem}
.fact-content{font-size:.83rem;margin:.4rem 0}
.fact-path{font-family:var(--mono);font-size:.74rem;color:var(--indigo)}
.fact-hash{font-family:var(--mono);font-size:.68rem;color:var(--gold2)}
/* ── auth ──────────────────────────────────────────────────────── */
.auth{min-height:100dvh;display:flex;align-items:center;justify-content:center;padding:1rem}
.auth-card{width:22rem;text-align:center;display:flex;flex-direction:column;gap:.9rem;
background:var(--bg);border:1px solid var(--line);border-radius:var(--radius);
box-shadow:var(--shadow);padding:2.2rem 1.8rem}
.auth-card h1{font-size:2.4rem;letter-spacing:.03em}
.auth-card h1{border-bottom:4px solid var(--gold);margin:0 auto;padding:0 .4rem .2rem}
.tagline{color:var(--dim)}
.auth-card form{display:flex;flex-direction:column;gap:.6rem}
.auth-card input{border:1px solid var(--line2);border-radius:10px;padding:.6rem .8rem;font:inherit}
.auth-card input:focus{outline:2px solid var(--gold);border-color:var(--gold)}
.send-wide{border:0;border-radius:10px;padding:.6rem;cursor:pointer;
background:var(--gold);color:var(--amber-deep);font-weight:700;font-size:.95rem}
.auth-error{color:var(--err);font-size:.85rem}
.auth-hint{color:var(--dim);font-size:.78rem}
/* scrollbars */
*::-webkit-scrollbar{width:10px;height:10px}
*::-webkit-scrollbar-thumb{background:var(--line2);border-radius:5px;border:2px solid var(--bg)}
*::-webkit-scrollbar-track{background:transparent}
")

(defn- css-handler [_]
  {:status 200
   :headers {"Content-Type" "text/css; charset=utf-8"
             "Cache-Control" "no-cache"}
   :body APP_CSS})

;; =============================================================================
;; Route contribution (whiteboard slot) + channel registration
;; =============================================================================

(defn- ui-routes
  "Reitit route data for the contribution; closes over the gateway token
   so /ui and /ui/auth can run the cookie exchange. Handlers go in as
   VARS so a REPL :reload serves new code on the very next request."
  [token]
  [["/ui" {:get #(index-handler % token)}]
   ["/ui/auth" {:post #(auth-handler % token)}]
   ["/ui/app.css" {:get #'css-handler}]
   ["/ui/js/:asset" {:get #'js-asset-handler}]
   ["/ui/dev-reload" {:get #'dev-reload-handler}]
   ["/ui/sessions" {:post #'create-session-handler}]
   ["/ui/session/:sid" {:get #'session-handler}]
   ["/ui/session/:sid/turns" {:post #'submit-turn-handler}]
   ["/ui/session/:sid/voice" {:post #'voice-handler}]
   ["/ui/session/:sid/stream" {:get #'stream-handler}]])

(defn- ui-contribution
  "The gateway pulls this through the `:gateway.slot/http-routes`
   whiteboard slot whenever it (re)builds its handler — no registration
   call, no ordering requirement between gateway start and this
   extension loading."
  []
  {:prefix "/ui"
   :routes ui-routes
   :open-uris #{"/ui" "/ui/auth" "/ui/app.css"
                "/ui/js/htmx.min.js" "/ui/js/htmx-sse.js"
                "/ui/js/ui.js" "/ui/js/dev-reload.js"}
   :request-authed-fn ui-authed?
   :on-unauthorized (fn [_request] {:status 303 :headers {"Location" "/ui"} :body ""})
   :form-params? true})

(defn- parse-flag [args flag]
  (some (fn [[a b]] (when (= a flag) b)) (partition 2 1 args)))

(defn channel-main
  "`vis channels web` - start the gateway (UI auto-mounted because this
   namespace is loaded), print the /ui address, park until SIGTERM."
  [args]
  (let [{:keys [port host token-file require-token?]}
        (vis/gateway-start! {:port (some-> (parse-flag args "--port") parse-long)
                             :host (parse-flag args "--host")
                             :token-file (parse-flag args "--token-file")
                             :require-token? (boolean (some #{"--require-token"} args))})]
    (println (str "vis web companion: http://" host ":" port "/ui"))
    (if require-token?
      (println (str "bearer token: " token-file))
      (println "auth: disabled (loopback default; pass --require-token to enable)"))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable vis/gateway-stop!))
    @(promise)))

(vis/register-extension!
  (vis/extension
    {:ext/name        "channel-web"
     :ext/description "Web companion - the gateway /ui chat instrument (hiccup + HTMX + SSE)."
     :ext/version     "0.2.0"
     :ext/author      "Blockether"
     :ext/owner       "vis"
     :ext/license     "Apache-2.0"
     :ext/channels    [{:channel/id      :web
                        :channel/cmd     "web"
                        :channel/doc     "Serve the gateway with the /ui web companion."
                        :channel/usage   "vis channels web [--port 7890] [--host 127.0.0.1]"
                        :channel/main-fn #'channel-main}]
     :ext/channel-contributions
     {:gateway.slot/http-routes [{:id :web/ui :fn #'ui-contribution}]}}))
