(ns com.blockether.vis.ext.channel-tui.provider
  "TUI provider management dialogs - model picker, model manager, provider router.
   Config I/O and data helpers live in tui/config.clj.

   The channel-neutral brain — status probing, limits, live model
   catalogs, presets, persistence shapes — lives in
   `com.blockether.vis.internal.providers` (exposed through `vis.core`)
   and can be SHARED across channels. This namespace owns only the
   lanterna interaction layer.

   GitHub Copilot OAuth: a hard dep. The TUI ships with the
   `vis-provider-github-copilot` jar on its classpath; the device-flow
   fns are required directly. (The previous `dynaload` indirection has
   been removed: explicit beats clever.)"
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.limits-fmt :as lfmt]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.provider-anthropic :as anthropic]
            [com.blockether.vis.ext.provider-github-copilot :as copilot]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.input KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen Screen$RefreshType TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Model list (core service + the TUI's 'Show all' affordance) ────────────

(defn- build-model-list
  "Build the model selection list from the gateway's LIVE catalog
   (`vis/gateway-provider-model-options`). The daemon owns OAuth token
   resolution, so the TUI NEVER builds a token-resolving router to list
   models — it proxies to the gateway. Appends the 'Show all models...'
   toggle when dated variants were hidden."
  [provider _default-models show-all?]
  (let
    [{:keys [models hidden-count]}
     (vis/gateway-provider-model-options (:id provider) show-all?)

     items
     (mapv (fn [id]
             {:label id :id id})
           models)]

    (if (and (not show-all?) (pos? (long hidden-count)))
      (conj items {:label "Show all models..." :id :show-all})
      items)))

;;; ── Provider setup dialog ──────────────────────────────────────────────────

(defn- select-model!
  "Show model selection dialog. Hides dated variants by default, with toggle to show all.
    Returns model id string or nil on cancel."
  [^TerminalScreen screen provider default-models]
  (loop [show-all? false]
    (let [models (build-model-list provider default-models show-all?)]
      (when-let [choice (dlg/select-dialog! screen "Select Model" models)]
        (if (= (:id choice) :show-all) (recur true) (:id choice))))))

(defn- select-provider-model!
  [^TerminalScreen screen provider]
  (select-model! screen provider (vis/provider-default-model-names provider)))

(def ^:private default-model-configs vis/provider-default-model-configs)

(def ^:private provider-config-with-models vis/provider-config-with-models)

;;; ── GitHub Copilot OAuth (hard dep) ──────────────────────────────────

(def ^:private github-copilot-account-types
  {:github-copilot-individual :individual
   :github-copilot-business :business
   :github-copilot-enterprise :enterprise})

(defn- github-copilot-provider? [provider-id] (contains? github-copilot-account-types provider-id))

(defn- github-copilot-account-type
  [provider-id]
  (get github-copilot-account-types provider-id :individual))

(def ^:private copilot-oauth-wait-poll-ms 200)

(def ^:private copilot-oauth-wait-timeout-ms (* 6 60 1000))

(def ^:private copilot-oauth-cancelled ::copilot-oauth-cancelled)

(defn- cancel-copilot-oauth-poll!
  [result]
  (when (instance? java.util.concurrent.Future result)
    (.cancel ^java.util.concurrent.Future result true)))

(defn- draw-copilot-waiting!
  [^TerminalScreen screen ^long started-at-ms]
  (let
    [size
     (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

     cols
     (.getColumns size)

     rows
     (.getRows size)

     g
     (.newTextGraphics screen)

     bounds
     (dlg/draw-dialog-chrome! g cols rows "GitHub Copilot - Waiting" 8)

     {:keys [left inner-w]}
     bounds

     left
     (long left)

     inner-w
     (long inner-w)

     {:keys [content-top content-h hint-row]}
     (dlg/dialog-layout bounds)

     content-top
     (long content-top)

     content-h
     (long content-h)

     hint-row
     (long hint-row)

     text-x
     (+ left 2)

     text-w
     (max 1 (- inner-w 2))

     elapsed-s
     (quot (max 0 (- (System/currentTimeMillis) started-at-ms)) 1000)

     lines
     ["Waiting for GitHub authorization..." "" "Finish login in the browser."
      "This dialog closes when GitHub confirms authorization." "" (str "Elapsed: " elapsed-s "s")]]

    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc left) content-top inner-w content-h)
    (doseq [[idx line] (map-indexed vector lines)]
      (let [row (+ content-top (long idx))]
        (when (< row (+ content-top content-h))
          (p/fill-rect! g (inc left) row inner-w 1)
          (p/put-str! g text-x row (dlg/ellipsize line text-w)))))
    (dlg/draw-hint-bar! g left hint-row inner-w [["Esc" "cancel"]])
    (.setCursorPosition screen (p/cursor-pos 0 0))
    (.refresh screen Screen$RefreshType/DELTA)))

(defn- wait-for-copilot-oauth!
  [^TerminalScreen screen result]
  (let
    [started-at-ms
     (System/currentTimeMillis)

     deadline-ms
     (+ started-at-ms (long copilot-oauth-wait-timeout-ms))]

    (loop []

      (cond (realized? result) @result
            (>= (System/currentTimeMillis) deadline-ms)
            (do (cancel-copilot-oauth-poll! result)
                (when screen
                  (dlg/text-view-dialog! screen
                                         "GitHub Copilot"
                                         ["Timed out waiting for GitHub authorization." ""
                                          "Restart auth when ready."]))
                copilot-oauth-cancelled)
            :else (do (when screen (draw-copilot-waiting! screen started-at-ms))
                      (if (and screen
                               (when-let [key (.pollInput screen)]
                                 (dlg/modal-escape-key? key)))
                        (do (cancel-copilot-oauth-poll! result) copilot-oauth-cancelled)
                        (do (Thread/sleep (long copilot-oauth-wait-poll-ms)) (recur))))))))

(defn- copilot-auth-instructions!
  [^TerminalScreen screen verification-uri user-code]
  (loop [status nil]
    (let
      [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
       cols (.getColumns size)
       rows (.getRows size)
       g (.newTextGraphics screen)
       bounds (dlg/draw-dialog-chrome! g cols rows "GitHub Copilot - Authenticate" 10)
       {:keys [left inner-w]} bounds
       {:keys [content-top content-h hint-row]} (dlg/dialog-layout bounds)
       left (long left)
       inner-w (long inner-w)
       content-top (long content-top)
       content-h (long content-h)
       hint-row (long hint-row)
       text-x (+ left 2)
       text-w (max 1 (- inner-w 2))
       url-label "Open this URL in your browser:"
       code-label "Enter this code in the browser:"
       help-lines ["After authorizing, press Enter here to continue."
                   "Click the URL to open it. Click the code to copy it."]
       url-row (min (+ content-top 1) (+ content-top content-h -1))
       code-row (min (+ content-top 4) (+ content-top content-h -1))
       status-row (min (+ content-top 8) (+ content-top content-h -1))
       url-col text-x
       code-col text-x]

      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g (inc left) content-top inner-w content-h)
      (doseq
        [[idx line] (map-indexed vector
                                 [url-label verification-uri "" code-label user-code ""
                                  (first help-lines) (second help-lines)])]
        (let [row (+ content-top (long idx))]
          (when (< row (+ content-top content-h))
            (p/fill-rect! g (inc left) row inner-w 1)
            (cond (= row url-row)
                  (do (p/set-colors! g t/link-chrome-fg t/dialog-bg)
                      (p/styled g
                                [p/BOLD]
                                (p/put-str! g url-col row (dlg/ellipsize verification-uri text-w))))
                  (= row code-row)
                  (do (p/set-colors! g t/link-chrome-fg t/dialog-bg)
                      (p/styled g
                                [p/BOLD]
                                (p/put-str! g code-col row (dlg/ellipsize user-code text-w))))
                  :else (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                            (p/put-str! g text-x row (dlg/ellipsize line text-w)))))))
      (when status
        (p/set-colors! g t/dialog-hint-key t/dialog-bg)
        (p/put-str! g text-x status-row (dlg/ellipsize status text-w)))
      (dlg/draw-hint-bar! g
                          left
                          hint-row
                          inner-w
                          [["Enter" "continue"] ["Click URL" "open"] ["Click code" "copy"]
                           ["Esc" "cancel"]])
      (.setCursorPosition screen (p/cursor-pos 0 0))
      (.refresh screen Screen$RefreshType/DELTA)
      (let [key (dlg/read-modal-key! screen)]
        (when key
          (cond (instance? MouseAction key)
                (let
                  [^MouseAction ma key
                   atype (.getActionType ma)
                   pos (.getPosition ma)
                   mx (.getColumn pos)
                   my (.getRow pos)
                   on-url? (and (= atype MouseActionType/CLICK_DOWN)
                                (= my url-row)
                                (>= mx url-col)
                                (< mx (+ url-col (count verification-uri))))
                   on-code? (and (= atype MouseActionType/CLICK_DOWN)
                                 (= my code-row)
                                 (>= mx code-col)
                                 (< mx (+ code-col (count user-code))))]

                  (cond on-url? (do (opener/open! verification-uri) (recur "Opened browser URL."))
                        on-code? (do (input/clipboard-copy! user-code)
                                     (recur "Copied device code to clipboard."))
                        :else (recur status)))
                :else (cond (dlg/modal-enter-key? key) true
                            (dlg/modal-escape-key? key) nil
                            (= KeyType/Character (.getKeyType key))
                            (case (Character/toLowerCase (.getCharacter key))
                              \o
                              (do (opener/open! verification-uri) (recur "Opened browser URL."))

                              \c
                              (do (input/clipboard-copy! user-code)
                                  (recur "Copied device code to clipboard."))

                              (recur status))
                            :else (recur status))))))))

(defn- copilot-oauth-flow!
  "Run the GitHub Copilot OAuth device flow inside the TUI.
   Shows the user code + URL, waits for authorization, returns the API key or nil.

   Returns nil immediately when the optional vis-providers-github-copilot
   jar isn't on the classpath."
  ([^TerminalScreen screen] (copilot-oauth-flow! screen :individual false))
  ([^TerminalScreen screen account-type] (copilot-oauth-flow! screen account-type false))
  ([^TerminalScreen screen account-type force?]
   (let
     [start-fn
      copilot/start-device-flow!

      poll-fn
      copilot/poll-for-token!

      exchange-fn
      copilot/get-copilot-token!

      detect-fn
      copilot/detect-oauth-token

      opts
      {:account-type account-type}]

     ;; Already authenticated?
     (if (and (not force?) (detect-fn))
       (try
         (let [{:keys [token]} (exchange-fn opts)]
           token)
         (catch Exception _
           (dlg/text-view-dialog! screen "Copilot" ["Existing token is invalid. Re-authenticate."])
           nil))
       ;; Device flow
       (try (let
              [start-result
               (vis/worker-future "vis-tui-copilot-oauth-start" #(start-fn opts))

               flow
               (wait-for-copilot-oauth! screen start-result)]

              (when-not (= copilot-oauth-cancelled flow)
                (let [{:keys [user-code verification-uri device-code interval expires-in]} flow]
                  (when (copilot-auth-instructions! screen verification-uri user-code)
                    (when force? (copilot/logout!))
                    ;; Poll in background, show waiting message
                    (let
                      [result (vis/worker-future "vis-tui-copilot-oauth-poll"
                                                 #(poll-fn device-code interval expires-in opts))
                       poll-result (wait-for-copilot-oauth! screen result)]

                      (when-not (= copilot-oauth-cancelled poll-result)
                        (let [{:keys [token]} (exchange-fn opts)]
                          ;; Success is silent: surfacing a redundant "Authenticated!" toast
                          ;; on top of the just-closed device-flow dialog confused users
                          ;; (cf. anthropic dialog feedback). Failure dialogs remain.
                          token)))))))
            (catch Exception e
              (dlg/text-view-dialog! screen "GitHub Copilot" [(str "Auth failed: " (ex-message e))])
              nil))))))

(defn- codex-oauth-ready!
  "Run OpenAI Codex browser OAuth from the TUI when needed.

   The shared provider flow owns browser launch and token exchange.
   The TUI supplies a dialog-backed manual collector for the final
   redirect URL, so the user can finish auth without dropping back to
   a shell prompt. With `force?`, start a fresh OAuth flow even when
   credentials already exist."
  ([^TerminalScreen screen] (codex-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (let
     [provider
      (vis/provider-by-id :openai-codex)

      detect-fn
      (:provider/detect-fn provider)]

     (if (and (not force?) detect-fn (detect-fn))
       true
       (when (dlg/confirm-dialog! screen
                                  "OpenAI Codex"
                                  ["Vis will start the ChatGPT/Codex browser OAuth flow." ""
                                   "After browser login, copy the final redirect URL from the"
                                   "address bar and paste it into the next dialog." ""
                                   "Fallback if needed:" "  vis providers auth openai-codex"])
         (try (let
                [_result (codex/login!
                           (constantly nil)
                           {:originator "vis-tui"
                            :force? force?
                            :manual-code-fn
                            (fn [_]
                              (dlg/text-input-dialog!
                                screen
                                "OpenAI Codex"
                                "Paste the final browser URL or authorization code:"))})]
                ;; Success is silent: parity with anthropic + copilot flows.
                true)
              (catch Exception e
                (dlg/text-view-dialog! screen
                                       "OpenAI Codex"
                                       [(str "Auth failed: " (ex-message e)) ""
                                        "If browser auth still fails here, run:"
                                        "  vis providers auth openai-codex"])
                false)))))))

(defn- anthropic-oauth-ready!
  "Run Anthropic Claude subscription browser OAuth from the TUI when needed."
  ([^TerminalScreen screen] (anthropic-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (let
     [provider
      (vis/provider-by-id :anthropic-coding-plan)

      detect-fn
      (:provider/detect-fn provider)]

     (if (and (not force?) detect-fn (detect-fn))
       true
       (when (dlg/confirm-dialog! screen
                                  "Anthropic"
                                  ["Vis will start the Anthropic Claude subscription OAuth flow." ""
                                   "After browser login, copy the final redirect URL from the"
                                   "address bar and paste it into the next dialog." ""
                                   "Fallback if needed:"
                                   "  vis providers auth anthropic-coding-plan"])
         (try (let
                [_result (anthropic/login!
                           (constantly nil)
                           {:force? force?
                            :manual-code-fn
                            (fn [_]
                              (dlg/text-input-dialog!
                                screen
                                "Anthropic"
                                "Paste the final browser URL or authorization code:"))})]
                true)
              (catch Exception e
                (dlg/text-view-dialog! screen
                                       "Anthropic"
                                       [(str "Auth failed: " (ex-message e)) ""
                                        "If browser auth still fails here, run:"
                                        "  vis providers auth anthropic-coding-plan"])
                false)))))))

(defn- add-provider!
  "Show add-provider flow. `existing-ids` is a set of already-configured :id keywords."
  [^TerminalScreen screen existing-ids]
  (let [available (vec (remove #(contains? existing-ids (:id %)) (vis/provider-presets)))]
    (if (empty? available)
      (do (dlg/text-view-dialog! screen "Add Provider" ["All providers already configured."]) nil)
      (when-let [preset (dlg/select-dialog! screen "Add Provider" available)]
        (let
          [pid (:id preset)
           local? (contains? #{:ollama :lmstudio} pid)
           ;; Local providers (LM Studio / Ollama) run wherever the user
           ;; hosts them, so let them override the default host:port.
           ;; Blank input or Esc keeps the preset default.
           base-url (if local?
                      (or (some-> (dlg/text-input-dialog! screen
                                                          (str (:label preset) " Setup")
                                                          "Base URL:"
                                                          :initial
                                                          (or (:base-url preset) ""))
                                  str/trim
                                  (str/replace #"/+$" "")
                                  not-empty)
                          (:base-url preset))
                      (:base-url preset))
           preset (assoc preset :base-url base-url)
           has-key? (some? (:api-key preset))
           ;; OAuth providers store credentials outside config.
           oauth?
           (or (github-copilot-provider? pid) (= :openai-codex pid) (= :anthropic-coding-plan pid))
           ;; Local providers need no key
           needs-key? (not (or has-key? oauth? local?))
           api-key (cond has-key? (:api-key preset)
                         (github-copilot-provider? pid)
                         (copilot-oauth-flow! screen (github-copilot-account-type pid))
                         (= pid :openai-codex) (when (codex-oauth-ready! screen) :oauth-ready)
                         (= pid :anthropic-coding-plan) (when (anthropic-oauth-ready! screen)
                                                          :oauth-ready)
                         needs-key? (let
                                      [raw (dlg/text-input-dialog! screen
                                                                   (str (:label preset) " Setup")
                                                                   "API Key:"
                                                                   :mask
                                                                   \*)]
                                      (when-not (str/blank? raw) raw))
                         :else nil)
           auth-ok? (cond has-key? true
                          oauth? (some? api-key)
                          needs-key? (some? api-key)
                          :else true)]

          (when auth-ok?
            (if-let [oauth-models (when oauth? (not-empty (default-model-configs preset)))]
              (provider-config-with-models preset oauth-models)
              (when-let
                [model (select-provider-model! screen
                                               (cond->
                                                 {:id (:id preset)
                                                  :base-url base-url
                                                  :default-models (:default-models preset)}
                                                 api-key
                                                 (assoc :api-key api-key)))]
                (cond-> (provider-config-with-models preset [{:name model}])
                  (and api-key (not oauth?))
                  (assoc :api-key api-key))))))))))

;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, p/clamp, dlg/visible-window-start, dlg/clear-screen!

(defn- priority-label
  "Compact priority badge. Circled numerals ①–⑳ (U+2460+) read as a single
   tidy glyph next to the provider label; beyond 20 fall back to `(N)`."
  [^long idx]
  (let [n (inc idx)]
    (if (<= 1 n 20) (str (char (+ 0x245F n))) (str "(" n ")"))))

(def ^:private url-host vis/provider-url-host)

(def ^:private ^:const card-rows 3)   ;; lines per card

(def ^:private ^:const card-gap 1)    ;; blank line between cards

(defn- card-height
  "Total rows for n provider cards including gaps."
  ^long [^long n]
  (if (pos? n) (+ (* n card-rows) (* (dec n) card-gap)) 0))

(defn- card-start-row
  "Starting row offset for card at index i."
  ^long [^long i]
  (* i (+ card-rows card-gap)))

(defn- card-visible-count
  "Number of full two-line cards visible in `content-h`, respecting the
   one-row gap between cards."
  ^long [^long content-h]
  (max 1 (quot (+ (max 0 content-h) card-gap) (+ card-rows card-gap))))

(defn- card-window-start
  [^long selected ^long current-start ^long content-h ^long total]
  (dlg/visible-window-start selected current-start (card-visible-count content-h) total))

(defn- draw-provider-card!
  "Draw a 2-line provider card.
    Line 1: ① Label  url.host  ●
    Line 2:    ★ root-model  (+N models) / RPM/TMP summary"
  [g left row inner-w idx selected? provider status limits]
  ;; Reserve `p/SELECTION_WIDTH` cols at the start of the card row
  ;; for the selection gutter (`>` glyph at `(inc left)` + 1 margin
  ;; col). Card body text shifts right by the gutter so the marker
  ;; sits inside the dialog's inner edge with breathing room before
  ;; the priority label.
  (let
    [left
     (long left)

     row
     (long row)

     inner-w
     (long inner-w)

     idx
     (long idx)

     text-w
     (max 0 (- inner-w 2 p/SELECTION_WIDTH))

     text-x
     (+ left 2 p/SELECTION_WIDTH)

     pri
     (priority-label idx)

     host
     (url-host (or (vis/provider-base-url provider) ""))

     loading-status?
     (get status "is_loading")

     loading-limits?
     (= :loading (:status limits))

     ok?
     (boolean (get status "is_authenticated"))

     label
     (vis/display-label (:id provider))

     models
     (or (:models provider) [])

     model-count
     (count (or models []))

     root-name
     (or (:name (first models)) "--")

     suffix
     (cond (<= model-count 1) "(1 model)"
           (= model-count 2) "(+1 model)"
           :else (str "(+" (dec model-count) " models)"))

     ;; Dynamic per-account rows (e.g. `:zai-coding-plan-5h`, `:codex-7d`)
     ;; come from `[:dynamic :limits]`; they're what the footer shows
     ;; and what the user actually cares about. Static `:rpm`/`:tpm`
     ;; are svar catalog defaults (`{:rpm 500 :tpm 2000000}`), the
     ;; same for every provider - useful as a fallback only when no
     ;; dynamic rows are reported. Sharing `lfmt/dynamic-summary`
     ;; with footer.clj keeps both surfaces in sync.
     dynamic-text
     (when-not loading-limits? (lfmt/dynamic-summary limits))

     limit-summary
     (->> [(when loading-status? "checking auth") (when loading-limits? "checking limits")
           dynamic-text
           (when-not dynamic-text
             (when-let [rpm (get-in limits [:static :rpm])]
               (str "catalog RPM " rpm)))
           (when-not dynamic-text
             (when-let [tpm (get-in limits [:static :tpm])]
               (str "catalog TPM " tpm)))]
          (remove nil?)
          (str/join " / "))

     ;; Layout line 1:  "① Label" ... "host  ●"
     left-part
     (str pri " " (or label "?"))

     right-part
     (str host "  ●")]

    ;; Selection visual: the cursor is a `> ` glyph painted in the
    ;; dialog padding column (between the dialog frame and the card
    ;; body). The card itself keeps the normal `dialog-bg` palette so
    ;; URL hint, status dot color and dim subtitle survive selection
    ;; — previously the inverse-on-`dialog-title-bg` path collapsed
    ;; all four colors onto `dialog-title-fg`.
    (p/set-bg! g t/dialog-bg)
    (dotimes [r card-rows]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))
    ;; `> ` glyph in the dialog padding column, anchored to line 1.
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/draw-selection-marker! g (inc left) row selected?)
    ;; Line 1 left - priority + label (bold)
    (p/set-fg! g t/dialog-fg)
    (p/styled g
              [p/BOLD]
              (p/put-str! g text-x row (dlg/ellipsize left-part (- text-w (count right-part) 1))))
    ;; Line 1 right - host (italic dimmed) + status dot
    (let
      [dot-col
       (+ text-x text-w -1)

       host-col
       (- dot-col 2 (count host))]

      ;; Host
      (p/set-fg! g t/dialog-hint)
      (p/styled g [p/ITALIC] (p/put-str! g (max (+ text-x (count left-part) 1) host-col) row host))
      ;; Status dot - green/red after probe, dim while background checks run.
      (p/set-fg! g
                 (cond (or loading-status? loading-limits?) t/dialog-hint-key
                       ok? t/status-ok
                       :else t/status-bad))
      (p/put-str! g dot-col row "●"))
    ;; Line 2 - a connection / diagnostics error wins (red); otherwise the
    ;; model + static limits summary. Surfacing `:error` here is what makes a
    ;; dead local provider (Ollama / LM Studio not running) actually SAY so
    ;; instead of just a silent red dot.
    (let
      [error-text (when-not (or loading-status? loading-limits?)
                    (or (:error status) (get-in limits [:error :message])))]
      (if (seq error-text)
        (do (p/set-fg! g t/status-bad)
            (p/put-str! g text-x (inc row) (dlg/ellipsize (str "   ⚠ " error-text) text-w)))
        (do (p/set-fg! g t/dialog-fg)
            (p/put-str! g
                        text-x
                        (inc row)
                        (dlg/ellipsize (str "   ★ "
                                            root-name
                                            "  "
                                            suffix
                                            (when (seq limit-summary) (str " / " limit-summary)))
                                       text-w)))))))

(defn- draw-model-card!
  "Two-line model card. Mirrors `draw-provider-card!` layout:
     Line 1: ① model-name                         ★ Primary
     Line 2:    -> then {next}  /  after {previous} -> then {next}  /  ...

   Line 2 spells out the **default fallback chain**: svar's default
   routing picks `(first candidates)` from this provider's `:models`
   after filtering, so list order = chain order.

   `previous-name` and `next-name` are the names of the model just before /
   after this one in the chain (nil at the ends)."
  [g left row inner-w idx selected? is-root? _provider-id model previous-name next-name]
  ;; Same selection-gutter convention as `draw-provider-card!`.
  (let
    [left
     (long left)

     row
     (long row)

     inner-w
     (long inner-w)

     idx
     (long idx)

     model-name
     (or (:name model) (str "model-" (inc idx)))

     text-w
     (max 0 (- inner-w 2 p/SELECTION_WIDTH))

     text-x
     (+ left 2 p/SELECTION_WIDTH)

     pri
     (priority-label idx)

     left-part
     (str pri " " model-name)

     tag
     (when is-root? "★ Primary")

     ;; Build the chain breadcrumb. Use Unicode arrows so the flow
     ;; reads left-to-right at a glance: "after X -> then Y".
     subtitle
     (cond (and (nil? previous-name) (nil? next-name)) "   only model -- no fallback configured"
           (nil? previous-name) (str "   -> then " next-name)
           (nil? next-name) (str "   after " previous-name " -- last fallback")
           :else (str "   after " previous-name " -> then " next-name))]

    ;; See `draw-provider-card!` for the rationale: keep the body in
    ;; the normal palette and paint a `> ` cursor glyph in the dialog
    ;; padding column instead of inverting the entire card.
    (p/set-bg! g t/dialog-bg)
    (dotimes [r card-rows]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/draw-selection-marker! g (inc left) row selected?)
    ;; Line 1 left - priority + model name (bold), trimmed to leave room
    ;; for the right-aligned tag.
    (let [reserved (if tag (+ (count tag) 1) 0)]
      (p/set-fg! g t/dialog-fg)
      (p/styled g
                [p/BOLD]
                (p/put-str! g text-x row (dlg/ellipsize left-part (max 0 (- text-w reserved))))))
    ;; Line 1 right - ★ Primary tag, right-aligned in status-ok green.
    (when tag
      (let [tag-col (+ text-x (- text-w (count tag)))]
        (p/set-fg! g t/status-ok)
        (p/styled g [p/BOLD] (p/put-str! g tag-col row tag))))
    ;; Line 2 - dimmed italic chain breadcrumb.
    (p/set-fg! g t/dialog-hint)
    (p/styled g [p/ITALIC] (p/put-str! g text-x (inc row) (dlg/ellipsize subtitle text-w)))))

(defn- swap-items
  [items i j]
  (-> items
      (assoc i (nth items j))
      (assoc j (nth items i))))

(defn- remove-provider-by-id [items provider-id] (vec (remove #(= provider-id (:id %)) items)))

(defn- move-model-to-front
  [models idx]
  (if (or (neg? (long idx)) (>= (long idx) (count models)) (zero? (long idx)))
    models
    (let [m (nth models idx)]
      (vec (cons m (concat (subvec models 0 idx) (subvec models (inc (long idx)))))))))


;; Channel-neutral status / limits / persistence shapes — the core
;; provider service (channel-neutral). Aliased privately so
;; the dialog code below reads unchanged.
(def ^:private persisted-provider-config vis/provider-persisted-config)

(def ^:private local-no-auth-provider-ids vis/provider-local-no-auth-ids)

(def ^:private initial-provider-status vis/provider-initial-status)

(def ^:private initial-provider-limits vis/provider-initial-limits)

(defn- gateway-provider-status-safe
  [provider]
  (try (vis/gateway-provider-status (:id provider))
       (catch Throwable e {"is_authenticated" false "error" (or (ex-message e) (str e))})))

(defn- gateway-provider-limits-safe
  [provider]
  (try (vis/gateway-provider-limits (:id provider))
       (catch Throwable e
         {:provider-id (:id provider)
          :status :error
          :static {}
          :dynamic {:limits []}
          :error {:message (or (ex-message e) (str e))}})))

(def ^:private safe-provider-status gateway-provider-status-safe)

(def ^:private configured-provider-status gateway-provider-status-safe)

(def ^:private safe-provider-limits gateway-provider-limits-safe)

(defn- refresh-provider-diagnostics!
  [provider statuses limits]
  (let [pid (:id provider)]
    (swap! statuses assoc pid (initial-provider-status provider))
    (swap! limits assoc pid (initial-provider-limits provider))
    (vis/worker-future "vis-tui-provider-status"
                       #(swap! statuses assoc pid (configured-provider-status provider)))
    (vis/worker-future "vis-tui-provider-limits"
                       #(swap! limits assoc pid (safe-provider-limits provider))))
  nil)

(defn- refresh-providers-diagnostics!
  [providers statuses limits]
  (doseq [provider providers]
    (refresh-provider-diagnostics! provider statuses limits))
  nil)

(defn- provider-diagnostics-loading?
  [statuses limits]
  (boolean (or (some #(get % "is_loading") (vals statuses))
               (some #(= :loading (:status %)) (vals limits)))))

(defn- provider-authenticated?
  ([provider] (boolean (get (configured-provider-status provider) "is_authenticated")))
  ([_provider status] (boolean (get status "is_authenticated"))))

(defn show-provider-status!
  "Status + limits as the RICH canonical markdown form, painted through the IR
   walker — the same report the web renders as markdown. The fallback arity
   fetches diagnostics through the gateway, never through local provider OAuth."
  ([^TerminalScreen screen provider]
   (show-provider-status! screen
                          provider
                          (configured-provider-status provider)
                          (safe-provider-limits provider)))
  ([^TerminalScreen screen provider status limits]
   (dlg/markdown-viewer-dialog! screen
                                (str (vis/display-label (:id provider)) " Status & Limits")
                                (vis/provider-status-md provider status limits))))

(defn- provider-supports-auth?
  [provider]
  (not (contains? local-no-auth-provider-ids (:id provider))))

(defn- status-md->line
  "Flatten ONE markdown status line to plain terminal text: drop heading `#`,
   emphasis `**`/`__`/`_`, and inline-code backticks; turn `- ` bullets into `• `."
  [s]
  (-> (str s)
      (str/replace #"^\s*#{1,6}\s+" "")
      (str/replace #"^(\s*)[-*]\s+" "$1• ")
      (str/replace #"\*\*" "")
      (str/replace #"__" "")
      (str/replace #"`" "")
      (str/replace #"(?<![A-Za-z0-9])_([^_]+)_(?![A-Za-z0-9])" "$1")))

(defn provider-action-items
  ([provider] (provider-action-items provider (configured-provider-status provider)))
  ([provider status]
   (let
     [registered
      (vis/provider-by-id (:id provider))

      is-authenticated
      (provider-authenticated? provider status)

      auth-label
      (if is-authenticated "Re-authenticate" "Authenticate")]

     (cond-> [{:id :models :label "Configure Models" :key \m}]
       (provider-supports-auth? provider)
       (conj {:id :authenticate :label auth-label :key \a :force? is-authenticated})

       (or (:provider/status-fn registered) (:provider/detect-fn registered) (:api-key provider))
       (conj {:id :status :label "Show Status + Limits" :key \s})

       (or (:provider/logout-fn registered) (:api-key provider))
       (conj {:id :logout :label "Log Out" :key \l})))))

(def ^:private api-key-prompt-cancelled ::api-key-prompt-cancelled)

(defn- trim-blank-lines
  [lines]
  (->> lines
       (drop-while str/blank?)
       reverse
       (drop-while str/blank?)
       reverse
       vec))

(defn- provider-auth-prompt-body
  [provider]
  (let [registered (vis/provider-by-id (:id provider))]
    (if-let [prompt-fn (:provider/auth-prompt-fn registered)]
      (not-empty (trim-blank-lines (prompt-fn)))
      (when-let [auth-fn (:provider/auth-fn registered)]
        (let [lines (atom [])]
          (try (auth-fn #(swap! lines conj %))
               (not-empty (trim-blank-lines @lines))
               (catch Throwable e
                 (not-empty (trim-blank-lines (conj @lines
                                                    ""
                                                    (str "Authentication info failed: "
                                                         (or (ex-message e) (str e)))))))))))))

(defn- prompt-for-api-key!
  [^TerminalScreen screen provider]
  (let
    [raw (dlg/text-input-dialog! screen
                                 (str (vis/display-label (:id provider)) " Authentication")
                                 "API Key:"
                                 :mask \*
                                 :flat? true
                                 :logo dlg/vis-logo-lines
                                 :body (provider-auth-prompt-body provider))]
    (cond (nil? raw) api-key-prompt-cancelled
          (str/blank? raw) nil
          :else (assoc provider :api-key raw))))

(def ^:private auth-fn-success-results
  "Return values that signal auth-fn completed successfully and the user does
   not need to read printed instructions. Any other return value (or `nil`)
   means \"user must act\" and printed lines (e.g. instructions, env-var hints)
   should be surfaced. Throwing is always a failure, handled separately."
  #{:ok :already-authenticated :authenticated true})

(defn- run-generic-provider-auth!
  [^TerminalScreen screen provider]
  (let [registered (vis/provider-by-id (:id provider))]
    (if-let [auth-fn (:provider/auth-fn registered)]
      (let
        [lines (atom [])
         print! #(swap! lines conj %)]

        (try
          (let [result (auth-fn print!)]
            ;; Success is silent: typical/standard providers (zai-coding, etc.)
            ;; print "Already authenticated with X." or "Persisted X key from
            ;; env var.\" on the success path - surfacing those as a popup is
            ;; exactly the noise the user vetoed (cf. anthropic/copilot/codex).
            ;; Lines are surfaced ONLY when auth-fn signals it could not
            ;; complete on its own (`:no-credentials`, `nil`, `false`,
            ;; or any non-success keyword) so the user knows what to do next.
            (when-not (contains? auth-fn-success-results result)
              (when-let [collected (seq @lines)]
                (dlg/text-viewer-dialog! screen
                                         (str (vis/display-label (:id provider)) " Authentication")
                                         (str/join "\n" collected)))))
          provider
          (catch Throwable e
            (dlg/text-viewer-dialog!
              screen
              (str (vis/display-label (:id provider)) " Authentication")
              (str/join "\n"
                        (concat @lines
                                ["" (str "Authentication failed: " (or (ex-message e) (str e)))])))
            nil)))
      (do (dlg/text-view-dialog! screen
                                 "Authenticate Provider"
                                 [(str (vis/display-label (:id provider))
                                       " does not expose an interactive auth flow.")])
          nil))))

(defn authenticate-provider!
  ([^TerminalScreen screen provider] (authenticate-provider! screen provider false))
  ([^TerminalScreen screen provider force?]
   (cond (github-copilot-provider? (:id provider))
         (when (copilot-oauth-flow! screen (github-copilot-account-type (:id provider)) force?)
           provider)
         (= :openai-codex (:id provider)) (when (codex-oauth-ready! screen force?) provider)
         (= :anthropic-coding-plan (:id provider)) (when (anthropic-oauth-ready! screen force?)
                                                     provider)
         (= :ollama (:id provider)) nil
         (= :lmstudio (:id provider)) nil
         :else (let [prompted (prompt-for-api-key! screen provider)]
                 (cond (= api-key-prompt-cancelled prompted) nil
                       prompted prompted
                       :else (run-generic-provider-auth! screen provider))))))

(defn- perform-logout!
  "Network logout + config removal for `provider`. No dialogs — the caller owns
   confirmation and any success feedback. Returns true."
  [provider]
  (let
    [provider-id
     (:id provider)

     registered
     (vis/provider-by-id provider-id)]

    (when-let [logout-fn (:provider/logout-fn registered)]
      (logout-fn))
    (vis/remove-config-provider! provider-id :tui-provider-logout)
    true))

(defn logout-provider!
  [^TerminalScreen screen provider]
  (let
    [provider-id
     (:id provider)

     registered
     (vis/provider-by-id provider-id)]

    (when (dlg/confirm-dialog! screen
                               (str (vis/display-label provider-id) " Authentication")
                               [(str "Log out of " (vis/display-label provider-id) "?")])
      (when-let [logout-fn (:provider/logout-fn registered)]
        (logout-fn))
      (vis/remove-config-provider! provider-id :tui-provider-logout)
      (dlg/text-view-dialog!
        screen
        (str (vis/display-label provider-id) " Authentication")
        [(str "Logged out of " (vis/display-label provider-id) ". Provider removed from config.")])
      true)))

(defn auth-provider-items
  []
  (->> (vis/registered-providers)
       (remove #(contains? local-no-auth-provider-ids (:provider/id %)))
       (map (fn [provider]
              (let [status (safe-provider-status provider)]
                {:provider-id (:provider/id provider)
                 :provider provider
                 :label
                 (str (:provider/label provider)
                      " / "
                      (if (get status "is_authenticated") "authenticated" "not authenticated"))})))
       (sort-by :label)
       vec))

(defn show-provider-auth-dialog!
  [^TerminalScreen screen]
  (when-let [item (dlg/select-dialog! screen "Authenticate Provider" (auth-provider-items))]
    (let [provider (or (:provider item) (vis/provider-by-id (:provider-id item)))]
      (cond (github-copilot-provider? (:provider/id provider))
            (boolean (copilot-oauth-flow! screen
                                          (github-copilot-account-type (:provider/id provider))))
            (= :openai-codex (:provider/id provider)) (boolean (codex-oauth-ready! screen false))
            (= :anthropic-coding-plan (:provider/id provider))
            (boolean (anthropic-oauth-ready! screen false))
            :else (if-let [auth-fn (:provider/auth-fn provider)]
                    (let
                      [lines (atom [])
                       print! #(swap! lines conj %)]

                      (try (let [result (auth-fn print!)]
                             ;; Same silent-success rule as run-generic-provider-auth!.
                             ;; Lines surface only when auth-fn signals the user must act.
                             (when-not (contains? auth-fn-success-results result)
                               (when-let [collected (seq @lines)]
                                 (dlg/text-viewer-dialog! screen
                                                          (str (:provider/label provider)
                                                               " Authentication")
                                                          (str/join "\n" collected))))
                             result)
                           (catch Throwable e
                             (dlg/text-viewer-dialog!
                               screen
                               (str (:provider/label provider) " Authentication")
                               (str/join "\n"
                                         (concat @lines
                                                 [""
                                                  (str "Authentication failed: "
                                                       (or (ex-message e) (str e)))])))
                             nil)))
                    nil)))))

;; ── First-run welcome ──────────────────────────────────────────────────────

(def ^:private welcome-lines
  "Centered brand moment. Kept sparse on purpose — one mark, one promise, one
   action. Accent lines (the wordmark + the call to action) are highlighted."
  ["◆  v i s" "Your terminal, now agentic." "" "To begin, connect an AI provider." ""
   "→  Connect a provider" "" "Sign in: GitHub · OpenAI · Anthropic"
   "…or paste an API key   …or run local"])

(def ^:private welcome-accent-lines #{"◆  v i s" "→  Connect a provider"})

(def ^:private how-key-lines
  ["How vis uses your provider" "" "• Your API key (or OAuth token) is stored locally in"
   "  ~/.vis/config.edn — on this machine only."
   "• vis sends your prompts and the files you ask it to read"
   "  directly to the provider you choose. Nothing else."
   "• No vis servers sit in between. No telemetry of your code."
   "• Remove a provider any time from the Router (C-x C-p → Router)." ""
   "Local providers (Ollama / LM Studio) keep everything on-device."])

(defn show-welcome!
  "First-run welcome screen. The single primary action (Enter) drops straight
   into the provider picker; `?` explains how the key is used; Esc quits.

   Returns `{:providers [cfg]}` once a provider is added, or nil if the user
   quits without connecting one."
  [^TerminalScreen screen]
  (loop []

    (let
      [size
       (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

       cols
       (.getColumns size)

       rows
       (.getRows size)

       g
       (.newTextGraphics screen)

       bounds
       (dlg/draw-dialog-chrome! g cols rows "Welcome to vis" nil)

       {:keys [left inner-w]}
       bounds

       {:keys [content-top content-h hint-row]}
       (dlg/dialog-layout bounds)

       left
       (long left)

       inner-w
       (long inner-w)

       content-top
       (long content-top)

       content-h
       (long content-h)]

      (p/set-bg! g t/dialog-bg)
      (p/fill-rect! g (inc left) content-top inner-w content-h)
      (let
        [n
         (long (count welcome-lines))

         start
         (+ content-top (long (max 0 (quot (- content-h n) 2))))]

        (doseq [[i line] (map-indexed vector welcome-lines)]
          (p/set-colors!
            g
            ;; Accent lines (the `v i s` wordmark + the connect CTA) use the
            ;; brand accent, NOT `dialog-title-fg` — that is white, meant for
            ;; the dark title BAR, so it vanished on the light dialog BODY.
            ;; `header-active-tab-accent` has real contrast on every theme's
            ;; dialog background (indigo on light, sky on dark).
            (if (contains? welcome-accent-lines line) t/header-active-tab-accent t/dialog-fg)
            t/dialog-bg)
          (p/draw-centered! g (inc left) (+ start (long i)) inner-w line)))
      (dlg/draw-hint-bar! g
                          left
                          hint-row
                          inner-w
                          [["Enter" "connect a provider"] ["?" "how your key is used"]
                           ["Esc" "quit"]])
      (.setCursorPosition screen (p/cursor-pos 0 0))
      (.refresh screen Screen$RefreshType/DELTA)
      (let [key (dlg/read-modal-key! screen)]
        (if (nil? key)
          (recur)
          (condp = (.getKeyType key)
            KeyType/Enter (if-let [cfg (add-provider! screen #{})]
                            ;; PERSIST to ~/.vis/config.edn — same path the
                            ;; provider manager uses (see Esc branch above).
                            ;; Returning the config in-memory only made the
                            ;; first-run connect vanish on exit, so the next
                            ;; launch saw an empty config and re-showed the
                            ;; welcome screen. Preserve any other global keys.
                            (let
                              [persisted (assoc (or (vis/load-config-raw) {})
                                           "providers" [(persisted-provider-config cfg)])]
                              (vis/save-config! persisted)
                              persisted)
                            (recur))
            KeyType/Escape nil
            KeyType/Character
            (do (when (= \? (.getCharacter key))
                  (dlg/text-view-dialog! screen "How vis uses your key" how-key-lines))
                (recur))
            (recur)))))))

(defn show-provider-dialog!
  "Provider manager dialog.
   Esc saves and closes, returning {:providers [...]} in priority order.
   Optional `current-config` seeds the dialog with current state."
  ([^TerminalScreen screen] (show-provider-dialog! screen nil nil))
  ([^TerminalScreen screen current-config] (show-provider-dialog! screen current-config nil))
  ([^TerminalScreen screen current-config _opts]
   (let
     [seed
      (or current-config (vis/load-config) {:providers []})

      items
      (atom (vec (or (:providers seed) [])))

      statuses
      (atom (into {}
                  (map (fn [provider]
                         [(:id provider) (initial-provider-status provider)]))
                  @items))

      limits
      (atom (into {}
                  (map (fn [provider]
                         [(:id provider) (initial-provider-limits provider)]))
                  @items))

      selected
      (atom 0)

      scroll
      (atom 0)

      mode
      (atom :list)

      action-sel
      (atom 0)

      status-scroll
      (atom 0)

      pending
      (atom nil)

      model-items
      (atom nil)

      model-sel
      (atom 0)

      model-scroll
      (atom 0)]

     (refresh-providers-diagnostics! @items statuses limits)
     (loop []

       (let
         [size
          (or (.doResizeIfNecessary screen) (.getTerminalSize screen))

          cols
          (.getColumns size)

          rows
          (.getRows size)

          g
          (.newTextGraphics screen)

          ;; Do NOT clear the whole terminal here - keep the chat
          ;; visible behind the dialog. Sub-dialog artifact concern is
          ;; moot: the remaining sub-modals (`add-provider!`,
          ;; `select-model!`, the OAuth flows) repaint their own chrome on
          ;; every frame and on return the parent loop's next iteration
          ;; redraws the parent chrome on top of any leftovers. The model
          ;; editor and every provider action now live inline as bottom
          ;; transients / a full-content `:models` sub-view of THIS chrome.
          total
          (long (count @items))

          ;; Size the box to the cards via the explicit-height chrome arity.
          ;; The default arity substitutes a tall proportional footprint and
          ;; vertically centers the cards inside it, leaving dead empty rows
          ;; after the last provider. `golden-dialog-size` floors height to
          ;; `content + chrome`, so passing the real card height fits the box
          ;; (and clamps to the terminal when there are many providers).
          ;; Floor the Router at the full default footprint so it reads as a
          ;; substantial panel (more height), not a tiny box hugging 2 cards;
          ;; a long provider list still grows past it and scrolls.
          content-rows
          (max (card-height (max 1 total)) (dlg/default-content-height rows))

          bounds
          (dlg/draw-dialog-chrome! g
                                   cols
                                   rows
                                   "Router"
                                   (dlg/default-content-width cols)
                                   content-rows)

          {:keys [left inner-w]}
          bounds

          {:keys [content-top content-h hint-row]}
          (dlg/dialog-layout bounds content-rows)

          left
          (long left)

          inner-w
          (long inner-w)

          content-top
          (long content-top)

          content-h
          (long content-h)

          visible-count
          (card-visible-count content-h)

          scrollable?
          (> total visible-count)

          card-inner-w
          (if scrollable? (max 1 (dec inner-w)) inner-w)

          _
          (swap! selected #(p/clamp % 0 (max 0 (dec total))))

          _
          (swap! scroll #(card-window-start @selected % content-h total))]

         ;; Clear content area
         (p/set-bg! g t/dialog-bg)
         (p/fill-rect! g (inc left) content-top inner-w content-h)
         (cond
           (= @mode :status)
           ;; Inline status + limits — the selected provider card on top, its
           ;; markdown Status & Limits report as plain rows below, scrollable.
           ;; Replaces the separate markdown-viewer popup.
           (let
             [provider
              (nth @items @selected)

              body-top
              (+ content-top card-rows card-gap)

              body-h
              (max 1 (- content-h card-rows card-gap))

              raw
              (str (vis/provider-status-md provider
                                           (get @statuses (:id provider))
                                           (get @limits (:id provider))))

              lines
              (mapv status-md->line (str/split-lines raw))

              total-l
              (count lines)

              maxw
              (max 0 (- inner-w 2))

              sc
              (p/clamp (long @status-scroll) 0 (max 0 (- total-l body-h)))]

             (reset! status-scroll sc)
             (draw-provider-card! g
                                  left
                                  content-top
                                  inner-w
                                  @selected
                                  false
                                  provider
                                  (get @statuses (:id provider))
                                  (get @limits (:id provider)))
             (doseq [i (range body-h)]
               (let
                 [li (+ sc (long i))
                  row (+ (long body-top) (long i))]

                 (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) row inner-w 1)
                 (when (< li total-l)
                   (let
                     [ln (nth lines li)
                      shown (subs ln 0 (min (count ln) maxw))]

                     (p/put-str! g (+ left 2) row shown))))))
           (= @mode :models)
           (let
             [ms
              @model-items

              mtotal
              (long (count ms))

              _
              (swap! model-sel #(p/clamp % 0 (max 0 (dec mtotal))))

              _
              (swap! model-scroll #(card-window-start @model-sel % content-h mtotal))

              mvis
              (card-visible-count content-h)

              mscrollable?
              (> mtotal mvis)

              mci
              (if mscrollable? (max 1 (dec inner-w)) inner-w)

              pid
              (:id (nth @items @selected))]

             (if (zero? mtotal)
               (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                   (p/draw-centered! g
                                     (inc left)
                                     (+ content-top (quot content-h 2))
                                     inner-w
                                     "No models. Press A to add."))
               (doseq [idx (range @model-scroll (min mtotal (+ (long @model-scroll) mvis)))]
                 (let
                   [idx (long idx)
                    card-y (+ content-top (card-start-row (- idx (long @model-scroll))))
                    model (nth ms idx)
                    previous-name (when (pos? idx) (:name (nth ms (dec idx))))
                    next-name (when (< idx (dec mtotal)) (:name (nth ms (inc idx))))]

                   (draw-model-card! g
                                     left
                                     card-y
                                     mci
                                     idx
                                     (= idx @model-sel)
                                     (zero? idx)
                                     pid
                                     model
                                     previous-name
                                     next-name)))))
           :else (if (zero? total)
                   (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                       (p/draw-centered! g
                                         (inc left)
                                         (+ content-top (quot content-h 2))
                                         inner-w
                                         "No providers. Press A to add."))
                   ;; Draw visible cards
                   (doseq [idx (range @scroll (min total (+ (long @scroll) visible-count)))]
                     (let
                       [idx (long idx)
                        card-y (+ content-top (card-start-row (- idx (long @scroll))))]

                       (draw-provider-card! g
                                            left
                                            card-y
                                            card-inner-w
                                            idx
                                            (= idx @selected)
                                            (nth @items idx)
                                            (get @statuses (:id (nth @items idx)))
                                            (get @limits (:id (nth @items idx))))))))
         (when (not= @mode :status)
           (scrollbar/draw! g
                            {:col (+ left inner-w)
                             :top content-top
                             :track-h content-h
                             :total-h (if (= @mode :models) (count @model-items) total)
                             :inner-h (card-visible-count content-h)
                             :scroll (if (= @mode :models) @model-scroll @scroll)}))
         ;; Bottom-anchored magit-style transients painted OVER the card list —
         ;; the provider stays visible above, actions/confirm live at the base.
         (cond (and (= @mode :actions) (pos? total))
               (let
                 [provider
                  (nth @items @selected)

                  actions
                  (provider-action-items provider (get @statuses (:id provider)))

                  n
                  (count actions)

                  last-body
                  (+ content-top content-h -1)

                  body-top
                  (max (+ content-top 2) (- (inc last-body) n))

                  title-row
                  (dec body-top)

                  sep-row
                  (dec title-row)]

                 (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) sep-row inner-w 1)
                 (p/set-colors! g t/dialog-border t/dialog-bg)
                 (p/draw-separator! g left (+ left inner-w 1) sep-row)
                 (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) title-row inner-w 1)
                 (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                 (p/styled g
                           [p/BOLD]
                           (p/put-str! g
                                       (+ left 2)
                                       title-row
                                       (str (vis/display-label (:id provider)) " — actions")))
                 (doseq [[i action] (map-indexed vector actions)]
                   (let
                     [row (+ (long body-top) (long i))
                      sel? (= (long i) (long @action-sel))
                      keytxt (str (:key action))
                      kx (+ left 2 p/SELECTION_WIDTH)
                      lx (+ kx (p/display-width keytxt) 2)]

                     (p/set-colors! g t/dialog-fg t/dialog-bg)
                     (p/fill-rect! g (inc left) row inner-w 1)
                     (p/draw-selection-marker! g (inc left) row sel? t/dialog-hint-key)
                     (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                     (p/put-str! g kx row keytxt)
                     (p/set-colors! g (if sel? t/dialog-fg t/dialog-hint) t/dialog-bg)
                     (if sel?
                       (p/styled g [p/BOLD] (p/put-str! g lx row (:label action)))
                       (p/put-str! g lx row (:label action))))))
               (= @mode :confirm)
               (let
                 [prompt
                  (:prompt @pending)

                  last-body
                  (+ content-top content-h -1)

                  title-row
                  last-body

                  sep-row
                  (dec title-row)]

                 (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) sep-row inner-w 1)
                 (p/set-colors! g t/dialog-border t/dialog-bg)
                 (p/draw-separator! g left (+ left inner-w 1) sep-row)
                 (p/set-colors! g t/dialog-fg t/dialog-bg)
                 (p/fill-rect! g (inc left) title-row inner-w 1)
                 (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                 (p/styled g [p/BOLD] (p/put-str! g (+ left 2) title-row (str prompt)))))
         (dlg/draw-hint-bar! g
                             left
                             hint-row
                             inner-w
                             (case @mode
                               :actions
                               [["↑/↓" "move"] ["Enter" "run"] ["key" "pick"] ["Esc" "back"]]

                               :status
                               [["↑/↓" "scroll"] ["Esc" "back"]]

                               :confirm
                               [["y" "confirm"] ["n/Esc" "cancel"]]

                               :models
                               [["↑/↓" "move"] ["^P/^N" "reorder"] ["R" "primary"] ["A" "add"]
                                ["D" "del"] ["Esc" "back"]]

                               [["↑/↓" "move"] ["^P/^N" "reorder"] ["A" "add"] ["D" "del"]
                                ["Enter" "actions"] ["Esc" "done"]]))
         (.setCursorPosition screen (p/cursor-pos 0 0))
         (.refresh screen Screen$RefreshType/DELTA)
         (let
           [key (if (provider-diagnostics-loading? @statuses @limits)
                  (some-> (.pollInput screen)
                          dlg/normalize-modal-key)
                  (dlg/read-modal-key! screen))]
           (if (nil? key)
             (do (Thread/sleep 100) (recur))
             (cond
               (= @mode :status)
               (if (instance? MouseAction key)
                 (let
                   [^MouseAction ma key
                    at (.getActionType ma)]

                   (cond (= at MouseActionType/SCROLL_UP)
                         (do (swap! status-scroll #(max 0 (dec (long %)))) (recur))
                         (= at MouseActionType/SCROLL_DOWN)
                         (do (swap! status-scroll #(inc (long %))) (recur))
                         :else (recur)))
                 (let [ktype (.getKeyType ^com.googlecode.lanterna.input.KeyStroke key)]
                   (cond (= ktype KeyType/Escape) (do (reset! mode :actions) (recur))
                         (= ktype KeyType/ArrowUp) (do (swap! status-scroll #(max 0 (dec (long %))))
                                                       (recur))
                         (= ktype KeyType/ArrowDown) (do (swap! status-scroll #(inc (long %)))
                                                         (recur))
                         :else (recur))))
               (= @mode :confirm)
               (if (instance? MouseAction key)
                 (recur)
                 (let
                   [ktype (.getKeyType ^com.googlecode.lanterna.input.KeyStroke key)
                    c (when (= ktype KeyType/Character)
                        (Character/toLowerCase (.getCharacter
                                                 ^com.googlecode.lanterna.input.KeyStroke key)))]

                   (cond (or (= ktype KeyType/Enter) (= c \y)) (do (when-let [run (:run @pending)]
                                                                     (run))
                                                                   (reset! pending nil)
                                                                   (reset! mode :list)
                                                                   (recur))
                         (or (= ktype KeyType/Escape) (= c \n))
                         (do (reset! pending nil) (reset! mode :list) (recur))
                         :else (recur))))
               (= @mode :actions)
               (if (instance? MouseAction key)
                 (recur)
                 (let
                   [ktype (.getKeyType ^com.googlecode.lanterna.input.KeyStroke key)
                    provider (nth @items @selected)
                    actions (provider-action-items provider (get @statuses (:id provider)))
                    n (count actions)
                    run-action!
                    (fn [action]
                      (case (:id action)
                        :models
                        (do (reset! model-items (->> (:models provider)
                                                     (keep vis/->svar-model)
                                                     vec))
                            (reset! model-sel 0)
                            (reset! model-scroll 0)
                            (reset! mode :models))

                        :authenticate
                        (do (when-let
                              [updated (authenticate-provider! screen provider (:force? action))]
                              (swap! items assoc @selected updated))
                            (reset! mode :list))

                        :status
                        (do (reset! status-scroll 0) (reset! mode :status))

                        :logout
                        (do (reset! pending {:prompt (str "Log out of "
                                                          (vis/display-label (:id provider))
                                                          "?  y / n")
                                             :run
                                             (fn []
                                               (when (perform-logout! provider)
                                                 (swap! items remove-provider-by-id (:id provider))
                                                 (swap! statuses dissoc (:id provider))
                                                 (swap! limits dissoc (:id provider))
                                                 (swap! selected
                                                   #(p/clamp % 0 (max 0 (dec (count @items)))))))})
                            (reset! mode :confirm))

                        (reset! mode :list))
                      (when-let [provider* (get @items @selected)]
                        (refresh-provider-diagnostics! provider* statuses limits)))]

                   (cond (= ktype KeyType/Escape) (do (reset! mode :list) (recur))
                         (= ktype KeyType/ArrowUp)
                         (do (swap! action-sel #(p/clamp (dec (long %)) 0 (max 0 (dec n)))) (recur))
                         (= ktype KeyType/ArrowDown)
                         (do (swap! action-sel #(p/clamp (inc (long %)) 0 (max 0 (dec n)))) (recur))
                         (= ktype KeyType/Enter)
                         (do (run-action! (nth actions
                                               (p/clamp (long @action-sel) 0 (max 0 (dec n)))))
                             (recur))
                         (= ktype KeyType/Character)
                         (let
                           [c (Character/toLowerCase
                                (.getCharacter ^com.googlecode.lanterna.input.KeyStroke key))]
                           (if-let [action (some #(when (= c (:key %)) %) actions)]
                             (do (run-action! action) (recur))
                             (recur)))
                         :else (recur))))
               (= @mode :models)
               (if (instance? MouseAction key)
                 (let
                   [^MouseAction ma key
                    at (.getActionType ma)]

                   (cond (= at MouseActionType/SCROLL_UP)
                         (do (swap! model-sel #(p/clamp (dec (long %))
                                                        0
                                                        (max 0 (dec (count @model-items)))))
                             (recur))
                         (= at MouseActionType/SCROLL_DOWN)
                         (do (swap! model-sel #(p/clamp (inc (long %))
                                                        0
                                                        (max 0 (dec (count @model-items)))))
                             (recur))
                         :else (recur)))
                 (let
                   [ktype (.getKeyType ^com.googlecode.lanterna.input.KeyStroke key)
                    ms @model-items
                    n (long (count ms))
                    provider (nth @items @selected)]

                   (cond
                     (= ktype KeyType/Escape) (do (swap! items assoc
                                                    @selected
                                                    (assoc (nth @items @selected)
                                                      :models (vec @model-items)))
                                                  (reset! mode :list)
                                                  (recur))
                     (= ktype KeyType/ArrowUp)
                     (if (input/reorder-modifier? key)
                       (do (when (pos? (long @model-sel))
                             (swap! model-items swap-items @model-sel (dec (long @model-sel)))
                             (swap! model-sel dec))
                           (recur))
                       (do (swap! model-sel #(p/clamp (dec (long %)) 0 (max 0 (dec n)))) (recur)))
                     (= ktype KeyType/ArrowDown)
                     (if (input/reorder-modifier? key)
                       (do (when (< (long @model-sel) (dec n))
                             (swap! model-items swap-items @model-sel (inc (long @model-sel)))
                             (swap! model-sel inc))
                           (recur))
                       (do (swap! model-sel #(p/clamp (inc (long %)) 0 (max 0 (dec n)))) (recur)))
                     (= ktype KeyType/Character)
                     (let
                       [c (Character/toLowerCase (.getCharacter
                                                   ^com.googlecode.lanterna.input.KeyStroke key))
                        ctrl (.isCtrlDown ^com.googlecode.lanterna.input.KeyStroke key)]

                       (cond (and ctrl (= c keymap/picker-reorder-up))
                             (do (when (pos? (long @model-sel))
                                   (swap! model-items swap-items @model-sel (dec (long @model-sel)))
                                   (swap! model-sel dec))
                                 (recur))
                             (and ctrl (= c keymap/picker-reorder-down))
                             (do (when (< (long @model-sel) (dec n))
                                   (swap! model-items swap-items @model-sel (inc (long @model-sel)))
                                   (swap! model-sel inc))
                                 (recur))
                             (= c \r) (do (when (pos? n)
                                            (swap! model-items move-model-to-front @model-sel)
                                            (reset! model-sel 0))
                                          (recur))
                             (= c \d) (do (when (pos? n)
                                            (let [sel (long @model-sel)]
                                              (swap! model-items #(vec (concat (subvec % 0 sel)
                                                                               (subvec %
                                                                                       (inc sel)))))
                                              (swap! model-sel
                                                #(p/clamp % 0 (max 0 (dec (count @model-items)))))))
                                          (recur))
                             (= c \a) (do (when-let
                                            [model-name
                                             (select-model!
                                               screen
                                               provider
                                               (->> (concat (map vis/model-name @model-items)
                                                            (:default-models (vis/provider-template
                                                                               (:id provider)))
                                                            (:default-models provider))
                                                    (remove nil?)
                                                    distinct
                                                    vec))]
                                            (when-not (some #(= model-name (vis/model-name %))
                                                            @model-items)
                                              (swap! model-items conj {:name model-name})
                                              (reset! model-sel (dec (count @model-items)))))
                                          (recur))
                             :else (recur)))
                     :else (recur))))
               (instance? MouseAction key)
               (let
                 [^MouseAction ma key
                  action (.getActionType ma)
                  pos (.getPosition ma)
                  mx (.getColumn pos)
                  my (.getRow pos)
                  hit-idx (when (and (>= mx (inc left))
                                     (< mx (+ left inner-w))
                                     (>= my content-top)
                                     (< my (+ content-top content-h)))
                            (+ (long @scroll) (quot (- my content-top) (+ card-rows card-gap))))]

                 (cond (= action MouseActionType/SCROLL_UP)
                       (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur))
                       (= action MouseActionType/SCROLL_DOWN)
                       (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                       (and (= action MouseActionType/CLICK_DOWN) hit-idx (< (long hit-idx) total))
                       (do (reset! selected hit-idx) (recur))
                       :else (recur)))
               :else
               (let [ktype (.getKeyType ^com.googlecode.lanterna.input.KeyStroke key)]
                 (cond
                   (= ktype KeyType/Escape) (let
                                              [cfg (assoc (or (vis/load-config-raw) {})
                                                     "providers" (->> @items
                                                                      (map
                                                                        persisted-provider-config)
                                                                      vec))]
                                              (vis/save-config! cfg)
                                              cfg)
                   ;; ↑/↓ navigate; Ctrl+P/Ctrl+N (or Shift/Alt+↑/↓ where supported) reorder
                   (= ktype KeyType/ArrowUp)
                   (if (input/reorder-modifier? key)
                     (do (when (pos? (long @selected))
                           (swap! items swap-items @selected (dec (long @selected)))
                           (swap! selected dec))
                         (recur))
                     (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur)))
                   (= ktype KeyType/ArrowDown)
                   (if (input/reorder-modifier? key)
                     (do (when (< (long @selected) (dec total))
                           (swap! items swap-items @selected (inc (long @selected)))
                           (swap! selected inc))
                         (recur))
                     (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur)))
                   ;; Enter - open the inline actions view for the selected provider
                   (= ktype KeyType/Enter)
                   (do (when (pos? total) (reset! action-sel 0) (reset! mode :actions)) (recur))
                   (= ktype KeyType/Character)
                   (let
                     [c (Character/toLowerCase (.getCharacter
                                                 ^com.googlecode.lanterna.input.KeyStroke key))
                      ctrl (.isCtrlDown ^com.googlecode.lanterna.input.KeyStroke key)]

                     (cond
                       ;; Ctrl+P / Ctrl+N - reorder the selected provider up / down,
                       ;; the SAME Emacs prev/next-line keys used in every input
                       ;; (modified arrows are unreliable on stock macOS terminals;
                       ;; this replaces the old vim-style K/J).
                       (and ctrl (= c keymap/picker-reorder-up))
                       (do (when (pos? (long @selected))
                             (swap! items swap-items @selected (dec (long @selected)))
                             (swap! selected dec))
                           (recur))
                       (and ctrl (= c keymap/picker-reorder-down))
                       (do (when (< (long @selected) (dec total))
                             (swap! items swap-items @selected (inc (long @selected)))
                             (swap! selected inc))
                           (recur))
                       ;; A - add provider
                       (= c \a) (do (when-let [p (add-provider! screen (into #{} (map :id) @items))]
                                      (swap! items conj p)
                                      (refresh-provider-diagnostics! p statuses limits)
                                      (reset! selected (dec (count @items))))
                                    (recur))
                       ;; D - delete provider (inline confirm, no popup)
                       (= c \d)
                       (do (when (pos? total)
                             (let
                               [sel @selected
                                provider-id (:id (nth @items sel))]

                               (reset! pending
                                 {:prompt (str "Remove " (vis/display-label provider-id) "?  y / n")
                                  :run
                                  (fn []
                                    (swap! items #(vec (concat (subvec % 0 sel)
                                                               (subvec % (inc (long sel))))))
                                    (swap! statuses dissoc provider-id)
                                    (swap! limits dissoc provider-id)
                                    (swap! selected #(p/clamp % 0 (max 0 (dec (count @items))))))})
                               (reset! mode :confirm)))
                           (recur))
                       :else (recur)))
                   :else (recur)))))))))))
