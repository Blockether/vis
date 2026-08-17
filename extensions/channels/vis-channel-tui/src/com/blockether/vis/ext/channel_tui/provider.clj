(ns com.blockether.vis.ext.channel-tui.provider
  "TUI provider management dialogs - model picker, model manager, provider router.
   Config I/O and data helpers live in tui/config.clj.

   The channel-neutral brain — status probing, limits, live model
   catalogs, presets, persistence shapes — lives in
   `com.blockether.vis.internal.providers` (exposed through `vis.core`)
   and can be SHARED across channels. This namespace owns only the
   lanterna interaction layer.

   ALL provider OAuth is driven ENTIRELY through the gateway —
   Anthropic + Codex over browser/PKCE, GitHub Copilot over device code —
   via `/v1/providers/:id/auth/{start,complete,poll,cancel}`. Teardown is ONE
   verb — `DELETE /v1/providers/:id` — because the daemon runs the extension's own
   logout there and drops the config entry in the same step, exactly as the
   companion's Remove does.
   The TUI therefore needs NO provider extension on its own classpath, holds
   no credential secret at any moment, and behaves identically when attached
   to a gateway on another machine."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.limits-fmt :as lfmt]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.input KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen Screen$RefreshType TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Model list (core service + the TUI's 'Show all' affordance) ────────────

(defn- same-id?
  [a b]
  (= (some-> a
             name)
     (some-> b
             name)))

(defn- move-matches-first
  "Stable partition used only for presentation; non-matches keep their order."
  [pred xs]
  (let [{matches true others false} (group-by (comp boolean pred) xs)]
    (into (vec matches) others)))

(defn- default-first-providers
  [providers provider-id]
  (if provider-id (move-matches-first #(same-id? (:id %) provider-id) providers) (vec providers)))

(defn- build-model-list
  "Build the model selection list from the gateway's LIVE catalog.
   Preferred/default models are presented first. Appends the 'Show all models...'
   toggle when dated variants were hidden."
  [provider preferred-models show-all?]
  (let
    [{:keys [models hidden-count]}
     (vis/gateway-provider-model-options (:id provider) show-all?)

     preferred
     (into #{} (map #(if (map? %) (vis/model-name %) (str %))) preferred-models)

     items
     (->> models
          (mapv (fn [id]
                  {:label id :id id}))
          (move-matches-first #(contains? preferred (:id %))))]

    (if (and (not show-all?) (pos? (long hidden-count)))
      (conj items {:label "Show all models..." :id :show-all})
      items)))



(def ^:private default-model-configs vis/provider-default-model-configs)

(def ^:private provider-config-with-models vis/provider-config-with-models)

;;; ── Provider OAuth — every flow runs through the gateway ─────────────

(def ^:private github-copilot-account-types
  {:github-copilot-individual :individual
   :github-copilot-business :business
   :github-copilot-enterprise :enterprise})

(defn- github-copilot-provider? [provider-id] (contains? github-copilot-account-types provider-id))

(defn- gateway-authenticated?
  "Ask the DAEMON whether `provider-id` already holds usable credentials.

   The TUI must not read the auth file itself: the gateway may be on another
   machine, and it is the one process that owns credential resolution."
  [provider-id]
  (try (boolean (get (vis/gateway-provider-status provider-id) "is_authenticated"))
       (catch Exception _ false)))

(def ^:private device-wait-poll-ms 200)

(def ^:private device-wait-timeout-ms (* 6 60 1000))

(def ^:private device-auth-cancelled ::device-auth-cancelled)

(defn- cancel-device-poll!
  [result]
  (when (instance? java.util.concurrent.Future result)
    (.cancel ^java.util.concurrent.Future result true)))

(defn- draw-device-waiting!
  [^TerminalScreen screen label ^long started-at-ms]
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
     (dlg/draw-dialog-chrome! g cols rows (str label " - Waiting") 8)

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
     ["Waiting for authorization..." "" "Finish login in the browser."
      "This dialog closes when vis confirms authorization." "" (str "Elapsed: " elapsed-s "s")]]

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

(defn- wait-for-device-auth!
  [^TerminalScreen screen label result]
  (let
    [started-at-ms
     (System/currentTimeMillis)

     deadline-ms
     (+ started-at-ms (long device-wait-timeout-ms))]

    (loop []

      (cond (realized? result) @result
            (>= (System/currentTimeMillis) deadline-ms)
            (do (cancel-device-poll! result)
                (when screen
                  (dlg/text-view-dialog! screen
                                         label
                                         ["Timed out waiting for authorization." ""
                                          "Restart auth when ready."]))
                device-auth-cancelled)
            :else (do (when screen (draw-device-waiting! screen label started-at-ms))
                      (if (and screen
                               (when-let [key (.pollInput screen)]
                                 (dlg/modal-escape-key? key)))
                        (do (cancel-device-poll! result) device-auth-cancelled)
                        (do (Thread/sleep (long device-wait-poll-ms)) (recur))))))))

(defn- device-auth-instructions!
  [^TerminalScreen screen label verification-uri user-code]
  (loop [status nil]
    (let
      [size (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
       cols (.getColumns size)
       rows (.getRows size)
       g (.newTextGraphics screen)
       bounds (dlg/draw-dialog-chrome! g cols rows (str label " - Authenticate") 10)
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

(defn- gateway-device-login!
  "Run one DEVICE-code OAuth flow for `provider-id` THROUGH THE GATEWAY.

   `POST auth/start` mints the flow daemon-side and returns only what the user
   must SEE (verification URI + user code); the device code, the token exchange
   and the credential file all stay in the daemon. This leg shows the code and
   asks `auth/poll` for the verdict — exactly what the phone app does, so a TUI
   attached to a REMOTE gateway signs in on the right machine.

   Returns true on success, nil on cancel or failure (dialog already shown)."
  ([^TerminalScreen screen provider-id label]
   (gateway-device-login! screen provider-id label false))
  ([^TerminalScreen screen provider-id label force?]
   (if (and (not force?) (gateway-authenticated? provider-id))
     true
     (try
       (let
         [flow
          (vis/gateway-provider-auth-start! provider-id)

          flow-id
          (get flow "flow_id")

          uri
          (or (get flow "verification_uri") (get flow "url"))

          user-code
          (get flow "user_code")

          interval-ms
          (max 1000 (long (or (get flow "interval_ms") 5000)))]

         (cond (not (and flow-id uri user-code))
               (do (dlg/text-view-dialog! screen label ["No device code came back from vis."]) nil)
               (not (device-auth-instructions! screen label uri user-code))
               (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
               :else (let
                       [poll
                        (vis/worker-future
                          "vis-tui-device-auth-poll"
                          #(loop []

                             (let [verdict (vis/gateway-provider-auth-poll! provider-id flow-id)]
                               (if (= "pending" (get verdict "status"))
                                 (do (Thread/sleep interval-ms) (recur))
                                 verdict))))

                        verdict
                        (wait-for-device-auth! screen label poll)]

                       (cond (= device-auth-cancelled verdict)
                             (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
                             ;; Success is silent: an "Authenticated!" toast on top of the
                             ;; just-closed device dialog is the noise the user vetoed.
                             (= "ok" (get verdict "status")) true
                             :else (do (dlg/text-view-dialog! screen
                                                              label
                                                              [(str "Auth failed: "
                                                                    (or (get verdict "message")
                                                                        "authorization failed"))])
                                       nil)))))
       (catch Exception e
         (dlg/text-view-dialog! screen
                                label
                                [(str "Auth failed: " (ex-message e)) "" "Fallback if needed:"
                                 (str "  vis-agent providers auth " (name provider-id))])
         nil)))))

(defn- gateway-pkce-login!
  "Run one browser (PKCE) OAuth flow for `provider-id` THROUGH THE GATEWAY.

   `POST auth/start` mints the flow daemon-side and returns only the
   authorization URL plus an opaque flow id — the PKCE verifier never reaches
   this process. The user finishes in a browser, pastes the final redirect URL
   back, and `POST auth/complete` exchanges and persists the credentials in the
   daemon. So a TUI attached to a REMOTE gateway signs in exactly like the phone
   app does, and no channel needs the provider extension on its own classpath.

   Returns true on success, nil on cancel or failure (dialog already shown)."
  [^TerminalScreen screen provider-id label]
  (try
    (let
      [flow
       (vis/gateway-provider-auth-start! provider-id)

       flow-id
       (get flow "flow_id")

       url
       (get flow "url")]

      (if-not (and flow-id url)
        (do (dlg/text-view-dialog! screen label ["No authorization URL came back from vis."]) nil)
        (do (opener/open! url)
            (let
              [pasted
               (dlg/text-input-dialog! screen
                                       label
                                       "Paste the final browser URL or authorization code:")

               input
               (some-> pasted
                       str/trim)]

              (if (str/blank? input)
                (do (vis/gateway-provider-auth-cancel! provider-id flow-id) nil)
                (do (vis/gateway-provider-auth-complete! provider-id flow-id input)
                    ;; Success is silent: parity with the copilot flow.
                    true))))))
    (catch Exception e
      (dlg/text-view-dialog! screen
                             label
                             [(str "Auth failed: " (ex-message e)) ""
                              "If browser auth still fails here, run:"
                              (str "  vis-agent providers auth " (name provider-id))])
      nil)))

(defn- codex-oauth-ready!
  "Run OpenAI Codex browser OAuth from the TUI when needed.

   The GATEWAY owns the flow end to end (see `gateway-pkce-login!`); the TUI
   only opens the browser and collects the pasted redirect URL. With `force?`,
   start a fresh OAuth flow even when credentials already exist."
  ([^TerminalScreen screen] (codex-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (if (and (not force?) (gateway-authenticated? :openai-codex))
     true
     (when (dlg/confirm-dialog! screen
                                "OpenAI Codex"
                                ["Vis will start the ChatGPT/Codex browser OAuth flow." ""
                                 "After browser login, copy the final redirect URL from the"
                                 "address bar and paste it into the next dialog." ""
                                 "Fallback if needed:" "  vis-agent providers auth openai-codex"])
       (boolean (gateway-pkce-login! screen :openai-codex "OpenAI Codex"))))))

(defn- anthropic-oauth-ready!
  "Run Anthropic Claude subscription browser OAuth from the TUI when needed.

   Gateway-driven, exactly like Codex — see `gateway-pkce-login!`."
  ([^TerminalScreen screen] (anthropic-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (if (and (not force?) (gateway-authenticated? :anthropic-coding-plan))
     true
     (when (dlg/confirm-dialog! screen
                                "Anthropic"
                                ["Vis will start the Anthropic Claude subscription OAuth flow." ""
                                 "After browser login, copy the final redirect URL from the"
                                 "address bar and paste it into the next dialog." ""
                                 "Fallback if needed:"
                                 "  vis-agent providers auth anthropic-coding-plan"])
       (boolean (gateway-pkce-login! screen :anthropic-coding-plan "Anthropic"))))))



;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, p/clamp, dlg/visible-window-start, dlg/clear-screen!

(def ^:private url-host vis/provider-url-host)

(def ^:private ^:const card-rows 3)   ;; lines per card

(def ^:private ^:const card-gap 1)    ;; blank line between cards

(defn- card-height
  "Total rows for n provider cards including gaps."
  ^long [^long n]
  (if (pos? n) (+ (* n (long card-rows)) (* (dec n) (long card-gap))) 0))

(defn- card-start-row
  "Starting row offset for card at index i."
  ^long [^long i]
  (* i (+ (long card-rows) (long card-gap))))

(defn- card-visible-count
  "Number of full two-line cards visible in `content-h`, respecting the
   one-row gap between cards."
  ^long [^long content-h]
  (max 1 (quot (+ (max 0 content-h) (long card-gap)) (+ (long card-rows) (long card-gap)))))

(defn- card-window-start
  [^long selected ^long current-start ^long content-h ^long total]
  (dlg/visible-window-start selected current-start (card-visible-count content-h) total))

(defn- tagged?
  "True when `provider` carries the tag in `selection` — either role's
   provider/model pair. An untagged fleet has no `:provider-id`, and must never
   match a provider."
  [provider selection]
  (boolean (and (some? (:provider-id selection))
                (same-id? (:id provider) (:provider-id selection)))))

(defn- role-chip
  "Chip ink `[fg bg]` for one router-root tag.

   PRIMARY fills with the theme ACCENT (the colour the active header tab already
   claims) and FALLBACK with the palette's violet, the one hue that stays clearly
   apart from the accent in EVERY theme — the blockether accents are gold, so the
   warning amber would have collapsed onto them. The label is picked by contrast
   against the fill, and both are read on every paint, so `/theme` recolours the
   badges instead of freezing the load-time palette."
  [role]
  (let [fill (if (= role :fallback) t/link-chrome-fg t/header-active-tab-accent)]
    [(t/contrast-ink fill) fill]))

(defn- draw-runs!
  "Paint `runs` left to right from `x`, clipped to `max-w` columns; returns the
   column after the last painted cell.

   A run is `{:text … :fg … :bg … :bold? …}` and nil runs are skipped. Giving a
   run its own `:bg` is what turns a label into a FILLED chip instead of more
   body text — the reason the badges can carry colour while the rest of the card
   keeps the flat `dialog-bg` palette. A run that no longer fits is truncated and
   the runs after it are dropped."
  [g x row max-w runs]
  (let [end (+ (long x) (long max-w))]
    (reduce (fn [col run]
              (let
                [col (long col)
                 s (str (:text run))
                 room (max 0 (- end col))
                 shown (if (<= (count s) room) s (subs s 0 room))]

                (if (zero? (count shown))
                  col
                  (do (p/set-colors! g (or (:fg run) t/dialog-fg) (or (:bg run) t/dialog-bg))
                      (if (:bold? run)
                        (p/styled g [p/BOLD] (p/put-str! g col row shown))
                        (p/put-str! g col row shown))
                      (+ col (count shown))))))
            (long x)
            (remove nil? runs))))

(defn- draw-provider-card!
  "Draw a two-line provider card with the PRIMARY and FALLBACK tags highlighted.

   Both tags are painted as FILLED chips (`role-chip`): accent for PRIMARY,
   violet for FALLBACK, with each row's ◆/◇ glyph in its own chip fill. The
   Colour, not wording, is what finds the two router roots in a long list."
  [g left row inner-w _idx selected? provider status limits default-selection fallback-selection]
  ;; Reserve `p/SELECTION_WIDTH` cols at the start of the card row
  ;; for the selection gutter (`>` glyph plus breathing room).
  (let
    [left
     (long left)

     row
     (long row)

     inner-w
     (long inner-w)

     default?
     (= (some-> (:id provider)
                name)
        (some-> (:provider-id default-selection)
                name))

     ;; The two tags are mutually exclusive by construction (the daemon
     ;; refuses a fallback on the primary's provider), but paint DEFAULT
     ;; first anyway so a stale config can never show both badges.
     fallback?
     (and (not default?)
          (some? (:provider-id fallback-selection))
          (same-id? (:id provider) (:provider-id fallback-selection)))

     ;; ONE role drives both lines: glyph, badge word and chip ink.
     tag-role
     (cond default? :default
           fallback? :fallback
           :else nil)

     [tag-fg tag-bg]
     (when tag-role (role-chip tag-role))

     ;; Marker glyphs: the SAME diamond the vis welcome screen leads with, so
     ;; the badge reads as product furniture instead of a rating. Filled for the
     ;; live primary, hollow for the standby fallback — one shape, two states,
     ;; and both are narrower than the East-Asian-Wide ★ that used to sit here.
     tag-glyph
     (case tag-role
       :default
       "◆ "

       :fallback
       "◇ "

       nil)

     tag-word
     (case tag-role
       :default
       " DEFAULT "

       :fallback
       " FALLBACK "

       nil)

     text-w
     (max 0 (- inner-w 2 p/SELECTION_WIDTH))

     text-x
     (+ left 2 p/SELECTION_WIDTH)

     host
     (url-host (or (vis/provider-base-url provider) ""))

     loading-status?
     (get status "is_loading")

     loading-limits?
     (= :loading (:status limits))

     ;; A credential file can survive an expired subscription. The limits
     ;; probe is the live account verdict, so it must override that stale
     ;; credential-presence status on the card.
     limits-unauthenticated?
     (= :unauthenticated (:status limits))

     ok?
     (and (not limits-unauthenticated?) (boolean (get status "is_authenticated")))

     label
     (vis/display-label (:id provider))

     models
     (or (:models provider) [])

     model-count
     (count (or models []))

     default-model
     (:model default-selection)

     fallback-model
     (:model fallback-selection)

     tag-model
     (case tag-role
       :default
       default-model

       :fallback
       fallback-model

       nil)

     catalog-summary
     (str model-count (if (= 1 model-count) " model available" " models available"))

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

     right-part
     (str host "  ●")

     ;; Layout line 1: glyph + label + badge chip ... host/status.
     line1-w
     (max 0 (- text-w (count right-part) 1))

     ;; Only the LABEL is ellipsized: a long provider name can never eat its
     ;; own PRIMARY/FALLBACK badge off the end of the row.
     label-w
     (max 0 (- line1-w (count (str tag-glyph)) (count (str tag-word)) (if tag-role 2 0)))

     line1-runs
     (vec (concat (when tag-role [{:text tag-glyph :fg tag-bg :bold? true}])
                  [{:text (dlg/ellipsize (or label "?") label-w) :fg t/dialog-fg :bold? true}]
                  (when tag-role
                    [{:text "  "} {:text tag-word :fg tag-fg :bg tag-bg :bold? true}])))

     left-part
     (apply str (map :text line1-runs))]

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
    ;; Line 1 left - tag glyph + label (bold) + badge chip
    (draw-runs! g text-x row line1-w line1-runs)
    (p/set-colors! g t/dialog-fg t/dialog-bg)
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
    ;; routing line: the tagged model, or the catalog count. Surfacing `:error`
    ;; here is what makes a dead local provider (Ollama / LM Studio not running)
    ;; actually SAY so instead of just a silent red dot.
    ;;
    ;; Line 3 is the ACCOUNT line. The card already reserves three rows
    ;; (`card-rows`) and used to leave the third one blank while the limits were
    ;; glued behind the model with a " / " and chopped mid-word on a narrow
    ;; dialog ("… (92992 left) · Ch"). One fact per line, the dead row earns its
    ;; keep, and the tail ellipsizes instead of amputating.
    (let
      [error-text (when-not (or loading-status? loading-limits?)
                    (or (:error status)
                        (when limits-unauthenticated? (get-in limits [:dynamic :note]))
                        (get-in limits [:error :message])))]
      (if (seq error-text)
        (do (p/set-fg! g t/status-bad)
            (p/put-str! g text-x (inc row) (dlg/ellipsize (str "   ⚠ " error-text) text-w)))
        ;; The tagged model repeats the row's chip, so PRIMARY/FALLBACK reads the
        ;; same on both lines; the limits stay dim so the badge leads.
        (do (draw-runs! g
                        text-x
                        (inc row)
                        text-w
                        (if tag-role
                          [{:text "   "} {:text tag-glyph :fg tag-bg :bold? true}
                           {:text (str tag-model) :fg t/dialog-fg} {:text "  "}
                           {:text tag-word :fg tag-fg :bg tag-bg :bold? true}]
                          [{:text (str "   " catalog-summary) :fg t/dialog-fg}]))
            (when (seq limit-summary)
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g text-x (+ row 2) (dlg/ellipsize (str "   " limit-summary) text-w)))
            (p/set-colors! g t/dialog-fg t/dialog-bg))))))

;; Channel-neutral status / limits / persistence shapes — the core
;; provider service (channel-neutral). Aliased privately so
;; the dialog code below reads unchanged.
(def ^:private persisted-provider-config vis/provider-persisted-config)

(defn- save-provider-config!
  "Persist the current provider set, then return the reloaded domain config.
   Credentials are the DAEMON's (`auth/complete` writes them), so every row is
   merged ONTO its persisted entry: fields the TUI does not carry — notably
   `:api-key` — survive the write."
  [items]
  (let
    [persisted
     (into {} (map (juxt :id identity)) (vis/configured-providers))

     rows
     (mapv #(persisted-provider-config (merge (get persisted (:id %)) %)) items)

     cfg
     (assoc (or (vis/load-config-raw) {}) "providers" rows)]

    (vis/save-config! cfg)
    (vis/load-config)))

(def ^:private local-no-auth-provider-ids vis/provider-local-no-auth-ids)

(def ^:private initial-provider-status vis/provider-initial-status)

(def ^:private initial-provider-limits vis/provider-initial-limits)

(def gateway-probe-timeout-ms
  "Wall for ONE gateway provider diagnostics call, in milliseconds.

   A provider's `detect_fn`/`status_fn` is extension code and may block for as
   long as it likes; the terminal must not. Every probe runs on a worker thread
   and is abandoned here, so a wedged extension costs ONE card its verdict
   instead of freezing the key/paint loop until the gateway client's own
   request budget expires."
  6000)

(defn- probe-within
  "Run `f` on a worker future and give up after `gateway-probe-timeout-ms`,
   answering `(on-timeout message)` rather than blocking the caller further."
  [label f on-timeout]
  (let
    [fut
     (vis/worker-future label f)

     value
     (deref fut gateway-probe-timeout-ms ::timed-out)]

    (if (identical? ::timed-out value)
      (do (.cancel ^java.util.concurrent.Future fut true)
          (on-timeout (str "timed out after " gateway-probe-timeout-ms "ms")))
      value)))

(defn- gateway-provider-status-safe
  [provider]
  (probe-within "vis-tui-provider-status-probe"
                (fn []
                  (try (vis/gateway-provider-status (:id provider))
                       (catch Throwable e
                         {"is_authenticated" false "error" (or (ex-message e) (str e))})))
                (fn [message]
                  {"is_authenticated" false "error" message})))

(defn- gateway-provider-limits-safe
  [provider]
  (probe-within "vis-tui-provider-limits-probe"
                (fn []
                  (try (vis/gateway-provider-limits (:id provider))
                       (catch Throwable e
                         {:provider-id (:id provider)
                          :status :error
                          :static {}
                          :dynamic {:limits []}
                          :error {:message (or (ex-message e) (str e))}})))
                (fn [message]
                  {:provider-id (:id provider)
                   :status :error
                   :static {}
                   :dynamic {:limits []}
                   :error {:message message}})))

(defn- refresh-provider-diagnostics!
  [provider statuses limits]
  (let [pid (:id provider)]
    (swap! statuses assoc pid (initial-provider-status provider))
    (swap! limits assoc pid (initial-provider-limits provider))
    (vis/worker-future "vis-tui-provider-status"
                       #(swap! statuses assoc pid (gateway-provider-status-safe provider)))
    (vis/worker-future "vis-tui-provider-limits"
                       #(swap! limits assoc pid (gateway-provider-limits-safe provider))))
  nil)

(defn- gateway-router-diagnostics-safe
  "Every provider's status AND limits from ONE `GET /v1/router`, or nil when that
   single call fails or outruns the probe wall — the caller then falls back to
   per-provider probes rather than showing a dialog full of errors."
  []
  (probe-within "vis-tui-router-diagnostics-probe"
                (fn []
                  (try (vis/gateway-router-diagnostics) (catch Throwable _ nil)))
                (fn [_message]
                  nil)))

(defn- refresh-providers-diagnostics!
  "Fill the whole dialog from ONE gateway call.

   `GET /v1/router` already carries `status` and `limits` for every provider, so
   opening the provider manager costs one request instead of 2×N — N status
   probes and N limits probes, each with its own 6s wall. Only when that single
   read fails do we fan out per provider."
  [providers statuses limits]
  (doseq [provider providers]
    (swap! statuses assoc (:id provider) (initial-provider-status provider))
    (swap! limits assoc (:id provider) (initial-provider-limits provider)))
  (vis/worker-future
    "vis-tui-router-diagnostics"
    (fn []
      (if-let [fleet (gateway-router-diagnostics-safe)]
        (doseq [provider providers]
          (let
            [pid (:id provider)
             entry (get fleet pid)]

            (if entry
              (do (swap! statuses assoc pid (:status entry))
                  (swap! limits assoc pid (:limits entry)))
              (do (swap! statuses assoc pid (gateway-provider-status-safe provider))
                  (swap! limits assoc pid (gateway-provider-limits-safe provider))))))
        (doseq [provider providers]
          (refresh-provider-diagnostics! provider statuses limits)))))
  nil)

(defn- provider-diagnostics-loading?
  [statuses limits]
  (boolean (or (some #(get % "is_loading") (vals statuses))
               (some #(= :loading (:status %)) (vals limits)))))

(defn- provider-authenticated?
  ([provider] (boolean (get (gateway-provider-status-safe provider) "is_authenticated")))
  ([_provider status] (boolean (get status "is_authenticated"))))

(defn show-provider-status!
  "Status + limits as the RICH canonical markdown form, painted through the IR
   walker — the same report the web renders as markdown. The fallback arity
   fetches diagnostics through the gateway, never through local provider OAuth."
  ([^TerminalScreen screen provider]
   (show-provider-status! screen
                          provider
                          (gateway-provider-status-safe provider)
                          (gateway-provider-limits-safe provider)))
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
  "Actions for one provider row.

   `is-fallback` (the row already carries the FALLBACK tag) is what adds
   `:clear-fallback`; `is-default` (the row holds the PRIMARY tag) is what
   REMOVES `:fallback`. The daemon refuses a fallback naming the primary's own
   provider, so offering that action on the primary's card is a guaranteed
   rejection dialog — the web settings panel disables the same button for the
   same reason. Pass both values to the painter and the key handler or the two
   menus disagree."
  ([provider] (provider-action-items provider (gateway-provider-status-safe provider)))
  ([provider status] (provider-action-items provider status false))
  ([provider status is-fallback] (provider-action-items provider status is-fallback false))
  ([provider status is-fallback is-default]
   (let
     [registered
      (vis/provider-by-id (:id provider))

      is-authenticated
      (provider-authenticated? provider status)

      auth-label
      (if is-authenticated "Re-authenticate" "Authenticate")]

     (->
       (cond-> [{:id :default :label "Set as Default..." :key \d}]
         (not is-default)
         (conj {:id :fallback :label "Set as Fallback..." :key \f})

         is-fallback
         (conj {:id :clear-fallback :label "Clear Fallback" :key \c})

         (provider-supports-auth? provider)
         (conj {:id :authenticate :label auth-label :key \a :force? is-authenticated})

         (or (:provider/status-fn registered) (:provider/detect-fn registered) (:api-key provider))
         (conj {:id :status :label "Show Status + Limits" :key \s}))
       ;; Removal is the ONE teardown a provider has, the same verb the companion
       ;; offers: the daemon runs the extension's own logout AND drops the config
       ;; entry, so nothing survives to resurrect the row as an authenticated
       ;; preset. A separate "log out" only ever left that ghost behind — and a
       ;; fleet you can only ADD to is how a provider nobody wants stays on the
       ;; screen forever.
       (conj {:id :remove :label "Remove Provider" :key \x})))))

(def ^:private routing-action-ids
  "The actions that re-point the ROUTER. Magit groups a popup's commands by what
   they touch, and these are the ones that change where requests go."
  #{:default :fallback :clear-fallback})

(defn provider-transient-spec
  "PURE: the magit transient spec for ONE provider row's `actions` (whatever
   `provider-action-items` offered). Every action keeps the SINGLE key it already
   advertised, so the popup is driven by direct keystrokes — `d`, `f`, `a` — with
   no cursor at all, and the commands are grouped the way magit groups a popup:
   routing first, then the account verbs. A group with no surviving action is
   dropped, so a provider that cannot authenticate never shows an empty heading."
  [actions]
  (let
    [group (fn [title pred]
             (when-let
               [items (seq (into []
                                 (comp (filter pred)
                                       (map (fn [action]
                                              {:key (str (:key action))
                                               :type :action
                                               :id (:id action)
                                               :label (:label action)})))
                                 actions))]
               {:title title :items (vec items)}))]
    {:groups (into []
                   (remove nil?)
                   [(group "Routing" #(contains? routing-action-ids (:id %)))
                    (group "Account" #(not (contains? routing-action-ids (:id %))))])}))

(def ^:private model-transient-keys
  "Single-key bindings the model transient hands out, in order. Magit's own
   paging keys (`n` / `p`) are held back so a model can never shadow them."
  "abcdefghijklmoqrstuvwxyz")

(defn model-transient-page-size
  "PURE: how many models one transient page holds inside `rows` usable body rows.
   Reserves the popup's own chrome — the leading blank, the `Models` header and
   the `Commands` group carrying paging plus `Show every model` — and never asks
   for more single-key bindings than exist."
  [rows]
  (p/clamp (- (long rows) 7) 1 (count model-transient-keys)))

(defn- paged-key-groups
  "PURE: one PAGE of `items` (`{:id :label}`) as a magit group whose every row is
   bound to a single letter, plus the `Commands` group that pages it.

   Every picker in this dialog family — models, presets — is the same shape: too
   many choices for one screen, each chosen with ONE keystroke, `n` / `p` between
   pages. `:commands` are the caller's own extra commands, appended after paging."
  [{:keys [title items page page-size commands]}]
  (let
    [page-size
     (long
       (p/clamp (long (or page-size (count model-transient-keys))) 1 (count model-transient-keys)))

     items
     (vec items)

     pages
     (max 1 (quot (+ (count items) (dec page-size)) page-size))

     page
     (long (p/clamp (long page) 0 (dec pages)))

     from
     (min (count items) (* page page-size))

     window
     (subvec items from (min (count items) (+ from page-size)))

     commands
     (-> (if (> pages 1)
             [{:key "n" :type :action :id ::next-page :label "Next page"}
              {:key "p" :type :action :id ::prev-page :label "Previous page"}]
             [])
         (into commands))]

    {:groups (cond->
               [{:title (if (> pages 1) (str title "  " (inc page) "/" pages) title)
                 :items (into []
                              (map-indexed (fn [i item]
                                             (assoc item
                                               :key (str (nth model-transient-keys i))
                                               :type :action)))
                              window)}]
               (seq commands)
               (conj {:title "Commands" :items commands}))}))

(defn model-transient-spec
  "PURE: the magit transient spec for the model picker. `entries` are
   `build-model-list` rows, and each real model becomes a COMMAND bound to one
   letter — a model is chosen with a single keystroke exactly like `d` sets the
   default, never with a cursor. Models past one page are reached with magit's
   `n` / `p`, and the `:show-all` sentinel becomes `*`. `marks` is
   `{:default id :fallback id}` for the models this provider already holds, so
   the tagged pair stays findable without reading the cards."
  ([entries page marks] (model-transient-spec entries page marks (count model-transient-keys)))
  ([entries page marks page-size]
   (let
     [models
      (into [] (remove #(= :show-all (:id %))) entries)

      show-all?
      (boolean (some #(= :show-all (:id %)) entries))

      labelled
      (mapv (fn [model]
              (let
                [id
                 (:id model)

                 role
                 (cond (and (some? (:default marks)) (= id (:default marks))) "  (default)"
                       (and (some? (:fallback marks)) (= id (:fallback marks))) "  (fallback)"
                       :else "")]

                {:id id :label (str (:label model) role)}))
            models)]

     (paged-key-groups {:title "Models"
                        :items labelled
                        :page page
                        :page-size page-size
                        :commands
                        (when show-all?
                          [{:key "*" :type :action :id :show-all :label "Show every model"}])}))))



(defn- run-paged-transient!
  "Run a `paged-key-groups` band until one of its items is chosen, paging in
   place on `n` / `p`. `spec-of` builds the spec for a page number. Returns the
   chosen id, or nil when the user backed out with Esc.

   Pages differ in height, and a shorter page must leave nothing of the taller
   one above it — and nothing of the HOST's own rows blanked either. The region's
   `:restore!` snapshot (taken once, before the flow's first band painted) puts
   the rows between the two band tops back the way the host drew them. Wiping
   down from `:min-row` instead took the host's content with it: setting a
   default or fallback model from Settings blanked the settings pane behind the
   popup, frame and all."
  [^TerminalScreen screen g geom title spec-of]
  (let [geom (dlg/host-band-region screen geom)]
    (loop [page 0]
      (let [picked (:action (dlg/embed-transient! screen g geom title (spec-of page)))]
        (cond (nil? picked) nil
              (= picked ::next-page) (recur (inc page))
              (= picked ::prev-page) (recur (dec page))
              :else picked)))))

(defn- run-model-transient!
  "Magit transient model picker for `provider`: one keystroke per model, `n` / `p`
   to page a long catalog, `*` to expand the models the gateway hid. Returns the
   chosen model id, or nil when the user backed out with Esc."
  [^TerminalScreen screen g geom provider entries preferred marks page-size]
  (let
    [geom
     (dlg/host-band-region screen geom)

     title
     (str (or (:label provider) (vis/display-label (:id provider))) " — models")]

    (loop [entries entries]
      (let
        [picked (run-paged-transient! screen
                                      g
                                      geom
                                      title
                                      #(model-transient-spec entries % marks page-size))]
        (if (= picked :show-all) (recur (build-model-list provider preferred true)) picked)))))

(defn api-key-transient-spec
  "PURE: the magit transient an API-key sign-in runs. `k` reads the key INLINE on
   the dialog's hint row (echoed as `*`, and the armed value renders as dots, so
   the credential never lands on screen), `a` submits it to the gateway, Esc
   cancels. No cursor to move, no full-screen prompt: the provider's own guidance
   stays visible above the popup."
  []
  {:title "Sign in"
   :groups [{:title "Credential"
             :items [{:key "k"
                      :type :option
                      :id :api-key
                      :label "API key"
                      :prompt "API key:"
                      :mask \*
                      :secret? true}]}
            {:title "Authenticate"
             :items [{:key "a" :type :action :id :submit :label "Sign in with this key"}]}]})

(defn- read-api-key!
  "Run `api-key-transient-spec` until a non-blank key is submitted (`k`, then `a`)
   or the user backs out with Esc. Submitting nothing simply re-arms the popup.
   Returns the key, or nil on cancel."
  [^TerminalScreen screen title body]
  (loop []

    (let
      [{:keys [action options]} (dlg/transient-dialog! screen title body (api-key-transient-spec))]
      (when (= :submit action)
        (or (some-> (:api-key options)
                    str
                    str/trim
                    not-empty)
            (recur))))))

(defn- gateway-api-key-login!
  "Authenticate a plain API-key provider THROUGH THE GATEWAY.

   `POST auth/start` mints an `api-key` flow daemon-side and returns the
   provider's OWN guidance lines (`:provider/auth-prompt-fn`), so the TUI needs
   neither the provider extension nor its env-var knowledge on this classpath.
   The user types the key here and `POST auth/complete` persists it in the
   DAEMON's config — the TUI never runs a provider's `:provider/auth-fn` and
   never writes a credential itself, exactly like the OAuth flows.

   Returns the provider row WITHOUT the key on success (the daemon holds the
   credential), nil on cancel or failure (dialog already shown)."
  [^TerminalScreen screen provider]
  (let
    [pid
     (:id provider)

     label
     (str (vis/display-label pid) " Authentication")]

    (try
      (let
        [flow
         (vis/gateway-provider-auth-start! pid)

         flow-id
         (get flow "flow_id")

         body
         (not-empty (vec (get flow "instructions")))]

        (if-not flow-id
          (do (dlg/text-view-dialog! screen
                                     label
                                     [(str (vis/display-label pid)
                                           " cannot be authenticated through vis.")])
              nil)
          (let
            [raw
             (read-api-key! screen label body)

             api-key
             (some-> raw
                     str/trim
                     not-empty)]

            (if (nil? api-key)
              (do (vis/gateway-provider-auth-cancel! pid flow-id) nil)
              (do (vis/gateway-provider-auth-submit-key! pid flow-id api-key)
                  ;; The DAEMON persisted it. Hand the row back WITHOUT the key:
                  ;; a TUI attached to a REMOTE gateway must never write provider
                  ;; credentials into the config of the machine it happens to run
                  ;; on, and every later fleet write merges onto the persisted
                  ;; entry anyway.
                  (dissoc provider :api-key))))))
      (catch Exception e
        (dlg/text-view-dialog! screen
                               label
                               [(str "Authentication failed: " (or (ex-message e) (str e)))])
        nil))))

(defn authenticate-provider!
  "The ONE auth entry point for every channel action (auth dialog, provider
   manager, add-provider). EVERY kind goes through the gateway: device for
   GitHub Copilot, PKCE for Codex/Anthropic, `api-key` for everything else.
   No provider credential is ever exchanged or written in the TUI process."
  ([^TerminalScreen screen provider] (authenticate-provider! screen provider false))
  ([^TerminalScreen screen provider force?]
   (cond (vis/provider-command-minted? provider)
         ;; The credential is MINTED BY THE MACHINE: `api_key_command` runs per
         ;; request. Prompting for a key here would let a typed value silently
         ;; outrank the helper, so explain and refuse instead.
         (do (dlg/text-view-dialog! screen
                                    (str (vis/display-label (:id provider)) " Authentication")
                                    [(str (vis/display-label (:id provider))
                                          " mints its own credential.") ""
                                     "Its api_key_command helper is run for every request, so"
                                     "there is no API key to enter here. Change how the token"
                                     "is minted by editing api_key_command in the config."])
             nil)
         (github-copilot-provider? (:id provider))
         (when
           (gateway-device-login! screen (:id provider) (vis/display-label (:id provider)) force?)
           provider)
         (= :openai-codex (:id provider)) (when (codex-oauth-ready! screen force?) provider)
         (= :anthropic-coding-plan (:id provider)) (when (anthropic-oauth-ready! screen force?)
                                                     provider)
         (= :ollama (:id provider)) nil
         (= :lmstudio (:id provider)) nil
         :else (gateway-api-key-login! screen provider))))

(defn- perform-remove!
  "Fleet removal for `provider` THROUGH THE GATEWAY. No dialogs — the caller owns
   confirmation and feedback.

   The daemon owns BOTH halves of a provider: its config entry and its credential.
   Dropping the entry locally while the token stays on disk is why a removed
   OAuth provider used to come back on the next open — it re-surfaced as an
   authenticated preset. Returns nil on success, or a human-readable failure."
  [provider]
  (try (vis/gateway-provider-remove! (:id provider))
       nil
       (catch Throwable t (or (not-empty (str (ex-message t))) (str t)))))

(defn remove-provider!
  "Confirm IN THE CALLER'S BAND, then drop `provider` from the fleet through the
   gateway — config entry and credential both.

   This is the ONE teardown a provider has, the verb the companion offers under
   the same name: the daemon runs the extension's own logout before it forgets the
   entry, so nothing survives to resurrect the row as an authenticated preset. The
   question, what saying yes COSTS and any refusal all paint INSIDE the frame the
   transient was fired from — a verb reached from a band must never answer with a
   window stacked on top of it.

   Returns true when the removal ran."
  [^TerminalScreen screen g region provider]
  (let
    [label
     (vis/display-label (:id provider))

     {:keys [confirm! note!]}
     (dlg/band-questions screen g region)]

    (when (confirm! (str "Remove " label "?")
                    {:cost (str "Signs out of " label
                                " on the gateway machine and drops its entry there"
                                " — every device paired with it loses the provider.")
                     :yes-label "Yes, remove"
                     :no-label "Keep it"})
      (if-let [err (perform-remove! provider)]
        (do (note! (str label " — remove failed") err) false)
        true))))

(defn- choose-router-model!
  "Pick the model that completes a routing tag for `provider` and hand the pair to
   the daemon: `:primary` re-points the default root, `:fallback` the second one.

   The model list is its OWN transient — a magit popup picks a command, and the
   command that needs an argument opens the popup that supplies it. A daemon
   refusal (a fallback naming the primary's own provider is a 400) explains
   itself and returns `::rejected` instead of killing the caller's loop; nil
   means the user backed out."
  [^TerminalScreen screen g region provider role default-selection fallback-selection]
  (let
    [current
     (if (= role :fallback) fallback-selection default-selection)

     preferred
     (when (same-id? (:id provider) (:provider-id current)) [(:model current)])

     rows
     (max 1 (- (long (:hint-row region)) (long (:min-row region)) 1))

     choice
     (run-model-transient!
       screen
       g
       region
       provider
       (build-model-list provider preferred false)
       preferred
       {:default (when (same-id? (:id provider) (:provider-id default-selection))
                   (:model default-selection))
        :fallback (when (tagged? provider fallback-selection) (:model fallback-selection))}
       (model-transient-page-size rows))]

    (when choice
      (try (if (= role :fallback)
             (vis/gateway-set-router-fallback! (:id provider) choice)
             (vis/gateway-set-router-default! (:id provider) choice))
           (catch Exception e
             (dlg/text-view-dialog! screen
                                    (if (= role :fallback) "Fallback rejected" "Default rejected")
                                    [(or (ex-message e) (str e))])
             ::rejected)))))

(defn provider-transient!
  "Run ONE provider's magit transient inside the CALLER's frame — the same
   commands `show-provider-dialog!` offers on Enter, reachable straight from a
   Settings row so a provider needs no dialog of its own.

   The fleet, the default tag and the fallback tag are re-read from the LIVE
   config on every call, so the popup reasons about what the daemon persisted and
   not about a snapshot the caller carried in. Returns true when something
   changed and the caller should reload its inventory."
  [^TerminalScreen screen g region provider-id]
  (let
    [region
     (dlg/host-band-region screen region)

     config
     (or (vis/load-config) {:providers []})

     base
     (vec (or (:providers config) []))

     configured-ids
     (into #{} (map :id) base)

     fleet
     (into base
           (remove #(contains? configured-ids (:id %)))
           (try (vis/authenticated-preset-providers) (catch Throwable _ nil)))

     provider
     (first (filter #(same-id? (:id %) provider-id) fleet))

     default-selection
     {:provider-id (some-> (:default-provider config)
                           name
                           keyword)
      :model (:default-model config)}

     fallback-selection
     (when-let [pid (:fallback-provider config)]
       {:provider-id (keyword (name pid)) :model (:fallback-model config)})]

    (when provider
      (let
        [actions
         (provider-action-items provider
                                (gateway-provider-status-safe provider)
                                (tagged? provider fallback-selection)
                                (tagged? provider default-selection))

         picked
         (:action (dlg/embed-transient! screen
                                        g
                                        region
                                        (str (vis/display-label (:id provider)) " — actions")
                                        (provider-transient-spec actions)))

         action
         (some #(when (= picked (:id %)) %) actions)]

        (case (:id action)
          (:default :fallback)
          (let
            [selection (choose-router-model! screen
                                             g
                                             region
                                             provider
                                             (if (= (:id action) :fallback) :fallback :primary)
                                             default-selection
                                             fallback-selection)]
            (boolean (and (some? selection) (not= selection ::rejected))))

          :clear-fallback
          (try
            (vis/gateway-set-router-fallback!)
            true
            (catch Exception e
              (dlg/text-view-dialog! screen "Clear fallback failed" [(or (ex-message e) (str e))])
              false))

          :authenticate
          (boolean (authenticate-provider! screen provider (:force? action)))

          :status
          (do (show-provider-status! screen provider) false)

          :remove
          (boolean (remove-provider! screen g region provider))

          false)))))

;;; ── Add a provider — a BAND in the caller's frame, never a stacked wizard ──

(defn preset-transient-spec
  "PURE: the band that picks WHICH provider to add — one keystroke per preset,
   `n` / `p` between pages. The same shape as the model picker, because it is the
   same question asked about a different list."
  ([presets page] (preset-transient-spec presets page (count model-transient-keys)))
  ([presets page page-size]
   (paged-key-groups {:title "Providers"
                      :items (mapv (fn [preset]
                                     {:id (:id preset)
                                      :label (or (:label preset) (str (:id preset)))})
                                   presets)
                      :page page
                      :page-size page-size})))

(defn local-setup-transient-spec
  "PURE: the band that confirms a LOCAL provider's endpoint before it is added.
   `u` types the base URL on the hint row and `a` adds the provider with whatever
   the line says — LM Studio and Ollama run wherever the user hosts them."
  [label base-url]
  {:groups [{:title (str label)
             :items [{:key "u" :type :option :id :base-url :label "Base URL" :prompt "Base URL:"}]}
            {:title "Commands"
             :items [{:key "a"
                      :type :action
                      :id :add
                      :label (str "Add with "
                                  (or (not-empty (str base-url)) "the preset URL"))}]}]})

(defn- band-page-size
  "How many keyed rows one band may show inside `region` — the rows between the
   first row it may touch and the host's hint bar."
  [region]
  (model-transient-page-size (max 1
                                  (- (long (:hint-row region)) (long (or (:min-row region) 0)) 1))))

(defn- read-local-base-url!
  "Run the endpoint band. Returns the URL to use, or nil when the user backed out."
  [^TerminalScreen screen g region preset]
  (let
    [{:keys [action options]}
     (dlg/embed-transient! screen
                           g
                           region
                           (str (:label preset) " — setup")
                           (assoc (local-setup-transient-spec (:label preset) (:base-url preset))
                             :read-option (dlg/region-option-reader screen g region)))]
    (when (= :add action)
      (or (some-> (:base-url options)
                  str
                  str/trim
                  (str/replace #"/+$" "")
                  not-empty)
          (:base-url preset)))))

(defn- add-preset-provider!
  "Finish adding `preset`: endpoint (local providers), sign-in, model — each step
   a band in the SAME frame. Persists the provider and returns its config, or nil
   when the user backed out anywhere along the way."
  [^TerminalScreen screen g region preset]
  (let
    [pid
     (:id preset)

     local?
     (contains? local-no-auth-provider-ids pid)

     oauth?
     (or (github-copilot-provider? pid) (= :openai-codex pid) (= :anthropic-coding-plan pid))

     base-url
     (if local? (read-local-base-url! screen g region preset) (:base-url preset))

     preset
     (assoc preset :base-url base-url)

     ;; Sign-in is the one step that CANNOT be a band: OAuth needs a browser, a
     ;; device code, a pasted redirect URL. It runs as its own dialog and the
     ;; frame is put back exactly as it was, so the model band that follows lands
     ;; on the settings list instead of on the blank paper `open-nested!` leaves.
     auth-ok?
     (and (or (not local?) (some? base-url))
          (or local?
              (some? (:api-key preset))
              (some? (dlg/with-frame-restored! screen #(authenticate-provider! screen preset)))))

     config
     (when auth-ok?
       (if-let [oauth-models (when oauth? (not-empty (default-model-configs preset)))]
         (provider-config-with-models preset oauth-models)
         (let [preferred (vis/provider-default-model-names preset)]
           (when-let
             [model (run-model-transient! screen
                                          g
                                          region
                                          preset
                                          (build-model-list preset preferred false)
                                          preferred
                                          {}
                                          (band-page-size region))]
             (cond-> (provider-config-with-models preset [{:name model}])
               (and (string? (:api-key preset)) (not oauth?))
               (assoc :api-key (:api-key preset)))))))]

    (when config (save-provider-config! (conj (vec (vis/configured-providers)) config)) config)))

(defn add-provider-transient!
  "Add a provider as a BAND inside the caller's own frame: one keystroke picks the
   preset, a second band confirms a local endpoint, sign-in runs, a third band
   picks the model. No wizard, no stacked popups — the list behind it stays on
   screen the whole way, which is the whole point of a transient.

   Persists the provider and returns its config, or nil when nothing was added."
  ([^TerminalScreen screen g region]
   (add-provider-transient! screen g region (into #{} (map :id) (vis/configured-providers))))
  ([^TerminalScreen screen g region existing-ids]
   (let
     [existing
      (set existing-ids)

      available
      (vec (remove #(contains? existing (:id %)) (vis/provider-presets)))

      region
      (dlg/host-band-region screen region)]

     (if (empty? available)
       (do (dlg/embed-transient! screen
                                 g
                                 region
                                 "Add provider"
                                 {:groups [{:title "Nothing to add"
                                            :items [{:key "q"
                                                     :type :action
                                                     :id :done
                                                     :label
                                                     "Every provider is already configured"}]}]})
           nil)
       (when-let
         [pid (run-paged-transient! screen
                                    g
                                    region
                                    "Add provider"
                                    #(preset-transient-spec available % (band-page-size region)))]
         (add-preset-provider! screen g region (first (filter #(= pid (:id %)) available))))))))

(def ^:private provider-dialog-title "Providers")

(defn- pending-remove
  "The manager frame's inline `y / n` confirm that removes the SELECTED provider:
   the gateway drops config entry and credential, then the card leaves the list.
   One definition, because the list's `d` and a card's `Remove Provider` must not
   be able to mean two different things."
  [^TerminalScreen screen {:keys [g region items statuses limits selected]}]
  (let [provider (nth @items @selected)]
    {:prompt (str "Remove " (vis/display-label (:id provider)) "?  y / n")
     :run (fn []
            (if-let [err (perform-remove! provider)]
              ;; The refusal is a band in THIS frame as well: the card list the
              ;; removal was fired from stays behind it.
              ((:note! (dlg/band-questions screen g region))
                (str (vis/display-label (:id provider)) " — remove failed")
                err)
              (let [sel (long @selected)]
                (swap! items #(vec (concat (subvec % 0 sel) (subvec % (inc sel)))))
                (swap! statuses dissoc (:id provider))
                (swap! limits dissoc (:id provider))
                (swap! selected #(p/clamp % 0 (max 0 (dec (count @items))))))))}))

(defn show-provider-dialog!
  "Provider manager dialog.
   Esc saves and closes. Provider order has no routing semantics; choose exactly
   one default provider/model pair.

   A provider's OWN commands no longer live here: `provider-transient!` runs them
   as a magit band inside whatever frame asks, which is how Settings reaches
   them."
  ([^TerminalScreen screen] (show-provider-dialog! screen nil))
  ([^TerminalScreen screen current-config]
   (let
     [seed
      (or current-config (vis/load-config) {:providers []})

      items
      (atom (let
              [base
               (vec (or (:providers seed) []))

               configured-ids
               (into #{} (map :id) base)

               fleet
               (into base
                     (remove #(contains? configured-ids (:id %)))
                     (try (vis/authenticated-preset-providers) (catch Throwable _ nil)))]

              ;; Default-first is a presentation rule only. Provider order has no
              ;; routing semantics; the explicit provider/model pair remains canonical.
              (default-first-providers fleet (:default-provider seed))))

      default-selection
      (atom
        (let
          [provider-id
           (or (:default-provider seed) (:id (first @items)))

           provider
           (first (filter #(= (some-> (:id %)
                                      name)
                              (some-> provider-id
                                      name))
                          @items))]

          {:provider-id (some-> provider-id
                                name
                                keyword)
           :model (or (:default-model seed)
                      (some-> provider
                              :models
                              first
                              vis/model-name))}))

      ;; The FALLBACK tag: a second provider/model root, always on a
      ;; DIFFERENT provider than the primary (the daemon refuses the
      ;; primary's own). nil means the fleet carries no fallback.
      fallback-selection
      (atom (when-let [pid (:fallback-provider seed)]
              {:provider-id (keyword (name pid)) :model (:fallback-model seed)}))

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

      status-scroll
      (atom 0)

      pending
      (atom nil)]

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

          ;; Do NOT clear the whole terminal here - keep the chat visible behind
          ;; the dialog. Sub-dialogs repaint their own chrome; on return this
          ;; parent loop redraws the provider cards.
          total
          (long (count @items))

          ;; Size the box to the cards via the explicit-height chrome arity.
          ;; The default arity substitutes a tall proportional footprint and
          ;; vertically centers the cards inside it, leaving dead empty rows
          ;; after the last provider. `golden-dialog-size` floors height to
          ;; `content + chrome`, so passing the real card height fits the box
          ;; (and clamps to the terminal when there are many providers).
          ;; Floor Providers at the full default footprint so it reads as a
          ;; substantial panel (more height), not a tiny box hugging 2 cards;
          ;; a long provider list still grows past it and scrolls.
          content-rows
          (max (card-height (max 1 total)) (dlg/default-content-height rows))

          bounds
          (dlg/draw-dialog-chrome! g
                                   cols
                                   rows
                                   provider-dialog-title
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
          (swap! scroll #(card-window-start @selected % content-h total))

          ;; Everything a magit transient needs to live INSIDE this box: the
          ;; frame's own columns and hint row, plus a floor under the focused
          ;; provider's card so the popup never paints over the row it edits.
          geom
          {:left left
           :inner-w inner-w
           :hint-row hint-row
           :text-w (max 0 (- inner-w 4))
           :min-row (+ (long content-top) (long card-rows) (long card-gap))}]

         ;; Clear content area
         (p/set-bg! g t/dialog-bg)
         (p/fill-rect! g (inc left) content-top inner-w content-h)
         (cond (= @mode :status)
               ;; Inline status + limits — the selected provider card on top, its
               ;; markdown Status & Limits report as plain rows below, scrollable.
               ;; Replaces the separate markdown-viewer popup.
               (let
                 [provider
                  (nth @items @selected)

                  body-top
                  (+ (long content-top) (long card-rows) (long card-gap))

                  body-h
                  (max 1 (- (long content-h) (long card-rows) (long card-gap)))

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
                                      (get @limits (:id provider))
                                      @default-selection
                                      @fallback-selection)
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
                                                (get @limits (:id (nth @items idx)))
                                                @default-selection
                                                @fallback-selection)))))
         (when (not= @mode :status)
           (scrollbar/draw! g
                            {:col (+ left inner-w)
                             :top content-top
                             :track-h content-h
                             :total-h total
                             :inner-h (card-visible-count content-h)
                             :scroll @scroll}))
         ;; Bottom-anchored overlay painted OVER the card list — the provider
         ;; stays visible above it. Actions and models are real magit transients
         ;; (`tr/run!`) that run their OWN key loop, so the only
         ;; thing left to paint from here is the y/n confirm line.
         (when (= @mode :confirm)
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
                               :status
                               [["↑/↓" "scroll"] ["Esc" "back"]]

                               :confirm
                               [["y" "confirm"] ["n/Esc" "cancel"]]

                               [["↑/↓" "move"] ["A" "add"] ["D" "del"] ["Enter" "actions"]
                                ["Esc" "done"]]))
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
                            (+ (long @scroll)
                               (quot (- my (long content-top))
                                     (+ (long card-rows) (long card-gap)))))]

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
                   (= ktype KeyType/Escape) (save-provider-config! @items)
                   ;; Provider order is display-only; arrows only navigate.
                   (= ktype KeyType/ArrowUp)
                   (do (swap! selected #(p/clamp (dec (long %)) 0 (max 0 (dec total)))) (recur))
                   (= ktype KeyType/ArrowDown)
                   (do (swap! selected #(p/clamp (inc (long %)) 0 (max 0 (dec total)))) (recur))
                   ;; Enter — the provider's magit transient: every action fires
                   ;; from its OWN key (`d`, `f`, `a`, …) with no cursor at all,
                   ;; and picking a model is a second transient, not a menu.
                   (= ktype KeyType/Enter)
                   (do
                     (when (pos? total)
                       (let
                         [provider (nth @items @selected)
                          actions (provider-action-items provider
                                                         (get @statuses (:id provider))
                                                         (tagged? provider @fallback-selection)
                                                         (tagged? provider @default-selection))
                          ;; The card the transient is about stays painted above
                          ;; it (`:min-row` floors the popup under it), so the
                          ;; provider being configured is never off screen.
                          _ (draw-provider-card! g
                                                 left
                                                 content-top
                                                 inner-w
                                                 @selected
                                                 false
                                                 provider
                                                 (get @statuses (:id provider))
                                                 (get @limits (:id provider))
                                                 @default-selection
                                                 @fallback-selection)
                          picked (:action (dlg/embed-transient!
                                            screen
                                            g
                                            geom
                                            (str (vis/display-label (:id provider)) " — actions")
                                            (provider-transient-spec actions)))
                          action (some #(when (= picked (:id %)) %) actions)]

                         (case (:id action)
                           (:default :fallback)
                           (let
                             [role (if (= (:id action) :fallback) :fallback :primary)
                              ;; A model list needs the whole panel, so the tag
                              ;; and the argument it needs are TWO popups: this
                              ;; one reclaims the card's rows and pages the rest.
                              selection (choose-router-model! screen
                                                              g
                                                              (assoc geom :min-row content-top)
                                                              provider
                                                              role
                                                              @default-selection
                                                              @fallback-selection)]

                             (when (and (some? selection) (not= selection ::rejected))
                               (if (= role :fallback)
                                 (reset! fallback-selection selection)
                                 (do ;; The daemon drops a fallback that collides with
                                     ;; the new primary's provider — mirror that here.
                                   (when (tagged? provider @fallback-selection)
                                     (reset! fallback-selection nil))
                                   (reset! default-selection selection)
                                   (swap! items default-first-providers (:provider-id selection))
                                   (reset! selected 0)
                                   (reset! scroll 0)))))

                           :clear-fallback
                           (try (vis/gateway-set-router-fallback!)
                                (reset! fallback-selection nil)
                                (catch Exception e
                                  ;; Same contract as tagging: a daemon refusal
                                  ;; explains itself instead of killing the loop.
                                  (dlg/text-view-dialog! screen
                                                         "Clear fallback failed"
                                                         [(or (ex-message e) (str e))])))

                           :authenticate
                           (when (authenticate-provider! screen provider (:force? action))
                             ;; Auth wrote the credential DAEMON-side, so re-read
                             ;; the row from the persisted fleet instead of
                             ;; trusting what the dialog handed back — the TUI
                             ;; never holds the key itself.
                             (swap! items assoc
                               @selected
                               (or (first (filter (fn [p]
                                                    (= (:id p) (:id provider)))
                                                  (vis/configured-providers)))
                                   provider)))

                           :status
                           (do (reset! status-scroll 0) (reset! mode :status))

                           :remove
                           (do (reset! pending (pending-remove screen
                                                               {:g g
                                                                :region geom
                                                                :items items
                                                                :statuses statuses
                                                                :limits limits
                                                                :selected selected}))
                               (reset! mode :confirm))

                           nil)
                         (when-let [provider* (get @items @selected)]
                           (refresh-provider-diagnostics! provider* statuses limits))))
                     (recur))
                   (= ktype KeyType/Character)
                   (let
                     [c (Character/toLowerCase (.getCharacter
                                                 ^com.googlecode.lanterna.input.KeyStroke key))]
                     (cond
                       ;; A - add provider
                       ;; Adding is a BAND in this frame too — the preset, the
                       ;; endpoint and the model are each one keystroke over the
                       ;; card list, never a wizard stacked on top of it.
                       (= c \a) (do (when-let
                                      [p (add-provider-transient! screen
                                                                  g
                                                                  (assoc geom :min-row content-top)
                                                                  (into #{} (map :id) @items))]
                                      (swap! items conj p)
                                      (refresh-provider-diagnostics! p statuses limits)
                                      (reset! selected (dec (count @items))))
                                    (recur))
                       ;; D - remove provider (inline confirm, no popup)
                       (= c \d) (do (when (pos? total)
                                      (reset! pending (pending-remove screen
                                                                      {:g g
                                                                       :region geom
                                                                       :items items
                                                                       :statuses statuses
                                                                       :limits limits
                                                                       :selected selected}))
                                      (reset! mode :confirm))
                                    (recur))
                       :else (recur)))
                   :else (recur)))))))))))
