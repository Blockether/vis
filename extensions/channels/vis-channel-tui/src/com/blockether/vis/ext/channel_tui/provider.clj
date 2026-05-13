(ns com.blockether.vis.ext.channel-tui.provider
  "TUI provider management dialogs - model picker, model manager, provider router.
   Config I/O and data helpers live in tui/config.clj.

   GitHub Copilot OAuth: a hard dep. The TUI ships with the
   `vis-provider-github-copilot` jar on its classpath; the device-flow
   fns are required directly. (The previous `dynaload` indirection has
   been removed: explicit beats clever.)"
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.limits-fmt :as lfmt]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.provider-anthropic :as anthropic]
            [com.blockether.vis.ext.provider-github-copilot :as copilot]
            [com.blockether.vis.ext.provider-openai-codex :as codex]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna Symbols]
           [com.googlecode.lanterna.input KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen Screen$RefreshType TerminalScreen]
           [java.net URI]))

;;; ── Model fetching ─────────────────────────────────────────────────────────

(def ^:private non-chat-pattern
  "Regex matching model IDs that aren't chat/completion models."
  #"(?i)^(whisper|eleven|text-embedding|tts|dall-e|stable-diffusion|wav2vec|canary|speech)")

(defn- chat-model? [id]
  (not (re-find non-chat-pattern id)))

(defn- fetch-models
  "List models for a vis provider via `svar/models!`.

   Returns vec of chat model id strings, or nil on failure. Filters
   out TTS / embedding / speech / image and provider-excluded models.

   Routing through svar means the call automatically picks up
   provider-specific OAuth headers (`anthropic-version`,
   `anthropic-beta` for the Anthropic Claude subscription;
   `chatgpt-account-id` for OpenAI Codex; bare Bearer for everyone
   else). The previous raw `http/get` here only sent
   `Authorization: Bearer ...`, which silently 400'd against Anthropic
   OAuth and never showed the live model catalog (Opus 4.7,
   Sonnet 4.6, etc.).

   `provider` is a vis-shaped provider map. We coerce to svar shape
   (resolving OAuth tokens via the provider's `:provider/get-token-fn`
   when `:api-key` is absent) and ask svar."
  [provider]
  (try
    (let [provider-id  (:id provider)
          ;; vis/->svar-provider needs at least one model on the
          ;; provider for `normalize-provider` not to throw. The
          ;; concrete model doesn't matter for `/models`; use
          ;; whatever the provider already has, falling back to a
          ;; placeholder.
          probe        (cond-> provider
                         (empty? (:models provider))
                         (assoc :models [{:name "probe"}]))
          svar-provider (vis/->svar-provider probe)
          router        (svar/make-router [svar-provider])
          raw           (svar/models! router)]
      (->> raw
        (map (fn [m] (or (:id m) (:name m) (str m))))
        (filter string?)
        (filter chat-model?)
        (filter #(vis/provider-model-visible? provider-id %))
        distinct
        sort
        vec))
    (catch Exception _ nil)))

(def ^:private dated-variant-pattern
  "Matches model IDs that are dated snapshots, e.g. gpt-4o-2024-08-06, gpt-4.1-2025-04-14."
  #"-\d{4}-\d{2}-\d{2}$")

(defn- dated-variant? [id]
  (boolean (re-find dated-variant-pattern id)))

(defn- pin-default
  "Move env default model to front of list."
  [ids]
  (let [env-default (System/getenv "BLOCKETHER_LLM_DEFAULT_MODEL")]
    (if env-default
      (into (filterv #(= % env-default) ids)
        (remove #(= % env-default) ids))
      ids)))

(defn- build-model-list
  "Build the model selection list. Fetched + defaults, deduped, sorted.
    When `show-all?` is false, hides dated variants (e.g. gpt-4o-2024-08-06).
    Appends 'Show all models...' toggle when variants were hidden."
  [provider default-models show-all?]
  (let [provider-id (:id provider)
        fetched  (or (fetch-models provider) [])
        defaults (filterv #(vis/provider-model-visible? provider-id %) (or default-models []))
        all-ids  (->> (concat fetched defaults) distinct sort vec)
        pinned   (pin-default all-ids)
        ;; Filter dated variants unless show-all
        visible  (if show-all?
                   pinned
                   (filterv (complement dated-variant?) pinned))
        hidden?  (< (count visible) (count pinned))
        items    (mapv (fn [id] {:label id :id id}) visible)]
    (if (and (not show-all?) hidden?)
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
        (if (= (:id choice) :show-all)
          (recur true)
          (:id choice))))))

(defn- select-provider-model!
  [^TerminalScreen screen provider]
  (let [defaults (->> (map vis/model-name (:models provider))
                   (concat (:default-models (vis/provider-template (:id provider)))
                     (:default-models provider))
                   (remove nil?)
                   distinct
                   vec)]
    (select-model! screen provider defaults)))

(defn- default-model-configs
  [preset]
  (->> (:default-models preset)
    (keep (fn [model]
            (when-let [name (some-> model str str/trim not-empty)]
              {:name name})))
    distinct
    vec))

(defn- provider-config-with-models
  [preset models]
  (cond-> {:id (:id preset)
           :models models}
    (:base-url preset) (assoc :base-url (:base-url preset))))

;;; ── GitHub Copilot OAuth (hard dep) ──────────────────────────────────

(def ^:private github-copilot-account-types
  {:github-copilot-individual :individual
   :github-copilot-business   :business})

(defn- github-copilot-provider? [provider-id]
  (contains? github-copilot-account-types provider-id))

(defn- github-copilot-account-type [provider-id]
  (get github-copilot-account-types provider-id :individual))

(defn- copilot-auth-instructions!
  [^TerminalScreen screen verification-uri user-code]
  (loop [status nil]
    (let [size        (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
          cols        (.getColumns size)
          rows        (.getRows size)
          g           (.newTextGraphics screen)
          bounds      (dlg/draw-dialog-chrome! g cols rows "GitHub Copilot - Authenticate" 10)
          {:keys [left inner-w]} bounds
          {:keys [content-top content-h hint-row]} (dlg/dialog-layout bounds)
          text-x      (+ left 2)
          text-w      (max 1 (- inner-w 2))
          url-label   "Open this URL in your browser:"
          code-label  "Enter this code in the browser:"
          help-lines  ["After authorizing, press Enter here to continue."
                       "Click the URL to open it. Click the code to copy it."]
          url-row     (min (+ content-top 1) (+ content-top content-h -1))
          code-row    (min (+ content-top 4) (+ content-top content-h -1))
          status-row  (min (+ content-top 8) (+ content-top content-h -1))
          url-col     text-x
          code-col    text-x]
      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g (inc left) content-top inner-w content-h)

      (doseq [[idx line] (map-indexed vector
                           [url-label
                            verification-uri
                            ""
                            code-label
                            user-code
                            ""
                            (first help-lines)
                            (second help-lines)])]
        (let [row (+ content-top idx)]
          (when (< row (+ content-top content-h))
            (p/fill-rect! g (inc left) row inner-w 1)
            (cond
              (= row url-row)
              (do
                (p/set-colors! g t/link-chrome-fg t/dialog-bg)
                (p/styled g [p/BOLD]
                  (p/put-str! g url-col row (dlg/ellipsize verification-uri text-w))))

              (= row code-row)
              (do
                (p/set-colors! g t/link-chrome-fg t/dialog-bg)
                (p/styled g [p/BOLD]
                  (p/put-str! g code-col row (dlg/ellipsize user-code text-w))))

              :else
              (do
                (p/set-colors! g t/dialog-fg t/dialog-bg)
                (p/put-str! g text-x row (dlg/ellipsize line text-w)))))))

      (when status
        (p/set-colors! g t/dialog-hint-key t/dialog-bg)
        (p/put-str! g text-x status-row (dlg/ellipsize status text-w)))

      (dlg/draw-hint-bar! g left hint-row inner-w
        [["Enter" "continue"] ["Click URL" "open"] ["Click code" "copy"] ["Esc" "cancel"]])
      (.setCursorPosition screen (p/cursor-pos 0 0))
      (.refresh screen Screen$RefreshType/DELTA)

      (let [key (dlg/read-modal-key! screen)]
        (when key
          (cond
            (instance? MouseAction key)
            (let [^MouseAction ma key
                  atype (.getActionType ma)
                  pos   (.getPosition ma)
                  mx    (.getColumn pos)
                  my    (.getRow pos)
                  on-url?  (and (= atype MouseActionType/CLICK_DOWN)
                             (= my url-row)
                             (>= mx url-col)
                             (< mx (+ url-col (count verification-uri))))
                  on-code? (and (= atype MouseActionType/CLICK_DOWN)
                             (= my code-row)
                             (>= mx code-col)
                             (< mx (+ code-col (count user-code))))]
              (cond
                on-url?
                (do (opener/open! verification-uri)
                  (recur "Opened browser URL."))

                on-code?
                (do (input/clipboard-copy! user-code)
                  (recur "Copied device code to clipboard."))

                :else
                (recur status)))

            :else
            (case (.getKeyType key)
              KeyType/Enter  true
              KeyType/Escape nil
              KeyType/Character
              (case (Character/toLowerCase (.getCharacter key))
                \o (do (opener/open! verification-uri)
                     (recur "Opened browser URL."))
                \c (do (input/clipboard-copy! user-code)
                     (recur "Copied device code to clipboard."))
                (recur status))
              (recur status))))))))

(defn- copilot-oauth-flow!
  "Run the GitHub Copilot OAuth device flow inside the TUI.
   Shows the user code + URL, waits for authorization, returns the API key or nil.

   Returns nil immediately when the optional vis-providers-github-copilot
   jar isn't on the classpath."
  ([^TerminalScreen screen]
   (copilot-oauth-flow! screen :individual false))
  ([^TerminalScreen screen account-type]
   (copilot-oauth-flow! screen account-type false))
  ([^TerminalScreen screen account-type force?]
   (let [start-fn    copilot/start-device-flow!
         poll-fn     copilot/poll-for-token!
         exchange-fn copilot/get-copilot-token!
         detect-fn   copilot/detect-oauth-token
         opts        {:account-type account-type}]
      ;; Already authenticated?
     (if (and (not force?) (detect-fn))
       (try
         (let [{:keys [token]} (exchange-fn opts)]
           token)
         (catch Exception _
           (dlg/text-view-dialog! screen "Copilot" ["Existing token is invalid. Re-authenticate."])
           nil))
        ;; Device flow
       (try
         (let [{:keys [user-code verification-uri device-code interval expires-in]}
               (start-fn opts)]
           (when (copilot-auth-instructions! screen verification-uri user-code)
             (when force?
               (copilot/logout!))
            ;; Poll in background, show waiting message
             (let [result (vis/worker-future "vis-tui-copilot-oauth-poll"
                            #(poll-fn device-code interval expires-in opts))]
              ;; The poll runs in background; once authorized it returns.
               (loop [attempt 0]
                 (if (realized? result)
                   (let [_poll-result @result
                         {:keys [token]} (exchange-fn opts)]
                     ;; Success is silent: surfacing a redundant "Authenticated!" toast
                     ;; on top of the just-closed device-flow dialog confused users
                     ;; (cf. anthropic dialog feedback). Failure dialogs remain.
                     token)
                   (do
                     (Thread/sleep 2000)
                     (when (< attempt 180) ;; 6 min max
                       (recur (inc attempt)))))))))
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
  ([^TerminalScreen screen]
   (codex-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (let [provider  (vis/provider-by-id :openai-codex)
         detect-fn (:provider/detect-fn provider)]
     (if (and (not force?) detect-fn (detect-fn))
       true
       (when (dlg/confirm-dialog! screen "OpenAI Codex"
               ["Vis will start the ChatGPT/Codex browser OAuth flow."
                ""
                "After browser login, copy the final redirect URL from the"
                "address bar and paste it into the next dialog."
                ""
                "Fallback if needed:"
                "  vis providers auth openai-codex"])
         (try
           (let [_result (codex/login! (constantly nil)
                           {:originator     "vis-tui"
                            :force?         force?
                            :manual-code-fn (fn [_]
                                              (dlg/text-input-dialog! screen
                                                "OpenAI Codex"
                                                "Paste the final browser URL or authorization code:"))})]
             ;; Success is silent: parity with anthropic + copilot flows.
             true)
           (catch Exception e
             (dlg/text-view-dialog! screen "OpenAI Codex"
               [(str "Auth failed: " (ex-message e))
                ""
                "If browser auth still fails here, run:"
                "  vis providers auth openai-codex"])
             false)))))))

(defn- anthropic-oauth-ready!
  "Run Anthropic Claude subscription browser OAuth from the TUI when needed."
  ([^TerminalScreen screen]
   (anthropic-oauth-ready! screen false))
  ([^TerminalScreen screen force?]
   (let [provider  (vis/provider-by-id :anthropic-coding-plan)
         detect-fn (:provider/detect-fn provider)]
     (if (and (not force?) detect-fn (detect-fn))
       true
       (when (dlg/confirm-dialog! screen "Anthropic"
               ["Vis will start the Anthropic Claude subscription OAuth flow."
                ""
                "After browser login, copy the final redirect URL from the"
                "address bar and paste it into the next dialog."
                ""
                "Fallback if needed:"
                "  vis providers auth anthropic-coding-plan"])
         (try
           (let [_result (anthropic/login! (constantly nil)
                           {:force?         force?
                            :manual-code-fn (fn [_]
                                              (dlg/text-input-dialog! screen
                                                "Anthropic"
                                                "Paste the final browser URL or authorization code:"))})]
             true)
           (catch Exception e
             (dlg/text-view-dialog! screen "Anthropic"
               [(str "Auth failed: " (ex-message e))
                ""
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
        (let [pid        (:id preset)
              base-url   (:base-url preset)
              has-key?   (some? (:api-key preset))
              ;; OAuth providers store credentials outside config.
              oauth?     (or (github-copilot-provider? pid) (= :openai-codex pid) (= :anthropic-coding-plan pid))
              ;; Local providers need no key
              needs-key? (not (or has-key? oauth? (contains? #{:ollama :lmstudio} pid)))
              api-key    (cond
                           has-key?   (:api-key preset)
                           (github-copilot-provider? pid) (copilot-oauth-flow! screen (github-copilot-account-type pid))
                           (= pid :openai-codex)          (when (codex-oauth-ready! screen) :oauth-ready)
                           (= pid :anthropic-coding-plan) (when (anthropic-oauth-ready! screen) :oauth-ready)
                           needs-key? (let [raw (dlg/text-input-dialog! screen
                                                  (str (:label preset) " Setup")
                                                  "API Key:"
                                                  :mask \*)]
                                        (when-not (str/blank? raw) raw))
                           :else      nil)
              auth-ok?   (cond
                           has-key?   true
                           oauth?     (some? api-key)
                           needs-key? (some? api-key)
                           :else      true)]
          (when auth-ok?
            (if-let [oauth-models (when oauth? (not-empty (default-model-configs preset)))]
              (provider-config-with-models preset oauth-models)
              (when-let [model (select-provider-model! screen (cond-> {:id (:id preset)
                                                                       :base-url base-url
                                                                       :default-models (:default-models preset)}
                                                                api-key (assoc :api-key api-key)))]
                (cond-> (provider-config-with-models preset [{:name model}])
                  (and api-key (not oauth?)) (assoc :api-key api-key))))))))))

;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, dlg/clamp, dlg/visible-window-start, dlg/clear-screen!

(defn- priority-label [idx]
  (str "(" (inc idx) ")"))

(defn- url-host
  "Extract host from URL for display. 'https://llm.blockether.com/v1' -> 'llm.blockether.com'"
  [url]
  (try
    (.getHost (URI. url))
    (catch Exception _ (or url ""))))

(def ^:private card-rows 2)   ;; lines per card
(def ^:private card-gap 1)    ;; blank line between cards

(defn- card-height
  "Total rows for n provider cards including gaps."
  [n]
  (if (pos? n)
    (+ (* n card-rows) (* (dec n) card-gap))
    0))

(defn- card-start-row
  "Starting row offset for card at index i."
  [i]
  (* i (+ card-rows card-gap)))

(defn- card-visible-count
  "Number of full two-line cards visible in `content-h`, respecting the
   one-row gap between cards."
  [content-h]
  (max 1 (quot (+ (max 0 (long content-h)) card-gap)
           (+ card-rows card-gap))))

(defn- card-window-start
  [selected current-start content-h total]
  (dlg/visible-window-start selected current-start (card-visible-count content-h) total))

(defn- card-scrollbar-geometry
  [height total first-visible]
  (let [visible-count (card-visible-count height)]
    (when (and (pos? height) (> total visible-count))
      (let [thumb-h   (max 1 (int (* height (/ (double visible-count) total))))
            max-start (max 1 (- total visible-count))
            thumb-top (int (* (- height thumb-h)
                             (/ (double (dlg/clamp first-visible 0 max-start)) max-start)))]
        {:track-h height
         :thumb-h thumb-h
         :thumb-top thumb-top}))))

(defn- draw-card-scrollbar!
  [g col top height total first-visible]
  (when-let [{:keys [track-h thumb-h thumb-top]}
             (card-scrollbar-geometry height total first-visible)]
    (doseq [r (range track-h)]
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/set-char! g col (+ top r) Symbols/SINGLE_LINE_VERTICAL))
    (doseq [r (range thumb-h)]
      (p/set-colors! g t/dialog-hint-key t/dialog-bg)
      (p/set-char! g col (+ top thumb-top r) \█))))

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
  (let [text-w         (max 0 (- inner-w 2 p/SELECTION_WIDTH))
        text-x         (+ left 2 p/SELECTION_WIDTH)
        pri            (priority-label idx)
        host           (url-host (or (vis/provider-base-url provider) ""))
        loading-status? (:loading? status)
        loading-limits? (= :loading (:status limits))
        ok?            (boolean (:authenticated? status))
        label          (vis/display-label (:id provider))
        models         (or (:models provider) [])
        model-count    (count (or models []))
        root-name      (or (:name (first models)) "--")
        suffix         (if (<= model-count 1)
                         "(1 model)"
                         (str "(+" (dec model-count) " models)"))
        ;; Dynamic per-account rows (e.g. `:zai-coding-plan-5h`, `:codex-7d`)
        ;; come from `[:dynamic :limits]`; they're what the footer shows
        ;; and what the user actually cares about. Static `:rpm`/`:tpm`
        ;; are svar catalog defaults (`{:rpm 500 :tpm 2000000}`), the
        ;; same for every provider - useful as a fallback only when no
        ;; dynamic rows are reported. Sharing `lfmt/dynamic-summary`
        ;; with footer.clj keeps both surfaces in sync.
        dynamic-text   (when-not loading-limits?
                         (lfmt/dynamic-summary limits))
        limit-summary  (->> [(when loading-status?
                               "checking auth")
                             (when loading-limits?
                               "checking limits")
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
        left-part     (str pri " " (or label "?"))
        right-part    (str host "  ●")]
    ;; Selection visual: the cursor is a `> ` glyph painted in the
    ;; dialog padding column (between the dialog frame and the card
    ;; body). The card itself keeps the normal `dialog-bg` palette so
    ;; URL hint, status dot color and dim subtitle survive selection
    ;; — previously the inverse-on-`dialog-title-bg` path collapsed
    ;; all four colors onto `dialog-title-fg`.
    (p/set-bg! g t/dialog-bg)
    (doseq [r (range card-rows)]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))
    ;; `> ` glyph in the dialog padding column, anchored to line 1.
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/draw-selection-marker! g (inc left) row selected?)

    ;; Line 1 left - priority + label (bold)
    (p/set-fg! g t/dialog-fg)
    (p/styled g [p/BOLD]
      (p/put-str! g text-x row (dlg/ellipsize left-part (- text-w (count right-part) 1))))

    ;; Line 1 right - host (italic dimmed) + status dot
    (let [dot-col  (+ text-x text-w -1)
          host-col (- dot-col 2 (count host))]
      ;; Host
      (p/set-fg! g t/dialog-hint)
      (p/styled g [p/ITALIC]
        (p/put-str! g (max (+ text-x (count left-part) 1) host-col) row host))
      ;; Status dot - green/red after probe, dim while background checks run.
      (p/set-fg! g (cond
                     (or loading-status? loading-limits?) t/dialog-hint-key
                     ok?                              t/status-ok
                     :else                            t/status-bad))
      (p/put-str! g dot-col row "●"))

    ;; Line 2 - model + static limits summary
    (p/set-fg! g t/dialog-fg)
    (p/put-str! g text-x (inc row)
      (dlg/ellipsize (str "   ★ " root-name "  " suffix
                       (when (seq limit-summary)
                         (str " / " limit-summary)))
        text-w))))

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
  (let [model-name (or (:name model) (str "model-" (inc idx)))
        text-w     (max 0 (- inner-w 2 p/SELECTION_WIDTH))
        text-x     (+ left 2 p/SELECTION_WIDTH)
        pri        (priority-label idx)
        left-part  (str pri " " model-name)
        tag        (when is-root? "★ Primary")
        ;; Build the chain breadcrumb. Use Unicode arrows so the flow
        ;; reads left-to-right at a glance: "after X -> then Y".
        subtitle   (cond
                     (and (nil? previous-name) (nil? next-name))
                     "   only model -- no fallback configured"

                     (nil? previous-name)
                     (str "   -> then " next-name)

                     (nil? next-name)
                     (str "   after " previous-name " -- last fallback")

                     :else
                     (str "   after " previous-name " -> then " next-name))]
    ;; See `draw-provider-card!` for the rationale: keep the body in
    ;; the normal palette and paint a `> ` cursor glyph in the dialog
    ;; padding column instead of inverting the entire card.
    (p/set-bg! g t/dialog-bg)
    (doseq [r (range card-rows)]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/draw-selection-marker! g (inc left) row selected?)

    ;; Line 1 left - priority + model name (bold), trimmed to leave room
    ;; for the right-aligned tag.
    (let [reserved (if tag (+ (count tag) 1) 0)]
      (p/set-fg! g t/dialog-fg)
      (p/styled g [p/BOLD]
        (p/put-str! g text-x row (dlg/ellipsize left-part (max 0 (- text-w reserved))))))

    ;; Line 1 right - ★ Primary tag, right-aligned in status-ok green.
    (when tag
      (let [tag-col (+ text-x (- text-w (count tag)))]
        (p/set-fg! g t/status-ok)
        (p/styled g [p/BOLD]
          (p/put-str! g tag-col row tag))))

    ;; Line 2 - dimmed italic chain breadcrumb.
    (p/set-fg! g t/dialog-hint)
    (p/styled g [p/ITALIC]
      (p/put-str! g text-x (inc row) (dlg/ellipsize subtitle text-w)))))

(defn- swap-items
  [items i j]
  (-> items
    (assoc i (nth items j))
    (assoc j (nth items i))))

(defn- remove-provider-by-id
  [items provider-id]
  (vec (remove #(= provider-id (:id %)) items)))

(defn- move-model-to-front
  [models idx]
  (if (or (neg? idx) (>= idx (count models)) (zero? idx))
    models
    (let [m (nth models idx)]
      (vec (cons m (concat (subvec models 0 idx)
                     (subvec models (inc idx))))))))

(defn- show-model-manager!
  [^TerminalScreen screen provider]
  (let [models   (atom (->> (:models provider)
                         (keep vis/->svar-model)
                         vec))
        selected (atom 0)
        scroll   (atom 0)]
    ;; If still empty after init, prompt for a model
    (when (empty? @models)
      (if-let [model-name (select-model! screen provider
                            (:default-models (vis/provider-template (:id provider))))]
        (swap! models conj {:name model-name})
        ;; User cancelled - return nil (no changes)
        (reset! models [])))
    (when (seq @models)
      (loop []
        (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
              cols    (.getColumns size)
              rows    (.getRows size)
              g       (.newTextGraphics screen)
              total   (count @models)
              ;; Do NOT clear the whole terminal here - the chat
              ;; behind the dialog should stay visible (other modals
              ;; in `dialogs.clj` already behave this way). The dialog
              ;; chrome paints its own background + drop shadow over
              ;; whatever was underneath, which is the desired "floating
              ;; popup" look. Wiping `0 0 cols rows` to terminal-bg every
              ;; frame is what made the chat disappear behind the
              ;; provider dialogs.
              title   (str (vis/display-label (:id provider)) " Models")
              bounds  (dlg/draw-dialog-chrome! g cols rows title (card-height (max 1 total)))
              {:keys [left inner-w]} bounds
              {:keys [content-top content-h hint-row]} (dlg/dialog-layout bounds (card-height (max 1 total)))
              visible-count (card-visible-count content-h)
              scrollable? (> total visible-count)
              card-inner-w (if scrollable? (max 1 (dec inner-w)) inner-w)
              _       (swap! selected #(dlg/clamp % 0 (max 0 (dec total))))
              _       (swap! scroll #(card-window-start @selected % content-h total))]

          (p/set-bg! g t/dialog-bg)
          (p/fill-rect! g (inc left) content-top inner-w content-h)

          (if (zero? total)
            (do
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/draw-centered! g (inc left) (+ content-top (quot content-h 2)) inner-w
                "No models. Press A to add."))
            (doseq [idx (range @scroll (min total (+ @scroll visible-count)))]
              (let [card-y (+ content-top (card-start-row (- idx @scroll)))
                    model  (nth @models idx)
                    previous-name (when (pos? idx)
                                    (:name (nth @models (dec idx))))
                    next-name     (when (< idx (dec total))
                                    (:name (nth @models (inc idx))))]
                (draw-model-card! g left card-y card-inner-w idx (= idx @selected)
                  (zero? idx)
                  (:id provider)
                  model
                  previous-name
                  next-name))))
          (draw-card-scrollbar! g (+ left inner-w) content-top content-h total @scroll)

          (dlg/draw-hint-bar! g left hint-row inner-w
            [["↑/↓" "move"] ["Alt+↑/↓" "reorder"] ["A" "add"] ["D" "del"] ["R" "primary"] ["Esc" "back"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (.refresh screen Screen$RefreshType/DELTA)

          (let [key (dlg/read-modal-key! screen)]
            (when key
              (cond
                (instance? MouseAction key)
                (let [^MouseAction ma key
                      action (.getActionType ma)
                      pos    (.getPosition ma)
                      mx     (.getColumn pos)
                      my     (.getRow pos)
                      hit-idx (when (and (>= mx (inc left))
                                      (< mx (+ left inner-w))
                                      (>= my content-top)
                                      (< my (+ content-top content-h)))
                                (+ @scroll (quot (- my content-top) (+ card-rows card-gap))))]
                  (cond
                    (= action MouseActionType/SCROLL_UP)
                    (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                      (recur))

                    (= action MouseActionType/SCROLL_DOWN)
                    (do (swap! selected #(dlg/clamp (inc %) 0 (max 0 (dec total))))
                      (recur))

                    (and (= action MouseActionType/CLICK_DOWN) hit-idx (< hit-idx total))
                    (do (reset! selected hit-idx)
                      (recur))

                    :else (recur)))

                :else
                (let [ktype (.getKeyType key)]
                  (cond
                    (= ktype KeyType/Escape)
                    {:models (vec @models)}

                    (= ktype KeyType/ArrowUp)
                    (if (.isAltDown key)
                      (do (when (pos? @selected)
                            (swap! models swap-items @selected (dec @selected))
                            (swap! selected dec))
                        (recur))
                      (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                        (recur)))

                    (= ktype KeyType/ArrowDown)
                    (if (.isAltDown key)
                      (do (when (< @selected (dec total))
                            (swap! models swap-items @selected (inc @selected))
                            (swap! selected inc))
                        (recur))
                      (do (swap! selected #(dlg/clamp (inc %) 0 (max 0 (dec total))))
                        (recur)))

                    (= ktype KeyType/Character)
                    (let [c (Character/toLowerCase (.getCharacter key))]
                      (cond
                        (= c \a)
                        (do
                          (when-let [model-name (select-model! screen
                                                  provider
                                                  (->> (concat (map vis/model-name @models)
                                                         (:default-models (vis/provider-template (:id provider)))
                                                         (:default-models provider))
                                                    (remove nil?)
                                                    distinct
                                                    vec))]
                            (when-not (some #(= model-name (vis/model-name %)) @models)
                              (swap! models conj {:name model-name})
                              (reset! selected (dec (count @models)))))
                          (recur))

                        (= c \d)
                        (do
                          (when (and (pos? total)
                                  (dlg/confirm-dialog! screen "Remove Model"
                                    [(str "Remove " (:name (nth @models @selected)) "?")]))
                            (swap! models #(vec (concat (subvec % 0 @selected)
                                                  (subvec % (inc @selected)))))
                            (swap! selected #(dlg/clamp % 0 (max 0 (dec (count @models))))))
                          (recur))

                        (= c \r)
                        (do (when (pos? total)
                              (swap! models move-model-to-front @selected)
                              (reset! selected 0))
                          (recur))

                        :else (recur)))

                    :else (recur)))))))))))

(defn- ensure-base-url
  [provider]
  (if (:base-url provider)
    provider
    (if-let [resolved-base-url (:base-url (vis/provider-template (:id provider)))]
      (assoc provider :base-url resolved-base-url)
      provider)))

(defn- persisted-provider-config
  "Convert an in-memory dialog provider entry to the durable on-disk shape.
   This path saves the provider selected in the dialog; runtime credential
   resolution belongs to the router construction path."
  [provider]
  (ensure-base-url provider))

(def ^:private local-no-auth-provider-ids
  #{:ollama :lmstudio})

(defn- safe-provider-status
  [provider]
  (try
    (cond
      (:provider/status-fn provider) ((:provider/status-fn provider))
      (:provider/detect-fn provider) {:authenticated? (boolean ((:provider/detect-fn provider)))}
      :else                          nil)
    (catch Throwable e
      {:authenticated? false
       :error          (or (ex-message e) (str e))})))

(defn- configured-provider-status
  [provider]
  (let [registered (vis/provider-by-id (:id provider))]
    (cond
      (some? (:api-key provider))
      {:authenticated? true
       :source         :config
       :config-path    vis/config-path}

      registered
      (or (safe-provider-status registered)
        {:authenticated? false})

      :else
      {:authenticated? false})))

(defn- safe-provider-limits
  [provider]
  (try
    (vis/provider-limits (:id provider))
    (catch Throwable e
      {:provider-id (:id provider)
       :status      :error
       :static      {}
       :dynamic     {:limits []}
       :error       {:message (or (ex-message e) (str e))}})))

(defn- initial-provider-status
  [provider]
  (if (some? (:api-key provider))
    {:authenticated? true
     :source         :config
     :config-path    vis/config-path}
    {:authenticated? nil
     :loading?       true}))

(defn- initial-provider-limits
  [provider]
  {:provider-id (:id provider)
   :status      :loading
   :static      {}
   :dynamic     {:limits []}})

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
  (boolean
    (or (some :loading? (vals statuses))
      (some #(= :loading (:status %)) (vals limits)))))

(defn- provider-authenticated?
  ([provider]
   (boolean (:authenticated? (configured-provider-status provider))))
  ([_provider status]
   (boolean (:authenticated? status))))

(defn- status-entry-label
  [k]
  (-> (name k)
    (str/replace #"-" " ")
    (str/capitalize)))

(defn- format-status-value
  [v]
  (cond
    (keyword? v) (name v)
    :else        (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
      (when unit
        (str " " (or size 1) "/" (name unit)))
      (when resets-at-ms
        (str ", resets " (vis/format-date (java.util.Date. (long resets-at-ms))))))))

(defn- format-limit-row
  [{:keys [label scope kind unlimited? used limit remaining note window]}]
  (let [quota (cond
                unlimited?      "unlimited"
                (number? limit) (str (when (number? used) (str used "/")) limit
                                  (when (number? remaining)
                                    (str " (" remaining " left)")))
                (number? used)  (str "used " used)
                :else           nil)
        attrs (->> [(some-> scope name)
                    (some-> kind name)
                    (format-limit-window window)]
                (remove nil?))]
    (str label
      (when (seq attrs)
        (str " [" (str/join ", " attrs) "]"))
      (when quota
        (str ": " quota))
      (when note
        (str " - " note)))))

(defn- provider-status-text
  ([provider]
   (provider-status-text provider
     (configured-provider-status provider)
     (safe-provider-limits provider)))
  ([provider status limits]
   (let [status  (or status (initial-provider-status provider))
         limits  (or limits (initial-provider-limits provider))
         title   (str (vis/display-label (:id provider)) " Status")
         rows    (->> status
                   (remove (fn [[k _]] (= k :authenticated?)))
                   (sort-by (comp str key))
                   (map (fn [[k v]]
                          (str (status-entry-label k) ": " (format-status-value v)))))
         dynamic (get-in limits [:dynamic :limits])]
     (str/join "\n"
       (concat [title
                ""
                (str "Base URL: " (or (vis/provider-base-url provider) "-"))
                (str "Authenticated: " (if (:authenticated? status) "yes" "no"))]
         (when-let [e (:error status)]
           ["" (str "Error: " e)])
         (when (seq rows)
           (concat [""] rows))
         ["" "Limits"
          (str "Status: " (name (:status limits)))]
         (when-let [rpm (get-in limits [:static :rpm])]
           [(str "Catalog RPM: " rpm)])
         (when-let [tpm (get-in limits [:static :tpm])]
           [(str "Catalog TPM: " tpm)])
         (if (seq dynamic)
           (concat ["Dynamic limits:"]
             (map #(str "- " (format-limit-row %)) dynamic))
           ["Dynamic limits: none reported"])
         (when-let [note (get-in limits [:dynamic :note])]
           [(str "Note: " note)])
         (when (seq (:static limits))
           ["Catalog RPM / TPM come from the provider catalog, not live account quota usage."])
         (when-let [message (get-in limits [:error :message])]
           [(str "Limits error: " message)]))))))

(defn show-provider-status!
  ([^TerminalScreen screen provider]
   (dlg/text-viewer-dialog! screen
     (str (vis/display-label (:id provider)) " Status & Limits")
     (provider-status-text provider)))
  ([^TerminalScreen screen provider status limits]
   (dlg/text-viewer-dialog! screen
     (str (vis/display-label (:id provider)) " Status & Limits")
     (provider-status-text provider status limits))))

(defn- provider-supports-auth?
  [provider]
  (not (contains? local-no-auth-provider-ids (:id provider))))

(defn provider-action-items
  ([provider]
   (provider-action-items provider (configured-provider-status provider)))
  ([provider status]
   (let [registered     (vis/provider-by-id (:id provider))
         authenticated? (provider-authenticated? provider status)
         auth-label     (if authenticated?
                          "Re-authenticate"
                          "Authenticate")]
     (cond-> [{:id :models :label "Configure Models"}]
       (provider-supports-auth? provider)
       (conj {:id :authenticate :label auth-label :force? authenticated?})

       (or (:provider/status-fn registered)
         (:provider/detect-fn registered)
         (:api-key provider))
       (conj {:id :status :label "Show Status + Limits"})

       (or (:provider/logout-fn registered)
         (:api-key provider))
       (conj {:id :logout :label "Log Out"})))))

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
          (try
            (auth-fn #(swap! lines conj %))
            (not-empty (trim-blank-lines @lines))
            (catch Throwable e
              (not-empty
                (trim-blank-lines
                  (conj @lines "" (str "Authentication info failed: " (or (ex-message e) (str e)))))))))))))

(defn- prompt-for-api-key!
  [^TerminalScreen screen provider]
  (let [raw (dlg/text-input-dialog! screen
              (str (vis/display-label (:id provider)) " Authentication")
              "API Key:"
              :mask \*
              :body (provider-auth-prompt-body provider))]
    (cond
      (nil? raw) api-key-prompt-cancelled
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
      (let [lines (atom [])
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
            (dlg/text-viewer-dialog! screen
              (str (vis/display-label (:id provider)) " Authentication")
              (str/join "\n" (concat @lines ["" (str "Authentication failed: " (or (ex-message e) (str e)))])))
            nil)))
      (do
        (dlg/text-view-dialog! screen "Authenticate Provider"
          [(str (vis/display-label (:id provider)) " does not expose an interactive auth flow.")])
        nil))))

(defn authenticate-provider!
  ([^TerminalScreen screen provider]
   (authenticate-provider! screen provider false))
  ([^TerminalScreen screen provider force?]
   (cond
     (github-copilot-provider? (:id provider))
     (when (copilot-oauth-flow! screen (github-copilot-account-type (:id provider)) force?) provider)

     (= :openai-codex (:id provider)) (when (codex-oauth-ready! screen force?) provider)
     (= :anthropic-coding-plan (:id provider)) (when (anthropic-oauth-ready! screen force?) provider)
     (= :ollama (:id provider)) nil
     (= :lmstudio (:id provider)) nil
     :else
     (let [prompted (prompt-for-api-key! screen provider)]
       (cond
         (= api-key-prompt-cancelled prompted) nil
         prompted prompted
         :else (run-generic-provider-auth! screen provider))))))

(defn logout-provider!
  [^TerminalScreen screen provider]
  (let [provider-id (:id provider)
        registered  (vis/provider-by-id provider-id)]
    (when (dlg/confirm-dialog! screen
            (str (vis/display-label provider-id) " Authentication")
            [(str "Log out of " (vis/display-label provider-id) "?")])
      (when-let [logout-fn (:provider/logout-fn registered)]
        (logout-fn))
      (vis/remove-config-provider! provider-id :tui-provider-logout)
      (dlg/text-view-dialog! screen
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
              :provider    provider
              :label       (str (:provider/label provider)
                             " / "
                             (if (:authenticated? status)
                               "authenticated"
                               "not authenticated"))})))
    (sort-by :label)
    vec))

(defn show-provider-auth-dialog!
  [^TerminalScreen screen]
  (when-let [item (dlg/select-dialog! screen "Authenticate Provider" (auth-provider-items))]
    (let [provider (or (:provider item)
                     (vis/provider-by-id (:provider-id item)))]
      (cond
        (github-copilot-provider? (:provider/id provider))
        (boolean (copilot-oauth-flow! screen (github-copilot-account-type (:provider/id provider))))

        (= :openai-codex (:provider/id provider))
        (boolean (codex-oauth-ready! screen false))

        (= :anthropic-coding-plan (:provider/id provider))
        (boolean (anthropic-oauth-ready! screen false))

        :else
        (if-let [auth-fn (:provider/auth-fn provider)]
          (let [lines (atom [])
                print! #(swap! lines conj %)]
            (try
              (let [result (auth-fn print!)]
                ;; Same silent-success rule as run-generic-provider-auth!.
                ;; Lines surface only when auth-fn signals the user must act.
                (when-not (contains? auth-fn-success-results result)
                  (when-let [collected (seq @lines)]
                    (dlg/text-viewer-dialog! screen
                      (str (:provider/label provider) " Authentication")
                      (str/join "\n" collected))))
                result)
              (catch Throwable e
                (dlg/text-viewer-dialog! screen
                  (str (:provider/label provider) " Authentication")
                  (str/join "\n" (concat @lines ["" (str "Authentication failed: " (or (ex-message e) (str e)))])))
                nil)))
          nil)))))

(defn show-provider-dialog!
  "Provider manager dialog.
   Esc saves and closes, returning {:providers [...]} in priority order.
   Optional `current-config` seeds the dialog with current state."
  ([^TerminalScreen screen]
   (show-provider-dialog! screen nil))
  ([^TerminalScreen screen current-config]
   (let [seed      (or current-config (vis/load-config) {:providers []})
         items     (atom (vec (or (:providers seed) [])))
         statuses  (atom (into {}
                           (map (fn [provider]
                                  [(:id provider) (initial-provider-status provider)]))
                           @items))
         limits    (atom (into {}
                           (map (fn [provider]
                                  [(:id provider) (initial-provider-limits provider)]))
                           @items))
         selected  (atom 0)
         scroll    (atom 0)]
     (refresh-providers-diagnostics! @items statuses limits)
     (loop []
       (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
             cols    (.getColumns size)
             rows    (.getRows size)
             g       (.newTextGraphics screen)
             ;; Do NOT clear the whole terminal here - keep the chat
             ;; visible behind the dialog (see model-manager note).
             ;; Sub-dialog artifact concern is moot: every sub-modal
             ;; (`add-provider!`, `confirm-dialog!`, `select-dialog!`,
             ;; `show-model-manager!`) repaints its own chrome on every
             ;; frame and on return the parent loop’s next iteration
             ;; redraws the parent chrome on top of any leftovers.
             total   (count @items)
             bounds  (dlg/draw-dialog-chrome! g cols rows "Router" (card-height (max 1 total)))
             {:keys [left inner-w]} bounds
             {:keys [content-top content-h hint-row]} (dlg/dialog-layout bounds (card-height (max 1 total)))
             visible-count (card-visible-count content-h)
             scrollable? (> total visible-count)
             card-inner-w (if scrollable? (max 1 (dec inner-w)) inner-w)
             _       (swap! selected #(dlg/clamp % 0 (max 0 (dec total))))
             _       (swap! scroll #(card-window-start @selected % content-h total))]

         ;; Clear content area
         (p/set-bg! g t/dialog-bg)
         (p/fill-rect! g (inc left) content-top inner-w content-h)

         (if (zero? total)
           (do
             (p/set-colors! g t/dialog-hint t/dialog-bg)
             (p/draw-centered! g (inc left) (+ content-top (quot content-h 2)) inner-w
               "No providers. Press A to add."))
           ;; Draw visible cards
           (doseq [idx (range @scroll (min total (+ @scroll visible-count)))]
             (let [card-y (+ content-top (card-start-row (- idx @scroll)))]
               (draw-provider-card! g left card-y card-inner-w idx (= idx @selected)
                 (nth @items idx)
                 (get @statuses (:id (nth @items idx)))
                 (get @limits (:id (nth @items idx)))))))
         (draw-card-scrollbar! g (+ left inner-w) content-top content-h total @scroll)

         (dlg/draw-hint-bar! g left hint-row inner-w
           [["↑/↓" "move"] ["Alt+↑/↓" "reorder"] ["A" "add"] ["D" "del"] ["Enter" "actions"] ["Esc" "done"]])
         (.setCursorPosition screen (p/cursor-pos 0 0))
         (.refresh screen Screen$RefreshType/DELTA)

         (let [key (if (provider-diagnostics-loading? @statuses @limits)
                     (.pollInput screen)
                     (dlg/read-modal-key! screen))]
           (if (nil? key)
             (do
               (Thread/sleep 100)
               (recur))
             (cond
               (instance? MouseAction key)
               (let [^MouseAction ma key
                     action (.getActionType ma)
                     pos    (.getPosition ma)
                     mx     (.getColumn pos)
                     my     (.getRow pos)
                     hit-idx (when (and (>= mx (inc left))
                                     (< mx (+ left inner-w))
                                     (>= my content-top)
                                     (< my (+ content-top content-h)))
                               (+ @scroll (quot (- my content-top) (+ card-rows card-gap))))]
                 (cond
                   (= action MouseActionType/SCROLL_UP)
                   (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                     (recur))

                   (= action MouseActionType/SCROLL_DOWN)
                   (do (swap! selected #(dlg/clamp (inc %) 0 (max 0 (dec total))))
                     (recur))

                   (and (= action MouseActionType/CLICK_DOWN) hit-idx (< hit-idx total))
                   (do (reset! selected hit-idx)
                     (recur))

                   :else (recur)))

               :else
               (let [ktype (.getKeyType key)]
                 (cond
                   (= ktype KeyType/Escape)
                   (let [cfg (assoc (or (vis/load-config-raw) {})
                               :providers (->> @items
                                            (map persisted-provider-config)
                                            vec))]
                     (vis/save-config! cfg)
                     cfg)

                   ;; ↑/↓ navigate, Alt+↑/↓ reorder
                   (= ktype KeyType/ArrowUp)
                   (if (.isAltDown key)
                     (do (when (pos? @selected)
                           (swap! items swap-items @selected (dec @selected))
                           (swap! selected dec))
                       (recur))
                     (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                       (recur)))

                   (= ktype KeyType/ArrowDown)
                   (if (.isAltDown key)
                     (do (when (< @selected (dec total))
                           (swap! items swap-items @selected (inc @selected))
                           (swap! selected inc))
                       (recur))
                     (do (swap! selected #(dlg/clamp (inc %) 0 (max 0 (dec total))))
                       (recur)))

                   ;; Enter - open action menu for selected provider
                   (= ktype KeyType/Enter)
                   (do
                     (when (pos? total)
                       (let [provider (nth @items @selected)]
                         (when-let [action (dlg/select-dialog! screen
                                             (str (vis/display-label (:id provider)) " Actions")
                                             (provider-action-items provider
                                               (get @statuses (:id provider))))]
                           (case (:id action)
                             :models
                             (when-let [updated-models (show-model-manager! screen provider)]
                               (swap! items assoc @selected
                                 (assoc provider :models (:models updated-models))))

                             :authenticate
                             (when-let [updated (authenticate-provider! screen provider (:force? action))]
                               (swap! items assoc @selected updated))

                             :status
                             (show-provider-status! screen provider
                               (get @statuses (:id provider))
                               (get @limits (:id provider)))

                             :logout
                             (when (logout-provider! screen provider)
                               (swap! items remove-provider-by-id (:id provider))
                               (swap! statuses dissoc (:id provider))
                               (swap! limits dissoc (:id provider))
                               (swap! selected #(dlg/clamp % 0 (max 0 (dec (count @items))))))

                             nil)
                           (when-let [provider* (get @items @selected)]
                             (refresh-provider-diagnostics! provider* statuses limits)))))
                     (recur))

                   (= ktype KeyType/Character)
                   (let [c (Character/toLowerCase (.getCharacter key))]
                     (cond
                      ;; A - add provider
                       (= c \a)
                       (do (when-let [p (add-provider! screen (into #{} (map :id) @items))]
                             (swap! items conj p)
                             (refresh-provider-diagnostics! p statuses limits)
                             (reset! selected (dec (count @items))))
                         (recur))

                      ;; D - delete provider
                       (= c \d)
                       (do
                         (when (and (pos? total)
                                 (dlg/confirm-dialog! screen
                                   "Remove"
                                   [(str "Remove " (vis/display-label (:id (nth @items @selected))) "?")]))
                           (let [provider-id (:id (nth @items @selected))]
                             (swap! items #(vec (concat (subvec % 0 @selected)
                                                  (subvec % (inc @selected)))))
                             (swap! statuses dissoc provider-id)
                             (swap! limits dissoc provider-id)
                             (swap! selected #(dlg/clamp % 0 (max 0 (dec (count @items)))))))
                         (recur))

                       :else (recur)))

                   :else (recur)))))))))))
