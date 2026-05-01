(ns com.blockether.vis.ext.channel-tui.provider
  "TUI provider management dialogs — model picker, model manager, provider router.
   Config I/O and data helpers live in tui/config.clj.

   GitHub Copilot OAuth: a hard dep. The TUI ships with the
   `vis-provider-github-copilot` jar on its classpath; the device-flow
   fns are required directly. (The previous `dynaload` indirection has
   been removed: explicit beats clever.)"
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.provider-github-copilot :as copilot]
            [com.blockether.vis.ext.provider-openai-codex :as codex])
  (:import [com.googlecode.lanterna.input KeyType]
           [com.googlecode.lanterna.screen Screen$RefreshType TerminalScreen]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]))

;;; ── Model fetching ─────────────────────────────────────────────────────────

(def ^:private http-client
  (-> (HttpClient/newBuilder)
    (.connectTimeout (Duration/ofSeconds 10))
    (.build)))

(def ^:private non-chat-pattern
  "Regex matching model IDs that aren't chat/completion models."
  #"(?i)^(whisper|eleven|text-embedding|tts|dall-e|stable-diffusion|wav2vec|canary|speech)")

(defn- chat-model? [id]
  (not (re-find non-chat-pattern id)))

(defn- fetch-models
  "GET /models from the provider's API. Returns vec of chat model id strings or nil on failure.
    Filters out TTS, embedding, speech, and image models automatically."
  [base-url api-key]
  (try
    (let [url     (str base-url "/models")
          builder (-> (HttpRequest/newBuilder)
                    (.uri (URI. url))
                    (.timeout (Duration/ofSeconds 15))
                    (.GET))
          builder (if api-key
                    (.header builder "Authorization" (str "Bearer " api-key))
                    builder)
          request (.build builder)
          resp    (.send ^HttpClient http-client request (HttpResponse$BodyHandlers/ofString))
          parsed  (try (svar/str->data (.body resp)) (catch Throwable _ nil))
          body    (or (:value parsed) parsed)
          models  (or (:data body) [])]
      (->> models
        (map (fn [m] (or (:id m) (str m))))
        (filter chat-model?)
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
  [base-url api-key default-models show-all?]
  (let [fetched  (or (fetch-models base-url api-key) [])
        defaults (or default-models [])
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
  [^TerminalScreen screen base-url api-key default-models]
  (loop [show-all? false]
    (let [models (build-model-list base-url api-key default-models show-all?)]
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
    (select-model! screen (vis/provider-base-url provider) (:api-key provider) defaults)))

;;; ── GitHub Copilot OAuth (hard dep) ──────────────────────────────────

(defn- copilot-oauth-flow!
  "Run the GitHub Copilot OAuth device flow inside the TUI.
   Shows the user code + URL, waits for authorization, returns the API key or nil.

   Returns nil immediately when the optional vis-providers-github-copilot
   jar isn't on the classpath."
  [^TerminalScreen screen]
  (let [start-fn    copilot/start-device-flow!
        poll-fn     copilot/poll-for-token!
        exchange-fn copilot/get-copilot-token!
        detect-fn   copilot/detect-oauth-token]
      ;; Already authenticated?
    (if (detect-fn)
      (try
        (let [{:keys [token]} (exchange-fn)]
          token)
        (catch Exception _
          (dlg/confirm-dialog! screen "Copilot" "Existing token is invalid. Re-authenticate.")
          nil))
        ;; Device flow
      (try
        (let [{:keys [user-code verification-uri device-code interval expires-in]}
              (start-fn)]
            ;; Show the code to the user
          (dlg/confirm-dialog! screen "GitHub Copilot - Authenticate"
            [(str "1. Open:  " verification-uri)
             (str "2. Enter: " user-code)
             ""
             "Press Enter, then authorize in your browser."
             "Vis will wait for you."])
            ;; Poll in background, show waiting message
          (let [result (future (poll-fn device-code interval expires-in))]
              ;; Simple blocking wait with a confirm dialog
              ;; The poll runs in background; once authorized it returns
            (loop [attempt 0]
              (if (realized? result)
                (let [{:keys [token]} (exchange-fn)]
                  (dlg/confirm-dialog! screen "GitHub Copilot" "✓ Authenticated!")
                  token)
                (do
                  (Thread/sleep 2000)
                  (when (< attempt 180) ;; 6 min max
                    (recur (inc attempt))))))))
        (catch Exception e
          (dlg/confirm-dialog! screen "GitHub Copilot" (str "Auth failed: " (ex-message e)))
          nil)))))

(defn- codex-oauth-ready!
  "Run OpenAI Codex browser OAuth from the TUI when needed.

   The shared provider flow owns browser launch and token exchange.
   The TUI supplies a dialog-backed manual collector for the final
   redirect URL, so the user can finish auth without dropping back to
   a shell prompt."
  [^TerminalScreen screen]
  (let [provider  (vis/provider-by-id :openai-codex)
        detect-fn (:provider/detect-fn provider)]
    (if (and detect-fn (detect-fn))
      true
      (do
        (dlg/confirm-dialog! screen "OpenAI Codex"
          ["Vis will start the ChatGPT/Codex browser OAuth flow."
           ""
           "After browser login, copy the final redirect URL from the"
           "address bar and paste it into the next dialog."
           ""
           "Fallback if needed:"
           "  vis auth openai-codex"])
        (try
          (let [result (codex/login! (constantly nil)
                         {:originator     "vis-tui"
                          :manual-code-fn (fn [_]
                                            (dlg/text-input-dialog! screen
                                              "OpenAI Codex"
                                              "Paste the final browser URL or authorization code:"))})]
            (when (= result :ok)
              (dlg/confirm-dialog! screen "OpenAI Codex" "✓ Authenticated!"))
            true)
          (catch Exception e
            (dlg/confirm-dialog! screen "OpenAI Codex"
              [(str "Auth failed: " (ex-message e))
               ""
               "If browser auth still fails here, run:"
               "  vis auth openai-codex"])
            false))))))

(defn- add-provider!
  "Show add-provider flow. `existing-ids` is a set of already-configured :id keywords."
  [^TerminalScreen screen existing-ids]
  (let [available (vec (remove #(contains? existing-ids (:id %)) (vis/provider-presets)))]
    (if (empty? available)
      (do (dlg/confirm-dialog! screen "Add Provider" "All providers already configured.") nil)
      (when-let [preset (dlg/select-dialog! screen "Add Provider" available)]
        (let [pid        (:id preset)
              base-url   (:base-url preset)
              has-key?   (some? (:api-key preset))
              ;; OAuth providers store credentials outside config.
              oauth?     (contains? #{:github-copilot :openai-codex} pid)
              ;; Ollama needs no key
              needs-key? (not (or has-key? oauth? (= pid :ollama)))
              api-key    (cond
                           has-key?   (:api-key preset)
                           (= pid :github-copilot) (copilot-oauth-flow! screen)
                           (= pid :openai-codex)   (when (codex-oauth-ready! screen) :oauth-ready)
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
            (when-let [model (select-provider-model! screen (cond-> {:id (:id preset)
                                                                     :base-url base-url
                                                                     :default-models (:default-models preset)}
                                                              api-key (assoc :api-key api-key)))]
              (cond-> {:id (:id preset)
                       :base-url base-url
                       :models [{:name model}]}
                ;; For Copilot, leave the short-lived API token out of
                ;; the persisted config — config.clj resolves it
                ;; dynamically from the OAuth token on each request.
                (and api-key (not oauth?)) (assoc :api-key api-key)))))))))

;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, dlg/clamp, dlg/visible-window-start, dlg/clear-screen!

(defn- priority-label [idx]
  (str "(" (inc idx) ")"))

(defn- url-host
  "Extract host from URL for display. 'https://llm.blockether.com/v1' → 'llm.blockether.com'"
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

(defn- draw-provider-card!
  "Draw a 2-line provider card.
    Line 1: ① Label  url.host  ●
    Line 2:    ★ root-model  (+N models)"
  [g left row inner-w idx selected? provider]
  (let [text-w  (max 0 (- inner-w 2))
        text-x  (+ left 2)
        pri     (priority-label idx)
        host    (url-host (or (vis/provider-base-url provider) ""))
        ok?     (some? (:api-key provider))
        label   (vis/display-label (:id provider))
        models  (or (:models provider) [])
        model-count (count (or models []))
        root-name   (or (:name (first models)) "--")
        suffix      (if (<= model-count 1)
                      "(1 model)"
                      (str "(+" (dec model-count) " models)"))
        ;; Layout line 1:  "① Label" ... "host  ●"
        left-part  (str pri " " (or label "?"))
        right-part (str host "  ●")
        ;; Fill both rows with bg
        bg (if selected? t/dialog-title-bg t/dialog-bg)]
    (p/set-bg! g bg)
    (doseq [r (range card-rows)]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))

    ;; Line 1 left — priority + label (bold)
    (if selected?
      (p/set-fg! g t/dialog-title-fg)
      (p/set-fg! g t/dialog-fg))
    (p/styled g [p/BOLD]
      (p/put-str! g text-x row (dlg/ellipsize left-part (- text-w (count right-part) 1))))

    ;; Line 1 right — host (italic dimmed) + status dot
    (let [dot-col  (+ text-x text-w -1)
          host-col (- dot-col 2 (count host))]
      ;; Host
      (if selected?
        (p/set-fg! g t/dialog-title-fg)
        (p/set-fg! g t/dialog-hint))
      (p/styled g [p/ITALIC]
        (p/put-str! g (max (+ text-x (count left-part) 1) host-col) row host))
      ;; Status dot — always green/red regardless of selection
      (p/set-fg! g (if ok? t/status-ok t/status-bad))
      (p/put-str! g dot-col row "●"))

    ;; Line 2 — model (indented)
    (if selected?
      (p/set-fg! g t/dialog-title-fg)
      (p/set-fg! g t/dialog-fg))
    (p/put-str! g text-x (inc row)
      (dlg/ellipsize (str "   ★ " root-name "  " suffix) text-w))))

(defn- draw-model-card!
  "Two-line model card. Mirrors `draw-provider-card!` layout:
     Line 1: ① model-name                         ★ Primary
     Line 2:    → then {next}  /  after {previous} → then {next}  /  ...

   Line 2 spells out the **default fallback chain**: svar's default
   routing picks `(first candidates)` from this provider's `:models`
   after filtering, so list order = chain order.

   `previous-name` and `next-name` are the names of the model just before /
   after this one in the chain (nil at the ends)."
  [g left row inner-w idx selected? is-root? _provider-id model previous-name next-name]
  (let [model-name (or (:name model) (str "model-" (inc idx)))
        text-w     (max 0 (- inner-w 2))
        text-x     (+ left 2)
        pri        (priority-label idx)
        left-part  (str pri " " model-name)
        tag        (when is-root? "★ Primary")
        bg         (if selected? t/dialog-title-bg t/dialog-bg)
        ;; Build the chain breadcrumb. Use Unicode arrows so the flow
        ;; reads left-to-right at a glance: "after X → then Y".
        subtitle   (cond
                     (and (nil? previous-name) (nil? next-name))
                     "   only model -- no fallback configured"

                     (nil? previous-name)
                     (str "   → then " next-name)

                     (nil? next-name)
                     (str "   after " previous-name " -- last fallback")

                     :else
                     (str "   after " previous-name " → then " next-name))]
    ;; Background fill across both lines
    (p/set-bg! g bg)
    (doseq [r (range card-rows)]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))

    ;; Line 1 left — priority + model name (bold), trimmed to leave room
    ;; for the right-aligned tag.
    (let [reserved (if tag (+ (count tag) 1) 0)]
      (if selected? (p/set-fg! g t/dialog-title-fg) (p/set-fg! g t/dialog-fg))
      (p/styled g [p/BOLD]
        (p/put-str! g text-x row (dlg/ellipsize left-part (max 0 (- text-w reserved))))))

    ;; Line 1 right — ★ Primary tag, right-aligned. Green when not
    ;; selected, follows title-fg when the row is selected so it stays
    ;; readable on the highlight stripe.
    (when tag
      (let [tag-col (+ text-x (- text-w (count tag)))]
        (if selected? (p/set-fg! g t/dialog-title-fg) (p/set-fg! g t/status-ok))
        (p/styled g [p/BOLD]
          (p/put-str! g tag-col row tag))))

    ;; Line 2 — dimmed italic chain breadcrumb.
    (if selected? (p/set-fg! g t/dialog-title-fg) (p/set-fg! g t/dialog-hint))
    (p/styled g [p/ITALIC]
      (p/put-str! g text-x (inc row) (dlg/ellipsize subtitle text-w)))))

(defn- swap-items
  [items i j]
  (-> items
    (assoc i (nth items j))
    (assoc j (nth items i))))

(defn- move-model-to-front
  [models idx]
  (if (or (neg? idx) (>= idx (count models)) (zero? idx))
    models
    (let [m (nth models idx)]
      (vec (cons m (concat (subvec models 0 idx)
                     (subvec models (inc idx))))))))

(defn- show-model-manager!
  [^TerminalScreen screen provider]
  (let [base-url (vis/provider-base-url provider)
        api-key  (:api-key provider)
        models   (atom (->> (:models provider)
                         (keep vis/->svar-model)
                         vec))
        selected (atom 0)]
    ;; If still empty after init, prompt for a model
    (when (empty? @models)
      (if-let [model-name (select-model! screen base-url api-key
                            (:default-models (vis/provider-template (:id provider))))]
        (swap! models conj {:name model-name})
        ;; User cancelled — return nil (no changes)
        (reset! models [])))
    (when (seq @models)
      (loop []
        (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
              cols    (.getColumns size)
              rows    (.getRows size)
              g       (.newTextGraphics screen)
              total   (count @models)
              ;; Do NOT clear the whole terminal here — the chat
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
              _       (swap! selected #(dlg/clamp % 0 (max 0 (dec total))))]

          (p/set-bg! g t/dialog-bg)
          (p/fill-rect! g (inc left) content-top inner-w content-h)

          (if (zero? total)
            (do
              (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/draw-centered! g (inc left) (+ content-top (quot content-h 2)) inner-w
                "No models. Press A to add."))
            (doseq [idx (range total)]
              (let [card-y (+ content-top (card-start-row idx))
                    model  (nth @models idx)
                    previous-name (when (pos? idx)
                                    (:name (nth @models (dec idx))))
                    next-name     (when (< idx (dec total))
                                    (:name (nth @models (inc idx))))]
                (when (and (< card-y (+ content-top content-h))
                        (>= (+ card-y card-rows) content-top))
                  (draw-model-card! g left card-y inner-w idx (= idx @selected)
                    (zero? idx)
                    (:id provider)
                    model
                    previous-name
                    next-name)))))

          (dlg/draw-hint-bar! g left hint-row inner-w
            [["↑/↓" "move"] ["Alt+↑/↓" "reorder"] ["A" "add"] ["D" "del"] ["R" "primary"] ["Esc" "back"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (.refresh screen Screen$RefreshType/DELTA)

          (let [key (.readInput screen)]
            (when key
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
                                                (vis/provider-base-url provider)
                                                (:api-key provider)
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

                  :else (recur))))))))))

(defn- ensure-base-url
  [provider]
  (if (:base-url provider)
    provider
    (if-let [resolved-base-url (:base-url (vis/provider-template (:id provider)))]
      (assoc provider :base-url resolved-base-url)
      provider)))

(defn show-provider-dialog!
  "Provider manager dialog.
   Esc saves and closes, returning {:providers [...]} in priority order.
   Optional `current-config` seeds the dialog with current state."
  ([^TerminalScreen screen]
   (show-provider-dialog! screen nil))
  ([^TerminalScreen screen current-config]
   (let [seed      (or current-config (vis/load-config) {:providers []})
         items     (atom (vec (or (:providers seed) [])))
         selected  (atom 0)]
     (loop []
       (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
             cols    (.getColumns size)
             rows    (.getRows size)
             g       (.newTextGraphics screen)
             ;; Do NOT clear the whole terminal here — keep the chat
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
             _       (swap! selected #(dlg/clamp % 0 (max 0 (dec total))))]

         ;; Clear content area
         (p/set-bg! g t/dialog-bg)
         (p/fill-rect! g (inc left) content-top inner-w content-h)

         (if (zero? total)
           (do
             (p/set-colors! g t/dialog-hint t/dialog-bg)
             (p/draw-centered! g (inc left) (+ content-top (quot content-h 2)) inner-w
               "No providers. Press A to add."))
           ;; Draw visible cards
           (doseq [idx (range total)]
             (let [card-y (+ content-top (card-start-row idx))]
               (when (and (< card-y (+ content-top content-h))
                       (>= (+ card-y card-rows) content-top))
                 (draw-provider-card! g left card-y inner-w idx (= idx @selected)
                   (nth @items idx))))))

         (dlg/draw-hint-bar! g left hint-row inner-w
           [["↑/↓" "move"] ["Alt+↑/↓" "reorder"] ["A" "add"] ["D" "del"] ["Enter" "models"] ["Esc" "done"]])
         (.setCursorPosition screen (p/cursor-pos 0 0))
         (.refresh screen Screen$RefreshType/DELTA)

         (let [key (.readInput screen)]
           (when key
             (let [ktype (.getKeyType key)]
               (cond
                 (= ktype KeyType/Escape)
                 (let [cfg {:providers (->> @items
                                         (map ensure-base-url)
                                         (map vis/->svar-provider)
                                         vec)}]
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

                   ;; Enter — open model manager for selected provider
                 (= ktype KeyType/Enter)
                 (do
                   (when (pos? total)
                     (when-let [updated-models (show-model-manager! screen (nth @items @selected))]
                       (swap! items assoc @selected
                         (assoc (nth @items @selected)
                           :models (:models updated-models)))))
                   (recur))

                 (= ktype KeyType/Character)
                 (let [c (Character/toLowerCase (.getCharacter key))]
                   (cond
                      ;; A — add provider
                     (= c \a)
                     (do (when-let [p (add-provider! screen (into #{} (map :id) @items))]
                           (swap! items conj p)
                           (reset! selected (dec (count @items))))
                       (recur))

                      ;; D — delete provider
                     (= c \d)
                     (do
                       (when (and (pos? total)
                               (dlg/confirm-dialog! screen
                                 "Remove"
                                 [(str "Remove " (vis/display-label (:id (nth @items @selected))) "?")]))
                         (swap! items #(vec (concat (subvec % 0 @selected)
                                              (subvec % (inc @selected)))))
                         (swap! selected #(dlg/clamp % 0 (max 0 (dec (count @items))))))
                       (recur))

                     :else (recur)))

                 :else (recur))))))))))
