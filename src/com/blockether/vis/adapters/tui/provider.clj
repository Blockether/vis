(ns com.blockether.vis.adapters.tui.provider
  "TUI provider management dialogs — model picker, model manager, provider router.
   Config I/O and data helpers live in tui/config.clj."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.adapters.tui.dialogs :as dlg]
            [com.blockether.vis.adapters.tui.primitives :as p]
            [com.blockether.vis.adapters.tui.theme :as t])
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
          resp    (.send http-client request (HttpResponse$BodyHandlers/ofString))
          parsed  (svar/str->data (.body resp))
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
  (let [defaults (->> (map config/model-name (:models provider))
                   (concat (:default-models (config/provider-template (:id provider)))
                     (:default-models provider))
                   (remove nil?)
                   distinct
                   vec)]
    (select-model! screen (config/provider-base-url provider) (:api-key provider) defaults)))

(defn- add-provider!
  [^TerminalScreen screen]
  (when-let [preset (dlg/select-dialog! screen "Add Provider" (config/provider-presets))]
    (let [pid        (:id preset)
          base-url   (:base-url preset)
          has-key?   (some? (:api-key preset))
          needs-key? (not (or has-key? (= pid :ollama)))
          raw-key    (cond
                       has-key?   (:api-key preset)
                       needs-key? (dlg/text-input-dialog! screen
                                    (str (:label preset) " Setup")
                                    "API Key:"
                                    :mask \*)
                       :else nil)
          api-key    (when-not (str/blank? raw-key) raw-key)]
      (when (or (not needs-key?) api-key)
        (when-let [model (select-provider-model! screen (cond-> {:id (:id preset)
                                                                 :base-url base-url
                                                                 :default-models (:default-models preset)}
                                                          api-key (assoc :api-key api-key)))]
          (cond-> {:id (:id preset)
                   :base-url base-url
                   :models [{:name model}]}
            api-key (assoc :api-key api-key)))))))

;;; ── Reuse dialog infrastructure from dialogs.clj ───────────────────────────
;; dlg/dlg/draw-dialog-chrome!, dlg/dlg/dialog-layout, dlg/dlg/draw-hint-bar!,
;; dlg/dlg/ellipsize, dlg/clamp, dlg/visible-window-start, dlg/clear-screen!

(def ^:private circled-nums ["①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩"])

(defn- priority-label [idx]
  (if (< idx (count circled-nums))
    (nth circled-nums idx)
    (str (inc idx) ".")))

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
        host    (url-host (or (config/provider-base-url provider) ""))
        ok?     (some? (:api-key provider))
        label   (config/display-label (:id provider))
        models  (or (:models provider) [])
        model-count (count (or models []))
        root-name   (or (:name (first models)) "—")
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
  [g left row inner-w idx selected? is-root? provider-id model]
  (let [model-name   (:name model)
        text-w       (max 0 (- inner-w 2))
        text-x       (+ left 2)
        root-mark    (if is-root? "* " "  ")
        provider-line (str "  " (or (some-> provider-id name) "provider"))
        bg           (if selected? t/dialog-title-bg t/dialog-bg)]
    (p/set-bg! g bg)
    (doseq [r (range card-rows)]
      (p/fill-rect! g (inc left) (+ row r) inner-w 1))
    (if selected? (p/set-fg! g t/dialog-title-fg) (p/set-fg! g t/dialog-fg))
    (p/styled g [p/BOLD]
      (p/put-str! g text-x row
        (dlg/ellipsize (str root-mark (or model-name (str "model-" (inc idx)))) text-w)))
    (if selected? (p/set-fg! g t/dialog-title-fg) (p/set-fg! g t/dialog-hint))
    (p/put-str! g text-x (inc row) (dlg/ellipsize provider-line text-w))))

(defn- move-model-to-front
  [models idx]
  (if (or (neg? idx) (>= idx (count models)) (zero? idx))
    models
    (let [m (nth models idx)]
      (vec (cons m (concat (subvec models 0 idx)
                     (subvec models (inc idx))))))))

(defn- show-model-manager!
  [^TerminalScreen screen provider]
  (let [base-url (config/provider-base-url provider)
        api-key  (:api-key provider)
        models   (atom (->> (:models provider)
                         (keep config/->svar-model)
                         vec))
        selected (atom 0)]
    ;; If still empty after init, prompt for a model
    (when (empty? @models)
      (if-let [model-name (select-model! screen base-url api-key
                            (:default-models (config/provider-template (:id provider))))]
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
              _       (do (p/set-bg! g t/terminal-bg) (p/fill-rect! g 0 0 cols rows))
              cw      (min 84 (max 50 (- cols 12)))
              ch      (max 4 (card-height total))
              title   (str (config/display-label (:id provider)) " Models")
              bounds  (dlg/draw-dialog-chrome! g cols rows title cw ch)
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
                    model  (nth @models idx)]
                (when (and (< card-y (+ content-top content-h))
                        (>= (+ card-y card-rows) content-top))
                  (draw-model-card! g left card-y inner-w idx (= idx @selected)
                    (zero? idx)
                    (:id provider)
                    model)))))

          (dlg/draw-hint-bar! g left hint-row inner-w
            [["↑/↓" "move"] ["A" "add"] ["D" "del"] ["R" "root"] ["Esc" "back"]])
          (.setCursorPosition screen (p/cursor-pos 0 0))
          (.refresh screen Screen$RefreshType/DELTA)

          (let [key (.readInput screen)]
            (when key
              (let [ktype (.getKeyType key)]
                (cond
                  (= ktype KeyType/Escape)
                  {:models (vec @models)}

                  (= ktype KeyType/ArrowUp)
                  (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                    (recur))

                  (= ktype KeyType/ArrowDown)
                  (do (swap! selected #(dlg/clamp (inc %) 0 (max 0 (dec total))))
                    (recur))

                  (= ktype KeyType/Character)
                  (let [c (Character/toLowerCase (.getCharacter key))]
                    (cond
                      (= c \a)
                      (do
                        (when-let [model-name (select-model! screen
                                                (config/provider-base-url provider)
                                                (:api-key provider)
                                                (->> (concat (map config/model-name @models)
                                                       (:default-models (config/provider-template (:id provider)))
                                                       (:default-models provider))
                                                  (remove nil?)
                                                  distinct
                                                  vec))]
                          (when-not (some #(= model-name (config/model-name %)) @models)
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

(defn- swap-provider
  [items i j]
  (-> items
    (assoc i (nth items j))
    (assoc j (nth items i))))

(defn- ensure-base-url
  [provider]
  (if (:base-url provider)
    provider
    (if-let [resolved-base-url (:base-url (config/provider-template (:id provider)))]
      (assoc provider :base-url resolved-base-url)
      provider)))

(defn show-provider-dialog!
  "Provider manager dialog.
   Esc saves and closes, returning {:providers [...]} in priority order.
   Optional `current-config` seeds the dialog with current state."
  ([^TerminalScreen screen]
   (show-provider-dialog! screen nil))
  ([^TerminalScreen screen current-config]
   (let [seed      (or current-config (config/load-config) {:providers []})
         items     (atom (vec (or (:providers seed) [])))
         selected  (atom 0)]
     (loop []
       (let [size    (or (.doResizeIfNecessary screen) (.getTerminalSize screen))
             cols    (.getColumns size)
             rows    (.getRows size)
             g       (.newTextGraphics screen)
             ;; Clear full screen each frame — removes sub-dialog artifacts
             _       (do (p/set-bg! g t/terminal-bg) (p/fill-rect! g 0 0 cols rows))
             total   (count @items)
             cw      (min 72 (max 40 (- cols 16)))
             ch      (max 4 (card-height total))
             bounds  (dlg/draw-dialog-chrome! g cols rows "Router" cw ch)
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
                                         (map config/->svar-provider)
                                         vec)}]
                   (config/save-config! cfg)
                   cfg)

                   ;; ↑/↓ navigate, Alt+↑/↓ reorder
                 (= ktype KeyType/ArrowUp)
                 (if (.isAltDown key)
                   (do (when (pos? @selected)
                         (swap! items swap-provider @selected (dec @selected))
                         (swap! selected dec))
                     (recur))
                   (do (swap! selected #(dlg/clamp (dec %) 0 (max 0 (dec total))))
                     (recur)))

                 (= ktype KeyType/ArrowDown)
                 (if (.isAltDown key)
                   (do (when (< @selected (dec total))
                         (swap! items swap-provider @selected (inc @selected))
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
                     (do (when-let [p (add-provider! screen)]
                           (swap! items conj p)
                           (reset! selected (dec (count @items))))
                       (recur))

                      ;; D — delete provider
                     (= c \d)
                     (do
                       (when (and (pos? total)
                               (dlg/confirm-dialog! screen
                                 "Remove"
                                 [(str "Remove " (config/display-label (:id (nth @items @selected))) "?")]))
                         (swap! items #(vec (concat (subvec % 0 @selected)
                                              (subvec % (inc @selected)))))
                         (swap! selected #(dlg/clamp % 0 (max 0 (dec (count @items))))))
                       (recur))

                     :else (recur)))

                 :else (recur))))))))))
