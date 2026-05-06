(ns com.blockether.vis.internal.theme
  "Internal, channel-agnostic Vis theme data.

   Keep this namespace pure data: no Lanterna, Swing, browser, or terminal
   backend imports. Channels adapt these tokens to their own render types."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def default-theme-id
  "The built-in theme id used when config has no explicit theme."
  "vis-light")

(defn- byte? [x]
  (and (int? x) (<= 0 x 255)))

(defn rgb?
  "True when `x` is an RGB triple `[r g b]`, each channel 0..255."
  [x]
  (and (vector? x)
    (= 3 (count x))
    (every? byte? x)))

(defn- non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- theme-key? [x]
  (or (keyword? x) (non-blank-string? x)))

(defn- theme-setting-value? [x]
  (or (nil? x)
    (string? x)
    (number? x)
    (boolean? x)
    (keyword? x)
    (rgb? x)
    (and (vector? x) (every? theme-setting-value? x))
    (and (map? x)
      (every? theme-key? (keys x))
      (every? theme-setting-value? (vals x)))))

(s/def ::rgb rgb?)
(s/def ::name non-blank-string?)
(s/def ::display-name non-blank-string?)
(s/def ::mode #{:light :dark})
(s/def ::palette (s/map-of keyword? ::rgb :min-count 1))
(s/def ::fonts map?)
(s/def ::widths map?)
(s/def ::spacing map?)
(s/def ::settings (s/map-of string? theme-setting-value?))
(s/def ::theme
  (s/keys :req-un [::name ::display-name ::mode ::palette ::fonts ::widths ::spacing ::settings]))

(def common-fonts
  {:mono {:family "monospace"
          :size "inherit"
          :weight "normal"
          :style "normal"}
   :ui {:family "system-ui, sans-serif"
        :size "inherit"
        :weight "normal"
        :style "normal"}
   :heading {:family "system-ui, sans-serif"
             :size "inherit"
             :weight "700"
             :style "normal"}
   :code {:family "monospace"
          :size "inherit"
          :weight "normal"
          :style "normal"}})

(def common-widths
  {:dialog-width-ratio 0.84
   :dialog-min-width 76
   :dialog-max-width 132
   :dialog-height-ratio 0.72
   :dialog-min-height 24
   :dialog-max-height 40
   :dialog-chrome-w 4
   :dialog-chrome-h 6
   :settings-option-indent 2
   :input-min-width 20
   :chat-min-width 20})

(def common-spacing
  {:pad-x 1
   :padding "0px"
   :gap "1ch"
   :bubble-padding-x "1ch"
   :bubble-padding-y "0"})

(defn- theme-settings
  [theme-name mode]
  {"THEME_NAME" theme-name
   "MODE" (name mode)
   "PADDING" (:padding common-spacing)
   "GAP" (:gap common-spacing)
   "BUBBLE_PADDING_X" (:bubble-padding-x common-spacing)
   "BUBBLE_PADDING_Y" (:bubble-padding-y common-spacing)
   "FONT_FAMILY" (get-in common-fonts [:mono :family])
   "UI_FONT_FAMILY" (get-in common-fonts [:ui :family])
   "CODE_FONT_FAMILY" (get-in common-fonts [:code :family])
   "DIALOG_MIN_WIDTH" (:dialog-min-width common-widths)
   "DIALOG_MAX_WIDTH" (:dialog-max-width common-widths)})

(def light-palette
  {:terminal-bg [255 255 255]
   :text-fg [30 30 30]
   :header-fg [30 30 30]
   :header-hover-fg [70 70 70]
   :box-bg [255 255 255]
   :box-fg [30 30 30]
   :border-fg [80 80 80]
   :dialog-bg [248 248 248]
   :dialog-fg [30 30 30]
   :dialog-title-fg [255 255 255]
   :dialog-title-bg [60 60 60]
   :dialog-border [120 120 120]
   :dialog-shadow [200 200 200]
   :dialog-hint [120 120 120]
   :dialog-hint-key [50 50 50]
   :input-field-bg [255 255 252]
   :user-bubble-bg [255 255 255]
   :user-bubble-fg [30 30 30]
   :user-role-fg [130 90 0]
   :turn-separator-bg [248 244 235]
   :turn-separator-fg [190 150 40]
   :ai-bubble-bg [255 255 255]
   :ai-bubble-fg [30 30 30]
   :ai-role-fg [80 160 80]
   :status-ok [40 160 60]
   :status-bad [220 50 50]
   :warning-bg [255 245 180]
   :warning-fg [80 60 0]
   :warning-border [190 150 40]
   :cancelled-bg [240 240 240]
   :cancelled-fg [110 110 110]
   :code-block-bg [240 243 248]
   :code-ok-bg [232 248 232]
   :code-err-bg [253 235 235]
   :code-block-fg [30 30 30]
   :code-success-fg [40 160 60]
   :code-error-fg [220 50 50]
   :code-duration-fg [130 130 130]
   :code-result-fg [70 70 70]
   :code-error-result-fg [180 40 40]
   :code-syntax-special-fg [120 70 170]
   :code-syntax-keyword-fg [25 110 120]
   :code-syntax-string-fg [150 80 40]
   :code-syntax-number-fg [30 90 180]
   :code-syntax-comment-fg [120 120 120]
   :code-border-fg [90 95 110]
   :tool-color-read [30 120 190]
   :tool-color-search [140 80 190]
   :tool-color-preview [105 115 130]
   :tool-color-edit [180 120 20]
   :tool-color-create [40 150 75]
   :tool-color-delete [210 45 45]
   :tool-color-move [210 105 25]
   :tool-color-shell [95 105 120]
   :tool-color-meta [20 135 135]
   :stdout-bg [247 244 238]
   :stdout-fg [80 80 80]
   :stdout-label-fg [155 155 155]
   :stdout-sep-fg [210 205 195]
   :iteration-header-fg [170 170 170]
   :iteration-header-bg [244 244 244]
   :answer-sep-fg [190 190 190]
   :answer-sep-bg [250 250 250]
   :answer-bg [255 255 255]
   :answer-fg [25 25 25]
   :md-h1-fg [150 100 0]
   :md-h2-fg [125 80 0]
   :md-h3-fg [100 65 0]
   :confidence-fg [140 140 140]
   :md-summary-bg [226 214 250]
   :md-summary-fg [55 30 120]
   :th-md-summary-bg [218 214 236]
   :th-md-summary-fg [70 55 125]
   :proof-summary-bg [218 244 232]
   :proof-summary-fg [20 105 75]
   :th-proof-summary-bg [206 232 224]
   :th-proof-summary-fg [45 105 90]
   :link-chrome-fg [30 90 200]
   :link-chrome-arrow-fg [130 130 130]
   :link-chrome-url-fg [90 110 140]
   :link-chrome-hover-bg [232 240 252]
   :link-chrome-hover-fg [10 50 160]
   :link-chrome-blocked-fg [170 170 170]
   :footer-fg [60 60 60]
   :footer-fg-muted [140 140 140]
   :footer-fg-strong [30 30 30]
   :footer-spinner-fg [80 160 80]
   :footer-warning-fg [180 110 0]
   :footer-error-fg [200 40 40]})

(def dark-palette
  {:terminal-bg [12 14 18]
   :text-fg [226 232 240]
   :header-fg [226 232 240]
   :header-hover-fg [255 255 255]
   :box-bg [18 22 28]
   :box-fg [226 232 240]
   :border-fg [100 116 139]
   :dialog-bg [24 28 36]
   :dialog-fg [226 232 240]
   :dialog-title-fg [255 255 255]
   :dialog-title-bg [51 65 85]
   :dialog-border [100 116 139]
   :dialog-shadow [6 8 12]
   :dialog-hint [148 163 184]
   :dialog-hint-key [226 232 240]
   :input-field-bg [15 23 42]
   :user-bubble-bg [12 14 18]
   :user-bubble-fg [226 232 240]
   :user-role-fg [245 158 11]
   :turn-separator-bg [38 30 18]
   :turn-separator-fg [245 158 11]
   :ai-bubble-bg [12 14 18]
   :ai-bubble-fg [226 232 240]
   :ai-role-fg [74 222 128]
   :status-ok [74 222 128]
   :status-bad [248 113 113]
   :warning-bg [71 49 10]
   :warning-fg [253 230 138]
   :warning-border [245 158 11]
   :cancelled-bg [31 41 55]
   :cancelled-fg [148 163 184]
   :code-block-bg [15 23 42]
   :code-ok-bg [20 50 35]
   :code-err-bg [69 26 34]
   :code-block-fg [226 232 240]
   :code-success-fg [74 222 128]
   :code-error-fg [248 113 113]
   :code-duration-fg [148 163 184]
   :code-result-fg [203 213 225]
   :code-error-result-fg [252 165 165]
   :code-syntax-special-fg [216 180 254]
   :code-syntax-keyword-fg [94 234 212]
   :code-syntax-string-fg [253 186 116]
   :code-syntax-number-fg [147 197 253]
   :code-syntax-comment-fg [148 163 184]
   :code-border-fg [100 116 139]
   :tool-color-read [56 189 248]
   :tool-color-search [216 180 254]
   :tool-color-preview [148 163 184]
   :tool-color-edit [251 191 36]
   :tool-color-create [74 222 128]
   :tool-color-delete [248 113 113]
   :tool-color-move [251 146 60]
   :tool-color-shell [203 213 225]
   :tool-color-meta [45 212 191]
   :stdout-bg [34 31 25]
   :stdout-fg [203 213 225]
   :stdout-label-fg [148 163 184]
   :stdout-sep-fg [71 65 55]
   :iteration-header-fg [148 163 184]
   :iteration-header-bg [24 28 36]
   :answer-sep-fg [100 116 139]
   :answer-sep-bg [15 23 42]
   :answer-bg [12 14 18]
   :answer-fg [241 245 249]
   :md-h1-fg [251 191 36]
   :md-h2-fg [245 158 11]
   :md-h3-fg [217 119 6]
   :confidence-fg [148 163 184]
   :md-summary-bg [49 46 129]
   :md-summary-fg [237 233 254]
   :th-md-summary-bg [55 48 94]
   :th-md-summary-fg [221 214 254]
   :proof-summary-bg [20 83 45]
   :proof-summary-fg [187 247 208]
   :th-proof-summary-bg [19 78 74]
   :th-proof-summary-fg [153 246 228]
   :link-chrome-fg [147 197 253]
   :link-chrome-arrow-fg [148 163 184]
   :link-chrome-url-fg [125 211 252]
   :link-chrome-hover-bg [30 41 59]
   :link-chrome-hover-fg [191 219 254]
   :link-chrome-blocked-fg [100 116 139]
   :footer-fg [203 213 225]
   :footer-fg-muted [148 163 184]
   :footer-fg-strong [241 245 249]
   :footer-spinner-fg [74 222 128]
   :footer-warning-fg [251 191 36]
   :footer-error-fg [248 113 113]})

(defn- make-theme
  [id display-name mode palette]
  {:name id
   :display-name display-name
   :mode mode
   :palette palette
   :fonts common-fonts
   :widths common-widths
   :spacing common-spacing
   :settings (theme-settings id mode)})

(def vis-light
  "Default Vis light theme."
  (make-theme "vis-light" "Vis Light" :light light-palette))

(def vis-dark
  "Default Vis dark theme."
  (make-theme "vis-dark" "Vis Dark" :dark dark-palette))

(def default-theme vis-light)

(def built-in-themes
  {"vis-light" vis-light
   "vis-dark" vis-dark})

(def themes
  "Process theme registry atom, keyed by string theme id.

   Built-ins live here, and extensions can add more through
   `register-theme!`, `register-themes!`, or `:ext/theme` registration."
  (atom built-in-themes))

(def palette
  "Default palette. Kept as a named var for channels that only need colours."
  (:palette default-theme))

(def pallete
  "Deprecated misspelling retained as an alias for callers/searches using
   'pallete'. Prefer `palette`."
  palette)

(defn valid-theme? [x]
  (s/valid? ::theme x))

(defn explain-theme [x]
  (s/explain-data ::theme x))

(defn- normalize-theme-id
  [id]
  (cond
    (keyword? id) (name id)
    (string? id)  (let [s (str/trim id)]
                    (when-not (str/blank? s) s))
    :else         nil))

(defn- assert-theme!
  [theme-map]
  (when-not (valid-theme? theme-map)
    (throw (ex-info "Invalid theme"
             {:type :vis.internal.theme/invalid-theme
              :theme theme-map
              :explain (explain-theme theme-map)})))
  theme-map)

(defn theme-registry
  "Return current immutable theme registry map."
  []
  @themes)

(defn theme
  "Return registered theme by id (string or keyword). Unknown ids fall back to
   `default-theme`."
  [id]
  (or (some->> id normalize-theme-id (get @themes))
    default-theme))

(defn color
  "Return RGB vector for palette token `k` in `theme-map` (or selected/default theme)."
  ([k] (color default-theme k))
  ([theme-map k] (get-in theme-map [:palette k])))

(defn- settings->theme
  [id settings]
  (let [mode (case (some-> (get settings "MODE") str str/lower-case)
               "dark" :dark
               "light" :light
               (:mode default-theme))]
    (-> (if (= :dark mode) vis-dark vis-light)
      (assoc :name id
        :display-name id
        :mode mode)
      (update :settings merge settings {"THEME_NAME" id "MODE" (name mode)}))))

(defn- theme-entry
  [id entry]
  (let [id (or (normalize-theme-id id)
             (normalize-theme-id (:name entry)))]
    (cond
      (and id (valid-theme? entry))
      [id (assert-theme! (assoc entry :name id))]

      (and id (map? entry))
      [id (settings->theme id entry)]

      :else
      (throw (ex-info "Invalid theme registry entry"
               {:type :vis.internal.theme/invalid-theme-entry
                :id id
                :entry entry})))))

(defn register-theme!
  "Add or replace one theme in the process registry.

   Arity 1 expects a full theme map with `:name`. Arity 2 accepts either a
   full theme map or a compact settings map such as `{\"PADDING\" \"0px\"}`."
  ([theme-map]
   (let [[id theme-map] (theme-entry (:name theme-map) theme-map)]
     (swap! themes assoc id theme-map)
     theme-map))
  ([id theme-map]
   (let [[id theme-map] (theme-entry id theme-map)]
     (swap! themes assoc id theme-map)
     theme-map)))

(defn register-themes!
  "Add every entry from an extension-style theme map.

     {\"THEME_NAME\" {\"PADDING\" \"0px\"}}

   Values may be compact settings maps or full theme maps."
  [theme-map]
  (doseq [[id entry] (or theme-map {})]
    (register-theme! id entry))
  @themes)

(defn unregister-theme!
  "Remove a theme id. Built-ins reset to their built-in value instead of being
   removed, so the registry always keeps light and dark available."
  [id]
  (when-let [id (normalize-theme-id id)]
    (if-let [built-in (get built-in-themes id)]
      (swap! themes assoc id built-in)
      (swap! themes dissoc id)))
  @themes)

(defn unregister-themes!
  [ids]
  (doseq [id ids]
    (unregister-theme! id))
  @themes)

(defn reset-themes!
  "Reset process registry to built-in themes. Test/dev helper."
  []
  (reset! themes built-in-themes))

(defn extension-theme-map?
  "True for extension `:ext/theme` declarations.

   Shape is intentionally plain EDN for third-party packages:

     {:ext/theme {\"THEME_NAME\" {\"PADDING\" \"0px\"}}}

   Theme names may be strings or keywords. Values may be compact setting maps
   or full theme maps."
  [x]
  (and (map? x)
    (every? theme-key? (keys x))
    (every? #(and (map? %) (every? theme-key? (keys %)) (every? theme-setting-value? (vals %)))
      (vals x))))

(defn extension-theme-settings
  "Return a registry in extension `:ext/theme` compact settings shape."
  ([] (extension-theme-settings @themes))
  ([theme-registry]
   (into {}
     (map (fn [[id theme-map]] [id (:settings theme-map)]))
     (if (instance? clojure.lang.IDeref theme-registry)
       @theme-registry
       theme-registry))))

(defn extension-themes
  "Merge `:ext/theme` maps from registered extension descriptors.
   Later extensions override earlier themes with the same name."
  [extensions]
  (reduce (fn [acc ext]
            (merge acc (:ext/theme ext)))
    {}
    (or extensions [])))

(defn available-theme-ids
  "Theme ids from the process registry plus optional unregistered extension
   descriptor maps. Normally extensions are already installed into `themes`
   by `register-extension!`; the argument remains for pure tests/previews."
  ([] (available-theme-ids nil))
  ([extensions]
   (->> (merge @themes
          (into {}
            (map (fn [[id entry]] (theme-entry id entry)))
            (extension-themes extensions)))
     keys
     (map name)
     distinct
     sort
     vec)))
