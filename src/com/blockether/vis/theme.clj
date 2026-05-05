(ns com.blockether.vis.theme
  "Public, channel-agnostic Vis theme data.

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

(def vis-light
  "Default Vis light theme.

   Sections:
   - `:palette` — every reusable colour token.
   - `:fonts` — channel-neutral font tokens. Terminal channels may ignore
     unsupported fields but keep the names stable.
   - `:widths` — reusable layout-width tokens.
   - `:spacing` — reusable padding/gap tokens.
   - `:settings` — string-key extension/config form, e.g.
     `{\"PADDING\" \"0px\"}`."
  {:name "vis-light"
   :display-name "Vis Light"
   :mode :light
   :palette
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
    :footer-error-fg [200 40 40]}
   :fonts
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
           :style "normal"}}
   :widths
   {:dialog-width-ratio 0.84
    :dialog-min-width 76
    :dialog-max-width 132
    :dialog-height-ratio 0.84
    :dialog-min-height 18
    :dialog-max-height 42
    :settings-option-indent 2
    :input-min-width 20
    :chat-min-width 20}
   :spacing
   {:pad-x 1
    :padding "0px"
    :gap "1ch"
    :bubble-padding-x "1ch"
    :bubble-padding-y "0"}
   :settings
   {"THEME_NAME" "vis-light"
    "MODE" "light"
    "PADDING" "0px"
    "GAP" "1ch"
    "BUBBLE_PADDING_X" "1ch"
    "BUBBLE_PADDING_Y" "0"
    "FONT_FAMILY" "monospace"
    "UI_FONT_FAMILY" "system-ui, sans-serif"
    "CODE_FONT_FAMILY" "monospace"
    "DIALOG_MIN_WIDTH" 76
    "DIALOG_MAX_WIDTH" 132}})

(def default-theme vis-light)

(def themes
  "Built-in theme registry, keyed by string theme id."
  {default-theme-id vis-light})

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

(defn theme
  "Return built-in theme by id (string or keyword). Unknown ids fall back to
   `default-theme`."
  [id]
  (or (get themes (name id)) default-theme))

(defn color
  "Return RGB vector for palette token `k` in `theme-map` (or default theme)."
  ([k] (color default-theme k))
  ([theme-map k] (get-in theme-map [:palette k])))

(defn extension-theme-map?
  "True for extension `:ext/theme` declarations.

   Shape is intentionally plain EDN for third-party packages:

     {:ext/theme {\"THEME_NAME\" {\"PADDING\" \"0px\"}}}

   Theme names may be strings or keywords. Setting keys may be strings or
   keywords. Values are simple EDN scalars, RGB triples, vectors, or maps."
  [x]
  (and (map? x)
    (every? theme-key? (keys x))
    (every? #(and (map? %) (every? theme-key? (keys %)) (every? theme-setting-value? (vals %)))
      (vals x))))

(defn extension-theme-settings
  "Return built-in themes in extension `:ext/theme` shape."
  ([] (extension-theme-settings themes))
  ([theme-registry]
   (into {}
     (map (fn [[id theme-map]] [id (:settings theme-map)]))
     theme-registry)))

(defn extension-themes
  "Merge `:ext/theme` maps from registered extension descriptors.
   Later extensions override earlier themes with the same name."
  [extensions]
  (reduce (fn [acc ext]
            (merge acc (:ext/theme ext)))
    {}
    (or extensions [])))

(defn available-theme-ids
  "Theme ids from built-ins plus registered extension `:ext/theme` maps."
  ([] (available-theme-ids nil))
  ([extensions]
   (->> (merge themes (extension-themes extensions))
     keys
     (map name)
     distinct
     sort
     vec)))
