(ns com.blockether.vis.internal.theme
  "Internal, channel-agnostic Vis theme data.

   Keep this namespace pure data: no Lanterna, Swing, browser, or terminal
   backend imports. Channels adapt these tokens to their own render types."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def default-theme-id
  "The built-in theme id used when config has no explicit theme."
  "blockether-light")

(defn- byte? [x] (and (int? x) (<= 0 x 255)))

(defn rgb?
  "True when `x` is an RGB triple `[r g b]`, each channel 0..255."
  [x]
  (and (vector? x) (= 3 (count x)) (every? byte? x)))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- theme-key? [x] (or (keyword? x) (non-blank-string? x)))

(defn- theme-setting-value?
  [x]
  (or (nil? x)
      (string? x)
      (number? x)
      (boolean? x)
      (keyword? x)
      (rgb? x)
      (and (vector? x) (every? theme-setting-value? x))
      (and (map? x) (every? theme-key? (keys x)) (every? theme-setting-value? (vals x)))))

(s/def ::rgb rgb?)

(s/def ::name non-blank-string?)

(s/def ::display-name non-blank-string?)

(s/def ::mode #{:light :dark})

(s/def ::palette (s/and (s/map-of keyword? ::rgb) seq))

(s/def ::fonts map?)

(s/def ::widths map?)

(s/def ::spacing map?)

(s/def ::settings (s/map-of string? theme-setting-value?))

(s/def ::theme
  (s/keys :req-un [::name ::display-name ::mode ::palette ::fonts ::widths ::spacing ::settings]))

(def common-fonts
  {:mono {:family "monospace" :size "inherit" :weight "normal" :style "normal"}
   :ui {:family "system-ui, sans-serif" :size "inherit" :weight "normal" :style "normal"}
   :heading {:family "system-ui, sans-serif" :size "inherit" :weight "700" :style "normal"}
   :code {:family "monospace" :size "inherit" :weight "normal" :style "normal"}})

(def common-widths
  {:dialog-width-ratio 0.90
   :dialog-min-width 88
   :dialog-max-width 140
   :dialog-height-ratio 0.95
   :dialog-min-height 30
   :dialog-max-height 80
   :dialog-chrome-w 4
   :dialog-chrome-h 6
   :settings-option-indent 2
   :input-min-width 20
   :chat-min-width 20})

(def common-spacing
  {:pad-x 1 :padding "0px" :gap "1ch" :bubble-padding-x "1ch" :bubble-padding-y "0"})

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
   :header-hover-fg [10 50 160]
   :close-button-hover-fg [200 40 40]
   ;; Active workspace tab in the header. A calm indigo slab with a soft
   ;; off-white label reads as "selected" without the harsh pure black-on-
   ;; white invert; inactive tabs stay dim+italic from `border-fg`.
   :header-active-tab-fg [240 244 252]
   :header-active-tab-bg [37 99 235]
   :header-active-tab-accent [37 99 235]
   ;; Tab index badge — soft off-white, matching the active-tab label: one
   ;; calm indigo slab, no gold flash. (Premium restraint: the indigo IS the
   ;; accent; everything on it stays in the same off-white family.)
   :header-tab-number-fg [240 244 252]
   :box-bg [255 255 255]
   :box-fg [30 30 30]
   :border-fg [80 80 80]
   :dialog-bg [248 248 248]
   :dialog-fg [30 30 30]
   :dialog-title-fg [255 255 255]
   :dialog-title-bg [60 60 60]
   ;; Button chip — a SUBTLE non-white fill so a button reads as a raised
   ;; control on white surfaces (header, find bar), not blank text.
   :button-bg [226 232 240]
   :button-fg [30 41 59]
   :dialog-border [120 120 120]
   :dialog-shadow [200 200 200]
   :dialog-hint [120 120 120]
   :dialog-hint-key [50 50 50]
   :input-field-bg [255 255 252]
   :user-bubble-bg [255 255 255]
   :user-bubble-fg [30 30 30]
   ;; Role labels + turn chrome: slate neutrals, not gold — chrome should
   ;; recede; only semantic states (ok/warn/error) and the indigo accent
   ;; carry color.
   :user-role-fg [71 85 105]
   :turn-separator-bg [245 247 250]
   :turn-separator-fg [148 163 184]
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
   :result-bg [244 246 250]
   ;; Filename / path chips inside a RESULT body (e.g. rg's per-file headers):
   ;; a distinct blue band so paths read as clickable headers, not blended ink.
   :result-path-bg [219 234 254]
   :result-path-fg [23 45 130]
   ;; Matched search-term highlight in rg/cat result rows (SGR 7 reverse-video).
   ;; A highlight must POP: NOT warning-fg, whose light-theme ink is a dark olive
   ;; that reads muddy. Violet is unmistakable and clears 6:1 on the result band.
   :result-highlight-fg [124 40 180]
   :code-block-fg [30 30 30]
   :code-success-fg [40 160 60]
   :code-error-fg [220 50 50]
   :code-duration-fg [130 130 130]
   ;; Result text inside code/output blocks: the REGULAR fully-visible ink
   ;; (same as code text) — output is content the user reads, so it gets
   ;; first-class contrast, not a highlight color (the old amber/gold read
   ;; as an alarm) and not a dimmed gray. Solarized already does this
   ;; (its result ink == its body ink).
   :code-result-fg [30 30 30]
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
   ;; Shell badge: slate, not khaki/olive — terminal output is neutral
   ;; infrastructure, not a highlight.
   :tool-color-shell [100 116 139]
   :tool-color-meta [20 135 135]
   :tool-color-test [79 70 229]
   :iteration-header-fg [170 170 170]
   :iteration-header-bg [244 244 244]
   :answer-sep-fg [190 190 190]
   :answer-sep-bg [250 250 250]
   :answer-bg [255 255 255]
   :answer-fg [25 25 25]
   ;; Markdown headings: an editorial slate ladder (near-black → slate),
   ;; weight carries the hierarchy — no gold/brown ink.
   :md-h1-fg [15 23 42]
   :md-h2-fg [30 41 59]
   :md-h3-fg [51 65 85]
   :confidence-fg [140 140 140]
   :md-summary-bg [226 214 250]
   :md-summary-fg [55 30 120]
   :th-md-summary-bg [244 244 244]
   :th-md-summary-fg [80 80 80]
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
   :header-hover-fg [125 211 252]
   :close-button-hover-fg [248 113 113]
   ;; Calm blue slab (see light palette) with a soft off-white label for
   ;; the active tab — no harsh pure black/white invert; inactive tabs read
   ;; as dim italic via `border-fg`.
   :header-active-tab-fg [226 232 240]
   :header-active-tab-bg [37 99 235]
   :header-active-tab-accent [125 211 252]
   ;; Tab index badge matches the active-tab label (see light palette) —
   ;; the indigo slab is the accent; no gold on it.
   :header-tab-number-fg [226 232 240]
   :box-bg [18 22 28]
   :box-fg [226 232 240]
   :border-fg [100 116 139]
   :dialog-bg [24 28 36]
   :dialog-fg [226 232 240]
   :dialog-title-fg [255 255 255]
   :dialog-title-bg [51 65 85]
   ;; Button chip — lighter than the dark dialog bg so it reads as raised.
   :button-bg [51 65 85]
   :button-fg [226 232 240]
   :dialog-border [100 116 139]
   :dialog-shadow [6 8 12]
   :dialog-hint [148 148 148]
   :dialog-hint-key [226 232 240]
   :input-field-bg [15 23 42]
   :user-bubble-bg [12 14 18]
   :user-bubble-fg [226 232 240]
   ;; Slate role/turn chrome (see light palette) — chrome recedes.
   :user-role-fg [148 163 184]
   :turn-separator-bg [20 26 36]
   :turn-separator-fg [100 116 139]
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
   :result-bg [20 26 36]
   :result-path-bg [37 51 84]
   :result-path-fg [147 197 253]
   ;; Search-term highlight (SGR 7). See light palette — light lavender pops on dark.
   :result-highlight-fg [216 180 254]
   :code-block-fg [226 232 240]
   :code-success-fg [74 222 128]
   :code-error-fg [248 113 113]
   :code-duration-fg [148 163 184]
   ;; See light palette: result text is the regular fully-visible ink in
   ;; dark mode too — content, not a yellow alarm, not dimmed gray.
   :code-result-fg [226 232 240]
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
   ;; Slate shell badge (see light palette) — no khaki.
   :tool-color-shell [148 163 184]
   :tool-color-meta [45 212 191]
   :tool-color-test [129 140 248]
   :iteration-header-fg [148 148 148]
   :iteration-header-bg [30 30 30]
   :answer-sep-fg [100 116 139]
   :answer-sep-bg [15 23 42]
   :answer-bg [12 14 18]
   :answer-fg [241 245 249]
   ;; Editorial slate heading ladder (see light palette) — no amber ink.
   :md-h1-fg [241 245 249]
   :md-h2-fg [203 213 225]
   :md-h3-fg [148 163 184]
   :confidence-fg [148 163 184]
   :md-summary-bg [49 46 129]
   :md-summary-fg [237 233 254]
   :th-md-summary-bg [30 30 30]
   :th-md-summary-fg [180 180 180]
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

(def blockether-light-palette
  "Blockether brand light palette: warm cream ground + amber accent + brand
   green/red/teal — the exact tokens the blockether.com site and the spel
   Allure report use. A re-skin of `light-palette`; only the brand chrome is
   overridden (syntax + per-tool badge colours are inherited)."
  (merge
    light-palette
    {:terminal-bg [250 243 235]
     :text-fg [38 38 38]
     :header-fg [38 38 38]
     :header-hover-fg [180 120 0]
     :close-button-hover-fg [200 40 40]
     :header-active-tab-fg [38 30 0]
     :header-active-tab-bg [240 173 0]
     :header-active-tab-accent [240 173 0]
     :header-tab-number-fg [38 30 0]
     :box-bg [250 243 235]
     :box-fg [38 38 38]
     :border-fg [140 133 122]
     :dialog-bg [250 243 235]
     :dialog-fg [38 38 38]
     ;; Cream-on-ink title header — the spel-bridge dialog signature: the dark
     ;; frame ink carried across the header row with the warm cream ground as text.
     :dialog-title-fg [250 243 235]
     :dialog-title-bg [63 63 63]
     :button-bg [240 173 0]
     :button-fg [38 30 0]
     :dialog-border [63 63 63]
     :dialog-shadow [255 196 32]
     :dialog-hint [111 106 99]
     :dialog-hint-key [63 63 63]
     :input-field-bg [255 253 248]
     :user-bubble-bg [250 243 235]
     :user-bubble-fg [38 38 38]
     :user-role-fg [111 106 99]
     :turn-separator-bg [245 238 228]
     :turn-separator-fg [180 172 158]
     :ai-bubble-bg [250 243 235]
     :ai-bubble-fg [38 38 38]
     :ai-role-fg [22 163 74]
     :status-ok [22 163 74]
     :status-bad [220 38 38]
     :warning-bg [250 236 197]
     :warning-fg [122 74 0]
     :warning-border [217 119 6]
     :cancelled-bg [242 235 223]
     :cancelled-fg [111 106 99]
     :code-block-bg [242 235 223]
     :code-ok-bg [223 240 224]
     :code-err-bg [250 226 226]
     :result-bg [242 235 223]
     ;; Blue inline-code chips stay distinct from the amber summary headline.
     :result-path-bg [219 234 254]
     :result-path-fg [23 45 130]
     :code-block-fg [38 38 38]
     :code-success-fg [22 163 74]
     :code-error-fg [220 38 38]
     :code-duration-fg [111 106 99]
     :code-result-fg [38 38 38]
     :code-error-result-fg [180 40 40]
     :code-border-fg [140 133 122]
     :iteration-header-fg [176 167 152]
     :iteration-header-bg [245 238 228]
     :answer-sep-fg [200 191 176]
     :answer-sep-bg [250 243 235]
     :answer-bg [250 243 235]
     :answer-fg [38 38 38]
     :md-h1-fg [38 38 38]
     :md-h2-fg [63 63 63]
     :md-h3-fg [90 84 74]
     :confidence-fg [111 106 99]
     :md-summary-bg [250 236 197]
     :md-summary-fg [122 74 0]
     :th-md-summary-bg [245 238 228]
     :th-md-summary-fg [63 63 63]
     :link-chrome-fg [8 145 178]
     :link-chrome-arrow-fg [111 106 99]
     :link-chrome-url-fg [120 110 95]
     :link-chrome-hover-bg [250 236 197]
     :link-chrome-hover-fg [180 120 0]
     :link-chrome-blocked-fg [180 172 158]
     :footer-fg [63 63 63]
     :footer-fg-muted [111 106 99]
     :footer-fg-strong [38 38 38]
     :footer-spinner-fg [22 163 74]
     :footer-warning-fg [180 110 0]
     :footer-error-fg [200 40 40]}))

(def blockether-dark-palette
  "Blockether brand dark palette: deep slate ground + amber accent + brand
   green/red/teal — matching the blockether.com / spel report dark theme.
   A re-skin of `dark-palette`; syntax + per-tool badge colours are inherited."
  (merge
    dark-palette
    {:terminal-bg [15 17 23]
     :text-fg [243 244 246]
     :header-fg [243 244 246]
     :header-hover-fg [255 196 32]
     :close-button-hover-fg [248 113 113]
     :header-active-tab-fg [15 17 23]
     :header-active-tab-bg [255 196 32]
     :header-active-tab-accent [255 196 32]
     :header-tab-number-fg [15 17 23]
     :box-bg [22 24 32]
     :box-fg [243 244 246]
     :border-fg [70 74 84]
     :dialog-bg [22 24 32]
     :dialog-fg [243 244 246]
     ;; Amber accent title stripe with dark ink — brand signature on dark.
     :dialog-title-fg [15 17 23]
     :dialog-title-bg [255 196 32]
     :button-bg [40 43 54]
     :button-fg [243 244 246]
     :dialog-border [70 74 84]
     :dialog-shadow [4 5 8]
     :dialog-hint [156 163 175]
     :dialog-hint-key [243 244 246]
     :input-field-bg [12 14 18]
     :user-bubble-bg [15 17 23]
     :user-bubble-fg [243 244 246]
     :user-role-fg [156 163 175]
     :turn-separator-bg [20 22 30]
     :turn-separator-fg [70 74 84]
     :ai-bubble-bg [15 17 23]
     :ai-bubble-fg [243 244 246]
     :ai-role-fg [74 222 128]
     :status-ok [74 222 128]
     :status-bad [248 113 113]
     :warning-bg [60 46 8]
     :warning-fg [251 220 130]
     :warning-border [251 191 36]
     :cancelled-bg [30 32 40]
     :cancelled-fg [156 163 175]
     :code-block-bg [30 32 40]
     :code-ok-bg [20 50 35]
     :code-err-bg [69 26 34]
     :result-bg [22 24 32]
     ;; Blue inline-code chips stay distinct from the amber summary headline.
     :result-path-bg [37 51 84]
     :result-path-fg [147 197 253]
     :code-block-fg [243 244 246]
     :code-success-fg [74 222 128]
     :code-error-fg [248 113 113]
     :code-duration-fg [156 163 175]
     :code-result-fg [243 244 246]
     :code-error-result-fg [252 165 165]
     :code-border-fg [70 74 84]
     :iteration-header-fg [120 120 120]
     :iteration-header-bg [22 24 32]
     :answer-sep-fg [70 74 84]
     :answer-sep-bg [15 17 23]
     :answer-bg [15 17 23]
     :answer-fg [243 244 246]
     :md-h1-fg [243 244 246]
     :md-h2-fg [209 213 219]
     :md-h3-fg [156 163 175]
     :confidence-fg [156 163 175]
     :md-summary-bg [60 46 8]
     :md-summary-fg [255 220 150]
     :th-md-summary-bg [22 24 32]
     :th-md-summary-fg [180 180 180]
     :link-chrome-fg [34 211 238]
     :link-chrome-arrow-fg [156 163 175]
     :link-chrome-url-fg [130 200 220]
     :link-chrome-hover-bg [30 40 50]
     :link-chrome-hover-fg [140 230 250]
     :link-chrome-blocked-fg [70 74 84]
     :footer-fg [209 213 219]
     :footer-fg-muted [156 163 175]
     :footer-fg-strong [243 244 246]
     :footer-spinner-fg [74 222 128]
     :footer-warning-fg [251 191 36]
     :footer-error-fg [248 113 113]}))

(def solarized-light-palette
  ;; Ethan Schoonover's Solarized Light: base3 paper, base00/base01 ink,
  ;; standard accent hues mapped onto the same roles as `light-palette`.
  {:terminal-bg [253 246 227]
   :text-fg [101 123 131]
   :header-fg [88 110 117]
   :header-hover-fg [38 139 210]
   :close-button-hover-fg [220 50 47]
   :header-active-tab-fg [253 246 227]
   :header-active-tab-bg [38 139 210]
   :header-active-tab-accent [38 139 210]
   ;; Badge sits on the blue active-tab slab; match the base3 tab label —
   ;; one calm slab, no gold flash.
   :header-tab-number-fg [253 246 227]
   :box-bg [253 246 227]
   :box-fg [101 123 131]
   :border-fg [147 161 161]
   :dialog-bg [238 232 213]
   :dialog-fg [101 123 131]
   :dialog-title-fg [253 246 227]
   :dialog-title-bg [88 110 117]
   :button-bg [222 216 197]
   :button-fg [88 110 117]
   :dialog-border [147 161 161]
   :dialog-shadow [213 206 189]
   :dialog-hint [147 161 161]
   :dialog-hint-key [88 110 117]
   :input-field-bg [253 246 227]
   :user-bubble-bg [253 246 227]
   :user-bubble-fg [101 123 131]
   ;; Chrome recedes: base01/base1 ink for role labels + separators
   ;; (the yellow accent stays for genuinely semantic uses only).
   :user-role-fg [88 110 117]
   :turn-separator-bg [238 232 213]
   :turn-separator-fg [147 161 161]
   :ai-bubble-bg [253 246 227]
   :ai-bubble-fg [101 123 131]
   :ai-role-fg [133 153 0]
   :status-ok [133 153 0]
   :status-bad [220 50 47]
   :warning-bg [245 234 193]
   :warning-fg [88 66 0]
   :warning-border [181 137 0]
   :cancelled-bg [238 232 213]
   :cancelled-fg [147 161 161]
   :code-block-bg [238 232 213]
   :code-ok-bg [231 235 203]
   :code-err-bg [247 224 219]
   :result-bg [245 236 210]
   ;; Path chips inside a RESULT body must read as a distinct BLUE header band
   ;; (like `light-palette`), not blend into the warm cream. The old olive band
   ;; ([223 226 197]) shared the cream's hue and only reached 2.77:1 ink
   ;; contrast — a cool pale-blue slab with darkened Solarized-blue ink (6.1:1)
   ;; restores the clickable-header read.
   :result-path-bg [213 227 245]
   :result-path-fg [24 84 130]
   :result-highlight-fg [124 40 180]
   :code-block-fg [88 110 117]
   :code-success-fg [133 153 0]
   :code-error-fg [220 50 47]
   :code-duration-fg [147 161 161]
   :code-result-fg [88 110 117]
   :code-error-result-fg [203 75 22]
   :code-syntax-special-fg [108 113 196]
   :code-syntax-keyword-fg [42 161 152]
   :code-syntax-string-fg [203 75 22]
   :code-syntax-number-fg [38 139 210]
   :code-syntax-comment-fg [147 161 161]
   :code-border-fg [147 161 161]
   :tool-color-read [38 139 210]
   :tool-color-search [108 113 196]
   :tool-color-preview [147 161 161]
   :tool-color-edit [181 137 0]
   :tool-color-create [133 153 0]
   :tool-color-delete [220 50 47]
   :tool-color-move [203 75 22]
   ;; base00 ink, not khaki — shell output is neutral infrastructure.
   :tool-color-shell [101 123 131]
   :tool-color-meta [42 161 152]
   :tool-color-test [108 113 196]
   :iteration-header-fg [147 161 161]
   :iteration-header-bg [238 232 213]
   :answer-sep-fg [147 161 161]
   :answer-sep-bg [253 246 227]
   :answer-bg [253 246 227]
   :answer-fg [88 110 117]
   :md-h1-fg [181 137 0]
   :md-h2-fg [203 75 22]
   :md-h3-fg [108 113 196]
   :confidence-fg [147 161 161]
   :md-summary-bg [230 228 244]
   :md-summary-fg [73 78 150]
   :th-md-summary-bg [238 232 213]
   :th-md-summary-fg [101 123 131]
   :link-chrome-fg [38 139 210]
   :link-chrome-arrow-fg [147 161 161]
   :link-chrome-url-fg [42 161 152]
   :link-chrome-hover-bg [238 232 213]
   :link-chrome-hover-fg [32 110 170]
   :link-chrome-blocked-fg [147 161 161]
   :footer-fg [88 110 117]
   :footer-fg-muted [147 161 161]
   :footer-fg-strong [7 54 66]
   :footer-spinner-fg [133 153 0]
   :footer-warning-fg [181 137 0]
   :footer-error-fg [220 50 47]})

(def solarized-dark-palette
  ;; Ethan Schoonover's Solarized Dark: base03 ground, base0/base1 ink,
  ;; standard accent hues mapped onto the same roles as `dark-palette`.
  {:terminal-bg [0 43 54]
   :text-fg [131 148 150]
   :header-fg [131 148 150]
   :header-hover-fg [38 139 210]
   :close-button-hover-fg [220 50 47]
   :header-active-tab-fg [253 246 227]
   :header-active-tab-bg [38 139 210]
   :header-active-tab-accent [42 161 152]
   ;; Match the base3 tab label on the blue slab (see solarized-light).
   :header-tab-number-fg [253 246 227]
   :box-bg [0 43 54]
   :box-fg [131 148 150]
   :border-fg [88 110 117]
   :dialog-bg [7 54 66]
   :dialog-fg [131 148 150]
   :dialog-title-fg [253 246 227]
   :dialog-title-bg [88 110 117]
   :button-bg [88 110 117]
   :button-fg [253 246 227]
   :dialog-border [88 110 117]
   :dialog-shadow [0 30 38]
   :dialog-hint [101 123 131]
   :dialog-hint-key [147 161 161]
   :input-field-bg [7 54 66]
   :user-bubble-bg [0 43 54]
   :user-bubble-fg [131 148 150]
   ;; Chrome recedes (see solarized-light): base1/base01 ink.
   :user-role-fg [147 161 161]
   :turn-separator-bg [7 54 66]
   :turn-separator-fg [88 110 117]
   :ai-bubble-bg [0 43 54]
   :ai-bubble-fg [131 148 150]
   :ai-role-fg [133 153 0]
   :status-ok [133 153 0]
   :status-bad [220 50 47]
   :warning-bg [51 44 7]
   :warning-fg [222 188 80]
   :warning-border [181 137 0]
   :cancelled-bg [7 54 66]
   :cancelled-fg [88 110 117]
   :code-block-bg [7 54 66]
   :code-ok-bg [12 56 38]
   :code-err-bg [62 32 36]
   :result-bg [9 49 52]
   :result-path-bg [17 62 71]
   :result-path-fg [38 139 210]
   :result-highlight-fg [216 180 254]
   :code-block-fg [147 161 161]
   :code-success-fg [133 153 0]
   :code-error-fg [220 50 47]
   :code-duration-fg [88 110 117]
   :code-result-fg [147 161 161]
   :code-error-result-fg [203 75 22]
   :code-syntax-special-fg [108 113 196]
   :code-syntax-keyword-fg [42 161 152]
   :code-syntax-string-fg [203 75 22]
   :code-syntax-number-fg [38 139 210]
   :code-syntax-comment-fg [88 110 117]
   :code-border-fg [88 110 117]
   :tool-color-read [38 139 210]
   :tool-color-search [108 113 196]
   :tool-color-preview [147 161 161]
   :tool-color-edit [181 137 0]
   :tool-color-create [133 153 0]
   :tool-color-delete [220 50 47]
   :tool-color-move [203 75 22]
   ;; base0 ink, not khaki — shell output is neutral infrastructure.
   :tool-color-shell [131 148 150]
   :tool-color-meta [42 161 152]
   :tool-color-test [108 113 196]
   :iteration-header-fg [88 110 117]
   :iteration-header-bg [7 54 66]
   :answer-sep-fg [88 110 117]
   :answer-sep-bg [7 54 66]
   :answer-bg [0 43 54]
   :answer-fg [147 161 161]
   :md-h1-fg [181 137 0]
   :md-h2-fg [203 75 22]
   :md-h3-fg [108 113 196]
   :confidence-fg [88 110 117]
   :md-summary-bg [35 38 80]
   :md-summary-fg [191 193 235]
   :th-md-summary-bg [7 54 66]
   :th-md-summary-fg [131 148 150]
   :link-chrome-fg [38 139 210]
   :link-chrome-arrow-fg [88 110 117]
   :link-chrome-url-fg [42 161 152]
   :link-chrome-hover-bg [7 54 66]
   :link-chrome-hover-fg [108 174 224]
   :link-chrome-blocked-fg [88 110 117]
   :footer-fg [131 148 150]
   :footer-fg-muted [88 110 117]
   :footer-fg-strong [238 232 213]
   :footer-spinner-fg [133 153 0]
   :footer-warning-fg [181 137 0]
   :footer-error-fg [220 50 47]})

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

(def vis-light "Default Vis light theme." (make-theme "vis-light" "Vis Light" :light light-palette))

(def vis-dark "Default Vis dark theme." (make-theme "vis-dark" "Vis Dark" :dark dark-palette))

(def solarized-light
  "Solarized Light theme."
  (make-theme "solarized-light" "Solarized Light" :light solarized-light-palette))

(def solarized-dark
  "Solarized Dark theme."
  (make-theme "solarized-dark" "Solarized Dark" :dark solarized-dark-palette))

(def blockether-light
  "Blockether brand light theme — warm cream + amber, matching blockether.com."
  (make-theme "blockether-light" "Blockether Light" :light blockether-light-palette))

(def blockether-dark
  "Blockether brand dark theme — matching blockether.com / the spel report."
  (make-theme "blockether-dark" "Blockether Dark" :dark blockether-dark-palette))

(def default-theme blockether-light)

(def built-in-themes
  {"blockether-light" blockether-light
   "blockether-dark" blockether-dark
   "vis-light" vis-light
   "vis-dark" vis-dark
   "solarized-light" solarized-light
   "solarized-dark" solarized-dark})

(defonce themes
  ;; Process theme registry atom, keyed by string theme id.
  ;;
  ;; Built-ins live here, and extensions can add more through
  ;; `register-theme!`, `register-themes!`, or `:ext/theme` registration.
  ;;
  ;; `defonce`, NOT `def`: extension themes are registered as a
  ;; `register-extension!` side effect that `require` won't re-fire, so a
  ;; plain `def` would drop every extension-contributed theme on each
  ;; `:reload`. Tradeoff: editing the `built-in-themes` literal no longer
  ;; takes effect on a bare reload — call `(reset! themes built-in-themes)`
  ;; (as `unregister-themes!` does) or reboot to refresh built-ins.
  ;; (`defonce` takes no docstring — hence the `;;` comment.)
  (atom built-in-themes))

(def palette
  "Default palette. Kept as a named var for channels that only need colours."
  (:palette default-theme))

(def pallete
  "Deprecated misspelling retained as an alias for callers/searches using
   'pallete'. Prefer `palette`."
  palette)

(defn valid-theme? [x] (s/valid? ::theme x))

(defn explain-theme [x] (s/explain-data ::theme x))

(defn- normalize-theme-id
  [id]
  (cond (keyword? id) (name id)
        (string? id) (let [s (str/trim id)]
                       (when-not (str/blank? s) s))
        :else nil))

(defn- assert-theme!
  [theme-map]
  (when-not (valid-theme? theme-map)
    (throw (ex-info "Invalid theme"
                    {:type :vis.internal.theme/invalid-theme
                     :theme theme-map
                     :explain (explain-theme theme-map)})))
  theme-map)

(defn theme-registry "Return current immutable theme registry map." [] @themes)

(defn theme
  "Return registered theme by id (string or keyword). Unknown ids fall back to
   `default-theme`."
  [id]
  (or (some->> id
               normalize-theme-id
               (get @themes))
      default-theme))

(defn color
  "Return RGB vector for palette token `k` in `theme-map` (or selected/default theme)."
  ([k] (color default-theme k))
  ([theme-map k] (get-in theme-map [:palette k])))

(defn- settings->theme
  [id settings]
  (let
    [mode (case
            (some-> (get settings "MODE")
                    str
                    str/lower-case)
            "dark"
            :dark

            "light"
            :light

            (:mode default-theme))]
    (-> (if (= :dark mode) vis-dark vis-light)
        (assoc :name id
               :display-name id
               :mode mode)
        (update :settings merge settings {"THEME_NAME" id "MODE" (name mode)}))))

(defn- theme-entry
  [id entry]
  (let [id (or (normalize-theme-id id) (normalize-theme-id (:name entry)))]
    (cond (and id (valid-theme? entry)) [id (assert-theme! (assoc entry :name id))]
          (and id (map? entry)) [id (settings->theme id entry)]
          :else (throw (ex-info
                         "Invalid theme registry entry"
                         {:type :vis.internal.theme/invalid-theme-entry :id id :entry entry})))))

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
         (map (fn [[id theme-map]]
                [id (:settings theme-map)]))
         (if (instance? clojure.lang.IDeref theme-registry) @theme-registry theme-registry))))

(defn extension-themes
  "Merge `:ext/theme` maps from registered extension descriptors.
   Later extensions override earlier themes with the same name."
  [extensions]
  (reduce (fn [acc ext]
            (merge acc (:ext/theme ext)))
          {}
          (or extensions [])))

(def ^:private theme-id-priority
  "Ids pinned to the TOP of `available-theme-ids` (and thus every theme
   picker), in this order. Everything else follows alphabetically."
  ["vis-light" "vis-dark"])

(defn available-theme-ids
  "Theme ids from the process registry plus optional unregistered extension
   descriptor maps. Normally extensions are already installed into `themes`
   by `register-extension!`; the argument remains for pure tests/previews.

   `vis-light`/`vis-dark` are pinned to the top (see `theme-id-priority`);
   all other ids follow, sorted alphabetically."
  ([] (available-theme-ids nil))
  ([extensions]
   (let
     [rank
      (into {}
            (map-indexed (fn [i n]
                           [n i])
                         theme-id-priority))

      floor
      (count theme-id-priority)]

     (->> (merge @themes
                 (into {}
                       (map (fn [[id entry]]
                              (theme-entry id entry)))
                       (extension-themes extensions)))
          keys
          (map name)
          distinct
          (sort-by (juxt #(rank % floor) identity))
          vec))))

(defn- mix-rgb
  "Linear mix of two RGB triples: `t` 0.0 -> all `a`, 1.0 -> all `b`."
  [a b ^double t]
  (mapv (fn [x y]
          (let
            [x
             (double x)

             y
             (double y)]

            (int (Math/round (+ x (* t (- y x)))))))
        a
        b))

(defn rgb->css
  "Hex CSS color (\"#rrggbb\") for an RGB triple."
  [[r g b]]
  (format "#%02x%02x%02x" r g b))

(defn- rel-luminance
  "Rec. 709 relative luminance of an RGB triple (0-255 scale)."
  ^double [[r g b]]
  (+ (* 0.2126 (double r)) (* 0.7152 (double g)) (* 0.0722 (double b))))

(defn- mix-to-luminance-delta
  "Mix `bg` toward `target`, but only as far as needed to land a luminance
   distance of `delta` from `bg` (clamped to fully `target`). Keeps every
   theme's hairlines at EQUAL perceptual weight regardless of how close the
   palette's border color sits to its ground — fixing washed-out, near-white
   borders on low-contrast palettes (e.g. Solarized Light)."
  [bg target ^double delta]
  (let
    [dl
     (Math/abs (- (rel-luminance target) (rel-luminance bg)))

     t
     (if (> dl 1.0) (min 1.0 (/ delta dl)) 1.0)]

    (mix-rgb bg target t)))

(def web-css-palette-tokens
  "The shared TUI/HTML theme contract: CSS custom property -> palette token.
   The HTML transcript export renders these straight from the SAME palette
   the TUI paints with, so every registered theme works in both places."
  {"--bg" :terminal-bg
   "--fg" :text-fg
   "--panel2" :dialog-bg
   "--code-bg" :code-block-bg
   ;; Semantic accent/status vars (renamed from the legacy color-named
   ;; --gold/--gold2/--amber/--indigo the stylesheet used to carry). Values
   ;; still come STRAIGHT from the shared palette, so every theme tracks:
   ;;   --primary   the accent (active-tab slab)   --primary-fg its on-accent label
   ;;   --accent    the lighter accent companion   --surface    the neutral panel ground
   ;;   --secondary emphasis ink  --warning/-bg warn text+ground  --ok/--err status.
   "--surface" :dialog-bg
   ;; Dialog title stripe — the amber accent bar (blockether.com / spel report
   ;; signature) the TUI dialog paints, so web modals wear the SAME header.
   "--dialog-title-bg" :dialog-title-bg
   "--dialog-title-fg" :dialog-title-fg
   "--dialog-border" :dialog-border
   "--primary" :header-active-tab-bg
   "--primary-fg" :header-active-tab-fg
   "--accent" :header-active-tab-accent
   "--secondary" :code-result-fg
   "--warning" :warning-fg
   "--warning-bg" :warning-bg
   "--ok" :status-ok
   "--err" :status-bad
   ;; Per-tool BADGE colors — the SAME palette tokens the TUI paints native
   ;; tool op-cards with, so the web badge tracks every theme (light/dark/…).
   "--tool-read" :tool-color-read
   "--tool-search" :tool-color-search
   "--tool-preview" :tool-color-preview
   "--tool-edit" :tool-color-edit
   "--tool-create" :tool-color-create
   "--tool-delete" :tool-color-delete
   "--tool-move" :tool-color-move
   "--tool-shell" :tool-color-shell
   "--tool-meta" :tool-color-meta
   "--tool-test" :tool-color-test})

(def web-css-derived-tokens
  "CSS vars DERIVED from the palette as a bg->fg mix ratio, so hover/dim text
   track any theme's ground automatically (light or dark)."
  {"--hover" 0.05 "--dim" 0.52})

(def web-css-border-tokens
  "Hairline border vars keyed to a TARGET luminance delta from the ground (NOT
   a raw mix ratio): each is mixed from `--bg` toward the palette's `:border-fg`
   just far enough to hit that delta. Borders then read with equal weight on
   EVERY theme — the same presence vis-light's hairline carries — even where a
   palette's own border color sits close to its background (Solarized), instead
   of washing out to near-white."
  {"--line" 18.0 "--line2" 34.0})

(defn theme->web-css-vars
  "CSS custom-property map (\"--bg\" -> \"#rrggbb\") for a theme map -
   the palette-named tokens, the derived bg/fg mixes, and the
   luminance-delta-normalized border hairlines."
  [{:keys [palette]}]
  (let
    [bg
     (:terminal-bg palette)

     fg
     (:text-fg palette)

     border
     (:border-fg palette)]

    (merge (into (sorted-map)
                 (map (fn [[css-var palette-key]]
                        [css-var (rgb->css (get palette palette-key))]))
                 web-css-palette-tokens)
           (into (sorted-map)
                 (map (fn [[css-var ratio]]
                        [css-var (rgb->css (mix-rgb bg fg ratio))]))
                 web-css-derived-tokens)
           (into (sorted-map)
                 (map (fn [[css-var delta]]
                        [css-var (rgb->css (mix-to-luminance-delta bg border delta))]))
                 web-css-border-tokens))))

(defn web-css-root
  "A `:root{...}` CSS block for a theme id (or theme map): every shared
   var from `theme->web-css-vars` plus `color-scheme`, ready to serve
   AFTER the static stylesheet so it overrides the baked-in defaults."
  [theme-or-id]
  (let [theme-map (if (map? theme-or-id) theme-or-id (theme theme-or-id))]
    (str ":root{"
         (->> (theme->web-css-vars theme-map)
              (map (fn [[k v]]
                     (str k ":" v)))
              (str/join ";"))
         ";color-scheme:"
         (name (:mode theme-map :light))
         "}")))
