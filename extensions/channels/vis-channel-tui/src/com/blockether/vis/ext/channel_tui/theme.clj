(ns com.blockether.vis.ext.channel-tui.theme
  "Lanterna adapter for internal `com.blockether.vis.internal.theme` tokens."
  (:require [com.blockether.vis.internal.theme :as theme])
  (:import [com.googlecode.lanterna TextColor$RGB]))

(def active-theme-id (atom theme/default-theme-id))

(def default-theme (theme/theme @active-theme-id))
(def default-palette (:palette default-theme))
(def default-fonts (:fonts default-theme))
(def default-widths (:widths default-theme))
(def default-spacing (:spacing default-theme))

(defn- rgb*
  [theme-map token]
  (let [[r g b] (theme/color theme-map token)]
    (TextColor$RGB. r g b)))

(defn- rgb [token] (rgb* default-theme token))

(defn- width [token] (get default-widths token))

(defn- spacing [token] (get default-spacing token))

;;; ── Shared theme tokens adapted to Lanterna ────────────────────────────────

;; Terminal
(def terminal-bg (rgb :terminal-bg))
(def text-fg (rgb :text-fg))

;; Header
(def header-fg (rgb :header-fg))
(def header-hover-fg (rgb :header-hover-fg))
(def close-button-hover-fg (rgb :close-button-hover-fg))
(def header-active-tab-fg (rgb :header-active-tab-fg))
(def header-active-tab-bg (rgb :header-active-tab-bg))
(def header-active-tab-accent (rgb :header-active-tab-accent))
(def header-tab-number-fg (rgb :header-tab-number-fg))

;; Boxes (messages + input)
(def box-bg (rgb :box-bg))
(def box-fg (rgb :box-fg))
(def border-fg (rgb :border-fg))

;; Dialog
(def dialog-bg (rgb :dialog-bg))
(def dialog-fg (rgb :dialog-fg))
(def dialog-title-fg (rgb :dialog-title-fg))
(def dialog-title-bg (rgb :dialog-title-bg))
(def dialog-border (rgb :dialog-border))
(def dialog-shadow (rgb :dialog-shadow))
(def dialog-hint (rgb :dialog-hint))
(def dialog-hint-key (rgb :dialog-hint-key))
(def input-field-bg (rgb :input-field-bg))
(def button-bg (rgb :button-bg))
(def button-fg (rgb :button-fg))

;; Chat messages - user
(def user-bubble-bg (rgb :user-bubble-bg))
(def user-bubble-fg (rgb :user-bubble-fg))
(def user-role-fg (rgb :user-role-fg))
(def turn-separator-bg (rgb :turn-separator-bg))
(def turn-separator-fg (rgb :turn-separator-fg))

;; Chat messages - assistant
(def ai-bubble-bg (rgb :ai-bubble-bg))
(def ai-bubble-fg (rgb :ai-bubble-fg))
(def ai-role-fg (rgb :ai-role-fg))

;; Status indicators
(def status-ok (rgb :status-ok))
(def status-bad (rgb :status-bad))
(def warning-bg (rgb :warning-bg))
(def warning-fg (rgb :warning-fg))
(def warning-border (rgb :warning-border))
(def cancelled-bg (rgb :cancelled-bg))
(def cancelled-fg (rgb :cancelled-fg))

;; Code block styling
(def code-block-bg (rgb :code-block-bg))
(def code-ok-bg (rgb :code-ok-bg))
(def code-err-bg (rgb :code-err-bg))
;; Distinct RESULT-zone band — warmer than the cool code-bg / thinking gray, so a
;; tool op-card / eval output reads as its own zone (restores the lost stdout band).
(def result-bg (rgb :result-bg))
;; Filename / path chips inside a RESULT body (rg per-file headers, patch/move
;; targets): a distinct band so paths read as headers, not blended result ink.
(def result-path-bg (rgb :result-path-bg))
(def result-path-fg (rgb :result-path-fg))
(def code-block-fg (rgb :code-block-fg))
(def code-success-fg (rgb :code-success-fg))
(def code-error-fg (rgb :code-error-fg))
(def code-duration-fg (rgb :code-duration-fg))
(def code-result-fg (rgb :code-result-fg))
(def code-error-result-fg (rgb :code-error-result-fg))
(def code-syntax-special-fg (rgb :code-syntax-special-fg))
(def code-syntax-keyword-fg (rgb :code-syntax-keyword-fg))
(def code-syntax-string-fg (rgb :code-syntax-string-fg))
(def code-syntax-number-fg (rgb :code-syntax-number-fg))
(def code-syntax-comment-fg (rgb :code-syntax-comment-fg))
(def code-border-fg (rgb :code-border-fg))
(def tool-color-read (rgb :tool-color-read))
(def tool-color-search (rgb :tool-color-search))
(def tool-color-preview (rgb :tool-color-preview))
(def tool-color-edit (rgb :tool-color-edit))
(def tool-color-create (rgb :tool-color-create))
(def tool-color-delete (rgb :tool-color-delete))
(def tool-color-move (rgb :tool-color-move))
(def tool-color-shell (rgb :tool-color-shell))
(def tool-color-meta (rgb :tool-color-meta))
(def tool-color-test (rgb :tool-color-test))
(def iteration-header-fg (rgb :iteration-header-fg))
(def iteration-header-bg (rgb :iteration-header-bg))
(def answer-sep-fg (rgb :answer-sep-fg))
(def answer-sep-bg (rgb :answer-sep-bg))
(def answer-bg (rgb :answer-bg))
(def answer-fg (rgb :answer-fg))

;; Markdown
(def md-h1-fg (rgb :md-h1-fg))
(def md-h2-fg (rgb :md-h2-fg))
(def md-h3-fg (rgb :md-h3-fg))
(def confidence-fg (rgb :confidence-fg))
(def md-summary-bg (rgb :md-summary-bg))
(def md-summary-fg (rgb :md-summary-fg))
(def th-md-summary-bg (rgb :th-md-summary-bg))
(def th-md-summary-fg (rgb :th-md-summary-fg))

;; Link chrome
(def link-chrome-fg (rgb :link-chrome-fg))
(def link-chrome-arrow-fg (rgb :link-chrome-arrow-fg))
(def link-chrome-url-fg (rgb :link-chrome-url-fg))
(def link-chrome-hover-bg (rgb :link-chrome-hover-bg))
(def link-chrome-hover-fg (rgb :link-chrome-hover-fg))
(def link-chrome-blocked-fg (rgb :link-chrome-blocked-fg))

;; Footer
(def footer-fg (rgb :footer-fg))
(def footer-fg-muted (rgb :footer-fg-muted))
(def footer-fg-strong (rgb :footer-fg-strong))
(def footer-spinner-fg (rgb :footer-spinner-fg))
(def footer-warning-fg (rgb :footer-warning-fg))
(def footer-error-fg (rgb :footer-error-fg))

(defn chip-tint
  "Foreground/background pair for a semantically COLOURED button chip
   (`components/button!`'s `:tint`). The cap is FILLED with the accent colour and
   the label painted in the inverse-tab foreground for contrast — the same
   filled-cap treatment hovered header buttons use, so a tinted chip reads as a
   real button, not decoration. Unknown tints fall back to the neutral button
   palette."
  [tint]
  (case tint
    :git
    [header-active-tab-fg code-success-fg]

    (:draft :warning)
    [header-active-tab-fg footer-warning-fg]

    :error
    [header-active-tab-fg footer-error-fg]

    [button-fg button-bg]))

;; Widths
(def dialog-width-ratio (width :dialog-width-ratio))
(def dialog-min-width (width :dialog-min-width))
(def dialog-max-width (width :dialog-max-width))
(def dialog-height-ratio (width :dialog-height-ratio))
(def dialog-min-height (width :dialog-min-height))
(def dialog-max-height (width :dialog-max-height))
(def dialog-chrome-w (width :dialog-chrome-w))
(def dialog-chrome-h (width :dialog-chrome-h))
(def settings-option-indent (width :settings-option-indent))
(def input-min-width (width :input-min-width))
(def chat-min-width (width :chat-min-width))

;; Spacing
(def pad-x (spacing :pad-x))

(def ^:private color-vars
  {:terminal-bg #'terminal-bg
   :text-fg #'text-fg
   :header-fg #'header-fg
   :header-hover-fg #'header-hover-fg
   :close-button-hover-fg #'close-button-hover-fg
   :header-active-tab-fg #'header-active-tab-fg
   :header-active-tab-bg #'header-active-tab-bg
   :header-active-tab-accent #'header-active-tab-accent
   :header-tab-number-fg #'header-tab-number-fg
   :box-bg #'box-bg
   :box-fg #'box-fg
   :border-fg #'border-fg
   :dialog-bg #'dialog-bg
   :dialog-fg #'dialog-fg
   :dialog-title-fg #'dialog-title-fg
   :dialog-title-bg #'dialog-title-bg
   :dialog-border #'dialog-border
   :dialog-shadow #'dialog-shadow
   :dialog-hint #'dialog-hint
   :dialog-hint-key #'dialog-hint-key
   :input-field-bg #'input-field-bg
   :button-bg #'button-bg
   :button-fg #'button-fg
   :user-bubble-bg #'user-bubble-bg
   :user-bubble-fg #'user-bubble-fg
   :user-role-fg #'user-role-fg
   :turn-separator-bg #'turn-separator-bg
   :turn-separator-fg #'turn-separator-fg
   :ai-bubble-bg #'ai-bubble-bg
   :ai-bubble-fg #'ai-bubble-fg
   :ai-role-fg #'ai-role-fg
   :status-ok #'status-ok
   :status-bad #'status-bad
   :warning-bg #'warning-bg
   :warning-fg #'warning-fg
   :warning-border #'warning-border
   :cancelled-bg #'cancelled-bg
   :cancelled-fg #'cancelled-fg
   :code-block-bg #'code-block-bg
   :code-ok-bg #'code-ok-bg
   :code-err-bg #'code-err-bg
   :code-block-fg #'code-block-fg
   :code-success-fg #'code-success-fg
   :code-error-fg #'code-error-fg
   :code-duration-fg #'code-duration-fg
   :code-result-fg #'code-result-fg
   :code-error-result-fg #'code-error-result-fg
   :code-syntax-special-fg #'code-syntax-special-fg
   :code-syntax-keyword-fg #'code-syntax-keyword-fg
   :code-syntax-string-fg #'code-syntax-string-fg
   :code-syntax-number-fg #'code-syntax-number-fg
   :code-syntax-comment-fg #'code-syntax-comment-fg
   :code-border-fg #'code-border-fg
   :result-bg #'result-bg
   :result-path-bg #'result-path-bg
   :result-path-fg #'result-path-fg
   :tool-color-read #'tool-color-read
   :tool-color-search #'tool-color-search
   :tool-color-preview #'tool-color-preview
   :tool-color-edit #'tool-color-edit
   :tool-color-create #'tool-color-create
   :tool-color-delete #'tool-color-delete
   :tool-color-move #'tool-color-move
   :tool-color-shell #'tool-color-shell
   :tool-color-meta #'tool-color-meta
   :tool-color-test #'tool-color-test
   :iteration-header-fg #'iteration-header-fg
   :iteration-header-bg #'iteration-header-bg
   :answer-sep-fg #'answer-sep-fg
   :answer-sep-bg #'answer-sep-bg
   :answer-bg #'answer-bg
   :answer-fg #'answer-fg
   :md-h1-fg #'md-h1-fg
   :md-h2-fg #'md-h2-fg
   :md-h3-fg #'md-h3-fg
   :confidence-fg #'confidence-fg
   :md-summary-bg #'md-summary-bg
   :md-summary-fg #'md-summary-fg
   :th-md-summary-bg #'th-md-summary-bg
   :th-md-summary-fg #'th-md-summary-fg
   :link-chrome-fg #'link-chrome-fg
   :link-chrome-arrow-fg #'link-chrome-arrow-fg
   :link-chrome-url-fg #'link-chrome-url-fg
   :link-chrome-hover-bg #'link-chrome-hover-bg
   :link-chrome-hover-fg #'link-chrome-hover-fg
   :link-chrome-blocked-fg #'link-chrome-blocked-fg
   :footer-fg #'footer-fg
   :footer-fg-muted #'footer-fg-muted
   :footer-fg-strong #'footer-fg-strong
   :footer-spinner-fg #'footer-spinner-fg
   :footer-warning-fg #'footer-warning-fg
   :footer-error-fg #'footer-error-fg})

(def ^:private width-vars
  {:dialog-width-ratio #'dialog-width-ratio
   :dialog-min-width #'dialog-min-width
   :dialog-max-width #'dialog-max-width
   :dialog-height-ratio #'dialog-height-ratio
   :dialog-min-height #'dialog-min-height
   :dialog-max-height #'dialog-max-height
   :dialog-chrome-w #'dialog-chrome-w
   :dialog-chrome-h #'dialog-chrome-h
   :settings-option-indent #'settings-option-indent
   :input-min-width #'input-min-width
   :chat-min-width #'chat-min-width})

(def ^:private spacing-vars {:pad-x #'pad-x})

(defn apply-theme!
  "Apply registered theme id to the TUI adapter vars. Future paints use the
   updated Lanterna colors/widths/spacing without requiring channels to ship
   their own theme declaration."
  [theme-id]
  (let [theme-id
        (or theme-id theme/default-theme-id)

        theme-map
        (theme/theme theme-id)

        widths
        (:widths theme-map)

        spacing
        (:spacing theme-map)]

    (reset! active-theme-id (:name theme-map))
    (alter-var-root #'default-theme (constantly theme-map))
    (alter-var-root #'default-palette (constantly (:palette theme-map)))
    (alter-var-root #'default-fonts (constantly (:fonts theme-map)))
    (alter-var-root #'default-widths (constantly widths))
    (alter-var-root #'default-spacing (constantly spacing))
    (doseq [[token v] color-vars]
      (alter-var-root v (constantly (rgb* theme-map token))))
    (doseq [[token v] width-vars]
      (alter-var-root v (constantly (get widths token))))
    (doseq [[token v] spacing-vars]
      (alter-var-root v (constantly (get spacing token))))
    theme-map))
