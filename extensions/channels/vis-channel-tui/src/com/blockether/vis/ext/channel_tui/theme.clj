(ns com.blockether.vis.ext.channel-tui.theme
  "Lanterna adapter for public `com.blockether.vis.theme` tokens."
  (:require [com.blockether.vis.theme :as theme])
  (:import [com.googlecode.lanterna TextColor$RGB]))

(def default-theme theme/default-theme)
(def default-palette (:palette default-theme))
(def default-fonts (:fonts default-theme))
(def default-widths (:widths default-theme))
(def default-spacing (:spacing default-theme))

(defn- rgb
  [token]
  (let [[r g b] (theme/color default-theme token)]
    (TextColor$RGB. r g b)))

;;; ── Light theme — adapted from shared public tokens ────────────────────────

;; Terminal
(def terminal-bg    (rgb :terminal-bg))
(def text-fg        (rgb :text-fg))

;; Header
(def header-fg       (rgb :header-fg))
(def header-hover-fg (rgb :header-hover-fg))

;; Boxes (messages + input)
(def box-bg         (rgb :box-bg))
(def box-fg         (rgb :box-fg))
(def border-fg      (rgb :border-fg))

;; Dialog
(def dialog-bg       (rgb :dialog-bg))
(def dialog-fg       (rgb :dialog-fg))
(def dialog-title-fg (rgb :dialog-title-fg))
(def dialog-title-bg (rgb :dialog-title-bg))
(def dialog-border   (rgb :dialog-border))
(def dialog-shadow   (rgb :dialog-shadow))
(def dialog-hint     (rgb :dialog-hint))
(def dialog-hint-key (rgb :dialog-hint-key))

;; Chat messages — user
(def user-bubble-bg    (rgb :user-bubble-bg))
(def user-bubble-fg    (rgb :user-bubble-fg))
(def user-role-fg      (rgb :user-role-fg))
(def turn-separator-bg (rgb :turn-separator-bg))
(def turn-separator-fg (rgb :turn-separator-fg))

;; Chat messages — assistant
(def ai-bubble-bg      (rgb :ai-bubble-bg))
(def ai-bubble-fg      (rgb :ai-bubble-fg))
(def ai-role-fg        (rgb :ai-role-fg))

;; Status indicators
(def status-ok      (rgb :status-ok))
(def status-bad     (rgb :status-bad))
(def warning-bg     (rgb :warning-bg))
(def warning-fg     (rgb :warning-fg))
(def warning-border (rgb :warning-border))
(def cancelled-bg   (rgb :cancelled-bg))
(def cancelled-fg   (rgb :cancelled-fg))

;; Code block styling
(def code-block-bg     (rgb :code-block-bg))
(def code-ok-bg        (rgb :code-ok-bg))
(def code-err-bg       (rgb :code-err-bg))
(def code-block-fg     (rgb :code-block-fg))
(def code-success-fg   (rgb :code-success-fg))
(def code-error-fg     (rgb :code-error-fg))
(def code-duration-fg  (rgb :code-duration-fg))
(def code-result-fg    (rgb :code-result-fg))
(def code-error-result-fg (rgb :code-error-result-fg))
(def code-syntax-special-fg (rgb :code-syntax-special-fg))
(def code-syntax-keyword-fg (rgb :code-syntax-keyword-fg))
(def code-syntax-string-fg  (rgb :code-syntax-string-fg))
(def code-syntax-number-fg  (rgb :code-syntax-number-fg))
(def code-syntax-comment-fg (rgb :code-syntax-comment-fg))
(def code-border-fg    (rgb :code-border-fg))
(def stdout-bg         (rgb :stdout-bg))
(def stdout-fg         (rgb :stdout-fg))
(def stdout-label-fg   (rgb :stdout-label-fg))
(def stdout-sep-fg     (rgb :stdout-sep-fg))
(def iteration-header-fg    (rgb :iteration-header-fg))
(def iteration-header-bg    (rgb :iteration-header-bg))
(def answer-sep-fg     (rgb :answer-sep-fg))
(def answer-sep-bg     (rgb :answer-sep-bg))
(def answer-bg         (rgb :answer-bg))
(def answer-fg         (rgb :answer-fg))

;; Markdown
(def md-h1-fg          (rgb :md-h1-fg))
(def md-h2-fg          (rgb :md-h2-fg))
(def md-h3-fg          (rgb :md-h3-fg))
(def confidence-fg     (rgb :confidence-fg))
(def md-summary-bg     (rgb :md-summary-bg))
(def md-summary-fg     (rgb :md-summary-fg))
(def th-md-summary-bg  (rgb :th-md-summary-bg))
(def th-md-summary-fg  (rgb :th-md-summary-fg))
(def proof-summary-bg     (rgb :proof-summary-bg))
(def proof-summary-fg     (rgb :proof-summary-fg))
(def th-proof-summary-bg  (rgb :th-proof-summary-bg))
(def th-proof-summary-fg  (rgb :th-proof-summary-fg))

;; Link chrome
(def link-chrome-fg       (rgb :link-chrome-fg))
(def link-chrome-arrow-fg (rgb :link-chrome-arrow-fg))
(def link-chrome-url-fg   (rgb :link-chrome-url-fg))
(def link-chrome-hover-bg (rgb :link-chrome-hover-bg))
(def link-chrome-hover-fg (rgb :link-chrome-hover-fg))
(def link-chrome-blocked-fg (rgb :link-chrome-blocked-fg))

;; Footer
(def footer-fg          (rgb :footer-fg))
(def footer-fg-muted    (rgb :footer-fg-muted))
(def footer-fg-strong   (rgb :footer-fg-strong))
(def footer-spinner-fg  (rgb :footer-spinner-fg))
(def footer-warning-fg  (rgb :footer-warning-fg))
(def footer-error-fg    (rgb :footer-error-fg))

;; Padding
(def pad-x (:pad-x default-spacing))
