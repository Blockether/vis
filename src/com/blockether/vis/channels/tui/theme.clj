(ns com.blockether.vis.channels.tui.theme
  (:import [com.googlecode.lanterna TextColor$RGB]))

;;; ── Light theme — clean, high-contrast ─────────────────────────────────────

;; Terminal
(def terminal-bg    (TextColor$RGB. 255 255 255))  ;; pure white
(def text-fg        (TextColor$RGB. 30 30 30))     ;; near-black

;; Boxes (messages + input)
(def box-bg         (TextColor$RGB. 255 255 255))  ;; white
(def box-fg         (TextColor$RGB. 30 30 30))     ;; near-black
(def border-fg      (TextColor$RGB. 80 80 80))     ;; dark gray border — very visible

;; Dialog
(def dialog-bg       (TextColor$RGB. 248 248 248))  ;; very light gray
(def dialog-fg       (TextColor$RGB. 30 30 30))
(def dialog-title-fg (TextColor$RGB. 255 255 255))  ;; white text on accent bar
(def dialog-title-bg (TextColor$RGB. 60 60 60))     ;; dark gray accent bar
(def dialog-border   (TextColor$RGB. 120 120 120))  ;; solid gray border
(def dialog-shadow   (TextColor$RGB. 200 200 200))  ;; subtle shadow
(def dialog-hint     (TextColor$RGB. 120 120 120))  ;; muted hint text
(def dialog-hint-key (TextColor$RGB. 50 50 50))     ;; near-black key labels

;; Chat bubbles — user (white bg, just bordered)
(def user-bubble-bg    (TextColor$RGB. 255 255 255))  ;; white — same as terminal
(def user-bubble-fg    (TextColor$RGB. 30 30 30))     ;; near-black text
(def user-bubble-border (TextColor$RGB. 120 120 120)) ;; dark gray border
(def user-role-fg      (TextColor$RGB. 80 80 80))     ;; dark gray "you" label

;; Chat bubbles — assistant (white bg, just bordered)
(def ai-bubble-bg      (TextColor$RGB. 255 255 255))  ;; white — same as terminal
(def ai-bubble-fg      (TextColor$RGB. 30 30 30))     ;; near-black text
(def ai-bubble-border  (TextColor$RGB. 130 130 130))  ;; dark gray border
(def ai-role-fg        (TextColor$RGB. 80 160 80))    ;; green "vis" label

;; Status indicators
(def status-ok  (TextColor$RGB. 40 160 60))    ;; green
(def status-bad (TextColor$RGB. 220 50 50))    ;; red

;; Code block styling
(def code-block-bg     (TextColor$RGB. 242 242 242))  ;; light gray background for code
(def code-block-fg     (TextColor$RGB. 30 30 30))     ;; near-black text in code
(def code-success-fg   (TextColor$RGB. 40 160 60))    ;; green ✓ marker
(def code-error-fg     (TextColor$RGB. 220 50 50))    ;; red ✗ marker
(def code-duration-fg  (TextColor$RGB. 140 140 140))  ;; muted duration text
(def code-result-fg    (TextColor$RGB. 80 80 80))     ;; dim result text
(def code-error-result-fg (TextColor$RGB. 200 60 60)) ;; red-ish error result text
(def code-border-fg    (TextColor$RGB. 200 200 200))  ;; subtle border for code section
(def stdout-bg         (TextColor$RGB. 248 245 240))  ;; warm off-white for stdout
(def stdout-fg         (TextColor$RGB. 90 90 90))     ;; dim text in stdout
(def stdout-label-fg   (TextColor$RGB. 160 160 160))  ;; muted "stdout" label
(def stdout-sep-fg     (TextColor$RGB. 210 210 210))  ;; light separator in stdout
(def iter-header-fg    (TextColor$RGB. 100 100 100))  ;; iteration header
(def iter-header-bg    (TextColor$RGB. 235 235 235))  ;; iteration header background

;; Padding
(def pad-x 1)
