(ns com.blockether.vis.channels.tui.theme
  (:import [com.googlecode.lanterna TextColor$RGB]))

;;; ── Light theme — clean, high-contrast ─────────────────────────────────────

;; Terminal
(def terminal-bg    (TextColor$RGB. 255 255 255))  ;; pure white
(def text-fg        (TextColor$RGB. 30 30 30))     ;; near-black

;; Boxes (messages + input)
(def box-bg         (TextColor$RGB. 255 255 255))  ;; white
(def box-fg         (TextColor$RGB. 30 30 30))     ;; near-black
(def border-fg      (TextColor$RGB. 140 140 140))  ;; visible mid-gray border

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
(def user-bubble-border (TextColor$RGB. 160 160 160)) ;; visible gray border
(def user-role-fg      (TextColor$RGB. 80 80 80))     ;; dark gray "you" label

;; Chat bubbles — assistant (white bg, just bordered)
(def ai-bubble-bg      (TextColor$RGB. 255 255 255))  ;; white — same as terminal
(def ai-bubble-fg      (TextColor$RGB. 30 30 30))     ;; near-black text
(def ai-bubble-border  (TextColor$RGB. 170 170 170))  ;; visible gray border
(def ai-role-fg        (TextColor$RGB. 80 160 80))    ;; green "vis" label

;; Status indicators
(def status-ok  (TextColor$RGB. 40 160 60))    ;; green
(def status-bad (TextColor$RGB. 220 50 50))    ;; red

;; Padding
(def pad-x 1)
