(ns com.blockether.vis.tui.theme
  (:import [com.googlecode.lanterna TextColor$RGB]))

;;; ── Light theme based on blockether.com (#fdf4e5) ──────────────────────────

;; Terminal
(def terminal-bg    (TextColor$RGB. 253 244 229))  ;; #fdf4e5 — blockether cream
(def text-fg        (TextColor$RGB. 45 42 38))     ;; warm dark brown

;; Boxes (messages + input share the same look)
(def box-bg         (TextColor$RGB. 253 244 229))  ;; same as terminal
(def box-fg         (TextColor$RGB. 45 42 38))     ;; warm dark brown
(def border-fg      (TextColor$RGB. 160 146 126))  ;; warm tan

;; Dialog
(def dialog-bg       (TextColor$RGB. 245 236 220))  ;; slightly darker cream
(def dialog-fg       (TextColor$RGB. 45 42 38))
(def dialog-title-fg (TextColor$RGB. 253 244 229))  ;; cream text on accent bar
(def dialog-title-bg (TextColor$RGB. 120 90 50))    ;; warm brown accent bar
(def dialog-border   (TextColor$RGB. 130 118 100))
(def dialog-shadow   (TextColor$RGB. 210 196 176))  ;; warm subtle shadow
(def dialog-hint     (TextColor$RGB. 170 158 140))  ;; very faint hint action text
(def dialog-hint-key (TextColor$RGB. 100 78 50))   ;; stronger brown for key labels (Enter, Esc, ↑/↓)

;; Chat bubbles
(def user-bubble-bg    (TextColor$RGB. 230 220 200))  ;; warm tan bubble
(def user-bubble-fg    (TextColor$RGB. 45 42 38))     ;; dark brown text
(def user-bubble-border (TextColor$RGB. 180 166 140)) ;; tan border
(def user-role-fg      (TextColor$RGB. 120 90 50))    ;; brown "you" label

(def ai-bubble-bg      (TextColor$RGB. 245 240 230))  ;; lighter cream bubble
(def ai-bubble-fg      (TextColor$RGB. 45 42 38))     ;; dark brown text
(def ai-bubble-border  (TextColor$RGB. 170 158 138))  ;; warm border
(def ai-role-fg        (TextColor$RGB. 80 110 80))    ;; muted green "ai" label

;; Status indicators
(def status-ok  (TextColor$RGB. 60 140 60))   ;; green dot — configured
(def status-bad (TextColor$RGB. 180 60 50))    ;; red dot — missing key

;; Padding
(def pad-x 1)
