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

;; Chat messages — user (warm light-yellow block, high-contrast text)
;;
;; Distinct from `warning-bg` (which is more saturated and means "pay
;; attention, something went wrong"). User-bubble is a pale,
;; paper-like tint so user input reads as its own zone without
;; competing with warnings or with the white assistant area.
;; Foreground is a near-black warm tone so the WCAG luminance ratio
;; against the yellow stays above 12:1 — readable in any terminal.
(def user-bubble-bg    (TextColor$RGB. 254 248 215))  ;; very pale warm yellow — "user said this" zone
(def user-bubble-fg    (TextColor$RGB. 25 22 5))      ;; near-black warm — high contrast on the yellow
(def user-role-fg      (TextColor$RGB. 130 90 0))     ;; bold amber accent for the "You" label

;; Chat messages — assistant (no background fill, terminal bg shows through)
(def ai-bubble-bg      (TextColor$RGB. 255 255 255))  ;; white — same as terminal
(def ai-bubble-fg      (TextColor$RGB. 30 30 30))     ;; near-black text
(def ai-role-fg        (TextColor$RGB. 80 160 80))    ;; green "vis" label

;; Status indicators
(def status-ok  (TextColor$RGB. 40 160 60))    ;; green
(def status-bad (TextColor$RGB. 220 50 50))    ;; red
(def warning-bg (TextColor$RGB. 255 245 180))  ;; soft yellow warning background
(def warning-fg (TextColor$RGB. 80 60 0))      ;; dark amber warning text
(def warning-border (TextColor$RGB. 190 150 40)) ;; amber warning border

;; Code block styling
(def code-block-bg     (TextColor$RGB. 240 243 248))  ;; light blue-gray — code blocks
(def code-err-bg       (TextColor$RGB. 253 235 235))  ;; very light red — failed code only
(def code-block-fg     (TextColor$RGB. 30 30 30))     ;; near-black text in code
(def code-success-fg   (TextColor$RGB. 40 160 60))    ;; green ✓ marker
(def code-error-fg     (TextColor$RGB. 220 50 50))    ;; red ✗ marker
(def code-duration-fg  (TextColor$RGB. 130 130 130))  ;; muted duration text
(def code-result-fg    (TextColor$RGB. 70 70 70))     ;; dim result text
(def code-error-result-fg (TextColor$RGB. 180 40 40)) ;; red result text (on red bg)
(def code-border-fg    (TextColor$RGB. 90 95 110))    ;; darker neutral — table borders / code-section dividers (heavy box-drawing chars need real contrast)
(def stdout-bg         (TextColor$RGB. 247 244 238))  ;; warm beige — stdout output
(def stdout-fg         (TextColor$RGB. 80 80 80))     ;; dim text in stdout
(def stdout-label-fg   (TextColor$RGB. 155 155 155))  ;; muted "stdout" label
(def stdout-sep-fg     (TextColor$RGB. 210 205 195))  ;; separator in stdout (warm)
(def iter-header-fg    (TextColor$RGB. 170 170 170))  ;; iteration header label (subtle)
(def iter-header-bg    (TextColor$RGB. 244 244 244))  ;; iteration zone background
(def answer-sep-fg     (TextColor$RGB. 190 190 190))  ;; answer separator line
(def answer-sep-bg     (TextColor$RGB. 250 250 250))  ;; answer separator background
;; Final answer zone background. Was previously (224 235 252) — a
;; clearly-blue tint that user feedback flagged as overpowering when
;; the answer is the bulk of the bubble. Dialed back to a barely-
;; perceptible cool tint that still distinguishes the answer zone
;; from the assistant white area / code blocks but no longer competes
;; with the heading colours or the inline code highlight. WCAG
;; contrast for `answer-fg` (near-black) on this bg is > 18:1, so
;; readability is unchanged.
(def answer-bg         (TextColor$RGB. 247 250 254))  ;; subtle cool tint — 'this is the answer' without screaming it
(def answer-fg         (TextColor$RGB. 25 25 25))     ;; near-black answer text

;; Markdown heading colours (answer-mode H1/H2/H3). Pre-fix headings
;; were near-black + bold, indistinguishable from body text + bold.
;; Now a saturated GOLD/AMBER gradient that pops on both the white
;; assistant background AND the new pale-blue answer-bg. WCAG ratios
;; on white: H1 ≈ 4.8, H2 ≈ 6.2, H3 ≈ 8.4 — all pass AA, H3 passes
;; AAA. Hierarchy reads strongest → most muted as you descend H1→H3,
;; matching what `glow` / `mdcat` do (and what every prose stylesheet
;; on the planet does).
;; Tuned to clear WCAG AA (>= 4.5:1) on BOTH white and the dialed-back
;; answer-bg — the heading-colours-test pins both surfaces. The
;; previous (184 124 0) hit only 3.5:1 on white, which counts as AA
;; for large/bold text but the bubble renderer doesn't enlarge the
;; glyph, only bolds it, so we hold to the stricter normal-text bar.
(def md-h1-fg          (TextColor$RGB. 150 100 0))    ;; rich amber/gold — H1 is the loudest
(def md-h2-fg          (TextColor$RGB. 125 80 0))     ;; deeper amber — H2 a step quieter
(def md-h3-fg          (TextColor$RGB. 100 65 0))     ;; deep bronze — H3 is the most muted of the three
(def confidence-fg     (TextColor$RGB. 140 140 140))  ;; muted confidence label

;; Footer (dedicated row below the input box)
;; Codex-style three-region status: left=identity, center=run-state, right=budget.
;; Colors are tuned to match the existing muted palette — the footer should
;; sit quietly until ctx pressure or an active turn pulls the eye to it.
(def footer-fg          (TextColor$RGB. 60 60 60))    ;; default text
(def footer-fg-muted    (TextColor$RGB. 140 140 140)) ;; separators, low-priority segments, cost
(def footer-fg-strong   (TextColor$RGB. 30 30 30))    ;; model name (bold)
(def footer-spinner-fg  (TextColor$RGB. 80 160 80))   ;; running spinner — same green as ai-role-fg
(def footer-warning-fg  (TextColor$RGB. 180 110 0))   ;; ctx ≤ 30% left, cancelling…
(def footer-error-fg    (TextColor$RGB. 200 40 40))   ;; ctx ≤ 10% left — red, bold

;; Padding
(def pad-x 1)
