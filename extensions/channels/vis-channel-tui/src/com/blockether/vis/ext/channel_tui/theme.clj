(ns com.blockether.vis.ext.channel-tui.theme
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
(def cancelled-bg (TextColor$RGB. 240 240 240))  ;; soft gray — "this turn was aborted" zone
(def cancelled-fg (TextColor$RGB. 110 110 110))  ;; muted gray text on the cancelled bg

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
(def iteration-header-fg    (TextColor$RGB. 170 170 170))  ;; iteration header label (subtle)
(def iteration-header-bg    (TextColor$RGB. 244 244 244))  ;; iteration zone background
(def answer-sep-fg     (TextColor$RGB. 190 190 190))  ;; answer separator line
(def answer-sep-bg     (TextColor$RGB. 250 250 250))  ;; answer separator background
;; Final answer zone background.
;;   1st pass: (224 235 252) — saturated blue, dominated the bubble.
;;   2nd pass: (247 250 254) — barely-tinted, still visible.
;;   3rd pass (current, per user request): identical to terminal-bg
;;     so there is NO visual answer-zone background at all. The
;;     answer is distinguished by the optional `FINAL ANSWER`
;;     header glyph + heading colours, NOT by a bg block. Cleaner
;;     read when answers are the bulk of the bubble.
;; Kept as a separate `def` (rather than aliased to `terminal-bg`)
;; so a future revert is a single-line theme tweak with no source
;; surgery; every painter still uses `t/answer-bg` and just sees
;; white today.
(def answer-bg         (TextColor$RGB. 255 255 255))  ;; identical to terminal-bg — no zone bg
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

;; <details><summary> disclosure label band. The TUI doesn't model
;; click-to-collapse (yet), so the summary line carries the WHOLE
;; visual weight of "this is a disclosure section" — it needs to
;; read as a distinct band, not just a bold paragraph. Pale lavender
;; was chosen because:
;;   - it's far enough from `code-block-bg` (light blue-gray) and
;;     `warning-bg` (soft yellow) that the eye doesn't confuse the
;;     three zone tints;
;;   - the saturation is low enough to sit quietly on white
;;     assistant bg without screaming.
;; WCAG ratio fg/bg ≈ 9.5 (AAA).
(def md-summary-bg     (TextColor$RGB. 240 235 250))  ;; pale lavender band
(def md-summary-fg     (TextColor$RGB. 70 40 130))    ;; deep violet — high-contrast text
;; Thinking-mode summary keeps the iteration-header tint family so
;; the disclosure stays inside the dim reasoning zone instead of
;; popping out of it. Slightly cooler / darker than the surrounding
;; iteration-header-bg so the band is still legible against it.
(def th-md-summary-bg  (TextColor$RGB. 232 230 240))  ;; cool gray-lavender, one notch off iteration-header-bg
(def th-md-summary-fg  (TextColor$RGB. 90 80 130))    ;; muted violet on the dim band

;; Clickable link / image / file-link chrome painted at the foot of
;; an assistant bubble. Three states:
;;
;;   normal   — enabled, not hovered. Reads as a quiet hyperlink.
;;   hover    — mouse cursor is over the row. Subtle bg fill +
;;              brighter fg so the click affordance is unambiguous.
;;   blocked  — ref's scheme is rejected (javascript:, ..-escape
;;              etc.). Painted dim + struck-through so the user
;;              sees “this is here but I won’t open it.”
;;
;; The hover bg is intentionally pale (no aggressive solid fill)
;; because mouse-mode terminals already invert text selection on
;; the row underneath the cursor; piling more colour on top makes
;; the highlight read as a glitch. WCAG ratio on hover-bg vs
;; link-chrome-fg holds at >= 7.0 (AAA).
(def link-chrome-fg       (TextColor$RGB. 30 90 200))   ;; classic underline-blue link colour
(def link-chrome-arrow-fg (TextColor$RGB. 130 130 130)) ;; the " → " separator between text + url
(def link-chrome-url-fg   (TextColor$RGB. 90 110 140))  ;; muted url tail
(def link-chrome-hover-bg (TextColor$RGB. 232 240 252)) ;; pale blue hover band
(def link-chrome-hover-fg (TextColor$RGB. 10 50 160))   ;; deeper blue on hover for stronger contrast
(def link-chrome-blocked-fg (TextColor$RGB. 170 170 170)) ;; muted gray for rejected entries

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
