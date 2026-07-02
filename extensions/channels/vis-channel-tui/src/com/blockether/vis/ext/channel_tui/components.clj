(ns com.blockether.vis.ext.channel-tui.components
  "Reusable header/tab UI components.

   Each drawable here is a self-contained fn that paints into a Lanterna
   `TextGraphics` and — when asked — registers its OWN click region, so the
   same widget can't drift between call sites (the bug that left the tab
   close `✕` painted in one place and made clickable in another, or not at
   all). Two kinds live here:

     - text layout helpers — `truncate-with-ellipsis`, `center-padded`
     - drawable components — `close-button!`, `tab-cell!`

   Components own their visual contract AND their interaction contract
   (the click region's `:kind`), keeping `header.clj` a thin layout caller."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.header :as vh]
            [com.blockether.vis.internal.format :as fmt]))
;; ── text layout ─────────────────────────────────────────────────────────────
(defn truncate-with-ellipsis
  "Truncate `s` so its display width fits in `max-cols`. When truncation
   actually happens, append `vh/workspace-ellipsis` so overflow is visible."
  ^String [s ^long max-cols]
  (let [s (or s "")
        ;; Reserve the ellipsis's ACTUAL display width, not 1: `…` (U+2026) is
        ;; EAW=A and renders TWO columns on ambiguous-wide terminals, so a fixed
        ;; `(dec max-cols)` over-ran the cell by a column.
        ew (long (p/display-width vh/workspace-ellipsis))]
    (cond (<= max-cols 0) ""
      (<= (p/display-width s) max-cols) s
      (<= max-cols ew) (p/truncate-cols vh/workspace-ellipsis max-cols)
      :else (str (p/truncate-cols s (- max-cols ew)) vh/workspace-ellipsis))))
(defn center-padded
  "Centre `s` inside a `cell-w`-wide cell with `vh/tab-entry-padding`
   reserved on each side; ellipsises overflow."
  ^String [s ^long cell-w]
  (let [inner (max 0 (- cell-w (* 2 (long vh/tab-entry-padding))))
        text (truncate-with-ellipsis s inner)
        text-w (p/display-width text)
        pad-total (max 0 (- cell-w text-w))
        left (quot pad-total 2)
        right (- pad-total left)]
    (str (apply str (repeat left \space)) text (apply str (repeat right \space)))))
;; ── close button ────────────────────────────────────────────────────────────
(def ^{:const true} close-button-width
  "Cells a `close-button!` occupies: a leading space + 1-col `✕` + a\n  trailing space. No divider — the bare ✕ reads as a button, not `|x`."
  3)
(def ^{:private true} close-button-glyph " ✕ ")
(defn close-button!
  "Draw a ` ✕ ` close affordance (space-padded ✕, no divider) at (col,row).
   At rest the ✕ sits on its OWN chip — the shared `button-fg`/`button-bg`
   pair every other TUI button uses at rest — so it reads as a SEPARATE
   button pinned to the tab's right edge instead of blending into the tab
   surface (a gray ✕ vanished on the inactive near-white tab; a white ✕
   melted into the active blue tab). The dark `button-fg` glyph stays legible
   on the light chip, and the chip's contrast against BOTH tab backgrounds is
   the affordance.
   On hover the cap escalates to a red pill (`close-button-hover-fg` bg behind
   a white `header-active-tab-fg` ✕) to signal the destructive click.
   Registers its `:close-tab` click region for `workspace-id`. Returns the
   consumed width (`close-button-width`)."
  [g col row _tab-fg _tab-bg workspace-id register?]
  (let [hovered (cr/hovered)
        hovered? (and (= :close-tab (:kind hovered)) (= workspace-id (:workspace-id hovered)))]
    (p/clear-styles! g)
    (p/set-colors! g
      (if hovered? t/header-active-tab-fg t/button-fg)
      (if hovered? t/close-button-hover-fg t/button-bg))
    (p/enable! g p/BOLD)
    (p/put-str! g col row close-button-glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width close-button-width},
                     :kind :close-tab,
                     :workspace-id workspace-id,
                     :text workspace-id,
                     :enabled? true}))
    close-button-width))
(defn button!
  "Generic clickable button: paint `label` (already space-padded, e.g. \" < \") as
   a filled accent cap at (col,row) — brighter on hover — AND register its click
   region under `:kind`, plus any identity keys in `extra` (merged into both the
   region and the hover match). One widget owns its look, its hover, and its
   click, so they can't drift apart. Returns the consumed width.

   Hover is detected by matching the registered `:kind` and every `extra` key
   against `cr/hovered`, so two buttons of the same kind but different ids stay
   independent."
  ([g col row label kind] (button! g col row label kind nil))
  ([g col row label kind {:keys [extra danger? accent? register?] :or {register? true}}]
   (let [w        (long (p/display-width label))
         hov      (cr/hovered)
         hovered? (and (= kind (:kind hov))
                    (every? (fn [[k v]] (= v (get hov k))) extra))]
     (p/clear-styles! g)
     ;; ONE button language across the whole TUI — the SAME chip every modal's
     ;; ✕ uses (`dialog-close-button!`): an inverted title-strip cap at rest, and
     ;; on hover it lifts to the accent (or the destructive red for `:danger?`
     ;; actions like close), always bold. So find-bar and dialog buttons read as
     ;; the same control instead of two palettes. `:accent?` is the exception: a
     ;; PRIMARY/CTA chip (the `+` new-workspace button). `header-active-tab-accent`
     ;; is the SAME blue as the active tab bg, so a white-on-accent chip vanished
     ;; against the tabs and its inverted hover became a blank WHITE block. Instead
     ;; the `+` rests as a GREEN create pill (white glyph on `code-success-fg`) that
     ;; contrasts every tab, and on hover DARKENS to a solid deep-blue fill
     ;; (`header-hover-fg`) — a clear, always-visible state change, never white.
     (cond
       (and hovered? danger?) (p/set-colors! g t/header-active-tab-fg t/close-button-hover-fg)
       (and hovered? accent?) (p/set-colors! g t/header-active-tab-fg t/header-hover-fg)
       accent?                (p/set-colors! g t/header-active-tab-fg t/code-success-fg)
       hovered?               (p/set-colors! g t/header-active-tab-fg t/header-active-tab-accent)
       :else                  (p/set-colors! g t/button-fg t/button-bg))
     (when (or hovered? accent?) (p/enable! g p/BOLD))
     (p/put-str! g col row label)
     (p/clear-styles! g)
     (when register?
       (cr/register! (merge {:bounds {:row row, :col col, :width w}, :kind kind, :enabled? true}
                       extra)))
     w)))

(def ^:private find-bar-buttons
  "Trailing buttons of the find bar: [click-kind glyph-label]. Padded GLYPHS —
   ◀ ▶ (narrow geometric) and ✕ (1-cell, as `close-button!` uses it). Spaced
   apart by `:gap` ops in `find-bar!`."
  [[:search-prev " ◀ "] [:search-next " ▶ "] [:search-close " ✕ "]])

(def ^:private find-input-width
  "Cells reserved for the white query field, so the box doesn't jitter as you
   type."
  22)

(defn find-bar!
  "Browser-style in-session find WIDGET: a single-line BORDERED box, right-aligned at the top of the messages area, holding a WHITE input field (the live query), the i/N match count, and spaced Aa/◀/▶/✕ glyph buttons (each its own click region via `button!`, so the mouse drives the same `:search-*` events as C-p / C-n). `search` is app-db's `:search` map; no-op when inactive."
  [g cols text-top {:keys [active? query hits index case? total]}]
  (when active?
    (let [n     (long (count hits))
          q     (str query)
          qshow (if (str/blank? q) "type to search…" q)
          ;; The white input field carries its OWN inner padding — a space each
          ;; side on input-field-bg — so the query text isn't jammed against the
          ;; field edge. That's padding the box border can't give.
          qfield (let [s (truncate-with-ellipsis qshow find-input-width)]
                   (str " " s
                     (apply str (repeat (max 0 (- find-input-width (long (p/display-width s)))) \space))
                     " "))
          cnt   (format " %-9s" (cond (str/blank? q) ""
                                  (zero? n)      "0/0"
                                  :else          (str (inc (long (or index 0))) "/" n
                                                   (when (> (long (or total n)) n)
                                                     (str " (" total ")")))))
          ;; content ops. :input is the white field; :chrome/:gap ride the box bg;
          ;; :btn delegates to the reusable button! widget.
          ;; Case chip: " ABC " = case-sensitive ON, " abc " = off. The label itself
          ;; demonstrates the mode — uppercase shows it cares about case.
          btns  (cons [:search-case (if case? " ABC " " abc ")] find-bar-buttons)
          ops   (concat
                  [[:chrome " "] [:input qfield] [:chrome "  "] [:chrome cnt] [:chrome " "]]
                  (interpose [:gap " "] (map (fn [[k l]] [:btn k l]) btns))
                  [[:chrome " "]])
          content-w (long (reduce + (map (fn [op] (long (p/display-width (last op)))) ops)))
          box-w (+ content-w 2)
          box-l (max 0 (- (long cols) box-w 2))
          box-t (long text-top)
          row   (inc box-t)
          x0    (inc box-l)]
      ;; box: fill bg, then single-line border
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/fill-rect! g box-l box-t box-w 3)
      (p/draw-box! g box-l box-t box-w 3)
      ;; content row (vertically centred between the borders)
      (reduce
        (fn [x op]
          (case (first op)
            :btn   (let [kind (nth op 1)]
                     (+ x (button! g x row (nth op 2) kind
                            (when (= kind :search-close) {:danger? true}))))
            ;; input field — the canonical `box-fg` on `input-field-bg`, the same
            ;; interior `draw-text-input-field!` paints, so every input matches.
            :input (do (p/clear-styles! g)
                     (p/set-colors! g t/box-fg t/input-field-bg)
                     (p/put-str! g x row (last op))
                     (+ x (long (p/display-width (last op)))))
            ;; chrome / gaps ride the dialog body palette.
            (do (p/clear-styles! g)
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/put-str! g x row (last op))
              (+ x (long (p/display-width (last op)))))))
        x0
        ops))))

;; (The header help/search chips are painted inline by `header.clj` — see its
;; right-slot cluster — so there's no shared chip def here.)

(def ^{:private true} tab-divider-glyph
  ;; U+250A LIGHT QUADRUPLE DASH VERTICAL — a soft, dotted separator that
  ;; reads gentler than a solid │ between tabs / cluster chips. Box-drawing
  ;; (EAW=A → one column in target terminals, same class as the borders).
  "┊")
(defn tab-divider!
  "Paint a 1-col dotted `┊` divider (between tabs, or between header-cluster
   chips) at (col,row). Painted in the high-contrast accent (dark on a light
   terminal, light on a dark one) so the sparse dotted glyph is actually
   visible — `footer-fg` washed it out."
  [g col row]
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg)
  (p/put-str! g col row tab-divider-glyph)
  (p/clear-styles! g))
;; ── tab cell ────────────────────────────────────────────────────────────────
(defn- tab-border-cols
  "Column offsets 0..width-1 to underline for a `:ready` tab — the WHOLE
   width, so the caller draws one steady, solid green \"ready to check\" line.
   Running tabs no longer use a per-column mask (they get an animated sweep,
   see `running-border!`); idle tabs (`nil`) get nothing."
  [status ^long width]
  (case status
    :ready (range width)
    nil))

(defn- running-border!
  "Paint the RUNNING tab's bottom border: a CONTINUOUS underline (no gaps — it
   runs under the label too) carrying a single bright amber band that sweeps
   left→right and restarts at the right edge. Continuous coverage + one moving
   highlight reads as smooth forward motion, unlike the old every-other-column
   parity mask that inverted the WHOLE line each tick (spaces + flicker).

   ONE uniform mechanism for EVERY column: `underline-cell!` folds an SGR
   UNDERLINE into the already-painted cell and recolours its foreground — dim
   amber (`warning-fg`) as the steady base, bright amber (`warning-border`)
   for the cells the sweeping band currently covers. The SAME underline shape
   sits under blank padding AND under the number/label glyphs (their text just
   tints amber as the band passes), so the line has ONE consistent weight and
   position — no `▁` block vs SGR-line mismatch, no per-character gaps.

   Phase is off the wall clock, so the running turn's own repaints animate it
   with no extra timer."
  [g ^long left row ^long width]
  (let [phase (quot (System/currentTimeMillis) 110)
        band  (max 3 (quot width 3))
        head  (long (mod phase (+ width band)))
        lo    (max 0 (- head band))
        hi    (min width head)]
    (doseq [c (range width)]
      (let [x       (+ left (long c))
            in-band? (and (>= (long c) lo) (< (long c) hi))]
        (p/underline-cell! g x row (if in-band? t/warning-border t/warning-fg))))))

(defn tab-cell!
  "Draw one workspace tab into the band at [left,row] spanning `width` cells:

     - the bg slab (active = inverted bold; inactive = dim italic);
     - a STATUS BORDER painted as a per-column UNDERLINE along the bottom of
       the cell (this replaced the old 2-col status prefix / edge rails, so the
       number now sits FLUSH \u2014 no leading gap). The border is all theme
       colour (no block glyphs, nothing to clash with the surface) and IS the
       status cue:
         :running -> a CONTINUOUS amber underline with a bright band sweeping
                     left→right along the bottom edge (the \"working\" crawl,
                     smooth — no gaps, no flicker);
         :ready   -> the whole cell glows GREEN (`status-ok`) under a steady,
                     solid underline \u2014 the \"ready to check\" outline;
         nil      -> NO border \u2014 an idle tab keeps the plain surface
                     background and blends in;
     - the centered `<num> | <label>`, given `close-button-width` fewer cells
       so it never collides with the \u2715;
     - an ALWAYS-VISIBLE `close-button!` pinned to the right edge (when the
       cell is wide enough to host both a title and the button).

   Registers the cell's `:workspace-entry` region FIRST, then the \u2715
   `:close-tab` region ON TOP, so a click on the glyph wins the topmost
   (last-registered) lookup and closes the tab instead of selecting it.

   `opts` keys: :left :row :width :label :tab-no :status :active? :workspace-id
   :index :register? :closable?"
  [g
   {:keys [left row width label tab-no status active? workspace-id index register? closable?],
    :or {closable? true}}]
  (let [width (long width)
        left (long left)
        show-close? (and closable? (>= width (+ close-button-width 3)))
        inner-w (if show-close? (max 0 (- width close-button-width)) width)
        ready? (= status :ready)
        ;; A ready tab glows green (border + label); otherwise the normal
        ;; active / inactive theme fg carries the underline border.
        fg (cond
             ready?  t/status-ok
             active? t/header-active-tab-fg
             :else   t/border-fg)
        bg (if active? t/header-active-tab-bg t/dialog-bg)
        ;; The per-column status border (a dashed/steady/none underline mask)
        ;; is painted LAST, after the cell is fully drawn \u2014 see below.
        num-str (when tab-no (str tab-no))
        display (if num-str (str num-str " | " label) label)
        text (center-padded display inner-w)
        ;; Where the tab number lands once `display` is centre-padded, so we can
        ;; repaint just those digits in a contrasting colour.
        lead (count (take-while #(= \space %) text))
        num-fg (cond
                 ready?  t/status-ok
                 active? t/header-tab-number-fg
                 :else   t/header-active-tab-accent)]
    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (p/enable! g (if active? p/BOLD p/ITALIC))
    (p/fill-rect! g left row width 1)
    (p/put-str! g left row text)
    (when (and num-str
            (<= (+ lead (count num-str)) (count text))
            (= num-str (subs text lead (+ lead (count num-str)))))
      (p/clear-styles! g)
      (p/set-colors! g num-fg bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (+ left lead) row num-str))
    (when (and num-str (<= (+ lead (count num-str) 3) (count text)))
      (p/clear-styles! g)
      (p/set-colors! g fg bg)
      (when active? (p/enable! g p/BOLD))
      (p/put-str! g (+ left lead (count num-str) 1) row "|"))
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col left, :width width},
                     :kind :workspace-entry,
                     :index index,
                     :workspace-id workspace-id,
                     :text workspace-id,
                     :enabled? true}))
    (when show-close?
      (close-button! g
        (+ left inner-w)
        row
        fg
        bg
        workspace-id
        register?))
    ;; Status border LAST, over the fully painted cell:
    ;;   :ready   -> fold a steady UNDERLINE into every cell (solid green line);
    ;;   :running -> `running-border!` — a continuous dim amber underline with a
    ;;               bright `warning-border` band sweeping across it (smooth,
    ;;               gapless, animated by the running turn's own repaints);
    ;;   nil      -> nothing.
    (case status
      :ready   (doseq [c (tab-border-cols status width)]
                 (p/underline-cell! g (+ left (long c)) row))
      :running (running-border! g left row width)
      nil)))
;; ── inert title ─────────────────────────────────────────────────────────────
(defn title!
  "Paint an INERT, centered header title in [left,row] over `width` cells —
   bold on the header surface, ellipsised on overflow. Registers no click
   region: a single-workspace title is not a switcher and must not look or
   behave like one."
  [g row left width text]
  (when (pos? (long width))
    (let [shown (truncate-with-ellipsis text width)
          w (p/display-width shown)
          col (+ (long left) (max 0 (quot (- (long width) w) 2)))]
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g col row shown)
      (p/clear-styles! g))))
;; ── nav arrow ───────────────────────────────────────────────────────────────
(defn nav-arrow!
  "Paint a workspace-overflow navigation arrow `glyph` at (col,row) as a FILLED
   BUTTON CHIP - routed through the shared `button!`, so the < > read as the
   SAME control as the F1/F2 and id-copy buttons (inverted cap at rest, lifts to
   the accent + bold on hover) instead of a bare character. Registers its
   `:workspace-entry` click region keyed by `direction` (`:prev` / `:next`), so a
   click still cycles the visible tab window."
  [g row col glyph direction register?]
  (button! g col row (str " " glyph " ") :workspace-entry
    {:extra {:index direction :workspace-id direction :text direction}
     :register? register?}))
;; ── help overlay ────────────────────────────────────────────────────────────
(def ^:private help-sections
  "Sections of `[keys description]` rows shown in the C-x h help card, grouped
   under a section `:title`. Chords are derived from `keymap` so the card never
   drifts from the live bindings; the editing keys use the same Emacs `C-…`
   notation."
  [{:title "Command palette"
    :rows [[keymap/palette-chord "Every command (new session, search, providers, sessions, voice, files…); type to filter"]]}
   {:title "Session, model & tools"
    :rows [[(keymap/label-for :cycle-model) "Cycle model"]
           [(keymap/label-for :pick-model) "Pick model (search)"]
           [(keymap/label-for :cycle-reasoning) "Cycle reasoning effort"]
           [(keymap/label-for :cycle-verbosity) "Cycle answer length"]
           [(keymap/label-for :search-open) "Search in session"]
           [(keymap/label-for :pick-file) "Attach file"]
           [(keymap/label-for :toggle-voice-recording) "Voice recording"]
           [(keymap/label-for :open-dirs) "Filesystem"]
           [(keymap/label-for :open-resources) "Resources"]
           [(keymap/label-for :toggle-help) "Toggle this help"]]}
   {:title "Messaging & navigation"
    :rows [["Enter" "Send message"]
           ["Esc · C-g" "Abort — cancel turn · close dialog · clear draft"]
           ["C-c" "Quit (on an empty draft)"]
           ["M-> · C-x j · C-l · C-End" "Jump to newest — end-of-buffer (or click the ↓ latest chip)"]
           ["M-<" "Jump to the top — beginning-of-buffer"]
           ["C-v · M-v · PgDn · PgUp" "Scroll a screen forward · back"]
           ["Tab · Shift+Tab" "Next · previous workspace"]
           ["C-x ← · C-x →" "Previous · next workspace"]
           ["C-x b" "Switch workspace — the buffer-list picker"]
           ["M-1 … M-9 · C-x 1 … C-x 9" "Jump straight to workspace 1–9"]
           [(keymap/label-for :close-tab) "Close (kill) the current workspace tab"]]}
   {:title "Text editing"
    :rows [["C-a · C-e" "Beginning · end of line"]
           ["C-b · C-f" "Backward · forward char"]
           ["C-p · C-n" "Previous · next line"]
           ["C-t" "Transpose chars"]
           ["C-k · C-u" "Kill to line end · start"]
           ["C-w · C-d" "Kill word back · delete char forward"]
           ["↑ · ↓ · ← · →" "History / move cursor (Alt+←/→ by word where supported)"]
           ["Copy / paste" "Use your terminal — select to copy, its paste key"]
           ["Mouse" "Click a tab to switch · ✕ close · + new session · ↓ latest to jump down"]]}])
;; ── header band chrome ──────────────────────────────────────────────────────
(defn band-rule!
  "Paint a full-width single-line horizontal rule across `cols` on `row`."
  ([g row cols] (band-rule! g row cols t/footer-fg-muted))
  ([g row cols fg]
   (p/clear-styles! g)
   (p/set-colors! g fg t/terminal-bg)
   (dotimes [c (long cols)] (p/set-char! g c row p/BOX_H))
   (p/clear-styles! g)))
(defn- level->fg
  "Map a notification level to a foreground color; unknown levels fall back
   to the muted-footer color so something still renders."
  [level]
  (case level
    :success t/footer-fg-strong
    :warn t/footer-warning-fg
    :error t/footer-error-fg
    :info t/footer-spinner-fg
    t/footer-fg-muted))
(defn notification-slot!
  "Paint the header's left-slot notification/status `text` at (col,row),
   colored by `level` (bold). No-op for blank text."
  [g col row text level]
  (when (seq text)
    (p/clear-styles! g)
    (p/set-colors! g (level->fg level) t/terminal-bg)
    (p/enable! g p/BOLD)
    (p/put-str! g col row text)
    (p/clear-styles! g)))
(defn id-badge!
  "Paint the session-id COPY BUTTON `text` at (col,row) as a filled chip — via
   the shared `button!`, so it reads as the SAME control as the F1/F2 buttons
   instead of bare text — and register its `:copy-id` click region carrying the
   FULL uuid (the click handler drops it on the clipboard). Brightens
   to the accent on hover. No-op for blank text."
  [g col row text full-uuid register?]
  (when (pos? (p/display-width text))
    (button! g col row text :copy-id
      {:extra {:text full-uuid},
       :register? (boolean (and register? full-uuid))})))
(defn header-badge!
  "Paint a clickable header chip `glyph` at (col,row) and register a click
   region of `kind` (e.g. :toggle-tasks / :toggle-help). Brightens on hover.
   Returns the consumed width. A terminal-safe, always-visible stand-in for
   the F1/F2 accelerators."
  [g col row glyph kind register?]
  (let [hovered (cr/hovered)
        hovered? (= kind (:kind hovered))
        w (p/display-width glyph)]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/header-hover-fg t/text-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width w}, :kind kind, :enabled? true}))
    w))
;; ── help overlay ────────────────────────────────────────────────────────────
(defn- pad-right
  ^String [^String s ^long w]
  (str s (apply str (repeat (max 0 (- w (p/display-width s))) \space))))
(defn dialog-close-button!
  "Draw a clickable `✕` close chip at a dialog's top-right title corner —
   the SAME button every modal dialog shows — and register its `kind`
   click region (`:toggle-help` / `:toggle-tasks`) so a mouse click
   dismisses the overlay. Inverted title chip at rest; red pill on hover.
   Returns nil."
  [g bounds kind]
  (let [{:keys [top right]} bounds
        hovered (cr/hovered)
        hovered? (= kind (:kind hovered))
        title-row (inc (long top))
        label " ✕ "
        w (p/display-width label)
        col (- (long right) w)]
    (p/clear-styles! g)
    (p/set-colors! g
      (if hovered? t/header-active-tab-fg t/dialog-title-bg)
      (if hovered? t/close-button-hover-fg t/dialog-title-fg))
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col title-row label)
    (p/clear-styles! g)
    (cr/register! {:bounds {:row title-row, :col col, :width w}, :kind kind, :enabled? true})
    nil))

(defn- box-grid-lines
  "Render `sections` (each `{:title :rows}`, rows `[left-cell right-cell]`) as a
   2-column box-drawing grid grouped under full-width section banners: a top
   rule, then per section a bold `:title` banner, a `┬`-split header rule, the
   `│`-framed `key-w`/`desc-w` cells with `┼` separators between rows, and a `┴`
   rule closing the columns before the next banner. `bd` is the border color,
   `key-fg`/`desc-fg` the cell text colors, `title-fg` the banner color.

   Descriptions WIDER than `desc-w` WRAP onto continuation lines (word-wrapped
   to `desc-w`) instead of being clipped — only the first visual line of a row
   carries its key, continuation lines leave the key cell blank, and the `┼`
   separator sits between logical rows, never inside a wrapped one. Returns a
   vec of segment-rows ready for the overlay painter — the self-contained table
   renderer the F1 help card uses. Callers derive the scroll extent from
   `(count …)` of the result, since a row may now span several lines."
  [sections key-w desc-w bd key-fg desc-fg title-fg]
  (let [key-w (long key-w)
        desc-w (long desc-w)
        dash (fn [^long n] (apply str (repeat n "─")))
        kd (+ key-w 2)
        dd (+ desc-w 2)
        full (fn [l r] [[(str l (dash (+ kd 1 dd)) r) bd false]])
        cols (fn [l m r] [[(str l (dash kd) m (dash dd) r) bd false]])
        title-row (fn [t] [["│ " bd false]
                           [(pad-right (str t) (+ key-w desc-w 3)) title-fg true]
                           [" │" bd false]])
        row-line (fn [k d] [["│ " bd false]
                            [(pad-right (str k) key-w) key-fg true]
                            [" │ " bd false]
                            [(pad-right (str d) desc-w) desc-fg false]
                            [" │" bd false]])
        row-block (fn [[k d]]
                    (let [ds (p/word-wrap (str d) desc-w)
                          ds (if (seq ds) ds [""])]
                      (map-indexed (fn [i dl] (row-line (if (zero? i) k "") dl)) ds)))
        section-lines (fn [{:keys [title rows]}]
                        (concat [(title-row title) (cols "├" "┬" "┤")]
                          (apply concat
                            (interpose [(cols "├" "┼" "┤")]
                              (mapv row-block rows)))))]
    (vec (concat [(full "┌" "┐")]
           (apply concat (interpose [(cols "├" "┴" "┤")]
                           (mapv section-lines sections)))
           [(cols "└" "┴" "┘")]))))

(defn scrollable-dialog-body!
  "Paint the scroll plumbing both modal overlays (F1 help, F2 context) share:
   clamp `scroll` to `[0, (- (count lines) content-h)]`, window `lines` by that
   effective offset and paint each visible row via `paint-line` (a
   `(fn [screen-row-i line])`), draw the shared `scrollbar/draw!` in `sb-col`
   when the content overflows, and a right-aligned `N-M / total` position hint
   on `hint-row` (anchored to `body-right`). `geom` is
   `{:content-top :content-h :hint-row :sb-col :body-right}`. Returns
   `{:scroll :max-scroll :sb? :shown-n}` so callers feed the clamp back and
   derive their own geometry (e.g. F2's selectable ranges)."
  [g lines {:keys [content-top content-h hint-row sb-col body-right]} scroll paint-line]
  (let [n (count lines)
        max-scroll (max 0 (- n content-h))
        eff (max 0 (min (long (or scroll 0)) max-scroll))
        sb? (> n content-h)
        shown-n (min content-h (- n eff))]
    (dotimes [i shown-n] (paint-line i (nth lines (+ eff i))))
    (when sb?
      (scrollbar/draw! g {:col sb-col, :top content-top, :track-h content-h, :total-h n, :inner-h content-h, :scroll eff}))
    (when (and hint-row sb?)
      (let [pos (str (inc eff) "–" (+ eff shown-n) " / " n)
            pw (p/display-width pos)]
        (p/clear-styles! g)
        (p/set-colors! g t/dialog-hint t/dialog-bg)
        (p/put-str! g (- body-right pw) hint-row pos)))
    {:scroll eff, :max-scroll max-scroll, :sb? sb?, :shown-n shown-n}))

(defn help-overlay!
  "Draw the keyboard-shortcut help as a dialog, using the shared
   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it matches the F2
   context panel (shadow, border, accent title bar, centered hint row).
   The body (built by `box-grid-lines`) is a sectioned 2-column grid: each
   group rides under a bold full-width banner, so the card reads as labelled
   SECTIONS instead of bare rows with blank gaps.

   The chrome is drawn FIRST (the shared 5-arg arity picks a fixed modal
   footprint and IGNORES the passed line count), so we learn the real inner
   width, then size the desc column to what actually FITS — reserving one
   scrollbar-lane column — and let `box-grid-lines` WRAP long descriptions
   instead of clipping them. The scroll extent is the wrapped line count, so
   scrolling always covers every visual line. Registers only its close-button
   click region; the caller dismisses it (Ctrl+H / F1 / any key). Returns
   `{:scroll :max-scroll}` so the caller can feed the clamp back, exactly like
   `context-overlay!`."
  [g cols rows scroll]
  (let [title "Keyboard shortcuts"
        all-rows (mapcat :rows help-sections)
        key-w (reduce max 0 (map (comp p/display-width first) all-rows))
        bd t/dialog-border
        bounds (dialogs/draw-dialog-chrome! g cols rows title nil)
        {:keys [left inner-w]} bounds
        ;; Fit the desc column to the box, minus the key column, its gutters and
        ;; one column for the scrollbar lane — long descriptions wrap into this
        ;; width rather than overflowing the border.
        desc-w (max 12 (- inner-w key-w 8 1))
        lines (box-grid-lines help-sections key-w desc-w bd t/footer-fg-strong t/footer-fg t/header-active-tab-accent)
        line-cnt (count lines)
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds line-cnt)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       (loop [x (+ left 2) ss segs]
                         (when-let [[text color bold?] (first ss)]
                           (let [avail (max 0 (- (+ left 1 inner-w) x))
                                 shown (dialogs/ellipsize (str text) avail)]
                             (p/clear-styles! g)
                             (p/set-colors! g color t/dialog-bg)
                             (when bold? (p/enable! g p/BOLD))
                             (p/put-str! g x r shown)
                             (recur (+ x (p/display-width shown)) (next ss)))))))
        geom (scrollable-dialog-body! g lines {:content-top content-top, :content-h content-h, :hint-row hint-row, :sb-col (+ left inner-w), :body-right (+ left 1 inner-w)} scroll paint-line)]
    (dialog-close-button! g bounds :toggle-help)
    (p/clear-styles! g)
    (select-keys geom [:scroll :max-scroll])))
;; ── tasks overlay (W3: user-visible :session/tasks) ──────────────────────────
(defn- task-status-glyph
  [status]
  (case status
    :candidate "◇"
    :done "✓"
    :doing "◐"
    :cancelled "✗"
    :rejected "⊘"
    "○"))
(defn- task-status-color
  [status]
  (case status
    :candidate t/warning-fg
    :done t/status-ok
    :doing t/warning-fg
    :cancelled t/cancelled-fg
    :rejected t/cancelled-fg
    t/footer-fg-muted))
(def ^:private task-status-rank {:candidate 0, :doing 1, :todo 2, :done 3, :cancelled 4, :rejected 5})
(defn- clip-str
  "Truncate `s` to `w` display columns with a trailing ellipsis.
   MUST clip by display width, not char index: `subs` indexes characters, so
   for any string with wide graphemes (display-width > char-count) `(dec w)`
   could exceed `(.length s)` and throw StringIndexOutOfBoundsException every
   frame — which froze the whole F2 context overlay. `p/truncate-cols` walks
   graphemes and never overruns the string."
  ^String [^String s ^long w]
  (if (<= (p/display-width s) w) s (str (p/truncate-cols s (max 0 (dec w))) "…")))
(def ^:private overlay-blank-row
  "A single empty overlay row — the spacer between context-dialog entries."
  [["" t/dialog-hint false]])
(def ^{:private true} overlay-card-indent
  "Leading columns inset each task/fact card from BOTH dialog rails (left via\n   indent-rows, right via the narrowed body width at the call sites)."
  3)
(defn- indent-rows
  "Prefix every row with `indent` (default `overlay-card-indent`) leading\n   spaces so the whole card insets from the dialog left rail. Archived\n   tasks pass a larger indent so their cards nest under the indented\n   ARCHIVED TASKS header."
  ([rows] (indent-rows rows overlay-card-indent))
  ([rows indent]
   (let [pad [(apply str (repeat indent \space)) t/dialog-hint false]]
     (mapv (fn [row] (with-meta (into [pad] row) (meta row))) rows))))
(defn- wrap-cols
  "Greedy display-width word-wrap to a vec of lines, each fitting `w` columns.
   Delegates to the native, grapheme/EAW-aware `p/word-wrap` (one shared
   implementation in the lanterna fork) — a token wider than `w` is hard-split
   at grapheme boundaries, blank input yields `[\"\"]`."
  [s ^long w]
  (p/word-wrap s w))
(defn- justify-line
  "Full-justify `s` to `w` display columns by spreading extra spaces between\n   words. Returns `s` untouched when it has fewer than two words or already\n   fills `w`."
  [s ^{:tag long} w]
  (let [words (str/split (str/trim s) #"\s+")
        n (count words)]
    (if (< n 2)
      s
      (let [text-w (reduce + (map p/display-width words))
            gaps (dec n)
            slack (- w text-w gaps)]
        (if (<= slack 0)
          s
          (let [base (quot slack gaps)
                extra (rem slack gaps)]
            (apply str
              (interleave
                words
                (concat (map (fn [i] (apply str (repeat (+ 1 base (if (< i extra) 1 0)) \space)))
                          (range gaps))
                  [""])))))))))
(defn- wrapped-rows
  "Rows for `text` wrapped to `w` columns: the FIRST row is prefixed by the\n   `head` segments (e.g. a colored glyph) and every continuation row is\n   indented by `indent` spaces so it aligns under the head. Non-final lines\n   are full-justified to `w`. Each row is a vec of `[text color bold?]`\n   segments. `indent` MUST equal the head's display width for clean alignment."
  [head indent text w body-color bold?]
  (let [pieces (wrap-cols text (long w))
        last-i (dec (count pieces))
        pad (apply str (repeat (long indent) \space))]
    (vec (map-indexed (fn [i piece]
                        (let [piece (if (< i last-i) (justify-line piece (long w)) piece)]
                          (if (zero? i)
                            (conj (vec head) [piece body-color bold?])
                            [[(str pad piece) body-color bold?]])))
           pieces))))
(defn- run->seg
  "Convert one styled IR run into a `[text color bold?]` overlay segment.
   `code` / link spans take the accent color; `bold` (or the caller's
   `base-bold?`) sets the bold flag; everything else inherits `base-color`."
  [{:keys [text style]} base-color base-bold?]
  (let [accent? (or (contains? style :code) (contains? style :link))]
    [text (if accent? t/header-active-tab-accent base-color)
     (boolean (or base-bold? (contains? style :bold)))]))
(defn- justify-segs
  "Full-justify a row of `[text color bold?]` segments to `w` display columns by\n   widening every inter-word whitespace run across the segments. Returns the\n   segments untouched when there are no gaps or no slack (single word / already\n   full). The styled twin of `justify-line` for `md-wrapped-rows`.\n\n   `prefix-n` leading segments are STRUCTURAL — a list marker (`- `, `• `,\n   `1. `) or a continuation hanging-indent (`  `). Their widths still count\n   toward the slack budget, but their whitespace is never stretched, so a\n   bulleted line keeps a single space after the marker (`- foo bar baz`,\n   not `-    foo  bar  baz`) while the content still justifies edge-to-edge."
  ([segs ^{:tag long} w] (justify-segs segs w 0))
  ([segs ^{:tag long} w ^{:tag long} prefix-n]
   (let [texts (map first segs)
         content-texts (drop prefix-n texts)
         gap-count (reduce + (map (fn [t] (count (re-seq #"\s+" t))) content-texts))
         text-w (reduce + (map p/display-width texts))
         slack (- w text-w)]
     (if (or (< gap-count 1) (<= slack 0)
           ;; Stretch cap: only justify lines that are ALREADY near-full.
           ;; When `slack >= gap-count`, every gap would grow by ≥1 space
           ;; (single→double across the whole line — the "A  lighter
           ;; alternative" rivers). Leave those ragged-right; only lines
           ;; within `gap-count` columns of full get the gentle treatment,
           ;; where `slack` gaps gain +1 space (max 2 per gap) and the rest
           ;; stay single.
           (>= slack gap-count))
       segs
       (let [base (quot slack gap-count)
             extra (rem slack gap-count)
             idx (atom -1)]
         (vec
           (map-indexed
             (fn [si [t color bold?]]
               (if (< si prefix-n)
                 [t color bold?]
                 [(str/replace t
                    #"\s+"
                    (fn [m]
                      (let [i (swap! idx inc)
                            add (+ base (if (< i extra) 1 0))]
                        (str m (apply str (repeat add \space))))))
                  color bold?]))
             segs)))))))
(defn- md-wrapped-rows
  "Markdown-aware `wrapped-rows`: `text` is lifted to canonical IR via\n   `vis/markdown->ir`, wrapped to `w` columns by the shared IR walker, and\n   each line's styled runs become `[text color bold?]` segments — **bold**,\n   `code`, and links render inline (markup stripped). The FIRST row is\n   prefixed by `head`; continuation rows indent `indent` spaces. Runs of\n   blank inter-block lines collapse to a single separator; `-`/`1.` list items\n   keep a `• ` marker. Only lines the wrapper broke on overflow (those\n   the IR walker tags `:wrap?`) are full-justified to `w` via\n   `justify-segs`; paragraph/block-terminal lines stay ragged-right so\n   short tails aren't stretched edge-to-edge."
  [head indent text w base-color base-bold?]
  (let [ir (vis/markdown->ir (str text))
        blank-line? (fn [{:keys [runs]}] (every? (fn [r] (str/blank? (:text r))) runs))
        lines (->> (ir-tui/ir->lines ir (long w))
                   ;; Collapse runs of blank inter-block lines to a single
                   ;; separator (and trim leading/trailing) so multi-paragraph /
                   ;; bulleted task bodies keep their block breaks instead of
                   ;; rendering as one crushed run — without opening huge gaps.
                (reduce (fn [acc line]
                          (if (blank-line? line)
                            (if (or (empty? acc) (blank-line? (peek acc))) acc (conj acc line))
                            (conj acc line)))
                  [])
                ((fn [v] (if (and (seq v) (blank-line? (peek v))) (pop v) v))))
        pad (apply str (repeat (long indent) \space))]
    (if (empty? lines)
      [(vec head)]
      (vec (map-indexed
             (fn [i {:keys [runs wrap?]}]
               (let [segs (mapv (fn [r] (run->seg r base-color base-bold?)) runs)
                     segs (if (seq segs) segs [["" base-color base-bold?]])
                     ;; Leading structural runs — a list `:marker` (`- `,
                     ;; `• `, `1. `) or the pure-whitespace hanging-indent on
                     ;; a wrapped continuation — must NOT be stretched, else
                     ;; justification blows a hole right after the bullet
                     ;; (`-      foo`). Count them so `justify-segs` protects
                     ;; that prefix and only justifies the content gaps.
                     prefix-n (count (take-while
                                       (fn [r] (or (contains? (:style r) :marker)
                                                 (str/blank? (:text r))))
                                       runs))
                     ;; Only lines the wrapper broke on overflow (`:wrap?`)
                     ;; get full-justified. Paragraph/block-terminal lines
                     ;; are short and ragged-right by nature — stretching
                     ;; them edge-to-edge is the "4 words, mega holes" bug.
                     segs (if wrap? (justify-segs segs (long w) (long prefix-n)) segs)]
                 (if (zero? i) (into (vec head) segs) (into [[pad base-color base-bold?]] segs))))
             lines)))))
(defn- task-entry-rows
  "Progressive-disclosure rows for ONE task. SETTLED tasks
   (done/cancelled/rejected/deferred) collapse to a single dim line -
   glyph + title - finished work reads as a quiet receipt. OPEN tasks
   (doing/todo/candidate) render the full card: colored glyph + wrapped
   bold title, a colored meta row (human status word +
   verified/unverified badge), then the contract as labelled dim
   sub-rows when present: rationale, acceptance, files, avoid,
   evidence, dependencies and linked facts. Everything wraps to
   `body-w`; `indent` left-insets the card. Rows are
   `[text color bold?]` segment vecs."
  [k t body-w indent]
  (let [status (or (:status t) :todo)
        settled? (contains? #{:done :cancelled :rejected :deferred} status)
        glyph-seg [(str (task-status-glyph status) " ") (task-status-color status) true]
        title-base (or (not-empty (str (:title t))) (name k))]
    (if settled?
      (conj (indent-rows (vec (md-wrapped-rows [glyph-seg] 2 title-base
                                (max 6 (- body-w 2)) t/footer-fg-muted false))
              indent)
        overlay-blank-row)
      (let [status-label (case status
                           :doing "in progress"
                           :todo "pending"
                           (name status))
            verify-seg (cond (:verified? t) ["  ✓ verified" t/status-ok false]
                         (:acceptance t) ["  ⚠ unverified" t/warning-fg false]
                         :else nil)
            meta-row (into [["  " t/footer-fg-muted false]
                            [status-label (task-status-color status) true]]
                       (when verify-seg [verify-seg]))
            sub-w (max 6 (- body-w 4))
            labelled (fn [marker text]
                       (when-let [s (not-empty (str text))]
                         (md-wrapped-rows [[(str "  " marker " ") t/footer-fg-muted false]]
                           4 s sub-w t/footer-fg-muted false)))
            joined (fn [marker xs sep]
                     (when (seq xs)
                       (wrapped-rows [[(str "  " marker " ") t/footer-fg-muted false]]
                         4 (str/join sep (map str xs)) sub-w t/footer-fg-muted false)))
            rationale-rows (labelled "≡" (:rationale t))
            accept-rows (labelled "▸" (:acceptance t))
            files-rows (joined "▢" (:files t) "  ·  ")
            avoid-rows (joined "⊘" (:avoid t) "  ·  ")
            evidence-rows (labelled "⚑" (:evidence t))
            dep-rows (when (seq (:depends_on t))
                       (wrapped-rows [["  ↳ needs " t/footer-fg-muted false]]
                         4 (str/join ", " (map pr-str (:depends_on t))) sub-w
                         t/footer-fg-muted false))
            fact-rows (joined "⛁ facts" (:facts t) ", ")]
        (-> (vec (md-wrapped-rows [glyph-seg] 2 title-base (max 6 (- body-w 2)) t/dialog-fg true))
          (conj meta-row)
          (conj overlay-blank-row)
          (into rationale-rows)
          (into accept-rows)
          (into files-rows)
          (into avoid-rows)
          (into evidence-rows)
          (into dep-rows)
          (into fact-rows)
          (indent-rows indent)
          (conj overlay-blank-row)
          (conj overlay-blank-row))))))
(defn- task-overlay-lines
  "TASKS section body with progressive disclosure: a progress header
   (▰▰▱▱ bar + `N of M done`), then one `task-entry-rows`
   entry per task, status-sorted (candidate → doing → todo →
   done → cancelled). Settled tasks collapse to one dim line; open
   tasks show their full card. Empty state is a single hint row.
   `indent` (default `overlay-card-indent`) lets ARCHIVED TASKS cards
   nest under their indented header."
  ([tasks body-w] (task-overlay-lines tasks body-w overlay-card-indent))
  ([tasks body-w indent]
   (if (empty? tasks)
     (indent-rows [[["No active tasks — tasks will appear here as work progresses." t/footer-fg-muted false]]] indent)
     (let [total (count tasks)
           settled #{:done :cancelled :rejected :deferred}
           done-n (count (filter (fn [[_ t]] (contains? settled (or (:status t) :todo))) tasks))
           bar-w 14
           filled (long (Math/round (* bar-w (/ done-n (double total)))))
           bar (str (apply str (repeat filled "▰"))
                 (apply str (repeat (- bar-w filled) "▱")))
           header [[(str bar "  ") (if (= done-n total) t/status-ok t/header-active-tab-accent) false]
                   [(str done-n " of " total " done") t/footer-fg-strong true]]
           cards (->> tasks
                   (sort-by (fn [[k t]] [(task-status-rank (or (:status t) :todo) 9) (str k)]))
                   (mapcat (fn [[k t]] (task-entry-rows k t (- body-w (* 2 indent)) indent))))]
       (-> (indent-rows [header] indent)
         (conj overlay-blank-row)
         (into cards)
         vec)))))
(defn- plan-history-lines
  "PLAN HISTORY section body — the append-only task ledger's PAST plan
   generations (`:timeline` on the F2 ctx cache, from `vis/plan-timeline`).
   One dim block per dropped generation, newest first: a `Plan #N` header
   then one glyph+title row per step, frozen at the status it had when the
   whole-replace dropped it. nil when there are no past generations."
  [timeline]
  (let [past (vec (remove :current? (or timeline [])))]
    (when (seq past)
      (-> (->> (rseq past)
            (mapcat (fn [{:keys [gen steps]}]
                      (concat
                        [[[(str "Plan #" gen "  ·  " (count steps)
                             " step" (when (not= 1 (count steps)) "s"))
                           t/footer-fg-strong true]]]
                        (map (fn [{:keys [key title status]}]
                               (let [st (keyword (str (or status "todo")))]
                                 [[(str (task-status-glyph st) " ") t/footer-fg-muted false]
                                  [(str (or (not-empty (str title)) key)) t/footer-fg-muted false]]))
                          steps)
                        [overlay-blank-row])))
            vec)
        (indent-rows 5)))))

(defn- fact-entry-rows
  "Modern multi-row card for ONE fact: a status glyph (active . / superseded
   x) + bold key, the WRAPPED content indented under it, then a blank spacer
   and a dim meta row joining `> N files`, `depends ...`, and `contradicts ...`
   when present, then a blank spacer. When the fact's key is in `expanded` the
   file-bearing card also lists each file path under the meta row, and the meta
   row carries `^{:fact-key <str>}` metadata so the overlay painter can wire a
   per-fact click region over it. Nothing truncated - content wraps to `body-w`."
  [k f body-w expanded]
  (let [super? (= :superseded (:status f))
        kstr (str k)
        files (:files f)
        file-count (count files)
        expandable? (pos? file-count)
        expanded? (and expandable? (contains? expanded kstr))
        glyph-seg [(if super? "⊘ " "• ") (if super? t/footer-fg-muted t/status-ok) true]
        ;; `turn_<N>` → `Turn <N>` for display (canonical via fmt); stored key
        ;; (kstr) stays snake so recall/fold + the click region still match.
        key-row [glyph-seg [(fmt/humanize-fact-key k) (if super? t/footer-fg-muted t/header-active-tab-accent) true]]
        content (not-empty (str (:content f)))
        content-rows (when content
                       (md-wrapped-rows [["  " t/dialog-fg false]]
                         2
                         content
                         (max 6 (- body-w 2))
                         (if super? t/footer-fg-muted t/dialog-fg)
                         false))
        files-label (when expandable?
                      (str (if expanded? "▾ " "▸ ") "⛁ " file-count " files"))
        meta-parts (cond-> []
                     files-label (conj files-label)
                     (seq (:depends_on f)) (conj (str "↳ depends "
                                                   (str/join ", " (map pr-str (:depends_on f)))))
                     (seq (:contradicts f))
                     (conj (str "⚡ contradicts "
                             (str/join ", " (map pr-str (sort (:contradicts f)))))))
        meta-rows (when (seq meta-parts)
                    (wrapped-rows [["  " t/footer-fg-muted false]]
                      2
                      (str/join "  \u00b7  " meta-parts)
                      (max 6 (- body-w 2))
                      t/footer-fg-muted
                      false))
        ;; Tag the FIRST meta row with the fact key so the overlay painter
        ;; can register a click region over it (toggle the file list). Only
        ;; file-bearing cards are clickable.
        meta-rows (if (and (seq meta-rows) expandable?)
                    (into [(vary-meta (first meta-rows) assoc :fact-key kstr)]
                      (rest meta-rows))
                    meta-rows)
        ;; When expanded, list each file path under the meta row.
        ;; When expanded, list each file path under the meta row, and under
        ;; each path its recorded regions (note + anchor) so the panel shows
        ;; WHERE in the file the fact points, not just the filename.
        file-rows (when expanded?
                    (vec (mapcat
                           (fn [file]
                             (let [path-row [["    · " t/footer-fg-muted false]
                                             [(str (or (:path file) file)) t/dialog-fg false]]
                                   region-rows (vec (mapcat
                                                      (fn [r]
                                                        (let [note (not-empty (str (or (:note r) (:src r))))
                                                              anchor (not-empty (str (or (:from_anchor r) (:from-anchor r))))
                                                              text (str note (when (and note anchor) "  ")
                                                                     (when anchor (str "(" anchor ")")))]
                                                          (when (or note anchor)
                                                            (wrapped-rows [["        \u21b3 " t/footer-fg-muted false]]
                                                              10
                                                              text
                                                              (max 6 (- body-w 10))
                                                              t/footer-fg-muted false))))
                                                      (:regions file)))]
                               (into [path-row] region-rows)))
                           files)))]
    (-> [key-row]
      (conj overlay-blank-row)
      (into content-rows)
      (conj overlay-blank-row)
      (into meta-rows)
      (into file-rows)
      indent-rows
      (conj overlay-blank-row)
      (conj overlay-blank-row))))
(defn- fact-overlay-lines
  "FACTS section body - one `fact-entry-rows` card per fact, active facts
   first then superseded. `expanded` is the set of fact keys (as strings)
   whose file list is currently unfolded. Empty state is a single hint row."
  [facts body-w expanded]
  (if (empty? facts)
    (indent-rows [[["No recorded facts - key findings will appear here as they're discovered." t/footer-fg-muted false]]])
    (->> facts
      (sort-by (fn [[k f]] [(if (= :superseded (:status f)) 1 0) (str k)]))
      (mapcat (fn [[k f]] (fact-entry-rows k f (- body-w (* 2 overlay-card-indent)) expanded)))
      vec)))
(defn- section-line
  "A bold section header line (single segment). Uses a DARK accent\n   (`header-active-tab-accent`) — NOT `dialog-title-fg`, which is white and\n   only legible on the dark title bar, not on the light dialog body.\n   Optional `indent` left-pads the label with that many columns so a\n   subsection can nest visually under its parent (e.g. ARCHIVED TASKS\n   under TASKS)."
  ([label] (section-line label 0))
  ([label indent]
   [[(str (apply str (repeat indent \space)) label) t/header-active-tab-accent true]]))
(defn context-overlay!
  "Dialog showing the session's working memory - `:session/tasks` AND
   `:session/facts` - the W3 user-visible panel (F2). Uses the shared
   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it looks like every other
   modal (shadow, border, accent title bar, hint row). Title carries a
   tasks-done + facts count summary; a TASKS section (status-sorted, colored
   glyphs, acceptance sub-lines, verify badges) then a FACTS section (active
   first, `> N` for file-bearing facts). `expanded` is the set of fact keys (as
   strings) whose file list is unfolded; each file-bearing fact's meta row gets
   a `:toggle-fact-files` click region so a click folds/unfolds its paths. The
   dialog SIZES to its content (grows to fit, clamped to the terminal); overflow
   scrolls through the shared `scrollable-dialog-body!` (same plumbing as F1
   help). `ctx` is `{:tasks ... :facts ...}`. Returns
   `{:scroll :max-scroll :selectable-ranges}`."
  [g cols rows {:keys [tasks facts archived timeline]} scroll expanded]
  (let [body-w (dialogs/default-content-width cols)
        blank [["" t/dialog-hint false]]
        title "Context"
        lines [blank
               [[(str "  Live task / fact / plan tracking was removed.")
                 t/dialog-hint false]]
               blank]
        n (count lines)
        cap-h (dialogs/default-content-height rows)
        req-h (min n cap-h)
        bounds (dialogs/draw-dialog-chrome! g cols rows title req-h)
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds req-h)
        sb? (> n content-h)
        body-right (+ left 1 inner-w)
        text-right (if sb? (dec body-right) body-right)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       ;; A meta row tagged with :fact-key gets a click region so
                       ;; clicking the `> N files` glyph toggles its path list.
                       (when-let [fk (:fact-key (meta segs))]
                         (cr/register! {:bounds {:row r, :col (+ left 1),
                                                 :width (max 0 (- text-right (+ left 1)))}
                                        :kind :toggle-fact-files, :fact-key fk, :enabled? true}))
                       (loop [x (+ left 1) ss segs]
                         (when-let [[text color bold?] (first ss)]
                           (let [avail (max 0 (- text-right x))
                                 shown (clip-str (str text) avail)]
                             (p/clear-styles! g)
                             (p/set-colors! g color t/dialog-bg)
                             (when bold? (p/enable! g p/BOLD))
                             (p/put-str! g x r shown)
                             (recur (+ x (p/display-width shown)) (next ss)))))))
        geom (scrollable-dialog-body! g lines
               {:content-top content-top, :content-h content-h, :hint-row hint-row,
                :sb-col (dec body-right), :body-right body-right}
               scroll paint-line)
        shown-n (:shown-n geom)]
    (dialog-close-button! g bounds :toggle-tasks)
    (p/clear-styles! g)
    {:scroll (:scroll geom), :max-scroll (:max-scroll geom),
     :selectable-ranges (vec (for [i (range shown-n)]
                               {:row (+ content-top i), :col (+ left 1),
                                :width (max 0 (- text-right (+ left 1)))}))}))
