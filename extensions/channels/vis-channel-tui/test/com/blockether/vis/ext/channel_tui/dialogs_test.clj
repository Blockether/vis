(ns com.blockether.vis.ext.channel-tui.dialogs-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.drafts :as drafts]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.terminals :as term]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.core :as vis]
            ;; Loaded for its side effect: registers the :shell/enabled toggle
            ;; (internal foundation, at ns load), which the settings-rows test asserts.
            [com.blockether.vis.internal.foundation.shell])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

;; Most dialog functions require a live TerminalScreen, so direct unit
;; testing is narrow. The bracketed-paste fix in text-input-dialog!
;; is verified indirectly: pasting into the API key field no longer
;; leaks PUA marker chars (\uE200, \uE201) into the stored value.

(defdescribe smoke-test
             (it "dialogs namespace loads and text-input-dialog! is public"
                 (expect (fn? (var-get #'dlg/text-input-dialog!)))))

(defdescribe
  modal-key-normalization-test
  (it "modal helpers accept Lanterna Enter/Escape and raw terminal CR/LF/ESC strokes"
      (expect (dlg/modal-enter-key? (KeyStroke. KeyType/Enter)))
      (expect (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \newline) false false false)))
      (expect (dlg/modal-enter-key? (KeyStroke. (Character/valueOf \return) false false false)))
      (expect (dlg/modal-escape-key? (KeyStroke. KeyType/Escape)))
      (expect (dlg/modal-escape-key? (KeyStroke. (Character/valueOf (char 27)) false false false))))
  (it "C-g is Escape for every modal, so keyboard-quit closes any dialog"
      ;; Emacs `keyboard-quit`: lanterna delivers it as `g` + Ctrl, some
      ;; terminals as the raw BEL byte. Both normalize to Escape, so the
      ;; `KeyType/Escape` branch of every dialog key loop fires without the
      ;; dialog knowing anything about C-g.
      (expect (dlg/modal-escape-key? (KeyStroke. (Character/valueOf \g) true false false)))
      (expect (dlg/modal-escape-key? (KeyStroke. (Character/valueOf (char 7)) false false false)))
      (expect (= KeyType/Escape
                 (.getKeyType ^KeyStroke
                              (dlg/normalize-modal-key
                                (KeyStroke. (Character/valueOf \g) true false false)))))
      ;; A plain `g` stays a plain `g` - dialog filters type it.
      (expect (not (dlg/modal-escape-key? (KeyStroke. (Character/valueOf \g) false false false))))))


(defn- wheel-down [] (MouseAction. MouseActionType/SCROLL_DOWN 0 (TerminalPosition. 10 10)))

(defdescribe modal-wheel-input-test
             (it "modal input coalesces wheel floods and preserves the next non-wheel key"
                 (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                       (term/virtual-screen)

                       read-modal-input!
                       (var-get #'dlg/read-modal-input!)]

                   (try (dotimes [_ 300]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= {:scroll-delta 300} (read-modal-input! screen)))
                        (expect (= KeyType/Enter
                                   (.getKeyType ^com.googlecode.lanterna.input.KeyStroke
                                                (:key (read-modal-input! screen)))))
                        (finally (.stopScreen screen))))))

(defdescribe modal-input-pending-test
             (it "reports a queued keystroke so a per-key search can debounce itself"
                 (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                       (term/virtual-screen)

                       read-modal-input!
                       (var-get #'dlg/read-modal-input!)]

                   ;; Nothing queued → the keystroke landed in a typing PAUSE, so
                   ;; the expensive gateway search is allowed to run.
                   (try (expect (false? (dlg/modal-input-pending? screen)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        ;; Still typing → skip the search this round. Peeking twice
                        ;; must not consume the stroke...
                        (expect (true? (dlg/modal-input-pending? screen)))
                        (expect (true? (dlg/modal-input-pending? screen)))
                        ;; ...and the modal loop still reads it.
                        (expect (= KeyType/Enter
                                   (.getKeyType ^KeyStroke (:key (read-modal-input! screen)))))
                        (expect (false? (dlg/modal-input-pending? screen)))
                        (finally (.stopScreen screen))))))

(defdescribe select-dialog-wheel-test
             (it "selection menu applies a wheel burst as one scroll movement"
                 (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                       (term/virtual-screen)

                       items
                       (mapv #(hash-map :label (str "Item " %) :id %) (range 20))]

                   (try (dotimes [_ 5]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= 1 (:id (dlg/select-dialog! screen "Items" items))))
                        (finally (.stopScreen screen))))))

;; ── run-modal! component spike ──────────────────────────────────────────────
;; The React-like payoff: `select-modal-component` is the pure heart of
;; `list-dialog!` — its :measure (geometry), :reconcile (scroll window) and
;; :on-key (nav / filter / select) are plain functions of immutable state, so
;; they test with NO live terminal at all (contrast every other dialog test
;; above, which must spin up a DefaultVirtualTerminal).
(defn- ks [t] (KeyStroke. ^KeyType t))

(defn- char-key [c] (KeyStroke. (Character/valueOf c) false false false))

(def ^:private done-key :com.blockether.vis.ext.channel-tui.dialogs/done)

(defdescribe select-modal-component-pure-test
             (it "arrow nav + Enter selects the right item with no screen"
                 (let [items
                       (mapv #(hash-map :label (str "Item " %) :id %) (range 20))

                       {:keys [init measure reconcile on-key]}
                       (dlg/select-modal-component "Items" items {:height :content})

                       geom
                       (measure init 80 30)

                       s0
                       (reconcile init geom)

                       down
                       (ks KeyType/ArrowDown)

                       s3
                       (nth (iterate #(on-key % down geom) s0) 3)

                       done
                       (on-key s3 (ks KeyType/Enter) geom)]

                   (expect (= 20 (:total geom)))
                   (expect (= 0 (:selected s0)))
                   (expect (= 3 (:selected s3)))
                   (expect (= {:label "Item 3" :id 3} (done-key done)))))
             (it "Escape closes with nil"
                 (let [{:keys [init measure on-key]}
                       (dlg/select-modal-component "X" [{:label "a"}] {})

                       geom
                       (measure init 80 30)]

                   (expect (contains? (on-key init (ks KeyType/Escape) geom) done-key))
                   (expect (nil? (done-key (on-key init (ks KeyType/Escape) geom))))))
             (it "type-to-filter narrows :filtered and backspace widens it again"
                 (let [items
                       (mapv #(hash-map :label %) ["apple" "apricot" "banana" "cherry"])

                       {:keys [init measure reconcile on-key]}
                       (dlg/select-modal-component "F" items {:filter? true :height :content})

                       step
                       (fn [s k]
                         (on-key s k (measure s 80 30)))

                       s1
                       (-> (reconcile init (measure init 80 30))
                           (step (char-key \a))
                           (step (char-key \p)))

                       s2
                       (step s1 (ks KeyType/Backspace))]

                   (expect (= "ap" (:query s1)))
                   (expect (= ["apple" "apricot"] (mapv :label (:filtered (measure s1 80 30)))))
                   (expect (= "a" (:query s2)))
                   (expect (= ["apple" "apricot" "banana"]
                              (mapv :label (:filtered (measure s2 80 30)))))))
             (it "a wheel burst moves selection by the coalesced step"
                 (let [items
                       (mapv #(hash-map :label (str %) :id %) (range 20))

                       {:keys [init measure reconcile on-key]}
                       (dlg/select-modal-component "W" items {:height :content})

                       geom
                       (measure init 80 30)

                       s0
                       (reconcile init geom)

                       burst
                       (MouseAction. MouseActionType/SCROLL_DOWN 5 (TerminalPosition. 10 10))]

                   (expect (= 5 (:selected (on-key s0 burst geom)))))))

(defdescribe session-dialog-wheel-test
             (it "session picker coalesces wheel floods and moves selection"
                 (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                       (term/virtual-screen)

                       sessions
                       (mapv (fn [idx]
                               {"id" idx "title" (str "Session " idx) "turn_count" idx})
                             (range 20))]

                   (try (dotimes [_ 5]
                          (.addInput terminal (wheel-down)))
                        (.addInput terminal (KeyStroke. KeyType/Enter))
                        (expect (= {:action :switch :id "1"}
                                   (dlg/session-picker-dialog! screen sessions nil)))
                        (finally (.stopScreen screen))))))


(defdescribe
  extension-display-label-namespace-test
  (it "namespace-derived labels titleize the meaningful tail segment, NEVER the vendor prefix"
      (let [label (var-get #'dlg/extension-display-label)]
        ;; plain ns -> tail segment titleized; vendor prefix dropped
        (expect (= "Voice" (label {:ext/name "voice"})))
        ;; trailing 'core' segment is dropped
        (expect (= "Goal" (label {:ext/name "goal"})))
        ;; hyphenated segment is split + titleized like other labels
        (expect (= "Channel Tui" (label {:ext/name "channel-tui"})))
        ;; regression: was rendered as 'Com.blockether.vis.ext.voice.core'
        (expect (not (str/starts-with? (label {:ext/name "voice"}) "Com.blockether")))))
  (it "provider / channel / alias labels still take precedence"
      (let [label (var-get #'dlg/extension-display-label)]
        (expect (= "Anthropic"
                   (label {:ext/providers [{:provider/label "Anthropic (API Key)"}]
                           :ext/name "provider-anthropic"})))
        (expect (= "Tui" (label {:ext/channels [{:channel/cmd "tui"}] :ext/name "channel-tui"})))
        (expect (= "V" (label {:ext/engine {:ext.engine/alias 'v} :ext/name "foundation"}))))))

(defdescribe reusable-table-test
             (it "table rows keep fixed width and expose shared filtering"
                 (let [columns
                       [{:id :kind :label "Kind" :width 8} {:id :label :label "Name" :flex 1}
                        {:id :status :label "Status" :width 8}]

                       row
                       {:kind "session" :label "Untitled session" :status "active"}

                       line
                       (table/row-line columns row 48 nil)]

                   (expect (= 48 (p/display-width line)))
                   (expect (str/includes? line "session"))
                   (expect (str/includes? (table/header-line columns 48) "Kind"))
                   (expect (= \┌ (first (table/boxed-border-line [8 27 8] :top))))
                   (expect (= \│
                              (first (table/boxed-row-line [8 27 8]
                                                           ["Kind" "Name" "Status"]
                                                           [:left :left :left]))))
                   (expect (table/row-matches? row "untitled"))
                   (expect (not (table/row-matches? row "workspace")))
                   ;; Boolean row flags (e.g. :focused?) are never search text.
                   (expect (not (table/row-matches? (assoc row :focused? true) "true"))))))

(defdescribe session-dialog-table-model-test
             (it "session rows sort by modified-at desc and split date/time columns"
                 (let [items
                       (dlg/session-dialog-items [{"id" "old"
                                                   "title" "Old"
                                                   "turn_count" 1
                                                   "created_at" #inst "2024-01-01T09:30:00.000Z"
                                                   "modified_at" #inst "2024-01-01T10:45:00.000Z"}
                                                  {"id" "new"
                                                   "title" "New"
                                                   "turn_count" 2
                                                   "created_at" #inst "2024-01-02T08:15:00.000Z"
                                                   "modified_at" #inst "2024-01-02T11:05:00.000Z"}]
                                                 "new"
                                                 96)

                       header
                       (dlg/session-dialog-header 96)]

                   (expect (= ["new" "old"] (mapv :id items)))
                   (expect (str/includes? header "Created at"))
                   (expect (str/includes? header "Modified at"))
                   (expect (str/includes? (:label (first items)) "2024-01-02"))
                   (expect (str/includes? (:label (first items)) "11:05"))))
             (it "session table uses boxed dialog-style borders with fixed width"
                 (let [items
                       (dlg/session-dialog-items [{:id "new"
                                                   :title "New"
                                                   :turn-count 2
                                                   :created-at #inst "2024-01-02T08:15:00.000Z"
                                                   :modified-at #inst "2024-01-02T11:05:00.000Z"}]
                                                 "new"
                                                 96)

                       border-line
                       (var-get #'dlg/session-table-border-line)]

                   (expect (= \┌ (first (border-line 96 :top))))
                   (expect (= 96 (p/display-width (border-line 96 :top))))
                   (expect (= 96 (p/display-width (:label (first items))))))))

;; 1:1 session<->workspace: one unified row per session, NOT a
;; duplicated session row + workspace row with a contradictory :kind.
;; The session you are currently in is the FOCUSED row: flagged, pinned
;; to the top, marked "● focused".
(defdescribe
  navigator-row-model-test
  (let [sessions [{"id" "empty" "title" nil "turn_count" 0 "created_at" 0 "modified_at" 7200000}
                  {"id" "s1" "title" nil "turn_count" 2 "created_at" 0 "modified_at" 3600000}
                  {"id" "s2" "title" "Second" "turn_count" 5 "created_at" 0 "modified_at" 0}]]
    (it "one unified row per session, no :kind / :switch-workspace"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})]

          (expect (= 2 (count rows)))
          (expect (every? #(not (contains? % :kind)) rows))
          (expect (= [{:action :switch :id "s1"} {:action :switch :id "s2"}] (mapv :target rows)))))
    (it "empty untitled shells hidden by default; focused session pinned to top"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              all-visible
              (all-rows {:active-session-id "s1" :sessions sessions :show-empty-untitled? true})]

          (expect (= ["s1" "s2"] (mapv (comp str :id :target) rows)))
          ;; Focused (s1) pinned first; the rest keep recency order below,
          ;; so all-visible is [s1 empty s2], not [empty s1 s2].
          (expect (= ["s1" "empty" "s2"] (mapv (comp str :id :target) all-visible)))))
    (it "focused session is flagged + pinned, marked '● focused'"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              r1 (first rows)]

          (expect (= "Untitled session" (:title r1)))
          (expect (= "s1" (:session r1)))
          (expect (:focused? r1))
          (expect (= "● focused" (:status r1)))))
    (it "non-active session is not focused and shows its turn count"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})]

          (expect (not (:focused? (second rows))))
          (expect (= "5 turns" (:status (second rows))))))
    (it "compact MM-dd HH:mm timestamps (UTC)"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              r1 (first rows)]

          (expect (= "01-01 00:00" (:created r1)))
          (expect (= "01-01 01:00" (:modified r1)))))
    (it "transcript-only matches are tagged and show an `in chat` status"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              visible-rows (var-get #'dlg/navigator-visible-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              id2 (str (:id (:target (second rows))))
              ;; Query matches no title/project cell; the id arrives ONLY from the
              ;; transcript (body) search, so the row is kept AND marked `in chat`.
              vis (visible-rows rows "zzz-no-title-match" #{id2})]

          (expect (= 1 (count vis)))
          (expect (:transcript-match? (first vis)))
          (expect (= "in chat" (:status (first vis))))))
    (it "body matches label the status by side and carry the You/Vis snippet"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              visible-rows (var-get #'dlg/navigator-visible-rows)
              preview-entries (var-get #'dlg/navigator-preview-entries)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              id2 (str (:id (:target (second rows))))
              mk (fn [k]
                   {id2 {:kind k
                         :request-snippet (when (#{:request :both} k) "…the search of…")
                         :reply-snippet (when (#{:reply :both} k) "…searchable now…")}})
              tag (fn [k]
                    (first (visible-rows rows "zzz-no-title-match" (mk k))))]

          ;; user-request hit → `in request`, only a You preview side.
          (expect (= "in request" (:status (tag :request))))
          (expect (= ["You"] (mapv :label (preview-entries (:transcript-match (tag :request))))))
          ;; assistant-reply hit → `in reply`, only a Vis preview side.
          (expect (= "in reply" (:status (tag :reply))))
          (expect (= ["Vis"] (mapv :label (preview-entries (:transcript-match (tag :reply))))))
          ;; both sides → `in chat`, You then Vis.
          (expect (= "in chat" (:status (tag :both))))
          (expect (= ["You" "Vis"] (mapv :label (preview-entries (:transcript-match (tag :both))))))
          ;; the match carries the session title so the preview leads with it,
          ;; before the You/Vis snippet — title first, then transcript.
          (expect (= (:title (tag :both)) (:title (:transcript-match (tag :both)))))))
    (it "a body match previews EVERY hit the server sent, in order"
        (let [preview-entries (var-get #'dlg/navigator-preview-entries)
              m {:request-snippet "…legacy ask…"
                 :reply-snippet "…legacy reply…"
                 :hits [{:side :reply :snippet "newest reply"} {:side :request :snippet "older ask"}
                        {:side :reply :snippet ""} {:side :reply :snippet "oldest reply"}]}]

          ;; Every non-blank hit shows, newest first — a session that matched
          ;; many times no longer collapses to one arbitrary line.
          (expect (= ["Vis" "You" "Vis"] (mapv :label (preview-entries m))))
          (expect (= ["newest reply" "older ask" "oldest reply"] (mapv :text (preview-entries m))))
          ;; A hit-less match (older gateway) still renders the legacy pair.
          (expect (= ["You" "Vis"] (mapv :label (preview-entries (dissoc m :hits)))))))
    (it
      "a live query paints the GATEWAY's rank: name, ask, reply, thinking"
      (let [all-rows (var-get #'dlg/navigator-all-rows)
            visible-rows (var-get #'dlg/navigator-visible-rows)
            ;; Listed newest-first by the gateway: the thinking-only session leads.
            rows
            (all-rows
              {:active-session-id "none"
               :sessions
               [{"id" "muse" "title" "Muse hit" "turn_count" 4 "created_at" 0 "modified_at" 4}
                {"id" "reply" "title" "Reply hit" "turn_count" 3 "created_at" 0 "modified_at" 3}
                {"id" "ask" "title" "Ask hit" "turn_count" 2 "created_at" 0 "modified_at" 2}
                {"id" "named"
                 "title" "Needle in the name"
                 "turn_count" 1
                 "created_at" 0
                 "modified_at" 1}]})
            ;; The bands are the SERVER's (`:rank`): 0 title, 1 request, 2 reply,
            ;; 3 thinking. The picker sorts by them and invents nothing.
            matches {"muse" {:rank 3 :kind :thinking :reply-snippet "…needle…"}
                     "reply" {:rank 2 :kind :reply :reply-snippet "…needle…"}
                     "ask" {:rank 1 :kind :request :request-snippet "…needle…"}
                     "named" {:rank 0 :kind :title}}
            vis (visible-rows rows "needle" matches)
            ;; Same rows, gateway ranks turned around: the picker follows the
            ;; server even when its OWN title cell matched the query.
            flipped (visible-rows rows
                                  "needle"
                                  (assoc matches
                                    "named" {:rank 3 :kind :title}
                                    "muse" {:rank 0 :kind :thinking :reply-snippet "…needle…"}))]

        ;; The name a human typed beats anything said in the chat; what the user
        ;; asked beats what the assistant answered; the assistant's reasoning
        ;; aside comes last. Recency decides only between equals.
        (expect (= ["named" "ask" "reply" "muse"] (mapv (comp str :id :target) vis)))
        (expect (= ["muse" "ask" "reply" "named"] (mapv (comp str :id :target) flipped)))
        (expect (= "in thinking"
                   (:status (first (filter #(= "muse" (str (:id (:target %)))) vis)))))))
    (it "every matching row carries its own snippets, inline, like the app"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              visible-rows (var-get #'dlg/navigator-visible-rows)
              hit-entries (var-get #'dlg/navigator-hit-entries)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              ids (mapv #(str (:id (:target %))) rows)
              ;; Query DOES match every title, and the body search returns hits for
              ;; both rows — including the focused one. The app previews all of
              ;; them, so the TUI must attach a match to all of them too.
              matches (into {}
                            (map (fn [id]
                                   [id {:hits [{:side :reply :snippet "…hit…"}]}])
                                 ids))
              vis (visible-rows rows "session" matches)]

          (expect (= (count rows) (count vis)))
          (expect (every? #(= 1 (count (hit-entries %))) vis))))
    (it "the inline list budgets painted LINES and never scrolls past the end"
        (let [heights (var-get #'dlg/navigator-block-heights)
              blocks (var-get #'dlg/navigator-visible-blocks)
              scroll-start (var-get #'dlg/navigator-scroll-start)
              hit (fn [n]
                    {:transcript-match {:hits (vec (repeat n {:side :reply :snippet "x"}))}})
              vis [(hit 3) (hit 0) (hit 2)]
              hs (heights vis)
              shape (fn [plan]
                      (mapv (juxt #(count (:hits %)) :spacer?) plan))]

          ;; Two content lines + one spacer per session, plus one line per snippet.
          (expect (= [6 3 5] hs))
          ;; The content base always survives. Snippets clip before the spacer;
          ;; only a viewport filled exactly by the base omits that spacer.
          (expect (= [[0 false]] (shape (blocks vis 0 2))))
          (expect (= [[0 true]] (shape (blocks vis 0 3))))
          (expect (= [[2 true]] (shape (blocks vis 0 5))))
          (expect (= [[3 true]] (shape (blocks vis 0 6))))
          ;; A project heading keeps one top-margin row with its first session.
          (let [grouped [(assoc (hit 0) :group-start? true)]]
            (expect (empty? (blocks grouped 0 3)))
            (expect (= [[0 false]] (shape (blocks grouped 0 4))))
            (expect (= [[0 true]] (shape (blocks grouped 0 5)))))
          ;; scroll advances only as far as the selected row needs
          (expect (= 1 (scroll-start hs 2 0 4)))
          (expect (= 2 (scroll-start hs 2 2 4)))
          (expect (= 0 (scroll-start hs 0 0 99)))))
    (it "keeps selection plain while sessions retain one row of breathing room"
        (let [{:keys [^TerminalScreen screen]} (term/virtual-screen)
              draw-session (var-get #'dlg/draw-navigator-session!)
              draw-hit (var-get #'dlg/draw-navigator-hit-line!)
              entry {:focused? false
                     :status "idle"
                     :title "First session"
                     :session "abc1234"
                     :draft "trunk"
                     :modified "now"}
              x 4
              row 6
              width 16]

          (try
            (let [g (.newTextGraphics screen)]
              (draw-session g x row width entry true)
              (draw-hit g x (+ row 2) width "needle" {:label "U" :role :user :text "needle match"})
              (draw-session g x (+ row 4) width entry false)
              (let [selected-title-bg (.getBackgroundColor
                                        (.getBackCharacter screen (int (+ x 2)) (int row)))
                    selected-meta-bg (.getBackgroundColor
                                       (.getBackCharacter screen (int (+ x 2)) (int (inc row))))
                    selected-hit-bg (.getBackgroundColor
                                      (.getBackCharacter screen (int (+ x 2)) (int (+ row 2))))
                    inactive-bg (.getBackgroundColor
                                  (.getBackCharacter screen (int (+ x 2)) (int (+ row 4))))
                    spacer-glyph (.getCharacterString
                                   (.getBackCharacter screen (int x) (int (+ row 3))))]

                (expect (= selected-title-bg selected-meta-bg selected-hit-bg inactive-bg))
                (expect (= " " spacer-glyph))))
            (finally (.stopScreen screen)))))
    (it "highlight segments bold only the case-insensitive needle occurrences"
        (let [segs (var-get #'dlg/navigator-highlight-segments)]
          (expect (= [["a " false] ["Search" true] [" b" false]] (segs "a Search b" "search")))
          (expect (= [["no needle here" false]] (segs "no needle here" "")))))
    (it "visible rows are project-grouped instead of table-shaped"
        (let [all-rows (var-get #'dlg/navigator-all-rows)
              visible-rows (var-get #'dlg/navigator-visible-rows)
              rows (all-rows {:active-session-id "s1" :sessions sessions})
              visible (visible-rows rows "" {})]

          (expect (= 1 (count (filter :group-start? visible))))
          (expect (= [2 2] (mapv :group-count visible)))
          (expect (= 1 (count (visible-rows rows "second" #{}))))
          (expect (= 2 (count visible)))))
    (it "transcript lookup never blocks the typing thread"
        (let [schedule! (var-get #'dlg/schedule-navigator-search!)
              task (atom nil)
              generation (atom 0)
              result (atom nil)
              entered (promise)
              release (promise)
              _ (future (Thread/sleep 500) (deliver release true))
              started (System/nanoTime)]

          (schedule! task
                     generation
                     result
                     "needle"
                     (fn [query]
                       (deliver entered query)
                       @release
                       {"s2" {:kind :reply}}))
          (let [elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
            (expect (< elapsed-ms 250.0)))
          (expect (= "needle" (deref entered 1000 ::timeout)))
          (deliver release true)
          (loop [attempts 50]
            (when (and (nil? @result) (pos? attempts)) (Thread/sleep 20) (recur (dec attempts))))
          (expect (= {"s2" {:kind :reply}} (:matches @result)))
          (when-let [running @task]
            (future-cancel running))))))

(defdescribe
  navigator-search-debounce-test
  ;; The picker asks the gateway, and the gateway ranks a whole store per
  ;; query. Typing `stale-proof` must cost ONE lookup, and the answer to a
  ;; prefix the user has already replaced must never win a race against it.
  (it
    "a superseded query never lands on the one the user is looking at"
    (let [schedule!
          (var-get #'dlg/schedule-navigator-search!)

          task
          (atom nil)

          generation
          (atom 0)

          result
          (atom nil)

          entered
          (promise)

          release
          (promise)

          ;; Blocks like a slow gateway AND survives the cancel, so what is
          ;; under test is the generation guard rather than the interrupt.
          stale-fn
          (fn [_q]
            (loop []

              (when-not (realized? release)
                (deliver entered true)
                (try (Thread/sleep 5) (catch InterruptedException _))
                (recur)))
            {"stale" {:kind :reply}})

          first-token
          (schedule! task generation result "sta" stale-fn)]

      (expect (= true (deref entered 2000 ::timeout)))
      ;; The user keeps typing while that lookup is in flight.
      (let [second-token (schedule! task
                                    generation
                                    result
                                    "stale-proof"
                                    (fn [_q]
                                      {"fresh" {:kind :title}}))]
        (expect (< (long first-token) (long second-token))))
      (loop [attempts 100]
        (when (and (nil? @result) (pos? attempts)) (Thread/sleep 20) (recur (dec attempts))))
      (expect (= "stale-proof" (:query @result)))
      (expect (= {"fresh" {:kind :title}} (:matches @result)))
      ;; Now let the superseded lookup finish: it is discarded, not painted.
      (deliver release true)
      (Thread/sleep 200)
      (expect (= "stale-proof" (:query @result)))
      (expect (= {"fresh" {:kind :title}} (:matches @result)))
      (when-let [running @task]
        (future-cancel running))))
  (it
    "an emptied query cancels the lookup instead of searching for nothing"
    (let [schedule!
          (var-get #'dlg/schedule-navigator-search!)

          task
          (atom nil)

          generation
          (atom 0)

          result
          (atom nil)

          asked
          (atom 0)]

      (schedule! task
                 generation
                 result
                 "needle"
                 (fn [_q]
                   (swap! asked inc)
                   {}))
      (schedule! task
                 generation
                 result
                 "   "
                 (fn [_q]
                   (swap! asked inc)
                   {}))
      (expect (nil? @task))
      (Thread/sleep 300)
      (expect (zero? @asked))
      (expect (nil? @result)))))

(defdescribe scrollbar-geometry-test
             (it "scrollbar geometry sanity (canonical primitive)"
                 ;; Canonical primitive: 20 items in a 10-row viewport, scroll=5
                 ;; ⇒ 1-cell thumb halfway down the 10-row track. Overflow gone
                 ;; when total ≤ inner (3 items in a 10-row view).
                 (let [scrollbar-geom
                       (requiring-resolve 'com.blockether.vis.ext.channel-tui.scrollbar/geometry)

                       g
                       (scrollbar-geom 20 10 5)]

                   (expect (= 1 (:thumb-h g)))
                   (expect (= 10 (:track-h g)))
                   (expect (= 10 (:max-scroll g)))
                   (expect (= 4 (:thumb-top-rel g)))
                   (expect (nil? (scrollbar-geom 3 10 0))))))

(defdescribe settings-dialog-footprint-and-indent-test
             (it "shared dialogs use the same footprint as settings"
                 (let [settings-content-width
                       (var-get #'dlg/settings-content-width)

                       settings-content-height
                       (var-get #'dlg/settings-content-height)

                       theme-picker-content-width
                       (var-get #'dlg/theme-picker-content-width)]

                   (expect (= (dlg/default-content-width 160) (settings-content-width 160)))
                   (expect (= (dlg/default-content-height 50) (settings-content-height 50)))
                   (expect (= (settings-content-width 160) (theme-picker-content-width 160)))
                   (expect (<= (+ (dlg/default-content-width 60) 4) 60))
                   (expect (<= (+ (dlg/default-content-height 16) 6) 16))))
             (it "extension headings are flush; options are indented by renderer"
                 (let [settings-subsection-text (var-get #'dlg/settings-subsection-text)]
                   (expect (= "◆ Exa" (settings-subsection-text "Exa" 80))))))

(defdescribe
  apply-settings-option-test
  (it "toggle rows flip booleans"
      (let [apply-settings-option (var-get #'dlg/apply-settings-option)]
        (expect (= {:show-thinking false}
                   (apply-settings-option {:show-thinking true}
                                          {:key :show-thinking :type :toggle})))))
  (it "registry-toggle rows route through the toggles registry, not the local settings map"
      ;; Use a throwaway test toggle so we don't disturb the canonical
      ;; host toggles. Settings map stays UNTOUCHED: registry rows are
      ;; side-effecting and the apply path returns `values` unchanged.
      (let [apply-settings-option
            (var-get #'dlg/apply-settings-option)

            settings-row-mark
            (var-get #'dlg/settings-row-mark)

            id
            "dialogs_test_registry_row"

            _
            (vis/register-toggle! {:id id :label "Test" :default false})]

        (try (expect (false? (vis/toggle-enabled? id)))
             (let [out (apply-settings-option {:something "else"}
                                              {:type :registry-toggle :toggle-id id})]
               (expect (= {:something "else"} out))
               (expect (true? (vis/toggle-enabled? id))))
             ;; Boolean state is now carried by the leading status glyph (●/○), not
             ;; "(on)/(off)" text in the label.
             (let [[on-glyph] (settings-row-mark {:type :registry-toggle :toggle-id id} {})]
               (expect (= "●" on-glyph)))
             (vis/toggle-reset-to-default! id)
             (let [[off-glyph] (settings-row-mark {:type :registry-toggle :toggle-id id} {})]
               (expect (= "○" off-glyph)))
             (finally (vis/toggle-reset-to-default! id)))))
  ;; NOTE: the old "registry rows normalize fallback labels instead of
  ;; leaking raw ids" case was retired — the toggles registry now
  ;; REQUIRES a :label (register-toggle! rejects label-less specs), so the
  ;; id-derived fallback-label path no longer exists.
  (it "registry enum rows cycle through the toggles registry"
      (let [apply-settings-option
            (var-get #'dlg/apply-settings-option)

            settings-option-label
            (var-get #'dlg/settings-option-label)

            id
            "dialogs_test_registry_enum"]

        (vis/register-toggle!
          {:id id :label "Enum Test" :type :enum :choices [:low :medium :high] :default :low})
        (try (expect (= "Enum Test: low"
                        (settings-option-label
                          {:type :registry-toggle :toggle-id id :label "Enum Test"}
                          {})))
             (let [out (apply-settings-option {:something "else"}
                                              {:type :registry-toggle :toggle-id id})]
               (expect (= {:something "else"} out))
               (expect (= "medium" (vis/toggle-value id)))
               (expect (= "Enum Test: medium"
                          (settings-option-label
                            {:type :registry-toggle :toggle-id id :label "Enum Test"}
                            {}))))
             (finally (vis/toggle-reset-to-default! id)))))
  (it "choice rows cycle quick -> balanced -> deep -> quick"
      (let [apply-settings-option (var-get #'dlg/apply-settings-option)]
        (expect (= {:reasoning-level :balanced}
                   (apply-settings-option
                     {:reasoning-level :quick}
                     {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
        (expect (= {:reasoning-level :quick}
                   (apply-settings-option
                     {:reasoning-level :deep}
                     {:key :reasoning-level :type :choice :choices [:quick :balanced :deep]})))
        (expect (= {:verbosity :high}
                   (apply-settings-option
                     {:verbosity :medium}
                     {:key :verbosity :type :choice :choices [:low :medium :high]})))))
  (it "choice labels surface the live value"
      (let [settings-option-label (var-get #'dlg/settings-option-label)]
        (expect (= "Reasoning effort: deep"
                   (settings-option-label {:key :reasoning-level
                                           :type :choice
                                           :choices [:quick :balanced :deep]
                                           :label "Reasoning effort"}
                                          {:reasoning-level :deep})))
        (expect
          (= "Verbosity: high"
             (settings-option-label
               {:key :verbosity :type :choice :choices [:low :medium :high] :label "Verbosity"}
               {:verbosity :high})))))
  (it "choice labels do not crash when row also carries a nil name field"
      (let [settings-option-label (var-get #'dlg/settings-option-label)]
        (expect (= "Reasoning effort: quick"
                   (settings-option-label {:key :reasoning-level
                                           :type :choice
                                           :choices [:quick :balanced :deep]
                                           :label "Reasoning effort"
                                           :name nil}
                                          {})))))
  (it "settings row activation notifies on-change without redrawing behind the modal"
      (let [activate-settings-row!
            (var-get #'dlg/activate-settings-row!)

            values
            (atom {:show-timestamps false})

            changed
            (atom nil)

            calls
            (atom [])]

        (activate-settings-row! nil
                                nil
                                nil
                                values
                                {:on-change #(do (reset! changed %) (swap! calls conj [:change %]))
                                 :redraw-ui #(swap! calls conj [:redraw @values])}
                                {:key :show-timestamps :type :toggle})
        (expect (= {:show-timestamps true} @values))
        (expect (= {:show-timestamps true} @changed))
        (expect (= [[:change {:show-timestamps true}]] @calls))))
  (it
    "settings descriptions wrap into paint rows instead of truncating inline"
    (let
      [settings-render-entries
       (var-get #'dlg/settings-render-entries)

       rows
       [{:type :section :label "Terminal UI"}
        {:key :show-thinking
         :type :toggle
         :label "Show model thinking"
         :description
         "Stream reasoning deltas inside each iteration bubble without collapsing this text into ellipsis."}]

       entries
       (settings-render-entries rows 16)]

      (expect (< 2 (count entries)))
      (expect (some #(= :option-desc (:part %)) entries))
      (expect (every? #(not (str/includes? (str (:text %)) "...")) entries))))
  (it "an info row is a head line + its own body; an inline state never wraps"
      (let [settings-render-entries
            (var-get #'dlg/settings-render-entries)

            entries
            (settings-render-entries
              [{:type :info :tone :bad :label "MCP unavailable" :description "connection refused"}
               {:type :mcp
                :label "filesystem"
                :description "connected · 12 tools"
                :inline-description true}]
              24)]

        ;; section prose is a bold head line plus a dim body — never label and
        ;; description glued into one run-on sentence
        (expect (= [{:row-idx 0 :part :info-line :text "MCP unavailable" :head? true}
                    {:row-idx 0 :part :info-line :text "connection refused"}
                    {:row-idx 1 :part :option}]
                   entries))))
  (it "theme picker rows label registered themes"
      (let [theme-picker-items (var-get #'dlg/theme-picker-items)]
        (expect (= [{:theme-id :vis-dark :label "Vis Dark"}
                    {:theme-id :vis-light :label "Vis Light"}]
                   (theme-picker-items [:vis-dark :vis-light])))))
  (it
    "Settings is ONE flat list (no tabs): Terminal UI + grouped toggles + Models"
    (let [settings-rows (var-get #'dlg/settings-rows)]
      (with-redefs [vis/registered-extensions (constantly [])
                    vis/get-router (constantly nil)]

        (let [rows (settings-rows)
              sections (->> rows
                            (filter #(= :section (:type %)))
                            (mapv :label))]

          ;; flat list, web-shaped: Terminal UI chrome always present. The
          ;; Models section was retired (it only carried reasoning-effort,
          ;; which moved to Ctrl+R).
          (expect (some #{"Terminal UI"} sections))
          (expect (not-any? #{"Models"} sections))
          (expect (some #(= :theme-name (:key %)) rows))
          ;; vis-dark/light are pinned to the TOP; blockether + solarized
          ;; themes follow, sorted by id.
          (expect (= [:vis-light :vis-dark :blockether-dark :blockether-light :solarized-dark
                      :solarized-light]
                     (:choices (first (filter #(= :theme-name (:key %)) rows)))))
          ;; Mouse auto-copy is now ALWAYS ON (`:settings? false`) — out of Settings.
          (expect (not-any? #(= :mouse-selection-copy (:key %)) rows))
          (expect (not-any? #(= "mouse_selection_copy" (:toggle-id %)) rows))
          ;; Network access is ALWAYS ON too — also out of Settings.
          (expect (not-any? #(= "network_enabled" (:toggle-id %)) rows))
          ;; Shell + harness verbs are always on now (no user toggle); the retired
          ;; display gates (show-thinking/iterations/silent/timestamps) and the
          ;; own-control knobs (reasoning-effort :settings? false) stay out.
          (expect (not-any? #(= "shell_enabled" (:toggle-id %)) rows))
          (expect (not-any? #(= "show_thinking" (:toggle-id %)) rows))
          (expect (not-any? #(= "reasoning_level" (:toggle-id %)) rows))
          ;; toggles group by :group now — no single "Feature Toggles" bucket;
          ;; with no declared extensions there is no "Extension Settings" section
          (expect (not-any? #{"Feature Toggles"} sections))
          (expect (not-any? #{"Extension Settings"} sections))))))
  (it "registered extension themes appear in the channel Theme setting"
      (let [settings-rows
            (var-get #'dlg/settings-rows)

            settings-option-label
            (var-get #'dlg/settings-option-label)]

        (try (vis/register-themes! {"THEME_NAME" {"PADDING" "0px"}})
             (with-redefs [vis/get-router (constantly nil)]
               (let [row (first (filter #(= :theme-name (:key %)) (settings-rows)))]
                 (expect (= [:vis-light :vis-dark :THEME_NAME :blockether-dark :blockether-light
                             :solarized-dark :solarized-light]
                            (:choices row)))
                 (expect (= "Theme: THEME_NAME"
                            (settings-option-label row {:theme-name :THEME_NAME})))))
             (finally (vis/reset-themes!)))))
  (it
    "extension-declared env vars render their external source under Extensions / Exa"
    (let [settings-rows
          (var-get #'dlg/settings-rows)

          settings-option-label
          (var-get #'dlg/settings-option-label)]

      (with-redefs [vis/get-router
                    (constantly nil)

                    vis/registered-extensions
                    (fn []
                      [{:ext/name "test.ext"
                        :ext/engine {:ext.engine/alias 'exa}
                        :ext/env [{:name "EXA_API_KEY"
                                   :label "Exa API key"
                                   :description "Optional key."
                                   :secret? true}]}])

                    vis/extension-env-status
                    (fn [name]
                      {:name name :source :dotenv :value "secret"})]

        (let [rows
              (settings-rows)

              row
              (first (filter #(= [:environment "EXA_API_KEY"] (:id %)) rows))]

          (expect (= "Extension Settings"
                     (->> rows
                          (filter #(= :section (:type %)))
                          (mapv :label)
                          last)))
          (expect (= ["Exa"]
                     (->> rows
                          (filter #(= :subsection (:type %)))
                          (mapv :label))))
          (expect (= :env-var (:type row)))
          (expect (= "Exa API key: set in .env" (settings-option-label row {})))
          (expect (false? ((var-get #'dlg/settings-selectable?) row)))))))
  (it
    "retired extension setting declarations are dropped, registry owns the rows"
    (let [settings-rows (var-get #'dlg/settings-rows)]
      (with-redefs [vis/get-router (constantly nil)
                    ;; hermetic: the Codex knob's :visible-fn consults the
                    ;; CONFIGURED providers — pin "none" so the assertion
                    ;; below can't flip on a dev machine that has Codex.
                    vis/has-provider? (constantly false)
                    vis/registered-extensions
                    (fn []
                      [{:ext/name "voice"
                        :ext/settings
                        [{:key :voice/tui-auto-read? :type :toggle :label "TUI auto-read"}]}
                       {:ext/name "provider-openai-codex"
                        :ext/providers [{:provider/id :openai-codex :provider/label "OpenAI Codex"}]
                        :ext/settings [{:key :verbosity
                                        :type :choice
                                        :choices [:low :medium :high]
                                        :label "Verbosity"}]}])]

        (let [rows (settings-rows)
              ids (set (map :id rows))
              toggles (set (keep :toggle-id rows))]

          ;; Reasoning-effort has its OWN control (Ctrl+R) — `:settings? false`
          ;; keeps it registered but out of the Settings dialog.
          (expect (not (contains? toggles "reasoning_level")))
          ;; Verbosity has its OWN control too (Ctrl+X l and the footer chip),
          ;; so it is `:settings? false` for the same reason and must not appear
          ;; here — nor may a provider extension's legacy declaration of the
          ;; same key smuggle a second copy of it back in.
          (expect (not (contains? toggles "verbosity")))
          (expect (contains? ids [:extension-setting "voice" :voice/tui-auto-read?]))
          (expect (not (contains? ids [:extension-setting "provider-openai-codex" :verbosity])))))))
  (it "provider-declared legacy settings are ignored"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs [vis/get-router (constantly nil)
                      vis/registered-extensions
                      (fn []
                        [{:ext/name "provider-openai-codex"
                          :ext/providers [{:provider/id :openai-codex
                                           :provider/label "OpenAI Codex (ChatGPT OAuth)"}]
                          :ext/settings [{:key :verbosity
                                          :type :choice
                                          :choices [:low :medium :high]
                                          :label "Verbosity"
                                          :description "Output detail."}]}])]

          (let [rows (settings-rows)]
            (expect (not-any? #(= [:extension-setting "provider-openai-codex" :verbosity] (:id %))
                              rows))))))
  (it "active Z.ai hides reasoning effort and Codex-only provider settings"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs [vis/get-router (constantly :router)
                      vis/resolve-effective-model (fn [_]
                                                    {:provider :zai
                                                     :name "glm-4.7"
                                                     :reasoning? true
                                                     :reasoning-style :zai-thinking
                                                     :reasoning-effort? false})
                      vis/registered-extensions
                      (fn []
                        [{:ext/name "provider-openai-codex"
                          :ext/providers [{:provider/id :openai-codex
                                           :provider/label "OpenAI Codex (ChatGPT OAuth)"}]
                          :ext/settings [{:key :verbosity
                                          :type :choice
                                          :choices [:low :medium :high]
                                          :label "Verbosity"
                                          :description "Output detail."}]}])]

          (let [rows (settings-rows)]
            ;; Reasoning-effort + verbosity are OUT of Settings entirely now
            ;; (own controls); no "unavailable" placeholder either.
            (expect (not-any? #(= :reasoning-level (:key %)) rows))
            (expect (not-any? #(= "Reasoning effort unavailable" (:label %)) rows))
            (expect (not-any? #(= :verbosity (:key %)) rows))))))
  (it "channel-declared settings render under Channel Settings, once, in the flat list"
      (let [settings-rows (var-get #'dlg/settings-rows)]
        (with-redefs [vis/get-router (constantly nil)
                      vis/registered-extensions
                      (fn []
                        [{:ext/name "channel-example"
                          :ext/channels [{:channel/id :example :channel/cmd "example"}]
                          :ext/settings [{:key :example-notify
                                          :type :toggle
                                          :label "Example notifications"
                                          :description "Send channel notifications."}]}])]

          (let [rows (settings-rows)
                row-id [:extension-setting "channel-example" :example-notify]
                row (first (filter #(= row-id (:id %)) rows))]

            (expect (contains? (set (->> rows
                                         (filter #(= :section (:type %)))
                                         (mapv :label)))
                               "Channel Settings"))
            (expect (= ["Example"]
                       (->> rows
                            (filter #(= :subsection (:type %)))
                            (mapv :label))))
            (expect (= :toggle (:type row)))
            ;; appears exactly once — no tab duplicated it
            (expect (= 1 (count (filter #(= row-id (:id %)) rows))))))))
  (it
    "session picker keeps new/fork out of the table and renders justified cells"
    (let [session-items
          dlg/session-dialog-items

          body-w
          96

          header
          (dlg/session-dialog-header body-w)

          rows
          (session-items [{"id" "123e4567-e89b-12d3-a456-426614174000"
                           "title" (str "Title " (apply str (repeat 80 "汉")))
                           "turn_count" 2
                           "fork_count" 3
                           "modified_at" #inst "2024-01-03T04:05:00.000-00:00"
                           "created_at" #inst "2024-01-01T01:02:00.000-00:00"}
                          {"id" "abcdef00-e89b-12d3-a456-426614174000"
                           "title" ""
                           "turn_count" 0
                           "modified_at" nil
                           "created_at" #inst "2024-01-02T01:02:00.000-00:00"}]
                         "123e4567-e89b-12d3-a456-426614174000"
                         body-w)

          active-label
          (:label (nth rows 0))

          inactive-label
          (:label (nth rows 1))

          fork-label
          (dlg/session-dialog-label {"id" "fedcba00-e89b-12d3-a456-426614174000"
                                     "title" "Forkable"
                                     "turn_count" 4
                                     "fork_count" 3
                                     "modified_at" #inst "2024-01-04T04:05:00.000-00:00"
                                     "created_at" #inst "2024-01-01T01:02:00.000-00:00"}
                                    nil
                                    body-w)]

      (expect (= [:switch :switch] (mapv :action rows)))
      (expect (not-any? #{:new :fork} (map :action rows)))
      (expect (= [] (session-items [] nil body-w)))
      (expect (= [body-w body-w body-w body-w]
                 (mapv p/display-width [header active-label inactive-label fork-label])))
      (expect (every? #(str/includes? % "│") [header active-label inactive-label]))
      (expect (str/includes? header "ID"))
      (expect (str/includes? header "Turns"))
      ;; The active marker sits in the gutter column. `●` (U+25CF) renders in
      ;; its own cell (` ● `); inactive rows leave that column blank. Assert
      ;; the marker *cell* (between the first two │) rather than the raw glyph.
      (expect (= " ● " (second (str/split active-label #"│"))))
      (expect (= "   " (second (str/split inactive-label #"│"))))
      (expect (str/includes? active-label "│ 123e4567 │"))
      (expect (str/includes? active-label "│     2 │"))
      (expect (str/includes? active-label "2024-01-03"))
      (expect (str/includes? active-label "04:05"))
      (expect (str/includes? active-label "2024-01-01"))
      (expect (str/includes? active-label "01:02"))
      (expect (str/includes? active-label "Title"))
      (expect (str/includes? fork-label "[forks:3]"))
      (expect (str/includes? active-label "…"))
      (expect (str/includes? inactive-label "│ abcdef00 │"))
      (expect (str/includes? inactive-label "│     0 │"))
      (expect (str/includes? inactive-label "-"))
      (expect (str/includes? inactive-label "Untitled session"))))
  (it
    "the draft band is a TRANSIENT: one key per verb, and a `/draft …` slash is one of them already pressed"
    ;; Drafts used to be a modal picker, then a text-input modal, then a confirm
    ;; modal — three windows stacked over the very session the draft belongs to.
    ;; It is one band inside the session's frame now, exactly like the HITL form.
    (let [drafts
          [{"workspace_id" "ws-a" "label" "feature-a" "is_current" true}
           {"workspace_id" "ws-b" "label" "feature-b" "is_current" false}]

          ch
          (fn [c]
            (KeyStroke. (Character/valueOf (char c)) false false false))

          band!
          ;; `pressed` is the band command a slash already named; nil is the band
          ;; opening as itself and reading every key from the human.
          (fn [rows pressed keys]
            (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
                  (term/virtual-screen)]
              (try (doseq [k keys]
                     (.addInput terminal k))
                   (dlg/draft-transient! screen {:content-top 1 :prompt-h 3} rows pressed)
                   (finally (.stopScreen screen)))))]

      ;; Switching is its OWN command: `s` opens a second band over the same
      ;; rows, where a parked draft carries its own key — no cursor, no Enter.
      (expect (= {:action :draft :workspace-id "ws-b" :label "feature-b" :current? false}
                 (band! drafts nil [(ch \s) (ch \b)])))
      ;; `t` is always trunk, and it knows that is not where we are.
      (expect (= {:action :trunk :label "Trunk" :current? false}
                 (band! drafts nil [(ch \s) (ch \t)])))
      ;; Creating is two commands rather than a command plus an armed flag:
      ;; `c` forks the committed HEAD, `d` carries the working tree along, and
      ;; either way the name is read INLINE on the hint row.
      (expect (= {:action :new :clean? true :label "wire-rework"}
                 (band! drafts
                        nil
                        (concat [(ch \c)] (map ch "wire-rework") [(KeyStroke. KeyType/Enter)]))))
      (expect (= {:action :new :clean? false :label "wire-rework"}
                 (band! drafts
                        nil
                        (concat [(ch \d)] (map ch "wire-rework") [(KeyStroke. KeyType/Enter)]))))
      ;; `/draft new` used to pop a text-input WINDOW for the label. It is the
      ;; band's own `d`, already pressed: same band, same inline question, no
      ;; keystroke to repeat.
      (expect
        (= {:action :new :clean? false :label "wire-rework"}
           (band! drafts :new-dirty (concat (map ch "wire-rework") [(KeyStroke. KeyType/Enter)]))))
      (expect
        (= {:action :new :clean? true :label "wire-rework"}
           (band! drafts :new-clean (concat (map ch "wire-rework") [(KeyStroke. KeyType/Enter)]))))
      (expect (= {:action :draft :workspace-id "ws-b" :label "feature-b" :current? false}
                 (band! drafts :switch [(ch \b)])))
      ;; A command the band does not offer right now (`/draft resume` with no
      ;; drafts) opens the band itself rather than firing something the human
      ;; was never shown — `c` still means `c`.
      (expect (= {:action :new :clean? true :label "x"}
                 (band! [] :switch (concat [(ch \c)] (map ch "x") [(KeyStroke. KeyType/Enter)]))))))
  (it "command palette exposes the frequent app verbs; Providers is the provider/settings hub"
      (let [palette-commands
            (var-get #'dlg/palette-commands)

            labels
            (mapv :label palette-commands)

            ids
            (set (mapv :id palette-commands))]

        ;; Providers and Settings remain separate, explicit palette verbs.
        (expect (some #{"Providers"} labels))
        (expect (some #{"Settings"} labels))
        (expect (not (some #{"Configure Providers"} labels)))
        (expect (contains? ids :providers))
        (expect (contains? ids :settings))
        ;; The palette is THE entry point (Ctrl+P) for the verbs whose Alt chords
        ;; don't survive macOS — so the frequent ones must be present + runnable.
        (expect (every? ids
                        [:search-open :show-sessions :pick-file :new-session :new-session-in
                         :fork-session]))
        (expect (not (some ids
                           [:cycle-model :pick-model :cycle-reasoning :cycle-verbosity :open-drafts
                            :open-magit])))
        (expect (not (contains? ids :open-resources)))))
  (it "a turnless session hides BOTH fork verbs from the palette"
      ;; Forking a session with no turns is prohibited, so it must not even be
      ;; discoverable: `palette-commands-for` drops the `:has-turns` entries.
      (let [ids-for
            (fn [ctx]
              (set (mapv :id (dlg/palette-commands-for ctx))))

            fresh
            (ids-for {:has-turns? false})

            with-turns
            (ids-for {:has-turns? true})]

        (expect (not (contains? fresh :fork-session)))
        (expect (not (contains? fresh :fork-at-turn)))
        (expect (contains? with-turns :fork-session))
        (expect (contains? with-turns :fork-at-turn))
        ;; nil ctx = the conservative turnless case
        (expect (= fresh (ids-for nil)))
        ;; gating touches ONLY the fork verbs
        (expect (= #{:fork-session :fork-at-turn} (set (remove fresh with-turns))))
        (expect (contains? fresh :new-session))))
  (it "command palette filters by a typed query (searchable)"
      ;; The palette is searchable: the filter is a case-insensitive substring
      ;; match on :label, the spine `searchable-select!` applies.
      (let [labels
            (mapv :label (var-get #'dlg/palette-commands))

            match
            (fn [q]
              (filterv #(clojure.string/includes? (clojure.string/lower-case %)
                                                  (clojure.string/lower-case q))
                labels))]

        (expect (some #{"Switch Session"} (match "session")))
        (expect (= [] (match "zzz-no-such-command"))))))

(defdescribe fork-turn-items-test
             (it "builds filterable palette rows: message label, tN hint, turn-id, truncation"
                 (let [turns
                       [{:id "s1" :position 1 :user-request "  first   question here  "}
                        {:id "s2" :position 2 :user-request (apply str (repeat 200 "x"))}
                        {:id "s3" :position 3 :user-request "   "}]

                       rows
                       (dlg/fork-turn-items turns)]

                   ;; each row carries the soul id the fork copies THROUGH
                   (expect (= ["s1" "s2" "s3"] (mapv :turn-id rows)))
                   ;; ordinal hint
                   (expect (= ["t1" "t2" "t3"] (mapv :hint rows)))
                   ;; whitespace collapsed for the searchable label
                   (expect (= "first question here" (:label (first rows))))
                   ;; long messages truncated with an ellipsis
                   (expect (<= (count (:label (second rows))) 72))
                   (expect (clojure.string/ends-with? (:label (second rows)) "…"))
                   ;; blank message gets a placeholder
                   (expect (= "(no message)" (:label (nth rows 2)))))))

;; Regression, issue #session-list-work-dir: project names used to split one working directory into separate groups.
;; Navigator hierarchy: the working directory comes first, regardless of project name.
;; A project name is metadata, not a second place to put the same directory.
(defdescribe
  navigator-work-dir-grouping-test
  (it
    "keeps the focused work dir first and every work dir contiguous"
    (let [all-rows
          (var-get #'dlg/navigator-all-rows)

          sessions
          [{"id" "s1"
            "title" "A1"
            "project_name" "named-a"
            "turn_count" 1
            "created_at" 0
            "modified_at" 4000
            :work-dir "~/proj-a"}
           {"id" "s2"
            "title" "B1"
            "project_name" "named-b"
            "turn_count" 1
            "created_at" 0
            "modified_at" 3000
            :work-dir "~/proj-b"}
           {"id" "s3"
            "title" "A2"
            "project_name" "another-name-for-a"
            "turn_count" 1
            "created_at" 0
            "modified_at" 2000
            :work-dir "~/proj-a"}
           {"id" "s4"
            "title" "B2"
            "project_name" "another-name-for-b"
            "turn_count" 1
            "created_at" 0
            "modified_at" 1000
            :work-dir "~/proj-b"}]

          rows
          (all-rows {:active-session-id "s1" :sessions sessions})]

      ;; Focused work dir A is first and remains one coherent group, followed by B.
      (expect (= ["s1" "s3" "s2" "s4"] (mapv (comp str :id :target) rows)))))
  (it "sessions without a work-dir share one group and keep recency order"
      (let [all-rows
            (var-get #'dlg/navigator-all-rows)

            sessions
            [{"id" "s1" "title" "A" "turn_count" 1 "created_at" 0 "modified_at" 3000}
             {"id" "s2" "title" "B" "turn_count" 1 "created_at" 0 "modified_at" 2000}
             {"id" "s3" "title" "C" "turn_count" 1 "created_at" 0 "modified_at" 1000}]

            rows
            (all-rows {:active-session-id "s1" :sessions sessions})]

        (expect (= ["s1" "s2" "s3"] (mapv (comp str :id :target) rows))))))

(defdescribe
  fit-hint-pairs-test
  "The hint bar must CLIP to the dialog's content width by dropping whole
   trailing chords — `put-str!` clips to the screen, not the box, so an
   unfitted footer (e.g. magit's 162-col one) would paint across the border."
  (it "returns all pairs when they fit exactly"
      (let [hints
            (var-get #'dlg/magit-hints)

            w
            (dlg/hint-bar-width hints)]

        (expect (= hints (dlg/fit-hint-pairs hints w)))))
  (it "drops whole trailing pairs when the bar is too wide"
      (let [hints
            (var-get #'dlg/magit-hints)

            fitted
            (dlg/fit-hint-pairs hints 112)]

        (expect (< (count fitted) (count hints)))
        (expect (= fitted (subvec (vec hints) 0 (count fitted))))
        (expect (<= (dlg/hint-bar-width (vec fitted)) 112))))
  (it "never exceeds text-w at any width (whole-pair invariant)"
      (let [hints (var-get #'dlg/magit-hints)]
        (expect (every? (fn [tw]
                          (<= (dlg/hint-bar-width (vec (dlg/fit-hint-pairs hints tw))) (max tw 0)))
                        (range 0 200)))))
  (it "fits nothing into a sliver without blowing up"
      (expect (= [] (dlg/fit-hint-pairs (var-get #'dlg/magit-hints) 3)))
      (expect (= [] (dlg/fit-hint-pairs (var-get #'dlg/magit-hints) 0)))))

(defdescribe
  mcp-settings-section-test
  (it
    "MCP servers are settings ROWS — one toggle each, not a dialog of their own"
    (let [inventory
          (var-get #'dlg/mcp-inventory)

          mcp-rows
          (var-get #'dlg/mcp-settings-rows)

          mark
          (var-get #'dlg/settings-row-mark)

          selectable?
          (var-get #'dlg/settings-selectable?)

          original
          @inventory]

      (try
        ;; No gateway read yet → Settings stays MCP-free.
        (reset! inventory {:status :unloaded :servers [] :error nil})
        (expect (nil? (mcp-rows)))
        (reset! inventory
          {:status :ok
           :error nil
           :servers [{"name" "fs" "enabled" true "is_managed" true "is_connected" true "tools" 3}
                     {"name" "gh" "enabled" false "is_managed" true}
                     {"name" "hand" "enabled" true "is_managed" false "is_killed" true}]})
        (let [rows
              (mcp-rows)

              toggles
              (filterv #(= :mcp (:type %)) rows)]

          (expect (= [:section :mcp :mcp :mcp :action] (mapv :type rows)))
          (expect (= "MCP Servers" (:label (first rows))))
          (expect (= ["fs" "gh" "hand"] (mapv :label toggles)))
          (expect (= "connected · 3 tools" (:description (first toggles))))
          (expect (= :mcp-add (:id (last rows))))
          ;; a server's live status rides its own row, so the section reads as a
          ;; table instead of costing a wrapped description row per server
          (expect (every? :inline-description toggles))
          ;; on = enabled AND not killed, so a killed config-file server reads off
          (expect (= [p/STATUS_ON p/STATUS_OFF p/STATUS_OFF] (mapv #(first (mark % {})) toggles)))
          (expect (every? selectable? (remove #(= :section (:type %)) rows))))
        ;; A gateway that is down degrades to an inline row, never a modal.
        (reset! inventory {:status :error :servers [] :error "connection refused"})
        (expect (= [:section :info :action] (mapv :type (mcp-rows))))
        ;; the failure reads AS a failure: bad tone, so the head line paints red
        (expect (= :bad (:tone (second (mcp-rows)))))
        (finally (reset! inventory original)))))
  (it
    "settings-rows carries the MCP section and Settings can open focused on it"
    (let [settings-rows
          (var-get #'dlg/settings-rows)

          inventory
          (var-get #'dlg/mcp-inventory)

          initial-index
          (var-get #'dlg/settings-initial-index)

          original
          @inventory]

      (try (reset! inventory {:status :ok
                              :error nil
                              :servers [{"name" "fs" "enabled" true "is_managed" true}]})
           (with-redefs [vis/registered-extensions
                         (constantly [])

                         vis/get-router
                         (constantly nil)]

             (let [rows
                   (settings-rows)

                   row
                   (first (filter #(= :mcp (:type %)) rows))]

               (expect (some #{"MCP Servers"}
                             (->> rows
                                  (filter #(= :section (:type %)))
                                  (mapv :label))))
               (expect (= "fs" (:label row)))
               ;; the palette's MCP entry parks the cursor on the section's first row
               (expect (= "fs" (:label (nth rows (initial-index rows "MCP Servers")))))
               (expect (not= (initial-index rows "MCP Servers") (initial-index rows nil)))))
           (finally (reset! inventory original)))))
  (it
    "Enter on an MCP row runs that server's verbs as a transient band, not a toggle"
    (let [activate!
          (var-get #'dlg/activate-settings-row!)

          inventory
          (var-get #'dlg/mcp-inventory)

          original
          @inventory

          server
          {"name" "fs" "enabled" true "is_managed" true}

          spec
          (atom nil)

          fired
          (atom [])]

      (try (with-redefs-fn {#'dlg/embed-transient! (fn [_screen _g _region s]
                                                     (reset! spec s)
                                                     {:action :kill})}
             (fn []
               (with-redefs [vis/gateway-mcp-servers (constantly [server])]
                 (reset! inventory {:status :unloaded :servers [] :error nil})
                 (activate! nil
                            nil
                            {:left 0 :inner-w 40 :hint-row 20 :text-w 38 :min-row 3}
                            (atom {})
                            {:mcp-action #(swap! fired conj %)}
                            {:type :mcp :label "fs" :server server})
                 ;; the band IS this server: its live status in the title, its own
                 ;; verbs grouped the way magit groups a popup
                 (expect (= "fs \u00b7 idle" (:title @spec)))
                 (expect (= ["Runtime" "Configuration" "Inspect"] (mapv :title (:groups @spec))))
                 ;; the picked verb reaches the manager verbatim — with the
                 ;; BAND it was fired from, so the questions it asks next
                 ;; (a confirm, a refusal) land in this frame and not in a window
                 (expect (= [{:server server :action :kill}]
                            (mapv #(select-keys % [:server :action]) @fired)))
                 (expect (= [{:left 0 :inner-w 40 :hint-row 20 :text-w 38 :min-row 3 :restore! nil}]
                            (mapv :region @fired)))
                 ;; and it changes what the row says, so the inventory is re-read
                 (expect (= :ok (:status @inventory))))))
           (finally (reset! inventory original))))))

;; `transient-dialog!`: a magit popup hosted in its OWN modal.
;;
;; This replaced the API-key prompt that painted a full-screen vis logo above a
;; text box. The caller's guidance stays visible, the key is read INLINE on the
;; hint row, and the armed credential renders as dots — never as text.

(defdescribe
  transient-dialog-test
  (it
    "reads a masked credential inline and submits it without echoing it"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
          (term/virtual-screen)

          spec
          {:title "Sign in"
           :groups [{:title "Credential"
                     :items [{:key "k"
                              :type :option
                              :id :api-key
                              :label "API key"
                              :prompt "API key:"
                              :mask \*
                              :secret? true}]}
                    {:title "Authenticate"
                     :items
                     [{:key "a" :type :action :id :submit :label "Sign in with this key"}]}]}]

      (doseq [c [\k \s \k \- \1]]
        (.addInput terminal (term/keystroke c)))
      (.addInput terminal (KeyStroke. KeyType/Enter))
      (.addInput terminal (term/keystroke \a))
      (let [ret
            (dlg/transient-dialog! screen "Z.AI Authentication" ["Paste your key."] spec)

            text
            (str/join "\n" (map :text (term/painted-rows terminal)))]

        (expect (= :submit (:action ret)))
        (expect (= "sk-1" (get-in ret [:options :api-key])))
        ;; The provider's guidance is still on screen above the popup.
        (expect (str/includes? text "Paste your key."))
        (expect (str/includes? text "Sign in with this key"))
        ;; The credential is dots, and the raw key is nowhere on the screen.
        (expect (str/includes? text "••••••"))
        (expect (not (str/includes? text "sk-1"))))))
  (it "Esc backs out of the popup without a value"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
            (term/virtual-screen)

            spec
            {:title "Sign in"
             :groups [{:title "Authenticate"
                       :items [{:key "a" :type :action :id :submit :label "Sign in"}]}]}]

        (.addInput terminal (term/keystroke :esc))
        (expect (nil? (dlg/transient-dialog! screen "Auth" ["Guidance."] spec))))))

(defdescribe
  provider-settings-section-test
  (it
    "providers are settings ROWS — auth, model, default — not a dialog of their own"
    (let [inventory
          (var-get #'dlg/provider-inventory)

          provider-rows
          (var-get #'dlg/provider-settings-rows)

          mark
          (var-get #'dlg/settings-row-mark)

          selectable?
          (var-get #'dlg/settings-selectable?)

          original
          @inventory]

      (try
        ;; No gateway read yet → Settings stays provider-free.
        (reset! inventory {:status :unloaded :providers [] :error nil})
        (expect (nil? (provider-rows)))
        (reset! inventory {:status :ok
                           :error nil
                           :providers
                           [{:provider {:id :anthropic :models [{:name "claude"}]}
                             :auth :on
                             :default? true}
                            {:provider {:id :openai :models ["gpt"]} :auth :off :default? false}
                            {:provider {:id :ollama :models []} :auth :local :default? false}]})
        (let [rows
              (provider-rows)

              providers
              (filterv #(= :provider (:type %)) rows)]

          (expect (= [:section :provider :provider :provider :action] (mapv :type rows)))
          (expect (= "Providers" (:label (first rows))))
          (expect (= :provider-add (:id (last rows))))
          ;; the row carries the provider itself, so Enter can open ITS menu
          (expect (= [:anthropic :openai :ollama] (mapv #(:id (:provider %)) providers)))
          ;; the router tag LEADS the line: `d`/`f` have no other mark on the row
          (expect (= "default · signed in · claude" (:description (first providers))))
          (expect (= "not signed in · gpt" (:description (second providers))))
          ;; the dot is the gateway's verdict; a local provider needs no credential
          (expect (= [p/STATUS_ON p/STATUS_OFF p/MARK_VALUE] (mapv #(first (mark % {})) providers)))
          (expect (every? selectable? (remove #(= :section (:type %)) rows))))
        ;; A gateway that is down degrades to an inline row, never a modal.
        (reset! inventory {:status :error :providers [] :error "connection refused"})
        (expect (= [:section :info :action] (mapv :type (provider-rows))))
        (finally (reset! inventory original)))))
  (it
    "settings-rows carries the Providers section and Settings can open focused on it"
    (let [settings-rows
          (var-get #'dlg/settings-rows)

          inventory
          (var-get #'dlg/provider-inventory)

          initial-index
          (var-get #'dlg/settings-initial-index)

          original
          @inventory]

      (try (reset! inventory {:status :ok
                              :error nil
                              :providers
                              [{:provider {:id :anthropic :models []} :auth :on :default? true}]})
           (with-redefs [vis/registered-extensions
                         (constantly [])

                         vis/get-router
                         (constantly nil)]

             (let [rows
                   (settings-rows)

                   row
                   (first (filter #(= :provider (:type %)) rows))]

               (expect (some #{"Providers"}
                             (->> rows
                                  (filter #(= :section (:type %)))
                                  (mapv :label))))
               (expect (= :anthropic (:id (:provider row))))
               ;; the palette's Providers entry parks the cursor on the first row
               (expect (= (:label row) (:label (nth rows (initial-index rows "Providers")))))
               (expect (not= (initial-index rows "Providers") (initial-index rows nil)))))
           (finally (reset! inventory original)))))
  (it
    "the fleet is config first, then authenticated presets, each with the gateway's verdict"
    (let [inventory
          (var-get #'dlg/provider-inventory)

          original
          @inventory

          router-reads
          (atom 0)]

      (try (with-redefs [vis/load-config
                         (constantly {:providers [{:id :anthropic :models ["claude-sonnet-4"]}
                                                  {:id :ollama}]
                                      :default-provider "anthropic"
                                      :default-model "claude-sonnet-4"
                                      :fallback-provider "ollama"
                                      :fallback-model "llama3"})

                         vis/authenticated-preset-providers
                         (constantly [{:id :openai :models ["gpt"]} {:id :anthropic :models []}])

                         vis/gateway-router-fleet
                         (fn []
                           (swap! router-reads inc)
                           [{"id" "anthropic" "status" {"is_authenticated" true}}
                            {"id" "openai" "status" {"is_authenticated" false}}])]

             (dlg/load-provider-inventory!)
             (let [{:keys [status providers]} @inventory]
               (expect (= :ok status))
               ;; a configured provider is NOT duplicated by its preset twin
               (expect (= [:anthropic :ollama :openai] (mapv #(:id (:provider %)) providers)))
               ;; a local provider needs no credential, so the gateway is never asked
               (expect (= [:on :local :off] (mapv :auth providers)))
               ;; …and it costs ONE gateway read for the WHOLE fleet, never one
               ;; per provider
               (expect (= 1 @router-reads))
               ;; the router selection travels WITH the entry — Settings can only
               ;; show what `d`/`f` did if the model comes back on the row
               (expect (= [true false false] (mapv :default? providers)))
               (expect (= ["claude-sonnet-4" nil nil] (mapv :default-model providers)))
               (expect (= [false true false] (mapv :fallback? providers)))
               (expect (= [nil "llama3" nil] (mapv :fallback-model providers)))))
           ;; a blown-up read is data, not a throw into the dialog loop
           (with-redefs [vis/load-config (fn []
                                           (throw (ex-info "boom" {})))]
             (dlg/load-provider-inventory!)
             (expect (= :error (:status @inventory)))
             (expect (= "boom" (:error @inventory))))
           (finally (reset! inventory original)))))
  ;; Regression (user report): the machine's ONLY provider showed as no default at
  ;; all. Settings read the raw `default_provider` key, which a fleet that was just
  ;; created has not got — so nothing was tagged until the user set it by hand,
  ;; while the router had been routing to that provider the whole time.
  (it "an untagged fleet still shows the provider the router would route to"
      (let [inventory
            (var-get #'dlg/provider-inventory)

            original
            @inventory]

        (try (with-redefs [vis/load-config
                           (constantly {:providers [{:id :acme-llm :models ["acme-1"]}]})

                           vis/authenticated-preset-providers
                           (constantly [])

                           vis/gateway-router-fleet
                           (constantly [{"id" "acme-llm" "status" {"is_authenticated" true}}])]

               (dlg/load-provider-inventory!)
               (let [{:keys [providers]} @inventory]
                 (expect (= [true] (mapv :default? providers)))
                 (expect (= ["acme-1"] (mapv :default-model providers)))))
             (finally (reset! inventory original)))))
  (it
    "Enter on a provider row runs THAT provider's transient, then re-reads the fleet"
    (let [activate!
          (var-get #'dlg/activate-settings-row!)

          inventory
          (var-get #'dlg/provider-inventory)

          opened
          (atom [])

          original
          @inventory

          region
          {:left 0 :inner-w 40 :hint-row 20 :text-w 38 :min-row 3}]

      (try (with-redefs [vis/load-config
                         (constantly {:providers [{:id :openai}]})

                         vis/authenticated-preset-providers
                         (constantly [])

                         vis/gateway-router-fleet
                         (constantly [{"id" "openai" "status" {"is_authenticated" false}}])]

             (reset! inventory {:status :unloaded :providers [] :error nil})
             (activate! nil
                        ::graphics
                        region
                        (atom {})
                        {:provider-transient #(swap! opened conj %)}
                        {:type :provider :provider {:id :openai}})
             ;; the transient paints INSIDE the settings frame, so it is handed
             ;; that frame's own graphics and geometry instead of opening a dialog
             (expect (= [{:provider-id :openai :g ::graphics :region region}] @opened))
             ;; signing in or picking a model changes what the row says: re-read it
             (expect (= :ok (:status @inventory)))
             (expect (= [:openai] (mapv #(:id (:provider %)) (:providers @inventory)))))
           (finally (reset! inventory original))))))

;;; ── One band, every question it can ask ─────────────────────────────────────

(defdescribe
  band-questions-test
  (it "binds every question a band can ask to that band's OWN region"
      ;; Each host used to unpack `:left`/`:inner-w`/`:hint-row`/`:text-w` again
      ;; and reach for `tr/run!` plus `transient-host` itself — five copies of
      ;; the same six coordinates, which is how two bands drift apart. The magit
      ;; status buffer, Settings, `transient-dialog!` and
      ;; the session band all compose THIS map now.
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try
          (let [g (.newTextGraphics screen)
                region {:left 2 :inner-w 40 :hint-row 20 :text-w 38}
                questions (dlg/band-questions screen g region)
                feed! (fn [& ks]
                        (doseq [k ks]
                          (.addInput
                            terminal
                            (if (= :enter k) (KeyStroke. KeyType/Enter) (term/keystroke k)))))]

            (expect (= #{:read! :choose! :confirm! :note! :wait! :transient! :read-option}
                       (set (keys questions))))
            ;; One typed answer, in THIS region's own band and nowhere else: the
            ;; prompt is the band's title (row 16) and the text is typed into a
            ;; field under it (row 18), directly above the region's hint row.
            (feed! \h \i :enter)
            (expect (= "hi" ((:read! questions) "Name:")))
            (let [grid (vec (term/grid terminal))]
              (expect (str/includes? (nth grid 16) "Name:"))
              (expect (str/includes? (nth grid 18) "hi")))
            ;; y/n and WHICH-one, same row, single key.
            (feed! \y)
            (expect (true? ((:confirm! questions) "Sure?")))
            (feed! \b)
            (expect (= :bb
                       ((:choose! questions)
                         "Which:"
                         [{:key \a :id :aa :label "A"} {:key \b :id :bb :label "B"}])))
            ;; A transient that opens a transient: the second band lands INSIDE
            ;; the first one's box (its rows start at the region's own left
            ;; edge), which is how magit asks a second thing without a second
            ;; frame.
            (feed! \x)
            (expect (= :ex
                       (:action ((:transient! questions)
                                  {:title "T"
                                   :groups [{:title "G"
                                             :items
                                             [{:key "x" :type :action :id :ex :label "Ex"}]}]}))))
            (expect (some #(str/starts-with? % "  │  x  Ex") (term/grid terminal)))
            ;; And the `:read-option` an OPTION item hands `tr/run!`, bound to
            ;; the same hint row.
            (feed! \z \z :enter)
            (expect (= "zz" ((:read-option questions) {:label "Token" :prompt "Token:"} nil))))
          (finally (.stopScreen screen))))))

;; Regression (reported from the TUI, screenshot of the draft band): a band asked
;; its follow-up question on its own hint row while its COMMAND rows stayed
;; painted right above it — `c`, `d`, `s` and `k` still advertised as verbs while
;; every one of those keys typed a letter into the draft's name — and the answer
;; itself was a bare label on the footer line instead of a field anyone could see
;; they were typing into.
(defdescribe
  band-question-test
  (it "a band's typed question REPLACES its commands and is drawn as an INPUT"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   region (assoc (tr/band-region 80 30 1) :restore! (dlg/frame-restorer screen))
                   rows [{"workspace_id" "ws-a" "label" "feature-a" "is_current" true}]
                   host (dlg/transient-host screen g)
                   _ (do (tr/paint! host region (drafts/spec rows) {:switches #{} :options {}})
                         ((:refresh! host)))
                   _ (doseq [k (concat (map term/keystroke "wire-rework")
                                       [(KeyStroke. KeyType/Enter)])]
                       (.addInput terminal k))
                   answer ((:read! (dlg/band-questions screen g region))
                            "Name the draft (with my changes):")
                   painted (str/join "\n" (map :text (term/painted-rows terminal)))]

               (expect (= "wire-rework" answer))
               ;; The question owns the band: its own title row, and the line being
               ;; typed on the very field surface the human-input form paints.
               (expect (str/includes? painted "Name the draft (with my changes):"))
               (expect (str/includes? painted "wire-rework"))
               ;; …and NOTHING on screen still claims a command key does something,
               ;; because every key belongs to the field now.
               (expect (not (str/includes? painted "New draft from the committed HEAD")))
               (expect (not (str/includes? painted "New draft with my uncommitted changes")))
               (expect (not (str/includes? painted "Switch to another draft"))))
             (finally (.stopScreen screen)))))
  (it "a band's y/n question is a band too — the question, then Yes and No on their own keys"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   region (assoc (tr/band-region 80 30 1) :restore! (dlg/frame-restorer screen))
                   rows [{"workspace_id" "ws-a" "label" "feature-a" "is_current" true}]
                   host (dlg/transient-host screen g)
                   _ (do (tr/paint! host region (drafts/spec rows) {:switches #{} :options {}})
                         ((:refresh! host)))
                   _ (.addInput terminal (term/keystroke \y))
                   answer ((:confirm! (dlg/band-questions screen g region)) "Discard 'feature-a'?")
                   painted (str/join "\n" (map :text (term/painted-rows terminal)))]

               (expect (true? answer))
               (expect (str/includes? painted "Discard 'feature-a'?"))
               (expect (str/includes? painted "Yes"))
               (expect (str/includes? painted "No"))
               (expect (not (str/includes? painted "Abandon draft"))))
             (finally (.stopScreen screen)))))
  (it "a DESTRUCTIVE y/n names what saying yes COSTS, and what each answer does"
      ;; `Yes` alone never says what it agrees to. The companion's confirm row
      ;; spells the cost over the two answers and labels them with the verb, so
      ;; the band that asks the same question in the terminal does too.
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   region (assoc (tr/band-region 80 30 1) :restore! (dlg/frame-restorer screen))
                   _ (.addInput terminal (term/keystroke \y))
                   answer ((:confirm! (dlg/band-questions screen g region))
                            "Remove GitHub Copilot?"
                            {:cost "Signs out on the gateway machine and drops its entry there."
                             :yes-label "Yes, remove"
                             :no-label "Keep it"})
                   painted (str/join "\n" (map :text (term/painted-rows terminal)))]

               (expect (true? answer))
               (expect (str/includes? painted "Remove GitHub Copilot?"))
               (expect (str/includes? painted "Signs out on the gateway machine"))
               (expect (str/includes? painted "Yes, remove"))
               (expect (str/includes? painted "Keep it")))
             (finally (.stopScreen screen)))))
  (it "a band SAYS a refusal in its own frame instead of opening a window over it"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   region (assoc (tr/band-region 80 30 1) :restore! (dlg/frame-restorer screen))
                   _ (.addInput terminal (term/keystroke \q))
                   answer ((:note! (dlg/band-questions screen g region))
                            "GitHub Copilot — remove failed"
                            "provider remove failed: 400")
                   painted (str/join "\n" (map :text (term/painted-rows terminal)))]

               (expect (nil? answer))
               (expect (str/includes? painted "remove failed"))
               (expect (str/includes? painted "provider remove failed: 400"))
               (expect (str/includes? painted "Dismiss")))
             (finally (.stopScreen screen)))))
  (it "a band HOLDS its own frame while a browser round-trip finishes"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   region (assoc (tr/band-region 80 30 1) :restore! (dlg/frame-restorer screen))
                   ticks (atom 0)
                   answer ((:wait! (dlg/band-questions screen g region))
                            "GitHub Copilot — waiting for authorization"
                            (fn []
                              "Finish the login in the browser · 0s")
                            ;; Not done on the first paint: the band has to hold.
                            (fn []
                              (> (swap! ticks inc) 1)))
                   painted (str/join "\n" (map :text (term/painted-rows terminal)))]

               (expect (= true answer))
               (expect (str/includes? painted "waiting for authorization"))
               (expect (str/includes? painted "Finish the login in the browser"))
               ;; Esc is the way out of a wait, and the band says so.
               (expect (str/includes? painted "cancel")))
             (finally (.stopScreen screen))))))

;;; ── One band COMPONENT, one instance per host ────────────────────────────────
;; `embed-transient!` is the band; a host differs only by the region it hands in.
;; Settings and the provider band each kept their own one-line wrapper around
;; it (`settings-transient!`, `provider/run-transient!` + `provider/band-region`)
;; and the session screen spelled its own frame snapshot twice — four copies of a
;; component that has exactly one implementation.
(defdescribe
  band-instance-test
  (it
    "the session screen and a host frame are two INSTANCES of the same band"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
      (try
        (let [spec {:groups [{:items [{:key "a" :type :action :id :aa :label "Alpha"}]}]}
              rows (.getRows (.getTerminalSize screen))
              seen (atom nil)
              _ (.addInput terminal (term/keystroke \a))
              result (dlg/session-band! screen
                                        {:content-top 1 :prompt-h 3}
                                        spec
                                        (fn [{:keys [region result]}]
                                          (reset! seen region)
                                          (:action result)))
              session-region @seen]

          ;; the band painted, took the keystroke, and put the frame back
          (expect (= :aa result))
          ;; the session INSTANCE is anchored above the prompt and carries
          ;; the one frame snapshot the whole flow restores from
          (expect (= (- (long rows) 3 3) (long (:hint-row session-region))))
          (expect (ifn? (:restore! session-region)))
          ;; the SAME component in a host's own frame (magit, Settings,
          ;; providers): a different region, one snapshot, one `:title`
          ;; inked on the band's opening rule
          (let [host-region (dlg/host-band-region
                              screen
                              {:left 2 :inner-w 40 :hint-row 20 :text-w 38 :min-row 3})]
            (expect (ifn? (:restore! host-region)))
            (expect (identical? (:restore! host-region)
                                (:restore! (dlg/host-band-region screen host-region))))
            (.addInput terminal (term/keystroke \a))
            (expect (= :aa
                       (:action (dlg/embed-transient! screen
                                                      (.newTextGraphics screen)
                                                      host-region
                                                      "Alpha band"
                                                      spec))))
            (expect (str/includes? (str/join "\n" (map :text (term/painted-rows terminal)))
                                   "Alpha band"))))
        (finally (.stopScreen screen))))))
;;; ── A field's ring belongs INSIDE the frame ──────────────────────────────────
;; Regression (reported from the TUI, photo of the magit commit band): the accent
;; ring `▎` a focused field wears was painted in the frame's OWN border column,
;; and the row's paper was cleared from that column too, so the box lost its left
;; rail on exactly the row the keyboard was in — the answer read as a rail hanging
;; outside the border instead of a field inside it.
(defdescribe
  band-question-frame-test
  (it "a band's typed answer keeps BOTH rails and wears its ring inside them"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]} (term/virtual-screen)]
        (try (let [g (.newTextGraphics screen)
                   left 2
                   inner-w 40
                   region {:left left :inner-w inner-w :hint-row 20 :text-w 38}
                   _ (doseq [k (concat (map term/keystroke "hi") [(KeyStroke. KeyType/Enter)])]
                       (.addInput terminal k))
                   answer ((:read! (dlg/band-questions screen g region)) "Name:")
                   field-row (first (filter #(str/includes? % "▎") (term/grid terminal)))]

               (expect (= "hi" answer))
               ;; the typed line is on that row …
               (expect (str/includes? field-row "hi"))
               ;; … the frame's two rails both survive it …
               (expect (= \│ (nth field-row left)))
               (expect (= \│ (nth field-row (+ left inner-w 1))))
               ;; … and the ring sits INSIDE them, on the field's own left edge
               (expect (< left (long (str/index-of field-row "▎")) (+ left inner-w 1)))
               (expect (str/includes? field-row "▎hi")))
             (finally (.stopScreen screen))))))

(defdescribe
  navigator-input-needed-test
  "The session list's own answer to \"which of these is waiting on ME\". The row
   reported turn counts and focus; a run parked on an unanswered human-input
   request — normally parked in ANOTHER process — read as just another quiet
   session, and this list is exactly where its operator goes looking for it."
  (it "reports the demand instead of the turn count"
      (let [row (var-get #'dlg/navigator-session-row)]
        (expect
          (= "! input needed"
             (:status
               (row nil
                    {"id" "s-parked" "title" "Deploy" "turn_count" 3 "is_awaiting_input" true}))))
        (expect (true? (:awaiting-input?
                         (row nil {"id" "s-parked" "title" "Deploy" "is_awaiting_input" true}))))))
  (it "leaves an unparked row's status exactly as it was"
      (let [row (var-get #'dlg/navigator-session-row)]
        (expect (= "3 turns" (:status (row nil {"id" "s-quiet" "title" "Deploy" "turn_count" 3}))))
        (expect (= "● focused"
                   (:status (row "s-quiet" {"id" "s-quiet" "title" "Deploy" "turn_count" 3}))))
        (expect (not (:awaiting-input? (row nil {"id" "s-quiet" "title" "Deploy"})))))))

;; Regression (user report): the fullscreen log viewer a magit RET-visit opens
;; for a diff painted a scrollbar its key loop never wired mouse events to, so
;; pressing the scrollbar did nothing — only the wheel scrolled.
(defdescribe
  log-view-scrollbar-click-test
  (it "CLICK_DOWN on the track jumps to that fraction and a DRAG keeps following"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
            (term/virtual-screen)

            lines
            (mapv #(format "line-%03d" %) (range 200))

            ;; 80×30 fullscreen viewer: body rows 1..28, scrollbar in col 79.
            bar-x
            79]

        (try
          (.addInput
            terminal
            (MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. (int bar-x) (int 20))))
          ;; drag to the track's last row → scroll ≈ 26/27 of max-scroll 172
          (.addInput terminal
                     (MouseAction. MouseActionType/DRAG 0 (TerminalPosition. (int bar-x) (int 27))))
          (.addInput
            terminal
            (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. (int bar-x) (int 27))))
          (.addInput terminal (KeyStroke. KeyType/Escape))
          (dlg/log-view-dialog! screen "log" lines :grammar nil)
          (let [top (str/triml (:text (nth (term/painted-rows terminal) 1)))]
            (expect (str/starts-with? top "line-166")))
          (finally (.stopScreen screen))))))

;; Regression (user report): pressing a scrollbar scrolled nothing. Settings painted
;; one whose press/drag moved `scroll` alone - and the very next paint recomputes
;; `scroll` from the SELECTED row, so the list snapped straight back under the thumb
;; and only the keyboard could move it.
(defdescribe
  settings-scrollbar-drag-test
  (it
    "CLICK_DOWN on the settings track scrolls the list and the thumb stays there"
    (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
          (term/virtual-screen)

          ;; The scrollbar as PAINTED, scanned off its glyphs (█ thumb on a
          ;; │ track): a hit-test that drifts from the painter fails here
          ;; instead of agreeing with itself.
          bar
          (fn []
            (let [g
                  (term/grid terminal)

                  cells
                  (for [y
                        (range (count g))

                        x
                        (range (count (nth g y)))

                        :let [c
                              (.charAt ^String (nth g y) x)]
                        :when (or (= c \█) (= c \│))]

                    [x y c])

                  thumb
                  (filter #(= \█ (nth % 2)) cells)

                  col
                  (when (seq thumb) (apply max (map first thumb)))]

              (when col
                {:col col
                 :thumb (vec (sort (map second (filter #(= col (first %)) thumb))))
                 :track (vec (sort (map second (filter #(= col (first %)) cells))))})))

          row-text
          (fn [y]
            (str/trim (nth (term/grid terminal) y)))

          open!
          (fn []
            (dlg/settings-dialog! screen {} nil))

          press!
          (fn [x y]
            (.addInput
              terminal
              (MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. (int x) (int y))))
            (.addInput terminal (KeyStroke. KeyType/Escape)))]

      ;; Settings reads its gateway inventories once its first frame is up; this
      ;; test is about the scrollbar, so it never pays for that round trip.
      (with-redefs-fn {#'dlg/load-inventories! (fn []
                                                 nil)}
        (fn []
          (try (.addInput terminal (KeyStroke. KeyType/Escape))
               (open!)
               (let [{:keys [col track]}
                     (bar)

                     track-top
                     (long (first track))

                     track-bottom
                     (long (last track))

                     before
                     (row-text track-top)

                     ;; One press on the track, then the frame it produced.
                     press-at
                     (fn [x y]
                       (press! x y)
                       (open!)
                       {:thumb (first (:thumb (bar))) :row (row-text track-top)})]

                 (expect (some? col))
                 (expect (= track-top (first (:thumb (bar)))))
                 (let [middle
                       (press-at col (quot (+ track-top track-bottom) 2))

                       bottom
                       (press-at col track-bottom)

                       top
                       (press-at col track-top)

                       beside
                       (press-at (dec (long col)) track-bottom)]

                   ;; The thumb follows the cursor down the track, and the list under
                   ;; it moves with it - it no longer snaps back to the old selection.
                   (expect (< track-top (long (:thumb middle)) (long (:thumb bottom))))
                   (expect (not= before (:row middle)))
                   (expect (not= (:row middle) (:row bottom)))
                   ;; Back on the track's first row: the list is back at its start.
                   (expect (= track-top (:thumb top)))
                   (expect (= before (:row top)))
                   ;; One column left of the bar is list content, not the track.
                   (expect (= track-top (:thumb beside)))
                   (expect (= before (:row beside)))))
               (finally (.stopScreen screen))))))))
