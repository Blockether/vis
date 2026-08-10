(ns com.blockether.vis.ext.channel-tui.transient-test
  "The TRANSIENT component on its own: its pure model (`toggle`, `key-glyph`,
   `item-arg`, `columns`), and what it actually PAINTS into a host's rectangle —
   driven through `dialogs/transient-host`, the standard modal host, on a real
   virtual terminal."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.terminals :as term]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.ext.channel-tui.transient :as tr])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private commit-transient-spec
  "The spec `magit-commit-flow!` hands the transient: one FLAG group and one
   COMMAND group, exactly like Emacs magit's commit popup."
  {:groups [{:title "Arguments"
             :items
             [{:key "h" :type :switch :id :no-verify :label "Disable hooks" :arg "--no-verify"}]}
            {:title "Commands"
             :items [{:key "c" :type :action :id :commit :label "Commit staged"}
                     {:key "a" :type :action :id :amend :label "Amend last commit"}]}]})

(defn- drive-transient!
  "Run `tr/run!` on a virtual terminal, feeding `keys` (characters, `:esc`
   for Escape). Returns `{:ret … :rows …}` — the transient's result plus its LAST
   paint, which is what the user is looking at when the key lands."
  [spec keys]
  (let
    [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
     (term/virtual-screen)

     g
     (.newTextGraphics screen)]

    (doseq [c keys]
      (.addInput terminal (term/keystroke c)))
    {:ret (tr/run! (dlg/transient-host screen g)
                   {:left 0 :inner-w 78 :hint-row 28 :text-w 70}
                   (assoc spec :title "Commit"))
     :rows (term/painted-rows terminal)}))

(defn- transient-grid!
  "EVERY terminal row (blanks KEPT) after one paint of `tr/run!` at the
   host-dialog geometry `magit-status-buffer!` hands it, so the popup's own band
   geometry — its capping rule, its margin rows, the row its hint bar lands on —
   is inspectable. `opts` goes straight through. `pre!` paints the HOST buffer
   first, so whatever the popup covers (or fails to cover) shows up."
  ([spec left inner-w hint-row] (transient-grid! spec left inner-w hint-row nil))
  ([spec left inner-w hint-row opts] (transient-grid! spec left inner-w hint-row opts nil))
  ([spec left inner-w hint-row opts pre!]
   (let
     [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
      (term/virtual-screen)

      g
      (.newTextGraphics screen)]

     (when pre! (pre! g))
     (.addInput terminal (term/keystroke :esc))
     (tr/run! (dlg/transient-host screen g)
              (merge {:left left :inner-w inner-w :hint-row hint-row :text-w 70} opts)
              (assoc spec :title "Commit"))
     (term/grid terminal))))

(defn- row-with [rows needle] (some #(when (str/includes? (:text %) needle) %) rows))

(defn- blank-band-row?
  "True when terminal row `s` carries no band CONTENT — a padding row, which on a
   framed host still wears the dialog's own `│` rails."
  [s]
  (str/blank? (str/replace (str s) #"[│├┤]" " ")))

(defdescribe
  transient-toggle-test
  (let
    [spec
     {:groups [{:title "Arguments"
                :items [{:key "f" :type :switch :id :force :label "Force" :arg "--force-with-lease"}
                        {:key "u" :type :switch :id :set-upstream :label "Upstream" :arg "-u"}
                        {:key "t" :type :option :id :topic :label "Topic" :arg "%topic="}]}
               {:title "Commands" :items [{:key "p" :type :action :id :push :label "Push"}]}]}

     init
     {:switches #{} :options {}}]

    (it "binds a key to its item across every group"
        (expect (= :force (:id (tr/item-by-key spec \f))))
        (expect (= :push (:id (tr/item-by-key spec \p))))
        (expect (nil? (tr/item-by-key spec \z))))
    (it "a switch flips on then off; an unbound key is a no-op"
        (let
          [on
           (tr/toggle spec init \f)

           off
           (tr/toggle spec (:state on) \f)]

          (expect (= :continue (:kind on)))
          (expect (= #{:force} (:switches (:state on))))
          (expect (= #{} (:switches (:state off))))
          (expect (= init (:state (tr/toggle spec init \z))))))
    (it "two switches accumulate independently"
        (let
          [s (-> (tr/toggle spec init \f)
                 :state
                 (->> (#(tr/toggle spec % \u)))
                 :state)]
          (expect (= #{:force :set-upstream} (:switches s)))))
    (it "an option key asks the caller to read a value"
        (let [r (tr/toggle spec init \t)]
          (expect (= :option (:kind r)))
          (expect (= :topic (:id (:item r))))))
    (it "an action key fires with the item, leaving state untouched"
        (let [r (tr/toggle spec {:switches #{:force} :options {}} \p)]
          (expect (= :action (:kind r)))
          (expect (= :push (:id (:item r))))))))

(defdescribe transient-columns-test
             (it "flags carry magit's leading dash; commands show the bare key"
                 (expect (= "-h" (tr/key-glyph {:type :switch :key "h"})))
                 (expect (= "-t" (tr/key-glyph {:type :option :key "t"})))
                 (expect (= "c" (tr/key-glyph {:type :action :key "c"}))))
             (it "the argument cell names the git flag, with an option's value inline"
                 (expect (= "(--no-verify)" (tr/item-arg {:type :switch :arg "--no-verify"} nil)))
                 (expect (= "(%topic=fix)" (tr/item-arg {:type :option :arg "%topic="} "fix")))
                 (expect (nil? (tr/item-arg {:type :action :arg "--nope"} nil)))
                 (expect (nil? (tr/item-arg {:type :switch} nil)))
                 ;; A credential rides the transient WITHOUT ever being echoed.
                 (expect (= "(••••••)" (tr/item-arg {:type :option :secret? true} "sk-live-123")))
                 (expect (nil? (tr/item-arg {:type :option :secret? true} ""))))
             (it "one shared key + description column aligns every group into a grid"
                 (expect (= {:key-w 2 :label-w (count "Amend last commit")}
                            (tr/columns commit-transient-spec)))))

(defdescribe
  transient-render-test
  (it "a flag and a command render as two visibly different grid rows"
      (let
        [rows
         (:rows (drive-transient! commit-transient-spec [:esc]))

         flag
         (row-with rows "Disable hooks")

         command
         (row-with rows "Commit staged")]

        (expect (str/includes? (:text flag) "-h  Disable hooks"))
        (expect (str/includes? (:text flag) "(--no-verify)"))
        ;; A flag starts OFF: dim, nothing bold.
        (expect (str/blank? (:bold flag)))
        ;; A command is a BOLD key plus its description — never dim.
        ;; The key column is as wide as the widest key in the pane (`-h` here), so
        ;; a one-letter key is padded to it before the [[key-gap]] gutter.
        (expect (str/includes? (:text command) "c   Commit staged"))
        (expect (= "c" (:bold command)))))
  (it
    ;; Regression, issue: the hydra band spent its two top rows on a title and a
    ;; second rule — the band's FIRST row is the `───` and everything under it is
    ;; the column grid.
    "the popup is a band INSIDE the host frame whose first row is a rule over the columns"
    (let
      [grid
       (transient-grid! commit-transient-spec 3 74 27)

       rule-y
       (first (keep-indexed (fn [i s]
                              (when (str/includes? s "────") i))
                            grid))

       args-y
       (first (keep-indexed (fn [i s]
                              (when (str/includes? s "Arguments") i))
                            grid))]

      ;; The capping rule ends in T-junctions ON the host's box border and
      ;; never spills into the columns outside it.
      (expect (= \├ (nth (nth grid rule-y) 3)))
      (expect (= \┤ (nth (nth grid rule-y) 78)))
      (expect (str/blank? (subs (nth grid rule-y) 0 3)))
      (expect (str/blank? (subs (nth grid rule-y) 79)))
      ;; The hint bar lands ON the host's own hint row — the row below it, the
      ;; dialog's bottom border, is never swallowed.
      (expect (str/includes? (nth grid 27) "toggle flag"))
      (expect (str/blank? (nth grid 28)))
      ;; The popup is GLUED to that bottom chrome: it repaints the host's own
      ;; `├───┤` rule directly above the hint bar, so its last command never
      ;; runs straight into the footer text with the rule swallowed.
      (expect (str/includes? (nth grid 26) "────"))
      (expect (= [\├ \┤] [(nth (nth grid 26) 3) (nth (nth grid 26) 78)]))
      ;; The band is GLUED to that rule, one blank padding row apart: the last
      ;; command never runs straight into the chrome, and no host row shows in
      ;; the gap.
      (expect (str/includes? (nth grid 24) "Amend last commit"))
      (expect (blank-band-row? (subs (nth grid 25) 3)))
      ;; NO TITLE BAND: the opening rule, one blank row, then the first group
      ;; header — `───` / ` ` / `Arguments`. There is no second rule and no
      ;; heading row repeating what the columns already say.
      (expect (= rule-y (- (long args-y) 2)))
      (expect (blank-band-row? (nth grid (dec (long args-y)))))
      (expect (not (str/includes? (nth grid (dec (long rule-y))) "Commit")))
      (expect (not (str/includes? (nth grid (dec (long rule-y))) "────")))))
  (it "the popup wipes every host row it covers and no column outside the frame"
      ;; The status buffer paints its OWN hint bar on `hint-row`, framed by the
      ;; dialog's box borders. The popup replaces that hint bar in place: any row
      ;; it fails to own reads as a SECOND hint bar stacked between its commands
      ;; and its footer, and any column it owns outside the border reads as the
      ;; popup escaping the dialog.
      (let
        [host!
         (fn [g]
           (doseq [y (range 30)]
             (p/put-str! g 0 y (apply str (repeat 80 \H)))))

         grid
         (transient-grid! commit-transient-spec 3 74 27 nil host!)

         rule-y
         (first (keep-indexed (fn [i s]
                                (when (str/includes? s "────") i))
                              grid))

         band
         (subvec grid (long rule-y) 28)]

        (expect (some? rule-y))
        ;; Above the rule the host buffer is untouched — the popup never wipes
        ;; the status rows it is supposed to leave visible.
        (expect (= (apply str (repeat 80 \H)) (nth grid (dec (long rule-y)))))
        ;; From the rule down to its own hint bar the popup owns every INNER
        ;; column …
        (expect (every? #(not (str/includes? (subs % 4 78) "H")) band))
        ;; … and not one column outside the host's frame.
        (expect (every? #(str/starts-with? % "HHH") band))
        (expect (every? #(str/ends-with? % "H") band))
        ;; The row under the hint bar — the dialog's bottom border — survives.
        (expect (= (apply str (repeat 80 \H)) (nth grid 28)))
        (expect (str/includes? (nth grid 27) "toggle flag"))
        ;; … and the popup's closing rule sits directly on that hint bar.
        (expect (str/includes? (nth grid 26) "────"))))
  (it "the popup repaints the frame edge over a host separator it covers"
      ;; Wiping only the INNER columns left the host's own section separator
      ;; showing as stray `├`/`┤` junctions in the border columns beside the
      ;; popup — the band looked like it had been torn out of the frame.
      (let
        [host!
         (fn [g]
           (doseq [y (range 30)]
             (p/put-str! g 3 y "│")
             (p/put-str! g 78 y "│"))
           ;; a host separator INSIDE the rows the band will take over
           (p/put-str! g 3 25 (str "├" (apply str (repeat 74 \─)) "┤")))

         grid
         (transient-grid! commit-transient-spec 3 74 27 nil host!)

         rule-y
         (first (keep-indexed (fn [i s]
                                (when (str/includes? s "────") i))
                              grid))]

        (expect (some? rule-y))
        ;; The popup's OWN rule keeps its T-junctions …
        (expect (= \├ (nth (nth grid (long rule-y)) 3)))
        (expect (= \┤ (nth (nth grid (long rule-y)) 78)))
        ;; … there is no second rule under a title, because the band has none …
        ;; … its CLOSING rule keeps them too …
        (expect (= [\├ \┤] [(nth (nth grid 26) 3) (nth (nth grid 26) 78)]))
        ;; … and every other row it covers gets a plain frame edge back, the
        ;; host's junctions included.
        (expect (every? (fn [y]
                          (= [\│ \│] [(nth (nth grid y) 3) (nth (nth grid y) 78)]))
                        (remove #{26} (range (inc (long rule-y)) 28))))
        ;; The host separator's body is gone, not just its junctions.
        (expect (not (str/includes? (nth grid 25) "────")))))
  (it "pressing a flag key arms it and pressing it again disarms it"
      (let
        [on
         (row-with (:rows (drive-transient! commit-transient-spec [\h :esc])) "Disable hooks")

         off
         (row-with (:rows (drive-transient! commit-transient-spec [\h \h :esc])) "Disable hooks")]

        (expect (str/includes? (:bold on) "--no-verify"))
        (expect (str/blank? (:bold off)))))
  (it "the flag toggles the argument the command finally runs with"
      (expect (= {:action :commit :switches #{:no-verify} :options {}}
                 (:ret (drive-transient! commit-transient-spec [\h \c]))))
      (expect (= {:action :commit :switches #{} :options {}}
                 (:ret (drive-transient! commit-transient-spec [\h \h \c]))))
      (expect (= {:action :commit :switches #{:no-verify} :options {}}
                 (:ret (drive-transient! commit-transient-spec [\h \h \h \c])))))
  (it "transient keys are case-sensitive, exactly like magit"
      (expect (= {:action :commit :switches #{} :options {}}
                 (:ret (drive-transient! commit-transient-spec [\H \c]))))
      (let
        [spec
         {:groups
          [{:title "Arguments"
            :items
            [{:key "f" :type :switch :id :lease :label "Force with lease" :arg "--force-with-lease"}
             {:key "F" :type :switch :id :force :label "Force" :arg "--force"}]}
           {:title "Commands" :items [{:key "p" :type :action :id :push :label "Push"}]}]}]
        (expect (= #{:lease} (:switches (:ret (drive-transient! spec [\f \p])))))
        (expect (= #{:force} (:switches (:ret (drive-transient! spec [\F \p])))))
        (expect (= #{:force :lease} (:switches (:ret (drive-transient! spec [\f \F \p])))))))
  (it "Esc cancels the transient, armed flags and all"
      (expect (nil? (:ret (drive-transient! commit-transient-spec [\h :esc]))))))

(defdescribe
  transient-paging-test
  ;; The model picker runs the SAME transient for PAGES of models of DIFFERENT
  ;; heights (`provider/run-model-transient!`). The band itself never reaches
  ;; above its own separator — the paging HOST owns those rows and erases them
  ;; with `tr/clear-rows!` before it runs the next page.
  (it
    "a short band leaves a taller page's rows for the host to clear"
    (let
      [stale!
       (fn [g]
         (doseq [y (range 6 26)]
           (p/put-str! g 4 y "STALE PAGE ROW")))

       kept
       (transient-grid! commit-transient-spec 3 74 27 {:min-row 6} stale!)

       cleared
       (transient-grid! commit-transient-spec
                        3
                        74
                        27
                        {:min-row 6}
                        (fn [g]
                          (stale! g)
                          (tr/clear-rows! g {:left 3 :inner-w 74} 6 26)))]

      ;; Left alone, the band covers only its own rows: the taller page above
      ;; it is still on screen — which is exactly why magit chrome works.
      (expect (some #(str/includes? % "STALE") kept))
      ;; The host's own wipe is what makes a shorter page land on clean paper.
      (expect (not-any? #(str/includes? % "STALE") cleared))
      ;; …and it puts the frame's plain edge back in both border columns, on
      ;; every row it blanked above the band's own opening rule.
      (expect (every? (fn [y]
                        (= [\│ \│] [(nth (nth cleared y) 3) (nth (nth cleared y) 78)]))
                      (range 6
                             (long (first (keep-indexed (fn [i s]
                                                          (when (str/includes? s "────") i))
                                                        cleared))))))
      ;; Nothing above `:min-row` is touched either way.
      (expect (every? str/blank? (take 6 cleared)))
      ;; The hint bar still lands ON the host's hint row, so the dialog's
      ;; bottom border below it is never swallowed.
      (expect (str/includes? (nth cleared 27) "toggle flag"))
      (expect (str/blank? (nth cleared 28)))))
  (it "a transient taller than the host box stops at :min-row instead of climbing over it"
      (let
        [tall
         {:groups (vec (for [gi (range 3)]
                         {:title (str "Group " gi)
                          :items (vec (for [i (range 5)]
                                        {:key (str (char (+ (int \a) (* gi 5) i)))
                                         :type :action
                                         :id (keyword (str "a" gi i))
                                         :label (str "Action " gi "-" i)}))}))}

         grid
         (transient-grid! tall 3 74 27 {:min-row 6})]

        ;; The band's first row IS the rule, at the `:min-row` floor, with one
        ;; blank padding row between it and the first column heading.
        (expect (str/includes? (nth grid 6) "────"))
        (expect (blank-band-row? (nth grid 7)))
        (expect (str/includes? (nth grid 8) "Group 0"))
        (expect (every? str/blank? (take 6 grid)))))
  ;; Regression (user report): "THE FIRST row should have the ------ and
  ;; it should be only columns" — the band spent its first row on a bold title
  ;; and its second on that title's own rule, so two rows of a short band were
  ;; chrome before a single command showed.
  (it "inks the title ON its opening rule, never on a row of its own"
      (let
        [grid
         ;; `transient-grid!` titles every spec "Commit".
         (transient-grid! {:groups [{:title "Models"
                                     :items [{:key "a" :type :action :id :a :label "acme-1"}]}]}
                          3 74
                          27 {:min-row 6})

         rule
         (long (first (keep-indexed (fn [i s]
                                      (when (str/includes? s "────") i))
                                    grid)))]

        ;; The title is ON the rule row …
        (expect (str/includes? (nth grid rule) "Commit"))
        (expect (str/includes? (nth grid rule) "────"))
        ;; … and one blank padding row later the column grid begins.
        (expect (blank-band-row? (nth grid (inc rule))))
        (expect (str/includes? (nth grid (+ rule 2)) "Models"))
        ;; … and it is said exactly once.
        (expect (= 1 (count (filter #(str/includes? % "Commit") grid)))))))

;;; ── The contract: one seam, one refusal ─────────────────────────────────────

(def ^:private legal-region
  "The rectangle `magit-status-buffer!` hands the popup."
  {:left 0 :inner-w 78 :hint-row 28 :text-w 70})

(def ^:private topic-transient-spec
  {:groups [{:title "Arguments"
             :items [{:key "t" :type :option :id :topic :label "Topic" :arg "%topic="}]}]})

(defn- stub-host
  "A host that SATISFIES the contract and paints nothing: every refusal below
   fires before the first cell, so no terminal is needed to watch it."
  []
  {:g :graphics
   :hint-bar! (fn [& _])
   :refresh! (fn [])
   :read-key! (fn []
                :esc)})

(defn- refusal
  "The `:type` of the `ex-info` `f` throws, nil when it returns normally. `run!`
   is the ONE place the component refuses, so a caller matches on that keyword."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defdescribe
  transient-layout-test
  (it "ONE pass over the spec answers every question a frame asks"
      (let [lay (tr/layout commit-transient-spec)]
        (expect (= (tr/rows commit-transient-spec) (:rows lay)))
        (expect (= (count (:rows lay)) (:row-count lay)))
        (expect (= (tr/columns commit-transient-spec) (:columns lay)))
        (expect (= (tr/hint-pairs commit-transient-spec) (:hint-pairs lay)))
        (expect (= (tr/height commit-transient-spec) (:height lay)))
        ;; The key loop dispatches on this index instead of rescanning
        ;; every group for each keystroke.
        (expect (= (tr/item-by-key commit-transient-spec \c) (get (:by-key lay) "c")))
        (expect (nil? (get (:by-key lay) "z")))))
  (it "a band of pure commands never advertises a flag key nothing responds to"
      (expect (= [["key" "run command"] ["Esc" "cancel"]]
                 (:hint-pairs
                   (tr/layout
                     {:groups [{:title "Commands"
                                :items [{:key "c" :type :action :id :commit :label "Commit"}]}]}))))
      (expect (some #{["-key" "toggle flag"]} (:hint-pairs (tr/layout commit-transient-spec))))))

(def ^:private leader-spec
  "A which-key style LEADER menu: many small groups, the shape the C-x hydra has."
  {:title "C-x — vis commands"
   :groups (mapv (fn [g]
                   {:title (str "Group " g)
                    :items (mapv (fn [i]
                                   {:key (str g i)
                                    :type :action
                                    :id (keyword (str "g" g "-" i))
                                    :label (str "verb " g i)})
                                 (range 4))})
                 (range 5))})

(def ^:private leader-band-region
  "A SIDELESS in-session band on a wide terminal: `dialogs/prefix-band!`'s own
   rectangle, with the transcript above `:min-row`."
  {:left 1 :inner-w 100 :text-w 98 :hint-row 20 :min-row 1 :cols 104 :is-sideless true})

(defdescribe
  transient-panes-test
  (it "one pane IS the single column" (expect (= [(tr/rows leader-spec)] (tr/panes leader-spec 1))))
  (it "groups are dealt WHOLE into panes, and every pane is padded to a rectangle"
      (let [ps (tr/panes leader-spec 3)]
        (expect (= 3 (count ps)))
        (expect (= 1 (count (distinct (map count ps)))))
        ;; nothing is lost and no group is split: every verb is still on the grid,
        ;; each one under the heading it belongs to.
        (expect (= (mapv (comp :id :item) (filter #(= :item (:kind %)) (tr/rows leader-spec)))
                   (sort-by (fn [id]
                              (name id))
                            (mapv (comp :id :item)
                                  (filter #(= :item (:kind %)) (apply concat ps))))))
        (expect (= 5 (count (filter #(= :header (:kind %)) (apply concat ps)))))))
  (it "EVERY CATEGORY GETS ITS OWN COLUMN on a band wide enough to hold them"
      (let
        [n
         (tr/pane-count leader-spec leader-band-region)

         lay
         (tr/layout leader-spec leader-band-region)]

        (expect (= (count (:groups leader-spec)) n))
        (expect (= n (:pane-count lay)))
        ;; one heading per pane: no category is stacked under another
        (expect (= (repeat n 1)
                   (map (fn [pane]
                          (count (filter #(= :header (:kind %)) pane)))
                        (:panes lay))))
        ;; and the panes FILL the width between them, the `│` gaps already paid
        ;; for: no empty third at the trailing edge
        (expect (= 100 (+ (reduce + 0 (:pane-ws lay)) (* 3 (dec n)))))
        ;; each pane still holds its own grid: nobody is squeezed under a
        ;; neighbour's widest label
        (expect (every? true?
                        (map (fn [w pane]
                               (>= (long w) (tr/pane-natural pane)))
                             (:pane-ws lay)
                             (:panes lay))))))
  (it "width is the only bound: a narrow band packs categories together"
      (expect (< (tr/pane-count leader-spec (assoc leader-band-region :inner-w 40))
                 (count (:groups leader-spec)))))
  (it "a MODAL keeps magit's single column: its paper is sized to the spec"
      (expect (= 1 (tr/pane-count leader-spec (dissoc leader-band-region :is-sideless))))
      (expect (= 1 (tr/pane-count leader-spec nil))))
  (it "panes are never invented: two short categories stay two columns"
      (expect (= 2 (count (:groups commit-transient-spec))))
      ;; neither category is tall enough to cut, so the default four columns
      ;; cannot be filled with anything and the band does not pad itself with
      ;; empty ones
      (expect (= 2 (tr/pane-count commit-transient-spec leader-band-region)))))

(def ^:private list-spec
  "A band that is ONE long list, the shape `C-x d`'s draft chooser has."
  {:title "Drafts"
   :groups [{:title "Switch to"
             :items (mapv (fn [i]
                            {:key (str i)
                             :type :action
                             :id (keyword (str "d" i))
                             :label (str "draft " i)})
                          (range 12))}]})

;; Regression, issue #C-x resolution-aware columns: the band took its column
;; count from how many categories the spec happened to have, so a one-group list
;; painted a single column down the left of a 160-column terminal with the rest
;; of the band empty and the list running off the bottom.
(defdescribe
  transient-resolution-test
  (it "a one-group list FILLS the default four columns on a wide band"
      (let [lay (tr/layout list-spec (assoc leader-band-region :inner-w 160))]
        (expect (= 4 (:pane-count lay)))
        ;; the heading is said ONCE: a continuation column carries a blank where
        ;; its heading was, so its verbs sit on the same rows as the half above
        (expect (= 1 (count (filter #(= :header (:kind %)) (apply concat (:panes lay))))))
        ;; and every verb is still on the grid, in order
        (expect (= (mapv :id (:items (first (:groups list-spec))))
                   (mapv (comp :id :item)
                         (filter #(= :item (:kind %))
                                 (apply concat (:panes lay))))))))
  (it "THE WIDTH DECIDES how many of those four there is room for"
      (expect (= [1 2 3 4 4 4]
                 (mapv (fn [w]
                         (tr/pane-count list-spec (assoc leader-band-region :inner-w w)))
                       [40 60 80 100 120 160]))))
  (it "at every resolution the panes fill the width and none is ellipsized"
      (expect (every? true?
                      (map (fn [w]
                             (let [lay (tr/layout list-spec (assoc leader-band-region :inner-w w))
                                   ws (:pane-ws lay)]
                               (and (= (long w) (+ (reduce + 0 ws) (* 3 (dec (count ws)))))
                                    (every? true?
                                            (map (fn [pw pane] (>= (long pw) (tr/pane-natural pane)))
                                                 ws
                                                 (:panes lay))))))
                           [40 60 80 100 120 160]))))
  (it "four is a FLOOR on a wide screen, not a ceiling on the spec"
      (expect (= 5 (count (:groups leader-spec))))
      (expect (= 5 (tr/pane-count leader-spec leader-band-region)))))

;; Regression, issue #C-x d columns: the verbs started in the SAME column as the
;; heading above them and their descriptions sat one space off a one-letter key,
;; so the band read as a ragged list instead of a grid; and every pane was as
;; wide as the widest label in the WHOLE spec, which packed a wide band's columns
;; into its left half and left the trailing third empty.
(defdescribe
  transient-column-grid-test
  (it
    "verbs are INDENTED under their heading and the description clears the key"
    (let
      [grid
       (transient-grid! commit-transient-spec 0 78 28)

       head
       (some #(when (str/includes? % "Commands") %) grid)

       row
       (some #(when (str/includes? % "Commit staged") %) grid)]

      (expect (= (+ (long (str/index-of head "Commands")) (long tr/item-indent))
                 (str/index-of row "c")))
      (expect (= (+ (long (str/index-of row "c"))
                    (long (:key-w (tr/columns commit-transient-spec)))
                    (long tr/key-gap))
                 (str/index-of row "Commit staged")))))
  ;; Regression, issue #C-x band grid: each column was sized in proportion to its
  ;; own widest verb, so four headings started at four unrelated offsets
  ;; (37/35/37/38 on a 160-column band) and every column trailed a ragged tail of
  ;; blanks — full width, and still not a grid.
  (it
    "the band is ONE GRID: equal columns, none narrower than its own verbs"
    (let
      [narrow
       {:groups [{:title "A" :items [{:key "a" :type :action :id :a :label "go"}]}
                 {:title "B"
                  :items [{:key "b"
                           :type :action
                           :id :b
                           :label "a very much longer verb indeed"}]}]}

       ws
       (:pane-ws (tr/layout narrow leader-band-region))

       lay
       (tr/layout leader-spec leader-band-region)

       lws
       (:pane-ws lay)]

      ;; a narrow category stands at the SAME stride as a wide one: the cells that
      ;; do not divide are the only difference between two columns
      (expect (>= 1 (- (long (apply max ws)) (long (apply min ws)))))
      (expect (>= 1 (- (long (apply max lws)) (long (apply min lws)))))
      ;; and no column is ever squeezed under what its own verbs need
      (expect (every? true?
                      (map (fn [w pane]
                             (>= (long w) (tr/pane-natural pane)))
                           lws
                           (:panes lay))))
      (expect (= 100 (+ (long (reduce + 0 lws)) (* 3 (dec (count lws))))))))
  (it
    "a pane is wide enough for its OWN widest verb, ellipsis and all"
    (let
      [wide
       {:groups [{:title "A"
                  :items [{:key "a" :type :action :id :a :label "a verb that fills its column"}]}]}

       grid
       (transient-grid! wide 0 (+ 2 (tr/pane-natural (first (tr/panes wide 1)))) 28)]

      (expect (some #(str/includes? % "a verb that fills its column") grid))))
  (it
    "side by side, the second heading starts exactly one gap past the first column"
    (let
      [grid
       (transient-grid! commit-transient-spec 0 78 28 {:cols 80 :min-row 1 :is-sideless true})

       [w0]
       (:pane-ws (tr/layout commit-transient-spec
                            {:left 0 :inner-w 78 :text-w 70 :hint-row 28 :cols 80 :min-row 1
                             :is-sideless true}))

       head
       (some #(when (str/includes? % "Arguments") %) grid)]

      (expect (= (+ (long (str/index-of head "Arguments")) (long w0) 3)
                 (str/index-of head "Commands"))))))

(defdescribe
  transient-contract-test
  (it "`check` ANSWERS instead of throwing, so a producer asks before it paints"
      (expect (nil? (tr/check commit-transient-spec)))
      (expect (string? (tr/check (assoc commit-transient-spec :nope 1)))))
  (it "an illegal spec is refused at the seam, before a single cell is painted"
      (expect (= :vis/transient-invalid-spec
                 (refusal #(tr/run!
                             (stub-host)
                             legal-region
                             (assoc-in commit-transient-spec [:groups 0 :items 0 :labl] "typo")))))
      ;; A second row on `c` is a command nobody can ever fire: the
      ;; first one wins the keystroke forever.
      (expect (= :vis/transient-invalid-spec
                 (refusal #(tr/run!
                             (stub-host)
                             legal-region
                             (update-in
                               commit-transient-spec
                               [:groups 1 :items]
                               conj
                               {:key "c" :type :action :id :other :label "Something else"}))))))
  (it "a region the popup cannot paint into is refused"
      (expect (= :vis/transient-invalid-region
                 (refusal
                   #(tr/run! (stub-host) (dissoc legal-region :hint-row) commit-transient-spec)))))
  (it "a host that cannot answer a keystroke is refused"
      (expect (= :vis/transient-invalid-host
                 (refusal
                   #(tr/run! (dissoc (stub-host) :read-key!) legal-region commit-transient-spec)))))
  (it "a `:read-option` value no row can paint is refused, not painted"
      (expect (= :vis/transient-invalid-option
                 (refusal #(drive-transient! (assoc topic-transient-spec
                                               :read-option (fn [_ _]
                                                              "   "))
                                             [\t :esc]))))
      ;; nil is how a cancelled prompt says "unchanged" — it is not a value.
      (expect (nil? (refusal #(drive-transient! (assoc topic-transient-spec
                                                  :read-option (fn [_ _]
                                                                 nil))
                                                [\t :esc]))))))

;; Regression, issue #C-x band padding: the hydra's first heading sat directly on
;; the rule that carried its title and its last row directly on the closing rule,
;; so the band read as a table jammed between two lines.
(defdescribe
  transient-band-padding-test
  (it "every pane opens and closes with a blank row, so the body is not glued to the chrome"
      (doseq [pane (tr/panes leader-spec 3)]
        (expect (= :blank (:kind (first pane))))
        (expect (= :blank (:kind (last pane)))))
      (expect (= :blank (:kind (first (tr/rows commit-transient-spec)))))
      (expect (= :blank (:kind (last (tr/rows commit-transient-spec))))))
  (it "one pane is still exactly the single column, padding and all"
      (expect (= [(tr/rows leader-spec)] (tr/panes leader-spec 1))))
  (it "the height a host sizes its box with pays for those blank rows"
      (expect (= (+ 1 (count (tr/rows commit-transient-spec))) (tr/height commit-transient-spec))))
  (it "the padding rows are the band's OWN paper, not the transcript showing through"
      (let
        [grid
         (transient-grid! commit-transient-spec
                          3
                          74
                          27
                          nil
                          (fn [g]
                            (dotimes [row 28]
                              (p/put-str! g 0 row (apply str (repeat 74 \X))))))

         rule-y
         (long (first (keep-indexed (fn [i s]
                                      (when (str/includes? s "────") i))
                                    grid)))]

        ;; the rule, then a blank row, then the first heading
        (expect (blank-band-row? (subs (nth grid (inc rule-y)) 3)))
        (expect (str/includes? (nth grid (+ rule-y 2)) "Arguments"))
        ;; and a blank row between the last verb and the closing rule
        (expect (str/includes? (nth grid 24) "Amend last commit"))
        (expect (blank-band-row? (subs (nth grid 25) 3)))
        (expect (str/includes? (nth grid 26) "────")))))

;; Regression, issue #C-x band caret: the hardware cursor stayed parked in the
;; prompt and went on blinking behind the hydra band, as if the band were not up.
(defdescribe band-caret-test
             (it "the band hides the terminal caret for as long as it owns the keyboard"
                 (let
                   [{:keys [^TerminalScreen screen]}
                    (term/virtual-screen)

                    seen
                    (do (.setCursorPosition screen (p/cursor-pos 4 4))
                        (#'dlg/session-band-instance!
                         screen
                         {:content-top 1 :prompt-h 3}
                         (fn [_ _]
                           (.getCursorPosition screen))))]

                   (expect (nil? seen)))))

;; Regression, issue #C-x band pointer: the hydra band vanished as soon as the
;; mouse moved — one MOVE report was taken as the chord's second key and the
;; resolver read it as an abort, so C-x could not survive a nudged cursor.
(defdescribe
  band-pointer-drift-test
  (it
    "pointer drift never answers the chord: the band waits for a real key"
    (let
      [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
       (term/virtual-screen)

       drift
       [MouseActionType/MOVE MouseActionType/DRAG MouseActionType/SCROLL_UP MouseActionType/SCROLL_DOWN]]

      (doseq [^MouseActionType a drift]
        (.addInput terminal (MouseAction. a 1 (TerminalPosition. 4 4))))
      (.addInput terminal (term/keystroke \d))
      (expect (= \d
                 (.getCharacter (dlg/prefix-band! screen
                                                  {:content-top 1 :prompt-h 3}
                                                  leader-spec)))))))

;; Regression, issue #C-x band width: the band washed its paper across the WHOLE
;; terminal width, so its background ran past both of its own rules to the screen
;; edges instead of reading as a slab inset to them.
(defdescribe
  band-paper-width-test
  (it
    "the band's paper stops at its rules; the margins keep the terminal's own bg"
    (let
      [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
       (term/virtual-screen)

       g
       (.newTextGraphics screen)

       region
       (tr/band-region 80 30 1)

       bg-at
       (fn [x y]
         (.getBackgroundColor (.getCharacter terminal (p/cursor-pos (int x) (int y)))))

       painted
       (do (.addInput terminal (term/keystroke :esc))
           (tr/run! (dlg/transient-host screen g)
                    region
                    (assoc commit-transient-spec :title "Commit"))
           (term/grid terminal))

       ;; every row the band owns, from its opening rule to its closing one
       rule-rows
       (keep-indexed (fn [i s]
                       (when (str/includes? s "────") i))
                     painted)

       band-rows
       (range (long (first rule-rows)) (inc (long (last rule-rows))))

       left
       (long (:left region))

       inner-w
       (long (:inner-w region))]

      (doseq [y band-rows]
        ;; the band wears the TERMINAL's own paper — no tint, body or footer:
        ;; the border is what marks the band.
        (expect (= t/terminal-bg (bg-at (inc left) y)))
        (expect (= t/terminal-bg (bg-at (+ left inner-w) y)))
        (expect (= t/terminal-bg (bg-at 0 y)))
        (expect (= t/terminal-bg (bg-at (+ left inner-w 1) y)))
        (expect (= t/terminal-bg (bg-at 79 y))))
      ;; the footer is FENCED OFF: its own rule sits directly above the hint bar,
      ;; so the band has three rules — opening, footer, closing.
      (expect (= 3 (count rule-rows)))
      (expect (= (long (last rule-rows)) (+ 2 (long (second rule-rows)))))
      ;; and it is BORDERED: corner-capped rules with rails down both edges.
      (let
        [char-at
         (fn [x y]
           (.getCharacterString (.getCharacter terminal (p/cursor-pos (int x) (int y)))))

         top
         (long (first rule-rows))

         bottom
         (long (last rule-rows))

         right
         (+ left inner-w 1)]

        (expect (= ["┌" "┐"] [(char-at left top) (char-at right top)]))
        (expect (= ["└" "┘"] [(char-at left bottom) (char-at right bottom)]))
        (doseq [y (range (inc top) bottom)]
          (expect (= ["│" "│"] [(char-at left y) (char-at right y)])))
        ;; no rule between the columns of the grid
        (expect (not-any? #(str/includes? % "│ ")
                          (map #(subs % (inc (long left)) (+ (long left) (long inner-w)))
                               (subvec (vec painted) (inc top) bottom))))))))
