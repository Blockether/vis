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
            [com.blockether.vis.ext.channel-tui.transient :as tr])
  (:import [com.googlecode.lanterna.screen TerminalScreen]
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

        (expect (str/includes? (:text flag) "-h Disable hooks"))
        (expect (str/includes? (:text flag) "(--no-verify)"))
        ;; A flag starts OFF: dim, nothing bold.
        (expect (str/blank? (:bold flag)))
        ;; A command is a BOLD key plus its description — never dim.
        (expect (str/includes? (:text command) "c  Commit staged"))
        (expect (= "c" (:bold command)))))
  (it
    "the popup is a band INSIDE the host frame with a rule under its title"
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
      ;; The band is CONTIGUOUS: the last command sits directly on that rule,
      ;; never one row above it with a host row left showing between.
      (expect (str/includes? (nth grid 25) "Amend last commit"))
      ;; TITLE BAND: opening rule, the bold title, the title's OWN closing rule,
      ;; then the first group header — `───` / `Commit` / `───` / body, the chrome
      ;; the host gives any other titled section. Both rules end in T-junctions
      ;; ON the frame, never in a blank margin row floating under the title.
      (expect (str/includes? (nth grid (dec (long args-y))) "────"))
      (expect (= [\├ \┤]
                 [(nth (nth grid (dec (long args-y))) 3) (nth (nth grid (dec (long args-y))) 78)]))
      (expect (str/includes? (nth grid (- args-y 2)) "Commit"))
      (expect (str/includes? (nth grid (- args-y 3)) "────"))
      (expect (= [\├ \┤] [(nth (nth grid (- args-y 3)) 3) (nth (nth grid (- args-y 3)) 78)]))
      (expect (= rule-y (- args-y 3)))))
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
        ;; … the rule under its TITLE does too …
        (expect (= [\├ \┤]
                   [(nth (nth grid (+ (long rule-y) 2)) 3)
                    (nth (nth grid (+ (long rule-y) 2)) 78)]))
        ;; … its CLOSING rule keeps them too …
        (expect (= [\├ \┤] [(nth (nth grid 26) 3) (nth (nth grid 26) 78)]))
        ;; … and every other row it covers gets a plain frame edge back, the
        ;; host's junctions included.
        (expect (every? (fn [y]
                          (= [\│ \│] [(nth (nth grid y) 3) (nth (nth grid y) 78)]))
                        (remove #{26 (+ (long rule-y) 2)} (range (inc (long rule-y)) 28))))
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

(defdescribe transient-paging-test
             ;; The provider dialog runs the SAME transient for PAGES of models in
             ;; one frame (`provider/run-transient!` passes `{:min-row … :clear-above?
             ;; true}`), so a short page must wipe everything a taller one left above
             ;; its title — without ever painting above `:min-row`.
             (it "clear-above? wipes from :min-row down and keeps the frame intact"
                 (let
                   [grid
                    (transient-grid! commit-transient-spec 3 74 27 {:min-row 6 :clear-above? true})

                    rule-y
                    (first (keep-indexed (fn [i s]
                                           (when (str/includes? s "────") i))
                                         grid))]

                   (expect (= \├ (nth (nth grid rule-y) 3)))
                   (expect (= \┤ (nth (nth grid rule-y) 78)))
                   (expect (str/blank? (subs (nth grid rule-y) 0 3)))
                   (expect (str/blank? (subs (nth grid rule-y) 79)))
                   ;; The hint bar lands ON the host's hint row, so the row below it — the
                   ;; dialog's bottom border — is never swallowed.
                   (expect (str/includes? (nth grid 27) "toggle flag"))
                   (expect (str/blank? (nth grid 28)))
                   ;; Nothing above `:min-row` is painted or wiped.
                   (expect (>= rule-y 6))
                   (expect (every? str/blank? (take 6 grid)))))
             (it
               "a transient taller than the host box stops at :min-row instead of climbing over it"
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
                  (transient-grid! tall 3 74 27 {:min-row 6 :clear-above? true})]

                 (expect (str/includes? (nth grid 6) "Commit"))
                 ;; …and the title keeps its own rule even when the band is clamped.
                 (expect (str/includes? (nth grid 7) "────"))
                 (expect (every? str/blank? (take 6 grid))))))
