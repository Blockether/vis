(ns com.blockether.vis.ext.channel-tui.theme-test
  "Sanity pins for the visual palette. We can't programmatically verify
   that 'this colour looks gold' — but we CAN pin:

     - The constants exist and have the right RGB tuples (catches
       accidental whitespace edits / cherry-pick drops).
     - WCAG luminance contrast vs the surfaces they paint on stays
       above the AA threshold (4.5:1 for normal text). If anyone
       'dials back' a colour into illegibility, this test screams
       before the user does."
  (:require [com.blockether.vis.ext.channel-tui.theme :as t]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TextColor$RGB]))

(defn- channel
  "Linearise an sRGB component per WCAG 2.x."
  ^double [^long c]
  (let [v (/ (double c) 255.0)]
    (if (<= v 0.03928)
      (/ v 12.92)
      (Math/pow (/ (+ v 0.055) 1.055) 2.4))))

(defn- relative-luminance
  ^double [^TextColor$RGB rgb]
  (+ (* 0.2126 (channel (.getRed rgb)))
    (* 0.7152 (channel (.getGreen rgb)))
    (* 0.0722 (channel (.getBlue rgb)))))

(defn- contrast-ratio
  "WCAG 2.x contrast ratio between two RGB triples. Symmetric, range [1, 21]."
  ^double [^TextColor$RGB a ^TextColor$RGB b]
  (let [la (relative-luminance a)
        lb (relative-luminance b)
        [hi lo] (if (>= la lb) [la lb] [lb la])]
    (/ (+ hi 0.05) (+ lo 0.05))))

(defdescribe heading-colours-test
  (describe "Markdown heading gold gradient exists and has the documented values"
    (it "H1 is rich amber"      (expect (= [150 100 0] [(.getRed t/md-h1-fg) (.getGreen t/md-h1-fg) (.getBlue t/md-h1-fg)])))
    (it "H2 is deeper amber"    (expect (= [125 80 0]  [(.getRed t/md-h2-fg) (.getGreen t/md-h2-fg) (.getBlue t/md-h2-fg)])))
    (it "H3 is deep bronze"     (expect (= [100 65 0]  [(.getRed t/md-h3-fg) (.getGreen t/md-h3-fg) (.getBlue t/md-h3-fg)]))))

  (describe "Headings are readable on every surface they're painted on (contrast >= AA 4.5)"
    (it "H1 on white assistant bg"  (expect (>= (contrast-ratio t/md-h1-fg t/ai-bubble-bg) 4.5)))
    (it "H2 on white assistant bg"  (expect (>= (contrast-ratio t/md-h2-fg t/ai-bubble-bg) 4.5)))
    (it "H3 on white assistant bg"  (expect (>= (contrast-ratio t/md-h3-fg t/ai-bubble-bg) 4.5)))
    (it "H1 on the dialed-back answer-bg"  (expect (>= (contrast-ratio t/md-h1-fg t/answer-bg) 4.5)))
    (it "H2 on the dialed-back answer-bg"  (expect (>= (contrast-ratio t/md-h2-fg t/answer-bg) 4.5)))
    (it "H3 on the dialed-back answer-bg"  (expect (>= (contrast-ratio t/md-h3-fg t/answer-bg) 4.5))))

  (describe "Visual hierarchy: H1 is more luminant than H2 is more luminant than H3"
    ;; Lower luminance = more contrast against the (light) background =
    ;; visually 'heavier'. By design H1 should be the LIGHTEST gold so
    ;; it reads as the loudest, H3 the darkest so it reads as the most
    ;; muted heading. (This is the opposite of font-weight hierarchy in
    ;; print but maps to terminal SGR + colour better.)
    (it "L(H1) > L(H2) > L(H3)"
      (let [l1 (relative-luminance t/md-h1-fg)
            l2 (relative-luminance t/md-h2-fg)
            l3 (relative-luminance t/md-h3-fg)]
        (expect (> l1 l2))
        (expect (> l2 l3))))))

(defdescribe answer-bg-test
  (describe "Answer background is now visually identical to terminal-bg (no zone bg)"
    (it "answer-bg RGB matches terminal-bg exactly"
      ;; Per user request: the answer zone no longer paints a coloured
      ;; block. The answer is distinguished by the FINAL ASSISTANT_TURN_ANSWER header
      ;; + heading colours, nothing else. Pinning the equality keeps
      ;; the renderer honest — if anyone re-introduces a tint, this
      ;; test fails immediately.
      (expect (= [(.getRed t/terminal-bg) (.getGreen t/terminal-bg) (.getBlue t/terminal-bg)]
                [(.getRed t/answer-bg) (.getGreen t/answer-bg) (.getBlue t/answer-bg)])))
    (it "answer-fg on answer-bg still passes WCAG AAA (>= 7)"
      (expect (>= (contrast-ratio t/answer-fg t/answer-bg) 7.0)))))
