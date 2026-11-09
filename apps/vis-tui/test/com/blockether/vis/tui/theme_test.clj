(ns com.blockether.vis.tui.theme-test
  "Sanity pins for the visual palette. We can't programmatically verify
   that 'this colour looks gold' - but we CAN pin:

     - The constants exist and have the right RGB tuples (catches
       accidental whitespace edits / cherry-pick drops).
     - WCAG luminance contrast vs the surfaces they paint on stays
       above the AA threshold (4.5:1 for normal text). If anyone
       'dials back' a colour into illegibility, this test screams
       before the user does."
  (:require [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TextColor$RGB]))

(defn rgb-tuple
  "[r g b] of a Lanterna RGB colour (typed to avoid reflection)."
  [^TextColor$RGB c]
  [(.getRed c) (.getGreen c) (.getBlue c)])

(defn- channel
  "Linearise an sRGB component per WCAG 2.x."
  ^double [^long c]
  (let [v (/ (double c) 255.0)]
    (if (<= v 0.03928) (/ v 12.92) (Math/pow (/ (+ v 0.055) 1.055) 2.4))))

(defn- relative-luminance
  ^double [^TextColor$RGB rgb]
  (+ (* 0.2126 (channel (.getRed rgb)))
     (* 0.7152 (channel (.getGreen rgb)))
     (* 0.0722 (channel (.getBlue rgb)))))

(defn- contrast-ratio
  "WCAG 2.x contrast ratio between two RGB triples. Symmetric, range [1, 21]."
  ^double [^TextColor$RGB a ^TextColor$RGB b]
  (let [la
        (relative-luminance a)

        lb
        (relative-luminance b)

        hi
        (if (>= la lb) la lb)

        lo
        (if (>= la lb) lb la)]

    (/ (+ hi 0.05) (+ lo 0.05))))

(defn- rgb-vec [^TextColor$RGB rgb] [(.getRed rgb) (.getGreen rgb) (.getBlue rgb)])

(defdescribe
  adapter-coverage-test
  (describe
    "TUI adapter consumes every shared theme token"
    (it "has a public Lanterna var for every palette token and matches light theme values"
        (try (t/apply-theme! :vis-light)
             (doseq [[token expected] (:palette shared-theme/vis-light)]
               (let [v (ns-resolve 'com.blockether.vis.tui.theme (symbol (name token)))]
                 (expect (some? v))
                 (expect (= expected (rgb-vec @v)))))
             (finally (t/apply-theme! (keyword shared-theme/default-theme-id)))))
    (it
      "keeps session titles, active selections, hints, and error banners AA-readable in every theme"
      (try (doseq [theme-id (map keyword (shared-theme/available-theme-ids))]
             (t/apply-theme! theme-id)
             (expect (>= (contrast-ratio t/header-fg t/terminal-bg) 4.5))
             (expect (>= (contrast-ratio t/header-active-tab-fg t/header-active-tab-bg) 4.5))
             (expect (>= (contrast-ratio t/dialog-hint-key t/dialog-bg) 4.5))
             (expect (>= (contrast-ratio t/status-bad t/warning-bg) 4.5))
             (expect (>= (contrast-ratio t/footer-error-fg t/warning-bg) 4.5))
             (expect (>= (contrast-ratio (t/contrast-ink t/code-success-fg) t/code-success-fg)
                         3.0)))
           (finally (t/apply-theme! (keyword shared-theme/default-theme-id)))))))

(defdescribe status-contrast-test
             (it "keeps status ink readable when a dark theme selects a bright row"
                 (with-redefs [t/text-fg (TextColor$RGB. 255 255 255)]
                   (let [paper (TextColor$RGB. 240 240 240)
                         ink (TextColor$RGB. 80 160 240)]

                     (expect (>= (contrast-ratio (t/legible-ink ink paper) paper) 4.5))))))
