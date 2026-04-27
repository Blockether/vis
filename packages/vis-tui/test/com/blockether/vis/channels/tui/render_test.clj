(ns com.blockether.vis.channels.tui.render-test
  "Coverage for the markdown → marker-prefixed-line pipeline.

   These tests pin the answer-mode rendering of ATX headings so a
   regression like 'H4/H5/H6 leak literal `####` into the answer
   body' can never sneak in unnoticed again.

   Background: `markdown->lines` is private; we reach in via
   `#'render/markdown->lines` because there is no narrower public
   entry point and we explicitly want to assert the per-line
   marker prefix the renderer downstream switches on."
  (:require [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.render :as render]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private md->lines @#'render/markdown->lines)

(defn- marker-of
  "First codepoint of `s` as a single-char string, or nil for empty."
  [s]
  (when (and (string? s) (pos? (count s)))
    (subs s 0 1)))

(defn- body-of
  "Drop the leading marker (PUA codepoint) and return the visible text."
  [s]
  (when (string? s) (subs s 1)))

(defdescribe markdown-headings-test
  (describe "ATX headings 1-3 each carry their own marker"
    (it "# Heading 1 → MARKER_MD_H1"
      (let [[line] (md->lines "# Heading 1" 80 :answer)]
        (expect (= p/MARKER_MD_H1 (marker-of line)))
        (expect (= "Heading 1" (body-of line)))))

    (it "## Heading 2 → MARKER_MD_H2"
      (let [[line] (md->lines "## Heading 2" 80 :answer)]
        (expect (= p/MARKER_MD_H2 (marker-of line)))
        (expect (= "Heading 2" (body-of line)))))

    (it "### Heading 3 → MARKER_MD_H3"
      (let [[line] (md->lines "### Heading 3" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 3" (body-of line))))))

  (describe "H4-H6 fold onto the H3 marker (terminal palettes top out)"
    ;; This is the regression we are pinning. Pre-fix, `#### Heading
    ;; 4` had no matching `cond` branch, so it fell through to
    ;; plain-text rendering and the literal `####` showed up in the
    ;; answer body. The fix collapses H4-H6 onto the H3 marker (same
    ;; convention as glow / mdcat / bat) so deep headings still read
    ;; AS headings instead of leaking hash characters.
    (it "#### Heading 4 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "#### Heading 4" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 4" (body-of line)))
        (expect (not (str/includes? (body-of line) "#")))))

    (it "##### Heading 5 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "##### Heading 5" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 5" (body-of line)))
        (expect (not (str/includes? (body-of line) "#")))))

    (it "###### Heading 6 → MARKER_MD_H3, no leading hashes in body"
      (let [[line] (md->lines "###### Heading 6" 80 :answer)]
        (expect (= p/MARKER_MD_H3 (marker-of line)))
        (expect (= "Heading 6" (body-of line)))
        (expect (not (str/includes? (body-of line) "#"))))))

  (describe "Boundary conditions"
    (it "####### (7 hashes) is NOT a heading — too deep, falls through"
      ;; CommonMark caps ATX headings at 6. A 7-hash line is plain
      ;; text. We assert that the marker is NOT one of the heading
      ;; markers.
      (let [[line] (md->lines "####### too deep" 80 :answer)
            mk    (marker-of line)]
        (expect (not= p/MARKER_MD_H1 mk))
        (expect (not= p/MARKER_MD_H2 mk))
        (expect (not= p/MARKER_MD_H3 mk))))

    (it "#NoSpace is NOT a heading — ATX requires a space after #"
      (let [[line] (md->lines "#NoSpace" 80 :answer)
            mk    (marker-of line)]
        (expect (not= p/MARKER_MD_H1 mk))
        (expect (not= p/MARKER_MD_H2 mk))
        (expect (not= p/MARKER_MD_H3 mk))))

    (it "All six heading levels in one document each render with a heading marker"
      (let [doc   (str/join "\n"
                    ["# H1" "## H2" "### H3" "#### H4" "##### H5" "###### H6"])
            lines (md->lines doc 80 :answer)
            heading-markers #{p/MARKER_MD_H1 p/MARKER_MD_H2 p/MARKER_MD_H3}]
        (expect (= 6 (count lines)))
        (expect (every? heading-markers (map marker-of lines)))
        ;; And specifically: H4-H6 all map to H3.
        (expect (= [p/MARKER_MD_H1 p/MARKER_MD_H2 p/MARKER_MD_H3
                    p/MARKER_MD_H3 p/MARKER_MD_H3 p/MARKER_MD_H3]
                  (mapv marker-of lines)))))))

(defdescribe markdown-headings-thinking-mode-test
  (describe "Thinking-mode headings use the thinking marker bundle"
    ;; The thinking-mode marker bundle is a parallel set of PUA
    ;; codepoints; the renderer paints them with the iter-bg + dim
    ;; italic style. Same H4-H6 → H3 collapse must apply.
    (it "#### Heading 4 (thinking) → MARKER_TH_MD_H3"
      (let [[line] (md->lines "#### Heading 4" 80 :thinking)]
        (expect (= p/MARKER_TH_MD_H3 (marker-of line)))
        (expect (= "Heading 4" (body-of line)))))))
