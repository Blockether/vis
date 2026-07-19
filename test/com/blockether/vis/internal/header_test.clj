(ns com.blockether.vis.internal.header-test
  (:require [com.blockether.vis.internal.header :as vh]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  shared-header-spec-test
  (it "ships slot ratios that always sum to <= 1"
      ;; A future non-TUI channel can read these ratios verbatim; if
      ;; they ever drift above 1, every layout downstream would overflow.
      (expect (<= (+ (double vh/left-slot-ratio) (double vh/right-slot-ratio)) 1.0)))
  (it "splits a row into [left center right] that adds back up to the original"
      ;; Channels rely on this contract to never lose / gain cells.
      (let [[l c r] (vh/slot-widths 80)]
        (expect (= 80 (+ l c r)))
        (expect (= 24 l))
        (expect (= 24 r))
        (expect (= 32 c)))
      (let [[l c r] (vh/slot-widths 0)]
        (expect (zero? l))
        (expect (zero? c))
        (expect (zero? r))))
  (it "lays out slots that never overlap, from narrow to very wide"
      ;; Regression: slot-layout once gave LEFT a fixed 60 cols, starving the
      ;; centre and letting the right cluster bleed into the left slot. Every
      ;; width must keep left | center | right strictly non-overlapping.
      (doseq [cols [40 60 80 100 116 120 160 200 300]]
        (let [{:keys [left-x left-w center-x center-w right-x right-w]} (vh/slot-layout cols)]
          ;; center starts at/after the end of left, right starts at/after end of center.
          (expect (>= center-x (+ left-x left-w)) (str "center overlaps left at " cols))
          (expect (>= right-x (+ center-x center-w)) (str "right overlaps center at " cols))
          ;; nothing runs past the band; no negative widths.
          (expect (<= (+ right-x right-w) cols) (str "right past band at " cols))
          (expect (every? (complement neg?) [left-w center-w right-w])
                  (str "negative width at " cols)))))
  (it "degrades a tiny band gracefully (centre collapses first, never negative)"
      (let [{:keys [left-w center-w right-w]} (vh/slot-layout 8)]
        (expect (zero? center-w))
        (expect (every? (complement neg?) [left-w center-w right-w])))
      ;; zero-width band: everything zero, no throw.
      (let [{:keys [left-w center-w right-w]} (vh/slot-layout 0)]
        (expect (= [0 0 0] [left-w center-w right-w]))))
  (it "caps visible workspaces in [min, max] depending on width and count"
      ;; Wide screen — capped to max.
      (expect (= 8 (vh/max-visible-workspace-count 12 400)))
      ;; Mid screen — clamped to min.
      (expect (= 5 (vh/max-visible-workspace-count 12 80)))
      ;; Few workspaces — never exceeds workspace count.
      (expect (= 3 (vh/max-visible-workspace-count 3 400)))
      ;; Narrow screen below the min floor — degrades to natural fit.
      (expect (= 2 (vh/max-visible-workspace-count 8 30))))
  (it "picks the placeholder for blank/missing titles, real title otherwise"
      (expect (= vh/untitled-session-label (vh/title-or-placeholder nil)))
      (expect (= vh/untitled-session-label (vh/title-or-placeholder "")))
      (expect (= vh/untitled-session-label (vh/title-or-placeholder "   ")))
      (expect (= "Real title" (vh/title-or-placeholder "Real title"))))
  (it "shortens a session id to the shared display length"
      (expect (= "123e4567" (vh/short-id "123e4567-e89b-12d3-a456-426614174000")))
      ;; Short input — returns as-is, never throws.
      (expect (= "abc" (vh/short-id "abc")))
      ;; Blank/nil — nil so callers can use truthy guards.
      (expect (nil? (vh/short-id nil)))
      (expect (nil? (vh/short-id "")))))
