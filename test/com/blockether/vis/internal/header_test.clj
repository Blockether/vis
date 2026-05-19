(ns com.blockether.vis.internal.header-test
  (:require [com.blockether.vis.internal.header :as vh]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe shared-header-spec-test
  (it "ships slot ratios that always sum to <= 1"
    ;; A future web/Telegram channel can read these ratios verbatim; if
    ;; they ever drift above 1, every layout downstream would overflow.
    (expect (<= (+ (double vh/left-slot-ratio) (double vh/right-slot-ratio)) 1.0)))

  (it "splits a row into [left center right] that adds back up to the original"
    ;; Channels rely on this contract to never lose / gain cells.
    (let [[l c r] (vh/slot-widths 80)]
      (expect (= 80 (+ l c r)))
      (expect (= 16 l))
      (expect (= 16 r))
      (expect (= 48 c)))
    (let [[l c r] (vh/slot-widths 0)]
      (expect (zero? l))
      (expect (zero? c))
      (expect (zero? r))))

  (it "caps visible tabs in [min, max] depending on width and tab count"
    ;; Wide screen — capped to max.
    (expect (= 8 (vh/max-visible-count 12 400)))
    ;; Mid screen — clamped to min.
    (expect (= 5 (vh/max-visible-count 12 80)))
    ;; Few tabs — never exceeds tabs-n.
    (expect (= 3 (vh/max-visible-count 3 400)))
    ;; Narrow screen below the min floor — degrades to natural fit.
    (expect (= 2 (vh/max-visible-count 8 30))))

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
