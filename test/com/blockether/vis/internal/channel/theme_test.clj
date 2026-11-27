(ns com.blockether.vis.internal.channel.theme-test
  (:require [com.blockether.vis.internal.channel.theme :as theme]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe monochrome-themes-test
             (doseq [id ["paper" "high-contrast-dark"]]
               (it id
                   (let [t (get theme/built-in-themes id)
                         palette (:palette t)]

                     (expect (some? t))
                     (when t
                       (expect (theme/valid-theme? t))
                       (expect (= (set (keys theme/light-palette)) (set (keys palette))))
                       ;; Ink and paper are pure; only the content surfaces take a grey wash.
                       (expect (every? #{[0 0 0] [255 255 255]}
                                       (vals (dissoc palette :user-bubble-bg :iteration-header-bg
                                                     :code-block-bg :result-bg))))
                       ;; User report: a message had no ground of its own on the page.
                       (expect (not= (:terminal-bg palette) (:user-bubble-bg palette)))
                       (doseq [[fg bg] [[:text-fg :terminal-bg] [:dialog-fg :dialog-bg]
                                        [:dialog-title-fg :dialog-title-bg]
                                        [:header-active-tab-fg :header-active-tab-bg]
                                        [:button-fg :button-bg] [:answer-fg :answer-bg]
                                        [:code-block-fg :code-block-bg] [:dialog-hint :dialog-bg]]]
                         (expect (not= (get palette fg) (get palette bg))))
                       (let [css (theme/theme->web-css-vars t)]
                         (expect (= (get css "--fg") (get css "--line") (get css "--line2")))
                         (expect (= (get css "--bg") (get css "--hover")))
                         (expect (= (get css "--primary") (get css "--ok-surface")))))))))

(defn- contrast
  "WCAG 2 contrast ratio of two RGB triples."
  [a b]
  (let [channel
        (fn [v]
          (let [s (/ (double v) 255.0)]
            (if (<= s 0.03928) (/ s 12.92) (Math/pow (/ (+ s 0.055) 1.055) 2.4))))

        luminance
        (fn [[r g b]]
          (+ (* 0.2126 (channel r)) (* 0.7152 (channel g)) (* 0.0722 (channel b))))

        la
        (luminance a)

        lb
        (luminance b)]

    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))

(defdescribe small-text-inks-read-on-their-ground-test
             ;; Measured: the iteration header of blockether-dark stood at 4.01:1 and its
             ;; blocked link at 2.13:1; the blocked link of vis-dark at 4.06:1.
             (doseq [[id {:keys [palette]}] theme/built-in-themes]
               (it id
                   (doseq [[fg bg] [[:iteration-header-fg :iteration-header-bg]
                                    [:link-chrome-blocked-fg :terminal-bg]
                                    [:link-chrome-blocked-fg :panel-bg] [:dialog-hint :dialog-bg]
                                    [:dialog-hint :panel-bg]]]
                     (expect (<= 4.5 (contrast (get palette fg) (get palette bg)))
                             (str id " " fg " on " bg))))))

(defdescribe field-edge-reads-on-every-field-ground-test
             ;; Measured: the edge of a form field stood at 1.13:1 in blockether-dark.
             ;; WCAG 1.4.11 asks 3:1 for the edge that shows where a control is.
             (doseq [[id t] theme/built-in-themes]
               (it id
                   (let [css (theme/theme->web-css-vars t)
                         rgb (fn [css-var]
                               (let [hex (subs (get css css-var) 1)]
                                 (mapv #(Integer/parseInt (subs hex % (+ % 2)) 16) [0 2 4])))]

                     (doseq [ground ["--input-field-bg" "--surface" "--bg" "--box-bg"]]
                       (expect (<= 3.0 (contrast (rgb "--field-edge") (rgb ground)))
                               (str id " --field-edge on " ground)))))))

(defdescribe the-auto-preference-id-test
             ;; `auto` is the preference that follows the system. An extension theme with
             ;; that id showed "Auto" two times in the theme picker.
             (it "refuses an extension theme with the id auto"
                 (try (expect (= :vis.internal.theme/reserved-theme-id
                                 (try (theme/register-themes! {"auto" {"MODE" "dark"}})
                                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                      (expect (not (contains? (theme/theme-registry) "auto")))
                      (finally (theme/unregister-theme! "auto")))))
