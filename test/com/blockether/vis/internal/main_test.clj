(ns com.blockether.vis.internal.main-test
  "Smoke tests for `com.blockether.vis.internal.main` — the host CLI
   binary's namespace. Exercises the public introspection fn
   (`list-extensions`) plus the private rendering helpers powering
   `vis extensions list`: namespace shortener and word-wrapper."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.main :as main]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private short-ext-ns #'main/short-ext-ns)
(def ^:private wrap-str     #'main/wrap-str)

(defdescribe short-ext-ns-test
  (it "rewrites the canonical extension package as `v/`"
    (expect (= "v/foundation.core"
              (short-ext-ns 'com.blockether.vis.ext.foundation.core)))
    (expect (= "v/provider-github-copilot"
              (short-ext-ns 'com.blockether.vis.ext.provider-github-copilot))))
  (it "passes through namespaces outside the canonical package"
    (expect (= "some.third.party.ext"
              (short-ext-ns 'some.third.party.ext)))))

(defdescribe wrap-str-test
  (describe "short input"
    (it "returns a single line when the input fits"
      (expect (= ["hello"] (wrap-str "hello" 10))))
    (it "returns one empty line for blank input"
      (expect (= [""] (wrap-str "" 10)))
      (expect (= [""] (wrap-str "   " 10)))))

  (describe "word wrap"
    (it "splits on whitespace and never overflows the column"
      (let [lines (wrap-str "the quick brown fox jumps over the lazy dog" 12)]
        (expect (every? #(<= (count %) 12) lines))
        (expect (= "the quick brown fox jumps over the lazy dog"
                  (str/join " " lines))))))

  (describe "hard split"
    (it "breaks tokens longer than the column width"
      (let [lines (wrap-str "supercalifragilisticexpialidocious" 10)]
        (expect (every? #(<= (count %) 10) lines))
        (expect (= "supercalifragilisticexpialidocious"
                  (apply str lines)))))))

(defdescribe list-extensions-test
  (it "returns a vec of row maps with the documented keys"
    (let [rows (main/list-extensions)]
      (expect (vector? rows))
      (expect (= (count (extension/registered-extensions)) (count rows)))
      (doseq [r rows]
        (expect (every? #(contains? r %)
                  [:namespace :doc :group :subgroup :version :cli-cmds]))
        (expect (string? (:namespace r)))
        (expect (string? (:group r)))
        (expect (string? (:subgroup r))))))

  (it "shortens every extension namespace with the `v/` prefix"
    (doseq [{:keys [namespace]} (main/list-extensions)]
      ;; Either a `v/` shortening, OR a non-canonical package
      ;; (test-only fixtures, third-party bundles) passes through.
      (expect (or (str/starts-with? namespace "v/")
                (not (str/starts-with? namespace "com.blockether.vis.ext."))))))

  (it "copies every provider label into :subgroup when the extension contributes one"
    (doseq [ext (extension/registered-extensions)]
      (let [labels   (->> (:ext/providers ext) (keep :provider/label))
            ext-name (str (:ext/namespace ext))
            row      (->> (main/list-extensions)
                       (filter #(or (= (:namespace %) ext-name)
                                  (and (str/starts-with? (:namespace %) "v/")
                                    (= ext-name
                                      (str "com.blockether.vis.ext."
                                        (subs (:namespace %) 2))))))
                       first)]
        (when (and row (seq labels))
          (doseq [label labels]
            (expect (str/includes? (:subgroup row) label))))))))
