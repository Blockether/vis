(ns com.blockether.vis.ext.foundation-core.environment.digest-test
  "`:session/env` slim digest contract — host / project / extensions slices.

   Tests are deterministic. Each case stubs the upstream snapshot fn and
   active-extensions reader so the digest contract stays independent of
   the running JVM / repo state."
  (:require
   [com.blockether.vis.ext.foundation-core.environment.agents :as agents]
   [com.blockether.vis.ext.foundation-core.environment.core :as env]
   [com.blockether.vis.ext.foundation-core.environment.digest :as digest]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private fake-snapshot
  {:host {:cwd        "/Users/test/proj"
          :os-name    "Mac OS X"
          :shell      "/bin/zsh"
          :time       "2026-05-24T01:12:33+02:00"}
   :languages {:total-bytes 1000
               :primary "Clojure"
               :languages [{:language "Clojure" :bytes 910}
                           {:language "Markdown" :bytes 50}
                           {:language "SQL" :bytes 20}
                           {:language "EDN" :bytes 20}]}
   :monorepo {:shape :polylith
              :totals {:bricks 8}}
   :repositories nil
   :git nil})

(defdescribe digest-host-slice-test
  (it "lifts cwd, normalised OS, shell, and ISO clock"
    (with-redefs [env/snapshot   (constantly fake-snapshot)
                  agents/instructions (constantly {:found? true})]
      (let [d (digest/digest)]
        (expect (= "/Users/test/proj" (get-in d [:host :cwd])))
        (expect (= :macos             (get-in d [:host :os])))
        (expect (= :zsh               (get-in d [:host :shell])))
        (expect (= "2026-05-24T01:12:33+02:00"
                  (get-in d [:host :clock]))))))

  (it "handles Linux / Windows / unknown OS strings"
    (with-redefs [env/snapshot (constantly (assoc-in fake-snapshot
                                             [:host :os-name] "Linux"))
                  agents/instructions (constantly {:found? false})]
      (expect (= :linux (get-in (digest/digest) [:host :os]))))
    (with-redefs [env/snapshot (constantly (assoc-in fake-snapshot
                                             [:host :os-name] "Windows 11"))
                  agents/instructions (constantly {:found? false})]
      (expect (= :windows (get-in (digest/digest) [:host :os]))))
    (with-redefs [env/snapshot (constantly (assoc-in fake-snapshot
                                             [:host :os-name] "Plan9"))
                  agents/instructions (constantly {:found? false})]
      (expect (= :unknown (get-in (digest/digest) [:host :os]))))))

(defdescribe digest-project-slice-test
  (it "derives :kind, primary language, language share, brick count"
    (with-redefs [env/snapshot   (constantly fake-snapshot)
                  agents/instructions (constantly {:found? true})]
      (let [p (:project (digest/digest))]
        (expect (= :polylith (:kind p)))
        (expect (= :clojure (:primary-language p)))
        (expect (true? (:agents-md? p)))
        (expect (= 8 (:extension-count p)))
        ;; Language share is rounded to 0.01 so the section stays compact.
        (expect (= 0.91 (get-in p [:language-share :clojure])))
        (expect (= 0.05 (get-in p [:language-share :markdown]))))))

  (it "marks :single when no monorepo shape is detected"
    (with-redefs [env/snapshot (constantly (assoc fake-snapshot :monorepo nil))
                  agents/instructions (constantly {:found? false})]
      (let [p (:project (digest/digest))]
        (expect (= :single (:kind p)))
        (expect (nil? (:extension-count p)))
        (expect (false? (:agents-md? p)))))))

(defdescribe digest-extensions-slice-test
  (it "counts active extensions and collects SCI aliases"
    (with-redefs [env/snapshot   (constantly fake-snapshot)
                  agents/instructions (constantly {:found? true})
                  prompt/active-extensions
                  (constantly [{:ext/name "foundation-core"
                                :ext/sci {:ext.sci/alias 'v}}
                               {:ext/name "foundation-git"
                                :ext/sci {:ext.sci/alias 'git}}
                               {:ext/name "no-alias-ext"}])]
      (let [e (:extensions (digest/digest {:fake true}))]
        (expect (= 3 (:active-count e)))
        (expect (= #{'v 'git} (:aliases e))))))

  (it "is absent when no environment is provided (digest fn no-arg arity)"
    (with-redefs [env/snapshot   (constantly fake-snapshot)
                  agents/instructions (constantly {:found? false})]
      (expect (nil? (:extensions (digest/digest)))))))

(defdescribe digest-resilience-test
  (it "returns an empty map when every upstream blows up"
    (with-redefs [env/snapshot (fn [] (throw (ex-info "boom" {})))
                  agents/instructions (fn [] (throw (ex-info "boom" {})))
                  prompt/active-extensions (fn [_] (throw (ex-info "boom" {})))]
      (let [d (digest/digest {})]
        ;; project slice still lands because it falls back to nil snapshot
        ;; \u2192 :kind :single. Host slice is omitted when host is nil.
        (expect (map? d))
        (expect (nil? (:host d)))
        (expect (= :single (get-in d [:project :kind])))))))
