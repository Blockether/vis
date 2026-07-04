(ns com.blockether.vis.internal.env-digest-test
  "Internal `\"session_env\"` digest contract (STRING-KEYED).

   The digest is core functionality (drives the per-iter `;; ctx`),
   not extension-owned. Tests stub upstream agents + prompt readers so
   the contract stays independent of the running JVM / repo state."
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.env-digest :as env-digest]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp-root*
  [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-digest-test-"})]
    (try (f (.toFile tmp))
      (finally (fs/delete-tree tmp)))))

(defdescribe deep-merge-test
  (it "merges nested maps key-by-key, scalar wins on collision"
    (expect (= {:a 1 :b {:c 2 :d 3}}
              (env-digest/deep-merge {:a 1 :b {:c 2}} {:b {:d 3}}))))

  (it "later scalar replaces earlier"
    (expect (= {:a 2} (env-digest/deep-merge {:a 1} {:a 2}))))

  (it "nil in `b` preserves `a` value"
    (expect (= 1 (:a (env-digest/deep-merge {:a 1} {:a nil}))))))

(defdescribe base-digest-test
  (it "produces host + project + extensions when an environment is supplied"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions
                  (constantly [{:ext/name "foundation-core"
                                :ext/engine {:ext.engine/alias 'v}}
                               {:ext/name "foundation-git"
                                :ext/engine {:ext.engine/alias 'git}}
                               {:ext/name "no-alias-ext"}])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [d (env-digest/base-digest {:fake true})]
              (expect (contains? d "host"))
              ;; "cwd" removed — it duplicated session["workspace"]["root"].
              (expect (not (contains? (get d "host") "cwd")))
              (expect (string? (get-in d ["host" "os"])))
              (expect (contains? d "project"))
              ;; AGENTS.md content now rides in PROJECT-INSTRUCTIONS
              ;; system block; the digest no longer carries the flag.
              (expect (not (contains? (get d "project") "agents_md")))
              (expect (= "single" (get-in d ["project" "kind"])))
              (expect (= 3 (get-in d ["extensions" "active_count"])))
              (expect (= #{"v" "git"} (get-in d ["extensions" "aliases"])))))))))

  (it "detects polylith when bases/ + components/ are both present"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions (constantly [])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (.mkdir (java.io.File. root "bases"))
          (.mkdir (java.io.File. root "components"))
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [d (env-digest/base-digest nil)]
              (expect (= "polylith" (get-in d ["project" "kind"])))))))))

  (it "detects monorepo when one of packages/apps/modules/crates exists"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions (constantly [])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (.mkdir (java.io.File. root "packages"))
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (expect (= "monorepo" (get-in (env-digest/base-digest nil)
                                    ["project" "kind"]))))))))

  (it "guesses primary_language from canonical build files"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions (constantly [])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (spit (java.io.File. root "deps.edn") "{}")
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (expect (= "clojure" (get-in (env-digest/base-digest nil)
                                   ["project" "primary_language"]))))))))

  (it "never surfaces agents_md — AGENTS.md content lives in PROJECT-INSTRUCTIONS system block"
    (with-redefs [agents/instructions (constantly {:found? true
                                                   :source :repo
                                                   :path "/tmp/AGENTS.md"
                                                   :content "rule"})
                  prompt/active-extensions (constantly [])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [d (env-digest/base-digest nil)]
              (expect (not (contains? (get d "project") "agents_md")))))))))

  (it "skips extensions slice when no environment is provided"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions (constantly [])]
      (with-tmp-root*
        (fn [^java.io.File root]
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [d (env-digest/base-digest nil)]
              (expect (nil? (get d "extensions"))))))))))

(defdescribe session-env-merges-extension-contributions-test
  (it "deep-merges `\"session_env\"` slices coming from extension `:ext/ctx-fn`"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions
                  (constantly [{:ext/name "voice-ext"
                                :ext/engine  {:ext.engine/alias 'voice}}])
                  extension/ctx-contributions
                  (constantly {"session_env" {"voice" {"tts_loaded" true}}})]
      (with-tmp-root*
        (fn [^java.io.File root]
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [env-block (env-digest/session-env {:fake true} nil)]
              (expect (contains? env-block "host"))
              (expect (contains? env-block "voice"))
              (expect (true? (get-in env-block ["voice" "tts_loaded"])))))))))

  (it "treats non-`\"session_env\"` extension contributions as opaque (ignored here)"
    (with-redefs [agents/instructions (constantly {:found? false})
                  prompt/active-extensions (constantly [])
                  extension/ctx-contributions
                  (constantly {"session_workspace" {"should_be_ignored" true}
                               "session_env" {"custom" {"x" 1}}})]
      (with-tmp-root*
        (fn [^java.io.File root]
          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
            (let [env-block (env-digest/session-env {} nil)]
              (expect (contains? env-block "custom"))
              (expect (= 1 (get-in env-block ["custom" "x"])))
              (expect (not (contains? env-block "should_be_ignored"))))))))))
