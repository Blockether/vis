(ns com.blockether.vis.internal.context.env-digest-test
  "Internal `\"session_env\"` digest contract (STRING-KEYED).

   The digest is core functionality (drives the per-iter `;; ctx`),
   not extension-owned. Tests stub upstream agents + prompt readers so
   the contract stays independent of the running JVM / repo state."
  (:require [babashka.fs :as fs]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.context.env-digest :as env-digest]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp-root*
  [f]
  (let [^java.nio.file.Path tmp (fs/create-temp-dir {:prefix "vis-digest-test-"})]
    (try (f (.toFile tmp)) (finally (fs/delete-tree tmp)))))

(defdescribe
  deep-merge-test
  (it "merges nested maps key-by-key, scalar wins on collision"
      (expect (= {:a 1 :b {:c 2 :d 3}} (env-digest/deep-merge {:a 1 :b {:c 2}} {:b {:d 3}}))))
  (it "nil in `b` preserves `a` value" (expect (= 1 (:a (env-digest/deep-merge {:a 1} {:a nil}))))))

(defdescribe
  base-digest-test
  (it "produces host + project + extensions when an environment is supplied"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [{:ext/name "foundation-core" :ext/engine {:ext.engine/alias 'v}}
                                 {:ext/name "sample-tools" :ext/engine {:ext.engine/alias 'sample}}
                                 {:ext/name "no-alias-ext"}])]

        (with-tmp-root* (fn [^java.io.File root]
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
                              (expect (= #{"v" "sample"} (get-in d ["extensions" "aliases"])))))))))
  (it "detects polylith when bases/ + components/ are both present"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [])]

        (with-tmp-root* (fn [^java.io.File root]
                          (.mkdir (java.io.File. root "bases"))
                          (.mkdir (java.io.File. root "components"))
                          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                            (let [d (env-digest/base-digest nil)]
                              (expect (= "polylith" (get-in d ["project" "kind"])))))))))
  (it "detects monorepo when one of packages/apps/modules/crates exists"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [])]

        (with-tmp-root*
          (fn [^java.io.File root]
            (.mkdir (java.io.File. root "packages"))
            (binding [workspace/*workspace-root* (.getCanonicalPath root)]
              (expect (= "monorepo" (get-in (env-digest/base-digest nil) ["project" "kind"]))))))))
  (it "guesses primary_language from canonical build files"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [])]

        (with-tmp-root* (fn [^java.io.File root]
                          (spit (java.io.File. root "deps.edn") "{}")
                          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                            (expect (= "clojure"
                                       (get-in (env-digest/base-digest nil)
                                               ["project" "primary_language"]))))))))
  ;; The digest checks marker names one by one instead of listing the workspace,
  ;; which cost 150-400 ms per render in a 69 000-entry directory. A name must
  ;; still match the listing exactly, and a linked marker still counts.
  (it "matches marker names exactly, links included, without listing the root"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [])]

        (with-tmp-root* (fn [^java.io.File root]
                          (.mkdir (java.io.File. root "PACKAGES"))
                          (spit (java.io.File. root "Gemfile.lock") "")
                          (java.nio.file.Files/createSymbolicLink
                            (.toPath (java.io.File. root "go.mod"))
                            (.toPath (java.io.File. root "missing"))
                            (make-array java.nio.file.attribute.FileAttribute 0))
                          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                            (expect (= {"kind" "single" "primary_language" "go"}
                                       (get (env-digest/base-digest nil) "project")))))))))

(defdescribe
  session-env-merges-extension-contributions-test
  (it "deep-merges `\"session_env\"` slices coming from extension `:ext/ctx-fn`"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [{:ext/name "voice-ext" :ext/engine {:ext.engine/alias 'voice}}])

                    extension/ctx-contributions
                    (constantly {"session_env" {"voice" {"tts_loaded" true}}})]

        (with-tmp-root* (fn [^java.io.File root]
                          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                            (let [env-block (env-digest/session-env {:fake true} nil)]
                              (expect (contains? env-block "host"))
                              (expect (contains? env-block "voice"))
                              (expect (true? (get-in env-block ["voice" "tts_loaded"])))))))))
  (it "treats non-`\"session_env\"` extension contributions as opaque (ignored here)"
      (with-redefs [agents/instructions
                    (constantly {:found? false})

                    prompt/active-extensions
                    (constantly [])

                    extension/ctx-contributions
                    (constantly {"session_workspace" {"should_be_ignored" true}
                                 "session_env" {"custom" {"x" 1}}})]

        (with-tmp-root* (fn [^java.io.File root]
                          (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                            (let [env-block (env-digest/session-env {} nil)]
                              (expect (contains? env-block "custom"))
                              (expect (= 1 (get-in env-block ["custom" "x"])))
                              (expect (not (contains? env-block "should_be_ignored"))))))))))
