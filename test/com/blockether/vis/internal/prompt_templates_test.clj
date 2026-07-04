(ns com.blockether.vis.internal.prompt-templates-test
  "File-based prompt templates: frontmatter, discovery precedence,
   `$ARGUMENTS` expansion, and provider-contributed dynamic templates."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt-templates :as templates]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp-dir* [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-prompt-templates-"})]
    (try (f (.toFile tmp))
      (finally (fs/delete-tree tmp)))))

(defdescribe parse-invocation-test
  (it "splits /name from the argument tail"
    (expect (= {:name "deploy" :args "staging fast"}
              (templates/parse-invocation "/deploy staging fast"))))
  (it "no args -> empty string"
    (expect (= {:name "deploy" :args ""} (templates/parse-invocation "/deploy"))))
  (it "namespaced template names pass through (skill:foo)"
    (expect (= "skill:commit" (:name (templates/parse-invocation "/skill:commit now")))))
  (it "plain prose is not an invocation"
    (expect (nil? (templates/parse-invocation "deploy this")))
    (expect (nil? (templates/parse-invocation "/ leading space")))
    (expect (nil? (templates/parse-invocation nil)))))

(defdescribe discover-in-test
  (it "parses frontmatter name/description; filename stem is the fallback name"
    (with-tmp-dir*
      (fn [^java.io.File root]
        (let [proj (doto (java.io.File. root "proj") .mkdirs)]
          (spit (java.io.File. proj "review.md")
            "---\ndescription: Review the diff\n---\nDo a review.")
          (spit (java.io.File. proj "named.md")
            "---\nname: custom-name\n---\nBody here.")
          (let [ts (templates/discover-in proj nil)
                by-name (into {} (map (juxt :name identity)) ts)]
            (expect (= #{"review" "custom-name"} (set (keys by-name))))
            (expect (= "Review the diff" (:description (by-name "review"))))
            (expect (str/includes? (:body (by-name "review")) "Do a review.")))))))

  (it "project template wins a name collision with global"
    (with-tmp-dir*
      (fn [^java.io.File root]
        (let [proj   (doto (java.io.File. root "proj") .mkdirs)
              global (doto (java.io.File. root "global") .mkdirs)]
          (spit (java.io.File. proj "deploy.md") "PROJECT-BODY")
          (spit (java.io.File. global "deploy.md") "GLOBAL-BODY")
          (spit (java.io.File. global "release.md") "RELEASE-BODY")
          (let [ts (templates/discover-in proj global)
                by-name (into {} (map (juxt :name identity)) ts)]
            (expect (= "PROJECT-BODY" (:body (by-name "deploy"))))
            (expect (= :project (:scope (by-name "deploy"))))
            ;; non-colliding global templates still discovered
            (expect (= "RELEASE-BODY" (:body (by-name "release")))))))))

  (it "missing dirs are fine"
    (expect (= [] (templates/discover-in nil nil)))))

(defdescribe expansion-test
  (it "substitutes every $ARGUMENTS occurrence"
    (with-tmp-dir*
      (fn [^java.io.File root]
        (let [proj (doto (java.io.File. root "proj") .mkdirs)]
          (spit (java.io.File. proj "fix.md") "Fix $ARGUMENTS and test $ARGUMENTS.")
          (with-redefs [templates/file-templates
                        (constantly (templates/discover-in proj nil))]
            (let [{:keys [text]} (templates/expand {} "/fix the bug")]
              (expect (= "Fix the bug and test the bug." text))))))))

  (it "appends args as a trailing paragraph when body has no $ARGUMENTS"
    (with-tmp-dir*
      (fn [^java.io.File root]
        (let [proj (doto (java.io.File. root "proj") .mkdirs)]
          (spit (java.io.File. proj "review.md") "Review the code.")
          (with-redefs [templates/file-templates
                        (constantly (templates/discover-in proj nil))]
            (expect (= "Review the code.\n\nfocus on errors"
                      (:text (templates/expand {} "/review focus on errors"))))
            (expect (= "Review the code."
                      (:text (templates/expand {} "/review")))))))))

  (it "unknown template -> nil (engine falls back to unknown-slash error)"
    (with-redefs [templates/file-templates (constantly [])]
      (expect (nil? (templates/expand {} "/no-such-template args"))))))

(defdescribe provider-test
  (it "provider templates are consulted after file templates and can expand dynamically"
    (templates/register-provider! ::test-provider
      (fn [] [{:name "dyn" :description "dynamic"
               :expand-fn (fn [env args] (str "ENV=" (:k env) " ARGS=" args))}]))
    (try
      (with-redefs [templates/file-templates (constantly [])]
        (let [{:keys [text]} (templates/expand {:k "v"} "/dyn go")]
          (expect (= "ENV=v ARGS=go" text))))
      (finally
        ;; deregister so other tests don't see the fixture provider
        (templates/register-provider! ::test-provider (constantly nil)))))

  (it "a file template shadows a same-named provider template"
    (templates/register-provider! ::shadow-provider
      (fn [] [{:name "deploy" :body "PROVIDER-BODY"}]))
    (try
      (with-redefs [templates/file-templates
                    (constantly [{:name "deploy" :body "FILE-BODY" :scope :project}])]
        (expect (= "FILE-BODY" (:text (templates/expand {} "/deploy")))))
      (finally
        (templates/register-provider! ::shadow-provider (constantly nil))))))
