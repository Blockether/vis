(ns com.blockether.vis.ext.foundation.environment.render-test
  (:require
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.environment.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-host
  {:cwd "/tmp/x"
   :user "alice"
   :time "2026-05-05T14:30:00-07:00"})

(defdescribe render-test
  (it "renders runtime snapshot as ctx data comment with no XML"
    (let [out (render/render {:host base-host :git {:root "/tmp/x"}})]
      (expect (string/starts-with? out ";; ctx.runtime =\n"))
      (expect (string/includes? out ":host"))
      (expect (string/includes? out ":git"))
      (expect (not (string/includes? out "<environment>")))))

  (it "renders project guidance as comments with source/path metadata"
    (let [out (render/format-project-guidance-block
                {:found? true :source :repo :path "AGENTS.md" :content "rules"})]
      (expect (string/includes? out ";; ctx.project-guidance ="))
      (expect (string/includes? out "AGENTS.md"))
      (expect (string/includes? out "rules"))
      (expect (not (string/includes? out "<project-guidance")))))

  (it "renders scan warnings as ctx data comments"
    (let [out (render/format-scan-warnings-block [{:path "x" :reason "bad"}])]
      (expect (string/includes? out ";; ctx.scan-warnings ="))
      (expect (string/includes? out ":reason"))
      (expect (not (string/includes? out "<scan-warnings")))))

  (it "drops empty optional fragments"
    (expect (nil? (render/format-project-guidance-block {:found? false})))
    (expect (nil? (render/format-scan-warnings-block [])))))
