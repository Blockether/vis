(ns com.blockether.vis.ext.foundation.environment.render-test
  (:require
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.environment.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private base-host
  {:cwd        "/tmp/x"
   :user       "alice"
   :home       "/home/alice"
   :os-name    "Linux"
   :os-version "6.6.0"
   :os-arch    "aarch64"
   :shell      "/bin/zsh"
   :locale     "en-US"
   :time       "2026-05-05T14:30:00-07:00"
   :timezone   "America/Los_Angeles"
   :jvm        "OpenJDK 21"})

(defdescribe render-test
  (it "renders a minimal block with no git, no languages, no monorepo"
    (let [out (render/render {:host base-host :git nil :languages nil :monorepo nil})]
      (expect (string/starts-with? out "<environment>\n"))
      (expect (string/ends-with?   out "</environment>\n"))
      (expect (string/includes?    out "cwd: /tmp/x"))
      (expect (string/includes?    out "user: alice (home: /home/alice)"))
      (expect (string/includes?    out "<current_time timezone=\"America/Los_Angeles\">2026-05-05T14:30:00-07:00</current_time>"))
      (expect (string/includes?    out "platform: Linux 6.6.0 (aarch64)"))
      (expect (not (string/includes? out "git.")))
      (expect (not (string/includes? out "languages:")))
      (expect (not (string/includes? out "monorepo:")))))

  (it "annotates cwd as `(= git root)` when cwd matches the repo root"
    (let [git {:root "/tmp/x" :branch "main" :detached? false
               :worktree? false :submodules? false :clean? true
               :dirty? false :changes? false
               :modified 0 :untracked 0 :added 0 :changed 0
               :missing 0 :removed 0 :conflicting 0}
          out (render/render {:host base-host :git git
                              :languages nil :monorepo nil})]
      (expect (string/includes? out "cwd: /tmp/x (= git root)"))
      (expect (string/includes? out "git.branch: main"))
      (expect (string/includes? out "git.status: clean"))
      (expect (string/includes? out "git.summary: stale: unknown | changes: no | dirty: no | stash: no (0)"))
      (expect (string/includes? out "submodules: false"))
      (expect (string/includes? out "worktree: false"))))

  (it "annotates cwd as a subpath when cwd is below the repo root"
    (let [git {:root "/tmp/repo" :branch "feat" :detached? false
               :worktree? false :submodules? false :clean? true
               :modified 0 :untracked 0 :added 0 :changed 0
               :missing 0 :removed 0 :conflicting 0}
          out (render/render {:host (assoc base-host :cwd "/tmp/repo/src/foo")
                              :git git :languages nil :monorepo nil})]
      (expect (string/includes? out "subpath: src/foo"))))

  (it "shows detached HEAD with the short sha when off-branch"
    (let [git {:root "/tmp/x" :branch nil :detached? true
               :detached-sha "abcdef012345"
               :worktree? false :submodules? false :clean? true
               :modified 0 :untracked 0 :added 0 :changed 0
               :missing 0 :removed 0 :conflicting 0}
          out (render/render {:host base-host :git git
                              :languages nil :monorepo nil})]
      (expect (string/includes? out "git.branch: detached @ abcdef012345"))))

  (it "summarizes dirty status as a comma list"
    (let [git {:root "/tmp/x" :branch "main" :detached? false
               :worktree? false :submodules? false :clean? false
               :modified 3 :untracked 2 :added 0 :changed 0
               :missing 0 :removed 0 :conflicting 0}
          out (render/render {:host base-host :git git
                              :languages nil :monorepo nil})]
      (expect (string/includes? out "dirty (3 modified, 2 untracked)"))
      (expect (string/includes? out "changes: unknown | dirty: unknown | stash: no (0)"))))

  (it "renders multirepo paths with Git summaries"
    (let [repositories {:count 2
                        :repositories [{:path "." :branch "main" :clean? true
                                        :dirty? false :changes? false :stale? false
                                        :stash-count 0}
                                       {:path "services/api" :branch "feature"
                                        :clean? false :dirty? true :changes? true
                                        :stale? true :upstream "origin/main"
                                        :ahead 0 :behind 2 :stash-count 1}]}
          out (render/render {:host base-host :git nil :languages nil
                              :monorepo nil :repositories repositories})]
      (expect (string/includes? out "repositories: 2 git repos"))
      (expect (string/includes? out "- .: main; stale: no | changes: no | dirty: no | stash: no (0)"))
      (expect (string/includes? out "- services/api: feature; stale: yes (upstream: origin/main, ahead: 0, behind: 2) | changes: yes | dirty: yes | stash: yes (1)"))))

  (it "renders top languages with bytes-percentage and a primary line"
    (let [languages {:total-files 100 :total-bytes 100000
                     :truncated? false :elapsed-ms 12
                     :primary "clojure"
                     :languages [{:language "clojure" :files 78 :bytes 78000
                                  :files-pct 0.78 :bytes-pct 0.78}
                                 {:language "markdown" :files 12 :bytes 12000
                                  :files-pct 0.12 :bytes-pct 0.12}]}
          out (render/render {:host base-host :git nil
                              :languages languages :monorepo nil})]
      (expect (string/includes? out "clojure 78%"))
      (expect (string/includes? out "markdown 12%"))
      (expect (string/includes? out "primary-language: clojure"))
      (expect (string/includes? out "100 files"))))

  (it "drops monorepo line when shape is nil"
    (let [out (render/render {:host base-host :git nil :languages nil
                              :monorepo {:shape nil :totals {} :files {}}})]
      (expect (not (string/includes? out "monorepo:"))))))
