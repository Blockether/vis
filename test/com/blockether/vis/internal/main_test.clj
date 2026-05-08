(ns com.blockether.vis.internal.main-test
  "Smoke tests for `com.blockether.vis.internal.main` — the host CLI
   binary's namespace. Exercises the public introspection fn
   (`list-extensions`) plus the private rendering helpers powering
   `vis extensions list`: namespace shortener and word-wrapper."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.main :as main]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.provider-limits :as provider-limits]
   [com.blockether.vis.internal.registry :as registry]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private short-ext-ns            #'main/short-ext-ns)
(def ^:private wrap-str                #'main/wrap-str)
(def ^:private expand-table-cols       #'main/expand-table-cols)
(def ^:private table-width             #'main/table-width)
(def ^:private terminal-width          #'main/terminal-width)
(def ^:private print-table!            #'main/print-table!)
(def ^:private parse-run-args          #'main/parse-run-args)
(def ^:private scaffold-extension-files #'main/scaffold-extension-files)
(def ^:private parse-scaffold-opts     #'main/parse-scaffold-opts)
(def ^:private conversation-rows       #'main/conversation-rows)
(def ^:private strip-global-args       #'main/strip-global-args)
(def ^:private startup-measure?        #'main/startup-measure?)
(def ^:private root-help-request?      #'main/root-help-request?)
(def ^:private help-request?           #'main/help-request?)
(def ^:private channel-help-request?   #'main/channel-help-request?)
(def ^:private config-with-model-override #'main/config-with-model-override)

(defdescribe result-json-test
  (it "normalizes nested non-string map keys in the trace envelope"
    (let [slot-key [(java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
                    :verification]
          encoded  (main/result->json
                     {:answer "ok"
                      :status :done
                      :trace [{:expected-proof {:slots {slot-key {:required? true}}}
                               :entry (first {:k :v})
                               :exception (ex-info "boom" {:reason :bad-key})}]})
          decoded  (json/read-json encoded)]
      (expect (= "ok" (get decoded "answer")))
      (expect (= "done" (get decoded "status")))
      (expect (= {"required?" true}
                (get-in decoded ["trace" 0 "expected-proof" "slots"
                                 "[#uuid \"00000000-0000-0000-0000-000000000001\" :verification]"])))
      (expect (= ["k" "v"] (get-in decoded ["trace" 0 "entry"])))
      (expect (= "boom" (get-in decoded ["trace" 0 "exception" "message"])))
      (expect (= "bad-key" (get-in decoded ["trace" 0 "exception" "data" "reason"]))))))

(defdescribe extension-scaffold-test
  (it "builds a minimal extension project layout"
    (let [files (scaffold-extension-files {:name "my-tools"})]
      (expect (= #{"deps.edn"
                   "resources/META-INF/vis-extension/vis.edn"
                   "src/vis/ext/my_tools.clj"}
                (set (keys files))))
      (expect (str/includes? (get files "src/vis/ext/my_tools.clj")
                "(def vis-extension"))
      (expect (str/includes? (get files "src/vis/ext/my_tools.clj")
                "(vis/register-extension! vis-extension)"))
      (expect (str/includes? (get files "resources/META-INF/vis-extension/vis.edn")
                "vis.ext.my-tools"))))

  (it "parses scaffold options from residual argv"
    (expect (= {:name "My-Extension"
                :dir "custom-dir"
                :namespace "my.ns"
                :force? true}
              (parse-scaffold-opts {} ["My Extension" "--dir" "custom-dir" "--namespace" "my.ns" "--force"])))))

(defdescribe cli-run-persistence-test
  (it "defaults `vis run` to ephemeral execution and requires --persist for disk writes"
    (expect (= {:prompt "throwaway probe"}
              (parse-run-args ["throwaway" "probe"])))
    (expect (= {:persist? true :prompt "keep this"}
              (parse-run-args ["--persist" "keep" "this"]))))

  (it "run! uses an in-memory environment unless persistence is explicitly requested"
    (let [events (atom [])]
      (with-redefs [config/resolve-config (fn ([] {:providers [{:id :openai :models [{:name "gpt-5"}]}]})
                                            ([_] {:providers [{:id :openai :models [{:name "gpt-5"}]}]}))
                    lp/get-router (fn [] :router)
                    lp/create-environment (fn [router opts]
                                            (swap! events conj [:create-environment router opts])
                                            :env)
                    lp/turn! (fn [env messages opts]
                               (swap! events conj [:turn env messages opts])
                               {:answer "ok" :iteration-count 1 :duration-ms 2})
                    lp/dispose-environment! (fn [env]
                                              (swap! events conj [:dispose env]))
                    lp/create! (fn [& _]
                                 (throw (ex-info "persistent create must not run by default" {})))
                    lp/send! (fn [& _]
                               (throw (ex-info "persistent send must not run by default" {})))]
        (let [result (main/run! (main/agent {:name "cli"}) "hi")]
          (expect (= nil (:conversation-id result)))
          (expect (= "ok" (:answer result)))
          (expect (= [[:create-environment :router {:db :memory}]
                      [:turn :env [(svar/user "hi")] {}]
                      [:dispose :env]]
                    @events))))))

  (it "run! persists only when :persist? is true"
    (let [cid    (java.util.UUID/randomUUID)
          events (atom [])]
      (with-redefs [config/resolve-config (fn ([] {:providers [{:id :openai :models [{:name "gpt-5"}]}]})
                                            ([_] {:providers [{:id :openai :models [{:name "gpt-5"}]}]}))
                    lp/create! (fn [channel opts]
                                 (swap! events conj [:create channel opts])
                                 {:id cid})
                    lp/send! (fn [conversation-id messages opts]
                               (swap! events conj [:send conversation-id messages opts])
                               {:answer "saved" :iteration-count 1 :duration-ms 2})
                    lp/create-environment (fn [& _]
                                            (throw (ex-info "ephemeral env must not run when persisted" {})))]
        (let [result (main/run! (main/agent {:name "cli"}) "save me" {:persist? true})]
          (expect (= cid (:conversation-id result)))
          (expect (= "saved" (:answer result)))
          (expect (= [[:create :cli {:title "save me"}]
                      [:send cid [(svar/user "save me")] {}]]
                    @events))))))

  (it "run! honors provider-qualified model overrides without mutating config order"
    (let [base-config {:providers [{:id :openai-codex
                                    :models [{:name "gpt-5.5"}]}
                                   {:id :zai-coding
                                    :base-url "https://api.z.ai/api/coding/paas/v4"
                                    :api-key "tok"
                                    :models [{:name "glm-5-turbo"}
                                             {:name "glm-5.1"}]}]}
          db-spec     {:backend :sqlite :path ".verification/proof-regression/test/vis.db"}
          events      (atom [])]
      (expect (= [:openai-codex :zai-coding] (mapv :id (:providers base-config))))
      (expect (= [:zai-coding :openai-codex]
                (mapv :id (:providers (config-with-model-override base-config "zai-coding/glm-5.1")))))
      (expect (= "glm-5.1"
                (get-in (config-with-model-override base-config "zai-coding/glm-5.1")
                  [:providers 0 :models 0 :name])))
      (expect (= [:openai-codex :zai-coding] (mapv :id (:providers base-config))))
      (with-redefs [config/resolve-config (fn ([] base-config) ([_] base-config))
                    svar/make-router (fn [providers]
                                       (swap! events conj [:make-router providers])
                                       {:providers providers})
                    lp/create-environment (fn [router opts]
                                            (swap! events conj [:create-environment router opts])
                                            :env)
                    lp/turn! (fn [env messages opts]
                               (swap! events conj [:turn env messages opts])
                               {:answer "ok" :iteration-count 1 :duration-ms 2
                                :cost {:provider :zai-coding :model "glm-5.1"}})
                    lp/dispose-environment! (fn [env]
                                              (swap! events conj [:dispose env]))]
        (let [result (main/run! (main/agent {:name "cli"}) "hi" {:model "zai-coding/glm-5.1"
                                                                 :db db-spec})
              router-providers (-> @events first second)]
          (expect (= "ok" (:answer result)))
          (expect (= :zai-coding (:id (first router-providers))))
          (expect (= "glm-5.1" (get-in router-providers [0 :models 0 :name])))
          (expect (= [:create-environment {:providers router-providers} {:db db-spec}]
                    (second @events))))))))

(defdescribe global-measure-flag-test
  (it "strips --measure before command dispatch"
    (expect (= ["help"] (strip-global-args ["--measure" "help"])))
    (expect (= ["providers" "list"]
              (strip-global-args ["providers" "--measure" "list"]))))

  (it "enables startup measurement from the global flag"
    (expect (startup-measure? ["--measure" "help"])))

  (it "recognizes root-only help requests before extension discovery"
    (doseq [args [[] ["help"] ["--help"] ["-h"]]]
      (expect (true? (root-help-request? args))))
    (doseq [args [["channels"] ["channels" "tui" "--help"] ["providers" "list"]]]
      (expect (false? (root-help-request? args)))))

  (it "recognizes help at any command depth for fast help dispatch"
    (doseq [args [[] ["help"] ["providers" "--help"] ["channels" "tui" "--help"]]]
      (expect (true? (help-request? args))))
    (expect (false? (help-request? ["providers" "list"])))
    (expect (true? (channel-help-request? ["channels" "tui" "--help"])))
    (expect (true? (channel-help-request? ["channels" "telegram" "-h"])))
    (expect (false? (channel-help-request? ["channels" "other" "--help"])))))

(defdescribe short-ext-ns-test
  (it "rewrites the canonical extension package as `v/`"
    (expect (= "v/foundation.core"
              (short-ext-ns 'com.blockether.vis.ext.foundation.core)))
    (expect (= "v/provider-github-copilot"
              (short-ext-ns 'com.blockether.vis.ext.provider-github-copilot))))
  (it "passes through namespaces outside the canonical package"
    (expect (= "some.third.party.ext"
              (short-ext-ns 'some.third.party.ext)))))

(defdescribe terminal-width-test
  (it "uses COLUMNS when the shell exports it"
    (with-redefs-fn {#'main/terminal-env      (fn [_] "177")
                     #'main/shell-first-line (fn [_] nil)}
      #(expect (= 177 (terminal-width)))))

  (it "falls back to stty because interactive shells often do not export COLUMNS"
    (with-redefs-fn {#'main/terminal-env      (fn [_] nil)
                     #'main/shell-first-line (fn [cmd]
                                               (case cmd
                                                 "stty size < /dev/tty" "48 211"
                                                 "tput cols" "80"
                                                 nil))}
      #(expect (= 211 (terminal-width)))))

  (it "uses the non-interactive fallback when no terminal probe succeeds"
    (with-redefs-fn {#'main/terminal-env      (fn [_] nil)
                     #'main/shell-first-line (fn [_] nil)}
      #(expect (= 120 (terminal-width))))))

(defdescribe table-width-test
  (it "grows marked columns to fill the requested table width"
    (let [cols     [{:key :id :label "ID" :width 4 :align :left}
                    {:key :title :label "Title" :width 6 :align :left :grow? true}
                    {:key :date :label "Date" :width 4 :align :left}]
          expanded (expand-table-cols cols 40)]
      (expect (= 40 (table-width expanded)))
      (expect (= 4 (:width (first expanded))))
      (expect (= 24 (:width (second expanded))))
      (expect (= 4 (:width (nth expanded 2))))))

  (it "grows the final column when no column opts into growth"
    (let [cols     [{:key :a :label "A" :width 4 :align :left}
                    {:key :b :label "B" :width 4 :align :left}]
          expanded (expand-table-cols cols 20)]
      (expect (= 4 (:width (first expanded))))
      (expect (= 11 (:width (second expanded))))
      (expect (= 20 (table-width expanded)))))

  (it "renders table lines at the detected terminal width"
    (let [out (java.io.ByteArrayOutputStream.)
          ps  (java.io.PrintStream. out true "UTF-8")]
      (with-redefs-fn {#'main/terminal-width        (fn [] 50)
                       #'config/original-stdout ps}
        #(print-table! [{:key :id :label "ID" :width 4 :align :left}
                        {:key :title :label "Title" :width 6 :align :left :grow? true}]
           [{:id "1" :title "hello"}]))
      (.flush ps)
      (let [lines (str/split-lines (.toString out "UTF-8"))]
        (expect (seq lines))
        (expect (every? #(= 50 (count %)) lines))))))

(defdescribe conversation-rows-test
  (it "sorts by last turn descending, puts empty conversations last, and includes the last channel"
    (let [older-id  (java.util.UUID/randomUUID)
          newer-id  (java.util.UUID/randomUUID)
          empty-id  (java.util.UUID/randomUUID)
          convs     [{:id older-id :channel :telegram :title "older" :created-at (java.util.Date. 1000)}
                     {:id empty-id :channel :cli :title "empty" :fork-count 2 :created-at (java.util.Date. 9000)}
                     {:id newer-id :channel :tui :title "newer" :fork-count 1 :created-at (java.util.Date. 2000)}]
          turns     {older-id [{:created-at (java.util.Date. 3000)}]
                     newer-id [{:created-at (java.util.Date. 5000)}]}]
      (with-redefs [persistance/db-list-conversation-turns
                    (fn [_ cid] (get turns cid []))]
        (let [rows (conversation-rows :db convs)]
          (expect (= ["newer" "older" "empty"] (mapv :title rows)))
          (expect (= ["tui" "telegram" "cli"] (mapv :last-channel rows)))
          (expect (= [1 1 0] (mapv :turns rows)))
          (expect (= [1 0 2] (mapv :forks rows)))
          (expect (= "—" (:last-turn (last rows)))))))))

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
                  [:namespace :doc :kind :group :author :owner :license :version]))
        (expect (string? (:namespace r)))
        (expect (string? (:kind r)))
        (expect (string? (:group r)))
        (expect (string? (:author r)))
        (expect (string? (:owner r)))
        (expect (string? (:license r))))))

  (it "shortens every extension namespace with the `v/` prefix"
    (doseq [{:keys [namespace]} (main/list-extensions)]
      ;; Either a `v/` shortening, OR a non-canonical package
      ;; (test-only fixtures, third-party bundles) passes through.
      (expect (or (str/starts-with? namespace "v/")
                (not (str/starts-with? namespace "com.blockether.vis.ext."))))))

  (it "copies every provider label into :group when the extension's kind is providers"
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
        (when (and row (seq labels) (= "providers" (:kind row)))
          (doseq [label labels]
            (expect (str/includes? (:group row) label)))))))

  (it "copies every channel cmd into :group when the extension's kind is channels"
    (doseq [ext (extension/registered-extensions)]
      (let [cmds     (->> (:ext/channels ext) (keep :channel/cmd))
            ext-name (str (:ext/namespace ext))
            row      (->> (main/list-extensions)
                       (filter #(or (= (:namespace %) ext-name)
                                  (and (str/starts-with? (:namespace %) "v/")
                                    (= ext-name
                                      (str "com.blockether.vis.ext."
                                        (subs (:namespace %) 2))))))
                       first)]
        (when (and row (seq cmds) (= "channels" (:kind row)))
          (doseq [cmd cmds]
            (expect (str/includes? (:group row) cmd))))))))

(defdescribe configured-provider-status-test
  (it "treats config-backed api-key providers as authenticated and exposes the config path"
    (with-redefs [config/current-config (fn [] {:providers [{:id :zai
                                                             :api-key "tok"
                                                             :models [{:name "glm-5.1"}]}]})]
      (expect (= {:authenticated? true
                  :source :config
                  :config-path config/config-path}
                (@#'main/configured-provider-status {:provider/id :zai}))))))

(defdescribe provider-limit-lines-test
  (it "labels static svar metadata as catalog limits rather than live quota"
    (with-redefs [provider-limits/provider-limits (constantly {:provider-id :openai-codex
                                                               :status :ok
                                                               :static {:rpm 500 :tpm 2000000}
                                                               :dynamic {:limits []
                                                                         :note "No live quota endpoint."}})]
      (let [lines (@#'main/provider-limit-lines :openai-codex)]
        (expect (some #(str/includes? % "Catalog RPM:    500") lines))
        (expect (some #(str/includes? % "Catalog TPM:    2000000") lines))
        (expect (some #(str/includes? % "not live account quota usage") lines))))))

(defdescribe cli-providers-logout-test
  (it "clears provider token storage and removes the provider from config"
    (let [logout-called? (atom false)
          removed        (atom nil)
          out            (java.io.ByteArrayOutputStream.)
          ps             (java.io.PrintStream. out true "UTF-8")]
      (with-redefs [shutdown-agents (fn [] nil)
                    config/init-cli! (fn [] nil)
                    config/original-stdout ps
                    config/load-config-raw (fn [] {:providers [{:id :anthropic-coding-plan
                                                                :models [{:name "claude-sonnet-4-6"}]}]})
                    config/remove-config-provider! (fn [provider-id source]
                                                     (reset! removed {:provider-id provider-id :source source})
                                                     true)
                    registry/provider-by-id (fn [provider-id]
                                              (when (= :anthropic-coding-plan provider-id)
                                                {:provider/id :anthropic-coding-plan
                                                 :provider/label "Anthropic (Claude Subscription)"
                                                 :provider/logout-fn #(reset! logout-called? true)}))]
        (@#'main/cli-providers-logout! {} ["anthropic-coding-plan"])
        (.flush ps)
        (expect (= true @logout-called?))
        (expect (= {:provider-id :anthropic-coding-plan
                    :source :cli-provider-logout}
                  @removed))
        (expect (str/includes? (.toString out "UTF-8") "Tokens and config cleared"))))))

(defdescribe providers-list-rows-test
  (it "marks config-backed providers as authenticated in the CLI list"
    (with-redefs [config/current-config (fn [] {:providers [{:id :zai
                                                             :api-key "tok"
                                                             :base-url "https://api.z.ai/api/paas/v4"
                                                             :models [{:name "glm-5.1"}]}]})
                  registry/registered-providers (fn [] [{:provider/id :zai
                                                         :provider/label "Z.ai (Pass)"
                                                         :provider/status-fn (constantly {:authenticated? false})}])
                  provider-limits/provider-limits (constantly {:provider-id :zai
                                                               :status :ok
                                                               :static {:rpm 120 :tpm 3400}
                                                               :dynamic {:limits []}})]
      (expect (= [{:id "zai"
                   :label "Z.ai (Pass)"
                   :auth "yes"
                   :rpm "120"
                   :tpm "3400"
                   :base-url "https://api.z.ai/api/paas/v4"}]
                (@#'main/providers-list-rows))))))
