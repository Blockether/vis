(ns com.blockether.vis.internal.extension-test
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn [& _] nil)

;; ── STRONG flat native-tool spec (everything on the symbol) ───────────────────

(def ^:private a-render
  (fn [r]
    {:summary (str (:hits r) " hits")}))

(defn- ext-with [& syms] {:ext/name "test.lift" :ext/engine {:ext.engine/symbols (vec syms)}})

(defn flat-native-tool
  "A native tool declared the STRONG way — schema/name/handler on the SYMBOL."
  [_input]
  {:ok true})

(defdescribe
  flat-native-tool-spec-test
  (it ":native-tool? + symbol-level :schema/:name/:handler/:render produce the whole native surface"
      (let
        [sym
         (extension/symbol
           #'flat-native-tool
           {:tag :observation
            :native-tool? true
            :name "flat_tool"
            :description "Compact routing and result semantics."
            :schema {:type "object" :properties {"x" {:type "string"}}}
            :replay {:elide-args {"x" 1024} :retry-on #{:too-large} :retry-overrides {"force" true}}
            :handler (fn [_env _in]
                       {:ok true})
            :render a-render
            :color-role :tool-color/meta})

         ext
         (ext-with sym)

         schema
         (first (filter #(= "flat_tool" (:name %)) (extension/native-tool-schemas [ext])))]

        (expect (some? schema))
        (expect (= "Compact routing and result semantics." (:description schema)))
        (expect (= {:type "object" :properties {"x" {:type "string"}}} (:schema schema)))
        (expect (= {:elide-args {"x" 1024} :retry-on #{:too-large} :retry-overrides {"force" true}}
                   (get (extension/native-tool-replay-policies [ext]) "flat_tool")))
        (expect (fn? (get (extension/native-tool-handlers [ext]) "flat_tool")))
        (expect (= a-render (get (extension/native-tool-renderers [ext]) "flat_tool")))
        (expect (= :tool-color/meta (get (extension/native-tool-color-roles [ext]) "flat_tool")))))
  (it "a symbol with neither :native-tool? nor a legacy :native-tool map is NOT a native tool"
      (let
        [sym
         (extension/symbol #'flat-native-tool {:tag :observation})

         ext
         (ext-with sym)]

        (expect (empty? (extension/native-tool-schemas [ext])))
        (expect (empty? (extension/native-tool-handlers [ext])))))
  (it "a native description remains separate from the implementation docstring"
      (let
        [sym
         (extension/symbol #'flat-native-tool
                           {:tag :observation
                            :native-tool? true
                            :name "flat_tool"
                            :schema {:type "object"}
                            :description "explicit model-facing desc"})

         ext
         (ext-with sym)

         schema
         (first (filter #(= "flat_tool" (:name %)) (extension/native-tool-schemas [ext])))]

        (expect (= "explicit model-facing desc" (:description schema)))))
  (it ":native-tool? true WITHOUT a compact :description is rejected at build time"
      (expect (try (extension/symbol #'flat-native-tool
                                     {:tag :observation
                                      :native-tool? true
                                      :name "no_description_tool"
                                      :schema {:type "object"}})
                   false
                   (catch Throwable _ true))))
  (it "doc text combines compact semantics with schema parameters exactly once"
      (let
        [sym
         (extension/symbol #'flat-native-tool
                           {:tag :observation
                            :native-tool? true
                            :name "flat_tool"
                            :description "Compact routing and result semantics."
                            :schema {:type "object"
                                     :properties {"query" {:oneOf [{:type "string"}
                                                                   {:type "array"
                                                                    :items {:type "string"}}]
                                                           :description "Exact query input."}}
                                     :required ["query"]}})

         doc
         (extension/symbol-doc-text sym)]

        (expect (= 1 (count (re-seq #"Compact routing" doc))))
        (expect (not (re-find #"A native tool declared the STRONG way" doc)))
        (expect (= 1 (count (re-seq #"`query`" doc))))
        (expect (re-find #"string\|array<string>, required" doc))))
  (it "generic extension prompts omit native tools and their implementation docstrings"
      (let
        [native
         (extension/symbol #'flat-native-tool
                           {:tag :observation
                            :native-tool? true
                            :description "Native routing only."
                            :schema {:type "object" :properties {}}})

         python-only
         (extension/symbol #'flat-native-tool {:tag :observation})

         prompt
         (extension/render-prompt {:heading "TOOLS" :symbols [native python-only]})]

        (expect (re-find #"TOOLS" prompt))
        (expect (re-find #"A native tool declared the STRONG way" prompt))
        (expect (not (re-find #"Native routing only" prompt)))))
  (it ":native-tool? true WITHOUT a :schema is rejected at build time"
      (expect (try (extension/symbol #'flat-native-tool
                                     {:tag :observation :native-tool? true :name "no_schema_tool"})
                   false
                   (catch Throwable _ true))))
  (it "rejects provider-incompatible top-level schema unions at build time"
      (let
        [err (try (extension/symbol #'flat-native-tool
                                    {:tag :observation
                                     :native-tool? true
                                     :description "Bad root union."
                                     :schema {:type "object" :anyOf [{:required ["x"]}]}})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :extension/native-tool-nonportable-schema (:type (ex-data err)))))))

(defdescribe prompt-normalization-test
             (it "normalizes string and fn extension prompts"
                 (let
                   [prompt-text
                    "\n\n    First line\n\n\n\n      Nested line\n"

                    string-ext
                    (extension/extension {:ext/name "test.prompt-string"
                                          :ext/description "Test prompt string."
                                          :ext/prompt-fn prompt-text})

                    fn-ext
                    (extension/extension {:ext/name "test.prompt-fn"
                                          :ext/description "Test prompt fn."
                                          :ext/prompt-fn (fn [_]
                                                           prompt-text)})]

                   (expect (= "First line\n\n  Nested line" ((:ext/prompt-fn string-ext) {})))
                   (expect (= "First line\n\n  Nested line" ((:ext/prompt-fn fn-ext) {}))))))

(defdescribe ctx-contributions-test
             (it "binds active workspace root while building extension ctx"
                 (let
                   [root
                    (.getCanonicalPath (java.io.File. "target/test-workspace-ctx"))

                    ext
                    {:ext/name "test.ctx-workspace"
                     :ext/ctx-fn (fn [_]
                                   {:project {:ctx-root workspace/*workspace-root*
                                              :cwd (.getCanonicalPath (workspace/cwd))}})}

                    ctx
                    (extension/ctx-contributions {:workspace/root root} [ext])]

                   (expect (= root (get-in ctx [:project :ctx-root])))
                   (expect (= root (get-in ctx [:project :cwd]))))))

(defdescribe
  channel-contributions-test
  (it "extension accepts channel contributions and derives channel kind"
      (let
        [ext (extension/extension {:ext/name "test.channel-contribution"
                                   :ext/description "Test channel contribution."
                                   :ext/channel-contributions {:tui.slot/commands
                                                               [{:id :test/command
                                                                 :fn #'sample-channel-fn}]}})]
        (expect (= "channels" (:ext/kind ext)))
        (expect (= {:tui.slot/commands [{:id :test/command :fn #'sample-channel-fn}]}
                   (:ext/channel-contributions ext)))))
  (it "normalizes slot keys into channel-id and slot fields"
      (with-redefs
        [extension/registered-extensions
         (fn []
           [{:ext/channel-contributions
             {:tui.slot/commands [{:id :voice/input :fn #'sample-channel-fn}]
              :api.slot/preamble [{:id :api/preamble :fn #'sample-channel-fn}]}}])]
        (expect
          (= [{:id :voice/input :fn #'sample-channel-fn :channel-id :tui :slot :tui.slot/commands}]
             (extension/channel-contributions-for :tui :tui.slot/commands)))
        (expect (= [:tui.slot/commands] (mapv :slot (extension/channel-contributions-for :tui)))))))

(defdescribe
  workspace-backend-extension-test
  (it "registers and deregisters workspace backends with their extension"
      (let
        [backend-id
         :test/extension-workspace

         ext-name
         "test.workspace-backend"

         backend
         (workspace/workspace-backend {:workspace.backend/id backend-id
                                       :workspace.backend/priority 500
                                       :workspace.backend/capabilities #{:isolated-fork :rollback}
                                       :workspace.backend/available-fn (constantly true)
                                       :workspace.backend/fork-fn (fn [_]
                                                                    "/tmp/test-workspace")
                                       :workspace.backend/discard-fn (fn [_]
                                                                       nil)})]

        (try (extension/register-extension! {:ext/name ext-name
                                             :ext/description "Workspace backend registration test."
                                             :ext/workspace-backends [backend]})
             (expect (some #(= backend-id (:workspace.backend/id %))
                           (workspace/registered-backends)))
             (finally (extension/deregister-extension! ext-name)))
        (expect (not-any? #(= backend-id (:workspace.backend/id %))
                          (workspace/registered-backends))))))

(defdescribe startable-resource-visibility-test
             ;; The SAME `registered-startable-resources` feeds the web Resources modal AND
             ;; the TUI resource dialog, so a `:visible-fn` gate (e.g. MCP behind
             ;; :mcp/enabled) hides a startable from BOTH channels at once. This pins that
             ;; filter so the two surfaces can never drift.
             (it
               "drops startables whose :visible-fn is false; keeps gate-less + true ones"
               (let
                 [ext-name
                  "test.startable-visibility"

                  flag
                  (atom false)

                  kinds
                  (fn []
                    (set (map :kind (extension/registered-startable-resources))))]

                 (try (extension/register-extension! {:ext/name ext-name
                                                      :ext/description "Startable visibility test."
                                                      :ext/startable-resources [{:kind :test/always
                                                                                 :label "always"
                                                                                 :start-fn (fn [_ _]
                                                                                             nil)}
                                                                                {:kind :test/gated
                                                                                 :label "gated"
                                                                                 :start-fn (fn [_ _]
                                                                                             nil)
                                                                                 :visible-fn
                                                                                 (fn []
                                                                                   @flag)}]})
                      ;; flag false → gated startable hidden, always-on one present
                      (reset! flag false)
                      (expect (contains? (kinds) :test/always))
                      (expect (not (contains? (kinds) :test/gated)))
                      ;; flip the gate on → it appears (same fn, both channels see it)
                      (reset! flag true)
                      (expect (contains? (kinds) :test/gated))
                      (finally (extension/deregister-extension! ext-name)))
                 (expect (not (contains? (kinds) :test/gated)))))
             (it "a throwing :visible-fn fails OPEN (shown), never hides a needed control"
                 (let
                   [ext-name
                    "test.startable-visibility-throw"

                    kinds
                    (fn []
                      (set (map :kind (extension/registered-startable-resources))))]

                   (try (extension/register-extension!
                          {:ext/name ext-name
                           :ext/description "Startable visibility fail-open test."
                           :ext/startable-resources [{:kind :test/boom
                                                      :label "boom"
                                                      :start-fn (fn [_ _]
                                                                  nil)
                                                      :visible-fn (fn []
                                                                    (throw (ex-info "nope" {})))}]})
                        (expect (contains? (kinds) :test/boom))
                        (finally (extension/deregister-extension! ext-name))))))

(defdescribe
  slash-command-registration-test
  (it "slash path collisions across extensions are rejected at register-extension! time"
      ;; The union of `:ext/slash-commands` across all registered
      ;; extensions must contain unique `[parent name]`
      ;; paths. A second extension that declares the same path as an
      ;; already-registered extension is refused. Hot reload of the
      ;; SAME extension id is still allowed because its prior entry is
      ;; excluded from the conflict scan.
      (try (extension/register-extension! {:ext/name "test.slash-collide-a"
                                           :ext/description "first owner of /probe"
                                           :ext/slash-commands [{:slash/name "probe"
                                                                 :slash/doc "probe original"
                                                                 :slash/run-fn (fn [_]
                                                                                 {:slash/status
                                                                                  :ok})}]})
           (let
             [thrown (try (extension/register-extension!
                            {:ext/name "test.slash-collide-b"
                             :ext/description "duplicate owner of /probe"
                             :ext/slash-commands [{:slash/name "probe"
                                                   :slash/doc "probe dup"
                                                   :slash/run-fn (fn [_]
                                                                   {:slash/status :ok})}]})
                          nil
                          (catch clojure.lang.ExceptionInfo e (ex-data e)))]
             (expect (= :extension/slash-path-collision (:type thrown)))
             (expect (= ["probe"]
                        (-> thrown
                            :collisions
                            first
                            :path))))
           ;; Hot-reload of the SAME id with the SAME path = allowed.
           (expect (some? (extension/register-extension!
                            {:ext/name "test.slash-collide-a"
                             :ext/description "reload owner of /probe"
                             :ext/slash-commands [{:slash/name "probe"
                                                   :slash/doc "probe reloaded"
                                                   :slash/run-fn (fn [_]
                                                                   {:slash/status :ok})}]})))
           (finally (extension/deregister-extension! "test.slash-collide-a")
                    (extension/deregister-extension! "test.slash-collide-b")))))

(defdescribe
  op-hook-test
  "Generic cross-cutting operation hooks: any extension may decorate an op it
   does NOT own, wired once at the invoke-symbol-wrapper chokepoint."
  (let
    [run-after
     @#'extension/run-op-after-hooks

     run-before
     @#'extension/run-op-before-hooks

     run-around
     @#'extension/run-op-around]

    (it "after-hooks compose: the result threads through each registered hook"
        (extension/register-op-hook! {:op :ophtest1
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (update-in r [:result :n] (fnil inc 0)))})
        (extension/register-op-hook! {:op :ophtest1
                                      :owner :b
                                      :fn (fn [_ _ _ r]
                                            (update-in r [:result :n] (fnil inc 0)))})
        (let [out (run-after :ophtest1 {} [] (extension/success {:result {:n 0}}))]
          (expect (= 2 (get-in out [:result :n])))))
    (it "registration is idempotent per owner+phase — re-register REPLACES, no dup"
        (extension/register-op-hook! {:op :ophtest2
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (assoc-in r [:result :v] 1))})
        (extension/register-op-hook! {:op :ophtest2
                                      :owner :a
                                      :fn (fn [_ _ _ r]
                                            (assoc-in r [:result :v] 9))})
        (let [out (run-after :ophtest2 {} [] (extension/success {:result {}}))]
          (expect (= 9 (get-in out [:result :v])))
          (expect (= 1 (count (get (deref @#'extension/op-hooks) :ophtest2))))))
    (it "a throwing after-hook is SKIPPED (best-effort) and the result is unchanged"
        (extension/register-op-hook! {:op :ophtest3
                                      :owner :a
                                      :fn (fn [_ _ _ _]
                                            (throw (ex-info "boom" {})))})
        (let [base (extension/success {:result {:ok true}})]
          (expect (= base (run-after :ophtest3 {} [] base)))))
    (it "before-hooks can rewrite the args vector"
        (extension/register-op-hook! {:op :ophtest4
                                      :phase :before
                                      :owner :a
                                      :fn (fn [_ _ args]
                                            (conj (vec args) :extra))})
        (expect (= [:x :extra] (run-before :ophtest4 {} [:x]))))
    (it "no :around hook → run-op-around is just (apply f args)"
        (expect (= 3 (run-around :ophtest-none {} + [1 2]))))
    (it "around middleware wraps the call, catches a throw, and recovers"
        (extension/register-op-hook! {:op :ophtest5
                                      :phase :around
                                      :owner :a
                                      :fn (fn [_ _ args nxt]
                                            (try (nxt args) (catch Throwable _ :recovered)))})
        (expect (= :recovered
                   (run-around :ophtest5
                               {}
                               (fn [& _]
                                 (throw (ex-info "boom" {})))
                               [:x]))))
    (it "around middleware can RETRY with rewritten args (the don't-fail pattern)"
        (let [attempts (atom 0)]
          (extension/register-op-hook! {:op :ophtest6
                                        :phase :around
                                        :owner :a
                                        :fn (fn [_ _ args nxt]
                                              (try (nxt args) (catch Throwable _ (nxt [:fixed]))))})
          (let
            [f (fn [a]
                 (swap! attempts inc)
                 (if (= a :fixed) :ok (throw (ex-info "nope" {}))))]
            (expect (= :ok (run-around :ophtest6 {} f [:bad])))
            (expect (= 2 @attempts)))))
    (it "declarative :ext/op-hooks install on register and tear down on deregister"
        (try (extension/register-extension! {:ext/name "test.ophooks-ext"
                                             :ext/description "declarative op-hooks lifecycle"
                                             :ext/op-hooks [{:op :ophtest-decl
                                                             :phase :after
                                                             :fn (fn [_ _ _ r]
                                                                   r)}]})
             (let [hooks (get (deref @#'extension/op-hooks) :ophtest-decl)]
               (expect (= 1 (count hooks)))
               ;; owner derived from the ext name, no explicit :owner in the manifest
               (expect (= :ext/test.ophooks-ext (:owner (first hooks)))))
             (extension/deregister-extension! "test.ophooks-ext")
             (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-decl)))
             (finally (extension/deregister-extension! "test.ophooks-ext"))))
    (it "unregister-op-hooks-for-owner! dynamically tears down ALL of an owner's hooks"
        (extension/register-op-hook! {:op :ophtest-o1
                                      :owner :ext/zz
                                      :fn (fn [_ _ _ r]
                                            r)})
        (extension/register-op-hook! {:op :ophtest-o2
                                      :owner :ext/zz
                                      :fn (fn [_ _ _ r]
                                            r)})
        (extension/unregister-op-hooks-for-owner! :ext/zz)
        (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-o1)))
        (expect (nil? (get (deref @#'extension/op-hooks) :ophtest-o2))))))

(defdescribe reload-hooks-test
             (it "runs every hook and isolates failures per id"
                 (let [ran (atom [])]
                   (extension/register-reload-hook! ::rh-ok
                                                    (fn []
                                                      (swap! ran conj :ok)))
                   (extension/register-reload-hook! ::rh-boom
                                                    (fn []
                                                      (throw (ex-info "boom" {}))))
                   (try (let [results (extension/run-reload-hooks!)]
                          (expect (= [:ok] @ran))
                          (expect (true? (:ok? (get results ::rh-ok))))
                          (expect (false? (:ok? (get results ::rh-boom))))
                          (expect (= "boom" (:error (get results ::rh-boom)))))
                        (finally
                          ;; neutralize the fixtures so other tests' /reload paths stay clean
                          (extension/register-reload-hook! ::rh-ok
                                                           (fn []
                                                             nil))
                          (extension/register-reload-hook! ::rh-boom
                                                           (fn []
                                                             nil))))))
             (it "re-registering an id replaces the hook (idempotent)"
                 (let [n (atom 0)]
                   (extension/register-reload-hook! ::rh-idem
                                                    (fn []
                                                      (swap! n inc)))
                   (extension/register-reload-hook! ::rh-idem
                                                    (fn []
                                                      (swap! n + 10)))
                   (try (extension/run-reload-hooks!)
                        (expect (= 10 @n))
                        (finally (extension/register-reload-hook! ::rh-idem
                                                                  (fn []
                                                                    nil)))))))

(def ^:private folded->pos #'com.blockether.vis.internal.extension/folded-kwargs->positional)

(defdescribe
  folded-kwargs->positional-test
  (it "all-kwargs on a shaped tool bind IDENTICALLY to positional"
      ;; shell_logs :call {:pos ["id"] :opt-pos ["n"]} — the reported defect:
      ;; `shell_logs(id=…, n=…)` folded to one dict must expand to [id n].
      (expect (= ["lint" 100] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint" "n" 100}])))
      (expect (= ["lint"] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint"}])))
      ;; shell_run/{:pos [cmd] :rest :opt}: leftover keys ride a trailing opts dict.
      (expect (= ["ls"] (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls"}])))
      (expect (= ["ls" {"cwd" "/tmp"}]
                 (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls" "cwd" "/tmp"}])))
      ;; shell_bg/{:pos [id cmd]} and shell_send/{:pos [id text] :rest :opt}.
      (expect (= ["x" "sleep 1"] (folded->pos {:pos ["id" "cmd"]} [{"id" "x" "cmd" "sleep 1"}])))
      (expect (= ["x" "hi"]
                 (folded->pos {:pos ["id" "text"] :rest :opt} [{"id" "x" "text" "hi"}]))))
  (it "leaves everything ambiguous or already-correct untouched"
      ;; already positional — pass through verbatim.
      (expect (= ["lint" 100] (folded->pos {:pos ["id"] :opt-pos ["n"]} ["lint" 100])))
      ;; no :call shape — the generic single-dict tools keep their lone map.
      (expect (= [{"id" "lint"}] (folded->pos nil [{"id" "lint"}])))
      ;; a required :pos key missing — a genuine single-map positional, not kwargs.
      (expect (= [{"id" "x"}] (folded->pos {:pos ["id" "cmd"]} [{"id" "x"}])))
      ;; undeclared key with no :rest — do not silently drop it.
      (expect (= [{"id" "x" "bogus" 1}] (folded->pos {:pos ["id"]} [{"id" "x" "bogus" 1}])))
      ;; function-valued shape — cannot key-spread, leave as-is.
      (expect (= [{"id" "x"}]
                 (folded->pos (fn [_]
                                "src")
                              [{"id" "x"}])))
      ;; empty / multi-arg / non-map — never touched.
      (expect (= [] (folded->pos {:pos ["id"]} [])))
      (expect (= ["x" {"n" 1}] (folded->pos {:pos ["id"]} ["x" {"n" 1}])))))
