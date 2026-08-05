(ns com.blockether.vis.internal.extension-test
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as vis-loop]
            [com.blockether.vis.internal.prompt :as prompt]
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
  (it
    ":native-tool? + symbol-level :schema/:name/:handler/:render produce the whole native surface"
    (let
      [sym
       (extension/symbol
         #'flat-native-tool
         {:tag :observation
          :native-tool? true
          :name "flat_tool"
          :description "Compact routing and result semantics."
          :result "A map with boolean `ok`."
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
      (expect (= 'flat-native-tool (:symbol (first (extension/native-tools-for [ext])))))
      (expect (not (contains? schema :symbol)))
      (expect (= "Compact routing and result semantics.\n\nRaw result: A map with boolean `ok`."
                 (:description schema)))
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
                            :description "explicit model-facing desc"
                            :result "an explicit result map"})

         ext
         (ext-with sym)

         schema
         (first (filter #(= "flat_tool" (:name %)) (extension/native-tool-schemas [ext])))]

        (expect (= "explicit model-facing desc\n\nRaw result: an explicit result map"
                   (:description schema)))))
  (it ":native-tool? true WITHOUT a compact :description is rejected at build time"
      (expect (try (extension/symbol #'flat-native-tool
                                     {:tag :observation
                                      :native-tool? true
                                      :name "no_description_tool"
                                      :result "a result map"
                                      :schema {:type "object"}})
                   false
                   (catch Throwable _ true))))
  (it ":native-tool? true WITHOUT a raw :result contract is rejected at build time"
      (let
        [err (try (extension/symbol #'flat-native-tool
                                    {:tag :observation
                                     :native-tool? true
                                     :name "no_result_tool"
                                     :description "Has semantics."
                                     :schema {:type "object"}})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (= :extension/native-tool-missing-result (:type (ex-data err))))))
  (it "reserves the Raw result label for centralized projection"
      (doseq
        [[opts expected-type] [[{:description "Raw result: duplicated" :result "a map"}
                                :extension/native-tool-result-in-description]
                               [{:description "Has semantics." :result "Raw result: duplicated"}
                                :extension/native-tool-result-has-label]]]
        (let
          [err (try (extension/symbol #'flat-native-tool
                                      (merge {:tag :observation
                                              :native-tool? true
                                              :name "bad_result_label_tool"
                                              :schema {:type "object"}}
                                             opts))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
          (expect (= expected-type (:type (ex-data err)))))))
  (it "budgets the whole model-facing prose surface, schema descriptions included"
      (doseq
        [[label opts]
         [["a bloated :description"
           {:description (apply str (repeat 2100 "x")) :result "A map." :schema {:type "object"}}]
          ["bloated nested schema :description entries"
           {:description "Compact."
            :result "A map."
            :schema {:type "object"
                     :properties {"q" {:type "string"
                                       :description (apply str (repeat 2100 "x"))}}}}]]]
        (let
          [err (try (extension/symbol
                      #'flat-native-tool
                      (merge {:tag :observation :native-tool? true :name "bloated_tool"} opts))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
          (expect (= :extension/native-tool-over-budget (:type (ex-data err))) label))))
  (it "every registered builtin native tool stays inside the prose budget"
      (let
        [budget
         @#'extension/native-prose-budget

         tools
         (->> (#'extension/registered-extensions)
              (mapcat #(get-in % [:ext/engine :ext.engine/symbols]))
              (filterv :ext.symbol/native-tool?))]

        ;; never let this pass vacuously on an unloaded registry
        (expect (< 10 (count tools)))
        (doseq [sym tools]
          (expect (<= (long (#'extension/native-prose-chars sym)) budget)
                  (str (:ext.symbol/symbol sym)
                       " spends " (#'extension/native-prose-chars sym)
                       " of " budget)))))
  (it "doc text combines compact semantics with schema parameters exactly once"
      (let
        [sym
         (extension/symbol #'flat-native-tool
                           {:tag :observation
                            :native-tool? true
                            :name "flat_tool"
                            :description "Compact routing and result semantics."
                            :result "A map with boolean `ok`."
                            :schema {:type "object"
                                     :properties {"query" {:oneOf [{:type "string"}
                                                                   {:type "array"
                                                                    :items {:type "string"}}]
                                                           :description "Exact query input."}}
                                     :required ["query"]}})

         doc
         (extension/symbol-doc-text sym)]

        (expect (= 1 (count (re-seq #"Compact routing" doc))))
        (expect (= 1 (count (re-seq #"Raw result:" doc))))
        (expect (= 1 (count (re-seq #"A map with boolean `ok`" doc))))
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
                            :result "A native result."
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
                                     {:tag :observation
                                      :native-tool? true
                                      :name "no_schema_tool"
                                      :description "No schema."
                                      :result "A result map."})
                   false
                   (catch Throwable _ true))))
  (it "rejects provider-incompatible top-level schema unions at build time"
      (let
        [err (try (extension/symbol #'flat-native-tool
                                    {:tag :observation
                                     :native-tool? true
                                     :description "Bad root union."
                                     :result "A result map."
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
     @#'extension/run-op-before-hooks]

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
    (it "the generic operation dispatcher invokes a host operation with no :around hook"
        (expect (= 3 (extension/invoke-operation :ophtest-none {} + [1 2]))))
    (it "around middleware wraps the call, catches a throw, and recovers"
        (extension/register-op-hook! {:op :ophtest5
                                      :phase :around
                                      :owner :a
                                      :fn (fn [_ _ args nxt]
                                            (try (nxt args) (catch Throwable _ :recovered)))})
        (expect (= :recovered
                   (extension/invoke-operation :ophtest5
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
            (expect (= :ok (extension/invoke-operation :ophtest6 {} f [:bad])))
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
      ;; a :call {:pos ["id"] :opt-pos ["n"]} tool — the reported defect:
      ;; `tool(id=…, n=…)` folded to one dict must expand to [id n].
      (expect (= ["lint" 100] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint" "n" 100}])))
      (expect (= ["lint"] (folded->pos {:pos ["id"] :opt-pos ["n"]} [{"id" "lint"}])))
      ;; shell/{:opt-pos [cmd] :rest :opt}: leftover keys ride a trailing opts dict.
      (expect (= ["ls"] (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls"}])))
      (expect (= ["ls" {"cwd" "/tmp"}]
                 (folded->pos {:pos ["cmd"] :rest :opt} [{"cmd" "ls" "cwd" "/tmp"}])))
      ;; an id+cmd shape folds to [id cmd], an op-only call folds to [id {op …}],
      ;; and a {:pos [id text] :rest :opt} shape folds to [id text].
      (expect (= ["x" "sleep 1"]
                 (folded->pos {:pos ["id"] :opt-pos ["cmd"] :rest :opt}
                              [{"id" "x" "cmd" "sleep 1"}])))
      (expect (= ["x" {"op" "stop"}]
                 (folded->pos {:pos ["id"] :opt-pos ["cmd"] :rest :opt} [{"id" "x" "op" "stop"}])))
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

(defn constrained-native-tool
  "A native tool whose schema carries JSON-schema validation constraints."
  [_input]
  {:ok true})

(defdescribe
  wire-schema-constraints-test
  (it
    "enforceable bounds survive; provider-unenforceable bounds become description prose"
    (let
      [sym
       (extension/symbol #'constrained-native-tool
                         {:tag :observation
                          :native-tool? true
                          :name "constrained_tool"
                          :description "Edits files."
                          :result "One row per edit."
                          :schema {:type "object"
                                   :properties {"edits" {:type "array"
                                                         :minItems 1
                                                         :maxItems 8
                                                         :description "Edit maps."
                                                         :items {:type "object"
                                                                 :properties {"path" {:type "string"
                                                                                      :minLength
                                                                                      1}}}}
                                                "pair" {:type "array" :minItems 2 :maxItems 2}
                                                "depth" {:type "integer" :minimum 1 :maximum 9}}
                                   :required ["edits"]}})

       schema
       (:schema (first (filter #(= "constrained_tool" (:name %))
                               (extension/native-tool-schemas [(ext-with sym)]))))

       edits
       (get-in schema [:properties "edits"])]

      ;; `minItems 0/1` is the one array bound a tool API keeps as a real
      ;; constraint, so it survives on the wire.
      (expect (= 1 (:minItems edits)))
      ;; The live API rejects `maxItems` outright and every `minItems` past 0/1,
      ;; so those are inlined instead of dropped.
      (expect (nil? (:maxItems edits)))
      (expect (= "Edit maps. {maxItems: 8}" (:description edits)))
      (expect (= {:type "array" :description "{maxItems: 2, minItems: 2}"}
                 (get-in schema [:properties "pair"])))
      ;; Outside what a tool API enforces ⇒ inlined into the node's own description,
      ;; never dropped, appended after an existing description.
      (expect (= {:type "string" :description "{minLength: 1}"}
                 (get-in edits [:items :properties "path"])))
      (expect (= {:type "integer" :description "{minimum: 1, maximum: 9}"}
                 (get-in schema [:properties "depth"])))
      (expect (= ["edits"] (:required schema)))))
  (it "rewrites oneOf unions to anyOf — no tool-schema validator accepts oneOf"
      (expect (= {:type "object"
                  :properties {"x" {:anyOf [{:type "string"} {:type "integer"}]}}
                  :additionalProperties false}
                 (#'extension/wire-schema
                  {:type "object"
                   :properties {"x" {:oneOf [{:type "string"} {:type "integer"}]}}
                   :additionalProperties false})))))

;; Regression: switching to `github-copilot`/`gpt-5.6-terra` (an
;; OpenAI-compatible chat wire) made EVERY turn fail before a single token, with
;; no provider information in the TUI beyond "turn failed" — the provider 400ed
;; the whole request: `Invalid schema for function 'struct_index': In
;; context=('properties', 'paths', 'items', 'anyOf', '1'), 'required' is required
;; to be supplied and to be an array including every key in properties. Missing
;; 'ranges'.` Vis used to stamp `:strict true` on every schema it judged
;; grammar-samplable — a judgement calibrated on ANTHROPIC's subset, which allows
;; an optional property and a real `minItems` bound. Nothing is advertised strict.
(defdescribe
  unconstrained-tools-test
  (it "the very shape an OpenAI-compatible wire refused is advertised untouched"
      (let
        [schema
         {:type "object"
          :properties {"paths" {:type "array"
                                :minItems 1
                                :items {:anyOf [{:type "string"}
                                                {:type "object"
                                                 :properties {"path" {:type "string"}
                                                              "ranges" {:type "array"
                                                                        :items {:type "integer"}}}
                                                 :required ["path"]
                                                 :additionalProperties false}]}}}
          :required ["paths"]
          :additionalProperties false}

         tool
         (extension/advertise-tool {:name "struct_index" :description "d" :schema schema})]

        (expect (nil? (:strict tool)))
        (expect (= schema (:schema tool)))))
  (it "a closed, fully-required scalar schema is not advertised strict either"
      ;; This one is inside EVERY provider's strict subset, and the flag is gone
      ;; regardless: no advertised tool depends on which wire built the request.
      (let
        [tool (extension/advertise-tool {:name "scalars"
                                         :description "d"
                                         :schema {:type "object"
                                                  :properties {"q" {:type "string"}}
                                                  :required ["q"]
                                                  :additionalProperties false}})]
        (expect (nil? (:strict tool)))
        (expect (= #{:name :description :schema} (set (keys tool)))))))

;; ── ONE context for the extension and the session (issue #104) ────────────────
;;
;; Regression, issue #104: each callback site installed its OWN SUBSET of the
;; context. The turn hooks, the ctx contribution and the prompt/activation
;; callbacks bound the extension identity but NOT the session env, so anything
;; those callbacks raised — `vis.ask` above all — was session-less: the gateway
;; bridge had no session to append the request to and no surface ever drew it.
(defdescribe
  one-extension-context-test
  (it
    "every extension callback site runs inside the session's own context"
    (let
      [seen
       (atom [])

       snap
       (fn [site]
         (swap! seen conj
           {:site site
            :session-id (:session-id extension/*current-environment*)
            :root workspace/*workspace-root*
            :ext (extension/current-extension-id)})
         nil)

       phase-hook
       (fn [phase]
         {:id (keyword "one-context" (name phase))
          :phase phase
          :fn (fn [_ctx]
                (snap phase))})

       ext
       {:ext/name "one-context-probe"
        :ext/hooks [(phase-hook :turn.answer/validate) (phase-hook :turn.iteration/start)]
        :ext/ctx-fn (fn [_env]
                      (snap :ext/ctx-fn)
                      {})
        :ext/prompt-fn (fn [_env]
                         (snap :ext/prompt-fn))
        :ext/activation-fn (fn [_env]
                             (snap :ext/activation-fn)
                             true)}

       root
       (workspace/normalize-root (System/getProperty "user.dir"))

       env
       {:session-id "sid-one-context" :workspace/root root :extensions (atom [ext])}]

      (vis-loop/final-answer-gate-error env 1 [] "an answer" [ext])
      (#'vis-loop/collect-iteration-start-hints env [ext] {:environment env})
      (extension/ctx-contributions env [ext])
      (prompt/active-extensions env)
      (#'prompt/extensions-prompt-block env [ext])
      (expect (= #{:turn.answer/validate :turn.iteration/start :ext/ctx-fn :ext/prompt-fn
                   :ext/activation-fn}
                 (set (map :site @seen))))
      ;; ONE context: same session, same workspace, same identity, every site.
      (expect (every? (fn [s]
                        (= {:session-id "sid-one-context" :root root :ext "one-context-probe"}
                           (dissoc s :site)))
                      @seen))))
  (it "a callback handed no environment of its own keeps the caller's session"
      ;; Regression, issue #104: a caller with nothing to say about the session
      ;; passes an EMPTY env — `(prompt-fn {})`. That used to REPLACE the live
      ;; context with an empty map, so `vis.state` fell through to the
      ;; process-wide DB and `vis.ask` raised a session-less request. An empty
      ;; env is not a context: it inherits the ambient one, and only a real env
      ;; replaces it.
      (extension/with-context
        {:env {:session-id "sid-ambient" :db-info ::probe-db}}
        (expect (= "sid-ambient"
                   (extension/with-context {:env {}}
                                           (:session-id extension/*current-environment*))))
        (expect (= ::probe-db
                   (extension/with-context {:env nil} (:db-info extension/*current-environment*))))
        (expect (= "sid-inner"
                   (extension/with-context {:env {:session-id "sid-inner"}}
                                           (:session-id extension/*current-environment*)))))))

;; ── format_code/lint_code/etc. share invoke-symbol-wrapper with draft-aware cwd ──
;;
;; Turn 10-13's bug (`git`/`shell`/`repl*` bypassing `with-context`) lived in
;; `run-native-handler`, the seam ONLY for symbols carrying an explicit
;; `:ext.symbol/handler`. `format_code`/`lint_code` (and every plain
;; `:inject-env? true` engine symbol without a `:handler`) never went through
;; that seam at all: they dispatch through `invoke-symbol-wrapper`, which has
;; wrapped every call in `with-context` since before this investigation
;; started. This pins that the SAME generic path — the one
;; `format_code`/`lint_code` actually use — resolves `workspace/*workspace-root*`
;; from the call's own `:workspace/root`, so a draft session's format/lint
;; already run against the draft, not trunk.
(defn draft-cwd-probe
  "A plain engine-bound symbol shaped like format_code/lint_code: no :handler,
   :inject-env? true so the live env lands as its first argument."
  [env]
  (extension/success {:result {"root" (str workspace/*workspace-root*)
                               "env-root" (str (:workspace/root env))}}))

(defdescribe invoke-symbol-wrapper-workspace-root-test
             (it "binds workspace/*workspace-root* from :workspace/root for a plain engine symbol"
                 (let
                   [draft-root
                    (.getCanonicalPath (java.io.File. "target/test-draft-cwd-probe"))

                    sym
                    (extension/symbol #'draft-cwd-probe {:tag :observation :inject-env? true})

                    ext
                    {:ext/name "test.draft-cwd-probe" :ext/engine {:ext.engine/symbols [sym]}}

                    result
                    (extension/invoke-symbol-wrapper ext sym [] {:workspace/root draft-root})]

                   (expect (= draft-root (get result "root")))
                   (expect (= draft-root (get result "env-root"))))))
