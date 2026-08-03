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

      ;; `minItems 1` is the one array bound the strict subset enforces: it stays,
      ;; and is what makes `"[{…}]"` unsamplable as a STRING.
      (expect (= 1 (:minItems edits)))
      ;; The live API rejects `maxItems` outright and every `minItems` past 0/1,
      ;; so those are inlined instead of dropped.
      (expect (nil? (:maxItems edits)))
      (expect (= "Edit maps. {maxItems: 8}" (:description edits)))
      (expect (= {:type "array" :description "{maxItems: 2, minItems: 2}"}
                 (get-in schema [:properties "pair"])))
      ;; Outside the Anthropic subset ⇒ inlined into the node's own description,
      ;; never dropped, appended after an existing description.
      (expect (= {:type "string" :description "{minLength: 1}"}
                 (get-in edits [:items :properties "path"])))
      (expect (= {:type "integer" :description "{minimum: 1, maximum: 9}"}
                 (get-in schema [:properties "depth"])))
      (expect (= ["edits"] (:required schema)))))
  (it "rewrites oneOf unions to anyOf — no strict subset accepts oneOf"
      (expect (= {:type "object"
                  :properties {"x" {:anyOf [{:type "string"} {:type "integer"}]}}
                  :additionalProperties false}
                 (#'extension/wire-schema
                  {:type "object"
                   :properties {"x" {:oneOf [{:type "string"} {:type "integer"}]}}
                   :additionalProperties false})))))

(defdescribe
  strict-samplable-schema-test
  (it "a closed object schema is advertised with :strict true"
      (let
        [tool (extension/advertise-tool
                {:name "patchy"
                 :description "d"
                 :schema {:type "object"
                          :properties {"edits" {:type "array"
                                                :minItems 1
                                                :items {:type "object"
                                                        :properties {"path" {:type "string"}}
                                                        :required ["path"]
                                                        :additionalProperties false}}}
                          :required ["edits"]
                          :additionalProperties false}})]
        (expect (true? (:strict tool)))
        (expect (= 1 (get-in tool [:schema :properties "edits" :minItems])))))
  (it "a FOREIGN/free-form object payload is never advertised strict"
      ;; `mcp__call`'s `args` is another server's schema; `struct_patch`'s edit
      ;; maps are heterogeneous. An open object cannot be grammar-constrained, so
      ;; the flag is simply absent instead of 400-ing the whole request.
      (expect (nil? (:strict (extension/advertise-tool
                               {:name "mcpish"
                                :description "d"
                                :schema {:type "object"
                                         :properties {"server" {:type "string"}
                                                      "args" {:type "object"
                                                              :description "Foreign args."}}
                                         :required ["server"]
                                         :additionalProperties false}}))))
      (expect (nil? (:strict (extension/advertise-tool {:name "editsish"
                                                        :description "d"
                                                        :schema {:type "object"
                                                                 :properties
                                                                 {"edits" {:type "array"
                                                                           :items {:type "object"}}}
                                                                 :required ["edits"]
                                                                 :additionalProperties false}})))))
  (it "an object that does not close itself is not strict-samplable"
      (expect (false? (extension/strict-samplable-schema?
                        {:type "object" :properties {"a" {:type "string"}} :required ["a"]})))
      (expect (false? (extension/strict-samplable-schema?
                        {:type "object"
                         :properties {"a" {:type "object" :additionalProperties false}}
                         :additionalProperties false})))
      (expect (true? (extension/strict-samplable-schema?
                       {:type "object"
                        :properties {"a" {:anyOf [{:type "string"}
                                                  {:type "object"
                                                   :properties {"b" {:type "string"}}
                                                   :additionalProperties false}]}}
                        :additionalProperties false})))))

(defn- batch-tool
  "A strict-samplable tool taking a BATCH — an array of maps, `payload-risk` 0 —
   plus `n` optional scalar properties."
  [name n]
  (extension/advertise-tool {:name name
                             :description "d"
                             :schema {:type "object"
                                      :properties
                                      (into {"edits" {:type "array"
                                                      :items {:type "object"
                                                              :properties {"path" {:type "string"}}
                                                              :required ["path"]
                                                              :additionalProperties false}}}
                                            (map (fn [i]
                                                   [(str "o" i) {:type "string"}]))
                                            (range n))
                                      :required ["edits"]
                                      :additionalProperties false}}))

(defn- flat-tool
  "A strict-samplable tool whose only container is an array of STRINGS —
   `payload-risk` 1."
  [name]
  (extension/advertise-tool {:name name
                             :description "d"
                             :schema {:type "object"
                                      :properties {"paths" {:type "array" :items {:type "string"}}}
                                      :required ["paths"]
                                      :additionalProperties false}}))

(defn- scalar-tool
  "A strict-samplable tool with scalars only — `payload-risk` 2, nothing a grammar
   could fix."
  [name]
  (extension/advertise-tool {:name name
                             :description "d"
                             :schema {:type "object"
                                      :properties {"q" {:type "string"}}
                                      :required ["q"]
                                      :additionalProperties false}}))

(def ^:private strict-grammar-budget-for-test
  "The production grammar budget, read from the var the wire projection uses."
  @#'extension/strict-grammar-budget)

(defn- grammar-spend
  "Total compiled-grammar cost of the tools that KEPT `:strict`."
  [tools]
  (reduce + 0 (map #(#'extension/schema-grammar-cost (:schema %)) (filter :strict tools))))

(defdescribe
  strict-budget-test
  (it "a batch of maps is what earns the request's grammar budget"
      ;; Every mis-serialized argument in this repo's journals was a nested
      ;; container; a scalars-only tool cannot be malformed that way, so it never
      ;; spends budget a batch tool could have used.
      (let [tools (extension/budget-strict-tools [(scalar-tool "scalars") (batch-tool "batch" 0)])]
        (expect (= [nil true] (mapv :strict tools)))))
  (it "the more exposed payloads outbid the flatter one for a scarce budget"
      ;; The flat array of strings is the CHEAPEST tool here and still loses: the
      ;; batches of maps are ranked first, and what they leave does not fit it.
      (let
        [tools
         (extension/budget-strict-tools (conj (mapv #(batch-tool (str "b" %) 1) (range 6))
                                              (flat-tool "flat")))

         strict
         (into #{} (comp (filter :strict) (map :name)) tools)]

        (expect (not (contains? strict "flat")))
        (expect (seq strict))
        (expect (every? #(= \b (first %)) strict))
        (expect (>= (long strict-grammar-budget-for-test) (long (grammar-spend tools))))))
  (it "the EXPENSIVE schemas lose strict once the request grammar is spent"
      ;; Anthropic compiles ONE grammar per request: individually legal strict
      ;; tools still 400 the whole call together. Cheapest first, deterministic.
      (let
        [tools
         (extension/budget-strict-tools (mapv #(batch-tool (str "b" %) 0) (range 40)))

         strict
         (filter :strict tools)]

        (expect (seq strict))
        (expect (< (count strict) (count tools)))
        (expect (>= (long strict-grammar-budget-for-test) (long (grammar-spend tools))))
        (expect (= (mapv :name tools) (mapv :name (extension/budget-strict-tools tools))))))
  (it "the optional-parameter limit binds before the grammar does"
      ;; Measured live: `Schemas contains too many optional parameters … limit: 24`.
      ;; Two 13-optional batches are cheap enough to compile and still illegal.
      (let [tools (extension/budget-strict-tools [(batch-tool "a" 13) (batch-tool "b" 13)])]
        (expect (= 1 (count (filter :strict tools))))
        (expect (>= (long strict-grammar-budget-for-test) (long (grammar-spend tools))))
        (expect (>= 24
                    (reduce +
                            0
                            (map #(#'extension/optional-parameter-count (:schema %))
                                 (filter :strict tools)))))))
  (it "unconstrained tools cost nothing and are left alone"
      ;; The provider counts the optional parameters of STRICT tools only, so an
      ;; open/foreign payload never crowds out a constrained one.
      (let
        [foreign
         {:name "mcpish" :description "d" :schema {:type "object"}}

         tools
         (extension/budget-strict-tools [foreign (batch-tool "a" 0)])]

        (expect (nil? (:strict (first tools))))
        (expect (true? (:strict (second tools))))
        (expect (= "mcpish" (:name (first tools))))))
  (it "nested optional properties are counted, exactly as the provider counts them"
      (let [nested (batch-tool "nested" 0)]
        ;; "path" is required; add one optional INSIDE the array items.
        (expect (= 0 (#'extension/optional-parameter-count (:schema nested))))
        (expect (= 1
                   (#'extension/optional-parameter-count
                    (assoc-in (:schema nested)
                      [:properties "edits" :items :properties "ranges"]
                      {:type "string"})))))))
