(ns com.blockether.vis.loop.runtime.prompt-activation-test
  "Tests for the data-driven tool-prompt pipeline.

   The system prompt itself carries NO tool-specific copy — tools contribute
   their own `:prompt` via the registry, and `render-active-tools` filters
   by per-tool `:activation-fn`. These tests pin down that invariant: a
   fresh env with no tools produces no <tools> block; a tool with an
   activation-fn returning false is elided; a tool with :prompt gets its
   text injected when active."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.prompt :as prompt]))

(defn- build
  "Invoke build-system-prompt. `opts` may carry :env / :tool-defs to
   activate the data-driven <tools> block; missing → no tools rendered."
  [opts]
  (prompt/build-system-prompt
    (merge
      {:has-reasoning? false
       :has-documents? false
       :env nil
       :tool-defs nil
       :custom-docs nil
       :skill-registry nil}
      opts)))

(defn- tool
  "Build a minimal canonical tool-def map for tests. String :prompt gets
   wrapped into `(constantly s)` to match the normalizer's output."
  [{:keys [sym group prompt activation-fn arglists examples]
    :or {activation-fn (constantly true)
         arglists '([])}}]
  (cond-> {:sym sym
           :group (or group "misc")
           :arglists arglists
           :activation-fn activation-fn}
    prompt           (assoc :prompt (if (fn? prompt) prompt (constantly prompt)))
    (seq examples)   (assoc :examples examples)))

(defdescribe tools-block-absent-without-env
  (describe "fresh build with no env or tool-defs"
    (it "omits the data-driven <tools> wrapper entirely"
      ;; The opening `\n<tools>\n` sequence only appears as the block
      ;; delimiter — never in narrative text. ARCH/GROUNDING mention the
      ;; word `<tools>` inline, so we match on the surrounding newlines.
      (let [p (build {})]
        (expect (not (str/includes? p "\n<tools>\n")))
        (expect (not (str/includes? p "<tool name=")))
        (expect (not (str/includes? p "<group name=")))))

    (it "still carries the fixed agent-contract blocks"
      (let [p (build {})]
        (expect (str/includes? p "MINDSET:"))
        (expect (str/includes? p "CONTEXT MODEL"))
        (expect (str/includes? p "ARCH:"))
        (expect (str/includes? p "CLJ:"))))))

(defdescribe tools-block-filters-by-activation
  (describe "<tools> rendering"
    (it "renders a tool whose activation-fn returns truthy"
      (let [p (build {:env {}
                      :tool-defs [(tool {:sym 'read-file :group "filesystem"
                                         :prompt "Read a file from disk."
                                         :examples ["(read-file \"/tmp/x\")"]})]})]
        (expect (str/includes? p "\n<tools>\n"))
        (expect (str/includes? p "<tool name=\"read-file\""))
        (expect (str/includes? p "Read a file from disk."))
        (expect (str/includes? p "(read-file \"/tmp/x\")"))))

    (it "elides a tool whose activation-fn returns falsy"
      (let [p (build {:env {}
                      :tool-defs [(tool {:sym 'concept-info :group "concepts"
                                         :activation-fn (constantly false)
                                         :prompt "Inspect a concept."})]})]
        (expect (not (str/includes? p "concept-info")))
        (expect (not (str/includes? p "Inspect a concept.")))))

    (it "passes the env to activation-fn so tools can gate on DB state"
      (let [active-env   {:has-docs? true}
            inactive-env {:has-docs? false}
            doc-tool     (tool {:sym 'search-documents :group "documents"
                                :activation-fn (fn [e] (:has-docs? e))
                                :prompt "Full-text search."})
            p-on  (build {:env active-env   :tool-defs [doc-tool]})
            p-off (build {:env inactive-env :tool-defs [doc-tool]})]
        (expect (str/includes? p-on "search-documents"))
        (expect (not (str/includes? p-off "search-documents")))))

    (it "passes the env to :prompt so tool copy can vary per env"
      (let [p (build {:env {:repo-name "myrepo"}
                      :tool-defs [(tool {:sym 'git-blame :group "git"
                                         :prompt (fn [env]
                                                   (str "Blame a file in " (:repo-name env) "."))})]})]
        (expect (str/includes? p "Blame a file in myrepo."))))

    (it "groups tools by :group alphabetically"
      (let [p (build {:env {}
                      :tool-defs [(tool {:sym 'read-file :group "filesystem" :prompt "R"})
                                  (tool {:sym 'var-history :group "conversation" :prompt "V"})
                                  (tool {:sym 'git-blame :group "git" :prompt "G"})]})
            conv-idx (.indexOf p "group name=\"conversation\"")
            fs-idx   (.indexOf p "group name=\"filesystem\"")
            git-idx  (.indexOf p "group name=\"git\"")]
        (expect (pos? conv-idx))
        (expect (pos? fs-idx))
        (expect (pos? git-idx))
        (expect (< conv-idx fs-idx git-idx))))))

(defdescribe ambient-tools-always-render
  (describe "AMBIENT_TOOL_DEFS"
    (it "renders sub-rlm-query even with no registered tools"
      (let [p (build {:env {} :tool-defs []})]
        (expect (str/includes? p "<tool name=\"sub-rlm-query\""))
        ;; Wording was tightened for token budget — the phrase "sub-query"
        ;; still appears in the rewritten prompt.
        (expect (str/includes? p "sub-query"))))

    (it "renders request-more-iterations with its prompt"
      (let [p (build {:env {} :tool-defs []})]
        (expect (str/includes? p "<tool name=\"request-more-iterations\""))
        ;; Prompt wording was compacted; anchor on the key phrase that
        ;; encodes the same behavior contract.
        (expect (str/includes? p "Extend iteration budget by `n`"))))

    (it "registry entries win over ambient entries for the same :sym"
      ;; Prevents a caller from accidentally overriding `sub-rlm-query`
      ;; with a stale ambient entry — the per-query binding is the truth.
      (let [override (tool {:sym 'sub-rlm-query :group "meta"
                            :prompt "OVERRIDDEN"})
            p (build {:env {} :tool-defs [override]})]
        (expect (str/includes? p "OVERRIDDEN"))))))

(defdescribe iteration-spec-follows-has-documents
  (describe "RESPONSE FORMAT :sources field"
    (it "is advertised when has-documents? is true"
      (let [p (build {:has-documents? true})]
        (expect (str/includes? p "sources:"))
        (expect (str/includes? p "IDs of sources"))))

    (it "is omitted when has-documents? is false"
      (let [p (build {:has-documents? false})]
        (expect (not (str/includes? p "sources:")))
        (expect (not (str/includes? p "IDs of sources")))))))

(defdescribe constants-rendered-separately
  (describe "<constants> block"
    (it "renders :type :def entries from :custom-docs"
      (let [p (build {:custom-docs [{:type :def :sym 'MAX
                                     :doc "Upper bound"
                                     :examples ["MAX"]}]})]
        (expect (str/includes? p "<constants>"))
        (expect (str/includes? p "<constant name=\"MAX\""))
        (expect (str/includes? p "Upper bound"))))

    (it "omits :type :fn entries (those flow through <tools>)"
      (let [p (build {:custom-docs [{:type :fn :sym 'legacy-tool
                                     :doc "Should not appear here"}]})]
        (expect (not (str/includes? p "Should not appear here")))
        (expect (not (str/includes? p "<constants>")))))))

(defdescribe environment-block-is-universal
  (describe "<environment> block"
    ;; This block must appear in the prompt for EVERY adapter (web, tui,
    ;; telegram, cli) — the single chokepoint is `build-system-prompt`
    ;; itself. Adapters never concat `<environment>` manually anymore;
    ;; these tests pin that contract down so we never regress into the
    ;; per-adapter copy-paste that shipped earlier.
    (it "renders even with no env, tool-defs, or adapter system-prompt"
      (let [p (build {})]
        (expect (str/includes? p "<environment>"))
        (expect (str/includes? p "</environment>"))
        (expect (str/includes? p "Working directory:"))))

    (it "includes the relative-paths hint for file tools"
      ;; Without this sentence the model defaults to absolute paths
      ;; because tool examples historically used /path/to/... templates.
      (let [p (build {})]
        (expect (str/includes? p "Prefer RELATIVE paths"))
        (expect (str/includes? p "user.dir"))))

    (it "reports the JVM's actual working directory"
      (let [p   (build {})
            cwd (System/getProperty "user.dir")]
        (expect (str/includes? p (str "Working directory: " cwd)))))

    (it "renders exactly once (no adapter-side duplication)"
      (let [p     (build {:system-prompt "Adapter persona."})
            opens (count (re-seq #"<environment>" p))]
        ;; If an adapter starts concatenating its own copy again, this
        ;; guard trips instead of silently doubling tokens.
        (expect (= 1 opens))))

    (it "sits before the INSTRUCTIONS block so adapter prose can reference CWD"
      ;; `build-system-prompt` places `<environment>` ahead of
      ;; INSTRUCTIONS on purpose: adapter instructions can mention
      ;; relative paths knowing the model already saw the CWD.
      (let [p       (build {:system-prompt "Adapter persona."})
            env-idx (.indexOf p "<environment>")
            ins-idx (.indexOf p "INSTRUCTIONS:")]
        (expect (pos? env-idx))
        (expect (pos? ins-idx))
        (expect (< env-idx ins-idx))))))

(defdescribe environment-block-direct
  (describe "prompt/environment-block"
    ;; The public helper exists so tests and future callers can assert on
    ;; the shape without rebuilding the full system prompt.
    (it "is a standalone string containing the expected field labels"
      (let [b (prompt/environment-block)]
        (expect (string? b))
        (expect (str/includes? b "Working directory:"))
        (expect (str/includes? b "Home directory:"))
        (expect (str/includes? b "User:"))
        (expect (str/includes? b "Platform:"))
        (expect (str/includes? b "Shell:"))
        (expect (str/includes? b "</environment>"))))))
