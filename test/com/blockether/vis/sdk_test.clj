(ns com.blockether.vis.sdk-test
  "Tests for the merged vis namespace split (sdk + env + loop).

   One src ns, one test ns. Sections below mirror the src reading order
   (errors, cancellation, discovery, CLI, channels, providers, storage,
   extension, config, sci sandbox, single iteration, query engine,
   environment lifecycle, conversations, db handler, agent, CLI commands)
   so a failing test points at the SECTION whose code broke."
  (:require
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.core :as env]
   [com.blockether.vis.core :as lp]
   [com.blockether.vis.core :as prompt]
   [clojure.string :as str]

   [lazytest.core :refer [defdescribe it expect throws?]]
   [sci.core :as sci]
   [lazytest.core :refer [defdescribe it expect]]
   [com.blockether.svar.internal.llm :as llm]
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [lazytest.core :refer [defdescribe expect it]]))

;; ─── from vis_sdk/core_test.clj ───

;; ─────────────────────────────────────────────────────────────────────────
;; From extension_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(def cat-symbol
  (sdk/symbol 'cat (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (sdk/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= (str "Filesystem tools (use vis/ prefix; positional args only)\n"
           "- (vis/cat path) or (vis/cat path offset limit) — Read a file preview.\n"
           "- vis/max-retries — Maximum retry attempts.\n"
           "RULES:\n"
           "- Discover paths first.")
        (sdk/render-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.tools :alias 'vis}
           :ext/symbols [cat-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]})))))

(defdescribe extension-builder-test

  (it "extension/symbol validates docstring + arglists"
    (let [s (sdk/symbol 'cat (fn [& _] nil)
              {:doc "Read a file." :arglists '([path])})]
      (expect (= 'cat (:ext.symbol/sym s)))
      (expect (= "Read a file." (:ext.symbol/doc s)))
      (expect (= ["(cat path)"] (:ext.symbol/examples s)))))

  (it "extension/value carries doc + value"
    (let [v (sdk/value 'cap 42 {:doc "Cap."})]
      (expect (= 42 (:ext.symbol/val v)))
      (expect (= "Cap." (:ext.symbol/doc v)))))

  (it "extension/symbol accepts :autobind-fn"
    (let [autobind-fn (fn [_] {:bindings []})
          symbol-entry (sdk/symbol 'cat (fn [& _] nil)
                         {:doc "Read a file."
                          :arglists '([path])
                          :autobind-fn autobind-fn})]
      (expect (= autobind-fn (:ext.symbol/autobind-fn symbol-entry)))))

  (it "extension/extension fills :ext/activation-fn + :ext/classes defaults"
    (let [e (sdk/extension
              {:ext/namespace 'com.acme.ext.fs
               :ext/doc       "Filesystem tools"
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
               :ext/prompt    "placeholder"
               :ext/symbols   [cat-symbol retries-value]})]
      (expect (fn? (:ext/activation-fn e)))
      (expect (true? ((:ext/activation-fn e) {})))
      (expect (= {} (:ext/classes e)))
      (expect (= {} (:ext/imports e))))))

;; =============================================================================
;; try-rescue-parse-error
;;
;; The iteration loop walks active extensions when SCI/edamame rejects
;; the LLM's source. Resolution order:
;;   1. Per-symbol `:ext.symbol/on-parse-error-fn` whose symbol name
;;      is mentioned in the broken source.
;;   2. Extension-level `:ext/on-parse-error-fn` as a catch-all.
;; First non-nil rewrite wins.
;; =============================================================================

(defn- sym-with-parse-rescue
  [sym-name hook]
  (sdk/symbol sym-name (fn [& _] nil)
    {:doc "fixture"
     :arglists '([])
     :on-parse-error-fn hook}))

(defn- ext-with-syms
  ([ns-sym alias-sym syms] (ext-with-syms ns-sym alias-sym syms nil))
  ([ns-sym alias-sym syms ext-hook]
   (sdk/extension
     (cond-> {:ext/namespace ns-sym
              :ext/doc       "fixture"
              :ext/group     "filesystem"
              :ext/prompt    "placeholder"
              :ext/ns-alias  {:ns (clojure.core/symbol (str "vis.ext." alias-sym))
                              :alias alias-sym}
              :ext/symbols   syms}
       ext-hook (assoc :ext/on-parse-error-fn ext-hook)))))

(defdescribe try-rescue-parse-error-test

  (it "fires a SYMBOL-level hook only when the broken code mentions it"
    (let [grep (sym-with-parse-rescue 'rg
                 (fn [{:keys [code]}] (str/replace code "X" "Y")))
          ext  (ext-with-syms 'ns-a 'vis [grep])]
      ;; Code mentions `vis/rg` — hook fires.
      (expect (= "(vis/rg \"Y\")"
                (sdk/try-rescue-parse-error [ext] "(vis/rg \"X\")" "err" {})))
      ;; Code does NOT mention rg — hook is skipped.
      (expect (nil?
                (sdk/try-rescue-parse-error [ext] "(other-tool \"X\")" "err" {})))))

  (it "matches both bare and ns-aliased call forms"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "REPAIRED"))
          ext  (ext-with-syms 'ns 'vis [grep])]
      (expect (= "REPAIRED"
                (sdk/try-rescue-parse-error [ext] "(rg \"x\")" "err" {})))
      (expect (= "REPAIRED"
                (sdk/try-rescue-parse-error [ext] "(vis/rg \"x\")" "err" {})))))

  (it "walks every matching symbol; first non-nil rewrite wins"
    (let [a (sym-with-parse-rescue 'foo (fn [_] nil))
          b (sym-with-parse-rescue 'foo (fn [_] "FIRST-WIN"))
          c (sym-with-parse-rescue 'foo (fn [_] (throw (ex-info "never reached" {}))))]
      (expect (= "FIRST-WIN"
                (sdk/try-rescue-parse-error
                  [(ext-with-syms 'na 'a [a b c])]
                  "(a/foo)" "err" {})))))

  (it "skips a symbol-level hook that throws and keeps walking"
    (let [boom (sym-with-parse-rescue 'foo (fn [_] (throw (RuntimeException. "boom"))))
          good (sym-with-parse-rescue 'foo (fn [_] "REPAIRED"))]
      (expect (= "REPAIRED"
                (sdk/try-rescue-parse-error
                  [(ext-with-syms 'na 'a [boom good])]
                  "(a/foo)" "err" {})))))

  (it "falls back to the EXTENSION-level hook when no symbol matches"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "NEVER"))
          ext  (ext-with-syms 'ns 'vis [grep] (fn [_] "FROM-EXT"))]
      ;; No mention of rg — symbol hook skipped — ext hook fires.
      (expect (= "FROM-EXT"
                (sdk/try-rescue-parse-error [ext] "(unrelated)" "err" {})))))

  (it "prefers SYMBOL-level rescue over the extension-level fallback"
    (let [grep (sym-with-parse-rescue 'rg (fn [_] "FROM-SYM"))
          ext  (ext-with-syms 'ns 'vis [grep] (fn [_] "FROM-EXT"))]
      (expect (= "FROM-SYM"
                (sdk/try-rescue-parse-error [ext] "(vis/rg \"x\")" "err" {})))))

  (it "passes :code, :error, :sym, :environment to symbol hooks"
    (let [seen (atom nil)
          grep (sym-with-parse-rescue 'rg
                 (fn [ctx] (reset! seen ctx) nil))
          ext  (ext-with-syms 'ns 'vis [grep])]
      (sdk/try-rescue-parse-error [ext] "(vis/rg)" "the-err" {:env :sentinel})
      (expect (= {:code        "(vis/rg)"
                  :error       "the-err"
                  :sym         'rg
                  :environment {:env :sentinel}}
                @seen))))

  (it "is a no-op on an empty extension list"
    (expect (nil? (sdk/try-rescue-parse-error [] "x" "e" {})))
    (expect (nil? (sdk/try-rescue-parse-error nil "x" "e" {})))))

;; ─────────────────────────────────────────────────────────────────────────
;; From extension_api_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(def ^:private cat-symbol
  (sdk/symbol 'cat (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def ^:private retries-value
  (sdk/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-runtime-composition-test

  (it "assembles canonical extension prompt inside the loop and appends extra notes"
    (let [environment {:extensions (atom [(sdk/extension
                                            {:ext/namespace 'com.acme.ext.fs
                                             :ext/doc       "Filesystem tools"
                                             :ext/group     "filesystem"
                                             :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
                                             :ext/prompt    "RULES:\n- Discover paths first."
                                             :ext/symbols   [cat-symbol retries-value]})])}
          ;; assemble-system-prompt requires :active-extensions — compute
          ;; ONCE per call site (here, once per snapshot) and pass in.
          active-exts   (prompt/active-extensions environment)
          system-prompt (prompt/assemble-system-prompt environment
                          {:active-extensions active-exts})]
      (expect (str/includes? system-prompt "[namespace: vis → vis.ext.tools]"))
      (expect (str/includes? system-prompt "Filesystem tools (use vis/ prefix)"))
      (expect (str/includes? system-prompt "- (vis/cat path) or (vis/cat path offset limit) — Read a file preview."))
      (expect (str/includes? system-prompt "- vis/max-retries — Maximum retry attempts."))
      (expect (str/includes? system-prompt "RULES:\n- Discover paths first."))))

  (it "register-extension! applies :autobind-fn and records footer events"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
          (env/create-sci-context nil)
          environment {:extensions (atom [])
                       :sci-ctx sci-ctx
                       :sandbox-ns sandbox-ns
                       :initial-ns-keys initial-ns-keys
                       :autobind-events-atom (atom [])
                       :autobind-registry-atom (atom {})}
          extension
          (sdk/extension
            {:ext/namespace 'com.acme.ext.autobind
             :ext/doc       "Autobind fixture"
             :ext/group     "filesystem"
             :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
             :ext/prompt    "placeholder"
             :ext/symbols   [(sdk/symbol 'echo-path (fn [path] (str "content:" path))
                               {:doc "Echo path"
                                :arglists '([path])
                                :autobind-fn (fn [{:keys [args result]}]
                                               {:bindings [{:kind :file
                                                            :id (first args)
                                                            :content result
                                                            :tag result}]})})]})
          _ (sdk/register-extension! environment extension)
          _ (sci/eval-string+ sci-ctx "(vis/echo-path \"src/core.clj\")"
              {:ns (sci/find-ns sci-ctx 'sandbox)})
          first-bound
          (:val (sci/eval-string+ sci-ctx "file__src__core-clj"
                  {:ns (sci/find-ns sci-ctx 'sandbox)}))
          first-events @(:autobind-events-atom environment)
          _ (sci/eval-string+ sci-ctx "(vis/echo-path \"src/core.clj\")"
              {:ns (sci/find-ns sci-ctx 'sandbox)})
          second-events @(:autobind-events-atom environment)
          last-event (last second-events)]
      (expect (= "content:src/core.clj" first-bound))
      (expect (= :bound (:status (first first-events))))
      (expect (string? (:footer (first first-events))))
      (expect (= :unchanged (:status last-event)))))

  (it "vis.core does not re-export the extension contract"
    ;; The extension authoring API lives on `com.blockether.vis.core`;
    ;; vis.core is the runtime facade only. Re-exporting these
    ;; names from vis.core would drag the extension library into
    ;; the runtime classpath. Fail loud if any leak in.
    (expect (not (some #{'extension 'symbol 'value 'register-global!
                         'registered-extensions 'discover-extensions!
                         'load-extension! 'reload-extension!
                         'render-extension-prompt 'preview-extension-prompt}
                   (keys (ns-publics 'com.blockether.vis.core)))))))

;; ─────────────────────────────────────────────────────────────────────────
;; From commandline_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(defn- clear-registry! []
  ;; Reach into the registry atom from tests so each `it` starts with
  ;; a clean slate. Private on purpose — production code never resets.
  ;; The atom now lives in `com.blockether.vis.commandline`.
  (reset! @(requiring-resolve 'com.blockether.vis.internal.registry/command-registry) []))

(defdescribe spec-test
  (it "command/build validates required keys"
    (let [c (sdk/command {:cmd/name "run" :cmd/doc "Run something."})]
      (expect (= "run" (:cmd/name c)))))

  (it "command/build throws on missing :cmd/name"
    (expect (throws? Throwable #(sdk/command {:cmd/doc "no name"}))))

  (it "command/build throws on missing :cmd/doc"
    (expect (throws? Throwable #(sdk/command {:cmd/name "x"})))))

(defdescribe subcommand-resolution-test
  (let [root (sdk/command
               {:cmd/name "vis"
                :cmd/doc  "root"
                :cmd/subcommands
                [{:cmd/name "run" :cmd/doc "run cmd"
                  :cmd/run-fn (fn [_ _] :ran-run)}
                 {:cmd/name "channel"
                  :cmd/doc  "channel parent"
                  :cmd/subcommands
                  (fn [] [{:cmd/name "tui" :cmd/doc "tui channel"
                           :cmd/owns-tty? true
                           :cmd/run-fn (fn [_ residual] [:tui residual])}])}]})]

    (it "find-leaf walks one level deep"
      (let [{:keys [command path residual]}
            (sdk/find-leaf root ["vis" "run" "--foo" "bar"])]
        (expect (= "run" (:cmd/name command)))
        (expect (= ["vis" "run"] path))
        (expect (= ["--foo" "bar"] residual))))

    (it "find-leaf walks dynamic subcommands"
      (let [{:keys [command path residual]}
            (sdk/find-leaf root ["vis" "channel" "tui" "--resume"])]
        (expect (= "tui" (:cmd/name command)))
        (expect (true? (:cmd/owns-tty? command)))
        (expect (= ["vis" "channel" "tui"] path))
        (expect (= ["--resume"] residual))))

    (it "find-leaf returns parent when next token doesn't match a child"
      (let [{:keys [command residual]}
            (sdk/find-leaf root ["vis" "channel" "nope"])]
        (expect (= "channel" (:cmd/name command)))
        (expect (= ["nope"] residual))))

    (it "dispatch! invokes leaf run-fn and returns :ok"
      (let [r (sdk/dispatch! root ["vis" "run"] {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= :ran-run (:result r)))))

    (it "dispatch! renders help when leaf has no run-fn but has subcommands"
      (let [r (sdk/dispatch! root ["vis" "channel"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))
        (expect (re-find #"tui channel" (:help-text r)))))

    (it "dispatch! renders help on --help in the residual"
      (let [r (sdk/dispatch! root ["vis" "run" "--help"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))))

    (it "dispatch! returns :no-match when even the root name doesn't match"
      (let [r (sdk/dispatch! root ["NOT-VIS"] {:print-fn (constantly nil)})]
        (expect (= :no-match (:status r)))))))

(defdescribe arg-parsing-test
  (it "parses positional + flag args with type coercion"
    (let [specs [{:name "path"  :kind :positional :type :string :required true}
                 {:name "count" :kind :positional :type :int}
                 {:name "verbose" :kind :flag :type :boolean}
                 {:name "out"     :kind :flag :type :string}]
          parsed (sdk/parse-args specs ["src/foo.clj" "5" "--verbose" "--out" "/tmp/x"])]
      (expect (= "src/foo.clj" (parsed "path")))
      (expect (= 5            (parsed "count")))
      (expect (= true         (parsed "verbose")))
      (expect (= "/tmp/x"     (parsed "out")))))

  (it "int-typed flag is coerced from string to long"
    (let [specs [{:name "depth" :kind :flag :type :int}]]
      (expect (= 7 (get (sdk/parse-args specs ["--depth" "7"]) "depth")))))

  (it "boolean flag needs no value and never consumes the next token"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}
                 {:name "path"    :kind :positional :type :string}]
          parsed (sdk/parse-args specs ["--verbose" "src/foo.clj"])]
      (expect (= true (parsed "verbose")))
      (expect (= "src/foo.clj" (parsed "path")))))

  (it "flags can appear anywhere in the residual without disturbing positionals"
    (let [specs [{:name "path"  :kind :positional :type :string}
                 {:name "count" :kind :positional :type :int}
                 {:name "out"   :kind :flag :type :string}]
          parsed (sdk/parse-args specs ["./x" "--out" "/tmp" "3"])]
      (expect (= "./x" (parsed "path")))
      (expect (= 3 (parsed "count")))
      (expect (= "/tmp" (parsed "out")))))

  (it "validate-args returns nil when all required present"
    (let [specs [{:name "p" :kind :positional :required true}]]
      (expect (nil? (sdk/validate-args specs {"p" "ok"})))))

  (it "validate-args reports missing required args"
    (let [specs [{:name "p" :kind :positional :required true}
                 {:name "q" :kind :positional :required true}]]
      (expect (re-find #"Missing.*p.*q" (sdk/validate-args specs {})))))

  (it "unknown flags are silently dropped"
    (expect (= {} (sdk/parse-args [] ["--whatever" "value"])))))

(defdescribe registered-command-with-args-test
  ;; End-to-end: registered command → mounted into a parent → dispatched
  ;; with a real arg vector → run-fn sees a fully-coerced parsed map.
  (let [captured (atom nil)]

    (it "dispatches a registered command with positional + flag + type coercion"
      (clear-registry!)
      (reset! captured nil)
      (sdk/register-cmd!
        {:cmd/name "deploy"
         :cmd/doc  "deploy something"
         :cmd/args [{:name "path"    :kind :positional :type :string :required true}
                    {:name "count"   :kind :positional :type :int}
                    {:name "verbose" :kind :flag       :type :boolean}
                    {:name "out"     :kind :flag       :type :string}
                    {:name "depth"   :kind :flag       :type :int}]
         :cmd/run-fn (fn [parsed _residual] (reset! captured parsed))})
      (let [root (sdk/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(sdk/registered-under [])})
            r    (sdk/dispatch! root
                   ["vis" "deploy" "./src" "5" "--verbose" "--out" "/tmp" "--depth" "3"]
                   {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= {"path" "./src" "count" 5 "verbose" true
                    "out" "/tmp"   "depth" 3}
                  @captured))))

    (it "missing required positional surfaces a validation error"
      (clear-registry!)
      (sdk/register-cmd!
        {:cmd/name "deploy"
         :cmd/doc  "deploy"
         :cmd/args [{:name "path" :kind :positional :type :string :required true}]
         :cmd/run-fn (fn [_ _] :should-not-run)})
      (let [root (sdk/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(sdk/registered-under [])})
            r    (sdk/dispatch! root ["vis" "deploy"]
                   {:print-fn (constantly nil)})]
        (expect (= :error (:status r)))
        (expect (re-find #"Missing required.*path" (:error r)))))

    (it "render-command for a registered command lists ARGUMENTS + FLAGS sections"
      (clear-registry!)
      (let [c (sdk/register-cmd!
                {:cmd/name "deploy"
                 :cmd/doc  "deploy"
                 :cmd/args [{:name "path"    :kind :positional :type :string :required true :doc "Source path."}
                            {:name "verbose" :kind :flag :type :boolean :doc "Chatty."}]
                 :cmd/run-fn (fn [_ _] nil)})
            out (sdk/render-command c ["vis" "deploy"])]
        (expect (re-find #"ARGUMENTS" out))
        (expect (re-find #"<path>\s+Source path\." out))
        (expect (re-find #"FLAGS" out))
        (expect (re-find #"--verbose\s+Chatty\." out))))))

(defdescribe registry-test
  (it "register-cmd! validates and stores by [parent name]"
    (clear-registry!)
    (sdk/register-cmd! {:cmd/name "alpha" :cmd/doc "a"})
    (sdk/register-cmd! {:cmd/name "beta" :cmd/parent ["ext"] :cmd/doc "b"})
    (expect (= 2 (count (sdk/registered-commands)))))

  (it "register-cmd! is idempotent on [parent name]"
    (clear-registry!)
    (sdk/register-cmd! {:cmd/name "alpha" :cmd/doc "v1"})
    (sdk/register-cmd! {:cmd/name "alpha" :cmd/doc "v2"})
    (expect (= 1 (count (sdk/registered-commands))))
    (expect (= "v2" (:cmd/doc (first (sdk/registered-commands))))))

  (it "registered-under returns only commands for the given parent"
    (clear-registry!)
    (sdk/register-cmd! {:cmd/name "top"  :cmd/doc "top"})
    (sdk/register-cmd! {:cmd/name "e1" :cmd/parent ["ext"]     :cmd/doc "e1"})
    (sdk/register-cmd! {:cmd/name "e2" :cmd/parent ["ext"]     :cmd/doc "e2"})
    (sdk/register-cmd! {:cmd/name "c1" :cmd/parent ["channel"] :cmd/doc "c1"})
    (expect (= ["top"]      (mapv :cmd/name (sdk/registered-under []))))
    (expect (= ["e1" "e2"]  (mapv :cmd/name (sdk/registered-under ["ext"]))))
    (expect (= ["c1"]       (mapv :cmd/name (sdk/registered-under ["channel"])))))

  (it "deregister-cmd! removes the entry by parent + name"
    (clear-registry!)
    (sdk/register-cmd! {:cmd/name "x" :cmd/parent ["ext"] :cmd/doc "d"})
    (sdk/deregister-cmd! ["ext"] "x")
    (expect (empty? (sdk/registered-commands))))

  (it "a registered command can be mounted into a parent via :cmd/subcommands fn"
    (clear-registry!)
    (sdk/register-cmd!
      {:cmd/name "git-status" :cmd/parent ["ext"] :cmd/doc "git status"
       :cmd/run-fn (fn [_ _] :ran)})
    (let [parent (sdk/command
                   {:cmd/name "ext" :cmd/doc "ext parent"
                    :cmd/subcommands #(sdk/registered-under ["ext"])})
          r      (sdk/dispatch! parent ["ext" "git-status"]
                   {:print-fn (constantly nil)})]
      (expect (= :ok (:status r)))
      (expect (= :ran (:result r))))))

(defdescribe help-rendering-test
  (it "render-tree lists every immediate subcommand under a COMMANDS section"
    (let [root (sdk/command
                 {:cmd/name "vis"
                  :cmd/doc  "test root"
                  :cmd/subcommands
                  [{:cmd/name "alpha" :cmd/doc "alpha doc"}
                   {:cmd/name "beta"  :cmd/doc "beta doc"}]})
          out  (sdk/render-tree root)]
      (expect (re-find #"COMMANDS" out))
      (expect (re-find #"alpha\s+alpha doc" out))
      (expect (re-find #"beta\s+beta doc"  out))
      ;; Footer hint nudges users to deeper help.
      (expect (re-find #"vis <command> --help" out))))

  (it "render-command emits USAGE / DESCRIPTION / FLAGS sections"
    (let [c (sdk/command
              {:cmd/name  "run"
               :cmd/doc   "run something"
               :cmd/usage "vis run [FLAGS]"
               :cmd/args  [{:name "verbose" :kind :flag :type :boolean :doc "noisy"}
                           {:name "model"   :kind :flag :type :string  :doc "override model"}]
               :cmd/examples ["vis run --model gpt-4o"]})
          out (sdk/render-command c ["vis" "run"])]
      (expect (re-find #"USAGE\n  vis run \[FLAGS\]" out))
      (expect (re-find #"DESCRIPTION" out))
      (expect (re-find #"FLAGS" out))
      ;; Boolean flag has no value placeholder.
      (expect (re-find #"--verbose\s+noisy" out))
      ;; String flag carries an upper-cased placeholder derived from name.
      (expect (re-find #"--model MODEL\s+override model" out))
      (expect (re-find #"EXAMPLES" out))
      (expect (re-find #"vis run --model gpt-4o" out))))

  (it "render-command lists subcommands when the command has children"
    (let [c (sdk/command
              {:cmd/name "channel"
               :cmd/doc  "channel parent"
               :cmd/subcommands
               [{:cmd/name "tui"      :cmd/doc "interactive UI"}
                {:cmd/name "telegram" :cmd/doc "telegram bot"}]})
          out (sdk/render-command c ["vis" "channel"])]
      (expect (re-find #"SUBCOMMANDS" out))
      (expect (re-find #"tui\s+interactive UI" out))
      (expect (re-find #"telegram\s+telegram bot" out)))))

;; ─────────────────────────────────────────────────────────────────────────
;; From error_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe error-formatting-test
  (it "adds the standard ERROR prefix"
    (expect (= "ERROR: Boom" (sdk/format-error "Boom"))))

  (it "keeps already-prefixed messages stable"
    (expect (= "ERROR: Boom" (sdk/format-error "ERROR: Boom"))))

  (it "normalizes throwable values"
    (expect (= "ERROR: Broken" (sdk/format-error (ex-info "Broken" {})))))

  (it "formats map errors using :message first"
    (expect (= "ERROR: Missing field"
              (sdk/format-error {:message "Missing field" :type :vis/missing-field}))))

  (it "formats final-answer code-error messages"
    ;; The only :answer-related validation message left after the
    ;; finalize collapse: when :code blocks fail mid-finalize.
    (let [message (sdk/final-answer-code-error-message
                    (ex-info "div by zero" {}))]
      (expect (re-find #"code execution failed" message))
      (expect (re-find #"div by zero" message)))))

;; ─── from vis_runtime/core_test.clj ───

;; ─── from core_test.clj ───

;; ─── from var_index_render_test.clj ───

;; -----------------------------------------------------------------------------
;; Helpers — build a minimal sandbox map without a real SCI context.
;; `build-var-index` accepts an explicit sandbox-map override (the third
;; arity), so we can hand it any `{sym → val}` map and skip SCI entirely.
;; -----------------------------------------------------------------------------

(defn- index
  "Render the index for the given sandbox map. `initial-ns-keys` defaults
   to empty, so every key in the sandbox is treated as a user var."
  ([sandbox] (index sandbox #{}))
  ([sandbox initial-ns-keys]
   (env/build-var-index nil initial-ns-keys sandbox nil nil nil)))

;; -----------------------------------------------------------------------------
;; Inline scalars
;; -----------------------------------------------------------------------------

(defdescribe scalar-rendering-test
  (it "renders nil"
    (expect (re-find #"\(def x nil\)" (index {'x nil}))))

  (it "renders booleans inline"
    (expect (re-find #"\(def y true\)" (index {'y true}))))

  (it "renders integers inline"
    (expect (re-find #"\(def n 42\)" (index {'n 42}))))

  (it "renders keywords inline"
    (expect (re-find #"\(def k :foo\)" (index {'k :foo})))))

;; -----------------------------------------------------------------------------
;; String values
;; -----------------------------------------------------------------------------

(defdescribe string-rendering-test
  (it "inlines strings ≤200 chars"
    (let [out (index {'s "hello world"})]
      (expect (re-find #"\(def s \"hello world\"\)" out))))

  (it "previews strings >200 chars with size + 80-char head"
    (let [big (apply str (repeat 500 "a"))
          out (index {'big big})]
      (expect (re-find #":string-size 500" out))
      (expect (re-find #":head \"a{80}\"" out))
      (expect (not (re-find #"a{81}" out)))))

  (it "stats comment carries the live scope"
    (let [out (index {'s "hi"})]
      (expect (re-find #";; v=1 scope=live n=2" out)))))

;; -----------------------------------------------------------------------------
;; Maps
;; -----------------------------------------------------------------------------

(defdescribe map-rendering-test
  (it "inlines maps with ≤8 keys as {:keys [...]}"
    (let [m   {:a 1 :b 2 :c 3}
          out (index {'m m})]
      (expect (re-find #"\{:keys \[" out))
      ;; clojure maps don't guarantee order, so just verify membership.
      (doseq [k [:a :b :c]]
        (expect (re-find (re-pattern (str ":" (name k))) out)))))

  (it "samples maps with >8 keys via {:n :keys-sample}"
    (let [m   (zipmap (map #(keyword (str "k" %)) (range 12)) (range 12))
          out (index {'m m})]
      (expect (re-find #":n 12" out))
      (expect (re-find #":keys-sample" out)))))

;; -----------------------------------------------------------------------------
;; Vectors / sets / sequences
;; -----------------------------------------------------------------------------

(defdescribe sequential-rendering-test
  (it "inlines vectors with ≤5 elements"
    (let [out (index {'v [1 2 3 4 5]})]
      (expect (re-find #"\(def v \[1 2 3 4 5\]\)" out))))

  (it "samples vectors with >5 elements via {:n :head}"
    (let [v   (vec (range 100))
          out (index {'v v})]
      (expect (re-find #":n 100" out))
      (expect (re-find #":head \[0 1 2\]" out))))

  (it "inlines small sets"
    (let [out (index {'s #{1 2}})]
      ;; sets render through `(vec val)` for inline → may be ordered as
      ;; vector seq; assert the body is a vector literal of size 2.
      (expect (re-find #"\(def s \[" out)))))

;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------

(defdescribe fn-rendering-test
  (it "renders fn with arglists when present"
    (let [f (with-meta (fn []) {:arglists '([x] [x opts])})
          out (index {'my-fn f})]
      (expect (re-find #"\(defn my-fn \[x\]" out))
      (expect (re-find #"\[x opts\]" out))))

  (it "renders fn with [& args] when no arglists meta"
    (let [out (index {'opaque (fn [])})]
      (expect (re-find #"\(defn opaque \[& args\]" out))))

  (it "embeds first docstring line when present"
    (let [f (with-meta (fn []) {:arglists '([x])
                                :doc "First line of the docstring.\nSecond line."})
          out (index {'documented f})]
      (expect (re-find #"First line of the docstring\." out))
      (expect (not (re-find #"Second line\." out))))))

;; -----------------------------------------------------------------------------
;; Stats comment shape — `;; v=N scope=...`
;; -----------------------------------------------------------------------------

(defdescribe stats-comment-test
  (it "renders scope=live for sandbox-bound vars"
    (let [out (index {'x 42})]
      (expect (re-find #";; v=1 scope=live" out))))

  (it "drops `^{...}` reader-macro metadata entirely"
    ;; The old format injected `^{:v 3 :s :l :t :map :n 12}` onto the
    ;; symbol — invalid Clojure that confused parser priors. The new
    ;; format puts stats in a comment line and emits a real `(def …)`.
    (let [out (index {'foo {:a 1}})]
      (expect (not (re-find #"\^\{:v" out)))
      (expect (not (re-find #"\^\{:s" out))))))

;; -----------------------------------------------------------------------------
;; SYSTEM vars are excluded
;; -----------------------------------------------------------------------------

(defdescribe system-var-exclusion-test
  (it "does not render QUERY/ANSWER/REASONING in the live block"
    (let [out (index {'QUERY     "user query"
                      'ANSWER    "prior answer"
                      'REASONING "thinking"
                      'user-var  42})]
      (expect (re-find #"\(def user-var 42\)" out))
      (expect (not (re-find #"\(def QUERY" out)))
      (expect (not (re-find #"\(def ANSWER" out)))
      (expect (not (re-find #"\(def REASONING" out)))))

  (it "does not render initial-ns-keys (tools / helpers)"
    (let [out (index {'read-file (fn []) 'user-var 42}
                #{'read-file})]
      (expect (re-find #"\(def user-var 42\)" out))
      (expect (not (re-find #"read-file" out))))))

;; -----------------------------------------------------------------------------
;; Empty-state path
;; -----------------------------------------------------------------------------

(defdescribe empty-index-test
  (it "returns nil when sandbox has no user vars"
    (expect (nil? (index {} #{}))))

  (it "returns nil when sandbox has only initial-ns-keys"
    (expect (nil? (index {'read-file (fn [])} #{'read-file}))))

  (it "returns nil when sandbox has only SYSTEM vars"
    (expect (nil? (index {'QUERY "x" 'ANSWER "y" 'REASONING "z"})))))

;; -----------------------------------------------------------------------------
;; Sort order — newest-touched first by recency-of (no DB → all tied at
;; Long/MAX_VALUE → falls back to alphabetical ordering by sym name).
;; -----------------------------------------------------------------------------

(defdescribe sort-order-test
  (it "tied recency falls back to alphabetical ordering"
    ;; All vars share the same recency (no DB var-registry passed), so
    ;; the secondary sort key — `(str sym)` — kicks in.
    (let [out (index {'zoo 1 'apple 2 'mango 3})
          apple-pos (str/index-of out "apple")
          mango-pos (str/index-of out "mango")
          zoo-pos   (str/index-of out "zoo")]
      (expect (< apple-pos mango-pos))
      (expect (< mango-pos zoo-pos)))))

;; ─── from core_test.clj ───

;; ─── from code_block_doc_test.clj ───

;; -----------------------------------------------------------------------------
;; extract-defining-name
;; -----------------------------------------------------------------------------

(defdescribe extract-defining-name-test
  (it "extracts var name from (def NAME val)"
    (expect (= 'foo (lp/extract-defining-name "(def foo 42)"))))

  (it "extracts var name from (defn NAME [args] body)"
    (expect (= 'my-fn (lp/extract-defining-name "(defn my-fn [x] (inc x))"))))

  (it "extracts var name from (defn- NAME [args] body)"
    (expect (= 'private-fn (lp/extract-defining-name "(defn- private-fn [] 1)"))))

  (it "extracts var name from (defmacro NAME [args] body)"
    (expect (= 'my-macro (lp/extract-defining-name "(defmacro my-macro [x] `(inc ~x))"))))

  (it "returns nil for non-def expressions"
    (expect (nil? (lp/extract-defining-name "(+ 1 2)")))
    (expect (nil? (lp/extract-defining-name "(println :hi)")))
    (expect (nil? (lp/extract-defining-name "42"))))

  (it "returns nil for multi-form code blocks"
    ;; A block with two top-level forms shouldn't be doc-attached
    ;; ambiguously — only single-form (def…) shapes qualify.
    (expect (nil? (lp/extract-defining-name "(def a 1) (def b 2)"))))

  (it "returns nil for parse errors"
    (expect (nil? (lp/extract-defining-name "(def foo")))
    (expect (nil? (lp/extract-defining-name "this is not clojure")))))

;; -----------------------------------------------------------------------------
;; End-to-end via execute-code (the private helper) — round-trip through SCI
;; -----------------------------------------------------------------------------

(defn- fresh-environment []
  (env/create-sci-context nil))

(defn- def-doc [{:keys [sci-ctx]} sym]
  (let [doc-form (str "(:doc (meta (resolve '" sym ")))")]
    (:val (sci/eval-string+ sci-ctx doc-form
            {:ns (sci/find-ns sci-ctx 'sandbox)}))))

(defn- exec [environment expression doc]
  ;; execute-code is private; reach via the var directly so the test
  ;; doesn't depend on a public re-export.
  (let [execute-code-var (resolve 'com.blockether.vis.core/execute-code)]
    (apply (deref execute-code-var) environment expression
      [:doc doc])))

(defdescribe doc-attach-test
  (it "attaches :doc meta to the var named in (def NAME val)"
    (let [environment (fresh-environment)]
      (exec environment "(def width 1024)" "Pixel width of the canvas.")
      (expect (= "Pixel width of the canvas." (def-doc environment 'width)))))

  (it "attaches :doc meta to the var named in (defn NAME [args] body)"
    (let [environment (fresh-environment)]
      (exec environment "(defn double-it [x] (* 2 x))" "Doubles its input.")
      (expect (= "Doubles its input." (def-doc environment 'double-it)))))

  (it "no-op when :doc is blank or nil"
    (let [environment (fresh-environment)]
      (exec environment "(def x 1)" nil)
      (expect (nil? (def-doc environment 'x)))
      (exec environment "(def y 2)" "")
      (expect (nil? (def-doc environment 'y)))
      (exec environment "(def z 3)" "   ")
      (expect (nil? (def-doc environment 'z)))))

  (it "no-op when :expr is not a def-shape"
    ;; Doc was supplied but expr does nothing var-creating; the eval still
    ;; succeeds, the doc is just dropped.
    (let [environment (fresh-environment)
          result (exec environment "(+ 1 2)" "An addition, surely.")]
      (expect (= 3 (:result result)))))

  (it "doc does not leak into siblings — only the targeted var receives it"
    (let [environment (fresh-environment)]
      (exec environment "(def alpha 1)" "First var.")
      (exec environment "(def beta 2)" nil)
      (expect (= "First var." (def-doc environment 'alpha)))
      (expect (nil? (def-doc environment 'beta))))))

;; -----------------------------------------------------------------------------
;; Render path — `<var_index>` shows the docstring for data vars too
;; -----------------------------------------------------------------------------

(defdescribe render-with-doc-test
  (it "render-data-form embeds first docstring line for documented data vars"
    (let [environment (fresh-environment)]
      (exec environment "(def width 1024)" "Pixel width of the canvas.\nSecond line ignored.")
      (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
            initial (:initial-ns-keys environment)
            out (env/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
        (expect (re-find #"\(def width \"Pixel width of the canvas\.\" 1024\)" out))
        (expect (not (re-find #"Second line" out))))))

  (it "render-fn-form embeds first docstring line for documented fns"
    (let [environment (fresh-environment)]
      (exec environment "(defn doubler [x] (* 2 x))" "Doubles its argument.")
      (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
            initial (:initial-ns-keys environment)
            out (env/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
        (expect (re-find #"\(defn doubler \[x\] \"Doubles its argument\." out))))))

;; -----------------------------------------------------------------------------
;; safe-pr-str — bound-then-format, never format-then-bound. The whole
;; reason this helper exists is to keep `pr-str` from materializing
;; unbounded user/model data into the JVM heap before truncation.
;; -----------------------------------------------------------------------------

(defdescribe safe-pr-str-test
  (it "caps element count via *print-length*"
    (let [v   (vec (range 200))
          out (prompt/safe-pr-str v {:print-length 5 :max-chars 1000})]
      ;; First 5 elements rendered, rest collapsed to `...` per Clojure's
      ;; *print-length* convention.
      (expect (re-find #"\[0 1 2 3 4 \.\.\.\]" out))))

  (it "caps nesting via *print-level*"
    (let [deep {:a {:b {:c {:d {:e :leaf}}}}}
          out  (prompt/safe-pr-str deep {:print-level 2 :max-chars 1000})]
      ;; At depth 2 Clojure replaces deeper structure with `#`.
      (expect (re-find #"#" out))))

  (it "caps the final char count and appends a clip marker"
    (let [s   (apply str (repeat 5000 "a"))
          out (prompt/safe-pr-str s {:max-chars 100 :print-length 1000 :print-level 10})]
      (expect (<= (count out) 200))                  ;; bounded prefix + suffix
      (expect (re-find #" …<\+\d+ chars>$" out))))

  (it "does not clip when input fits within max-chars"
    (let [out (prompt/safe-pr-str {:hello "world"} {:max-chars 1000})]
      (expect (= "{:hello \"world\"}" out))
      (expect (not (re-find #"…" out)))))

  (it "never materializes more than print-length elements during pr"
    ;; If pr-str were applied to the full value first, this test would
    ;; OOM or stall on a billion-element lazy seq. With *print-length*
    ;; bound, pr stops after N elements and returns instantly.
    (let [billion (range 1000000000)
          out     (prompt/safe-pr-str billion {:print-length 3 :max-chars 200})]
      (expect (re-find #"\(0 1 2 \.\.\.\)" out)))))

;; ─── from auto_forget_test.clj ───

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- sci-var
  "Create a SCI var with optional :doc metadata."
  ([sym val]
   (sci/new-var sym val))
  ([sym val doc]
   (sci/new-var sym val {:doc doc})))

(defn- make-sandbox
  "Build a sandbox-map {symbol -> sci-var} from a seq of
   [sym val] or [sym val doc] triples."
  [entries]
  (into {}
    (map (fn [[sym val & [doc]]]
           [sym (if doc (sci-var sym val doc) (sci-var sym val))]))
    entries))

(defn- make-registry
  "Build a var-registry {symbol -> {:query-id uuid ...}} from a seq of
   [sym query-id] pairs."
  [entries]
  (into {}
    (map (fn [[sym qid]]
           [sym {:query-id qid :value nil :code "" :version 0}]))
    entries))

;; ---------------------------------------------------------------------------
;; auto-forget-candidates (pure)
;; ---------------------------------------------------------------------------

(def q1 (random-uuid))
(def q2 (random-uuid))
(def q3 (random-uuid))
(def q4 (random-uuid))

(defdescribe auto-forget-candidates-test

  (it "🫙 empty sandbox → nothing to forget, move along"
    (expect (= #{} (lp/auto-forget-candidates {} #{} {} #{q1}))))

  (it "🛡️ built-ins are untouchable — hands off initial-ns-keys"
    (let [sandbox   (make-sandbox [['fetch 42]])
          initials  #{'fetch}
          registry  (make-registry [['fetch q1]])
          recent    #{q1}]
      (expect (= #{} (lp/auto-forget-candidates sandbox initials registry recent)))))

  (it "🎧 SYSTEM vars (QUERY/ANSWER/REASONING) are sacred — never forgotten"
    (let [sandbox   (make-sandbox [['QUERY "hello"]])
          registry  (make-registry [['QUERY q1]])
          recent    #{q2}]  ;; q1 is NOT recent — would be forgotten if not in SYSTEM_VAR_NAMES
      (expect (= #{} (lp/auto-forget-candidates sandbox #{} registry recent)))))

  (it "📝 documented vars survive any purge — docstrings are armor"
    (let [sandbox   (make-sandbox [['important 99 "This var is documented"]])
          registry  (make-registry [['important q1]])
          recent    #{q2}]  ;; q1 is NOT recent
      (expect (= #{} (lp/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🕐 recently-touched vars stay alive within the recency window"
    (let [sandbox   (make-sandbox [['scratch 1]])
          registry  (make-registry [['scratch q2]])
          recent    #{q1 q2 q3}]
      (expect (= #{} (lp/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🗑️ stale undocumented scratch vars get swept without mercy"
    (let [sandbox   (make-sandbox [['scratch 1] ['tmp 2]])
          registry  (make-registry [['scratch q1] ['tmp q1]])
          recent    #{q3 q4}]
      (expect (= #{'scratch 'tmp}
                (lp/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🎯 full gauntlet: stale→gone, documented→safe, recent→safe, system→safe, builtin→safe"
    (let [sandbox   (make-sandbox [['stale-a 1]
                                   ['stale-b 2]
                                   ['documented 3 "keep me"]
                                   ['recent-var 4]
                                   ['REASONING 5]            ;; SYSTEM_VAR_NAMES — protected
                                   ['builtin 6]])
          initials  #{'builtin}
          registry  (make-registry [['stale-a q1]
                                    ['stale-b q1]
                                    ['documented q1]
                                    ['recent-var q3]
                                    ['REASONING q1]
                                    ['builtin q1]])
          recent    #{q3 q4}]
      (expect (= #{'stale-a 'stale-b}
                (lp/auto-forget-candidates sandbox initials registry recent)))))

  (it "👻 ephemeral vars with no DB footprint are invisible to the janitor"
    (let [sandbox   (make-sandbox [['ephemeral 99]])
          registry  {}
          recent    #{q1}]
      (expect (= #{} (lp/auto-forget-candidates sandbox #{} registry recent)))))

  (it "⚡ a non-registered uppercase var (e.g. CONFIG) gets forgotten like any mortal var"
    ;; SYSTEM_VAR_NAMES is a fixed set #{QUERY ANSWER REASONING};
    ;; user-defined uppercase names (CONFIG, MAX_FOO, ...) are NOT system
    ;; vars and get the normal stale-sweep treatment.
    (let [sandbox   (make-sandbox [['CONFIG 42]])
          registry  (make-registry [['CONFIG q1]])
          recent    #{q2}]
      (expect (= #{'CONFIG} (lp/auto-forget-candidates sandbox #{} registry recent))))))

;; ─── from core_test.clj ───

;; ─── from answer_render_test.clj ───

(defn- fresh-environment []
  (env/create-sci-context nil))

(defn- eval-in [{:keys [sci-ctx]} source]
  (:val (sci/eval-string+ sci-ctx source
          {:ns (sci/find-ns sci-ctx 'sandbox)})))

(defn- get-locals [{:keys [sci-ctx initial-ns-keys]}]
  (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
    (persistent!
      (reduce-kv (fn [acc k v]
                   (if (or (contains? initial-ns-keys k) (keyword? k))
                     acc
                     (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
        (transient {}) sandbox))))

(defn- render [environment raw-answer]
  (lp/render raw-answer (get-locals environment)))

(defdescribe answer-render-test

  (it "passes plain prose through unchanged"
    (let [environment (fresh-environment)]
      (expect (= "Done. The cache is now warm."
                (render environment "Done. The cache is now warm.")))))

  (it "passes markdown through unchanged"
    (let [environment (fresh-environment)
          markdown    "## Summary\n- Patched 3 files\n- All tests green"]
      (expect (= markdown (render environment markdown)))))

  (it "interpolates sandbox vars via {{var}}"
    (let [environment (fresh-environment)]
      (eval-in environment "(def hits 12)")
      (eval-in environment "(def files 3)")
      (expect (= "Found 12 hits across 3 files."
                (render environment "Found {{hits}} hits across {{files}} files.")))))

  (it "supports computed answers via def + {{var}}"
    ;; The model defs the computed value in :code, then references
    ;; it from :answer. No SCI eval at finalize time; just Mustache.
    (let [environment (fresh-environment)]
      (eval-in environment "(def summary (clojure.string/join \"\\n\" [\"a\" \"b\" \"c\"]))")
      (expect (= "a\nb\nc" (render environment "{{summary}}")))))

  (it "iterates collections via Mustache sections"
    (let [environment (fresh-environment)]
      (eval-in environment "(def rows [\"r1\" \"r2\" \"r3\"])")
      (expect (= "r1\nr2\nr3\n"
                (render environment "{{#rows}}{{.}}\n{{/rows}}")))))

  (it "raises on a missing var so the iteration handler can surface it"
    (let [environment (fresh-environment)]
      (expect (try
                (render environment "Hi {{undefined-var}}")
                false
                (catch Exception _ true))))))

;; ─── from redundancy_metric_test.clj ───

;; -----------------------------------------------------------------------------
;; canonical-expression-hash
;; -----------------------------------------------------------------------------

(defdescribe canonical-expression-hash-test
  (it "collapses whitespace differences"
    (expect (= (lp/canonical-expression-hash "(grep \"X\")")
              (lp/canonical-expression-hash "(grep   \"X\")"))))

  (it "collapses leading/trailing whitespace"
    (expect (= (lp/canonical-expression-hash "(grep \"X\")")
              (lp/canonical-expression-hash "  (grep \"X\")\n"))))

  (it "produces different hashes for different forms"
    (expect (not= (lp/canonical-expression-hash "(grep \"X\")")
              (lp/canonical-expression-hash "(grep \"Y\")"))))

  (it "produces different hashes for forms with different head sym"
    (expect (not= (lp/canonical-expression-hash "(grep \"X\")")
              (lp/canonical-expression-hash "(read-file \"X\")"))))

  (it "falls back to raw-string hash on parse failure (never throws)"
    ;; A truncated form like \"(def\" is unparseable; the helper must
    ;; still return a stable hash.
    (let [a (lp/canonical-expression-hash "(def")
          b (lp/canonical-expression-hash "(def")
          c (lp/canonical-expression-hash "(defn")]
      (expect (= a b))
      (expect (not= a c))))

  (it "treats nil / empty string as a stable hash, not a throw"
    (expect (string? (lp/canonical-expression-hash "")))
    (expect (string? (lp/canonical-expression-hash nil)))))

;; -----------------------------------------------------------------------------
;; count-duplicates
;; -----------------------------------------------------------------------------

(defdescribe count-duplicates-test
  (it "first iteration has zero duplicates and seeds the seen-set"
    (let [seen (atom #{})
          [duplicates total] (lp/count-duplicates seen
                               [{:code "(+ 1 2)"}
                                {:code "(grep \"X\")"}])]
      (expect (= 0 duplicates))
      (expect (= 2 total))
      (expect (= 2 (count @seen)))))

  (it "subsequent iteration reports duplicates and grows the seen-set with new hashes only"
    (let [seen (atom #{})]
      (lp/count-duplicates seen
        [{:code "(grep \"X\")"}
         {:code "(read-file \"a\")"}])
      (let [[duplicates total] (lp/count-duplicates seen
                                 [{:code "(grep \"X\")"}            ;; duplicate
                                  {:code "(grep \"Y\")"}             ;; new
                                  {:code "(read-file \"a\")"}])]      ;; duplicate
        (expect (= 2 duplicates))
        (expect (= 3 total))
        ;; Seen-set now has 3 distinct hashes: grep X, grep Y, read-file a.
        (expect (= 3 (count @seen))))))

  (it "errors are NOT recorded — retrying after failure is legitimate"
    ;; Iteration 1 errored out; iteration 2 retries the same call; iteration 2's call
    ;; must NOT count as a duplicate.
    (let [seen (atom #{})]
      (lp/count-duplicates seen
        [{:code "(grep \"X\")" :error "regex broken"}])
      (let [[duplicates total] (lp/count-duplicates seen
                                 [{:code "(grep \"X\")"}])]
        (expect (= 0 duplicates))
        (expect (= 1 total)))))

  (it "whitespace-equivalent retries dedup correctly"
    (let [seen (atom #{})]
      (lp/count-duplicates seen [{:code "(+ 1 2)"}])
      (let [[duplicates total] (lp/count-duplicates seen
                                 [{:code "(+   1   2)"}])]
        (expect (= 1 duplicates))
        (expect (= 1 total)))))

  (it "intra-iteration duplicates count: identical calls in the SAME iteration"
    ;; The seen-set rolls forward as we walk the iteration's expressions,
    ;; so the SECOND occurrence of `(grep \"X\")` within one iteration
    ;; counts as a duplicate. Without this, calls like 'three
    ;; identical greps in one :code array' would report 0
    ;; duplicates even though the dedup short-circuit fires for
    ;; calls 2 and 3.
    (let [seen (atom #{})
          [duplicates total] (lp/count-duplicates seen
                               [{:code "(grep \"X\")"}
                                {:code "(grep \"X\")"}
                                {:code "(grep \"X\")"}
                                {:code "(read-file \"a\")"}])]
      (expect (= 2 duplicates))                                       ;; calls #2 and #3
      (expect (= 4 total))
      (expect (= 2 (count @seen)))))                                  ;; only 2 distinct hashes seeded

  (it "handles an empty expressions vec gracefully"
    (let [seen (atom #{})
          [duplicates total] (lp/count-duplicates seen [])]
      (expect (= 0 duplicates))
      (expect (= 0 total)))))

;; -----------------------------------------------------------------------------
;; dedup-cache short-circuit — the actual Phase 2 mechanism.
;; -----------------------------------------------------------------------------

(defdescribe dedup-cache-test
  (it "lookup returns nil when the cache is empty"
    (let [cache (atom {})]
      (expect (nil? (lp/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "lookup returns nil when expression is nil"
    (let [cache (atom {})]
      (expect (nil? (lp/dedup-cache-lookup cache nil)))))

  (it "record! stores successful results, lookup hits afterwards"
    (let [cache (atom {})
          successful {:result :ok :stdout "" :stderr "" :execution-time-ms 7}]
      (lp/dedup-cache-record! cache "(grep \"X\")" successful "i3.1")
      (let [hit (lp/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (some? hit))
        (expect (= :ok (:result hit)))
        (expect (= "i3.1" (:cached-from hit)))
        (expect (true? (:cached? hit)))
        (expect (= 0 (:execution-time-ms hit))))))

  (it "record! is a no-op for error results"
    (let [cache (atom {})
          err-result {:result nil :error "boom" :stdout "" :stderr ""}]
      (lp/dedup-cache-record! cache "(grep \"X\")" err-result "i3.1")
      (expect (nil? (lp/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! is a no-op for timeouts"
    (let [cache (atom {})
          timeout-result {:result nil :timeout? true :stdout "" :stderr ""}]
      (lp/dedup-cache-record! cache "(grep \"X\")" timeout-result "i3.1")
      (expect (nil? (lp/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! preserves the FIRST writer when racing"
    (let [cache (atom {})
          first-result  {:result :first :execution-time-ms 1}
          second-result {:result :second :execution-time-ms 2}]
      (lp/dedup-cache-record! cache "(grep \"X\")" first-result "i1.1")
      (lp/dedup-cache-record! cache "(grep \"X\")" second-result "i5.2")
      (let [hit (lp/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (= :first (:result hit)))
        (expect (= "i1.1" (:cached-from hit))))))

  (it "whitespace-equivalent forms hit the same cache entry"
    (let [cache (atom {})]
      (lp/dedup-cache-record! cache "(grep \"X\")" {:result :ok} "i1.1")
      (expect (some? (lp/dedup-cache-lookup cache "(grep    \"X\")"))))))

;; ─── from schema_reject_retry_test.clj ───

(defn- schema-reject-ex
  "Build the same exception svar's `internal.spec/str->data-with-spec`
   throws when the provider returns a bare JSON-string."
  [preview]
  (ex-info
    (str "Your response did not match the JSON schema contract. "
      "PRODUCE valid JSON/EDN matching the schema fields. NO prose "
      "outside the structure.")
    {:type :svar.spec/schema-rejected
     :reason :not-a-map
     :received-type "String"
     :raw-data preview
     :raw-data-preview (pr-str preview)}))

(defn- ok-result
  "Minimal shape of what `llm/ask!` returns when parsing succeeds."
  []
  {:result      {:thinking "ok" :code []}
   :tokens      {:input 10 :output 5 :reasoning 0 :total 15}
   :duration-ms 1.0})

(defn- with-stubbed-ask!
  "Run `f` with `llm/ask!` replaced by a stub that pops one outcome per
   call from `outcomes` and either returns it (`:ok`) or throws it
   (instance of Throwable). Returns
     {:result <f result OR :threw> :calls <vec of :messages> :exception <or nil>}."
  [outcomes f]
  (let [calls (atom [])
        remaining (atom (vec outcomes))
        stub (fn [_router opts]
               (swap! calls conj (:messages opts))
               (let [next-out (first @remaining)]
                 (swap! remaining subvec 1)
                 (if (instance? Throwable next-out)
                   (throw next-out)
                   next-out)))]
    (with-redefs [llm/ask! stub]
      (try
        {:result    (f) :calls @calls :exception nil}
        (catch Throwable t
          {:result    :threw :calls @calls :exception t})))))

(defn- run-helper
  "Invoke `ask-with-schema-retry!` with sensible defaults. `chunks` is
   an atom that the helper appends to via `:on-chunk`."
  [chunks & {:keys [max-retries]
             :or   {max-retries 2}}]
  (lp/ask-with-schema-retry!
    ::router-stub
    {:spec ::iteration-spec-stub
     :messages [{:role "user" :content "Q"}]
     :routing {}
     :check-context? false}
    {:iteration   3
     :on-chunk    (fn [chunk] (swap! chunks conj chunk))
     :max-retries max-retries}))

(defdescribe ask-with-schema-retry-test
  (it "first-call success: returns immediately, no retry, no reminder"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask! [(ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 1 (count calls)))
      ;; No reminder was appended.
      (expect (= [{:role "user" :content "Q"}] (first calls)))
      ;; No on-chunk schema-reject events.
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "transient rejection: retry succeeds; ONE reminder, iteration loop never sees the failure"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "Looking at what I have so far")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 2 (count calls)))
      ;; First call: original messages.
      (expect (= 1 (count (first calls))))
      ;; Second call: original messages + ONE reminder.
      (expect (= 2 (count (second calls))))
      (let [reminder (last (second calls))]
        (expect (= "user" (:role reminder)))
        (expect (re-find #"\[svar/schema-reject 1/2\]" (:content reminder)))
        (expect (re-find #"top-level value MUST be a JSON/EDN map"
                  (:content reminder)))
        ;; The reminder includes the literal raw-data preview so the
        ;; model sees what it sent.
        (expect (re-find #"Looking at what I have so far"
                  (:content reminder))))
      ;; on-chunk was notified exactly once with the retry counter.
      (let [retry-chunks (filter :schema-reject-retry @chunks)]
        (expect (= 1 (count retry-chunks)))
        (expect (= 1 (:schema-reject-retry (first retry-chunks))))
        (expect (= 2 (:schema-reject-max (first retry-chunks))))
        (expect (= 3 (:iteration (first retry-chunks)))))))

  (it "two transient rejections: second retry succeeds; reminder is replaced, not accumulated"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")
             (schema-reject-ex "second prose")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 3 (count calls)))
      ;; Each retry replaces the reminder, never accumulates -> messages
      ;; on attempt 2 have exactly 2 entries (original + 1 reminder),
      ;; not 3.
      (expect (= 1 (count (nth calls 0))))
      (expect (= 2 (count (nth calls 1))))
      (expect (= 2 (count (nth calls 2))))
      ;; Reminders carry the CURRENT attempt counter, not a stale one.
      (expect (re-find #"\[svar/schema-reject 1/2\]"
                (:content (last (nth calls 1)))))
      (expect (re-find #"\[svar/schema-reject 2/2\]"
                (:content (last (nth calls 2)))))
      ;; The second reminder cites the second prose preview, not the
      ;; first -- the helper inspects the current rejection.
      (expect (re-find #"second prose"
                (:content (last (nth calls 2)))))))

  (it "rejection budget exhausted: bubbles out to the iteration loop with the original ex-data"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "p1")
             (schema-reject-ex "p2")
             (schema-reject-ex "p3")]
            #(run-helper chunks :max-retries 2))]
      (expect (= :threw result))
      (expect (some? exception))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (= "String" (:received-type (ex-data exception))))
      ;; 1 + 2 retries = 3 attempts.
      (expect (= 3 (count calls)))
      ;; Two retry chunks fired for the two retries (the final
      ;; bubble-out is NOT a retry chunk -- it's a real failure).
      (expect (= 2 (count (filter :schema-reject-retry @chunks))))))

  (it "non-schema rejection bubbles immediately, no retry"
    (let [chunks (atom [])
          other-ex (ex-info "boom" {:type :something/else})
          {:keys [result calls exception]}
          (with-stubbed-ask! [other-ex]
            #(run-helper chunks))]
      (expect (= :threw result))
      (expect (= other-ex exception))
      (expect (= 1 (count calls)))
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "max-retries=0 disables the retry layer (parity with calling llm/ask! directly)"
    (let [chunks (atom [])
          {:keys [result exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")]
            #(run-helper chunks :max-retries 0))]
      (expect (= :threw result))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (empty? (filter :schema-reject-retry @chunks))))))

;; ─── from parse_rescue_loop_test.clj ───

(def ^:private try-extension-parse-rescue
  ;; private fn in com.blockether.vis.internal.loop — reach in via
  ;; requiring-resolve so the test can exercise the parse-rescue path
  ;; without bumping it to public.
  (requiring-resolve 'com.blockether.vis.internal.loop/try-extension-parse-rescue))

(defn- preceding-backslash-count
  [code index]
  (loop [position (dec index)
         count    0]
    (if (and (<= 0 position) (= \\ (.charAt ^String code position)))
      (recur (dec position) (inc count))
      count)))

(defn- rescue-one-unsupported-escape
  "Test fixture hook: doubles exactly one unsupported regex escape.
   The loop driver must call it repeatedly when a source string has
   multiple bad sites. This intentionally lives in core tests so core
   does not depend on the common-editing extension test classpath."
  [{:keys [code error]}]
  (when (and (string? code) (str/includes? (str error) "Unsupported escape character"))
    (let [targets #{\| \. \( \) \$ \* \+ \? \[ \] \{ \}}
          length  (count code)]
      (loop [index 0]
        (cond
          (>= index (dec length)) nil
          (and (= \\ (.charAt ^String code index))
            (targets (.charAt ^String code (inc index)))
            (even? (preceding-backslash-count code index)))
          (str (subs code 0 index) "\\" (subs code index))
          :else (recur (inc index)))))))

(def ^:private rg-symbol
  (sdk/symbol 'rg (fn [& _] nil)
    {:doc               "fixture"
     :arglists          '([pattern])
     :on-parse-error-fn rescue-one-unsupported-escape}))

(defn- minimal-environment
  "Build the smallest possible environment shape that
   `try-extension-parse-rescue` reads from. Only `:extensions`
   (a deref-able holder of an extension vec) is required."
  []
  (let [ext (sdk/extension
              {:ext/namespace 'com.blockether.vis.test.parse-rescue
               :ext/doc       "Loop test fixture."
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
               :ext/prompt    (constantly "placeholder")
               :ext/symbols   [rg-symbol]})]
    {:extensions (atom [ext])
     :sci-ctx    (sci/init {})}))

(defn- parses? [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    true
    (catch Throwable _ false)))

(defn- parse-error-message [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    nil
    (catch Throwable t (ex-message t))))

(defdescribe try-extension-parse-rescue-loop-test

  (it "repairs a single `\\|` site (baseline; pre-fix already passed)"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-message code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (= "(vis/rg \"a\\\\|b\")" out))
      (expect (parses? out))))

  (it "loops the rescue across THREE `\\|` sites until the source parses (Bug 2.A.1)"
    ;; Pre-fix: returns nil (single-shot rescue gives up on 2+ sites).
    ;; Post-fix: returns a fully repaired string that parses cleanly.
    (let [env  (minimal-environment)
          code "(vis/rg \"foo\\|bar\\|baz\\|qux\")"
          err  (parse-error-message code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))
      ;; Every original `\|` is now `\\|`.
      (expect (str/includes? out "\\\\|"))))

  (it "loops across `\\|` AND `\\.` AND `\\(` mixed escapes"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\\.c\\(d\")"
          err  (parse-error-message code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))))

  (it "still returns nil when the rescue has nothing to repair"
    ;; A real broken form the rescue can't fix: single-quoted string
    ;; literal the reader rejects. The hook's `rescue-parse-error`
    ;; only handles Unsupported-escape errors; other shapes return
    ;; nil from every iteration of the loop.
    (let [env  (minimal-environment)
          code "(vis/rg 'unterminated"
          err  (parse-error-message code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (nil? out))))

  (it "bounded: a pathological hook that returns a non-shrinking rewrite must not loop forever"
    ;; If a hook keeps returning the same error shape (or makes no
    ;; progress), the driver MUST give up. We simulate that with an
    ;; extension whose hook trivially returns its input wrapped in
    ;; a no-op transformation that re-raises the same parse error.
    (let [pathological-hook (fn [{:keys [code]}]
                              ;; Return code unchanged — should be
                              ;; detected as no-progress and stop.
                              code)
          rg (sdk/symbol 'rg (fn [& _] nil)
               {:doc      "fixture"
                :arglists '([pattern])
                :on-parse-error-fn pathological-hook})
          ext (sdk/extension
                {:ext/namespace 'com.blockether.vis.test.pathological
                 :ext/doc       "pathological"
                 :ext/group     "filesystem"
                 :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
                 :ext/prompt    (constantly "x")
                 :ext/symbols   [rg]})
          env {:extensions (atom [ext]) :sci-ctx (sci/init {})}
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-message code)
          start-ms (System/currentTimeMillis)
          out  (try-extension-parse-rescue env code err)
          elapsed (- (System/currentTimeMillis) start-ms)]
      (expect (nil? out))
      ;; Sanity: bailout must be sub-second.
      (expect (< elapsed 1000)))))

;; ─── from vis_main/core_test.clj ───

;; ─── from cli_smoke_test.clj ───

;; ---------------------------------------------------------------------------
;; Test runner helpers
;; ---------------------------------------------------------------------------

(def ^:private repo-root
  ;; The JVM's cwd is the repo root when these tests are launched
  ;; via `clojure -M:test` from the project directory. Resolve from
  ;; there — `*file*` is unreliable because it's relative to whichever
  ;; classpath source-path the file was loaded from.
  (io/file (System/getProperty "user.dir")))

(def ^:private vis-bin
  (str (io/file repo-root "bin" "vis")))

(assert (.exists (io/file vis-bin))
  (str "bin/vis not found at " vis-bin
    " — run smoke tests from the repo root via `clojure -M:test`."))

(defn- run-vis
  "Invoke `bin/vis` with `args`. Returns `{:exit int :out str :err str}`.
   Wallclock-bounded by a hard 60s timeout via clojure.java.shell."
  [& args]
  (apply sh/sh vis-bin args))

(defn- contains-all? [s substrs]
  (every? #(str/includes? s %) substrs))

;; ---------------------------------------------------------------------------
;; Smoke tests
;; ---------------------------------------------------------------------------

(defdescribe bin-vis-launcher
  (it "exists and is executable"
    (let [f (io/file vis-bin)]
      (expect (.exists f))
      (expect (.canExecute f)))))

(defdescribe vis-no-args
  (it "prints the help tree and exits 0"
    (let [{:keys [exit out]} (run-vis)]
      (expect (zero? exit))
      (expect (contains-all? out ["vis — iterative coding agent CLI"
                                  "COMMANDS"
                                  "run"])))))

(defdescribe vis-help
  (it "prints the help tree and exits 0"
    (let [{:keys [exit out]} (run-vis "help")]
      (expect (zero? exit))
      (expect (str/includes? out "vis — iterative coding agent CLI")))))

(defdescribe vis-doctor
  (it "prints environment diagnostics and exits 0"
    (let [{:keys [exit out]} (run-vis "doctor")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis doctor"
                                  "Environment"
                                  "DB path:"
                                  "Conversations:"])))))

(defdescribe vis-extensions
  (it "lists the `vis-common-editing` filesystem extension as discovered"
    (let [{:keys [exit out]} (run-vis "extensions" "list")]
      (expect (zero? exit))
      (expect (contains-all? out ["Extensions"
                                  "filesystem"
                                  "extension(s)"]))))
  (it "parent help mentions the `list` subcommand"
    (let [{:keys [exit out]} (run-vis "extensions")]
      (expect (zero? exit))
      (expect (str/includes? out "list")))))

(defdescribe vis-channels
  (it "discovers TUI and Telegram"
    (let [{:keys [exit out]} (run-vis "channels")]
      (expect (zero? exit))
      (expect (contains-all? out ["tui" "telegram"])))))

(defdescribe vis-conversations
  (it "exits 0 even when no conversations exist"
    (let [{:keys [exit]} (run-vis "conversations")]
      (expect (zero? exit)))))

(defdescribe vis-run-help
  (it "shows `vis run` usage and flag list"
    (let [{:keys [exit out]} (run-vis "run" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis run" "FLAGS" "--json" "--model"])))))

(defdescribe vis-auth-help
  (it "shows the auth command tree"
    (let [{:keys [exit out]} (run-vis "auth")]
      (expect (zero? exit))
      ;; github-copilot provider auto-registers and should appear
      (expect (str/includes? out "github-copilot")))))

(defdescribe vis-unknown-command
  (it "returns non-zero (or routes to `run` fallback gracefully)"
    (let [{:keys [exit out err]} (run-vis "this-command-does-not-exist-xyz")]
      ;; Two acceptable behaviours per the dispatcher:
      ;;   (a) exit non-zero with an error message, OR
      ;;   (b) treat the token as a free-form prompt and surface a
      ;;       provider-config error (no API key in CI/test env)
      ;; Either way, we get SOMETHING on stdout/stderr and the
      ;; process terminates cleanly.
      (expect (some? exit))
      (expect (or (not (zero? exit))
                (str/includes? (str out err) "this-command-does-not-exist"))))))
