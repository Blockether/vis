(ns com.blockether.vis-sdk.core-test
  "Tests for the vis-sdk core API. Single test ns matching the single
   src ns (`com.blockether.vis-sdk.core`). Sections below mirror the
   contracts the SDK ships:

     - Extension authoring (symbol/value/extension/render-prompt)
     - Cross-package extension API (active-extensions, assemble-system-prompt)
     - CLI command spec + registry
     - Error formatting"
  (:require
   [clojure.string :as str]
   [com.blockether.vis-runtime.core :as vis]
   [com.blockether.vis-runtime.loop.core :as loop-core]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.core :as environment-core]
   [com.blockether.vis-sdk.core :as sdk]
   [lazytest.core :refer [defdescribe it expect throws?]]
   [sci.core :as sci]))

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
          active-exts   (vis/active-extensions environment)
          system-prompt (vis/assemble-system-prompt environment
                          {:active-extensions active-exts})]
      (expect (str/includes? system-prompt "[namespace: vis → vis.ext.tools]"))
      (expect (str/includes? system-prompt "Filesystem tools (use vis/ prefix)"))
      (expect (str/includes? system-prompt "- (vis/cat path) or (vis/cat path offset limit) — Read a file preview."))
      (expect (str/includes? system-prompt "- vis/max-retries — Maximum retry attempts."))
      (expect (str/includes? system-prompt "RULES:\n- Discover paths first."))))

  (it "register-extension! applies :autobind-fn and records footer events"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
          (environment-core/create-sci-context nil)
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
          _ (loop-core/register-extension! environment extension)
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
    ;; The extension authoring API lives on `com.blockether.vis-sdk.core`;
    ;; vis.core is the runtime facade only. Re-exporting these
    ;; names from vis.core would drag the extension library into
    ;; the runtime classpath. Fail loud if any leak in.
    (expect (not (some #{'extension 'symbol 'value 'register-global!
                         'registered-extensions 'discover-extensions!
                         'load-extension! 'reload-extension!
                         'render-extension-prompt 'preview-extension-prompt}
                   (keys (ns-publics 'com.blockether.vis-runtime.core)))))))

;; ─────────────────────────────────────────────────────────────────────────
;; From commandline_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(defn- clear-registry! []
  ;; Reach into the registry atom from tests so each `it` starts with
  ;; a clean slate. Private on purpose - production code never resets.
  (reset! @#'sdk/command-registry []))

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
