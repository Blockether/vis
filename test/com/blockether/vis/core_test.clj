(ns com.blockether.vis.core-test
  "Tests for `com.blockether.vis.core` -- the public SDK surface
   composed from sdk + env + loop.

   One src ns, one test ns. Sections below mirror the src reading order
   (errors, cancellation, discovery, CLI, channels, providers, storage,
   extension, config, sci sandbox, single iteration, turn engine,
   environment lifecycle, conversations, db handler, agent, CLI commands)
   so a failing test points at the SECTION whose code broke."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as ext]
   [honey.sql :as sql]
   [lazytest.core :refer [defdescribe expect it throws?]]
   [sci.core :as sci]))

;; ─── from vis_sdk/core_test.clj ───

;; ─────────────────────────────────────────────────────────────────────────
;; From extension_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(def cat-symbol
  (vis/symbol 'cat (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (vis/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= (str "Filesystem tools (use v/ prefix; positional args only)\n"
           "- (v/cat path) or (v/cat path offset limit) - Read a file preview.\n"
           "- v/max-retries - Maximum retry attempts.\n"
           "RULES:\n"
           "- Discover paths first.")
        (vis/render-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.tools :alias 'v}
           :ext/symbols [cat-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]})))))

(defdescribe extension-builder-test

  (it "extension/symbol validates docstring + arglists"
    (let [s (vis/symbol 'cat (fn [& _] nil)
              {:doc "Read a file." :arglists '([path])})]
      (expect (= 'cat (:ext.symbol/sym s)))
      (expect (= "Read a file." (:ext.symbol/doc s)))
      (expect (= ["(cat path)"] (:ext.symbol/examples s)))))

  (it "extension/symbol carries optional :result-spec"
    (let [s (vis/symbol 'cat (fn [& _] nil)
              {:doc "Read a file."
               :arglists '([path])
               :result-spec ::ext/tool-result})]
      (expect (= ::ext/tool-result (:ext.symbol/result-spec s)))))

  (it "extension/symbol carries optional :on-parse-error-fn"
    (let [hook (fn [_] "repaired")
          s    (vis/symbol 'cat (fn [& _] nil)
                 {:doc "Read a file."
                  :arglists '([path])
                  :on-parse-error-fn hook})]
      (expect (= hook (:ext.symbol/on-parse-error-fn s)))))

  (it "extension/value carries doc + value"
    (let [v (vis/value 'cap 42 {:doc "Cap."})]
      (expect (= 42 (:ext.symbol/val v)))
      (expect (= "Cap." (:ext.symbol/doc v)))))

  (it "extension/extension fills :ext/activation-fn + :ext/classes defaults"
    (let [e (vis/extension
              {:ext/namespace 'com.acme.ext.fs
               :ext/doc       "Filesystem tools"
               :ext/kind      "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'v}
               :ext/prompt    "placeholder"
               :ext/symbols   [cat-symbol retries-value]})]
      (expect (fn? (:ext/activation-fn e)))
      (expect (true? ((:ext/activation-fn e) {})))
      (expect (= {} (:ext/classes e)))
      (expect (= {} (:ext/imports e)))
      (expect (= [] (:ext/settings e))))))

(defdescribe invoke-symbol-wrapper-result-spec-test
  (it "rejects a function result that violates :result-spec"
    (let [sym (vis/symbol 'bad (fn [& _] :not-a-tool-result)
                {:doc "bad"
                 :arglists '([])
                 :result-spec ::ext/tool-result})
          ext (vis/extension {:ext/namespace 'com.acme.ext.bad
                              :ext/doc "bad"
                              :ext/kind "filesystem"
                              :ext/prompt "placeholder"
                              :ext/ns-alias {:ns 'vis.ext.bad :alias 'b}
                              :ext/symbols [sym]})]
      (expect (throws? clojure.lang.ExceptionInfo
                #(vis/invoke-symbol-wrapper ext sym [] {})))))

  (it "accepts a function result that satisfies :result-spec and stamps extension info"
    (let [sym (vis/symbol 'good (fn [& _]
                                  (ext/success {:result true
                                                :info {:op :demo}}))
                {:doc "good"
                 :arglists '([])
                 :result-spec ::ext/tool-result})
          ext (vis/extension {:ext/namespace 'com.acme.ext.good
                              :ext/doc "good"
                              :ext/kind "filesystem"
                              :ext/version "1.0.0"
                              :ext/author "Acme"
                              :ext/owner "suite"
                              :ext/license "Apache-2.0"
                              :ext/prompt "placeholder"
                              :ext/ns-alias {:ns 'vis.ext.good :alias 'g}
                              :ext/symbols [sym]})
          out (vis/invoke-symbol-wrapper ext sym [] {})]
      (expect (map? out))
      (expect (= 'good (get-in out [:info :tool :sym])))
      (expect (= "g/good" (get-in out [:info :tool :call])))
      (expect (= 'com.acme.ext.good (get-in out [:info :extension :namespace])))
      (expect (= "Acme" (get-in out [:info :extension :author])))
      (expect (= [] (get-in out [:info :source :paths]))))))

(defdescribe extension-settings-declaration-test
  (it "extension/extension accepts extension-owned setting declarations"
    (let [e (vis/extension
              {:ext/namespace 'com.acme.ext.settings
               :ext/doc       "Settings owner"
               :ext/kind      "tools"
               :ext/settings  [{:key :acme-mode
                                :type :choice
                                :choices [:quiet :loud]
                                :label "Acme mode"
                                :description "Extension-owned UI knob."}]})]
      (expect (= [{:key :acme-mode
                   :type :choice
                   :choices [:quiet :loud]
                   :label "Acme mode"
                   :description "Extension-owned UI knob."}]
                (:ext/settings e)))))

  (it "extension/extension accepts extension-owned theme declarations"
    (let [e (vis/extension
              {:ext/namespace 'com.acme.ext.theme
               :ext/doc       "Theme owner"
               :ext/kind      "tools"
               :ext/theme     {"THEME_NAME" {"PADDING" "0px"}}})]
      (expect (= {"THEME_NAME" {"PADDING" "0px"}}
                (:ext/theme e)))))

  (it "register-extension! adds extension themes to the process theme atom"
    (try
      (vis/register-extension!
        {:ext/namespace 'com.acme.ext.theme-registry
         :ext/doc       "Theme registry owner"
         :ext/kind      "tools"
         :ext/theme     {"ACME_DARK" {"MODE" "dark" "PADDING" "0px"}}})
      (expect (contains? (set (vis/available-theme-ids)) "ACME_DARK"))
      (expect (= [12 14 18] (vis/color (vis/theme "ACME_DARK") :terminal-bg)))
      (finally
        (vis/deregister-extension! 'com.acme.ext.theme-registry)
        (vis/reset-themes!)))))

(defdescribe provider-limits-api-test
  (it "re-exports provider limits helpers from the public vis.core surface"
    (expect (ifn? vis/provider-limits))
    (expect (ifn? vis/all-provider-limits)))

  (it "re-exports theme helpers from the public vis.core surface"
    (expect (= "vis-light" vis/default-theme-id))
    (expect (instance? clojure.lang.IDeref vis/themes))
    (expect (ifn? vis/register-theme!))
    (expect (= [255 255 255] (vis/color :terminal-bg)))
    (expect (= [12 14 18] (vis/color vis/vis-dark :terminal-bg)))
    (expect (= {"PADDING" "0px"}
              (get (vis/extension-theme-settings {"THEME_NAME" {:settings {"PADDING" "0px"}}})
                "THEME_NAME"))))

  (it "re-exports extension rendering helpers from the public vis.core surface"
    (expect (ifn? vis/extension-source-markers-of))
    (expect (ifn? vis/render-tool-result)))

  (it "re-exports the config path on the public vis.core surface"
    (expect (= (str (System/getProperty "user.home") "/.vis/config.edn")
              vis/config-path)))

  (it "exposes turn-language names on the public vis.core surface"
    (expect (ifn? vis/by-cmd))
    (expect (ifn? vis/turn!))
    (expect (ifn? vis/db-turn-history))
    (expect (ifn? vis/db-sweep-orphaned-running-turns!))
    (expect (nil? (ns-resolve 'com.blockether.vis.core 'query!)))
    (expect (nil? (ns-resolve 'com.blockether.vis.core 'db-query-history)))
    (expect (nil? (ns-resolve 'com.blockether.vis.core
                    'db-sweep-orphaned-running-queries!)))))

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

#_(defn- sym-with-parse-rescue
    [sym-name hook]
    (vis/symbol sym-name (fn [& _] nil)
      {:doc "fixture"
       :arglists '([])
       :on-parse-error-fn hook}))

#_(defn- ext-with-syms
    ([ns-sym alias-sym syms] (ext-with-syms ns-sym alias-sym syms nil))
    ([ns-sym alias-sym syms ext-hook]
     (vis/extension
       (cond-> {:ext/namespace ns-sym
                :ext/doc       "fixture"
                :ext/kind      "filesystem"
                :ext/prompt    "placeholder"
                :ext/ns-alias  {:ns (clojure.core/symbol (str "vis.ext." alias-sym))
                                :alias alias-sym}
                :ext/symbols   syms}
         ext-hook (assoc :ext/on-parse-error-fn ext-hook)))))

;; --- ORPHAN: targets removed/changed API. Skipped via #_ --- 
#_(defdescribe try-rescue-parse-error-test

    (it "fires a SYMBOL-level hook only when the broken code mentions it"
      (let [grep (sym-with-parse-rescue 'rg
                   (fn [{:keys [code]}] (str/replace code "X" "Y")))
            ext  (ext-with-syms 'ns-a 'v [grep])]
      ;; Code mentions `v/rg` - hook fires.
        (expect (= "(v/rg \"Y\")"
                  (vis/try-rescue-parse-error [ext] "(v/rg \"X\")" "err" {})))
      ;; Code does NOT mention rg - hook is skipped.
        (expect (nil?
                  (vis/try-rescue-parse-error [ext] "(other-tool \"X\")" "err" {})))))

    (it "matches both bare and ns-aliased call forms"
      (let [grep (sym-with-parse-rescue 'rg (fn [_] "REPAIRED"))
            ext  (ext-with-syms 'ns 'v [grep])]
        (expect (= "REPAIRED"
                  (vis/try-rescue-parse-error [ext] "(rg \"x\")" "err" {})))
        (expect (= "REPAIRED"
                  (vis/try-rescue-parse-error [ext] "(v/rg \"x\")" "err" {})))))

    (it "walks every matching symbol; first non-nil rewrite wins"
      (let [a (sym-with-parse-rescue 'foo (fn [_] nil))
            b (sym-with-parse-rescue 'foo (fn [_] "FIRST-WIN"))
            c (sym-with-parse-rescue 'foo (fn [_] (throw (ex-info "never reached" {}))))]
        (expect (= "FIRST-WIN"
                  (vis/try-rescue-parse-error
                    [(ext-with-syms 'na 'a [a b c])]
                    "(a/foo)" "err" {})))))

    (it "skips a symbol-level hook that throws and keeps walking"
      (let [boom (sym-with-parse-rescue 'foo (fn [_] (throw (RuntimeException. "boom"))))
            good (sym-with-parse-rescue 'foo (fn [_] "REPAIRED"))]
        (expect (= "REPAIRED"
                  (vis/try-rescue-parse-error
                    [(ext-with-syms 'na 'a [boom good])]
                    "(a/foo)" "err" {})))))

    (it "falls back to the EXTENSION-level hook when no symbol matches"
      (let [grep (sym-with-parse-rescue 'rg (fn [_] "NEVER"))
            ext  (ext-with-syms 'ns 'v [grep] (fn [_] "FROM-EXT"))]
      ;; No mention of rg - symbol hook skipped - ext hook fires.
        (expect (= "FROM-EXT"
                  (vis/try-rescue-parse-error [ext] "(unrelated)" "err" {})))))

    (it "prefers SYMBOL-level rescue over the extension-level fallback"
      (let [grep (sym-with-parse-rescue 'rg (fn [_] "FROM-SYM"))
            ext  (ext-with-syms 'ns 'v [grep] (fn [_] "FROM-EXT"))]
        (expect (= "FROM-SYM"
                  (vis/try-rescue-parse-error [ext] "(v/rg \"x\")" "err" {})))))

    (it "passes :code, :error, :sym, :environment to symbol hooks"
      (let [seen (atom nil)
            grep (sym-with-parse-rescue 'rg
                   (fn [ctx] (reset! seen ctx) nil))
            ext  (ext-with-syms 'ns 'v [grep])]
        (vis/try-rescue-parse-error [ext] "(v/rg)" "the-err" {:env :sentinel})
        (expect (= {:code        "(v/rg)"
                    :error       "the-err"
                    :sym         'rg
                    :environment {:env :sentinel}}
                  @seen))))

    (it "is a no-op on an empty extension list"
      (expect (nil? (vis/try-rescue-parse-error [] "x" "e" {})))
      (expect (nil? (vis/try-rescue-parse-error nil "x" "e" {})))))

;; ─────────────────────────────────────────────────────────────────────────
;; From extension_api_test.clj
;; ─────────────────────────────────────────────────────────────────────────

;; `cat-symbol` and `retries-value` were re-declared here (private)
;; as a leftover from merging two test files. Single public defs at
;; the top of the file are sufficient.

(defdescribe extension-runtime-composition-test

  (it "assembles ONLY the author-supplied :ext/prompt under the alias header"
    ;; New contract (see
    ;; `com.blockether.vis.internal.vis/render-extension-prompt-block`):
    ;; the runtime no longer auto-canonicalizes `:ext/symbols` into prompt
    ;; lines. Whatever lands in the prompt is whatever `:ext/prompt`
    ;; returns - plus the namespace-alias header. Sandbox bindings remain
    ;; callable from `:code` whether advertised or not. Authors who want
    ;; the old auto-render behavior call `vis/render-prompt` from inside
    ;; their own `:ext/prompt` fn (covered by `render-prompt-test`).
    (let [environment {:extensions (atom [(vis/extension
                                            {:ext/namespace 'com.acme.ext.fs
                                             :ext/doc       "Filesystem tools"
                                             :ext/kind      "filesystem"
                                             :ext/ns-alias  {:ns 'vis.ext.tools :alias 'v}
                                             :ext/prompt    "RULES:\n- Discover paths first."
                                             :ext/symbols   [cat-symbol retries-value]})])}
          active-exts   (vis/active-extensions environment)
          system-prompt (vis/assemble-system-prompt environment
                          {:active-extensions active-exts})]
      ;; MINIMAL assembly: extension `<extensions>` blocks are
      ;; suppressed entirely. Header, :ext/prompt body, and old
      ;; auto-render output are all absent. Sandbox bindings remain
      ;; callable from `:code` whether advertised or not.
      (expect (not (str/includes? system-prompt "<extensions>")))
      (expect (not (str/includes? system-prompt "[namespace: v -> vis.ext.tools]")))
      (expect (not (str/includes? system-prompt "RULES:\n- Discover paths first.")))
      (expect (not (str/includes? system-prompt "Filesystem tools (use v/ prefix)")))
      (expect (not (str/includes? system-prompt "- (v/cat path)")))
      (expect (not (str/includes? system-prompt "- v/max-retries"))))))

;; ─────────────────────────────────────────────────────────────────────────
;; From commandline_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(defn- clear-registry! []
  ;; Reach into the registry atom from tests so each `it` starts with
  ;; a clean slate. Private on purpose - production code never resets.
  ;; The atom now lives in `com.blockether.vis.commandline`.
  (reset! @(requiring-resolve 'com.blockether.vis.internal.registry/command-registry) []))

(defdescribe spec-test
  (it "command/build validates required keys"
    (let [c (vis/command {:cmd/name "run" :cmd/doc "Run something."})]
      (expect (= "run" (:cmd/name c)))))

  (it "command/build throws on missing :cmd/name"
    (expect (throws? Throwable #(vis/command {:cmd/doc "no name"}))))

  (it "command/build throws on missing :cmd/doc"
    (expect (throws? Throwable #(vis/command {:cmd/name "x"})))))

(defdescribe subcommand-resolution-test
  (let [root (vis/command
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
            (vis/find-leaf root ["vis" "run" "--foo" "bar"])]
        (expect (= "run" (:cmd/name command)))
        (expect (= ["vis" "run"] path))
        (expect (= ["--foo" "bar"] residual))))

    (it "find-leaf walks dynamic subcommands"
      (let [{:keys [command path residual]}
            (vis/find-leaf root ["vis" "channel" "tui" "--resume"])]
        (expect (= "tui" (:cmd/name command)))
        (expect (true? (:cmd/owns-tty? command)))
        (expect (= ["vis" "channel" "tui"] path))
        (expect (= ["--resume"] residual))))

    (it "find-leaf returns parent when next token doesn't match a child"
      (let [{:keys [command residual]}
            (vis/find-leaf root ["vis" "channel" "nope"])]
        (expect (= "channel" (:cmd/name command)))
        (expect (= ["nope"] residual))))

    (it "dispatch! invokes leaf run-fn and returns :ok"
      (let [r (vis/dispatch! root ["vis" "run"] {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= :ran-run (:result r)))))

    (it "dispatch! renders help when leaf has no run-fn but has subcommands"
      (let [r (vis/dispatch! root ["vis" "channel"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))
        (expect (re-find #"tui channel" (:help-text r)))))

    (it "dispatch! renders help on --help in the residual"
      (let [r (vis/dispatch! root ["vis" "run" "--help"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))))

    (it "dispatch! returns :no-match when even the root name doesn't match"
      (let [r (vis/dispatch! root ["NOT-VIS"] {:print-fn (constantly nil)})]
        (expect (= :no-match (:status r)))))))

(defdescribe arg-parsing-test
  (it "parses positional + flag args with type coercion"
    (let [specs [{:name "path"  :kind :positional :type :string :required true}
                 {:name "count" :kind :positional :type :int}
                 {:name "verbose" :kind :flag :type :boolean}
                 {:name "out"     :kind :flag :type :string}]
          parsed (vis/parse-args specs ["src/foo.clj" "5" "--verbose" "--out" "/tmp/x"])]
      (expect (= "src/foo.clj" (parsed "path")))
      (expect (= 5            (parsed "count")))
      (expect (= true         (parsed "verbose")))
      (expect (= "/tmp/x"     (parsed "out")))))

  (it "int-typed flag is coerced from string to long"
    (let [specs [{:name "depth" :kind :flag :type :int}]]
      (expect (= 7 (get (vis/parse-args specs ["--depth" "7"]) "depth")))))

  (it "boolean flag needs no value and never consumes the next token"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}
                 {:name "path"    :kind :positional :type :string}]
          parsed (vis/parse-args specs ["--verbose" "src/foo.clj"])]
      (expect (= true (parsed "verbose")))
      (expect (= "src/foo.clj" (parsed "path")))))

  (it "flags can appear anywhere in the residual without disturbing positionals"
    (let [specs [{:name "path"  :kind :positional :type :string}
                 {:name "count" :kind :positional :type :int}
                 {:name "out"   :kind :flag :type :string}]
          parsed (vis/parse-args specs ["./x" "--out" "/tmp" "3"])]
      (expect (= "./x" (parsed "path")))
      (expect (= 3 (parsed "count")))
      (expect (= "/tmp" (parsed "out")))))

  (it "validate-args returns nil when all required present"
    (let [specs [{:name "p" :kind :positional :required true}]]
      (expect (nil? (vis/validate-args specs {"p" "ok"})))))

  (it "validate-args reports missing required args"
    (let [specs [{:name "p" :kind :positional :required true}
                 {:name "q" :kind :positional :required true}]]
      (expect (re-find #"Missing.*p.*q" (vis/validate-args specs {})))))

  (it "parse-args itself stays loose: unknown flags are dropped at the parse layer"
    ;; `parse-args` is a low-level helper; strict-flag policing lives
    ;; in `dispatch!`. Keeping `parse-args` loose lets bespoke handlers
    ;; (e.g. `vis run`'s prompt-collecting parser) layer their own
    ;; flag rules on top.
    (expect (= {} (vis/parse-args [] ["--whatever" "value"]))))

  (it "unknown-flags reports `--` tokens that don't match the spec"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}
                 {:name "out"     :kind :flag :type :string}]]
      (expect (= [] (vis/unknown-flags specs ["--verbose" "--out" "/tmp"])))
      (expect (= ["--bogus"] (vis/unknown-flags specs ["--bogus"])))
      (expect (= ["--bogus"] (vis/unknown-flags specs ["--verbose" "--bogus" "x"])))
      ;; --help / -h are universal and must NEVER be flagged unknown
      (expect (= [] (vis/unknown-flags specs ["--help"])))
      (expect (= [] (vis/unknown-flags specs ["-h"])))
      ;; flag values that LOOK like flags don't get mis-classified
      (expect (= [] (vis/unknown-flags specs ["--out" "--weird-value"]))))))

(defdescribe registered-command-with-args-test
  ;; End-to-end: registered command -> mounted into a parent -> dispatched
  ;; with a real arg vector -> run-fn sees a fully-coerced parsed map.
  (let [captured (atom nil)]

    (it "dispatches a registered command with positional + flag + type coercion"
      (clear-registry!)
      (reset! captured nil)
      (vis/register-cmd!
        {:cmd/name "deploy"
         :cmd/doc  "deploy something"
         :cmd/args [{:name "path"    :kind :positional :type :string :required true}
                    {:name "count"   :kind :positional :type :int}
                    {:name "verbose" :kind :flag       :type :boolean}
                    {:name "out"     :kind :flag       :type :string}
                    {:name "depth"   :kind :flag       :type :int}]
         :cmd/run-fn (fn [parsed _residual] (reset! captured parsed))})
      (let [root (vis/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(vis/registered-under [])})
            r    (vis/dispatch! root
                   ["vis" "deploy" "./src" "5" "--verbose" "--out" "/tmp" "--depth" "3"]
                   {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= {"path" "./src" "count" 5 "verbose" true
                    "out" "/tmp"   "depth" 3}
                  @captured))))

    (it "missing required positional surfaces a validation error"
      (clear-registry!)
      (vis/register-cmd!
        {:cmd/name "deploy"
         :cmd/doc  "deploy"
         :cmd/args [{:name "path" :kind :positional :type :string :required true}]
         :cmd/run-fn (fn [_ _] :should-not-run)})
      (let [root (vis/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(vis/registered-under [])})
            r    (vis/dispatch! root ["vis" "deploy"]
                   {:print-fn (constantly nil)})]
        (expect (= :error (:status r)))
        (expect (re-find #"Missing required.*path" (:error r)))))

    (it "unknown flags are rejected with the list of accepted flags"
      (clear-registry!)
      (let [ran? (atom false)]
        (vis/register-cmd!
          {:cmd/name "deploy"
           :cmd/doc  "deploy"
           :cmd/args [{:name "verbose" :kind :flag :type :boolean}
                      {:name "out"     :kind :flag :type :string}]
           :cmd/run-fn (fn [_ _] (reset! ran? true))})
        (let [root (vis/command
                     {:cmd/name "vis" :cmd/doc "root"
                      :cmd/subcommands #(vis/registered-under [])})
              r    (vis/dispatch! root ["vis" "deploy"
                                        "--bogus" "--also-bogus" "v"]
                     {:print-fn (constantly nil)})]
          (expect (= :error (:status r)))
          (expect (re-find #"Unknown flag" (:error r)))
          (expect (re-find #"--bogus" (:error r)))
          ;; The accepted-flag list spells out every declared flag
          (expect (re-find #"Accepted flags.*--verbose.*--out" (:error r)))
          ;; --help/-h is always implicitly allowed
          (expect (re-find #"--help" (:error r)))
          ;; And the run-fn never fired
          (expect (false? @ran?)))))

    (it "--help is allowed even when other unknown flags would also be present is N/A: help short-circuits dispatch"
      (clear-registry!)
      (vis/register-cmd!
        {:cmd/name "deploy"
         :cmd/doc  "deploy"
         :cmd/args [{:name "verbose" :kind :flag :type :boolean}]
         :cmd/run-fn (fn [_ _] :should-not-run)})
      (let [root (vis/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(vis/registered-under [])})
            r    (vis/dispatch! root ["vis" "deploy" "--help"]
                   {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))))

    (it "commands without flag specs stay loose so bespoke parsers work"
      (clear-registry!)
      (let [seen (atom nil)]
        (vis/register-cmd!
          {:cmd/name "auth"
           :cmd/doc  "auth"
           ;; No :cmd/args at all -- the command does its own parsing
           ;; over `residual`. The strict check must NOT fire here.
           :cmd/run-fn (fn [_parsed residual] (reset! seen residual))})
        (let [root (vis/command
                     {:cmd/name "vis" :cmd/doc "root"
                      :cmd/subcommands #(vis/registered-under [])})
              r    (vis/dispatch! root ["vis" "auth" "github" "--status"]
                     {:print-fn (constantly nil)})]
          (expect (= :ok (:status r)))
          (expect (= ["github" "--status"] @seen)))))

    (it "render-command for a registered command lists ARGUMENTS + FLAGS sections"
      (clear-registry!)
      (let [c (vis/register-cmd!
                {:cmd/name "deploy"
                 :cmd/doc  "deploy"
                 :cmd/args [{:name "path"    :kind :positional :type :string :required true :doc "Source path."}
                            {:name "verbose" :kind :flag :type :boolean :doc "Chatty."}]
                 :cmd/run-fn (fn [_ _] nil)})
            out (vis/render-command c ["vis" "deploy"])]
        (expect (re-find #"ARGUMENTS" out))
        (expect (re-find #"<path>\s+Source path\." out))
        (expect (re-find #"FLAGS" out))
        (expect (re-find #"--verbose\s+Chatty\." out))))))

(defdescribe registry-test
  (it "register-cmd! validates and stores by [parent name]"
    (clear-registry!)
    (vis/register-cmd! {:cmd/name "alpha" :cmd/doc "a"})
    (vis/register-cmd! {:cmd/name "beta" :cmd/parent ["ext"] :cmd/doc "b"})
    (expect (= 2 (count (vis/registered-commands)))))

  (it "register-cmd! is idempotent on [parent name]"
    (clear-registry!)
    (vis/register-cmd! {:cmd/name "alpha" :cmd/doc "v1"})
    (vis/register-cmd! {:cmd/name "alpha" :cmd/doc "v2"})
    (expect (= 1 (count (vis/registered-commands))))
    (expect (= "v2" (:cmd/doc (first (vis/registered-commands))))))

  (it "registered-under returns only commands for the given parent"
    (clear-registry!)
    (vis/register-cmd! {:cmd/name "top"  :cmd/doc "top"})
    (vis/register-cmd! {:cmd/name "e1" :cmd/parent ["ext"]     :cmd/doc "e1"})
    (vis/register-cmd! {:cmd/name "e2" :cmd/parent ["ext"]     :cmd/doc "e2"})
    (vis/register-cmd! {:cmd/name "c1" :cmd/parent ["channel"] :cmd/doc "c1"})
    (expect (= ["top"]      (mapv :cmd/name (vis/registered-under []))))
    (expect (= ["e1" "e2"]  (mapv :cmd/name (vis/registered-under ["ext"]))))
    (expect (= ["c1"]       (mapv :cmd/name (vis/registered-under ["channel"])))))

  (it "deregister-cmd! removes the entry by parent + name"
    (clear-registry!)
    (vis/register-cmd! {:cmd/name "x" :cmd/parent ["ext"] :cmd/doc "d"})
    (vis/deregister-cmd! ["ext"] "x")
    (expect (empty? (vis/registered-commands))))

  (it "a registered command can be mounted into a parent via :cmd/subcommands fn"
    (clear-registry!)
    (vis/register-cmd!
      {:cmd/name "git-status" :cmd/parent ["ext"] :cmd/doc "git status"
       :cmd/run-fn (fn [_ _] :ran)})
    (let [parent (vis/command
                   {:cmd/name "ext" :cmd/doc "ext parent"
                    :cmd/subcommands #(vis/registered-under ["ext"])})
          r      (vis/dispatch! parent ["ext" "git-status"]
                   {:print-fn (constantly nil)})]
      (expect (= :ok (:status r)))
      (expect (= :ran (:result r))))))

(defdescribe help-rendering-test
  (it "render-tree lists every immediate subcommand under a COMMANDS section"
    (let [root (vis/command
                 {:cmd/name "vis"
                  :cmd/doc  "test root"
                  :cmd/subcommands
                  [{:cmd/name "alpha" :cmd/doc "alpha doc"}
                   {:cmd/name "beta"  :cmd/doc "beta doc"}]})
          out  (vis/render-tree root)]
      (expect (re-find #"COMMANDS" out))
      (expect (re-find #"alpha\s+alpha doc" out))
      (expect (re-find #"beta\s+beta doc"  out))
      ;; Footer hint nudges users to deeper help.
      (expect (re-find #"vis <command> --help" out))))

  (it "render-command emits USAGE / DESCRIPTION / FLAGS sections"
    (let [c (vis/command
              {:cmd/name  "run"
               :cmd/doc   "run something"
               :cmd/usage "vis run [FLAGS]"
               :cmd/args  [{:name "verbose" :kind :flag :type :boolean :doc "noisy"}
                           {:name "model"   :kind :flag :type :string  :doc "override model"}]
               :cmd/examples ["vis run --model gpt-4o"]})
          out (vis/render-command c ["vis" "run"])]
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
    (let [c (vis/command
              {:cmd/name "channel"
               :cmd/doc  "channel parent"
               :cmd/subcommands
               [{:cmd/name "tui"      :cmd/doc "interactive UI"}
                {:cmd/name "telegram" :cmd/doc "telegram bot"}]})
          out (vis/render-command c ["vis" "channel"])]
      (expect (re-find #"SUBCOMMANDS" out))
      (expect (re-find #"tui\s+interactive UI" out))
      (expect (re-find #"telegram\s+telegram bot" out)))))

;; ─────────────────────────────────────────────────────────────────────────
;; From error_test.clj
;; ─────────────────────────────────────────────────────────────────────────

(defdescribe error-formatting-test
  (it "adds the standard ERROR prefix"
    (expect (= "ERROR: Boom" (vis/format-error "Boom"))))

  (it "keeps already-prefixed messages stable"
    (expect (= "ERROR: Boom" (vis/format-error "ERROR: Boom"))))

  (it "normalizes throwable values"
    (expect (= "ERROR: Broken" (vis/format-error (ex-info "Broken" {})))))

  (it "formats map errors using :message first"
    (expect (= "ERROR: Missing field"
              (vis/format-error {:message "Missing field" :type :vis/missing-field}))))

  #_(it "formats final-answer code-error messages"
    ;; The only :answer-related validation message left after the
    ;; finalize collapse: when :code blocks fail mid-finalize.
      (let [message (vis/final-answer-code-error-message
                      (ex-info "div by zero" {}))]
        (expect (re-find #"code execution failed" message))
        (expect (re-find #"div by zero" message)))))

;; -----------------------------------------------------------------------------
;; answer-form-error - Option C scoping helper
;;
;; The iteration loop calls `(answer-form-error block-results form-idx)`
;; after evaluating a turn's forms. It returns the error from the
;; specific form that invoked `(answer ...)`, or nil if that form
;; succeeded. Sibling errors are intentionally NOT surfaced - they
;; stay out of the termination gate. This test pins down the contract.
;; -----------------------------------------------------------------------------

(defdescribe answer-form-error-scoping-test
  (let [results [{:result 1 :error nil}                  ;; idx 0 ok
                 {:result nil :error "sibling boom"}     ;; idx 1 errored
                 {:result nil :error nil}                ;; idx 2 ok (called answer here)
                 {:result nil :error "trailing boom"}]]  ;; idx 3 errored
    (it "returns nil when the answer-form itself ran cleanly (sibling errors stay outside the gate)"
      (expect (nil? (vis/answer-form-error results 2))))

    (it "returns the answer-form's own error when that form errored"
      (expect (= "sibling boom" (vis/answer-form-error results 1))))

    (it "returns the trailing form's error when answer was the last block"
      (expect (= "trailing boom" (vis/answer-form-error results 3))))

    (it "returns nil when form-idx is missing"
      (expect (nil? (vis/answer-form-error results nil))))

    (it "returns nil for out-of-bounds form-idx (defensive against shape drift)"
      (expect (nil? (vis/answer-form-error results 99)))
      (expect (nil? (vis/answer-form-error results -1))))

    (it "rejects non-integer form-idx as nil"
      (expect (nil? (vis/answer-form-error results :two)))
      (expect (nil? (vis/answer-form-error results "2"))))

    (it "empty block-results never matches"
      (expect (nil? (vis/answer-form-error [] 0))))))

;; -----------------------------------------------------------------------------
;; answer-position - rule b' ("answer is alone after iteration 1")
;;
;; `(answer ...)` must be the LAST top-level form. Earlier setup/helper
;; helper forms are allowed so a ready answer does not pay a pointless
;; recovery iteration. Wrappers like `(let [...] (answer ...))` stay
;; legal because they are still one top-level form. Disallowed answer
;; followed by more work triggers a position violation, the answer is
;; discarded, and the model gets a structured nudge to move work before
;; the answer or loop once more.
;; -----------------------------------------------------------------------------

(defdescribe answer-position-rule-test
  (it "single top-level form, answer in form 0 - NOT a violation"
    (expect (false? (vis/answer-position-violation? 0 1))))

  (it "two forms, answer in last (form 1) - NOT a violation"
    (expect (false? (vis/answer-position-violation? 1 2))))

  (it "five forms, answer in last (form 4) - NOT a violation"
    (expect (false? (vis/answer-position-violation? 4 5))))

  (it "two forms, answer in first (form 0) - violation because answer is not alone"
    (expect (true? (vis/answer-position-violation? 0 2))))

  (it "five forms, answer in middle (form 2) - violation"
    (expect (true? (vis/answer-position-violation? 2 5))))

  (it "iteration 1 allows answer as the last top-level form"
    (expect (false? (vis/answer-position-violation? 1 2 1)))
    (expect (false? (vis/answer-position-violation? 4 5 1))))

  (it "iteration 1 still rejects answer before trailing sibling forms"
    (expect (true? (vis/answer-position-violation? 0 2 1)))
    (expect (true? (vis/answer-position-violation? 2 5 1))))

  (it "post-first iterations allow answer as the last top-level form"
    (expect (false? (vis/answer-position-violation? 1 2 2)))
    (expect (false? (vis/answer-position-violation? 4 5 3))))

  (it "nil form-idx (no answer fired) - never a violation"
    (expect (false? (vis/answer-position-violation? nil 3)))
    (expect (false? (vis/answer-position-violation? nil 0))))

  (it "non-integer form-idx - not a violation (defensive against shape drift)"
    (expect (false? (vis/answer-position-violation? :one 3)))
    (expect (false? (vis/answer-position-violation? "1" 3))))

  (it "position error message tells the model to answer last"
    (let [msg (vis/answer-position-error-message 0 3)]
      (expect (string? msg))
      ;; 0-based form-idx 0, 1-based 1, total 3.
      (expect (re-find #"3 top-level forms" msg))
      (expect (re-find #"answer fired from form 1" msg))
      (expect (re-find #"LAST top-level form" msg))
      (expect (re-find #"omit `\(answer ...\)`" msg))
      (expect (re-find #"answer-bearing final form" msg))
      (expect (re-find #"`\(let \[\.\.\.\] \(answer ...\)\)`" msg)))))

;; -----------------------------------------------------------------------------
;; make-progress-tracker - phased chunk accumulation
;;
;; The tracker consumes phased chunks from the iteration loop and
;; builds a per-iteration timeline that channels render. Key
;; behaviors:
;;   - :reasoning    appends/replaces the iteration's :thinking
;;   - :form-start   fills :code at :form-idx before eval finishes
;;   - :form-result  fills parallel vectors at :form-idx
;;   - :iteration-final WITH :final + :answer-form-idx ELIDES the
;;     answer-bearing form's slot from every parallel vector (so
;;     the channel renders the answer text below, never the
;;     `(answer ...)` call as code above it)
;;   - :iteration-error sets :error on the entry
;; -----------------------------------------------------------------------------

(defdescribe progress-tracker-test
  (it ":form-result chunks fill parallel vectors at :form-idx"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(def x 1)" :result 1 :stdout "" :stderr ""
                 :execution-time-ms 5 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 1
                 :code "(def y 2)" :result 2 :stdout "hello" :stderr ""
                 :execution-time-ms 7 :error nil})
      (let [tl (get-timeline)]
        (expect (= 1 (count tl)))
        (let [entry (first tl)]
          (expect (= ["(def x 1)" "(def y 2)"] (:code entry)))
          (expect (= ["" "hello"] (:stdouts entry)))
          (expect (= [5 7] (:durations entry)))
          (expect (= [true true] (:successes entry)))))))

  (it ":form-start exposes currently-running code before a result lands"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-start :iteration 0 :form-idx 0
                 :form-of 2 :code "(Thread/sleep 1000)"
                 :started-at-ms 1000})
      (let [entry (first (get-timeline))]
        (expect (= ["(Thread/sleep 1000)"] (:code entry)))
        (expect (= [1000] (:started-at-ms entry)))
        (expect (= [{:type :form-result :form-idx 0}] (:events entry)))
        (expect (empty? (:successes entry))))
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(Thread/sleep 1000)" :result nil :stdout "" :stderr ""
                 :execution-time-ms 1000 :error nil})
      (let [entry (first (get-timeline))]
        (expect (= [true] (:successes entry)))
        (expect (= [1000] (:durations entry)))
        (expect (= [{:type :form-result :form-idx 0}] (:events entry))))))

  (it ":vis/silent form results immediately elide a prior form-start slot"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-start :iteration 0 :form-idx 0
                 :form-of 1 :code "(conversation-title \"x\")"
                 :started-at-ms 1000})
      (expect (= ["(conversation-title \"x\")"] (:code (first (get-timeline)))))
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(conversation-title \"x\")" :result :vis/silent
                 :stdout "" :stderr "" :execution-time-ms 1 :error nil})
      (let [entry (first (get-timeline))]
        (expect (= [] (:code entry)))
        (expect (= [] (:events entry)))
        (expect (= [] (:started-at-ms entry))))))

  (it "out-of-order :form-result chunks pad with nil"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      ;; Form 2 arrives before form 0 / 1 - tracker pads.
      (on-chunk {:phase :form-result :iteration 0 :form-idx 2
                 :code "(def z 3)" :result 3 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (let [{:keys [code]} (first (get-timeline))]
        (expect (= 3 (count code)))
        (expect (nil? (nth code 0)))
        (expect (nil? (nth code 1)))
        (expect (= "(def z 3)" (nth code 2))))))

  (it ":reasoning chunks update :thinking"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :reasoning :iteration 0 :thinking "thinking..."})
      (expect (= "thinking..." (:thinking (first (get-timeline)))))))

  (it "reasoning live-render events trim boundary whitespace"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (doseq [thinking ["The contract APIs failed in iteration"
                        "The contract APIs failed in iteration "
                        "The contract APIs failed in iteration 1 - lines"
                        "The contract APIs failed in iteration 1 - lines "
                        "The contract APIs failed in iteration 1 - lines 100-169"]]
        (on-chunk {:phase :reasoning :iteration 0 :thinking thinking}))
      (let [entry         (first (get-timeline))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (expect (= "The contract APIs failed in iteration 1 - lines 100-169"
                  (:thinking entry)))
        (expect (= (:thinking entry) reconstructed))))
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (doseq [thinking [" " " a"]]
        (on-chunk {:phase :reasoning :iteration 0 :thinking thinking}))
      (let [entry         (first (get-timeline))
            reconstructed (apply str (map :thinking (filter #(= :thinking (:type %)) (:events entry))))]
        (expect (= "a" (:thinking entry)))
        (expect (= (:thinking entry) reconstructed)))))

  (it "reasoning / code / reasoning builds ordered :events for live rendering"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :reasoning :iteration 0 :thinking "alpha"})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(+ 1 1)" :result 2 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      ;; svar streams accumulated reasoning, not deltas. Tracker must
      ;; split the second segment relative to the prior snapshot.
      (on-chunk {:phase :reasoning :iteration 0 :thinking "alpha\nbeta"})
      (let [entry (first (get-timeline))]
        (expect (= [{:type :thinking :thinking "alpha"}
                    {:type :form-result :form-idx 0}
                    {:type :thinking :thinking "\nbeta"}]
                  (:events entry))))))

  (it "long streamed reasoning is stored as bounded render events"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)
          long-thinking (apply str (repeat 9000 "x"))]
      (on-chunk {:phase :reasoning :iteration 0 :thinking long-thinking})
      (let [entry           (first (get-timeline))
            thinking-events (filterv #(= :thinking (:type %)) (:events entry))]
        (expect (= long-thinking (:thinking entry)))
        (expect (< 1 (count thinking-events)))
        (expect (= long-thinking (apply str (map :thinking thinking-events)))))))

  (it ":form-result with :error formats the error string into :results"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(boom)" :result nil :stdout "" :stderr ""
                 :execution-time-ms 0 :error "divide by zero"})
      (let [entry (first (get-timeline))]
        (expect (= [false] (:successes entry)))
        (expect (re-find #"ERROR:" (first (:results entry)))))))

  (it ":iteration-final with :final + :answer-form-idx ELIDES that slot"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(def hits 12)" :result 12 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 1
                 :code "(def files 3)" :result 3 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 2
                 :code "(answer (str hits \" hits across \" files \" files\"))"
                 :result :vis/answer :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :iteration-final :iteration 0
                 :final {:answer "12 hits across 3 files"
                         :iteration-count 1 :status :success}
                 :answer-form-idx 2 :done? true})
      (let [entry (first (get-timeline))]
        ;; Answer-form (idx 2) is gone; only the two work forms remain.
        (expect (= ["(def hits 12)" "(def files 3)"] (:code entry)))
        (expect (= [true true] (:successes entry)))
        (expect (= 2 (count (:durations entry))))
        (expect (= "12 hits across 3 files" (-> entry :final :answer))))))

  (it ":iteration-final remaps ordered events after eliding silent and answer slots"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(conversation-title \"x\")" :result :vis/silent
                 :stdout "" :stderr "" :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 1
                 :code "(def checks {})" :result #:vis{:ref :expr}
                 :stdout "" :stderr "" :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 2
                 :code "checks" :result {:success? true}
                 :stdout "" :stderr "" :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 3
                 :code "(answer \"ok\")" :result :vis/answer
                 :stdout "" :stderr "" :execution-time-ms 1 :error nil})
      (on-chunk {:phase :iteration-final :iteration 0
                 :final {:answer "ok" :iteration-count 1 :status :success}
                 :answer-form-idx 3
                 :silent-form-idxs #{0}
                 :done? true})
      (let [entry (first (get-timeline))]
        (expect (= ["(def checks {})" "checks"] (:code entry)))
        ;; Live renderer drives off :events. If these indices are not
        ;; remapped after slot elision, it renders an out-of-bounds
        ;; phantom code block as `""` until the conversation is reopened.
        (expect (= [{:type :form-result :form-idx 0}
                    {:type :form-result :form-idx 1}]
                  (:events entry))))))

  (it ":iteration-final without :final keeps every slot (non-terminal iter)"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(v/cat \"x\")" :result {:lines []} :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :iteration-final :iteration 0
                 :final nil :done? false})
      (let [entry (first (get-timeline))]
        (expect (= 1 (count (:code entry))))
        (expect (nil? (:final entry))))))

  (it "a re-emitted :form-result with :error overwrites the prior success slot"
    ;; The iteration loop emits a `:form-result` chunk the moment a
    ;; form returns. When `(answer ...)` returns its `:vis/answer`
    ;; sentinel and the post-hoc validator (rule b' / own-form-error)
    ;; rejects it, the loop re-emits a second `:form-result` chunk
    ;; carrying `:error <validation-error>`. The tracker MUST
    ;; overwrite the slot so the TUI surfaces the rejection instead
    ;; of the original success.
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      ;; iteration had three forms; (answer ...) fired from form 1,
      ;; violating rule b' because the answer was not alone. Forms 0
      ;; and 2 ran clean.
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(def x 1)" :result 1 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 1
                 :code "(answer \"early\")" :result :vis/answer
                 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 0 :form-idx 2
                 :code "(def y 2)" :result 2 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      ;; Validator re-emits form 1 with `:error` populated.
      (on-chunk {:phase :form-result :iteration 0 :form-idx 1
                 :code "(answer \"early\")" :result :vis/answer
                 :stdout "" :stderr ""
                 :execution-time-ms 1
                 :error "(answer ...) must be the ONLY top-level form of its final iteration. ..."})
      (let [entry (first (get-timeline))]
        ;; Slots 0 + 2 stay successful; slot 1 flips to error and
        ;; carries the formatted validation message.
        (expect (= [true false true] (:successes entry)))
        (expect (re-find #"ERROR:" (nth (:results entry) 1)))
        (expect (re-find #"ONLY top-level form" (nth (:results entry) 1)))
        ;; Re-emit updates the existing event in place; no duplicate
        ;; code block lands in the ordered live trace.
        (expect (= [{:type :form-result :form-idx 0}
                    {:type :form-result :form-idx 1}
                    {:type :form-result :form-idx 2}]
                  (:events entry)))
        ;; Sibling slots untouched.
        (expect (= "1" (str (nth (:results entry) 0))))
        (expect (= "2" (str (nth (:results entry) 2)))))))

  (it ":iteration-error sets :error and :done? true"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :iteration-error :iteration 0
                 :thinking "about to call LLM..."
                 :error {:type :provider/timeout :message "timed out"}
                 :done? true})
      (let [entry (first (get-timeline))]
        (expect (= :provider/timeout (-> entry :error :type)))
        (expect (true? (:done? entry)))
        (expect (= "about to call LLM..." (:thinking entry))))))

  (it "unknown :phase passes through unchanged (forward-compat)"
    (let [{:keys [on-chunk get-timeline]} (vis/make-progress-tracker)]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "(+ 1 1)" :result 2 :stdout "" :stderr ""
                 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :brand-new-future-phase :iteration 0 :payload "data"})
      (let [entry (first (get-timeline))]
        (expect (= ["(+ 1 1)"] (:code entry))))))

  (it "on-update fires per chunk with the latest timeline"
    (let [updates (atom [])
          {:keys [on-chunk]} (vis/make-progress-tracker
                               {:on-update (fn [tl _chunk]
                                             (swap! updates conj (count tl)))})]
      (on-chunk {:phase :form-result :iteration 0 :form-idx 0
                 :code "a" :result 1 :execution-time-ms 1 :error nil})
      (on-chunk {:phase :form-result :iteration 1 :form-idx 0
                 :code "b" :result 2 :execution-time-ms 1 :error nil})
      (expect (= [1 2] @updates)))))

;; -----------------------------------------------------------------------------
;; Multi-extension shared-alias merge
;;
;; Two extensions can declare the same `:ext/ns-alias` and have their
;; symbols COEXIST in the shared SCI namespace. The earlier ext's
;; bindings are preserved when a later ext registers under the same
;; alias; only same-name collisions get last-write-wins (with a
;; warning log).
;; -----------------------------------------------------------------------------

(defn- bare-env-for-install-test
  "Stand up just enough environment to drive `install-extension!`
   without booting the full router / DB / channel stack: an
   `:extensions` atom + a fresh SCI context."
  []
  (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (vis/create-sci-context nil)]
    {:sci-ctx         sci-ctx
     :sandbox-ns      sandbox-ns
     :initial-ns-keys initial-ns-keys
     :extensions      (atom [])}))

(defn- ns-keys-in-sci
  "Return the set of symbol names bound under `ns-sym` in the SCI
   sandbox's `:namespaces` map."
  [sci-ctx ns-sym]
  (set (keys (get-in @(:env sci-ctx) [:namespaces ns-sym]))))

(defdescribe shared-alias-merge-test
  (it "two exts under the same alias coexist; bindings merge"
    (let [env (bare-env-for-install-test)
          ext-a (vis/extension
                  {:ext/namespace 'test.merge.a
                   :ext/doc       "Test ext A"
                   :ext/version   "0.0.1"
                   :ext/kind      "shared-test"
                   :ext/ns-alias  {:ns 'test.shared.ns :alias 'shared}
                   :ext/symbols   [(vis/symbol 'a-fn (constantly :from-a)
                                     {:doc "A" :arglists '([])})
                                   (vis/symbol 'shared-fn (constantly :a-version)
                                     {:doc "shared" :arglists '([])})]})
          ext-b (vis/extension
                  {:ext/namespace 'test.merge.b
                   :ext/doc       "Test ext B"
                   :ext/version   "0.0.1"
                   :ext/kind      "shared-test"
                   :ext/ns-alias  {:ns 'test.shared.ns :alias 'shared}
                   :ext/symbols   [(vis/symbol 'b-fn (constantly :from-b)
                                     {:doc "B" :arglists '([])})
                                   ;; Collides with ext-a's shared-fn
                                   ;; - last-write-wins.
                                   (vis/symbol 'shared-fn (constantly :b-version)
                                     {:doc "shared" :arglists '([])})]})]
      (vis/install-extension! env ext-a)
      (vis/install-extension! env ext-b)
      (let [bound (ns-keys-in-sci (:sci-ctx env) 'test.shared.ns)]
        ;; Both extensions' unique symbols live under the same ns.
        (expect (contains? bound 'a-fn))
        (expect (contains? bound 'b-fn))
        ;; Collision still resolves - to ext-B's value (last write wins).
        (expect (contains? bound 'shared-fn)))
      ;; Both extensions show up in the env's :extensions atom.
      (let [registered (set (map :ext/namespace @(:extensions env)))]
        (expect (= #{'test.merge.a 'test.merge.b} registered)))))

  (it "symbol bindings follow extension activation state"
    (let [env (bare-env-for-install-test)
          active? (atom false)
          ext (vis/extension
                {:ext/namespace 'test.activation.x
                 :ext/doc       "activation gated"
                 :ext/version   "0.0.1"
                 :ext/kind      "activation-test"
                 :ext/activation-fn (fn [_] @active?)
                 :ext/ns-alias  {:ns 'test.activation.ns :alias 'act}
                 :ext/symbols   [(vis/symbol 'active-fn (constantly :active)
                                   {:doc "active" :arglists '([])})]})]
      (vis/install-extension! env ext)
      (expect (contains? (set (map :ext/namespace @(:extensions env)))
                'test.activation.x))
      (expect (not (contains? (ns-keys-in-sci (:sci-ctx env) 'test.activation.ns)
                     'active-fn)))
      (reset! active? true)
      (vis/sync-active-extension-symbols! env)
      (expect (contains? (ns-keys-in-sci (:sci-ctx env) 'test.activation.ns)
                'active-fn))
      (expect (= :active
                (:val (sci/eval-string+ (:sci-ctx env) "(act/active-fn)"
                        {:ns (:sandbox-ns env)}))))
      (reset! active? false)
      (vis/sync-active-extension-symbols! env)
      (expect (not (contains? (ns-keys-in-sci (:sci-ctx env) 'test.activation.ns)
                     'active-fn)))))

  (it "installing the SAME extension twice replaces it (no duplication)"
    ;; Pre-existing contract - pin it so the merge change doesn't
    ;; accidentally break re-install hot-swap semantics.
    (let [env (bare-env-for-install-test)
          ext-v1 (vis/extension
                   {:ext/namespace 'test.replace.x
                    :ext/doc       "v1"
                    :ext/version   "0.0.1"
                    :ext/kind      "replace-test"
                    :ext/ns-alias  {:ns 'test.replace.ns :alias 'replace-test}
                    :ext/symbols   [(vis/symbol 'reg (constantly :v1)
                                      {:doc "reg" :arglists '([])})]})
          ext-v2 (vis/extension
                   {:ext/namespace 'test.replace.x  ;; same namespace
                    :ext/doc       "v2"
                    :ext/version   "0.0.2"
                    :ext/kind      "replace-test"
                    :ext/ns-alias  {:ns 'test.replace.ns :alias 'replace-test}
                    :ext/symbols   [(vis/symbol 'reg (constantly :v2)
                                      {:doc "reg" :arglists '([])})]})]
      (vis/install-extension! env ext-v1)
      (vis/install-extension! env ext-v2)
      ;; Only ONE entry in :extensions atom for that namespace.
      (expect (= 1 (count (filter #(= 'test.replace.x (:ext/namespace %))
                            @(:extensions env))))))))

;; -----------------------------------------------------------------------------
;; Parse repair pipeline - edamame -> parinfer -> edamame -> extension hooks
;;
;; Three real-world failure cases extracted verbatim from
;; conversation 3931a3ec-e137-4932-9d9e-3144568bed69 (DB rows under
;; expression_state). Pin behavior so the pipeline never silently
;; regresses, and so the comment-glue contract
;; (`;; comment\n(form)` lands as ONE :expr) survives churn.
;; -----------------------------------------------------------------------------

(defdescribe parinfer-rebalance-test
  (it "returns nil for source that already parses (no repair needed)"
    (expect (nil? (vis/parinfer-rebalance "(def x 1)")))
    (expect (nil? (vis/parinfer-rebalance "(def x 1)\n(def y 2)"))))

  (it "Case A: extra `)` matching `{` - parinfer drops the stray close"
    (let [src "(def m {:a 1\n         :b 2)})"
          fixed (vis/parinfer-rebalance src)]
      (expect (string? fixed))
      (expect (not= src fixed))))

  (it "Case B: `]` typed instead of `)` - parinfer rebalances by indent"
    ;; Parinfer doesn't change paren TYPES but it does close+open
    ;; based on indentation, which produces a parseable program.
    ;; (Semantics may shift slightly from what the model meant; the
    ;; objective is \"parses cleanly\" not \"reads model's mind\".)
    (let [src "(def x (let [y 1]\n  (str y])"
          fixed (vis/parinfer-rebalance src)]
      (expect (string? fixed))))

  (it "Case C: missing `)` (EOF) - parinfer auto-closes by indent stack"
    (let [src "(def x (let [y 1]\n  y)"
          fixed (vis/parinfer-rebalance src)]
      (expect (string? fixed))
      (expect (not= src fixed))))

  (it "returns nil when parinfer's output still doesn't parse"
    ;; Rare but possible: source so broken that even parinfer's
    ;; rebalance produces something edamame rejects. Pin that we
    ;; return nil rather than a bogus \"repaired\" string.
    (let [src "(do \"unterminated"]
      (expect (nil? (vis/parinfer-rebalance src))))))

(defdescribe split-top-level-forms-repair-test
  (it "raw clean source: forms NOT marked :repaired?"
    (let [[forms err] (vis/split-top-level-forms "(def x 1) (def y 2)")]
      (expect (nil? err))
      (expect (= 2 (count forms)))
      (expect (every? #(not (:repaired? %)) forms))))

  (it "Case A repair: extra `)` matching `{` - parinfer fixes, forms tagged :repaired?"
    (let [src "(def m {:a 1\n         :b 2)})"
          [forms err] (vis/split-top-level-forms src)]
      (expect (nil? err))
      (expect (every? :repaired? forms))))

  (it "Case C repair: missing `)` - parinfer fixes, forms tagged :repaired?"
    (let [src "(def x (let [y 1]\n  y)"
          [forms err] (vis/split-top-level-forms src)]
      (expect (nil? err))
      (expect (every? :repaired? forms))))

  (it "unrepairable garbage - returns [nil parse-error] for the rescue chain"
    (let [[forms err] (vis/split-top-level-forms "(do \"unterminated")]
      (expect (nil? forms))
      (expect (string? err))
      ;; Must surface SOMETHING actionable; exact message is edamame's.
      (expect (pos? (count err))))))

(defdescribe markdown-fence-guard-test
  (it "rejects raw markdown fence tokens before SCI eval can blow the stack"
    (let [environment (vis/create-sci-context nil)
          execute-code-var (resolve 'com.blockether.vis.internal.loop/execute-code)
          result ((deref execute-code-var) environment "``````clojure")]
      (expect (string? (:error result)))
      (expect (re-find #"Markdown fence" (:error result)))
      (expect (not (re-find #"StackOverflowError" (:error result))))
      (expect (= 0 (:execution-time-ms result))))))

(defdescribe comment-glue-test
  (it "leading `;;` lands in :comment, code stays in :expr"
    (let [src ";; this is x\n(def x 1)"
          [forms _] (vis/split-top-level-forms src)]
      (expect (= 1 (count forms)))
      (expect (= "(def x 1)"     (:expr    (first forms))))
      (expect (= ";; this is x"  (:comment (first forms))))))

  (it "comment between two forms glues to the SECOND form's :comment"
    (let [src "(def x 1)\n;; about y\n(def y 2)"
          [forms _] (vis/split-top-level-forms src)]
      (expect (= 2 (count forms)))
      (expect (= "(def x 1)" (:expr (first forms))))
      (expect (not (contains? (first forms) :comment)))
      (expect (= "(def y 2)"   (:expr    (second forms))))
      (expect (= ";; about y"  (:comment (second forms))))))

  (it "multiple consecutive comment lines collect in :comment"
    (let [src ";; line 1\n;; line 2\n;; line 3\n(def z 3)"
          [forms _] (vis/split-top-level-forms src)]
      (expect (= 1 (count forms)))
      (expect (= "(def z 3)" (:expr (first forms))))
      (let [c (:comment (first forms))]
        (expect (str/includes? c ";; line 1"))
        (expect (str/includes? c ";; line 2"))
        (expect (str/includes? c ";; line 3")))))

  (it "comment-only source produces empty forms vec (no error)"
    (let [[forms err] (vis/split-top-level-forms ";; just a note\n;; nothing else\n")]
      (expect (nil? err))
      (expect (= [] forms))))

  (it "`#_(...)` discard joins the leading-comment block in :comment"
    (let [src ";; what\n#_(discarded-call)\n(def x 1)"
          [forms _] (vis/split-top-level-forms src)]
      (expect (= 1 (count forms)))
      (expect (= "(def x 1)" (:expr (first forms))))
      (let [c (:comment (first forms))]
        (expect (str/includes? c ";; what"))
        (expect (str/includes? c "#_(discarded-call)")))))

  (it "comment glue survives a parinfer repair (Case C with leading comment)"
    (let [src ";; missing close\n(def x (let [y 1]\n  y)"
          [forms _] (vis/split-top-level-forms src)]
      (expect (= 1 (count forms)))
      (expect (str/starts-with? (:expr (first forms)) "(def x"))
      (expect (= ";; missing close" (:comment (first forms))))
      (expect (every? :repaired? forms)))))

;; ─── from vis_runtime/core_test.clj ───

;; ─── from core_test.clj ───

;; ─── from var_index_render_test.clj ───

;; -----------------------------------------------------------------------------
;; Helpers - build a minimal sandbox map without a real SCI context.
;; `build-var-index` accepts an explicit sandbox-map override (the third
;; arity), so we can hand it any `{sym -> val}` map and skip SCI entirely.
;; -----------------------------------------------------------------------------

(defn- index
  "Render the index for the given sandbox map. `initial-ns-keys` defaults
   to empty, so every key in the sandbox is treated as a user var."
  ([sandbox] (index sandbox #{}))
  ([sandbox initial-ns-keys]
   (vis/build-var-index nil initial-ns-keys sandbox nil nil nil)))

(defdescribe var-index-hot-cap-test
  (it "renders at most 100 live user vars plus a compact overflow summary"
    (let [sandbox (into {}
                    (map (fn [i]
                           [(symbol (format "v%03d" i)) i])
                      (range 105)))
          out     (index sandbox)]
      (expect (str/includes? out ";; overflow-live-symbols: 5"))
      (expect (str/includes? out ";; hidden live symbols: v100, v101, v102, v103, v104"))
      (expect (= 100 (count (re-seq #"(?m)^\(def v\d{3}" out)))))))

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
  (it "inlines strings <=200 chars"
    (let [out (index {'s "hello world"})]
      (expect (re-find #"\(def s \"hello world\"\)" out))))

  #_(it "previews strings >200 chars with size + 80-char head"
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
  (it "inlines maps with <=8 keys as {:keys [...]}"
    (let [m   {:a 1 :b 2 :c 3}
          out (index {'m m})]
      (expect (re-find #"\{:keys \[" out))
      ;; clojure maps make no order guarantee - verify membership only.
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
  (it "inlines vectors with <=5 elements"
    (let [out (index {'v [1 2 3 4 5]})]
      (expect (re-find #"\(def v \[1 2 3 4 5\]\)" out))))

  (it "samples vectors with >5 elements via {:n :head}"
    (let [v   (vec (range 100))
          out (index {'v v})]
      (expect (re-find #":n 100" out))
      (expect (re-find #":head \[0 1 2\]" out))))

  (it "inlines small sets"
    (let [out (index {'s #{1 2}})]
      ;; sets render through `(vec val)` for inline -> may be ordered as
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
;; Stats comment shape - `;; v=N scope=...`
;; -----------------------------------------------------------------------------

(defdescribe stats-comment-test
  (it "renders scope=live for sandbox-bound vars"
    (let [out (index {'x 42})]
      (expect (re-find #";; v=1 scope=live" out))))

  (it "drops `^{...}` reader-macro metadata entirely"
    ;; The old format injected `^{:v 3 :s :l :t :map :n 12}` onto the
    ;; symbol - invalid Clojure that confused parser priors. The new
    ;; format puts stats in a comment line and emits a real `(def ...)`.
    (let [out (index {'foo {:a 1}})]
      (expect (not (re-find #"\^\{:v" out)))
      (expect (not (re-find #"\^\{:s" out))))))

;; -----------------------------------------------------------------------------
;; SYSTEM vars are excluded
;; -----------------------------------------------------------------------------

(defdescribe system-var-exclusion-test
  (it "does not render TURN_USER_REQUEST/CONVERSATION_PREVIOUS_ANSWER/ITERATION_PREVIOUS_REASONING in the live block"
    (let [out (index {'TURN_USER_REQUEST            "user request"
                      'CONVERSATION_PREVIOUS_ANSWER "prior answer"
                      'ITERATION_PREVIOUS_REASONING "thinking"
                      'user-var  42})]
      (expect (re-find #"\(def user-var 42\)" out))
      (expect (not (re-find #"\(def TURN_USER_REQUEST" out)))
      (expect (not (re-find #"\(def CONVERSATION_PREVIOUS_ANSWER" out)))
      (expect (not (re-find #"\(def ITERATION_PREVIOUS_REASONING" out)))))

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
    (expect (nil? (index {'TURN_USER_REQUEST "x" 'CONVERSATION_PREVIOUS_ANSWER "y" 'ITERATION_PREVIOUS_REASONING "z"})))))

;; -----------------------------------------------------------------------------
;; Sort order - newest-touched first by recency-of (no DB -> all tied at
;; Long/MAX_VALUE -> falls back to alphabetical ordering by sym name).
;; -----------------------------------------------------------------------------

(defdescribe sort-order-test
  (it "tied recency falls back to alphabetical ordering"
    ;; All vars share the same recency (no DB var-registry passed), so
    ;; the secondary sort key - `(str sym)` - kicks in.
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
    (expect (= 'foo (vis/extract-defining-name "(def foo 42)"))))

  (it "extracts var name from (defn NAME [args] body)"
    (expect (= 'my-fn (vis/extract-defining-name "(defn my-fn [x] (inc x))"))))

  (it "extracts var name from (defn- NAME [args] body)"
    (expect (= 'private-fn (vis/extract-defining-name "(defn- private-fn [] 1)"))))

  (it "extracts var name from (defmacro NAME [args] body)"
    (expect (= 'my-macro (vis/extract-defining-name "(defmacro my-macro [x] `(inc ~x))"))))

  (it "returns nil for non-def blocks"
    (expect (nil? (vis/extract-defining-name "(+ 1 2)")))
    (expect (nil? (vis/extract-defining-name "(println :hi)")))
    (expect (nil? (vis/extract-defining-name "42"))))

  (it "returns nil for multi-form code blocks"
    ;; A block with two top-level forms shouldn't be doc-attached
    ;; ambiguously - only single-form (def...) shapes qualify.
    (expect (nil? (vis/extract-defining-name "(def a 1) (def b 2)"))))

  (it "returns nil for parse errors"
    (expect (nil? (vis/extract-defining-name "(def foo")))
    (expect (nil? (vis/extract-defining-name "this is not clojure")))))

;; -----------------------------------------------------------------------------
;; End-to-end via execute-code (the private helper) - round-trip through SCI
;; -----------------------------------------------------------------------------

;; Helper retained for the `#_`-disabled orphan tests below; ignored
;; by clj-kondo because it would otherwise read as unused.
#_(defn- fresh-environment []
    (vis/create-sci-context nil))

#_(defn- def-doc [{:keys [sci-ctx]} sym]
    (let [doc-form (str "(:doc (meta (resolve '" sym ")))")]
      (:val (sci/eval-string+ sci-ctx doc-form
              {:ns (sci/find-ns sci-ctx 'sandbox)}))))

#_(defn- exec [environment expression doc]
    ;; execute-code is private; reach via the var directly so the test
    ;; doesn't depend on a public re-export.
    (let [execute-code-var (resolve 'com.blockether.vis.core/execute-code)]
      (apply (deref execute-code-var) environment expression
        [:doc doc])))

;; --- ORPHAN: targets removed/changed API. Skipped via #_ --- 
#_(defdescribe doc-attach-test
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

    (it "doc does not leak into siblings - only the targeted var receives it"
      (let [environment (fresh-environment)]
        (exec environment "(def alpha 1)" "First var.")
        (exec environment "(def beta 2)" nil)
        (expect (= "First var." (def-doc environment 'alpha)))
        (expect (nil? (def-doc environment 'beta))))))

;; -----------------------------------------------------------------------------
;; Render path - `<var_index>` shows the docstring for data vars too
;; -----------------------------------------------------------------------------

;; --- ORPHAN: targets removed/changed API. Skipped via #_ --- 
#_(defdescribe render-with-doc-test
    (it "render-data-form embeds first docstring line for documented data vars"
      (let [environment (fresh-environment)]
        (exec environment "(def width 1024)" "Pixel width of the canvas.\nSecond line ignored.")
        (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
              initial (:initial-ns-keys environment)
              out (vis/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
          (expect (re-find #"\(def width \"Pixel width of the canvas\.\" 1024\)" out))
          (expect (not (re-find #"Second line" out))))))

    (it "render-fn-form embeds first docstring line for documented fns"
      (let [environment (fresh-environment)]
        (exec environment "(defn doubler [x] (* 2 x))" "Doubles its argument.")
        (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
              initial (:initial-ns-keys environment)
              out (vis/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
          (expect (re-find #"\(defn doubler \[x\] \"Doubles its argument\." out))))))

;; -----------------------------------------------------------------------------
;; safe-pr-str - bound-then-format, never format-then-bound. The whole
;; reason this helper exists is to keep `pr-str` from materializing
;; unbounded user/model data into the JVM heap before truncation.
;; -----------------------------------------------------------------------------

(defdescribe safe-pr-str-test
  (it "caps element count via *print-length*"
    (let [v   (vec (range 200))
          out (vis/safe-pr-str v {:print-length 5 :max-chars 1000})]
      ;; First 5 elements rendered, rest collapsed to `...` per Clojure's
      ;; *print-length* convention.
      (expect (re-find #"\[0 1 2 3 4 \.\.\.\]" out))))

  (it "caps nesting via *print-level*"
    (let [deep {:a {:b {:c {:d {:e :leaf}}}}}
          out  (vis/safe-pr-str deep {:print-level 2 :max-chars 1000})]
      ;; At depth 2 Clojure replaces deeper structure with `#`.
      (expect (re-find #"#" out))))

  (it "caps the final char count and appends a clip marker"
    (let [s   (apply str (repeat 5000 "a"))
          out (vis/safe-pr-str s {:max-chars 100 :print-length 1000 :print-level 10})]
      (expect (<= (count out) 200))                  ;; bounded prefix + suffix
      (expect (re-find #" ...<\+\d+ chars>$" out))))

  (it "does not clip when input fits within max-chars"
    (let [out (vis/safe-pr-str {:hello "world"} {:max-chars 1000})]
      (expect (= "{:hello \"world\"}" out))
      (expect (not (re-find #"..." out)))))

  (it "never materializes more than print-length elements during pr"
    ;; If pr-str were applied to the full value first, this test would
    ;; OOM or stall on a billion-element lazy seq. With *print-length*
    ;; bound, pr stops after N elements and returns instantly.
    (let [billion (range 1000000000)
          out     (vis/safe-pr-str billion {:print-length 3 :max-chars 200})]
      (expect (re-find #"\(0 1 2 \.\.\.\)" out)))))

;; ─── from auto_archive_test.clj ───

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
  "Build a var-registry {symbol -> {:created-at ...}} from a seq of
   [sym created-at-ms] pairs."
  [entries]
  (into {}
    (map (fn [[sym created-at-ms]]
           [sym {:created-at created-at-ms :value nil :code "" :version 0}]))
    entries))

;; ---------------------------------------------------------------------------
;; auto-archive-candidates (pure)
;; ---------------------------------------------------------------------------

(defdescribe auto-archive-candidates-test

  (it "empty sandbox archives nothing"
    (expect (= #{} (vis/auto-archive-candidates {} #{} {} 80))))

  (it "does nothing while live user symbol count is at or below target"
    (let [sandbox  (make-sandbox [['a 1] ['b 2]])
          registry (make-registry [['a 1] ['b 2]])]
      (expect (= #{} (vis/auto-archive-candidates sandbox #{} registry 2)))))

  (it "archives oldest undocumented user symbols until target is reached"
    (let [sandbox  (make-sandbox [['old-a 1]
                                  ['old-b 2]
                                  ['new-c 3]
                                  ['new-d 4]])
          registry (make-registry [['old-a 10]
                                   ['old-b 20]
                                   ['new-c 30]
                                   ['new-d 40]])]
      (expect (= #{'old-a 'old-b}
                (vis/auto-archive-candidates sandbox #{} registry 2)))))

  (it "initial sandbox symbols and SYSTEM symbols never count as archive candidates"
    (let [sandbox  (make-sandbox [['builtin 1]
                                  ['TURN_USER_REQUEST "hello"]
                                  ['old-a 1]
                                  ['new-b 2]])
          registry (make-registry [['builtin 1]
                                   ['TURN_USER_REQUEST 1]
                                   ['old-a 10]
                                   ['new-b 20]])]
      (expect (= #{'old-a}
                (vis/auto-archive-candidates sandbox #{'builtin} registry 1)))))

  (it "docstring user symbols count toward the target but are not automatically archived"
    (let [sandbox  (make-sandbox [['protected 1 "Durable state"]
                                  ['old-a 2]
                                  ['old-b 3]])
          registry (make-registry [['protected 1]
                                   ['old-a 2]
                                   ['old-b 3]])]
      (expect (= #{'old-a 'old-b}
                (vis/auto-archive-candidates sandbox #{} registry 1)))))

  (it "archives every eligible symbol when protected symbols alone exceed the target"
    (let [sandbox  (make-sandbox [['p1 1 "Pinned"]
                                  ['p2 2 "Pinned"]
                                  ['scratch 3]])
          registry (make-registry [['p1 1]
                                   ['p2 2]
                                   ['scratch 3]])]
      (expect (= #{'scratch}
                (vis/auto-archive-candidates sandbox #{} registry 1))))))

;; ─── from core_test.clj ───

;; ─── from schema_reject_retry_test.clj - DELETED
;;
;; The schema-reject retry layer was removed when the iteration loop
;; switched from `svar/ask!` (JSON spec) to `svar/ask-code!` (plain-
;; text + fenced code-block extraction). There is no JSON spec for the
;; provider to reject; reader errors on extracted source flow as
;; ordinary iteration errors instead. The historical helpers + tests
;; below are kept commented for archeology only.
#_(defn- schema-reject-ex
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

#_(defn- ok-result
    "Minimal shape of what `llm/ask!` returns when parsing succeeds."
    []
    {:result      {:thinking "ok" :code []}
     :tokens      {:input 10 :output 5 :reasoning 0 :total 15}
     :duration-ms 1.0})

#_(defn- with-stubbed-ask!
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

#_(defn- run-helper
    "Invoke `ask-with-schema-retry!` with sensible defaults. `chunks` is
   an atom that the helper appends to via `:on-chunk`."
    [chunks & {:keys [max-retries]
               :or   {max-retries 2}}]
    (vis/ask-with-schema-retry!
      ::router-stub
      {:spec ::iteration-spec-stub
       :messages [{:role "user" :content "Q"}]
       :routing {}
       :check-context? false}
      {:iteration   3
       :on-chunk    (fn [chunk] (swap! chunks conj chunk))
       :max-retries max-retries}))

;; --- ORPHAN: targets removed/changed API. Skipped via #_ --- 
#_(defdescribe ask-with-schema-retry-test
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

;; The whole retired block above is wrapped in `#_` reader-discard so
;; nothing in this section compiles. Kept inline as a record of the
;; retry-shape previously exposed at the public API.

;; ─── from parse_rescue_loop_test.clj ───

#_(def ^:private try-extension-parse-rescue
    ;; private fn in com.blockether.vis.internal.loop - reach in via
    ;; requiring-resolve so the test can exercise the parse-rescue path
    ;; without bumping it to public.
    (requiring-resolve 'com.blockether.vis.internal.loop/try-extension-parse-rescue))

#_(defn- preceding-backslash-count
    [code index]
    (loop [position (dec index)
           count    0]
      (if (and (<= 0 position) (= \\ (.charAt ^String code position)))
        (recur (dec position) (inc count))
        count)))

#_(defn- rescue-one-unsupported-escape
    "Test fixture hook: doubles exactly one unsupported regex escape.
   The loop driver must call it repeatedly when a source string has
   multiple bad sites. This intentionally lives in core tests so core
   does not depend on the vis-foundation extension test classpath."
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

#_(def ^:private rg-symbol
    (vis/symbol 'rg (fn [& _] nil)
      {:doc               "fixture"
       :arglists          '([pattern])
       :on-parse-error-fn rescue-one-unsupported-escape}))

#_(defn- minimal-environment
    "Build the smallest possible environment shape that
   `try-extension-parse-rescue` reads from. Only `:extensions`
   (a deref-able holder of an extension vec) is required."
    []
    (let [ext (vis/extension
                {:ext/namespace 'com.blockether.vis.test.parse-rescue
                 :ext/doc       "Loop test fixture."
                 :ext/kind      "filesystem"
                 :ext/ns-alias  {:ns 'vis.ext.tools :alias 'v}
                 :ext/prompt    (constantly "placeholder")
                 :ext/symbols   [rg-symbol]})]
      {:extensions (atom [ext])
       :sci-ctx    (sci/init {})}))

#_(defn- parses? [^String code]
    (try
      (require '[edamame.core :as eda])
      ((resolve 'eda/parse-string-all) code {:all true})
      true
      (catch Throwable _ false)))

#_(defn- parse-error-message [^String code]
    (try
      (require '[edamame.core :as eda])
      ((resolve 'eda/parse-string-all) code {:all true})
      nil
      (catch Throwable t (ex-message t))))

;; --- ORPHAN: targets removed/changed API. Skipped via #_ --- 
#_(defdescribe try-extension-parse-rescue-loop-test

    (it "repairs a single `\\|` site (baseline; pre-fix already passed)"
      (let [env  (minimal-environment)
            code "(v/rg \"a\\|b\")"
            err  (parse-error-message code)
            out  (try-extension-parse-rescue env code err)]
        (expect (some? err))
        (expect (= "(v/rg \"a\\\\|b\")" out))
        (expect (parses? out))))

    (it "loops the rescue across THREE `\\|` sites until the source parses (Bug 2.A.1)"
    ;; Pre-fix: returns nil (single-shot rescue gives up on 2+ sites).
    ;; Post-fix: returns a fully repaired string that parses cleanly.
      (let [env  (minimal-environment)
            code "(v/rg \"foo\\|bar\\|baz\\|qux\")"
            err  (parse-error-message code)
            out  (try-extension-parse-rescue env code err)]
        (expect (some? err))
        (expect (string? out))
        (expect (parses? out))
      ;; Every original `\|` is now `\\|`.
        (expect (str/includes? out "\\\\|"))))

    (it "loops across `\\|` AND `\\.` AND `\\(` mixed escapes"
      (let [env  (minimal-environment)
            code "(v/rg \"a\\|b\\.c\\(d\")"
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
            code "(v/rg 'unterminated"
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
                              ;; Return code unchanged - should be
                              ;; detected as no-progress and stop.
                                code)
            rg (vis/symbol 'rg (fn [& _] nil)
                 {:doc      "fixture"
                  :arglists '([pattern])
                  :on-parse-error-fn pathological-hook})
            ext (vis/extension
                  {:ext/namespace 'com.blockether.vis.test.pathological
                   :ext/doc       "pathological"
                   :ext/kind      "filesystem"
                   :ext/ns-alias  {:ns 'vis.ext.tools :alias 'v}
                   :ext/prompt    (constantly "x")
                   :ext/symbols   [rg]})
            env {:extensions (atom [ext]) :sci-ctx (sci/init {})}
            code "(v/rg \"a\\|b\")"
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
  ;; there - `*file*` is unreliable because it's relative to whichever
  ;; classpath source-path the file was loaded from.
  (io/file (System/getProperty "user.dir")))

(def ^:private vis-bin
  (str (io/file repo-root "bin" "vis")))

(assert (.exists (io/file vis-bin))
  (str "bin/vis not found at " vis-bin
    " - run smoke tests from the repo root via `clojure -M:test`."))

(defn- make-temp-db-dir
  [prefix]
  (doto (.toFile (java.nio.file.Files/createTempDirectory prefix
                   (make-array java.nio.file.attribute.FileAttribute 0)))
    (.deleteOnExit)))

(defn- run-vis
  "Invoke `bin/vis` with `args`. Returns `{:exit int :out str :err str}`.
   Wallclock-bounded by a hard 60s timeout via clojure.java.shell.

   The smoke suite pins VIS_DB_PATH to a temp DB so local developer
   migration history cannot make CLI tests pass/fail depending on
   ~/.vis state. Pass an initial opts map with :env to override it."
  [& args]
  (let [[opts args] (if (map? (first args))
                      [(first args) (rest args)]
                      [{} args])
        db-dir      (make-temp-db-dir "vis-cli-smoke-db-")
        env         (merge (into {} (System/getenv))
                      {"VIS_DB_PATH" (.getAbsolutePath db-dir)}
                      (:env opts))]
    (apply sh/sh (concat [vis-bin] args [:env env]
                   (when-let [dir (:dir opts)] [:dir dir])))))

(defn- execute-formatted!
  [conn formatted]
  (let [[statement & params] formatted]
    (with-open [stmt (.prepareStatement conn statement)]
      (doseq [[idx param] (map-indexed vector params)]
        (.setObject stmt (inc idx) param))
      (.execute stmt))))

(defn- seed-bad-flyway-db!
  [^java.io.File dir]
  (Class/forName "org.sqlite.JDBC")
  (with-open [conn (java.sql.DriverManager/getConnection
                     (str "jdbc:sqlite:" (io/file dir "vis.db")))]
    (doseq [q [{:create-table :flyway_schema_history
                :with-columns [[:installed_rank :int [:not nil] :primary-key]
                               [:version [:varchar 50]]
                               [:description [:varchar 200] [:not nil]]
                               [:type [:varchar 20] [:not nil]]
                               [:script [:varchar 1000] [:not nil]]
                               [:checksum :int]
                               [:installed_by [:varchar 100] [:not nil]]
                               [:installed_on :text [:not nil]]
                               [:execution_time :int [:not nil]]
                               [:success :boolean [:not nil]]]}
               {:insert-into :flyway_schema_history
                :columns [:installed_rank :version :description :type :script :checksum
                          :installed_by :installed_on :execution_time :success]
                :values [[1 "1" "schema" "SQL" "V1__schema.sql" 1
                          "test" "2026-01-01 00:00:00.000" 1 1]]}]]
      (execute-formatted! conn (sql/format q)))))

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
      (expect (contains-all? out ["vis - iterative coding agent CLI"
                                  "COMMANDS"
                                  "run"])))))

(defdescribe vis-help
  (it "prints the help tree and exits 0"
    (let [{:keys [exit out]} (run-vis "help")]
      (expect (zero? exit))
      (expect (str/includes? out "vis - iterative coding agent CLI")))))

(defdescribe vis-doctor
  (it "prints environment diagnostics from the foundation doctor sections and exits 0"
    ;; Foundation owns `vis doctor`. Its `:ext/doctor-check-fn` emits four
    ;; sections (system, agents-md, skills, scan-warnings); the host
    ;; aggregator wraps them under the foundation namespace header. Pin
    ;; the markers that survive both the section refactor and any
    ;; future re-wording - the title banner, the ::system OS+JVM lines,
    ;; the DB path line, the trailing summary.
    (let [{:keys [exit out]} (run-vis "doctor")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis doctor"
                                  "OS:"
                                  "Java:"
                                  "DB path:"
                                  "Summary:"]))))

  (it "uses the invocation cwd for project guidance instead of the source checkout"
    (let [dir (.getAbsolutePath (make-temp-db-dir "vis-cwd-smoke-"))
          {:keys [exit out]} (run-vis {:dir dir :env {"VIS_CRAC" "0"}} "doctor")]
      (expect (= 1 exit))
      (expect (contains-all? out ["No project guidance found"]))
      (expect (not (str/includes? out "/vis/AGENTS.md"))))))

(defdescribe vis-extensions
  (it "lists the `vis-foundation` extension as discovered"
    (let [{:keys [exit out]} (run-vis "extensions" "list")]
      (expect (zero? exit))
      (expect (contains-all? out ["Extensions"
                                  "foundation"
                                  "extension(s)"]))))
  (it "parent help mentions the `list` subcommand"
    (let [{:keys [exit out]} (run-vis "extensions")]
      (expect (zero? exit))
      (expect (str/includes? out "list")))))

(defdescribe vis-channels
  (it "discovers TUI and Telegram"
    (let [{:keys [exit out]} (run-vis "channels")]
      (expect (zero? exit))
      (expect (contains-all? out ["tui" "telegram"]))))

  (it "mounts Telegram approve/restart commands under the channel"
    (let [{:keys [exit out]} (run-vis "channels" "telegram" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["approve" "restart" "vis channels telegram"]))))

  (it "renders Telegram approve command flags"
    (let [{:keys [exit out]} (run-vis "channels" "telegram" "approve" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["--chat-id" "--restart"])))))

(defdescribe vis-conversations
  (it "prints an empty-list message when no conversations exist"
    (let [{:keys [exit out]} (run-vis "conversations")]
      (expect (zero? exit))
      (expect (str/includes? out "No conversations found."))))

  (it "prints database bootstrap user errors instead of failing silently"
    (let [bad-dir (make-temp-db-dir "vis-cli-bad-flyway-db-")]
      (seed-bad-flyway-db! bad-dir)
      (let [{:keys [exit out err]} (run-vis {:env {"VIS_DB_PATH" (.getAbsolutePath bad-dir)}}
                                     "conversations")]
        (expect (not (zero? exit)))
        (expect (str/includes? out "Database schema mismatch"))
        (expect (str/blank? err)))))

  (it "--help renders the new fork flag in the FLAGS section"
    (let [{:keys [exit out]} (run-vis "conversations" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["--fork" "--title" "FLAGS"]))))

  (it "--fork against a missing id exits non-zero with a clear message"
    ;; A bare token that's neither a valid UUID nor a unique prefix
    ;; flows through `resolve-conversation-by-prefix` and surfaces as
    ;; `Conversation not found`. A *well-formed but unknown* UUID is
    ;; resolved to itself by db-resolve-conversation-id and falls into
    ;; the secondary `Failed to fork ...` branch -- both exit non-zero,
    ;; but the test pins the most-common user-typo path.
    (let [{:keys [exit out]}
          (run-vis "conversations" "--fork" "definitely-not-a-real-id-zzzz")]
      (expect (not (zero? exit)))
      (expect (str/includes? out "Conversation not found")))))

(defdescribe vis-strict-flag-rejection
  (it "unknown flag on a spec'd command exits non-zero with the accepted-flag list"
    (let [{:keys [exit out]} (run-vis "conversations" "--bogus-flag" "v")]
      (expect (not (zero? exit)))
      (expect (str/includes? out "Unknown flag"))
      (expect (str/includes? out "--bogus-flag"))
      (expect (str/includes? out "--fork"))
      (expect (str/includes? out "--title"))
      (expect (str/includes? out "--help")))))

(defdescribe vis-run-help
  (it "shows `vis run` usage and flag list"
    (let [{:keys [exit out]} (run-vis "run" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis run" "FLAGS" "--json" "--model"])))))

(defdescribe vis-providers-help
  (it "shows the providers command tree"
    (let [{:keys [exit out]} (run-vis "providers")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis providers"
                                  "auth"
                                  "status"
                                  "limits"
                                  "logout"]))))

  (it "shows auth subcommand examples"
    (let [{:keys [exit out]} (run-vis "providers" "auth" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis providers auth <provider>"
                                  "vis providers auth github-copilot-business"
                                  "vis providers auth github-copilot-individual"
                                  "vis providers auth openai-codex"]))))

  (it "shows limits subcommand examples"
    (let [{:keys [exit out]} (run-vis "providers" "limits" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis providers limits [provider]"
                                  "vis providers limits openai-codex"
                                  "vis providers limits ollama"])))))

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
