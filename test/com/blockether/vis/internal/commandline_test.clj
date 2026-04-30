(ns com.blockether.vis.internal.commandline-test
  "Direct tests for the internal commandline namespace. Most of the
   public surface is exercised through `com.blockether.vis.core` in
   `core_test`; this file pins behavior that's namespace-internal or
   that complements the SDK-facing tests so the unit boundary stays
   covered when refactors push code around."
  (:require
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.registry :as registry]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe parse-args-test
  (describe "positional + flag parsing"
    (it "parses positionals in declaration order with type coercion"
      (let [specs [{:name "path"  :kind :positional :type :string}
                   {:name "count" :kind :positional :type :int}]
            parsed (commandline/parse-args specs ["src" "7"])]
        (expect (= "src" (parsed "path")))
        (expect (= 7 (parsed "count")))))

    (it "boolean flag does not consume its next token"
      (let [specs [{:name "verbose" :kind :flag :type :boolean}
                   {:name "path"    :kind :positional :type :string}]
            parsed (commandline/parse-args specs ["--verbose" "src"])]
        (expect (true? (parsed "verbose")))
        (expect (= "src" (parsed "path")))))

    (it "unknown flags are dropped at the parse layer (loose helper)"
      (expect (= {} (commandline/parse-args [] ["--bogus" "x"]))))))

(defdescribe validate-args-test
  (it "returns nil when every required arg is present"
    (let [specs [{:name "p" :kind :positional :required true}]]
      (expect (nil? (commandline/validate-args specs {"p" "ok"})))))

  (it "lists every missing required arg in the error message"
    (let [specs [{:name "a" :kind :positional :required true}
                 {:name "b" :kind :flag :type :string :required true}]]
      (expect (re-find #"Missing required.*a.*b"
                (commandline/validate-args specs {}))))))

(defdescribe unknown-flags-test
  (it "returns [] when every -- token is declared"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}
                 {:name "out"     :kind :flag :type :string}]]
      (expect (= [] (commandline/unknown-flags specs ["--verbose" "--out" "/tmp"])))))

  (it "reports tokens not in the spec"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}]]
      (expect (= ["--bogus"] (commandline/unknown-flags specs ["--bogus"])))))

  (it "treats --help / -h as universally accepted"
    (expect (= [] (commandline/unknown-flags [] ["--help"])))
    (expect (= [] (commandline/unknown-flags [] ["-h"]))))

  (it "does not mis-classify a string-flag VALUE that looks like a flag"
    (let [specs [{:name "out" :kind :flag :type :string}]]
      (expect (= [] (commandline/unknown-flags specs ["--out" "--weird"])))))

  (it "advances past unknown flags conservatively (one token at a time)"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}]]
      (expect (= ["--a" "--b"] (commandline/unknown-flags specs ["--a" "--b"]))))))

(defdescribe dispatch!-strict-flags-test
  (it "rejects unknown flags when the command declares any flag"
    (let [run-called (atom false)
          spec {:cmd/name "deploy"
                :cmd/doc  "deploy"
                :cmd/args [{:name "verbose" :kind :flag :type :boolean}]
                :cmd/run-fn (fn [_ _] (reset! run-called true))}
          root (registry/command
                 {:cmd/name "vis" :cmd/doc "root"
                  :cmd/subcommands [spec]})
          r    (commandline/dispatch! root ["vis" "deploy" "--bogus"]
                 {:print-fn (constantly nil)})]
      (expect (= :error (:status r)))
      (expect (re-find #"Unknown flag" (:error r)))
      (expect (re-find #"--bogus" (:error r)))
      (expect (false? @run-called))))

  (it "does NOT police flags when the command declares none (loose mode)"
    (let [seen (atom nil)
          spec {:cmd/name "auth"
                :cmd/doc  "auth"
                :cmd/run-fn (fn [_p residual] (reset! seen residual))}
          root (registry/command
                 {:cmd/name "vis" :cmd/doc "root"
                  :cmd/subcommands [spec]})
          r    (commandline/dispatch! root ["vis" "auth" "p" "--status"]
                 {:print-fn (constantly nil)})]
      (expect (= :ok (:status r)))
      (expect (= ["p" "--status"] @seen))))

  (it "renders help on --help even when other tokens would be unknown"
    (let [spec {:cmd/name "deploy" :cmd/doc "d"
                :cmd/args [{:name "verbose" :kind :flag :type :boolean}]
                :cmd/run-fn (fn [_ _] :should-not-run)}
          root (registry/command
                 {:cmd/name "vis" :cmd/doc "root"
                  :cmd/subcommands [spec]})
          r    (commandline/dispatch! root ["vis" "deploy" "--help"]
                 {:print-fn (constantly nil)})]
      (expect (= :help (:status r))))))

(defdescribe render-help-test
  (it "render-command lists declared flags so the user sees what's accepted"
    (let [c {:cmd/name "deploy"
             :cmd/doc  "Ship code somewhere."
             :cmd/args [{:name "verbose" :kind :flag :type :boolean :doc "Chatty mode."}
                        {:name "out"     :kind :flag :type :string  :doc "Target dir."}]}
          out (commandline/render-command c ["vis" "deploy"])]
      (expect (re-find #"--verbose" out))
      (expect (re-find #"--out OUT" out))))

  (it "render-tree shows immediate subcommands"
    (let [root (registry/command
                 {:cmd/name "vis" :cmd/doc "root"
                  :cmd/subcommands [{:cmd/name "deploy" :cmd/doc "ship"}
                                    {:cmd/name "rollback" :cmd/doc "undo"}]})]
      (expect (re-find #"deploy" (commandline/render-tree root)))
      (expect (re-find #"rollback" (commandline/render-tree root))))))
