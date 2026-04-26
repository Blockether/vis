(ns com.blockether.vis.commandline-test
  "Coverage for the reusable command-line primitives. Pure data tests
   \u2014 no I/O, no global state, no other vis packages required."
  (:require
   [com.blockether.vis.commandline :as cmd]
   [lazytest.core :refer [defdescribe it expect throws?]]))

(defn- clear-registry! []
  ;; Reach into the registry atom from tests so each `it` starts with
  ;; a clean slate. Private on purpose - production code never resets.
  (reset! @#'cmd/global-registry []))

(defdescribe spec-test
  (it "command/build validates required keys"
    (let [c (cmd/command {:cmd/name "run" :cmd/doc "Run something."})]
      (expect (= "run" (:cmd/name c)))))

  (it "command/build throws on missing :cmd/name"
    (expect (throws? Throwable #(cmd/command {:cmd/doc "no name"}))))

  (it "command/build throws on missing :cmd/doc"
    (expect (throws? Throwable #(cmd/command {:cmd/name "x"})))))

(defdescribe subcommand-resolution-test
  (let [root (cmd/command
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
            (cmd/find-leaf root ["vis" "run" "--foo" "bar"])]
        (expect (= "run" (:cmd/name command)))
        (expect (= ["vis" "run"] path))
        (expect (= ["--foo" "bar"] residual))))

    (it "find-leaf walks dynamic subcommands"
      (let [{:keys [command path residual]}
            (cmd/find-leaf root ["vis" "channel" "tui" "--resume"])]
        (expect (= "tui" (:cmd/name command)))
        (expect (true? (:cmd/owns-tty? command)))
        (expect (= ["vis" "channel" "tui"] path))
        (expect (= ["--resume"] residual))))

    (it "find-leaf returns parent when next token doesn't match a child"
      (let [{:keys [command residual]}
            (cmd/find-leaf root ["vis" "channel" "nope"])]
        (expect (= "channel" (:cmd/name command)))
        (expect (= ["nope"] residual))))

    (it "dispatch! invokes leaf run-fn and returns :ok"
      (let [r (cmd/dispatch! root ["vis" "run"] {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= :ran-run (:result r)))))

    (it "dispatch! renders help when leaf has no run-fn but has subcommands"
      (let [r (cmd/dispatch! root ["vis" "channel"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))
        (expect (re-find #"tui channel" (:help-text r)))))

    (it "dispatch! renders help on --help in the residual"
      (let [r (cmd/dispatch! root ["vis" "run" "--help"] {:print-fn (constantly nil)})]
        (expect (= :help (:status r)))))

    (it "dispatch! returns :no-match when even the root name doesn't match"
      (let [r (cmd/dispatch! root ["NOT-VIS"] {:print-fn (constantly nil)})]
        (expect (= :no-match (:status r)))))))

(defdescribe arg-parsing-test
  (it "parses positional + flag args with type coercion"
    (let [specs [{:name "path"  :kind :positional :type :string :required true}
                 {:name "count" :kind :positional :type :int}
                 {:name "verbose" :kind :flag :type :boolean}
                 {:name "out"     :kind :flag :type :string}]
          parsed (cmd/parse-args specs ["src/foo.clj" "5" "--verbose" "--out" "/tmp/x"])]
      (expect (= "src/foo.clj" (parsed "path")))
      (expect (= 5            (parsed "count")))
      (expect (= true         (parsed "verbose")))
      (expect (= "/tmp/x"     (parsed "out")))))

  (it "int-typed flag is coerced from string to long"
    (let [specs [{:name "depth" :kind :flag :type :int}]]
      (expect (= 7 (get (cmd/parse-args specs ["--depth" "7"]) "depth")))))

  (it "boolean flag needs no value and never consumes the next token"
    (let [specs [{:name "verbose" :kind :flag :type :boolean}
                 {:name "path"    :kind :positional :type :string}]
          parsed (cmd/parse-args specs ["--verbose" "src/foo.clj"])]
      (expect (= true (parsed "verbose")))
      (expect (= "src/foo.clj" (parsed "path")))))

  (it "flags can appear anywhere in the residual without disturbing positionals"
    (let [specs [{:name "path"  :kind :positional :type :string}
                 {:name "count" :kind :positional :type :int}
                 {:name "out"   :kind :flag :type :string}]
          parsed (cmd/parse-args specs ["./x" "--out" "/tmp" "3"])]
      (expect (= "./x" (parsed "path")))
      (expect (= 3 (parsed "count")))
      (expect (= "/tmp" (parsed "out")))))

  (it "validate-args returns nil when all required present"
    (let [specs [{:name "p" :kind :positional :required true}]]
      (expect (nil? (cmd/validate-args specs {"p" "ok"})))))

  (it "validate-args reports missing required args"
    (let [specs [{:name "p" :kind :positional :required true}
                 {:name "q" :kind :positional :required true}]]
      (expect (re-find #"Missing.*p.*q" (cmd/validate-args specs {})))))

  (it "unknown flags are silently dropped"
    (expect (= {} (cmd/parse-args [] ["--whatever" "value"])))))

(defdescribe registered-command-with-args-test
  ;; End-to-end: registered command → mounted into a parent → dispatched
  ;; with a real arg vector → run-fn sees a fully-coerced parsed map.
  (let [captured (atom nil)]

    (it "dispatches a registered command with positional + flag + type coercion"
      (clear-registry!)
      (reset! captured nil)
      (cmd/register-global!
        {:cmd/name "deploy"
         :cmd/doc  "deploy something"
         :cmd/args [{:name "path"    :kind :positional :type :string :required true}
                    {:name "count"   :kind :positional :type :int}
                    {:name "verbose" :kind :flag       :type :boolean}
                    {:name "out"     :kind :flag       :type :string}
                    {:name "depth"   :kind :flag       :type :int}]
         :cmd/run-fn (fn [parsed _residual] (reset! captured parsed))})
      (let [root (cmd/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(cmd/registered-under [])})
            r    (cmd/dispatch! root
                   ["vis" "deploy" "./src" "5" "--verbose" "--out" "/tmp" "--depth" "3"]
                   {:print-fn (constantly nil)})]
        (expect (= :ok (:status r)))
        (expect (= {"path" "./src" "count" 5 "verbose" true
                    "out" "/tmp"   "depth" 3}
                  @captured))))

    (it "missing required positional surfaces a validation error"
      (clear-registry!)
      (cmd/register-global!
        {:cmd/name "deploy"
         :cmd/doc  "deploy"
         :cmd/args [{:name "path" :kind :positional :type :string :required true}]
         :cmd/run-fn (fn [_ _] :should-not-run)})
      (let [root (cmd/command
                   {:cmd/name "vis" :cmd/doc "root"
                    :cmd/subcommands #(cmd/registered-under [])})
            r    (cmd/dispatch! root ["vis" "deploy"]
                   {:print-fn (constantly nil)})]
        (expect (= :error (:status r)))
        (expect (re-find #"Missing required.*path" (:error r)))))

    (it "render-command for a registered command lists ARGUMENTS + FLAGS sections"
      (clear-registry!)
      (let [c (cmd/register-global!
                {:cmd/name "deploy"
                 :cmd/doc  "deploy"
                 :cmd/args [{:name "path"    :kind :positional :type :string :required true :doc "Source path."}
                            {:name "verbose" :kind :flag :type :boolean :doc "Chatty."}]
                 :cmd/run-fn (fn [_ _] nil)})
            out (cmd/render-command c ["vis" "deploy"])]
        (expect (re-find #"ARGUMENTS" out))
        (expect (re-find #"<path>\s+Source path\." out))
        (expect (re-find #"FLAGS" out))
        (expect (re-find #"--verbose\s+Chatty\." out))))))

(defdescribe registry-test
  (it "register-global! validates and stores by [parent name]"
    (clear-registry!)
    (cmd/register-global! {:cmd/name "alpha" :cmd/doc "a"})
    (cmd/register-global! {:cmd/name "beta" :cmd/parent ["ext"] :cmd/doc "b"})
    (expect (= 2 (count (cmd/registered-commands)))))

  (it "register-global! is idempotent on [parent name]"
    (clear-registry!)
    (cmd/register-global! {:cmd/name "alpha" :cmd/doc "v1"})
    (cmd/register-global! {:cmd/name "alpha" :cmd/doc "v2"})
    (expect (= 1 (count (cmd/registered-commands))))
    (expect (= "v2" (:cmd/doc (first (cmd/registered-commands))))))

  (it "registered-under returns only commands for the given parent"
    (clear-registry!)
    (cmd/register-global! {:cmd/name "top"  :cmd/doc "top"})
    (cmd/register-global! {:cmd/name "e1" :cmd/parent ["ext"]     :cmd/doc "e1"})
    (cmd/register-global! {:cmd/name "e2" :cmd/parent ["ext"]     :cmd/doc "e2"})
    (cmd/register-global! {:cmd/name "c1" :cmd/parent ["channel"] :cmd/doc "c1"})
    (expect (= ["top"]      (mapv :cmd/name (cmd/registered-under []))))
    (expect (= ["e1" "e2"]  (mapv :cmd/name (cmd/registered-under ["ext"]))))
    (expect (= ["c1"]       (mapv :cmd/name (cmd/registered-under ["channel"])))))

  (it "deregister-global! removes the entry by parent + name"
    (clear-registry!)
    (cmd/register-global! {:cmd/name "x" :cmd/parent ["ext"] :cmd/doc "d"})
    (cmd/deregister-global! ["ext"] "x")
    (expect (empty? (cmd/registered-commands))))

  (it "a registered command can be mounted into a parent via :cmd/subcommands fn"
    (clear-registry!)
    (cmd/register-global!
      {:cmd/name "git-status" :cmd/parent ["ext"] :cmd/doc "git status"
       :cmd/run-fn (fn [_ _] :ran)})
    (let [parent (cmd/command
                   {:cmd/name "ext" :cmd/doc "ext parent"
                    :cmd/subcommands #(cmd/registered-under ["ext"])})
          r      (cmd/dispatch! parent ["ext" "git-status"]
                   {:print-fn (constantly nil)})]
      (expect (= :ok (:status r)))
      (expect (= :ran (:result r))))))

(defdescribe help-rendering-test
  (it "render-tree lists every immediate subcommand under a COMMANDS section"
    (let [root (cmd/command
                 {:cmd/name "vis"
                  :cmd/doc  "test root"
                  :cmd/subcommands
                  [{:cmd/name "alpha" :cmd/doc "alpha doc"}
                   {:cmd/name "beta"  :cmd/doc "beta doc"}]})
          out  (cmd/render-tree root)]
      (expect (re-find #"COMMANDS" out))
      (expect (re-find #"alpha\s+alpha doc" out))
      (expect (re-find #"beta\s+beta doc"  out))
      ;; Footer hint nudges users to deeper help.
      (expect (re-find #"vis <command> --help" out))))

  (it "render-command emits USAGE / DESCRIPTION / FLAGS sections"
    (let [c (cmd/command
              {:cmd/name  "run"
               :cmd/doc   "run something"
               :cmd/usage "vis run [FLAGS]"
               :cmd/args  [{:name "verbose" :kind :flag :type :boolean :doc "noisy"}
                           {:name "model"   :kind :flag :type :string  :doc "override model"}]
               :cmd/examples ["vis run --model gpt-4o"]})
          out (cmd/render-command c ["vis" "run"])]
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
    (let [c (cmd/command
              {:cmd/name "channel"
               :cmd/doc  "channel parent"
               :cmd/subcommands
               [{:cmd/name "tui"      :cmd/doc "interactive UI"}
                {:cmd/name "telegram" :cmd/doc "telegram bot"}]})
          out (cmd/render-command c ["vis" "channel"])]
      (expect (re-find #"SUBCOMMANDS" out))
      (expect (re-find #"tui\s+interactive UI" out))
      (expect (re-find #"telegram\s+telegram bot" out)))))
