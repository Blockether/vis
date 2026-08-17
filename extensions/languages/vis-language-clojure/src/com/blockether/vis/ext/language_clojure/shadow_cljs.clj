(ns com.blockether.vis.ext.language-clojure.shadow-cljs
  "shadow-cljs as the ClojureScript TEST RUNNER: which build runs the tests, how
   THIS machine invokes shadow-cljs, and the exact argv that runs a narrowed
   selection. A `*_test.cljs` never loads on the JVM, so `clojure -M:test` can no
   more run it than `node` can run a `.clj` — the build is not a preference here,
   it is the only runtime that exists.

   Three facts decide the command, and each has its own honest refusal instead of
   a guess:

   1. HOW shadow-cljs is installed. `node_modules/.bin/shadow-cljs` (npm) wins
      because it is what the project's own `npm test` runs; a
      `thheller/shadow-cljs` dependency in `deps.edn` runs as
      `clojure -M[:alias] -m shadow.cljs.devtools.cli` — the SAME project may
      carry it either way, and an alias-only dependency needs that alias on the
      command line or the classpath lacks the namespace being `-m`'d. Declared in
      `package.json` but not installed is answered as `npm install`, not as
      \"no ClojureScript runner\".
   2. WHICH build runs tests. `:node-test` runs headless and wins; `:karma`
      drives its own browser through the karma binary; `:browser-test` needs a
      browser RUNTIME to connect back to the build, which run_tests cannot
      supply — it is named and refused rather than compiled into a green run
      that asserted nothing.
   3. WHAT the run is narrowed to. `--config-merge` carries `:autorun true` (so a
      `compile` also RUNS) and `:ns-regexp` (the namespace focus), printed with
      `pr-str`: `\\.` is not a legal EDN escape, and a hand-built regexp string
      makes the shadow-cljs CLI print its help text and exit ZERO — a silent
      no-run that looks exactly like a pass."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private cli-main
  "shadow-cljs' own CLI entry point — every launcher that is not the npm binary
   ends at this main."
  "shadow.cljs.devtools.cli")

(def ^:private shadow-dep
  "The Maven coordinate a deps.edn / project.clj declares shadow-cljs under."
  'thheller/shadow-cljs)

(defn- read-edn
  "Read an EDN config file, or nil when it is absent or unreadable. Unknown
   reader tags (`#shadow/env`) are kept as their VALUE instead of throwing:
   discovery must never lose the run over a tag it does not know."
  [^java.io.File f]
  (try (when (.isFile f)
         (edn/read-string {:default (fn [_tag value]
                                      value)}
                          (slurp f)))
       (catch Throwable _ nil)))

(defn config
  "The project's `shadow-cljs.edn` as EDN, or nil when it has none."
  [root]
  (read-edn (io/file (str root) "shadow-cljs.edn")))

(defn- deps-alias-with-shadow
  "Where a deps.edn declares shadow-cljs: `:root` for the project's own `:deps`,
   the ALIAS keyword when only an alias carries it, nil when neither does.
   shadow-cljs is as often an alias-only tool dependency as a project one, and
   the alias has to reach the command line or `-m shadow.cljs.devtools.cli`
   resolves nothing."
  [edn]
  (when (map? edn)
    (if (contains? (:deps edn) shadow-dep)
      :root
      (some (fn [[alias-key alias-map]]
              (when (or (contains? (:extra-deps alias-map) shadow-dep)
                        (contains? (:deps alias-map) shadow-dep))
                alias-key))
            (:aliases edn)))))

(defn- npm-declares-shadow?
  "True when `package.json` NAMES shadow-cljs as a dependency. Read as text on
   purpose: this only decides which HINT a missing install gets, and a JSON parse
   that throws would trade a good message for none."
  [^java.io.File f]
  (boolean (try (when (.isFile f) (re-find #"\"shadow-cljs\"\s*:" (slurp f)))
                (catch Throwable _ nil))))

(defn launcher
  "How THIS project invokes the shadow-cljs CLI: `{:kind :npm|:deps|:lein :argv
   [...]}`, or `{:error ...}` naming exactly what is missing. The argv is a
   PREFIX — the caller appends `compile <build> ...` to it."
  [root]
  (let
    [dir
     (io/file (str root))

     bin
     (io/file dir "node_modules" ".bin" "shadow-cljs")

     deps-alias
     (deps-alias-with-shadow (read-edn (io/file dir "deps.edn")))

     lein
     (:lein (config root))]

    (cond (.isFile bin) {:kind :npm :argv [(.getPath bin)]}
          (= :root deps-alias) {:kind :deps :argv ["clojure" "-M" "-m" cli-main]}
          deps-alias {:kind :deps :argv ["clojure" (str "-M" deps-alias) "-m" cli-main]}
          (or lein (.isFile (io/file dir "project.clj")))
          {:kind :lein
           :argv (into ["lein"]
                       (concat (when-let [profile (:profile lein)]
                                 ["with-profile" (str profile)])
                               ["run" "-m" cli-main]))}
          (npm-declares-shadow? (io/file dir "package.json"))
          {:error
           (str "shadow-cljs is declared in package.json but not installed — run `npm install` in "
                (.getPath dir)
                " (node_modules/.bin/shadow-cljs is missing)")}
          :else {:error (str
                          "no way to run shadow-cljs in " (.getPath dir)
                          " — none of node_modules/.bin/shadow-cljs (npm install),"
                          " a thheller/shadow-cljs dependency in deps.edn, or a lein project")})))

(def ^:private target-kind
  "What each shadow-cljs TEST target needs to produce a result: `:headless` runs
   itself under node, `:karma` drives its own browser through the karma binary,
   and `:runtime` needs a browser to connect back to the build — compiling that
   one asserts NOTHING, which is why it is refused instead of reported green."
  {:node-test :headless :karma :karma :browser-test :runtime})

(defn- describe-builds
  "Every build in the config as `id (:target)`, id-sorted — what a refusal owes
   the caller: the choices it actually had."
  [builds]
  (if (seq builds)
    (str/join ", "
              (map (fn [[id build]]
                     (str id " (" (pr-str (:target build)) ")"))
                   (sort-by (comp str key) builds)))
    "none"))

(defn test-build
  "The build whose tests run, as `{:id :target :build}` — or `{:error ...}`.
   `requested` names one explicitly (the run_tests `build` selector); with none,
   the first `:node-test`, then `:karma`, then `:browser-test`, each in id order
   so two runs of the same project never pick differently."
  [cfg requested]
  (let
    [builds
     (:builds cfg)

     entries
     (sort-by (comp str key) builds)

     chosen
     (fn [[id build]]
       {:id id :target (:target build) :build build})]

    (if (seq (str requested))
      (let
        [id
         (keyword (str/replace (str requested) #"^:" ""))

         build
         (get builds id)]

        (cond
          (nil? build) {:error (str "shadow-cljs.edn has no build " id
                                    " — builds: " (describe-builds builds))}
          (nil? (target-kind (:target build)))
          {:error
           (str "build "
                id
                " targets "
                (pr-str (:target build))
                ", which runs no tests — a test build targets :node-test, :karma or :browser-test")}
          :else (chosen [id build])))
      (if-let
        [hit (some (fn [target]
                     (some (fn [[_ build :as entry]]
                             (when (= target (:target build)) entry))
                           entries))
                   [:node-test :karma :browser-test])]
        (chosen hit)
        {:error
         (str
           "shadow-cljs.edn declares no test build — builds: "
           (describe-builds builds)
           ". Add one, e.g. :builds {:test {:target :node-test :output-to \"target/node-tests.js\"}}")}))))

(defn ns-regexp
  "The `:ns-regexp` that selects EXACTLY `nses`. shadow-cljs `re-find`s this
   against every compiled namespace, so the anchors are what stop `a.core-test`
   from also dragging in `a.core-test-helpers`."
  [nses]
  (str "^("
       (str/join "|"
                 (map (fn [n]
                        (str/replace (str n) "." "\\."))
                      (sort nses)))
       ")$"))

(defn run-steps
  "The commands that RUN the ClojureScript tests of `root`, in order, as
   `{:build :target :kind :steps [{:argv [...]}]}` — or `{:error ...}` when this
   project cannot run them, which is data the caller reports, never an exception.
   `:nses` narrows the run (all of them when empty) and `:build` names one build
   instead of letting the target order pick.
   `:node-test` compiles WITH `:autorun true`, so the single step both builds and
   runs; `:karma` compiles and then runs karma's own single-shot binary."
  [root {:keys [nses build]}]
  (let [cfg (config root)]
    (if (nil? cfg)
      {:error (str "no shadow-cljs.edn in "
                   root
                   " — ClojureScript tests (*_test.cljs) run through a shadow-cljs build")}
      (let
        [{:keys [error id target]} (test-build cfg build)
         {launcher-error :error argv :argv kind :kind} (launcher root)]

        (cond error {:error error}
              ;; Refused BEFORE the launcher is resolved: installing shadow-cljs
              ;; would not make a browser build runnable, so `npm install` is the
              ;; wrong thing to be told first.
              (= :runtime (target-kind target))
              {:error (str "build "
                           id
                           " targets "
                           (pr-str target)
                           ": its tests run INSIDE a browser that connects back to the build,"
                           " and run_tests has no browser to connect."
                           " Run them with `shadow-cljs watch "
                           (name id)
                           "` and an open page,"
                           " or add a headless test build (:node-test, or :karma with"
                           " node_modules/.bin/karma installed)")}
              launcher-error {:error launcher-error}
              :else (let
                      [merged (cond-> {}
                                (= :node-test target)
                                (assoc :autorun true)

                                (seq nses)
                                (assoc :ns-regexp (ns-regexp nses)))
                       compile-argv (into (vec argv)
                                          (cond-> ["compile" (name id)]
                                            (seq merged)
                                            (into ["--config-merge" (pr-str merged)])))
                       base {:build (name id) :target target :kind kind}]

                      (if (= :karma (target-kind target))
                        (let [karma (io/file (str root) "node_modules" ".bin" "karma")]
                          (if (.isFile karma)
                            (assoc base
                              :steps [{:argv compile-argv}
                                      {:argv [(.getPath karma) "start" "--single-run"]}])
                            {:error (str "build "
                                         id
                                         " targets :karma but node_modules/.bin/karma is missing"
                                         " — run `npm install` in "
                                         root)}))
                        (assoc base :steps [{:argv compile-argv}]))))))))
