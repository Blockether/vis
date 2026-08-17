(ns com.blockether.vis.ext.language-clojure.shadow-repl-test
  "Hermetic tests for the shadow-cljs ClojureScript REPL seam: the port file a
   `watch` publishes, the read-only probe that tells a shadow-cljs nREPL from a
   plain JVM one, build selection, and the rule that keeps every later eval inside
   the build it named — never a sibling build sharing the same nREPL session.

   No shadow-cljs is started here — `nrepl-client` is redefined, and every answer
   staged below is one a LIVE shadow-cljs actually gave while verifying #151."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.shadow-repl :as shadow-repl]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir
  ^String []
  (.getAbsolutePath (.toFile (Files/createTempDirectory "vis-shadow-"
                                                        (into-array FileAttribute [])))))

(defn- with-port-file
  "A project dir where a `shadow-cljs watch` has published `content` as its nREPL
   port — the exact file, at the exact path, the real one writes."
  ^String [content]
  (let
    [dir
     (tmp-dir)

     f
     (apply io/file dir shadow-repl/port-file-path)]

    (io/make-parents f)
    (spit f content)
    dir))

(defdescribe nrepl-port-test
             (it "reads the port `shadow-cljs watch` published under the dir the caller named"
                 (expect (= 64579 (shadow-repl/nrepl-port (with-port-file "64579\n")))))
             (it "answers nil when no watch has published one — nothing is scanned"
                 (expect (nil? (shadow-repl/nrepl-port (tmp-dir)))))
             (it "answers nil, never throws, when the file holds something that is not a port"
                 (expect (nil? (shadow-repl/nrepl-port (with-port-file "starting…"))))))

(def ^:private live-probe-value
  "What the probe form printed on a real `shadow-cljs watch app`."
  (pr-str {:shadow true :builds [:npm :app] :worker true :target :node-script}))

(defdescribe
  probe-test
  (it "one read-only eval answers what the server is, which builds it loaded, and the target"
      (with-redefs
        [nrepl-client/eval! (fn [_]
                              {"value" live-probe-value})]
        (let [r (shadow-repl/probe! {:port 1 :build "app"})]
          (expect (true? (:shadow? r)))
          ;; The ids come back as STRINGS: they cross to the model, and the caller
          ;; compares them against the string it was given.
          (expect (= ["npm" "app"] (:builds r)))
          (expect (true? (:worker? r)))
          (expect (= :node-script (:target r))))))
  (it "RESOLVES every shadow Var, so the same form is safe on a plain JVM nREPL"
      (let [captured (atom nil)]
        (with-redefs
          [nrepl-client/eval! (fn [opts]
                                (reset! captured (:code opts))
                                {"value" live-probe-value})]
          (shadow-repl/probe! {:port 1 :build "app"})
          (expect (str/includes? @captured "(resolve 'shadow.cljs.devtools.api/worker-running?)"))
          ;; Never CALLED by name: an unloaded namespace would throw `No namespace`
          ;; and a plain JVM nREPL would read as unreachable instead of not-shadow.
          (expect (not (str/includes? @captured "(shadow.cljs.")))
          ;; The build reaches the server as the KEYWORD shadow indexes it by.
          (expect (str/includes? @captured ":app")))))
  (it "a plain JVM nREPL is reported as not-shadow, carrying its own words"
      (with-redefs
        [nrepl-client/eval! (fn [_]
                              {"value" "{:shadow false :builds nil :worker false}"})]
        (let [r (shadow-repl/probe! {:port 1 :build "app"})]
          (expect (false? (:shadow? r)))
          (expect (= [] (:builds r)))
          (expect (false? (:worker? r))))))
  (it "an unreachable server answers data with :error — the probe never throws"
      (with-redefs
        [nrepl-client/eval! (fn [_]
                              (throw (ex-info "connection refused" {})))]
        (let [r (shadow-repl/probe! {:port 1 :build "app"})]
          (expect (false? (:shadow? r)))
          (expect (str/includes? (str (:error r)) "connection refused"))))))

(defdescribe select-test
             (it "quits whatever build the session holds, then evaluates shadow's OWN nrepl-select"
                 (let [codes (atom [])]
                   (with-redefs
                     [nrepl-client/eval! (fn [opts]
                                           (swap! codes conj (:code opts))
                                           {"value" "[:selected :app]"})
                      nrepl-client/session-token (fn [_ _]
                                                   "tok-1")]

                     (let [r (shadow-repl/select! {:port 1 :build "app"})]
                       ;; `:cljs/quit` FIRST. Selecting from a session that already sits in
                       ;; a build compiles the select form AS ClojureScript and dies on the
                       ;; shadow namespace; in a session that never left CLJ the keyword
                       ;; evaluates to itself and costs one round trip.
                       (expect (= [":cljs/quit" "(shadow.cljs.devtools.api/nrepl-select :app)"]
                                  @codes))
                       (expect (true? (:selected? r)))
                       ;; What is selected is a property of the CONNECTION, build included.
                       (expect (shadow-repl/selected? {:port 1 :build "app"}))
                       (expect (not (shadow-repl/selected? {:port 1 :build "worker"})))))))
             (it "a refusal keeps the session in Clojure and reports shadow's own reason"
                 (with-redefs
                   [nrepl-client/eval! (fn [_]
                                         {"err" "watch for build not running\n"})]
                   (let [r (shadow-repl/select! {:port 2 :build "app"})]
                     (expect (false? (:selected? r)))
                     (expect (str/includes? (:message r) "watch for build not running"))
                     ;; Nothing may believe a selection that did not happen.
                     (expect (not (shadow-repl/selected? {:port 2 :build "app"})))))))

(defdescribe runtime-test
             (it "recognizes shadow's own no-runtime answer on stderr"
                 (expect (shadow-repl/no-runtime?
                           {"err" (str
                                    shadow-repl/no-runtime-marker
                                    ".\nSee https://shadow-cljs.github.io/docs/UsersGuide.html")}))
                 (expect (not (shadow-repl/no-runtime? {"value" "1"})))
                 (expect (not (shadow-repl/no-runtime? {"err" "Syntax error"}))))
             (it "phrases the missing runtime for THIS build's target, watch untouched"
                 (let [node (shadow-repl/runtime-hint "app" :node-script)]
                   (expect (str/includes? node "node"))
                   (expect (str/includes? node "shadow-cljs watch app"))
                   ;; A deps.edn-only project has no node_modules at all, and the
                   ;; script it just built dies on `Cannot find module 'ws'`.
                   (expect (str/includes? node "npm install ws")))
                 (expect (str/includes? (shadow-repl/runtime-hint "front" :browser) "browser"))
                 (expect (str/includes? (shadow-repl/runtime-hint "rn" :react-native) "simulator")))
             (it "still names both ways out for a target it does not know"
                 (let [h (shadow-repl/runtime-hint "x" :some-future-target)]
                   (expect (str/includes? h "node"))
                   (expect (str/includes? h "browser")))))

;; Regression, issue #151: `repl_eval` in a shadow-cljs project answered as JVM
;; Clojure — the build was never selected in the nREPL session the eval reused,
;; and a session replaced under vis (evicted socket, restarted watch) silently
;; went back to the JVM while still reporting the build. Tracking only that
;; SESSION was not enough either: a sibling build of the same watch shares it, so
;; selecting `:worker` made an eval that named `:app` answer from the worker's
;; runtime, with no error anywhere.
(defdescribe
  eval-selection-test
  (it "stays at ONE round trip while that build is still the selected one"
      (let [codes (atom [])]
        (with-redefs
          [nrepl-client/session-token (fn [_ _]
                                        "tok-1")
           nrepl-client/eval! (fn [opts]
                                (swap! codes conj (:code opts))
                                {"value" "[:selected :app]"})]

          (shadow-repl/select! {:port 10 :build "app"})
          (reset! codes [])
          (let [r (shadow-repl/eval! {:port 10 :build "app"} {:code "(greeting)"})]
            (expect (true? (:selected? r)))
            (expect (= ["(greeting)"] @codes))))))
  (it "re-selects when the nREPL session was replaced under it"
      (let
        [codes
         (atom [])

         answer
         (fn [opts]
           (swap! codes conj (:code opts))
           {"value" "[:selected :app]"})]

        (with-redefs
          [nrepl-client/session-token
           (fn [_ _]
             "tok-1")

           nrepl-client/eval!
           answer]

          (shadow-repl/select! {:port 11 :build "app"}))
        (reset! codes [])
        (with-redefs
          [nrepl-client/session-token
           (fn [_ _]
             "tok-2")

           nrepl-client/eval!
           answer]

          (shadow-repl/eval! {:port 11 :build "app"} {:code "1"})
          ;; A fresh session is CLJ again — the very same code would otherwise
          ;; answer as JVM Clojure while still reporting the build.
          (expect (= [":cljs/quit" "(shadow.cljs.devtools.api/nrepl-select :app)" "1"] @codes)))))
  (it "re-selects when a SIBLING build of the same watch took the session over"
      (let [codes (atom [])]
        (with-redefs
          [nrepl-client/session-token (fn [_ _]
                                        "tok-1")
           nrepl-client/eval! (fn [opts]
                                (swap! codes conj (:code opts))
                                {"value" "[:selected :selected]"})]

          (shadow-repl/select! {:port 12 :build "app"})
          ;; ONE session serves every build of that server: selecting :worker
          ;; moved :app's evals with it.
          (shadow-repl/select! {:port 12 :build "worker"})
          (expect (not (shadow-repl/selected? {:port 12 :build "app"})))
          (reset! codes [])
          (let [r (shadow-repl/eval! {:port 12 :build "app"} {:code "(app.core/greeting)"})]
            (expect (true? (:selected? r)))
            ;; Without this re-select the answer came from the WORKER's runtime.
            (expect (= [":cljs/quit" "(shadow.cljs.devtools.api/nrepl-select :app)"
                        "(app.core/greeting)"]
                       @codes))))))
  (it "selects on the FIRST eval of an attachment that never selected"
      (let [codes (atom [])]
        (with-redefs
          [nrepl-client/session-token (fn [_ _]
                                        "tok-1")
           nrepl-client/eval! (fn [opts]
                                (swap! codes conj (:code opts))
                                {"value" "[:selected :app]"})]

          (shadow-repl/eval! {:port 13 :build "app"} {:code "1"})
          (expect (= [":cljs/quit" "(shadow.cljs.devtools.api/nrepl-select :app)" "1"] @codes)))))
  (it "evaluates NOTHING when the build can no longer be selected"
      (let [codes (atom [])]
        (with-redefs
          [nrepl-client/session-token (fn [_ _]
                                        "tok-9")
           nrepl-client/eval! (fn [opts]
                                (swap! codes conj (:code opts))
                                {"err" "watch for build not running\n"})]

          (let [r (shadow-repl/eval! {:port 14 :build "app"} {:code "(greeting)"})]
            (expect (false? (:selected? r)))
            (expect (str/includes? (:message r) "watch for build not running"))
            ;; The user's code must not reach a session that is back in Clojure.
            (expect (not (some #{"(greeting)"} @codes)))))))
  (it "turns shadow's bare no-runtime error into the instruction that starts one"
      (with-redefs
        [nrepl-client/session-token
         (fn [_ _]
           "tok-1")

         nrepl-client/eval!
         (fn [opts]
           (if (str/includes? (str (:code opts)) "nrepl-select")
             {"value" "[:selected :app]"}
             {"err" (str shadow-repl/no-runtime-marker ".\nSee https://…")}))]

        (let
          [r (shadow-repl/eval! {:port 15 :build "app" :target :node-script} {:code "(greeting)"})]
          ;; The attachment is HEALTHY — it simply has nothing to evaluate in.
          (expect (true? (:selected? r)))
          (expect (str/includes? (:message r) "node"))
          (expect (str/includes? (:message r) "no JS runtime"))))))
