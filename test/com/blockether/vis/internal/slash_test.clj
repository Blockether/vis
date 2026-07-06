(ns com.blockether.vis.internal.slash-test
  "Slash dispatch tests.

   Slash specs are declarative — every test builds a tiny env map
   containing an `:extensions` atom of extension records whose
   `:ext/slash-commands` carry the fixtures. `active-slashes` and
   `dispatch` walk that env exactly the same way the live engine
   does at turn start."
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.slash :as slash]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- ext
  "Build a registered-shape extension carrying `slashes`."
  [name slashes]
  (extension/extension
    {:ext/name name :ext/description (str name " test extension") :ext/slash-commands slashes}))

(defn- env-of
  "Build an environment seeded with the supplied extensions. Mirrors
   the shape `prompt/active-extensions` reads (`:extensions` deref'd
   to a seq)."
  [exts]
  {:extensions (atom (vec exts))})

(def ^:private workspace-spec
  {:slash/name "workspace"
   :slash/doc "Workspace top-level"
   :slash/run-fn (fn [_ctx]
                   {:slash/status :ok :slash/title "workspace help"})})

(def ^:private workspace-apply
  {:slash/name "apply"
   :slash/parent ["workspace"]
   :slash/doc "Apply a workspace"
   :slash/requires #{:session :workspace}
   :slash/run-fn (fn [ctx]
                   {:slash/status :ok
                    :slash/title "applied"
                    :slash/argv (:command/argv ctx)
                    :slash/path (:command/path ctx)})})

(def ^:private voice-spec
  {:slash/name "voice"
   :slash/doc "Toggle voice"
   :slash/availability-fn (fn [ctx]
                            (= :tui (:channel/id ctx)))
   :slash/run-fn (fn [_]
                   {:slash/status :ok :slash/title "voice toggled"})})

(defdescribe slash-path-test
             (it "slash-path joins parent + name"
                 (expect (= ["workspace" "apply"] (extension/slash-path workspace-apply)))
                 (expect (= ["workspace"] (extension/slash-path workspace-spec)))))

(defdescribe
  aggregation-test
  (it "active-slashes aggregates declarative `:ext/slash-commands` across extensions"
      (let [env (env-of [(ext "alpha" [workspace-spec]) (ext "beta" [workspace-apply voice-spec])])]
        (expect (= 3 (count (slash/active-slashes env))))
        (expect (some #(= "workspace" (:slash/name %)) (slash/active-slashes env)))
        (expect (some #(= "apply" (:slash/name %)) (slash/active-slashes env)))
        (expect (some #(= "voice" (:slash/name %)) (slash/active-slashes env)))))
  (it "slash-by-path finds nested commands"
      (let [env (env-of [(ext "alpha" [workspace-spec workspace-apply])])]
        (expect (= "apply" (:slash/name (slash/slash-by-path env ["workspace" "apply"]))))
        (expect (= "workspace" (:slash/name (slash/slash-by-path env ["workspace"]))))
        (expect (nil? (slash/slash-by-path env ["missing"])))
        (expect (nil? (slash/slash-by-path env [])))))
  (it "slash-children filters by parent vec"
      (let [env (env-of [(ext "alpha" [workspace-spec workspace-apply voice-spec])])]
        (expect (= #{"workspace" "voice"} (set (map :slash/name (slash/slash-children env)))))
        (expect (= #{"apply"} (set (map :slash/name (slash/slash-children env ["workspace"]))))))))

(defdescribe parse-test
             (it "parses slash text into path tokens + args (raw, no registry lookup)"
                 (expect
                   (= {:path ["workspace" "apply" "--hard"] :args [] :raw "/workspace apply --hard"}
                      (slash/parse "/workspace apply --hard")))
                 (expect (= {:path ["voice"] :args [] :raw "/voice"} (slash/parse "/voice"))))
             (it "rejects non-slash text"
                 (expect (nil? (slash/parse "hello world")))
                 (expect (nil? (slash/parse "/")))
                 (expect (nil? (slash/parse "")))
                 (expect (nil? (slash/parse nil))))
             (it "rejects absolute file paths (terminal drop shape)"
                 ;; A dropped file pastes `/var/…/shot.png …` — an interior `/` in the
                 ;; first token means PATH, not slash. Regression: this died as
                 ;; `Unknown slash command` before the turn (and its image attachment
                 ;; scan) ever ran.
                 (expect (nil? (slash/parse "/var/folders/T/shot.png what do you see")))
                 (expect (nil? (slash/parse "/Users/x/img.jpg")))
                 (expect (nil? (slash/parse "//weird")))))

(defdescribe
  dispatch-test
  (it "{:handled? false} for non-slash text"
      (let [env (env-of [(ext "alpha" [workspace-spec])])]
        (expect (= {:handled? false} (slash/dispatch env {:channel/id :tui} "hello world")))
        (expect (= {:handled? false} (slash/dispatch env {:channel/id :tui} "/")))))
  (it "{:handled? false} for a dropped absolute file path"
      (let [env (env-of [(ext "alpha" [workspace-spec])])]
        (expect (= {:handled? false}
                   (slash/dispatch env
                                   {:channel/id :tui}
                                   "/var/folders/T/mozDraggedFiles/dog.jpg what do you see")))))
  (it "longest-prefix match resolves nested slash; trailing tokens become argv"
      (let [env
            (env-of [(ext "alpha" [workspace-spec workspace-apply])])

            out
            (slash/dispatch env
                            {:channel/id :tui :session/id "s1" :workspace/id "w1"}
                            "/workspace apply --hard")]

        (expect (true? (:handled? out)))
        (expect (= ["workspace" "apply"] (:path out)))
        (expect (= :ok (get-in out [:result :slash/status])))
        (expect (= ["--hard"] (get-in out [:result :slash/argv])))))
  (it "unknown slash returns :reason :unknown"
      (let [env
            (env-of [(ext "alpha" [workspace-spec])])

            out
            (slash/dispatch env {:channel/id :tui} "/missing thing")]

        (expect (true? (:handled? out)))
        (expect (= :unknown (:reason out)))))
  (it "missing :slash/requires entries surface :reason :requires-failed"
      (let [env
            (env-of [(ext "alpha" [workspace-apply])])

            out
            (slash/dispatch env
                            ;; no :session/id, no :workspace/id
                            {:channel/id :tui}
                            "/workspace apply")]

        (expect (true? (:handled? out)))
        (expect (= :requires-failed (:reason out)))
        (expect (= #{:session :workspace} (:missing out)))))
  (it "availability-fn=false yields :reason :unavailable"
      (let [env
            (env-of [(ext "alpha" [voice-spec])])

            out
            (slash/dispatch env {:channel/id :telegram} "/voice")]

        (expect (true? (:handled? out)))
        (expect (= :unavailable (:reason out)))))
  (it "availability-fn=true allows dispatch"
      (let [env
            (env-of [(ext "alpha" [voice-spec])])

            out
            (slash/dispatch env {:channel/id :tui} "/voice")]

        (expect (true? (:handled? out)))
        (expect (= :ok (get-in out [:result :slash/status])))))
  (it "run-fn throw surfaces :reason :run-failed"
      (let [throwing
            {:slash/name "boom"
             :slash/run-fn (fn [_]
                             (throw (ex-info "kaboom" {})))}

            env
            (env-of [(ext "alpha" [throwing])])

            out
            (slash/dispatch env {:channel/id :tui} "/boom")]

        (expect (true? (:handled? out)))
        (expect (= :run-failed (:reason out)))
        (expect (= "kaboom" (:error out)))))
  (it "slash with no :slash/run-fn surfaces :reason :no-run-fn"
      (let [info-only
            {:slash/name "info" :slash/doc "info"}

            env
            (env-of [(ext "alpha" [info-only])])

            out
            (slash/dispatch env {:channel/id :tui} "/info")]

        (expect (true? (:handled? out)))
        (expect (= :no-run-fn (:reason out))))))
