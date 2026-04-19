(ns com.blockether.vis.loop.runtime.inactive-tool-stub-test
  "Tests for the per-turn tool activation pass. An inactive tool must
   not be bound in the SCI sandbox at all — the symbol is physically
   removed, so the LLM:
     1. Doesn't see the name in the prompt (handled in runtime.prompt).
     2. Can't call it from SCI — gets `Unable to resolve symbol`.

   The pair is designed so the LLM treats inactive tools as NONEXISTENT
   for this turn, not as \"bound but throwing\"."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [sci.core :as sci]
    [com.blockether.vis.loop.runtime.core :as runtime]
    [com.blockether.vis.loop.tool :as tool-def]))

(defn- make-minimal-env
  "Build a minimal env: SCI ctx + tool-registry-atom. Avoids pulling in
   DB / router / conversation for this focused unit test."
  []
  (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (runtime/create-sci-context (fn [_] {:content "ok"}) nil nil nil)
        registry (atom {})]
    {:sci-ctx sci-ctx
     :sandbox-ns sandbox-ns
     :initial-ns-keys initial-ns-keys
     :tool-registry-atom registry}))

(defn- register-tool!
  [{:keys [tool-registry-atom]} sym handler activation-fn activation-doc]
  (let [canonical (tool-def/make-tool-def sym handler
                    {:doc (str sym " tool")
                     :arglists '[[& args]]
                     :examples [(str "(" sym ")")]
                     :activation-fn activation-fn
                     :activation-doc activation-doc})]
    (swap! tool-registry-atom assoc sym canonical)))

(defn- run-activation-pass!
  "Mirror the binding pass from runtime.query.core/prepare-query-context.
   Kept in sync with that loop — this test exists specifically to pin
   the contract `inactive ⇒ unbound`."
  [{:keys [sci-ctx tool-registry-atom] :as env}]
  (doseq [[sym tool-def] @tool-registry-atom]
    (let [tool-fn       (:fn tool-def)
          activation-fn (:activation-fn tool-def)
          active?       (boolean (activation-fn env))]
      (when tool-fn
        (if active?
          ;; For the test we bind a trivial wrapper rather than pulling in
          ;; the full wrap-tool-for-sci (which needs a full env).
          (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
            (sci/eval-string+ sci-ctx (str "(def " sym " nil)") {:ns ns-obj})
            (sci/intern sci-ctx ns-obj sym tool-fn))
          (swap! (:env sci-ctx) update-in
            [:namespaces 'sandbox] dissoc sym))))))

(defn- sandbox-has-sym?
  "True when `sym` is bound in the sandbox namespace."
  [{:keys [sci-ctx]} sym]
  (contains? (get-in @(:env sci-ctx) [:namespaces 'sandbox]) sym))

(defn- eval-or-error [{:keys [sci-ctx sandbox-ns]} code]
  (try
    {:ok (:val (sci/eval-string+ sci-ctx code {:ns sandbox-ns}))}
    (catch clojure.lang.ExceptionInfo e {:err e})
    (catch Throwable e {:err e})))

(defdescribe inactive-tool-unbound
  (describe "inactive tool becomes literally unresolvable in SCI"
    (it "sandbox does NOT contain the symbol after an inactive pass"
      (let [env (make-minimal-env)]
        (register-tool! env 'fake-search
          (fn fake-search-impl [_q] "never")
          (fn [_env] false)
          "no fake documents ingested")
        (run-activation-pass! env)
        (expect (not (sandbox-has-sym? env 'fake-search)))))

    (it "calling the inactive tool yields SCI's natural unresolved-symbol error"
      (let [env (make-minimal-env)]
        (register-tool! env 'fake-search
          (fn fake-search-impl [_q] "never")
          (fn [_env] false)
          "no fake documents ingested")
        (run-activation-pass! env)
        (let [{:keys [err]} (eval-or-error env "(fake-search \"hello\")")]
          (expect (some? err))
          ;; SCI's message is "Unable to resolve symbol: fake-search" — we
          ;; just check it names the symbol and doesn't mention nil-invoke,
          ;; which is the anti-pattern the dissoc approach exists to avoid.
          (expect (re-find #"fake-search" (ex-message err)))
          (expect (not (re-find #"Cannot invoke" (ex-message err)))))))

    (it "active tool IS bound and callable"
      (let [env (make-minimal-env)]
        (register-tool! env 'always-on
          (fn always-on-impl [x] (inc x))
          (fn [_env] true)
          "always active")
        (run-activation-pass! env)
        (expect (sandbox-has-sym? env 'always-on))
        (let [{:keys [ok]} (eval-or-error env "(always-on 41)")]
          (expect (= 42 ok)))))

    (it "a previously-active tool is REMOVED when it flips inactive next turn"
      ;; Simulates: turn 1 had a document (search-documents active), document
      ;; got deleted, turn 2's activation pass must dissoc the now-stale
      ;; binding instead of leaving it live.
      (let [active-atom (atom true)
            env (make-minimal-env)]
        (register-tool! env 'flip-flop
          (fn flip-flop-impl [] :ok)
          (fn [_env] @active-atom)
          "flip-flop tool")
        ;; Turn 1 — active
        (run-activation-pass! env)
        (expect (sandbox-has-sym? env 'flip-flop))
        ;; Flip to inactive, re-run the pass
        (reset! active-atom false)
        (run-activation-pass! env)
        (expect (not (sandbox-has-sym? env 'flip-flop)))))))
