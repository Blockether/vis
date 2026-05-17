(ns com.blockether.vis.internal.ctx-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx :as ctx]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- fresh []
  (env/create-sci-context nil))

(defn- ns-obj [sci-ctx]
  (sci/find-ns sci-ctx 'sandbox))

(defdescribe build-test
  (it "returns :conversation passthrough + :defs from sandbox state"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx
              "(def hits \"rg results\" {:hit-count 7 :hits []})
               (def hit-count 7)
               (defn hits-fn \"filter\" [pred] (filter pred [1 2 3]))"
              {:ns (ns-obj sci-ctx)})
          conv {:id :c :turn-id :t :iteration-id :i :user-request "q"}
          out  (ctx/build {:environment {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
                           :conversation conv})]
      (expect (= conv (:conversation out)))
      (expect (not (contains? out :tree)))
      (expect (some? (get-in out [:defs 'hits :doc])))
      (expect (= "rg results" (get-in out [:defs 'hits :doc])))
      (expect (some? (get-in out [:defs 'hits :shape])))
      (expect (= :fn (get-in out [:defs 'hits-fn :shape :type])))
      (expect (some? (get-in out [:defs 'hits-fn :shape :arglists])))))

  (it "includes active extension snapshot when supplied"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          ext {:ext/name "test.ctx-extension"
               :ext/description "test extension"
               :ext/kind "testing"
               :ext/activation-fn (constantly true)
               :ext/sci {:ext.sci/alias 't
                         :ext.sci/symbols [{:ext.symbol/symbol 'lookup}
                                           {:ext.symbol/symbol 'patch}]}}
          env {:sci-ctx sci-ctx
               :initial-ns-keys initial-ns-keys
               :extensions (atom [ext])}
          active (prompt/active-extensions env)
          out (ctx/build {:environment env
                          :conversation {:id :c}
                          :extensions (prompt/extensions-snapshot active)})]
      (expect (= [{:name "test.ctx-extension"
                   :alias 't
                   :description "test extension"
                   :kind "testing"
                   :registry-id 't
                   :symbols ['lookup 'patch]}]
                (:extensions out)))))

  (it "renders iteration hints as EDN inside ctx"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          hint {:id :vis.foundation/conversation-title
                :text "set <title> & go"
                :importance :high
                :satisfy-with '(satisfy-hint! :vis.foundation/conversation-title)}
          ctx-map (ctx/build {:environment {:sci-ctx sci-ctx
                                            :initial-ns-keys initial-ns-keys}
                              :conversation {:id :c}
                              :hints [hint]})
          out (ctx/render-iteration-trailer
                {:environment {:sci-ctx sci-ctx}
                 :ctx ctx-map})]
      (expect (= [hint] (:hints ctx-map)))
      (expect (str/includes? out ":hints"))
      (expect (str/includes? out "set <title> & go"))
      (expect (str/includes? out "(satisfy-hint! :vis.foundation/conversation-title)"))
      (expect (not (str/includes? out "<iteration_hints>")))
      (expect (not (str/includes? out "<iteration_hint")))))

  (it "hides engine-owned symbols from :defs"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx
              "(def ctx {:user {:request \"hi\"}}) (def satisfy-hint! :bad) (def my-var 1)"
              {:ns (ns-obj sci-ctx)})
          out (ctx/build {:environment {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
                          :conversation {}})]
      (expect (contains? (:defs out) 'my-var))
      (expect (not (contains? (:defs out) 'ctx)))
      (expect (not (contains? (:defs out) 'satisfy-hint!)))
      (expect (not (contains? (:defs out) '*1)))))

  (it "shape cache reuses schema when value identity is unchanged"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx "(def hits {:hit-count 1})" {:ns (ns-obj sci-ctx)})
          env {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
          a (ctx/build {:environment env :conversation {}})
          b (ctx/build {:environment env :conversation {}})]
      (expect (= (get-in a [:defs 'hits :shape])
                (get-in b [:defs 'hits :shape]))))))
