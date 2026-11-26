(ns com.blockether.vis.internal.gateway.wiring-test
  (:require [com.blockether.vis.internal.automation.runner :as automation-runner]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.foundation.mcp.sampling :as sampling]
            [com.blockether.vis.internal.gateway.journal :as journal]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.view :as gateway-view]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.router :as router]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.python.test-runner :as test-runner]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- slot [v] @(var-get v))

(defdescribe
  install-test
  (it "fills every lower-layer slot with the higher layer's Var"
      (wiring/install!)
      ;; Vars, not functions: a namespace reload during development keeps the
      ;; wiring live instead of calling the definition from the first load.
      (expect (identical? #'loop-env/reload-router! (providers/router-rebuild-hook-val)))
      (expect (identical? #'state/council-runtime (slot #'council/runtime-reader)))
      (expect (= {:eligible? #'state/council-wake-eligible? :wake! #'state/council-wake!}
                 (slot #'council/runtime-waker)))
      (expect (= state/agent-runtime (slot #'agents/runtime)))
      (expect (identical? #'state/append-iteration-attachment! (slot #'view/late-artifact-filer)))
      (expect (identical? #'wiring/may-ask? (slot #'view/ask-gate)))
      (expect (= {:net-probe-report #'python-extensions/net-probe-report
                  :close-session-contexts! #'python-extensions/close-session-contexts!}
                 (slot #'python-env/extension-hooks)))
      (expect (identical? #'test-runner/test-slash (slot #'python-extensions/test-slash-fn)))
      (expect (identical? #'state/ingest-mirrored-event! (slot #'journal/deliver-fn)))
      (expect (identical? #'state/session-known? (slot #'journal/relevant-sid-fn)))
      (expect (identical? #'journal/block-started-ms (slot #'introspection/block-clock)))
      (expect (identical? #'router/llm-text! (slot #'sampling/model)))
      (expect (identical? #'state/tracked-session-ids (slot #'journal/relevant-sids-fn)))
      ;; A persisted CLI run has no HTTP server, but its Tasks still reach the journal.
      (expect (identical? #'state/publish-task! (slot #'task/publisher))))
  (it "installs once, so a second call leaves the slots as they are"
      (wiring/install!)
      (let [waker (slot #'council/runtime-waker)]
        (wiring/install!)
        (expect (identical? waker (slot #'council/runtime-waker))))))

(defdescribe ask-gate-test
             (it "refuses an extension automation run and a subagent of it, where nobody answers"
                 (with-redefs [automation-runner/refuse-ask!
                               #(= "run" (str %))

                               gateway-view/input-home
                               #(if (= "child-of-run" (str %)) "run" (str %))]

                   (expect (false? (#'wiring/may-ask? "run")))
                   (expect (false? (#'wiring/may-ask? "child-of-run")))
                   (expect (true? (#'wiring/may-ask? "child-of-person"))))))
