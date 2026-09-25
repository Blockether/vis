(ns com.blockether.vis.internal.gateway.wiring
  "The composition root: fills every lower-layer slot that calls a higher layer.

   A lower layer that needs a higher layer's function exposes a slot
   (`install-*!`, `set-*-fn!`, `set-*-hook!`) instead of requiring upward or
   resolving at runtime. Only this namespace fills those slots, once per process:
   the binary entry, the gateway server and the stdio engine call `install!`
   before they serve. Tests that exercise a wired path call it too.

   Slots receive Vars, so reloading a namespace during development keeps the
   wiring live."
  (:require [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.python.test-runner :as test-runner]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.view.core :as view]))

(defonce ^:private installed
  (delay
    ;; Council reads presence from the gateway registry and wakes idle targets
    ;; through the gateway's turn submission.
    (council/install-runtime! #'state/council-runtime)
    (council/install-waker! #'state/council-wake-eligible? #'state/council-wake!)
    (agents/install-runtime! state/agent-runtime)
    ;; A live View a human stops after its block returned has no collector left;
    ;; the gateway owns the database, so it files the late artifact.
    (view/set-late-artifact-filer! #'state/append-iteration-attachment!)
    ;; Provider and default-model changes rebuild the shared router and reseed
    ;; every cached session environment, not only `/reload`.
    (providers/set-router-rebuild-hook! #'loop-env/reload-router!)
    (python-env/install-extension-hooks! {:net-probe-report #'python-extensions/net-probe-report
                                          :close-session-contexts!
                                          #'python-extensions/close-session-contexts!})
    (python-extensions/install-test-slash! #'test-runner/test-slash)
    ;; Events tailed from sibling processes reach this registry, and the tailer
    ;; drains only the journals of sessions this process tracks.
    (bus/set-deliver-fn! #'state/ingest-mirrored-event!)
    (bus/set-relevant-sid-fn! #'state/session-known?)
    (bus/set-relevant-sids-fn! #'state/tracked-session-ids)
    ;; A native image starts the tailer on its first publish instead; see `bus/publish!`.
    (when-not (System/getProperty "org.graalvm.nativeimage.imagecode") (bus/start!))
    true))

(defn install! "Fill every slot once. Idempotent; returns nil." [] @installed nil)
