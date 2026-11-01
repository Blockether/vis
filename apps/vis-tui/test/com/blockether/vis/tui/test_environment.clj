(ns com.blockether.vis.tui.test-environment
  "Suite-wide wall between the TUI tests and the developer's own gateway.

   The client dials the loopback gateway with `~/.vis/gateway.token` by default.
   On a machine that runs Vis, the suite then reads that gateway's real provider
   fleet, so footer and screen tests that pass in CI fail locally. CI has no
   gateway; this hook gives every run that same state.

   Screens that the tests paint start background gateway reads, and each read fails
   on the closed port. The hook sends diagnostics to the log file, as the TUI does
   at startup, so the test report shows only test results."
  (:require [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.paths :as paths]
            [lazytest.hooks :as hooks]
            [taoensso.telemere :as tel])
  (:import [java.net InetAddress ServerSocket]))

(defn closed-loopback-url
  "A loopback gateway URL where nothing listens, so each request fails at once."
  []
  (with-open [socket (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
    (str "http://127.0.0.1:" (.getLocalPort socket))))

(defonce ^:private installed (atom false))

(defn install!
  "Idempotently point the client at a closed loopback port, without a token, and
   send diagnostics to the log file under `vis.logs.dir` instead of the console."
  []
  (when (compare-and-set! installed false true)
    (tel/remove-handler! :default/console)
    (tel/add-handler! ::suite-log (tel/handler:file {:path (paths/log-file)}) {:min-level :info})
    (client/configure! {:url (closed-loopback-url)}))
  true)

(hooks/defhook no-live-gateway
               "Run the suite without the developer's live gateway."
               (pre-test-run [_config m] (install!) m))
