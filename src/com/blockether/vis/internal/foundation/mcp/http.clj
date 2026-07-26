(ns com.blockether.vis.internal.foundation.mcp.http
  "One JDK `java.net.http.HttpClient` shared by every MCP subsystem
   (request/response, OAuth discovery + token exchange, SSE listen loop).

   Rationale — perf + threading:
     * The JDK client owns a small internal selector pool AND an executor used
       to run response-body pipelines and `sendAsync` completions. Two client
       instances = two selector pools = wasted daemon threads for zero win.
       So we share ONE instance across `client.clj` + `oauth.clj`.
     * The executor is a `newVirtualThreadPerTaskExecutor` — on Java 25 virtual
       threads no longer pin on `synchronized` (JEP 491), so blocking-`.send`
       calls made from a virtual thread never park a carrier. Bounded footprint,
       no cached thread-pool growth under bursty MCP load.
     * `defonce` + `delay` = constructed lazily at runtime, never at
       namespace-load; safe for GraalVM native-image (the client owns non-heap
       state that can't land in the build image)."
  (:import (java.net.http HttpClient)
           (java.time Duration)
           (java.util.concurrent Executors)))

(def ^:private connect-timeout-ms 15000)

(defonce client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofMillis (long connect-timeout-ms)))
             (.executor (Executors/newVirtualThreadPerTaskExecutor))
             (.build))))
