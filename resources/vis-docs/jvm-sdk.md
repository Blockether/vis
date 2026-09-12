# Java and Clojure SDK

Use Vis from a JVM application to create sessions, submit tasks and read results.
Java calls the same public Clojure API through `clojure.java.api.Clojure`; there
is not a separate Java-only client package. For a small HTTP client without the
engine's JVM dependencies, consider the [Python SDK](python-sdk.md) instead.

## Choose a gateway connection or an engine process

For a service integration, run a [gateway](gateway-service.md) separately and
connect your application to it. The gateway can be native or JVM-based; its
implementation does not change the client API. Your Java process does not need
GraalVM just to talk to a native gateway.

The public JVM entry point is `com.blockether.vis.core` in the
[`com.blockether:vis` artifact](https://clojars.org/com.blockether/vis).
It includes the engine, not just a lightweight HTTP client. Use a supported
JDK 25 or newer. The examples below explicitly select a remote target, even
when it is on loopback: without that setting the JVM client can discover or
start a local gateway.

To own an entire engine subprocess instead, launch the installed `vis-agent`
wrapper. Python's [`LocalEngine`](python-sdk.md#let-your-program-own-a-private-agent)
already implements that lifecycle and its stdio protocol. Starting the gateway
in your own JVM is an engine-hosting integration, not a replacement for the
fully initialized CLI startup path.

## Add the dependency

Choose a published version on Clojars; `0.2.2` is used below. Keep it compatible
with your gateway. For a Java project, add the Clojars repository and dependency
to your Maven `pom.xml`:

```xml
<repositories>
  <repository>
    <id>clojars</id>
    <url>https://repo.clojars.org</url>
  </repository>
</repositories>

<dependencies>
  <dependency>
    <groupId>com.blockether</groupId>
    <artifactId>vis</artifactId>
    <version>0.2.2</version>
  </dependency>
</dependencies>
```

These are sections of an existing POM, not a complete project file. Clojure
projects can use the same release in `deps.edn`:

```edn
{:deps {com.blockether/vis {:mvn/version "0.2.2"}}}
```

## Connect from Java

Start the gateway and set `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` before
starting the JVM, as shown in the
[Python connection setup](python-sdk.md#connect-to-a-gateway-and-run-a-task).
The JVM client reads these settings itself and uses one process-wide gateway
target. Do not change environment settings per request to switch servers.
The project path must exist on the **gateway machine**. Requests can use its
tools and incur model charges.

Save this as `VisExample.java`. It creates a session, waits for a task and prints
the returned content blocks. It uses the public facade rather than internal
transport namespaces.

```java
// VisExample.java
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import java.util.Map;

public final class VisExample {
    private static IFn api(String name) {
        return Clojure.var("com.blockether.vis.core", name);
    }

    private static IPersistentMap options(String key, String value) {
        return PersistentArrayMap.create(Map.of(Keyword.intern(key), value));
    }

    public static void main(String[] args) {
        String gatewayUrl = System.getenv("VIS_GATEWAY_URL");
        if (gatewayUrl == null || gatewayUrl.isBlank() || args.length != 2) {
            throw new IllegalArgumentException(
                "Set VIS_GATEWAY_URL; pass the gateway project path and request");
        }
        Clojure.var("clojure.core", "require").invoke(
            Clojure.read("com.blockether.vis.core"));
        String sessionId = null;
        try {
            Map<?, ?> session = (Map<?, ?>) api("gateway-create-session!")
                .invoke(options("root", args[0]));
            sessionId = (String) session.get("id");
            System.out.println("Session: " + sessionId);
            Map<?, ?> result = (Map<?, ?>) api("gateway-submit-turn-sync!")
                .invoke(sessionId, options("request", args[1]));
            if (result.get("error") != null) {
                throw new IllegalStateException(result.get("error").toString());
            }
            System.out.println(result.get("content"));
            System.out.println("Status: " + result.get("status"));
        } finally {
            try {
                api("gateway-release-session!").invoke(sessionId);
            } finally {
                Clojure.var("clojure.core", "shutdown-agents").invoke();
            }
        }
    }
}
```

The options use Clojure **keyword** keys; gateway records returned here use
**string** keys. Passing an ordinary Java map with `"root"` as the option key
is not equivalent to passing `:root`. Do not use `Clojure.read` to parse untrusted
requests; this example reads only a fixed namespace symbol.

From the Maven project directory, compile and run it on macOS or Linux:

```bash
mvn -q dependency:build-classpath -Dmdep.outputFile=classpath.txt
javac -cp "$(cat classpath.txt)" VisExample.java
java -cp ".:$(cat classpath.txt)" VisExample \
  /srv/vis-project "Summarize this project without changing files."
```

You should see a session ID and a Clojure representation of the content blocks.
A task can finish with an error, cancellation or a request for human input;
inspect the returned record, not just whether the Java method returned.
The synchronous facade reports a suspended turn as `needs_input`.

`gateway-release-session!` releases runtime resources for that session and the
process's client lease; it does not delete the saved conversation or stop the
gateway. Passing `null` releases only the lease. `shutdown-agents` is appropriate
for this one-shot program; do not call it after every request in a long-lived
JVM application. Likewise, release the shared client lease only when your
application is finished with it, not while other requests are using it.

## Call the same API from Clojure

With the connection environment set before launching `clojure`, the equivalent
core calls are:

```clojure
(require '[com.blockether.vis.core :as vis])

(let [session (vis/gateway-create-session! {:root "/srv/vis-project"})
      sid (get session "id")]
  (try
    (let [result (vis/gateway-submit-turn-sync!
                   sid {:request "Summarize this project without changing files."})]
      (prn (select-keys result ["status" "error" "content"])))
    (finally
      (vis/gateway-release-session! sid))))
```

For a long-lived application, use `gateway-submit-turn!` and
`gateway-get-turn` when you need to manage waiting yourself. The synchronous
submission also accepts `:on-event`, a function called with gateway events.
Keep event handling short and do not log credentials or private tool results.

## Package a JVM application or a native runtime

A normal Java application can keep running on the JVM while the gateway runs as
a prebuilt native executable. This is usually the simplest deployment: update
the service and application independently, and keep credentials out of jars.

Compiling an application that embeds Vis is not just
`native-image -jar application.jar`. Clojure AOT classes, dynamic loading,
reflection, resources, FFM access and the Python runtime all need the engine's
build setup. Use the supported
[native build and bundle workflow](jvm-native-image.md#build-and-test-the-image)
when you want the Vis engine itself compiled ahead of time. A successful
`javac` compilation does not verify native execution.

If your own Java launcher must also be native, keep it a separate process wrapper
around the complete Vis bundle, or provide and test reachability metadata for
your embedding application. The repository's build compiles **Vis**; it does not
automatically compile arbitrary Java applications that depend on it.

## See also

- [Running a gateway](gateway-service.md) — install and supervise the service your application connects to.
- [Python SDK](python-sdk.md) — a client API with an owned-process wrapper and event streams.
- [Building the native binary](jvm-native-image.md) — select GraalVM CE, compile, test and package the engine.
