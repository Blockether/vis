# Java and Clojure SDK

Create a session on a running gateway, submit a task and read the answer from a
Java or Clojure application. The gateway runs the tools and owns the project
files; your JVM application acts as its client.

## Prepare the JVM classpath

Use a Vis source checkout, the Clojure CLI and the repository's pinned JDK.
Java calls the public `com.blockether.vis.core` namespace through
`clojure.java.api.Clojure`; there is no standalone Java-only SDK.

**Published artifact limitation:** `com.blockether:vis:0.2.2` does not provide a
complete Maven runtime classpath. Loading the API fails with a missing
`com.blockether.vis-python-runtime` namespace because that dependency is supplied
through Git in the source build. Use the prepared checkout below rather than
adding only the published Vis JAR to your application.

From the Vis repository root, select its toolchain and prepare Git dependencies:

```bash
eval "$(bin/require-graalvm --export)"
clojure -X:deps prep
export VIS_CLASSPATH="$(clojure -Spath)"
```

Keep this working directory while running the examples: the classpath includes
relative source and resource paths. The client loads the engine's JVM libraries
even though the gateway performs the actual tasks. For a separately installable
client package, use the [Python SDK](python-sdk.md).

## Connect from Java

[Start a gateway](gateway-service.md#start-a-local-gateway) with a configured
provider. For a local gateway using its default state directory, set these in
your private terminal before launching Java:

```bash
export VIS_GATEWAY_URL=http://127.0.0.1:7890
export VIS_GATEWAY_TOKEN="$(cat "$HOME/.vis/gateway.token")"
export VIS_PROJECT_ROOT="$PWD"
```

Do not print or commit the token. For a remote connection, use its HTTPS origin,
a securely supplied token and a project path **on the gateway machine**. See
[remote access](gateway-service.md#connect-from-another-machine).
Requests can use that machine's tools and incur model charges.

The JVM client reads these settings itself and uses one process-wide gateway
target. Without an explicit URL it can discover or start a local gateway; this
example requires the URL to avoid that side effect. Do not switch targets by
changing environment settings per request.

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
            Map<?, ?> turn = (Map<?, ?>) api("gateway-get-turn")
                .invoke(sessionId, result.get("session_turn_id"));
            System.out.println("Status: " + turn.get("status"));
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

Save `VisExample.java` in that checkout, then compile and run it on macOS or Linux:

```bash
javac -cp "$VIS_CLASSPATH" VisExample.java
java -cp ".:$VIS_CLASSPATH" VisExample \
  "$VIS_PROJECT_ROOT" "Summarize this project without changing files."
```

You should see a session ID, the content blocks and `Status: completed`.
The example reads the canonical turn record for its status. The synchronous
result is different: it has no `status` on success and uses `needs_input` for a
suspended turn. A returned Java method alone does not establish task success.

`gateway-release-session!` releases runtime resources for that session and the
process's client lease; it does not delete the saved conversation or stop the
gateway. Passing `null` releases only the lease. `shutdown-agents` is appropriate
for this one-shot program; do not call it after every request in a long-lived
JVM application. Likewise, release the shared client lease only when your
application is finished with it, not while other requests are using it.

## Call the same API from Clojure

With the same prepared checkout and connection environment, save this as
`task.clj` in the repository root and run `clojure -M task.clj`:

```clojure
(require '[com.blockether.vis.core :as vis])

(try
  (let [session (vis/gateway-create-session! {:root (System/getenv "VIS_PROJECT_ROOT")})
        sid (get session "id")]
    (try
      (let [result (vis/gateway-submit-turn-sync!
                     sid {:request "Summarize this project without changing files."})
            turn (vis/gateway-get-turn sid (get result "session_turn_id"))]
        (prn (select-keys turn ["status" "content"])))
      (finally
        (vis/gateway-release-session! sid))))
  (finally
    (shutdown-agents)))
```

For a long-lived application, use `gateway-submit-turn!` and
`gateway-get-turn` when you need to manage waiting yourself. The synchronous
submission also accepts `:on-event`, a function called with gateway events.
Keep event handling short and do not log credentials or private tool results.

## Package a JVM application or a native runtime

Your Java application can run on the JVM while the gateway runs as a prebuilt
native executable. Java does not need GraalVM to connect to a native gateway.
Update the service and application independently and keep credentials out of jars.
For a private child process rather than a shared service, the Python SDK's
[`Agent` and `LocalEngine`](python-sdk.md#handle-failures-and-choose-a-lifecycle)
already implement process ownership and the stdio protocol.

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
