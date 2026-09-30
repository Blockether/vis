(ns com.blockether.vis.tui.main
  "The terminal client process entry point.

   The terminal application is a gateway CONSUMER, exactly like the companion
   app: it owns no engine, no database, no provider credential and no session
   store. Every fact it paints arrives over HTTP/SSE from a Vis gateway, so this
   entry point does only what a client's front door owes - decide WHICH gateway
   to talk to, hand the session flags to the screen, and turn a user error into
   one line on the real terminal instead of a stack trace.

   Which gateway, in order: `--gateway` / `--gateway-token`, then
   `VIS_GATEWAY_URL` / `VIS_GATEWAY_TOKEN`, then the loopback default with
   `~/.vis/gateway.token`. The local file is used only without an explicit gateway
   or token. A bare `HOST[:PORT]` is accepted and read as `http://`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.voice-recorder :as recorder])
  (:import [java.util ServiceLoader]
           [javax.sound.sampled AudioFormat AudioSystem DataLine$Info SourceDataLine TargetDataLine]
           [javax.sound.sampled.spi MixerProvider])
  (:gen-class))

(def usage
  "vis-agent tui [--gateway HOST[:PORT]] [--gateway-token TOKEN] [--session-id ID | --resume | --continue | --check-audio]")

(def ^:private help-text
  [usage "" "The Vis terminal application. It talks to a Vis gateway over HTTP and SSE;"
   "vis-agent tui automatically manages a local gateway unless --gateway is supplied." ""
   "  --gateway HOST[:PORT]  gateway address (env VIS_GATEWAY_URL)"
   "  --gateway-token TOKEN  gateway token (env VIS_GATEWAY_TOKEN)"
   "                        no gateway/token supplied: use ~/.vis/gateway.token"
   "  --session-id ID        open one existing session"
   "  --resume, -r           pick a session to resume"
   "  --continue, -c         reopen the most recent session"
   "  --check-audio          check audio devices and recorder paths without opening a microphone"
   "  --version, -V          print the version" "  --help, -h             print this help"])

(defn- version
  "This build's release version: the `vis-tui/VERSION` resource written at build
   time from the repo-root VIS_VERSION, verbatim, else `dev` for a checkout."
  []
  (or (some-> (io/resource "vis-tui/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(defn- print-line!
  [^String s]
  (.println ^java.io.PrintStream vis/original-stdout s)
  (.flush ^java.io.PrintStream vis/original-stdout))

(defn- configure-native-audio!
  "Native images have no java.home/conf/sound.properties. Skip that JVM-only
   lookup unless a line provider was already selected explicitly."
  []
  (when (nil? (System/getProperty "java.home"))
    (doseq [key ["javax.sound.sampled.TargetDataLine" "javax.sound.sampled.SourceDataLine"]]
      (when (nil? (System/getProperty key)) (System/setProperty key "")))))

(defn- line-status
  [line-type format]
  (try (let [line (AudioSystem/getLine (DataLine$Info. line-type format))]
         (.close line)
         "available")
       (catch Throwable t (str "unavailable: " (or (ex-message t) (str t))))))

(defn- check-audio!
  []
  (let [format (AudioFormat. 16000.0 16 1 true false)]
    (print-line! (str "providers="
                      (count (iterator-seq (.iterator (ServiceLoader/load MixerProvider))))))
    (print-line! (str "mixers=" (alength (AudioSystem/getMixerInfo))))
    (print-line! (str "target-line=" (line-status TargetDataLine format)))
    (print-line! (str "source-line=" (line-status SourceDataLine format)))
    (when (or (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
              (str/includes? (str/lower-case (System/getProperty "os.name")) "linux"))
      (doseq [{:keys [backend path]} (recorder/external-backends)]
        (print-line! (str (name backend) "=" (or path "unavailable")))))))

(defn- missing-value? [v] (or (nil? v) (str/starts-with? v "--")))

(defn- flag-value
  [flag more]
  (let [v (first more)]
    (when (missing-value? v)
      (throw (ex-info (str flag " requires a value" "\nUsage: " usage) {:vis/user-error true})))
    v))

(defn parse-args
  "Split the command line into this front door's own options and the arguments
   the screen parses itself. Unknown flags are NOT rejected here - the screen
   owns the session vocabulary and its own usage error."
  [args]
  (loop [args
         (seq args)

         opts
         {:screen-args []}]

    (if-not args
      opts
      (let [arg
            (first args)

            more
            (next args)]

        (case arg
          "--gateway"
          (recur (next more) (assoc opts :gateway (flag-value arg more)))

          "--gateway-token"
          (recur (next more) (assoc opts :gateway-token (flag-value arg more)))

          ("--help" "-h" "help")
          (recur more (assoc opts :help true))

          ("--version" "-V" "version")
          (recur more (assoc opts :version true))

          "--check-audio"
          (recur more (assoc opts :check-audio true))

          (recur more (update opts :screen-args conj arg)))))))

(defn -main
  [& args]
  (let [{:keys [gateway gateway-token help screen-args] :as opts}
        (try (parse-args args)
             (catch clojure.lang.ExceptionInfo e
               (print-line! (str "vis-agent tui: " (.getMessage e)))
               (System/exit 2)))]
    (configure-native-audio!)
    (cond help (doseq [line help-text]
                 (print-line! line))
          (:version opts) (print-line! (version))
          (:check-audio opts) (check-audio!)
          :else (do (vis/configure! {:url gateway :token gateway-token})
                    (screen/channel-main screen-args)))))
