(ns com.blockether.vis.logging
  "Redirect ALL logging to file with secret redaction.

   Strategy:
   1. Open /dev/tty directly for Lanterna (independent of System/out)
   2. Replace System/out and System/err with redacting file-backed PrintStreams
   3. Rebind Clojure *out*/*err* to log file
   4. Remove telemere console handler, add file handler with redacting output-fn
   5. SLF4J bridge routes through telemere → file (via classpath)

   All secrets (API keys, tokens, passwords) are masked before hitting disk."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.redact :as redact]
            [taoensso.telemere :as t])
  (:import [java.io FileInputStream FileOutputStream]))

(def ^:private log-dir  (str (System/getProperty "user.home") "/.vis"))
(def ^:private log-path (str log-dir "/vis.log"))

;; Open /dev/tty directly for Lanterna — completely independent of System/out.
;; This is the ONLY way to guarantee log output can't corrupt the TUI.
;;
;; Wrapped in `delay` so non-TUI entrypoints (`vis run`, `vis telegram`, web
;; server) that have no controlling TTY don't crash at class-load time. The
;; TUI is the only caller and derefs on startup.
(def tty-in  (delay (FileInputStream.  "/dev/tty")))
(def tty-out (delay (FileOutputStream. "/dev/tty")))

;; Capture original stdout at class-load time, BEFORE any redirects.
;; CLI mode uses this to print results to the terminal after redirecting everything else.
(def original-stdout System/out)

(defn init!
  "Redirect System/out and System/err to log file. Lanterna uses tty-in/tty-out."
  []
  ;; Ensure log directory exists
  (let [dir (io/file log-dir)]
    (when-not (.exists dir) (.mkdirs dir)))

  ;; JVM-level redirect: replace System/out and System/err with redacting file streams.
  ;; Catches EVERYTHING — println, System.out.println, any library output.
  ;; Secrets are masked before hitting disk.
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (redact/redacting-print-stream raw-out)]
    (System/setOut log-stream)
    (System/setErr log-stream))

  ;; Rebind Clojure's *out* and *err* to the log file too
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))

  ;; Telemere: kill console handler, add file handler with redacting output-fn
  (t/remove-handler! :default/console)
  (let [default-fmt (t/format-signal-fn {})]
    (t/add-handler! :file/vis
                    (t/handler:file {:path              log-path
                                     :interval          :monthly
                                     :max-file-size     4000000
                                     :max-num-parts     8
                                     :max-num-intervals 6
                                     :output-fn         (fn [signal] (redact/redact (default-fmt signal)))})))

  ;; JVM shutdown hook to flush telemere handler buffers
  (t/call-on-shutdown! (fn [] (t/stop-handlers!))))

(defn init-cli!
  "Logging init for CLI (non-TUI) commands.
   Redirects ALL output (System.out, System.err, *out*, *err*) to log file.
   Use `original-stdout` for CLI output — it captures the real terminal stream
   before any redirects (saved at class-load time)."
  []
  (let [dir (io/file log-dir)]
    (when-not (.exists dir) (.mkdirs dir)))
  ;; Redirect JVM-level System.out/err → log file
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (redact/redacting-print-stream raw-out)]
    (System/setOut log-stream)
    (System/setErr log-stream))
  ;; Redirect Clojure *out*/*err* → log file (catches trove/telemere console writes)
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  ;; Telemere: kill console handler, add file-only handler
  (t/remove-handler! :default/console)
  (let [default-fmt (t/format-signal-fn {})]
    (t/add-handler! :file/vis
                    (t/handler:file {:path              log-path
                                     :interval          :monthly
                                     :max-file-size     4000000
                                     :max-num-parts     8
                                     :max-num-intervals 6
                                     :output-fn         (fn [signal] (redact/redact (default-fmt signal)))}))))

(defn shutdown!
  "Flush and stop all handlers. Call AFTER screen stops."
  []
  (t/stop-handlers!))
