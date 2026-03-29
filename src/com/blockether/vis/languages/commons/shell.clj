(ns com.blockether.vis.languages.commons.shell
  "Base SHELL tool for RLM agents.
   Executes shell commands with timeout, output-size limits, and
   background process support.  Modelled after Claude Code's Bash tool."
  (:require [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader]
           [java.util.concurrent TimeUnit]))

;;; ── Defaults ──────────────────────────────────────────────────────────

(def ^:private default-timeout-ms
  "Default command timeout (120 seconds)."
  120000)

(def ^:private max-timeout-ms
  "Maximum allowed timeout (10 minutes)."
  600000)

(def ^:private max-output-size
  "Max stdout/stderr to capture per stream (1 MB)."
  (* 1 1024 1024))

;;; ── Background process registry ──────────────────────────────────────

;; Registry of background processes.
;; {pid -> {:process Process :stdout-sb SB :stderr-sb SB :stdout-rdr BR :stderr-rdr BR :command str}}
(defonce ^:private bg-processes (atom {}))

;;; ── Helpers ───────────────────────────────────────────────────────────

(defn- drain
  "Read all available chars from a BufferedReader into a StringBuilder,
   respecting `max-output-size`."
  [^BufferedReader rdr ^StringBuilder sb]
  (let [buf (char-array 8192)]
    (loop []
      (when (and (.ready rdr) (< (.length sb) max-output-size))
        (let [n (.read rdr buf 0 (min (alength buf)
                                      (- max-output-size (.length sb))))]
          (when (pos? n)
            (.append sb buf 0 n)
            (recur)))))))

(defn- read-fully
  "Blocking read of a BufferedReader until EOF, respecting max-output-size."
  [^BufferedReader rdr ^StringBuilder sb]
  (let [buf (char-array 8192)]
    (loop []
      (when (< (.length sb) max-output-size)
        (let [n (.read rdr buf 0 (min (alength buf)
                                      (- max-output-size (.length sb))))]
          (when (pos? n)
            (.append sb buf 0 n)
            (recur)))))))

(defn- process-alive?
  [^Process proc]
  (.isAlive proc))

;;; ── Core: foreground execution ───────────────────────────────────────

(defn shell-exec
  "Execute a shell command and return its output.

   Params:
   - command        — Shell command string (required).  Executed via `sh -c`.
   - timeout        — Timeout in milliseconds (optional, default 120 000, max 600 000).
   - background     — Run in background, return immediately with PID (boolean, optional, default false).

   Foreground returns:
   - :exit-code — Process exit code (integer, -1 on timeout)
   - :stdout    — Captured standard output (string)
   - :stderr    — Captured standard error  (string)
   - :timed-out — true when the process was killed due to timeout

   Background returns:
   - :pid       — Process PID (long) — use with shell-bg-read / shell-bg-kill
   - :command   — The command that was started

   Behaviour notes (mirrors Claude Code's Bash tool):
   - Command is executed via `/bin/sh -c <command>`.
   - stdout and stderr are captured separately.
   - Output is truncated to 1 MB per stream.
   - On timeout the process tree is destroyed."
  ([command] (shell-exec command nil nil))
  ([command timeout] (shell-exec command timeout nil))
  ([command timeout background]
   (when (str/blank? command)
     (throw (ex-info "command must not be empty" {})))
   (let [pb   (doto (ProcessBuilder. ^java.util.List ["sh" "-c" command])
                (.redirectErrorStream false))
         proc (.start pb)
         pid  (.pid proc)]

     (if background
       ;; ── Background: register and return immediately ──────────────
       (let [stdout-sb  (StringBuilder.)
             stderr-sb  (StringBuilder.)
             stdout-rdr (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))
             stderr-rdr (BufferedReader. (InputStreamReader. (.getErrorStream proc) "UTF-8"))]
         ;; Drain streams in background threads so the process doesn't block
         (future (read-fully stdout-rdr stdout-sb))
         (future (read-fully stderr-rdr stderr-sb))
         (swap! bg-processes assoc pid {:process    proc
                                        :stdout-sb  stdout-sb
                                        :stderr-sb  stderr-sb
                                        :stdout-rdr stdout-rdr
                                        :stderr-rdr stderr-rdr
                                        :command    command})
         {:pid pid :command command})

       ;; ── Foreground: wait with timeout ────────────────────────────
       (let [timeout-ms (min max-timeout-ms
                             (max 1 (or timeout default-timeout-ms)))
             stdout-sb  (StringBuilder.)
             stderr-sb  (StringBuilder.)
             stdout-rdr (BufferedReader. (InputStreamReader. (.getInputStream proc) "UTF-8"))
             stderr-rdr (BufferedReader. (InputStreamReader. (.getErrorStream proc) "UTF-8"))
             stdout-fut (future (read-fully stdout-rdr stdout-sb))
             stderr-fut (future (read-fully stderr-rdr stderr-sb))
             finished?  (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
         (if finished?
           (do
             @stdout-fut
             @stderr-fut
             {:exit-code  (.exitValue proc)
              :stdout     (.toString stdout-sb)
              :stderr     (.toString stderr-sb)
              :timed-out  false})
           (do
             (.destroyForcibly proc)
             (drain stdout-rdr stdout-sb)
             (drain stderr-rdr stderr-sb)
             {:exit-code  -1
              :stdout     (.toString stdout-sb)
              :stderr     (.toString stderr-sb)
              :timed-out  true})))))))

;;; ── Core: background process management ──────────────────────────────

(defn shell-bg-read
  "Read current output from a background process.

   Params:
   - pid — Process PID returned by shell-exec with background=true.

   Returns:
   - :pid       — The PID
   - :alive     — Whether the process is still running
   - :exit-code — Exit code (nil if still alive)
   - :stdout    — Captured stdout so far
   - :stderr    — Captured stderr so far"
  [pid]
  (let [entry (get @bg-processes pid)]
    (when-not entry
      (throw (ex-info (str "No background process with PID " pid ". Use shell-exec with background=true first.")
                      {:pid pid})))
    (let [{:keys [^Process process ^StringBuilder stdout-sb ^StringBuilder stderr-sb]} entry
          alive (process-alive? process)]
      {:pid       pid
       :alive     alive
       :exit-code (when-not alive (.exitValue process))
       :stdout    (.toString stdout-sb)
       :stderr    (.toString stderr-sb)})))

(defn shell-bg-kill
  "Kill a background process.

   Params:
   - pid — Process PID to kill.

   Returns:
   - :pid    — The PID
   - :killed — true if the process was alive and killed"
  [pid]
  (let [entry (get @bg-processes pid)]
    (when-not entry
      (throw (ex-info (str "No background process with PID " pid)
                      {:pid pid})))
    (let [{:keys [^Process process]} entry
          was-alive (process-alive? process)]
      (when was-alive
        (.destroyForcibly process))
      (swap! bg-processes dissoc pid)
      {:pid pid :killed was-alive})))

;;; ── Tool definitions ────────────────────────────────────────────────────

(def tool-def
  {:sym 'shell-exec
   :fn  shell-exec
   :doc "Execute a shell command via /bin/sh -c. Returns exit code, stdout, and stderr. Set background=true for long-running commands (servers, watchers) — returns PID immediately."
   :params [{:name "command" :type :string :required true
             :description "Shell command to execute"}
            {:name "timeout" :type :int :required false
             :description "Timeout in milliseconds (default 120000, max 600000). Ignored when background=true."}
            {:name "background" :type :boolean :required false
             :description "Run in background, return PID immediately (default false)"}]
   :returns {:type :map
             :description "Foreground: {:exit-code int :stdout str :stderr str :timed-out bool}. Background: {:pid long :command str}"}})

(def bg-read-tool-def
  {:sym 'shell-bg-read
   :fn  shell-bg-read
   :doc "Read current output from a background process started with shell-exec background=true."
   :params [{:name "pid" :type :int :required true
             :description "Process PID returned by shell-exec"}]
   :returns {:type :map
             :description "{:pid long :alive bool :exit-code int|nil :stdout str :stderr str}"}})

(def bg-kill-tool-def
  {:sym 'shell-bg-kill
   :fn  shell-bg-kill
   :doc "Kill a background process and remove it from the registry."
   :params [{:name "pid" :type :int :required true
             :description "Process PID to kill"}]
   :returns {:type :map
             :description "{:pid long :killed bool}"}})
