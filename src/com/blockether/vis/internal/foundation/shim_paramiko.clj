(ns com.blockether.vis.internal.foundation.shim-paramiko
  "Built-in sandbox SHIM: a `paramiko`-compatible SSH2 module backed by the
   pure-Java mwiede JSch fork (`com.github.mwiede/jsch`) so `import paramiko`
   works without the native CPython cryptography/cffi wheels. SSH sessions and
   SFTP channels live HOST-side (JSch `Session`/`ChannelSftp` in integer-keyed
   registries); the Python classes are thin handle wrappers, exchanging
   command/path strings and base64 file bytes across the boundary."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.core :as vis])
  (:import [com.jcraft.jsch ChannelExec ChannelSftp JSch KeyPair Session SftpATTRS]
           [java.io ByteArrayInputStream ByteArrayOutputStream File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64 Properties]
           [org.apache.sshd.common.util.net SshdSocketAddress]
           [org.apache.sshd.common.session SessionListener]
           [org.apache.sshd.server SshServer]
           [org.apache.sshd.server.auth UserAuthNone UserAuthNoneFactory]
           [org.apache.sshd.server.auth.password PasswordAuthenticator UserAuthPasswordFactory]
           [org.apache.sshd.server.forward ForwardingFilter]
           [org.apache.sshd.server.keyprovider SimpleGeneratorHostKeyProvider]
           [org.graalvm.polyglot Value]))

;; Host-side registries: handle (long) -> JSch Session / ChannelSftp.

(defonce ^:private sess-registry (atom {}))

(defonce ^:private sess-counter (atom 0))

(defonce ^:private sftp-registry (atom {}))

(defonce ^:private sftp-counter (atom 0))

;; Host-side registry: handle (long) -> running Apache MINA SSHD server.

(defonce ^:private server-registry (atom {}))

(defonce ^:private server-counter (atom 0))

;; Hard cap on concurrently live MINA servers. Each server self-reaps when its
;; relayed connection ends (see the preamble's `start_server`), so this only
;; bites a pathological guest that opens servers faster than they close; the
;; oldest is stopped to keep the registry — and its threads — bounded.
(def ^:private max-live-servers 32)

;; Each MINA SSHD server uses MINA's OWN default per-server NIO2 io-service
;; factory (its own AsynchronousChannelGroup: acceptor + per-session
;; `nio2-thread-N`). We deliberately do NOT share one `Nio2ServiceFactoryFactory`
;; / executor across servers: MINA 2.15 disposes a shared io-service when the
;; FIRST server `.stop`s, which poisons every later server (its SSH banner is
;; never sent, so clients time out) — only the first server per JVM would ever
;; negotiate. Thread growth is instead bounded by each server's self-reap on
;; connection close (the `SessionListener` below) plus the `max-live-servers`
;; cap, which together keep SSHD threads flat across many start/stop cycles.

(defn- reg-sess!
  [^Session s]
  (let [h (swap! sess-counter inc)]
    (swap! sess-registry assoc h s)
    h))

(defn- sess-of
  ^Session [h]
  (or (get @sess-registry (long h)) (throw (ex-info "SSH session is not active." {}))))

(defn- reg-sftp!
  [^ChannelSftp c]
  (let [h (swap! sftp-counter inc)]
    (swap! sftp-registry assoc h c)
    h))

(defn- sftp-of
  ^ChannelSftp [h]
  (or (get @sftp-registry (long h)) (throw (ex-info "SFTP channel is closed." {}))))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- b64dec ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- non-empty? [x] (and x (seq (str x))))

;; SSH operations (JSch).

(defn- add-default-keys!
  "Mirror paramiko's `look_for_keys`: add the usual ~/.ssh private keys that"
  [^JSch js]
  (let [home (System/getProperty "user.home")]
    (doseq [n ["id_ed25519" "id_ecdsa" "id_rsa" "id_dsa"]]
      (let [f (File. (str home "/.ssh/" n))]
        (when (.exists f) (try (.addIdentity js (.getAbsolutePath f)) (catch Throwable _ nil)))))))

(defn- op-connect
  [opts]
  (let
    [{:strs [hostname port username password key_filename passphrase timeout_ms policy look_for_keys
             compress auth_none]}
     opts

     js
     (JSch.)]

    (when (non-empty? key_filename)
      (if (non-empty? passphrase)
        (.addIdentity js (str key_filename) (str passphrase))
        (.addIdentity js (str key_filename))))
    (when (and (not auth_none)
               (not (non-empty? password))
               (not (non-empty? key_filename))
               (not (false? look_for_keys)))
      (add-default-keys! js))
    (let
      [uname
       (if (non-empty? username) (str username) (System/getProperty "user.name"))

       ^Session sess
       (.getSession js uname (str hostname) (int (or port 22)))

       props
       (Properties.)]

      (when (non-empty? password) (.setPassword sess (str password)))
      (.put props "StrictHostKeyChecking" (if (= policy "reject") "yes" "no"))
      (.put props
            "PreferredAuthentications"
            (if auth_none "none" "publickey,keyboard-interactive,password"))
      (when compress
        (.put props "compression.s2c" "zlib@openssh.com,zlib,none")
        (.put props "compression.c2s" "zlib@openssh.com,zlib,none"))
      (.setConfig sess props)
      (.connect sess (int (or timeout_ms 0)))
      (reg-sess! sess))))

(defn- op-exec
  [conn-h ^String command timeout-ms ^String stdin-b64]
  (let
    [sess
     (sess-of conn-h)

     ^ChannelExec ch
     (.openChannel sess "exec")

     out
     (ByteArrayOutputStream.)

     err
     (ByteArrayOutputStream.)]

    (.setCommand ch command)
    (.setOutputStream ch out)
    (.setErrStream ch err)
    (when (non-empty? stdin-b64) (.setInputStream ch (ByteArrayInputStream. (b64dec stdin-b64))))
    (.connect ch)
    (let
      [deadline (when (and timeout-ms (pos? (long timeout-ms)))
                  (+ (System/currentTimeMillis) (long timeout-ms)))]
      (loop []

        (cond (.isClosed ch) nil
              (and deadline (> (System/currentTimeMillis) (long deadline)))
              (do (.disconnect ch) (throw (ex-info "SSH command timed out." {})))
              :else (do (Thread/sleep 15) (recur)))))
    (let [status (.getExitStatus ch)]
      (.disconnect ch)
      {"stdout" (b64enc (.toByteArray out))
       "stderr" (b64enc (.toByteArray err))
       "exit_status" status})))

(defn- op-ssh-active
  [conn-h]
  (boolean (when-let [^Session s (get @sess-registry (long conn-h))]
             (.isConnected s))))

(defn- op-ssh-close
  [conn-h]
  (when-let [^Session s (get @sess-registry (long conn-h))]
    (.disconnect s)
    (swap! sess-registry dissoc (long conn-h)))
  nil)

;; SFTP operations (JSch ChannelSftp).

(defn- attrs->map
  [filename longname ^SftpATTRS a]
  {"filename" filename
   "longname" longname
   "st_size" (.getSize a)
   "st_mtime" (.getMTime a)
   "st_atime" (.getATime a)
   "st_uid" (.getUId a)
   "st_gid" (.getGId a)
   "st_mode" (.getPermissions a)})

(defn- op-sftp-open
  [conn-h]
  (let
    [sess
     (sess-of conn-h)

     ch
     (.openChannel sess "sftp")]

    (.connect ch)
    (reg-sftp! ch)))

(defn- op-sftp-listdir
  [h ^String path attr?]
  (let [ch (sftp-of h)]
    (vec (keep (fn [^com.jcraft.jsch.ChannelSftp$LsEntry e]
                 (let [nm (.getFilename e)]
                   (when-not (or (= nm ".") (= nm ".."))
                     (if attr? (attrs->map nm (.getLongname e) (.getAttrs e)) nm))))
               (.ls ch path)))))

(defn- op-sftp-stat
  [h ^String path follow?]
  (let
    [ch
     (sftp-of h)

     a
     (if follow? (.stat ch path) (.lstat ch path))]

    (attrs->map path nil a)))

(defn- op-sftp-get
  [h ^String remote]
  (let
    [ch
     (sftp-of h)

     bos
     (ByteArrayOutputStream.)]

    (with-open [is (.get ch remote)]
      (io/copy is bos))
    (b64enc (.toByteArray bos))))

(defn- op-sftp-put
  [h ^String remote ^String b64]
  (let [ch (sftp-of h)]
    (.put ch (ByteArrayInputStream. (b64dec b64)) remote)
    (attrs->map remote nil (.stat ch remote))))

(defn- op-sftp-mkdir
  [h ^String path mode]
  (let [ch (sftp-of h)]
    (.mkdir ch path)
    (when mode (.chmod ch (int mode) path))
    nil))

(defn- op-sftp-rmdir [h ^String path] (.rmdir (sftp-of h) path) nil)

(defn- op-sftp-remove [h ^String path] (.rm (sftp-of h) path) nil)

(defn- op-sftp-rename
  [h ^String old-path ^String new-path _posix?]
  (.rename (sftp-of h) old-path new-path)
  nil)

(defn- op-sftp-chmod [h ^String path mode] (.chmod (sftp-of h) (int mode) path) nil)

(defn- op-sftp-symlink [h ^String src ^String dst] (.symlink (sftp-of h) src dst) nil)

(defn- op-sftp-pwd [h] (.pwd (sftp-of h)))

(defn- op-sftp-close
  [h]
  (when-let [^ChannelSftp ch (get @sftp-registry (long h))]
    (.disconnect ch)
    (swap! sftp-registry dissoc (long h)))
  nil)

(defn- ssh-envelope
  "Run thunk `f`, returning [true result] on success or [false message] on any
   Throwable, so the Python shim can raise a catchable `paramiko.SSHException`."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- key-type
  [kind]
  (case (str kind)
    "rsa"
    KeyPair/RSA

    "dss"
    KeyPair/DSA

    "ecdsa"
    KeyPair/ECDSA

    "ed25519"
    KeyPair/ED25519

    "ed448"
    KeyPair/ED448

    KeyPair/RSA))

(defn- default-key-bits
  [kt]
  (cond (= kt KeyPair/RSA) 2048
        (= kt KeyPair/DSA) 1024
        (= kt KeyPair/ECDSA) 256
        :else 0))

(defn- passphrase-bytes
  ^bytes [passphrase]
  (when (non-empty? passphrase) (.getBytes (str passphrase) "UTF-8")))

(defn- keypair->map
  [^KeyPair kp private-b64]
  {"name" (.getKeyTypeString kp)
   "bits" (.getKeySize kp)
   "fingerprint" (.getFingerPrint kp)
   "public" (b64enc (.getPublicKeyBlob kp))
   "private" private-b64})

(defn- op-key-generate
  [kind bits passphrase]
  (let
    [kt
     (key-type kind)

     size
     (int (or bits (default-key-bits kt)))

     ^KeyPair kp
     (if (zero? size) (KeyPair/genKeyPair (JSch.) kt) (KeyPair/genKeyPair (JSch.) kt size))]

    (try (let
           [out
            (ByteArrayOutputStream.)

            passb
            (passphrase-bytes passphrase)]

           (if (or (= kt KeyPair/ED25519) (= kt KeyPair/ED448))
             (.writeOpenSSHv1PrivateKey kp out passb)
             (if passb (.writePrivateKey kp out passb) (.writePrivateKey kp out)))
           (keypair->map kp (b64enc (.toByteArray out))))
         (finally (.dispose kp)))))

(defn- op-key-load
  [private-b64 passphrase]
  (let [^KeyPair kp (KeyPair/load (JSch.) (b64dec private-b64) nil)]
    (try (when (.isEncrypted kp)
           (let [passb (passphrase-bytes passphrase)]
             (when-not passb (throw (ex-info "Private key is encrypted; passphrase required." {})))
             (when-not (.decrypt kp passb)
               (throw (ex-info "Private key passphrase was rejected." {})))))
         (keypair->map kp private-b64)
         (finally (.dispose kp)))))

(defn- reg-server!
  [entry]
  ;; Keep the live-server set bounded: if a guest leaks servers faster than they
  ;; self-reap, stop the oldest so MINA instances (and their threads) can't grow
  ;; without limit.
  (let [snapshot @server-registry]
    (when (>= (count snapshot) (long max-live-servers))
      (let [oldest (first (sort (keys snapshot)))]
        (when-let [e (get snapshot oldest)]
          (try (.stop ^SshServer (:server e) true) (catch Throwable _ nil))
          (swap! server-registry dissoc oldest)))))
  (let [h (swap! server-counter inc)]
    (swap! server-registry assoc h entry)
    h))

(defn- guest->clj
  "Coerce a polyglot return `Value` (or a plain value) to a Clojure scalar."
  [r]
  (if (instance? Value r)
    (let [^Value v r]
      (cond (.isNull v) nil
            (.isBoolean v) (.asBoolean v)
            (.isNumber v) (.asInt v)
            (.isString v) (.asString v)
            :else (.as v Object)))
    r))

(defn- guest-call
  "Invoke guest callable `f` (a polyglot `Value`) or a Clojure IFn with `args`,
   coercing the result to a Clojure scalar. Safe from a non-guest (MINA acceptor)
   thread: the session Context is built with `allowCreateThread` and GraalPy
   permits concurrent access (see `env-python`)."
  [f args]
  (guest->clj (cond (instance? Value f) (.execute ^Value f (object-array args))
                    (ifn? f) (apply f args)
                    :else nil)))

(declare op-server-stop)

(defn- none-auth-factory
  "A MINA `none` user-auth factory that delegates to the guest `ServerInterface`'s
   `check_auth_none` (a paramiko AUTH_* int; 0 == AUTH_SUCCESSFUL). Without it MINA
   never offers `none`, so a server that advertises it through `get_allowed_auths`
   still rejects the client. The guest decides: a stock `ServerInterface` answers
   AUTH_FAILED, so nothing authenticates by default."
  [auth-none-fn]
  (proxy [UserAuthNoneFactory] []
    (createUserAuth [_session]
      (proxy [UserAuthNone] []
        (doAuth [_buffer init]
          (Boolean/valueOf
            (boolean (and init
                          (try (= 0 (guest-call auth-none-fn [(.getUsername ^UserAuthNone this)]))
                               (catch Throwable _ false))))))))))

(defn- op-server-start
  "Start an Apache MINA SSHD server on an ephemeral loopback port; returns
   `{\"handle\" H \"port\" P}`. The paramiko shim's `Transport.start_server` relays
   the pre-accepted client socket to `127.0.0.1:P`, so MINA terminates SSH while
   auth and reverse-forward decisions delegate to the guest `ServerInterface`:
   `auth-pw-fn` / `auth-none-fn` (-> paramiko AUTH_* int; 0 == success) and
   `forward-fn` (-> truthy to allow a `tcpip-forward` request). Only `none` and
   `password` are offered, and `none` succeeds only when the guest's
   `check_auth_none` says so. A fresh host key is generated per server.

   The server self-reaps HOST-side via a `SessionListener`: the instant its (single)
   relayed SSH session closes it is stopped and deregistered, so its acceptor and
   `-timer-thread` never outlive the connection. This does NOT depend on the guest
   Python `_reap` daemon — GraalPy cancels that thread when the context closes
   (logging \"Could not stop thread\"), which used to strand the MINA server and leak
   its threads (and grow `server-registry` unbounded across turns/sessions)."
  [auth-pw-fn forward-fn auth-none-fn]
  (let
    [hostkey
     (.resolve (Files/createTempDirectory "vis-sshd-hostkey" (make-array FileAttribute 0))
               "hostkey.ser")

     server
     (doto (SshServer/setUpDefaultServer)
       (.setHost "127.0.0.1")
       (.setPort 0)
       (.setKeyPairProvider (SimpleGeneratorHostKeyProvider. hostkey))
       (.setUserAuthFactories [(none-auth-factory auth-none-fn) UserAuthPasswordFactory/INSTANCE])
       (.setPasswordAuthenticator (reify
                                    PasswordAuthenticator
                                      (authenticate [_ u p _session]
                                        (try (= 0 (guest-call auth-pw-fn [u p]))
                                             (catch Throwable _ false)))))
       (.setForwardingFilter (reify
                               ForwardingFilter
                                 (canForwardAgent [_ _session _request-type] false)
                                 (canForwardX11 [_ _session _request-type] false)
                                 (canListen [_ address _session]
                                   (try (boolean (guest-call
                                                   forward-fn
                                                   [(.getHostName ^SshdSocketAddress address)
                                                    (.getPort ^SshdSocketAddress address)]))
                                        (catch Throwable _ false)))
                                 (canConnect [_ _type _address _session] true))))

     h
     (reg-server! {:server server :hostkey hostkey})]

    ;; Reap the instant the relayed SSH session ends, on a fresh daemon thread so
    ;; the stop never re-enters MINA's own session-close callback.
    (.addSessionListener server
                         (reify
                           SessionListener
                             (sessionClosed [_ _sess]
                               (doto (Thread. ^Runnable
                                              (fn []
                                                (op-server-stop h))
                                              "vis-sshd-reap")
                                 (.setDaemon true)
                                 (.start)))))
    (.start server)
    {"handle" h "port" (.getPort server)}))

(defn- op-server-stop
  "Stop and deregister the MINA server bound to handle `h`; returns nil."
  [h]
  (when-let [entry (get @server-registry (long h))]
    (try (.stop ^SshServer (:server entry) true) (catch Throwable _ nil))
    (swap! server-registry dissoc (long h)))
  nil)

(defn- paramiko-bridge-bindings
  "Host callables (pure-Java JSch) the paramiko shim delegates to."
  []
  {"__vis_ssh_connect__" (fn [opts]
                           (ssh-envelope #(op-connect opts)))
   "__vis_ssh_exec__" (fn [h cmd tmo stdin]
                        (ssh-envelope #(op-exec h cmd tmo stdin)))
   "__vis_ssh_active__" (fn [h]
                          (ssh-envelope #(op-ssh-active h)))
   "__vis_ssh_close__" (fn [h]
                         (ssh-envelope #(op-ssh-close h)))
   "__vis_sftp_open__" (fn [h]
                         (ssh-envelope #(op-sftp-open h)))
   "__vis_sftp_listdir__" (fn [h path attr?]
                            (ssh-envelope #(op-sftp-listdir h path attr?)))
   "__vis_sftp_stat__" (fn [h path follow?]
                         (ssh-envelope #(op-sftp-stat h path follow?)))
   "__vis_sftp_get__" (fn [h path]
                        (ssh-envelope #(op-sftp-get h path)))
   "__vis_sftp_put__" (fn [h path b64]
                        (ssh-envelope #(op-sftp-put h path b64)))
   "__vis_sftp_mkdir__" (fn [h path mode]
                          (ssh-envelope #(op-sftp-mkdir h path mode)))
   "__vis_sftp_rmdir__" (fn [h path]
                          (ssh-envelope #(op-sftp-rmdir h path)))
   "__vis_sftp_remove__" (fn [h path]
                           (ssh-envelope #(op-sftp-remove h path)))
   "__vis_sftp_rename__" (fn [h a b posix?]
                           (ssh-envelope #(op-sftp-rename h a b posix?)))
   "__vis_sftp_chmod__" (fn [h path mode]
                          (ssh-envelope #(op-sftp-chmod h path mode)))
   "__vis_sftp_symlink__" (fn [h a b]
                            (ssh-envelope #(op-sftp-symlink h a b)))
   "__vis_sftp_pwd__" (fn [h]
                        (ssh-envelope #(op-sftp-pwd h)))
   "__vis_sftp_close__" (fn [h]
                          (ssh-envelope #(op-sftp-close h)))
   "__vis_key_generate__" (fn [kind bits passphrase]
                            (ssh-envelope #(op-key-generate kind bits passphrase)))
   "__vis_key_load__" (fn [private-b64 passphrase]
                        (ssh-envelope #(op-key-load private-b64 passphrase)))
   "__vis_server_start__" (fn [auth-pw forward auth-none]
                            (ssh-envelope #(op-server-start auth-pw forward auth-none)))
   "__vis_server_stop__" (fn [h]
                           (ssh-envelope #(op-server-stop h)))})

;; Python preamble: publishes a paramiko-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-paramiko"
     :ext/description
     "Sandbox Paramiko-compatible SSH2 over pure-Java JSch: SSHClient exec/SFTP, RSA/DSS/ECDSA/Ed25519 keys, Transport, and server APIs/constants. Real-socket `start_server` uses Apache MINA SSHD for reverse `tcpip-forward` with ServerInterface auth/approval; no `invoke_shell`. Works without cryptography/cffi, pip, wheel, or host binary."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "paramiko"
       :shim/imports ["paramiko"]
       :shim/description
       "Paramiko-compatible SSH2 via pure-Java JSch: SSHClient exec/SFTP, RSA/DSS/ECDSA/Ed25519 keys, and server APIs/constants. `Transport(real_socket).start_server()` runs Apache MINA SSHD for reverse `tcpip-forward`, delegating `none`/password auth and approval to `ServerInterface` (`check_auth_none`/`check_auth_password`/`check_port_forward_request`); `Transport.auth_none` authenticates against a server that accepts it. Import and socket-less `start_server()` start nothing; live servers cap at 32 and self-reap. Not supported: `invoke_shell`; use `exec_command`/SFTP."
       :shim/bindings paramiko-bridge-bindings
       :shim/source "vis-shims/paramiko.py"}]}))

(vis/register-extension! vis-extension)
