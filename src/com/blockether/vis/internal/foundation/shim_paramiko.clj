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
           [org.apache.sshd.common Factory]
           [org.apache.sshd.common.io.nio2 Nio2ServiceFactoryFactory]
           [org.apache.sshd.common.util.threads ThreadUtils]
           [org.apache.sshd.server SshServer]
           [org.apache.sshd.server.auth.password PasswordAuthenticator]
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

;; ONE shared NIO2 pool for EVERY MINA SSHD server. Otherwise each
;; `SshServer/setUpDefaultServer` builds its OWN AsynchronousChannelGroup
;; (acceptor + per-session `nio2-thread-N`), so N servers => N thread pools —
;; the exact source of the runaway thread/RAM growth. A single shared, ELASTIC
;; cached pool lets threads be reused across servers (and reaped when idle)
;; instead of every server hoarding its own; combined with each server's
;; self-reap on connection close, SSHD threads stay flat. A cached (not fixed)
;; pool is what `AsynchronousChannelGroup` wants — a small fixed pool can
;; deadlock a single handshake. `protectExecutorServiceShutdown` stops a
;; server's own `.stop` from tearing the shared pool down (shut down on JVM exit).
(defonce ^:private sshd-io-factory
  (delay (let
           [pool (ThreadUtils/protectExecutorServiceShutdown
                   (ThreadUtils/newCachedThreadPool "vis-sshd-nio2")
                   true)]
           (Nio2ServiceFactoryFactory. (reify
                                         Factory
                                           (create [_] pool)
                                           (get [_] pool))))))

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
             compress]}
     opts

     js
     (JSch.)]

    (when (non-empty? key_filename)
      (if (non-empty? passphrase)
        (.addIdentity js (str key_filename) (str passphrase))
        (.addIdentity js (str key_filename))))
    (when (and (not (non-empty? password))
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
      (.put props "PreferredAuthentications" "publickey,keyboard-interactive,password")
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
    (cond (.isNull r) nil
          (.isBoolean r) (.asBoolean r)
          (.isNumber r) (.asInt r)
          (.isString r) (.asString r)
          :else (.as r Object))
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

(defn- op-server-start
  "Start an Apache MINA SSHD server on an ephemeral loopback port; returns
   `{\"handle\" H \"port\" P}`. The paramiko shim's `Transport.start_server` relays
   the pre-accepted client socket to `127.0.0.1:P`, so MINA terminates SSH while
   auth and reverse-forward decisions delegate to the guest `ServerInterface`:
   `auth-pw-fn` (-> paramiko AUTH_* int; 0 == success) and `forward-fn` (-> truthy
   to allow a `tcpip-forward` request). A fresh host key is generated per server."
  [auth-pw-fn forward-fn]
  (let
    [hostkey
     (.resolve (Files/createTempDirectory "vis-sshd-hostkey" (make-array FileAttribute 0))
               "hostkey.ser")

     server
     (doto (SshServer/setUpDefaultServer)
       (.setIoServiceFactoryFactory @sshd-io-factory)
       (.setHost "127.0.0.1")
       (.setPort 0)
       (.setKeyPairProvider (SimpleGeneratorHostKeyProvider. hostkey))
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
                                 (canConnect [_ _type _address _session] true))))]

    (.start server)
    {"handle" (reg-server! {:server server :hostkey hostkey}) "port" (.getPort server)}))

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
   "__vis_server_start__" (fn [auth-pw forward]
                            (ssh-envelope #(op-server-start auth-pw forward)))
   "__vis_server_stop__" (fn [h]
                           (ssh-envelope #(op-server-stop h)))})

;; Python preamble: publishes a paramiko-compatible module into sys.modules.

(def ^:private paramiko-shim-src
  "def __vis_install_paramiko__():
    import sys, types, base64, hashlib, os
    _bi = sys.modules['builtins']
    _connect = __vis_ssh_connect__
    _exec = __vis_ssh_exec__
    _active = __vis_ssh_active__
    _close = __vis_ssh_close__
    _sftp_open = __vis_sftp_open__
    _sftp_list = __vis_sftp_listdir__
    _sftp_stat = __vis_sftp_stat__
    _sftp_get = __vis_sftp_get__
    _sftp_put = __vis_sftp_put__
    _sftp_mkdir = __vis_sftp_mkdir__
    _sftp_rmdir = __vis_sftp_rmdir__
    _sftp_remove = __vis_sftp_remove__
    _sftp_rename = __vis_sftp_rename__
    _sftp_chmod = __vis_sftp_chmod__
    _sftp_symlink = __vis_sftp_symlink__
    _sftp_pwd = __vis_sftp_pwd__
    _sftp_close = __vis_sftp_close__
    _key_generate = __vis_key_generate__
    _key_load = __vis_key_load__
    _server_start = __vis_server_start__
    _server_stop = __vis_server_stop__
    _NLB = bytes([10])

    def _b64d(s):
        if s is None:
            return b''
        return base64.b64decode(s)

    def _b64e(b):
        return base64.b64encode(bytes(b)).decode('ascii')

    class SSHException(Exception):
        pass

    class AuthenticationException(SSHException):
        pass

    class PasswordRequiredException(AuthenticationException):
        pass

    class BadAuthenticationType(AuthenticationException):
        def __init__(self, explanation='', types=None):
            super().__init__(explanation)
            self.allowed_types = types or []

    class BadHostKeyException(SSHException):
        def __init__(self, hostname=None, got_key=None, expected_key=None):
            super().__init__('Host key for server ' + str(hostname) + ' does not match.')
            self.hostname = hostname
            self.key = got_key
            self.expected_key = expected_key

    class ChannelException(SSHException):
        def __init__(self, code=0, text=''):
            super().__init__(text)
            self.code = code

    class ProxyCommandFailure(SSHException):
        pass

    class ConfigParseError(SSHException):
        pass

    class NoValidConnectionsError(SSHException):
        def __init__(self, errors=None):
            if isinstance(errors, dict):
                super().__init__('Unable to connect to port')
                self.errors = errors
            else:
                super().__init__(str(errors))
                self.errors = {}

    def _raise(msg):
        low = (msg or '').lower()
        if 'auth' in low or 'password' in low or 'publickey' in low:
            raise AuthenticationException(msg)
        if ('refused' in low or 'unknownhost' in low or 'unknown host' in low
                or 'timed out' in low or 'timeout' in low or 'unable to connect' in low
                or 'no route' in low or 'connection' in low):
            raise NoValidConnectionsError(msg)
        raise SSHException(msg)

    def _call(fn, *args):
        res = fn(*args)
        ok = res[0]
        payload = res[1]
        if not ok:
            _raise(payload)
        return payload

    class MissingHostKeyPolicy(object):
        def missing_host_key(self, client, hostname, key):
            raise SSHException('Unknown server ' + str(hostname))

    class AutoAddPolicy(MissingHostKeyPolicy):
        def missing_host_key(self, client, hostname, key):
            return None

    class WarningPolicy(MissingHostKeyPolicy):
        def missing_host_key(self, client, hostname, key):
            return None

    class RejectPolicy(MissingHostKeyPolicy):
        def missing_host_key(self, client, hostname, key):
            raise SSHException('Server ' + str(hostname) + ' not found in known_hosts')

    class PKey(object):
        _key_kind = 'rsa'
        _default_bits = 0

        def __init__(self, path=None, password=None, _data=None):
            self._path = path
            self._password = password
            self._name = 'ssh-key'
            self._bits = 0
            self._public_blob = b''
            self._private_b64 = None
            self._fingerprint = None
            if _data:
                self._apply_key_data(_data)

        def _apply_key_data(self, data):
            self._name = data.get('name') or self._name
            self._bits = int(data.get('bits') or self._bits or 0)
            self._public_blob = _b64d(data.get('public'))
            self._private_b64 = data.get('private')
            self._fingerprint = data.get('fingerprint')
            return self

        @classmethod
        def _from_key_data(cls, data, path=None, password=None):
            obj = cls(path=path, password=password)
            return obj._apply_key_data(data)

        @classmethod
        def from_private_key_file(cls, filename, password=None):
            if os.path.exists(filename):
                with open(filename, 'rb') as f:
                    return cls.from_private_key(f, password=password)
            return cls(path=filename, password=password)

        @classmethod
        def from_private_key(cls, file_obj, password=None):
            data = file_obj.read()
            if isinstance(data, str):
                data = data.encode('utf-8')
            return cls._from_key_data(_call(_key_load, _b64e(data), password), password=password)

        @classmethod
        def generate(cls, bits=None, progress_func=None, **kw):
            password = kw.get('password') or kw.get('passphrase')
            size = bits if bits is not None else cls._default_bits
            return cls._from_key_data(_call(_key_generate, cls._key_kind, size, password), password=password)

        def get_name(self):
            return self._name

        def get_bits(self):
            return int(self._bits or 0)

        def get_fingerprint(self):
            if self._fingerprint:
                try:
                    return bytes(int(p, 16) for p in self._fingerprint.split(':') if p)
                except Exception:
                    pass
            return hashlib.md5(self.asbytes()).digest() if self.asbytes() else b''

        def asbytes(self):
            return bytes(self._public_blob or b'')

        def get_base64(self):
            return _b64e(self.asbytes())

        def write_private_key(self, file_obj, password=None):
            if self._private_b64 is None:
                raise SSHException('private key material is not available')
            data = _b64d(self._private_b64).decode('utf-8')
            file_obj.write(data)

        def write_private_key_file(self, filename, password=None):
            with open(filename, 'w') as f:
                self.write_private_key(f, password=password)

        def __str__(self):
            return self.get_base64()

    class RSAKey(PKey):
        _key_kind = 'rsa'
        _default_bits = 2048

        def __init__(self, path=None, password=None, _data=None):
            super().__init__(path, password, _data=_data)
            if not _data:
                self._name = 'ssh-rsa'

    class DSSKey(PKey):
        _key_kind = 'dss'
        _default_bits = 1024

        def __init__(self, path=None, password=None, _data=None):
            super().__init__(path, password, _data=_data)
            if not _data:
                self._name = 'ssh-dss'

    class ECDSAKey(PKey):
        _key_kind = 'ecdsa'
        _default_bits = 256

        def __init__(self, path=None, password=None, _data=None):
            super().__init__(path, password, _data=_data)
            if not _data:
                self._name = 'ecdsa-sha2-nistp256'

    class Ed25519Key(PKey):
        _key_kind = 'ed25519'
        _default_bits = 0

        def __init__(self, path=None, password=None, _data=None, **kw):
            super().__init__(path, password, _data=_data)
            if not _data:
                self._name = 'ssh-ed25519'

    class HostKeys(object):
        def __init__(self, filename=None):
            self._entries = {}

        def add(self, hostname, keytype, key):
            self._entries[hostname] = key

        def lookup(self, hostname):
            return self._entries.get(hostname)

        def load(self, filename):
            return None

        def save(self, filename):
            return None

        def keys(self):
            return list(self._entries.keys())

        def __contains__(self, k):
            return k in self._entries

    class _Channel(object):
        def __init__(self, code):
            self._code = code

        def recv_exit_status(self):
            return self._code

        def exit_status_ready(self):
            return True

        def shutdown_write(self):
            return None

        def shutdown(self, how):
            return None

        def settimeout(self, t):
            return None

        def close(self):
            return None

    class _ChannelFile(object):
        def __init__(self, data, channel):
            self._buf = data
            self._pos = 0
            self.channel = channel

        def read(self, size=None):
            if size is None or size < 0:
                r = self._buf[self._pos:]
                self._pos = len(self._buf)
                return r
            r = self._buf[self._pos:self._pos + size]
            self._pos += len(r)
            return r

        def readline(self, size=-1):
            idx = self._buf.find(_NLB, self._pos)
            if idx == -1:
                r = self._buf[self._pos:]
                self._pos = len(self._buf)
                return r
            r = self._buf[self._pos:idx + 1]
            self._pos = idx + 1
            return r

        def readlines(self, sizehint=None):
            out = []
            while True:
                line = self.readline()
                if not line:
                    break
                out.append(line)
            return out

        def __iter__(self):
            return iter(self.readlines())

        def flush(self):
            return None

        def close(self):
            return None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    class _ChannelStdinFile(object):
        def __init__(self):
            self.channel = _Channel(0)

        def write(self, data):
            return None

        def writelines(self, lines):
            return None

        def flush(self):
            return None

        def close(self):
            return None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            return False

    class SFTPAttributes(object):
        FLAG_SIZE = 1
        FLAG_UIDGID = 2
        FLAG_PERMISSIONS = 4
        FLAG_AMTIME = 8

        def __init__(self):
            self.st_size = None
            self.st_uid = None
            self.st_gid = None
            self.st_mode = None
            self.st_atime = None
            self.st_mtime = None
            self.filename = None
            self.longname = None
            self._flags = 0

        @classmethod
        def _from(cls, m):
            a = cls()
            a.st_size = m.get('st_size')
            a.st_uid = m.get('st_uid')
            a.st_gid = m.get('st_gid')
            a.st_mode = m.get('st_mode')
            a.st_atime = m.get('st_atime')
            a.st_mtime = m.get('st_mtime')
            a.filename = m.get('filename')
            a.longname = m.get('longname')
            return a

        def __repr__(self):
            return '<SFTPAttributes: size=' + str(self.st_size) + ' mode=' + str(self.st_mode) + '>'

    class SFTPFile(object):
        def __init__(self, sftp, path, mode='r'):
            self._sftp = sftp
            self._path = path
            self._mode = mode
            self._binary = 'b' in mode
            self._writable = any(c in mode for c in ('w', 'a', '+'))
            self._closed = False
            data = b''
            if 'w' not in mode:
                try:
                    data = sftp._get_bytes(path)
                except Exception:
                    if 'r' in mode and 'a' not in mode and '+' not in mode:
                        raise
                    data = b''
            self._data = bytearray(data)
            self._pos = len(self._data) if 'a' in mode else 0

        def read(self, size=None):
            if size is None or size < 0:
                r = bytes(self._data[self._pos:])
                self._pos = len(self._data)
            else:
                r = bytes(self._data[self._pos:self._pos + size])
                self._pos += len(r)
            return r

        def readline(self, size=-1):
            idx = self._data.find(_NLB, self._pos)
            if idx == -1:
                r = bytes(self._data[self._pos:])
                self._pos = len(self._data)
            else:
                r = bytes(self._data[self._pos:idx + 1])
                self._pos = idx + 1
            return r

        def readlines(self, sizehint=None):
            out = []
            while True:
                line = self.readline()
                if not line:
                    break
                out.append(line)
            return out

        def __iter__(self):
            return self

        def __next__(self):
            line = self.readline()
            if not line:
                raise StopIteration
            return line

        def write(self, data):
            if isinstance(data, str):
                data = data.encode('utf-8')
            else:
                data = bytes(data)
            end = self._pos + len(data)
            if end > len(self._data):
                self._data.extend(bytes(end - len(self._data)))
            self._data[self._pos:end] = data
            self._pos = end
            return len(data)

        def writelines(self, lines):
            for line in lines:
                self.write(line)

        def seek(self, offset, whence=0):
            if whence == 1:
                self._pos += offset
            elif whence == 2:
                self._pos = len(self._data) + offset
            else:
                self._pos = offset
            return self._pos

        def tell(self):
            return self._pos

        def flush(self):
            if self._writable and not self._closed:
                self._sftp._put_bytes(self._path, bytes(self._data))
            return None

        def close(self):
            if not self._closed:
                self.flush()
                self._closed = True

        def stat(self):
            return self._sftp.stat(self._path)

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    class SFTPClient(object):
        def __init__(self, handle):
            self._h = handle
            self._cwd = None

        @classmethod
        def from_transport(cls, transport, **kw):
            return transport.open_sftp_client()

        def _adjust(self, path):
            path = str(path)
            if self._cwd is None or path.startswith('/'):
                return path
            base = self._cwd
            if not base.endswith('/'):
                base = base + '/'
            return base + path

        def _get_bytes(self, path):
            return _b64d(_call(_sftp_get, self._h, self._adjust(path)))

        def _put_bytes(self, path, data):
            return _call(_sftp_put, self._h, self._adjust(path), _b64e(data))

        def listdir(self, path='.'):
            return list(_call(_sftp_list, self._h, self._adjust(path), False))

        def listdir_attr(self, path='.'):
            return [SFTPAttributes._from(m) for m in _call(_sftp_list, self._h, self._adjust(path), True)]

        def listdir_iter(self, path='.', read_aheads=50):
            return iter(self.listdir_attr(path))

        def stat(self, path):
            return SFTPAttributes._from(_call(_sftp_stat, self._h, self._adjust(path), True))

        def lstat(self, path):
            return SFTPAttributes._from(_call(_sftp_stat, self._h, self._adjust(path), False))

        def open(self, filename, mode='r', bufsize=-1):
            return SFTPFile(self, self._adjust(filename), mode)

        def file(self, filename, mode='r', bufsize=-1):
            return self.open(filename, mode, bufsize)

        def get(self, remotepath, localpath, callback=None, prefetch=True):
            data = self._get_bytes(remotepath)
            with open(localpath, 'wb') as f:
                f.write(data)
            if callback:
                callback(len(data), len(data))

        def getfo(self, remotepath, fl, callback=None, prefetch=True):
            data = self._get_bytes(remotepath)
            fl.write(data)
            if callback:
                callback(len(data), len(data))
            return len(data)

        def put(self, localpath, remotepath, callback=None, confirm=True):
            with open(localpath, 'rb') as f:
                data = f.read()
            m = self._put_bytes(remotepath, data)
            if callback:
                callback(len(data), len(data))
            return SFTPAttributes._from(m) if confirm else None

        def putfo(self, fl, remotepath, file_size=0, callback=None, confirm=True):
            data = fl.read()
            if isinstance(data, str):
                data = data.encode('utf-8')
            m = self._put_bytes(remotepath, data)
            if callback:
                callback(len(data), len(data))
            return SFTPAttributes._from(m) if confirm else None

        def mkdir(self, path, mode=511):
            _call(_sftp_mkdir, self._h, self._adjust(path), mode)

        def rmdir(self, path):
            _call(_sftp_rmdir, self._h, self._adjust(path))

        def remove(self, path):
            _call(_sftp_remove, self._h, self._adjust(path))

        def unlink(self, path):
            self.remove(path)

        def rename(self, oldpath, newpath):
            _call(_sftp_rename, self._h, self._adjust(oldpath), self._adjust(newpath), False)

        def posix_rename(self, oldpath, newpath):
            _call(_sftp_rename, self._h, self._adjust(oldpath), self._adjust(newpath), True)

        def chmod(self, path, mode):
            _call(_sftp_chmod, self._h, self._adjust(path), mode)

        def symlink(self, source, dest):
            _call(_sftp_symlink, self._h, source, self._adjust(dest))

        def chdir(self, path=None):
            if path is None:
                self._cwd = None
                return
            target = self._adjust(path)
            self.stat(target)
            self._cwd = target

        def getcwd(self):
            return self._cwd

        def normalize(self, path):
            return self._adjust(path)

        def close(self):
            try:
                _call(_sftp_close, self._h)
            except Exception:
                pass

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    class Transport(object):
        def __init__(self, sock=None, sess=None):
            self._sock = sock
            self._sess = sess
            self._server = None
            self._server_keys = []
            self._subsystem_handlers = {}
            self._server_started = False
            self._server_handle = None
            self._relay = None

        def is_active(self):
            if self._server_started:
                return True
            if self._sess is None:
                return False
            try:
                return bool(_call(_active, self._sess))
            except Exception:
                return False

        def open_session(self, *a, **k):
            raise SSHException('paramiko shim: Transport.open_session is unsupported; use SSHClient.exec_command')

        def open_sftp_client(self):
            if self._sess is None:
                raise SSHException('SSH session is not active')
            return SFTPClient(_call(_sftp_open, self._sess))

        def getpeername(self):
            if self._sock is not None and hasattr(self._sock, 'getpeername'):
                try:
                    return self._sock.getpeername()
                except Exception:
                    return None
            return None

        def start_server(self, event=None, server=None):
            self._server = server if server is not None else ServerInterface()
            srv = self._server
            sock = self._sock
            if sock is not None and hasattr(sock, 'recv') and hasattr(sock, 'sendall'):
                import socket as _socketmod, threading as _threadingmod
                def _auth_pw(u, p):
                    try:
                        return srv.check_auth_password(u, p)
                    except Exception:
                        return AUTH_FAILED
                def _forward(addr, port):
                    try:
                        return bool(srv.check_port_forward_request(addr, port))
                    except Exception:
                        return False
                info = _call(_server_start, _auth_pw, _forward)
                self._server_handle = info.get('handle')
                relay = _socketmod.create_connection(('127.0.0.1', int(info.get('port'))))
                self._relay = relay
                def _pump(a, b):
                    try:
                        while True:
                            d = a.recv(4096)
                            if not d:
                                break
                            b.sendall(d)
                    except Exception:
                        pass
                    try:
                        b.shutdown(_socketmod.SHUT_WR)
                    except Exception:
                        pass
                _t_up = _threadingmod.Thread(target=_pump, args=(sock, relay), daemon=True)
                _t_dn = _threadingmod.Thread(target=_pump, args=(relay, sock), daemon=True)
                _t_up.start()
                _t_dn.start()
                def _reap(up=_t_up, dn=_t_dn, relay=relay):
                    # Tie the MINA server's lifetime to this ONE relayed
                    # connection: once both pump directions end (the client
                    # socket closed), stop the server so its NIO2 threads and
                    # buffers are reclaimed even if the guest never calls close().
                    try:
                        up.join()
                        dn.join()
                    except Exception:
                        pass
                    h = getattr(self, '_server_handle', None)
                    if h is not None:
                        try:
                            _call(_server_stop, h)
                        except Exception:
                            pass
                        self._server_handle = None
                    try:
                        relay.close()
                    except Exception:
                        pass
                _threadingmod.Thread(target=_reap, daemon=True).start()
            self._server_started = True
            if event is not None and hasattr(event, 'set'):
                event.set()
            return None

        def start_client(self, event=None, timeout=None):
            if event is not None and hasattr(event, 'set'):
                event.set()
            return None

        def add_server_key(self, key):
            self._server_keys.append(key)
            return None

        def get_server_key(self):
            return self._server_keys[0] if self._server_keys else None

        def set_subsystem_handler(self, name, handler, *larg, **kwarg):
            self._subsystem_handlers[name] = (handler, larg, kwarg)
            return None

        def accept(self, timeout=None):
            return None

        def close(self):
            self._server_started = False
            if getattr(self, '_server_handle', None) is not None:
                try:
                    _call(_server_stop, self._server_handle)
                except Exception:
                    pass
                self._server_handle = None
            if getattr(self, '_relay', None) is not None:
                try:
                    self._relay.close()
                except Exception:
                    pass
                self._relay = None
            if self._sess is not None:
                try:
                    _call(_close, self._sess)
                except Exception:
                    pass
            if self._sock is not None and hasattr(self._sock, 'close'):
                try:
                    self._sock.close()
                except Exception:
                    pass

    class SSHClient(object):
        def __init__(self):
            self._policy = RejectPolicy()
            self._sess = None
            self._host_keys = HostKeys()

        def set_missing_host_key_policy(self, policy):
            self._policy = policy() if isinstance(policy, type) else policy

        def load_system_host_keys(self, filename=None):
            return None

        def load_host_keys(self, filename):
            return None

        def save_host_keys(self, filename):
            return None

        def get_host_keys(self):
            return self._host_keys

        def set_log_channel(self, name):
            return None

        def connect(self, hostname, port=22, username=None, password=None, pkey=None,
                    key_filename=None, timeout=None, allow_agent=True, look_for_keys=True,
                    compress=False, sock=None, gss_auth=False, gss_kex=False,
                    gss_deleg_creds=True, gss_host=None, banner_timeout=None,
                    auth_timeout=None, channel_timeout=None, passphrase=None,
                    disabled_algorithms=None, **kw):
            kf = key_filename
            if isinstance(kf, (list, tuple)):
                kf = kf[0] if kf else None
            pf = passphrase
            if pkey is not None:
                if getattr(pkey, '_path', None):
                    kf = pkey._path
                if pf is None:
                    pf = getattr(pkey, '_password', None)
            pol = 'add' if type(self._policy).__name__ in ('AutoAddPolicy', 'WarningPolicy') else 'reject'
            opts = {
                'hostname': str(hostname),
                'port': int(port),
                'username': username or '',
                'password': password or '',
                'key_filename': kf or '',
                'passphrase': pf or '',
                'policy': pol,
                'timeout_ms': int(timeout * 1000) if timeout else 0,
                'look_for_keys': bool(look_for_keys),
                'compress': bool(compress),
            }
            self._sess = _call(_connect, opts)

        def exec_command(self, command, bufsize=-1, timeout=None, get_pty=False, environment=None):
            if self._sess is None:
                raise SSHException('SSH session is not active')
            tmo = int(timeout * 1000) if timeout else 0
            res = _call(_exec, self._sess, command, tmo, '')
            out = _b64d(res.get('stdout'))
            err = _b64d(res.get('stderr'))
            code = res.get('exit_status')
            if code is None:
                code = -1
            chan = _Channel(int(code))
            return (_ChannelStdinFile(), _ChannelFile(out, chan), _ChannelFile(err, chan))

        def open_sftp(self):
            if self._sess is None:
                raise SSHException('SSH session is not active')
            return SFTPClient(_call(_sftp_open, self._sess))

        def get_transport(self):
            if self._sess is None:
                return None
            return Transport(sess=self._sess)

        def invoke_shell(self, *a, **k):
            raise SSHException('paramiko shim: interactive invoke_shell is unsupported; use exec_command')

        def close(self):
            if self._sess is not None:
                try:
                    _call(_close, self._sess)
                except Exception:
                    pass
                self._sess = None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    AUTH_SUCCESSFUL = 0
    AUTH_PARTIALLY_SUCCESSFUL = 1
    AUTH_FAILED = 2
    OPEN_SUCCEEDED = 0
    OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED = 1
    OPEN_FAILED_CONNECT_FAILED = 2
    OPEN_FAILED_UNKNOWN_CHANNEL_TYPE = 3
    OPEN_FAILED_RESOURCE_SHORTAGE = 4
    SFTP_OK = 0
    SFTP_EOF = 1
    SFTP_NO_SUCH_FILE = 2
    SFTP_PERMISSION_DENIED = 3
    SFTP_FAILURE = 4
    SFTP_BAD_MESSAGE = 5
    SFTP_NO_CONNECTION = 6
    SFTP_CONNECTION_LOST = 7
    SFTP_OP_UNSUPPORTED = 8
    SFTP_FLAG_READ = 1
    SFTP_FLAG_WRITE = 2
    SFTP_FLAG_APPEND = 4
    SFTP_FLAG_CREATE = 8
    SFTP_FLAG_TRUNC = 16
    SFTP_FLAG_EXCL = 32

    class InteractiveQuery(object):
        def __init__(self, name='', instructions='', *prompts):
            self.name = name
            self.instructions = instructions
            self.prompts = []
            for p in prompts:
                if isinstance(p, (tuple, list)):
                    self.prompts.append((p[0], bool(p[1])))
                else:
                    self.prompts.append((p, True))

        def add_prompt(self, prompt, echo=True):
            self.prompts.append((prompt, bool(echo)))

    class ServerInterface(object):
        def check_channel_request(self, kind, chanid):
            return OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED

        def get_allowed_auths(self, username):
            return 'password'

        def check_auth_none(self, username):
            return AUTH_FAILED

        def check_auth_password(self, username, password):
            return AUTH_FAILED

        def check_auth_publickey(self, username, key):
            return AUTH_FAILED

        def check_auth_interactive(self, username, submethods):
            return AUTH_FAILED

        def check_auth_interactive_response(self, responses):
            return AUTH_FAILED

        def check_auth_gssapi_with_mic(self, username, gss_authenticated=AUTH_FAILED, cc_file=None):
            return AUTH_FAILED

        def check_auth_gssapi_keyex(self, username, gss_authenticated=AUTH_FAILED, cc_file=None):
            return AUTH_FAILED

        def enable_auth_gssapi(self):
            return False

        def check_port_forward_request(self, address, port):
            return False

        def cancel_port_forward_request(self, address, port):
            return None

        def check_global_request(self, kind, msg):
            return False

        def check_channel_pty_request(self, channel, term, width, height, pixelwidth, pixelheight, modes):
            return False

        def check_channel_shell_request(self, channel):
            return False

        def check_channel_exec_request(self, channel, command):
            return False

        def check_channel_subsystem_request(self, channel, name):
            return False

        def check_channel_window_change_request(self, channel, width, height, pixelwidth, pixelheight):
            return False

        def check_channel_x11_request(self, channel, single_connection, auth_protocol, auth_cookie, screen_number):
            return False

        def check_channel_forward_agent_request(self, channel):
            return False

        def check_channel_direct_tcpip_request(self, chanid, origin, destination):
            return OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED

        def get_banner(self):
            return (None, None)

    class SubsystemHandler(object):
        def __init__(self, channel, name, server, *larg, **kwarg):
            self._channel = channel
            self._name = name
            self._server = server

        def get_server(self):
            return self._server

        def start_subsystem(self, name, transport, channel):
            pass

        def finish_subsystem(self):
            pass

    class SFTPServerInterface(object):
        def __init__(self, server, *larg, **kwarg):
            self.server = server

        def session_started(self):
            pass

        def session_ended(self):
            pass

        def open(self, path, flags, attr):
            return SFTP_OP_UNSUPPORTED

        def list_folder(self, path):
            return SFTP_OP_UNSUPPORTED

        def stat(self, path):
            return SFTP_OP_UNSUPPORTED

        def lstat(self, path):
            return SFTP_OP_UNSUPPORTED

        def remove(self, path):
            return SFTP_OP_UNSUPPORTED

        def rename(self, oldpath, newpath):
            return SFTP_OP_UNSUPPORTED

        def posix_rename(self, oldpath, newpath):
            return SFTP_OP_UNSUPPORTED

        def mkdir(self, path, attr):
            return SFTP_OP_UNSUPPORTED

        def rmdir(self, path):
            return SFTP_OP_UNSUPPORTED

        def chattr(self, path, attr):
            return SFTP_OP_UNSUPPORTED

        def readlink(self, path):
            return SFTP_OP_UNSUPPORTED

        def symlink(self, target_path, path):
            return SFTP_OP_UNSUPPORTED

        def canonicalize(self, path):
            import posixpath
            if posixpath.isabs(path):
                return posixpath.normpath(path)
            return posixpath.normpath('/' + path)

    class SFTPHandle(object):
        def __init__(self, flags=0):
            self._flags = flags
            self._name = None

        def close(self):
            pass

        def read(self, offset, length):
            return SFTP_OP_UNSUPPORTED

        def write(self, offset, data):
            return SFTP_OP_UNSUPPORTED

        def stat(self):
            return SFTP_OP_UNSUPPORTED

        def chattr(self, attr):
            return SFTP_OP_UNSUPPORTED

    class SFTPServer(SubsystemHandler):
        def __init__(self, channel, name, server, sftp_si=None, *larg, **kwarg):
            super().__init__(channel, name, server)
            if isinstance(sftp_si, type):
                self.server = sftp_si(server, *larg, **kwarg)
            else:
                self.server = sftp_si

        def start_subsystem(self, name, transport, channel):
            return None

        def finish_subsystem(self):
            pass

        @staticmethod
        def convert_errno(e):
            import errno
            if e == errno.EACCES:
                return SFTP_PERMISSION_DENIED
            if e in (errno.ENOENT, errno.ENOTDIR):
                return SFTP_NO_SUCH_FILE
            return SFTP_FAILURE

        @staticmethod
        def set_file_attr(filename, attr):
            import os
            flags = getattr(attr, '_flags', 0)
            if flags & SFTPAttributes.FLAG_PERMISSIONS and attr.st_mode is not None:
                os.chmod(filename, attr.st_mode)
            if flags & SFTPAttributes.FLAG_UIDGID:
                os.chown(filename, attr.st_uid, attr.st_gid)
            if flags & SFTPAttributes.FLAG_AMTIME:
                os.utime(filename, (attr.st_atime, attr.st_mtime))
            if flags & SFTPAttributes.FLAG_SIZE:
                with open(filename, 'r+') as f:
                    f.truncate(attr.st_size)

    import struct as _struct, io as _io, fnmatch as _fnmatch, shlex as _shlex

    _zero_byte = bytes([0])
    _one_byte = bytes([1])
    _max_byte = bytes([255])

    def _u(s, encoding='utf8'):
        if isinstance(s, bytes):
            return s.decode(encoding)
        return s

    def _asbytes(s):
        if isinstance(s, bytes):
            return s
        if isinstance(s, str):
            return s.encode('utf-8')
        if hasattr(s, 'asbytes'):
            return s.asbytes()
        raise Exception('Unknown type for ' + repr(s))

    def _inflate_long(s, always_positive=False):
        out = 0
        negative = 0
        if not always_positive and len(s) > 0 and s[0] >= 0x80:
            negative = 1
        if len(s) % 4:
            filler = _zero_byte
            if negative:
                filler = _max_byte
            s = filler * (4 - len(s) % 4) + s
        for i in range(0, len(s), 4):
            out = (out << 32) + _struct.unpack('>I', s[i:i + 4])[0]
        if negative:
            out -= (1 << (8 * len(s)))
        return out

    def _deflate_long(n, add_sign_padding=True):
        s = bytes()
        n = int(n)
        while (n != 0) and (n != -1):
            s = _struct.pack('>I', n & 0xffffffff) + s
            n >>= 32
        for i in enumerate(s):
            if (n == 0) and (i[1] != 0):
                break
            if (n == -1) and (i[1] != 255):
                break
        else:
            i = (0,)
            if n == 0:
                s = _zero_byte
            else:
                s = _max_byte
        s = s[i[0]:]
        if add_sign_padding:
            if (n == 0) and (len(s) and s[0] >= 0x80):
                s = _zero_byte + s
            if (n == -1) and (len(s) and s[0] < 0x80):
                s = _max_byte + s
        return s

    class Message(object):
        big_int = 0xff000000
        def __init__(self, content=None):
            if content is not None:
                self.packet = _io.BytesIO(bytes(content))
            else:
                self.packet = _io.BytesIO()
        def __bytes__(self):
            return self.asbytes()
        def __repr__(self):
            return 'paramiko.Message(' + repr(self.packet.getvalue()) + ')'
        def asbytes(self):
            return self.packet.getvalue()
        def rewind(self):
            self.packet.seek(0)
        def get_remainder(self):
            position = self.packet.tell()
            remainder = self.packet.read()
            self.packet.seek(position)
            return remainder
        def get_so_far(self):
            position = self.packet.tell()
            self.rewind()
            return self.packet.read(position)
        def get_bytes(self, n):
            b = self.packet.read(n)
            max_pad_size = 1 << 20
            if len(b) < n < max_pad_size:
                return b + _zero_byte * (n - len(b))
            return b
        def get_byte(self):
            return self.get_bytes(1)
        def get_boolean(self):
            b = self.get_bytes(1)
            return b != _zero_byte
        def get_adaptive_int(self):
            byte = self.get_bytes(1)
            if byte == _max_byte:
                return _inflate_long(self.get_binary())
            byte += self.get_bytes(3)
            return _struct.unpack('>I', byte)[0]
        def get_int(self):
            return _struct.unpack('>I', self.get_bytes(4))[0]
        def get_int64(self):
            return _struct.unpack('>Q', self.get_bytes(8))[0]
        def get_mpint(self):
            return _inflate_long(self.get_binary())
        def get_string(self):
            return self.get_bytes(self.get_int())
        def get_text(self):
            return _u(self.get_string())
        def get_binary(self):
            return self.get_bytes(self.get_int())
        def get_list(self):
            return self.get_text().split(',')
        def add_bytes(self, b):
            self.packet.write(b)
            return self
        def add_byte(self, b):
            self.packet.write(b)
            return self
        def add_boolean(self, b):
            if b:
                self.packet.write(_one_byte)
            else:
                self.packet.write(_zero_byte)
            return self
        def add_int(self, n):
            self.packet.write(_struct.pack('>I', n))
            return self
        def add_adaptive_int(self, n):
            if n >= Message.big_int:
                self.packet.write(_max_byte)
                self.add_string(_deflate_long(n))
            else:
                self.packet.write(_struct.pack('>I', n))
            return self
        def add_int64(self, n):
            self.packet.write(_struct.pack('>Q', n))
            return self
        def add_mpint(self, z):
            self.add_string(_deflate_long(z))
            return self
        def add_string(self, s):
            s = _asbytes(s)
            self.add_int(len(s))
            self.packet.write(s)
            return self
        def add_list(self, l):
            self.add_string(','.join(l))
            return self
        def _add(self, i):
            if type(i) is bool:
                return self.add_boolean(i)
            elif isinstance(i, int):
                return self.add_adaptive_int(i)
            elif type(i) is list:
                return self.add_list(i)
            else:
                return self.add_string(i)
        def add(self, *seq):
            for item in seq:
                self._add(item)

    class Channel(_Channel):
        pass

    class ChannelFile(_ChannelFile):
        pass

    class ChannelStderrFile(_ChannelFile):
        pass

    class ChannelStdinFile(_ChannelStdinFile):
        pass

    class BufferedFile(object):
        SEEK_SET = 0
        SEEK_CUR = 1
        SEEK_END = 2
        def __init__(self):
            self._closed = False
        def close(self):
            self._closed = True
        def __enter__(self):
            return self
        def __exit__(self, *a):
            self.close()
            return False

    class SFTPError(SSHException):
        pass

    class CouldNotCanonicalize(SSHException):
        pass

    class IncompatiblePeer(SSHException):
        pass

    class MessageOrderError(SSHException):
        pass

    class UnknownKeyType(Exception):
        def __init__(self, key_type=None, key_bytes=None):
            super().__init__('Unknown key type ' + str(key_type))
            self.key_type = key_type
            self.key_bytes = key_bytes

    class PublicBlob(object):
        def __init__(self, type_, blob, comment=None):
            self.key_type = type_
            self.key_blob = bytes(blob)
            self.comment = comment
        @classmethod
        def from_string(cls, s):
            fields = s.split(None, 2)
            if len(fields) < 2:
                raise ValueError('Not enough fields for public blob: ' + repr(s))
            kind = fields[0]
            blob = base64.b64decode(fields[1])
            comment = fields[2].strip() if len(fields) > 2 else None
            return cls(kind, blob, comment)
        @classmethod
        def from_file(cls, filename):
            with open(filename) as f:
                return cls.from_string(f.read())
        def __str__(self):
            ret = self.key_type + ' ' + _b64e(self.key_blob)
            if self.comment:
                ret += ' ' + self.comment
            return ret
        def __eq__(self, other):
            return (isinstance(other, PublicBlob)
                    and self.key_type == other.key_type
                    and self.key_blob == other.key_blob)
        def __hash__(self):
            return hash((self.key_type, self.key_blob))

    class SecurityOptions(object):
        def __init__(self, transport=None):
            self._transport = transport
            self._ciphers = ()
            self._digests = ()
            self._kex = ()
            self._key_types = ()
            self._compression = ()
        def __repr__(self):
            return '<paramiko.SecurityOptions for vis shim>'
        def _set(self, name, value):
            setattr(self, '_' + name, tuple(value))
        ciphers = property(lambda self: self._ciphers, lambda self, v: self._set('ciphers', v))
        digests = property(lambda self: self._digests, lambda self, v: self._set('digests', v))
        kex = property(lambda self: self._kex, lambda self, v: self._set('kex', v))
        key_types = property(lambda self: self._key_types, lambda self, v: self._set('key_types', v))
        compression = property(lambda self: self._compression, lambda self, v: self._set('compression', v))

    class AgentKey(PKey):
        def __init__(self, agent=None, blob=b'', comment='', **kw):
            super().__init__()
            self.agent = agent
            self.blob = bytes(blob)
            self.public_blob = None
            self.comment = comment
            self._name = 'ssh-agent-key'
        def asbytes(self):
            return self.blob
        def get_name(self):
            return self._name
        def sign_ssh_data(self, data, algorithm=None):
            raise SSHException('paramiko shim: SSH agent signing is unsupported in the sandbox')

    class Agent(object):
        def __init__(self):
            self._keys = ()
        def get_keys(self):
            return self._keys
        def keys(self):
            return self._keys
        def close(self):
            return None

    class ProxyCommand(object):
        def __init__(self, command_line):
            self.cmd = command_line
            self.timeout = None
            self.closed = False
        def send(self, content):
            raise ProxyCommandFailure(self.cmd, 'ProxyCommand is unsupported in the vis sandbox')
        def recv(self, size):
            raise ProxyCommandFailure(self.cmd, 'ProxyCommand is unsupported in the vis sandbox')
        def close(self):
            self.closed = True
        def settimeout(self, timeout):
            self.timeout = timeout

    SSH_PORT = 22

    class SSHConfigDict(dict):
        def as_bool(self, key):
            val = self.get(key)
            if val is None:
                return False
            if isinstance(val, bool):
                return val
            return str(val).lower() in ('1', 'true', 'yes')
        def as_int(self, key):
            return int(self.get(key))

    class SSHConfig(object):
        def __init__(self):
            self._config = []
        @classmethod
        def from_text(cls, text):
            obj = cls()
            obj.parse(_io.StringIO(text))
            return obj
        @classmethod
        def from_path(cls, path):
            with open(path) as fl:
                return cls.from_file(fl)
        @classmethod
        def from_file(cls, flo):
            obj = cls()
            obj.parse(flo)
            return obj
        def parse(self, file_obj):
            cur = {'host': ['*'], 'config': {}}
            self._config = [cur]
            for raw in file_obj:
                line = raw.strip()
                if not line or line.startswith('#'):
                    continue
                if '=' in line and (' ' not in line.split('=', 1)[0].strip()):
                    key, value = line.split('=', 1)
                else:
                    parts = line.split(None, 1)
                    key = parts[0]
                    value = parts[1] if len(parts) > 1 else ''
                key = key.strip().lower()
                value = value.strip()
                for q in (chr(34), chr(39)):
                    if len(value) >= 2 and value.startswith(q) and value.endswith(q):
                        value = value[1:-1]
                        break
                if key == 'host':
                    cur = {'host': self._get_hosts(value), 'config': {}}
                    self._config.append(cur)
                elif key == 'match':
                    cur = {'match': value, 'config': {}}
                    self._config.append(cur)
                else:
                    if key in ('identityfile', 'localforward', 'remoteforward',
                               'dynamicforward', 'certificatefile'):
                        cur['config'].setdefault(key, []).append(value)
                    elif key not in cur['config']:
                        cur['config'][key] = value
            return self._config
        def lookup(self, hostname):
            options = SSHConfigDict()
            for entry in self._config:
                if 'host' not in entry:
                    continue
                if not self._pattern_matches(entry['host'], hostname):
                    continue
                for key, value in entry['config'].items():
                    if key in ('identityfile', 'certificatefile'):
                        vals = value if isinstance(value, list) else [value]
                        options.setdefault(key, []).extend(vals)
                    elif key not in options:
                        options[key] = list(value) if isinstance(value, list) else value
            return self._expand_variables(options, hostname)
        def _pattern_matches(self, patterns, target):
            if isinstance(patterns, str):
                patterns = [patterns]
            match = False
            for pattern in patterns:
                neg = pattern.startswith('!')
                pat = pattern[1:] if neg else pattern
                if _fnmatch.fnmatch(target, pat):
                    if neg:
                        return False
                    match = True
            return match
        def _get_hosts(self, host):
            try:
                return _shlex.split(host)
            except ValueError:
                raise ConfigParseError('Unparsable host ' + host)
        def _safe_user(self):
            try:
                import getpass
                return getpass.getuser()
            except Exception:
                return os.environ.get('USER') or ''
        def _safe_host(self):
            try:
                import socket as _s
                return _s.gethostname()
            except Exception:
                return ''
        def _expand_variables(self, config, hostname):
            if 'hostname' in config:
                config['hostname'] = config['hostname'].replace('%h', hostname)
            else:
                config['hostname'] = hostname
            port = config['port'] if 'port' in config else str(SSH_PORT)
            user = self._safe_user()
            fqdn = self._safe_host()
            repl = {
                '%h': config.get('hostname', hostname),
                '%p': str(port),
                '%r': config.get('user', user),
                '%l': fqdn.split('.')[0] if fqdn else '',
                '%u': user,
                '%%': '%',
            }
            for key in list(config.keys()):
                val = config[key]
                if isinstance(val, list):
                    config[key] = [self._sub_tokens(v, repl) for v in val]
                elif isinstance(val, str):
                    config[key] = self._sub_tokens(val, repl)
            return config
        def _sub_tokens(self, s, repl):
            for k, v in repl.items():
                s = s.replace(k, v)
            return s
        def get_hostnames(self):
            hosts = set()
            for entry in self._config:
                if 'host' in entry:
                    hosts.update(entry['host'])
            return hosts

    def util_log_to_file(*a, **k):
        return None

    mod = types.ModuleType('paramiko')
    mod.__doc__ = 'vis sandbox paramiko-compat shim (pure-Java SSH2 via the mwiede JSch fork).'
    mod.__version__ = '3.5.0-vis'
    mod.__path__ = []
    mod.SSHClient = SSHClient
    mod.SSHConfig = SSHConfig
    mod.SSHConfigDict = SSHConfigDict
    mod.SecurityOptions = SecurityOptions
    mod.Channel = Channel
    mod.ChannelFile = ChannelFile
    mod.ChannelStderrFile = ChannelStderrFile
    mod.ChannelStdinFile = ChannelStdinFile
    mod.BufferedFile = BufferedFile
    mod.Message = Message
    mod.SFTP = SFTPClient
    mod.SFTPError = SFTPError
    mod.CouldNotCanonicalize = CouldNotCanonicalize
    mod.IncompatiblePeer = IncompatiblePeer
    mod.MessageOrderError = MessageOrderError
    mod.PublicBlob = PublicBlob
    mod.UnknownKeyType = UnknownKeyType
    mod.Agent = Agent
    mod.AgentKey = AgentKey
    mod.ProxyCommand = ProxyCommand
    mod.io_sleep = 0.01
    mod.key_classes = [DSSKey, RSAKey, Ed25519Key, ECDSAKey]
    mod.__version_info__ = (3, 5, 0)
    mod.__author__ = 'Jeff Forcier <jeff@bitprophet.org>'
    mod.__license__ = 'GNU Lesser General Public License (LGPL)'
    mod.Transport = Transport
    mod.SFTPClient = SFTPClient
    mod.SFTPFile = SFTPFile
    mod.SFTPAttributes = SFTPAttributes
    mod.MissingHostKeyPolicy = MissingHostKeyPolicy
    mod.AutoAddPolicy = AutoAddPolicy
    mod.RejectPolicy = RejectPolicy
    mod.WarningPolicy = WarningPolicy
    mod.HostKeys = HostKeys
    mod.PKey = PKey
    mod.RSAKey = RSAKey
    mod.DSSKey = DSSKey
    mod.ECDSAKey = ECDSAKey
    mod.Ed25519Key = Ed25519Key
    mod.SSHException = SSHException
    mod.AuthenticationException = AuthenticationException
    mod.PasswordRequiredException = PasswordRequiredException
    mod.BadAuthenticationType = BadAuthenticationType
    mod.BadHostKeyException = BadHostKeyException
    mod.ChannelException = ChannelException
    mod.ProxyCommandFailure = ProxyCommandFailure
    mod.ConfigParseError = ConfigParseError
    mod.NoValidConnectionsError = NoValidConnectionsError
    mod.ServerInterface = ServerInterface
    mod.InteractiveQuery = InteractiveQuery
    mod.SubsystemHandler = SubsystemHandler
    mod.SFTPServer = SFTPServer
    mod.SFTPServerInterface = SFTPServerInterface
    mod.SFTPHandle = SFTPHandle
    mod.AUTH_SUCCESSFUL = AUTH_SUCCESSFUL
    mod.AUTH_PARTIALLY_SUCCESSFUL = AUTH_PARTIALLY_SUCCESSFUL
    mod.AUTH_FAILED = AUTH_FAILED
    mod.OPEN_SUCCEEDED = OPEN_SUCCEEDED
    mod.OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED = OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED
    mod.OPEN_FAILED_CONNECT_FAILED = OPEN_FAILED_CONNECT_FAILED
    mod.OPEN_FAILED_UNKNOWN_CHANNEL_TYPE = OPEN_FAILED_UNKNOWN_CHANNEL_TYPE
    mod.OPEN_FAILED_RESOURCE_SHORTAGE = OPEN_FAILED_RESOURCE_SHORTAGE
    mod.SFTP_OK = SFTP_OK
    mod.SFTP_EOF = SFTP_EOF
    mod.SFTP_NO_SUCH_FILE = SFTP_NO_SUCH_FILE
    mod.SFTP_PERMISSION_DENIED = SFTP_PERMISSION_DENIED
    mod.SFTP_FAILURE = SFTP_FAILURE
    mod.SFTP_BAD_MESSAGE = SFTP_BAD_MESSAGE
    mod.SFTP_NO_CONNECTION = SFTP_NO_CONNECTION
    mod.SFTP_CONNECTION_LOST = SFTP_CONNECTION_LOST
    mod.SFTP_OP_UNSUPPORTED = SFTP_OP_UNSUPPORTED
    mod.SFTP_FLAG_READ = SFTP_FLAG_READ
    mod.SFTP_FLAG_WRITE = SFTP_FLAG_WRITE
    mod.SFTP_FLAG_APPEND = SFTP_FLAG_APPEND
    mod.SFTP_FLAG_CREATE = SFTP_FLAG_CREATE
    mod.SFTP_FLAG_TRUNC = SFTP_FLAG_TRUNC
    mod.SFTP_FLAG_EXCL = SFTP_FLAG_EXCL

    _util = types.ModuleType('paramiko.util')
    _util.log_to_file = util_log_to_file
    mod.util = _util

    _exc = types.ModuleType('paramiko.ssh_exception')
    for _n in ('SSHException', 'AuthenticationException', 'PasswordRequiredException',
               'BadAuthenticationType', 'BadHostKeyException', 'ChannelException',
               'ProxyCommandFailure', 'ConfigParseError', 'NoValidConnectionsError',
               'CouldNotCanonicalize', 'IncompatiblePeer', 'MessageOrderError'):
        setattr(_exc, _n, getattr(mod, _n))
    mod.ssh_exception = _exc

    _client = types.ModuleType('paramiko.client')
    _client.SSHClient = SSHClient
    _client.MissingHostKeyPolicy = MissingHostKeyPolicy
    _client.AutoAddPolicy = AutoAddPolicy
    _client.RejectPolicy = RejectPolicy
    _client.WarningPolicy = WarningPolicy
    mod.client = _client

    _sftp = types.ModuleType('paramiko.sftp_client')
    _sftp.SFTPClient = SFTPClient
    mod.sftp_client = _sftp

    _sftpf = types.ModuleType('paramiko.sftp_file')
    _sftpf.SFTPFile = SFTPFile
    mod.sftp_file = _sftpf

    _sftpa = types.ModuleType('paramiko.sftp_attr')
    _sftpa.SFTPAttributes = SFTPAttributes
    mod.sftp_attr = _sftpa

    _trans = types.ModuleType('paramiko.transport')
    _trans.Transport = Transport
    _trans.SecurityOptions = SecurityOptions
    mod.transport = _trans

    _pkey = types.ModuleType('paramiko.pkey')
    _pkey.PKey = PKey
    _pkey.PublicBlob = PublicBlob
    _pkey.UnknownKeyType = UnknownKeyType
    mod.pkey = _pkey

    _rsa = types.ModuleType('paramiko.rsakey')
    _rsa.RSAKey = RSAKey
    mod.rsakey = _rsa

    _ed = types.ModuleType('paramiko.ed25519key')
    _ed.Ed25519Key = Ed25519Key
    mod.ed25519key = _ed

    _ec = types.ModuleType('paramiko.ecdsakey')
    _ec.ECDSAKey = ECDSAKey
    mod.ecdsakey = _ec

    _server = types.ModuleType('paramiko.server')
    _server.ServerInterface = ServerInterface
    _server.InteractiveQuery = InteractiveQuery
    _server.SubsystemHandler = SubsystemHandler
    mod.server = _server

    _common = types.ModuleType('paramiko.common')
    for _cn in ('AUTH_SUCCESSFUL', 'AUTH_PARTIALLY_SUCCESSFUL', 'AUTH_FAILED',
                'OPEN_SUCCEEDED', 'OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED',
                'OPEN_FAILED_CONNECT_FAILED', 'OPEN_FAILED_UNKNOWN_CHANNEL_TYPE',
                'OPEN_FAILED_RESOURCE_SHORTAGE'):
        setattr(_common, _cn, getattr(mod, _cn))
    mod.common = _common

    _sftpm = types.ModuleType('paramiko.sftp')
    for _cn in ('SFTP_OK', 'SFTP_EOF', 'SFTP_NO_SUCH_FILE', 'SFTP_PERMISSION_DENIED',
                'SFTP_FAILURE', 'SFTP_BAD_MESSAGE', 'SFTP_NO_CONNECTION',
                'SFTP_CONNECTION_LOST', 'SFTP_OP_UNSUPPORTED', 'SFTP_FLAG_READ',
                'SFTP_FLAG_WRITE', 'SFTP_FLAG_APPEND', 'SFTP_FLAG_CREATE',
                'SFTP_FLAG_TRUNC', 'SFTP_FLAG_EXCL'):
        setattr(_sftpm, _cn, getattr(mod, _cn))
    mod.sftp = _sftpm
    _sftpm.SFTPError = SFTPError

    _sftpsrv = types.ModuleType('paramiko.sftp_server')
    _sftpsrv.SFTPServer = SFTPServer
    mod.sftp_server = _sftpsrv

    _sftpsi = types.ModuleType('paramiko.sftp_si')
    _sftpsi.SFTPServerInterface = SFTPServerInterface
    mod.sftp_si = _sftpsi

    _sftph = types.ModuleType('paramiko.sftp_handle')
    _sftph.SFTPHandle = SFTPHandle
    mod.sftp_handle = _sftph

    _channelmod = types.ModuleType('paramiko.channel')
    _channelmod.Channel = Channel
    _channelmod.ChannelFile = ChannelFile
    _channelmod.ChannelStderrFile = ChannelStderrFile
    _channelmod.ChannelStdinFile = ChannelStdinFile
    mod.channel = _channelmod
    _msgmod = types.ModuleType('paramiko.message')
    _msgmod.Message = Message
    mod.message = _msgmod
    _configmod = types.ModuleType('paramiko.config')
    _configmod.SSHConfig = SSHConfig
    _configmod.SSHConfigDict = SSHConfigDict
    mod.config = _configmod
    _agentmod = types.ModuleType('paramiko.agent')
    _agentmod.Agent = Agent
    _agentmod.AgentKey = AgentKey
    mod.agent = _agentmod
    _filemod = types.ModuleType('paramiko.file')
    _filemod.BufferedFile = BufferedFile
    mod.file = _filemod
    _proxymod = types.ModuleType('paramiko.proxy')
    _proxymod.ProxyCommand = ProxyCommand
    mod.proxy = _proxymod
    sys.modules['paramiko'] = mod
    for _sub in ('util', 'ssh_exception', 'client', 'sftp_client', 'sftp_file',
                 'sftp_attr', 'transport', 'pkey', 'rsakey', 'ed25519key', 'ecdsakey',
                 'server', 'common', 'sftp', 'sftp_server', 'sftp_si', 'sftp_handle',
                 'channel', 'message', 'config', 'agent', 'file', 'proxy'):
        sys.modules['paramiko.' + _sub] = getattr(mod, _sub)

    try:
        _bi.paramiko = mod
    except Exception:
        pass

__vis_install_paramiko__()
del __vis_install_paramiko__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-paramiko"
     :ext/description
     "Sandbox shim: a paramiko-compatible SSH2 module (SSHClient/exec_command/open_sftp/SFTPClient get/put/listdir/stat/open/mkdir/rename/RSAKey+Ed25519Key key generation and loading/AutoAddPolicy/SSHException tree/Transport, plus the server-side API surface: ServerInterface/SubsystemHandler/SFTPServer/SFTPServerInterface/SFTPHandle + AUTH_*/OPEN_*/SFTP_* constants) backed by the pure-Java mwiede JSch fork. GraalPy has no native cryptography/cffi, so CPython paramiko can't install; this makes `import paramiko` work. No pip, no native wheel, no host binary."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "paramiko"
       :shim/description
       "paramiko-compatible SSH2 module backed by pure-Java JSch (sessions + SFTP by integer handle). Supports key generation/loading for RSA/DSS/ECDSA/Ed25519 keys, SSHClient exec/SFTP client flows, and the server-side API surface (ServerInterface/SubsystemHandler/SFTPServer/SFTPServerInterface/SFTPHandle + AUTH_*/OPEN_*/SFTP_* constants) for server-oriented code. `Transport(sock).start_server()` starts a real Apache MINA SSHD only when given a real client socket — it serves reverse (`tcpip-forward`) port-forwarding over that socket, delegating password auth and forward approval to the Python `ServerInterface` (`check_auth_password`/`check_port_forward_request`). Nothing starts on `import` or on a socket-less `start_server()` (that degrades to a passive stub); every started server draws from one shared bounded NIO2 pool, is capped (max 32 live), and self-reaps when its connection ends. Also unsupported: interactive `invoke_shell`; prefer `SSHClient` + `exec_command`/SFTP for client flows."
       :shim/bindings paramiko-bridge-bindings
       :shim/preamble paramiko-shim-src}]}))

(vis/register-extension! vis-extension)
