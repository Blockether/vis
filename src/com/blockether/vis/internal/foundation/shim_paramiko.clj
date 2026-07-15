(ns com.blockether.vis.internal.foundation.shim-paramiko
  "Built-in sandbox SHIM: a `paramiko`-compatible SSH2 module backed by the
   pure-Java mwiede JSch fork (`com.github.mwiede/jsch`) so `import paramiko`
   works without the native CPython cryptography/cffi wheels. SSH sessions and
   SFTP channels live HOST-side (JSch `Session`/`ChannelSftp` in integer-keyed
   registries); the Python classes are thin handle wrappers, exchanging
   command/path strings and base64 file bytes across the boundary."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.core :as vis])
  (:import [com.jcraft.jsch ChannelExec ChannelSftp JSch Session SftpATTRS]
           [java.io ByteArrayInputStream ByteArrayOutputStream File]
           [java.util Base64 Properties]))

;; Host-side registries: handle (long) -> JSch Session / ChannelSftp.

(defonce ^:private sess-registry (atom {}))
(defonce ^:private sess-counter (atom 0))
(defonce ^:private sftp-registry (atom {}))
(defonce ^:private sftp-counter (atom 0))

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
  (let [{:strs [hostname port username password key_filename passphrase timeout_ms policy
                look_for_keys compress]}
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
    (let [uname
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
  (let [sess
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
    (let [deadline (when (and timeout-ms (pos? (long timeout-ms)))
                     (+ (System/currentTimeMillis) (long timeout-ms)))]
      (loop []

        (cond (.isClosed ch) nil
              (and deadline (> (System/currentTimeMillis) deadline))
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
  (let [sess
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
  (let [ch
        (sftp-of h)

        a
        (if follow? (.stat ch path) (.lstat ch path))]

    (attrs->map path nil a)))

(defn- op-sftp-get
  [h ^String remote]
  (let [ch
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
                          (ssh-envelope #(op-sftp-close h)))})

;; Python preamble: publishes a paramiko-compatible module into sys.modules.

(def ^:private paramiko-shim-src
  "def __vis_install_paramiko__():
    import sys, types, base64
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
        def __init__(self, path=None, password=None):
            self._path = path
            self._password = password
            self._name = 'ssh-key'

        @classmethod
        def from_private_key_file(cls, filename, password=None):
            return cls(path=filename, password=password)

        @classmethod
        def from_private_key(cls, file_obj, password=None):
            raise SSHException('paramiko shim: from_private_key(file_obj) is unsupported; use from_private_key_file(path)')

        @classmethod
        def generate(cls, *a, **k):
            raise SSHException('paramiko shim: key generation is unsupported')

        def get_name(self):
            return self._name

        def get_bits(self):
            return 0

        def get_fingerprint(self):
            return b''

        def asbytes(self):
            return b''

        def __str__(self):
            return ''

    class RSAKey(PKey):
        def __init__(self, path=None, password=None):
            super().__init__(path, password)
            self._name = 'ssh-rsa'

    class DSSKey(PKey):
        def __init__(self, path=None, password=None):
            super().__init__(path, password)
            self._name = 'ssh-dss'

    class ECDSAKey(PKey):
        def __init__(self, path=None, password=None):
            super().__init__(path, password)
            self._name = 'ecdsa-sha2-nistp256'

    class Ed25519Key(PKey):
        def __init__(self, path=None, password=None, **kw):
            super().__init__(path, password)
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
            return r if self._binary else r.decode('utf-8')

        def readline(self, size=-1):
            idx = self._data.find(_NLB, self._pos)
            if idx == -1:
                r = bytes(self._data[self._pos:])
                self._pos = len(self._data)
            else:
                r = bytes(self._data[self._pos:idx + 1])
                self._pos = idx + 1
            return r if self._binary else r.decode('utf-8')

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
            self._sess = sess
            if sess is None and sock is not None:
                raise SSHException('paramiko shim: low-level Transport(sock) is unsupported; use SSHClient')

        def is_active(self):
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
            return None

        def close(self):
            if self._sess is not None:
                try:
                    _call(_close, self._sess)
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

    def util_log_to_file(*a, **k):
        return None

    mod = types.ModuleType('paramiko')
    mod.__doc__ = 'vis sandbox paramiko-compat shim (pure-Java SSH2 via the mwiede JSch fork).'
    mod.__version__ = '3.5.0-vis'
    mod.__path__ = []
    mod.SSHClient = SSHClient
    mod.SSHConfig = None
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

    _util = types.ModuleType('paramiko.util')
    _util.log_to_file = util_log_to_file
    mod.util = _util

    _exc = types.ModuleType('paramiko.ssh_exception')
    for _n in ('SSHException', 'AuthenticationException', 'PasswordRequiredException',
               'BadAuthenticationType', 'BadHostKeyException', 'ChannelException',
               'ProxyCommandFailure', 'ConfigParseError', 'NoValidConnectionsError'):
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
    mod.transport = _trans

    _pkey = types.ModuleType('paramiko.pkey')
    _pkey.PKey = PKey
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

    sys.modules['paramiko'] = mod
    for _sub in ('util', 'ssh_exception', 'client', 'sftp_client', 'sftp_file',
                 'sftp_attr', 'transport', 'pkey', 'rsakey', 'ed25519key', 'ecdsakey'):
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
     "Sandbox shim: a paramiko-compatible SSH2 module (SSHClient/exec_command/open_sftp/SFTPClient get/put/listdir/stat/open/mkdir/rename/RSAKey+Ed25519Key/AutoAddPolicy/SSHException tree/Transport) backed by the pure-Java mwiede JSch fork. GraalPy has no native cryptography/cffi, so CPython paramiko can't install; this makes `import paramiko` work. No pip, no native wheel, no host binary."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "paramiko"
       :shim/description
       "paramiko-compatible SSH2 module backed by pure-Java JSch (sessions + SFTP channels live host-side by integer handle)."
       :shim/bindings paramiko-bridge-bindings
       :shim/preamble paramiko-shim-src}]}))

(vis/register-extension! vis-extension)
