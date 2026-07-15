(ns com.blockether.vis.internal.paramiko-compat-shim-test
  "The paramiko-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism: a `paramiko` module published into `sys.modules`,
   backed by the pure-Java mwiede JSch fork (GraalPy ships no native
   cryptography/cffi, so the CPython paramiko wheel cannot install). SSH sessions
   and SFTP channels live host-side by integer handle. The live SSH round-trip
   (exec_command / SFTP) needs a real server + credentials, so these tests cover
   the server-independent surface: module publication, the class/exception tree,
   key objects, connect-failure mapping, and SFTPAttributes marshaling."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context
                                                                         {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  paramiko-module-test
  (it "publishes paramiko under sys.modules with its submodules"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "import sys, paramiko\n" "from paramiko.ssh_exception import SSHException as E\n"
                  "paramiko is sys.modules['paramiko'] "
                  "and paramiko.__version__.endswith('-vis') "
                  "and E is paramiko.SSHException "
                  "and sys.modules['paramiko.sftp_client'].SFTPClient is paramiko.SFTPClient"))))))
  (it "works with no import (stapled onto builtins)"
      (with-python-context (expect (= "SSHClient"
                                      (ev python-context "type(paramiko.SSHClient()).__name__"))))))

(defdescribe
  paramiko-class-tree-test
  (it "exposes the client, policies, and key classes"
      (with-python-context
        (expect (= ["ssh-rsa" "ssh-ed25519" "ecdsa-sha2-nistp256" "/tmp/id"]
                   (ev python-context
                       (str "import paramiko\n"
                            "k=paramiko.RSAKey.from_private_key_file('/tmp/id')\n"
                            "[paramiko.RSAKey().get_name(), paramiko.Ed25519Key().get_name(), "
                            "paramiko.ECDSAKey().get_name(), k._path]"))))))
  (it
    "subclasses the exception tree like paramiko"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import paramiko\n"
              "issubclass(paramiko.AuthenticationException, paramiko.SSHException) "
              "and issubclass(paramiko.NoValidConnectionsError, paramiko.SSHException) "
              "and issubclass(paramiko.BadHostKeyException, paramiko.SSHException) "
              "and issubclass(paramiko.PasswordRequiredException, paramiko.AuthenticationException)")))))))

(defdescribe
  paramiko-connect-and-attrs-test
  (it "maps a refused connection to NoValidConnectionsError (an SSHException)"
      (with-python-context
        (expect (= "NoValidConnectionsError"
                   (ev python-context
                       (str
                         "import paramiko\n" "cli=paramiko.SSHClient()\n"
                         "cli.set_missing_host_key_policy(paramiko.AutoAddPolicy())\n" "out='?'\n"
                         "try:\n" "  cli.connect('127.0.0.1', port=1, username='x', password='y', "
                         "timeout=3, look_for_keys=False, allow_agent=False)\n"
                         "except paramiko.SSHException as e:\n"
                         "  out=type(e).__name__\n" "out"))))))
  (it "marshals SFTPAttributes fields from the host bridge shape"
      (with-python-context
        (expect (= [42 33188 1000 "readme.txt"]
                   (ev python-context
                       (str
                         "import paramiko\n" "a=paramiko.SFTPAttributes._from("
                         "{'st_size':42,'st_mode':33188,'st_uid':1000,'filename':'readme.txt'})\n"
                         "[a.st_size, a.st_mode, a.st_uid, a.filename]")))))))
