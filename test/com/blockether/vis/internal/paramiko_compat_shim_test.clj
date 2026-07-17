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
  (it "generates, serializes, and reloads private keys"
      (with-python-context
        (expect (= ["ssh-rsa" 1024 true true "ssh-rsa" 1024]
                   (ev python-context
                       (str
                         "import io, paramiko\n"
                         "k=paramiko.RSAKey.generate(bits=1024)\n" "buf=io.StringIO()\n"
                         "k.write_private_key(buf)\n"
                         "loaded=paramiko.RSAKey.from_private_key(io.StringIO(buf.getvalue()))\n"
                         "[k.get_name(), k.get_bits(), len(k.asbytes())>0, "
                         "len(k.get_fingerprint())==16, loaded.get_name(), loaded.get_bits()]"))))))
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

(defdescribe
  paramiko-server-side-test
  (it
    "exposes the server-side class tree, constants, and submodules"
    (with-python-context
      (expect
        (=
          [true true true true true true true true true true true true true true]
          (ev
            python-context
            (str
              "import paramiko\n"
              "from paramiko.server import ServerInterface, InteractiveQuery, SubsystemHandler\n"
              "from paramiko.sftp_server import SFTPServer\n"
              "from paramiko.sftp_si import SFTPServerInterface\n"
              "from paramiko.sftp_handle import SFTPHandle\n"
              "class S(ServerInterface):\n" "  def check_auth_password(self, u, p):\n"
              "    return paramiko.AUTH_SUCCESSFUL\n" "s=S()\n"
              "r=[]\n" "r.append(s.check_auth_password('u','p')==paramiko.AUTH_SUCCESSFUL)\n"
              "r.append(s.check_auth_publickey('u',None)==paramiko.AUTH_FAILED)\n"
              "r.append(s.check_channel_request('x',0)==paramiko.OPEN_FAILED_ADMINISTRATIVELY_PROHIBITED)\n"
              "r.append(s.get_allowed_auths('u')=='password')\n"
              "r.append(s.check_channel_exec_request(None,'ls') is False)\n"
              "r.append(paramiko.SFTP_OK==0 and paramiko.SFTP_EOF==1)\n"
              "r.append(issubclass(SFTPServer, SubsystemHandler))\n"
              "r.append(SFTPServer.convert_errno(2)==paramiko.SFTP_NO_SUCH_FILE)\n"
              "r.append(SFTPServer.convert_errno(13)==paramiko.SFTP_PERMISSION_DENIED)\n"
              "si=SFTPServerInterface(s)\n"
              "r.append(si.list_folder('/')==paramiko.SFTP_OP_UNSUPPORTED)\n"
              "r.append(si.stat('/x')==paramiko.SFTP_OP_UNSUPPORTED)\n" "h=SFTPHandle()\n"
              "r.append(h.close() is None)\n" "r.append(h.stat()==paramiko.SFTP_OP_UNSUPPORTED)\n"
              "r.append(hasattr(InteractiveQuery(),'add_prompt'))\n" "r"))))))
  (it "starts server mode and tracks server keys/subsystem handlers"
      (with-python-context
        (expect (= [true true "ssh-rsa" nil true]
                   (ev python-context
                       (str "import paramiko\n" "class S(paramiko.ServerInterface):\n"
                            "  pass\n" "class E:\n"
                            "  def __init__(self): self.flag=False\n"
                            "  def set(self): self.flag=True\n"
                            "e=E()\n" "t=paramiko.Transport(sock=object())\n"
                            "k=paramiko.RSAKey.generate(bits=1024)\n" "t.add_server_key(k)\n"
                            "t.set_subsystem_handler('sftp', paramiko.SFTPServer)\n"
                            "t.start_server(event=e, server=S())\n"
                            "r=[e.flag, t.is_active(), t.get_server_key().get_name(), "
                            "t.accept(timeout=0), 'sftp' in t._subsystem_handlers]\n"
                            "t.close()\n" "r")))))))
