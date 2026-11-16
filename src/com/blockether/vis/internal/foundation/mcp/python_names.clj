(ns com.blockether.vis.internal.foundation.mcp.python-names
  "The names that Python has before a session starts. An MCP server does not take
   one as its sandbox name: a block that says `import time` must get the module.")

(def words
  "Each keyword, builtin and standard library module of Python that is a lower-case
   identifier. A server name has no other form. `mcp.core-test` compares this set
   with the Python of the sandbox."
  #{"abc" "abs" "aiter" "all" "and" "anext" "annotationlib" "antigravity" "any" "argparse" "array"
    "as" "ascii" "assert" "ast" "async" "asyncio" "atexit" "await" "base64" "bdb" "bin" "binascii"
    "bisect" "bool" "break" "breakpoint" "builtins" "bytearray" "bytes" "bz2" "calendar" "callable"
    "case" "chr" "class" "classmethod" "cmath" "cmd" "code" "codecs" "codeop" "collections"
    "colorsys" "compile" "compileall" "complex" "compression" "concurrent" "configparser"
    "contextlib" "contextvars" "continue" "copy" "copyreg" "copyright" "credits" "csv" "ctypes"
    "curses" "dataclasses" "datetime" "dbm" "decimal" "def" "del" "delattr" "dict" "difflib" "dir"
    "dis" "divmod" "doctest" "elif" "else" "email" "encodings" "ensurepip" "enum" "enumerate"
    "errno" "eval" "except" "exec" "exit" "faulthandler" "fcntl" "filecmp" "fileinput" "filter"
    "finally" "float" "fnmatch" "for" "format" "fractions" "from" "frozenset" "ftplib" "functools"
    "gc" "genericpath" "getattr" "getopt" "getpass" "gettext" "glob" "global" "globals" "graphlib"
    "grp" "gzip" "hasattr" "hash" "hashlib" "heapq" "help" "hex" "hmac" "html" "http" "id" "idlelib"
    "if" "imaplib" "import" "importlib" "in" "input" "inspect" "int" "io" "ipaddress" "is"
    "isinstance" "issubclass" "iter" "itertools" "json" "keyword" "lambda" "len" "license"
    "linecache" "list" "locale" "locals" "logging" "lzma" "mailbox" "map" "marshal" "match" "math"
    "max" "memoryview" "mimetypes" "min" "mmap" "modulefinder" "msvcrt" "multiprocessing" "netrc"
    "next" "nonlocal" "not" "nt" "ntpath" "nturl2path" "numbers" "object" "oct" "opcode" "open"
    "operator" "optparse" "or" "ord" "os" "pass" "pathlib" "pdb" "pickle" "pickletools" "pkgutil"
    "platform" "plistlib" "poplib" "posix" "posixpath" "pow" "pprint" "print" "profile" "property"
    "pstats" "pty" "pwd" "py_compile" "pyclbr" "pydoc" "pydoc_data" "pyexpat" "queue" "quit"
    "quopri" "raise" "random" "range" "re" "readline" "repr" "reprlib" "resource" "return"
    "reversed" "rlcompleter" "round" "runpy" "sched" "secrets" "select" "selectors" "set" "setattr"
    "shelve" "shlex" "shutil" "signal" "site" "slice" "smtplib" "socket" "socketserver" "sorted"
    "sqlite3" "sre_compile" "sre_constants" "sre_parse" "ssl" "stat" "staticmethod" "statistics"
    "str" "string" "stringprep" "struct" "subprocess" "sum" "super" "symtable" "sys" "sysconfig"
    "syslog" "tabnanny" "tarfile" "tempfile" "termios" "textwrap" "this" "threading" "time" "timeit"
    "tkinter" "token" "tokenize" "tomllib" "trace" "traceback" "tracemalloc" "try" "tty" "tuple"
    "turtle" "turtledemo" "type" "types" "typing" "unicodedata" "unittest" "urllib" "uuid" "vars"
    "venv" "warnings" "wave" "weakref" "webbrowser" "while" "winreg" "winsound" "with" "wsgiref"
    "xml" "xmlrpc" "yield" "zip" "zipapp" "zipfile" "zipimport" "zlib" "zoneinfo"})
