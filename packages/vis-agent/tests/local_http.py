"""An HTTP server for the test doubles that does not look up the name of its address."""

from http.server import ThreadingHTTPServer
from socketserver import TCPServer


class LocalHTTPServer(ThreadingHTTPServer):
    """A `ThreadingHTTPServer` that binds without a name lookup.

    `HTTPServer.server_bind` asks the system for the name of the address. On a hosted
    macOS runner that lookup takes 35 s, one time in each process. No test reads the name.
    """

    def server_bind(self):
        TCPServer.server_bind(self)
        self.server_name, self.server_port = self.server_address[:2]
