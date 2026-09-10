"""Local MemoryBIO TLS handshakes for Vis #185; no external network or real keys."""

import ssl
from pathlib import Path


def tls_probe(directory):
    directory = Path(directory)

    def handshake(ca, leaf, hostname="gateway.example.com", trust=True):
        client = (
            ssl.create_default_context(cafile=str(directory / f"{ca}.pem"))
            if trust
            else ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
        )
        server = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        server.load_cert_chain(
            str(directory / f"{leaf}.pem"), str(directory / "key.pem")
        )
        incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
        peer_in, peer_out = ssl.MemoryBIO(), ssl.MemoryBIO()
        connection = client.wrap_bio(incoming, outgoing, server_hostname=hostname)
        peer = server.wrap_bio(peer_in, peer_out, server_side=True)
        complete = False
        try:
            for _ in range(16):
                try:
                    connection.do_handshake()
                    complete = True
                except ssl.SSLWantReadError:
                    pass
                peer_in.write(outgoing.read())
                try:
                    peer.do_handshake()
                    if complete:
                        return "ok"
                except ssl.SSLWantReadError:
                    pass
                incoming.write(peer_out.read())
        except ssl.SSLCertVerificationError as error:
            return error.verify_code
        raise AssertionError("TLS handshake did not finish")

    context = ssl.create_default_context()
    return {
        "strict": bool(context.verify_flags & ssl.VERIFY_X509_STRICT),
        "required": context.verify_mode == ssl.CERT_REQUIRED,
        "hostname": context.check_hostname,
        "valid": handshake("valid-ca", "valid"),
        "legacy": handshake("legacy-ca", "legacy"),
        "unknown": handshake("valid-ca", "valid", trust=False),
        "wrong_host": handshake("valid-ca", "valid", hostname="wrong.example.com"),
        "expired": handshake("valid-ca", "expired"),
        "signature": handshake("valid-ca", "signature"),
    }
