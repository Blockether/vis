"""The forward proxy of a gateway, as `GatewayClient.proxy()` gives it.

The gateway opens the hosts of `gateway.proxy.allowed_domains` in its global
configuration through the network of its machine, for example a company VPC.
A client sends only those links through it and opens every other link directly.
Each case of `vis-contract/fixtures/proxy-routes.json` holds this module, the
gateway and the Companion app to one answer.
"""

from __future__ import annotations

from dataclasses import dataclass
from urllib.parse import quote, urlsplit
from urllib.request import BaseHandler, OpenerDirector, ProxyHandler, build_opener

from blockether.vis.engine import _types

_DEFAULT_PORTS = {"http": 80, "https": 443}


def _host_key(host: str) -> str:
    return host.strip().strip("[]").lower().rstrip(".")


def _matches(pattern: str, host: str) -> bool:
    """`example.com` and `*.example.com` match example.com and each subdomain."""
    key = _host_key(pattern)
    base = key[2:] if key.startswith("*.") else key
    return bool(base) and base != "*" and (host == base or host.endswith("." + base))


@dataclass(frozen=True)
class GatewayProxy:
    """The forward proxy of one gateway.

    `url` holds the gateway token, so keep it out of logs. `covers(url)` says
    whether a link goes through the gateway, and `proxy_for(url)` gives the proxy
    URL for such a link, else None. `opener()` gives a `urllib` opener that sends
    only covered links through the gateway.

    ```python
    proxy = client.proxy()
    if proxy:
        with proxy.opener().open("https://wiki.corp.internal/") as answer:
            print(answer.status)
    ```

    With another HTTP library, give `proxy.proxy_for(link)` as the proxy of each
    request, for example `httpx.get(link, proxy=proxy.proxy_for(link))`.
    """

    host: str
    port: int
    token: str
    allowed_domains: tuple[str, ...]
    denied_domains: tuple[str, ...]
    ports: tuple[int, ...]

    def __repr__(self) -> str:
        return (
            f"GatewayProxy(host={self.host!r}, port={self.port}, "
            f"allowed_domains={self.allowed_domains!r}, "
            f"denied_domains={self.denied_domains!r}, ports={self.ports!r})"
        )

    @property
    def url(self) -> str:
        """`http://<token>:vis@<host>:<port>`: the gateway token is the proxy user name."""
        host = f"[{self.host}]" if ":" in self.host else self.host
        return f"http://{quote(self.token, safe='')}:vis@{host}:{self.port}"

    def covers(self, url: str) -> bool:
        """True when the gateway opens `url`: an HTTP(S) link to an allowed host and port."""
        try:
            parts = urlsplit(url)
            port = parts.port
        except ValueError:
            return False
        if parts.scheme not in _DEFAULT_PORTS or not parts.hostname:
            return False
        host = _host_key(parts.hostname)
        if any(_matches(p, host) for p in self.denied_domains):
            return False
        if not any(_matches(p, host) for p in self.allowed_domains):
            return False
        return (port or _DEFAULT_PORTS[parts.scheme]) in self.ports

    def proxy_for(self, url: str) -> str | None:
        """The proxy URL for `url` when the gateway covers it, else None."""
        return self.url if self.covers(url) else None

    def opener(self, *handlers: BaseHandler) -> OpenerDirector:
        """A `urllib` opener that sends covered links through the gateway.

        Other links open directly. A redirect is checked again, so it never takes
        the token to a host off the list. `handlers` go to `build_opener`.
        """
        return build_opener(_CoveredOnly(self), *handlers)


class _CoveredOnly(ProxyHandler):
    def __init__(self, proxy: GatewayProxy):
        super().__init__({"http": proxy.url, "https": proxy.url})
        self._proxy = proxy

    def proxy_open(self, req, proxy, type):
        if not self._proxy.covers(req.full_url):
            return None
        return super().proxy_open(req, proxy, type)


def from_answer(answer: _types.Proxy, *, host: str, token: str) -> GatewayProxy | None:
    """The proxy of a `GET /v1/proxy` answer, or None when the proxy is off."""
    port = answer["port"]
    if not answer["is_enabled"] or port is None:
        return None
    return GatewayProxy(
        host=host,
        port=port,
        token=token,
        allowed_domains=tuple(answer["allowed_domains"]),
        denied_domains=tuple(answer["denied_domains"]),
        ports=tuple(answer["ports"]),
    )
