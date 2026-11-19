# Private links

The Companion app can open a link to a company host that only the gateway computer can reach.
Examples are a wiki or a dashboard in a company VPC. The gateway sends the request through its own
network, and the app shows the page in a browser inside the app.

## When to use

- **A link in a session opens on the gateway computer but not on your phone.** [Turn on private
  links](#turn-on-private-links) on the gateway.
- **You must limit the company hosts that the app can open.** [Choose the domains and
  ports](#choose-the-domains-and-ports).
- **A private link opens in the system browser or shows an error.** [Fix a link that does not
  open](#fix-a-link-that-does-not-open).
- **A script must reach the same hosts.** Read [Private links API](private-links-api.md).

To limit the hosts that the commands of the agent can reach, read [Process jail and network
policy](jail.md).

## Turn on private links

1. On the gateway computer, add `gateway.proxy` to the global configuration file
   `~/.vis/config.yml`:

   ```yaml
   gateway:
     proxy:
       allowed_domains:
         - corp.internal
         - "*.acme.io"
         - 10.20.0.5
       denied_domains:
         - secret.corp.internal
       ports: [80, 443]
       port: 7891
   ```

2. Open the Companion app. The app reads the proxy settings when it connects to the gateway and
   each time that you open the app again.
3. Tap a link to an allowed domain. The link opens in the browser of the app. Tap **Close** to go
   back to the session.

The gateway applies a change of `gateway.proxy` without a restart. Only the global configuration
turns on the proxy. A project configuration file cannot turn it on.

## Choose the domains and ports

The keys of `gateway.proxy` are these:

| Key | Default | Description |
|---|---|---|
| `allowed_domains` | None, required | The hosts that the gateway opens. Give one or more. |
| `denied_domains` | `[]` | The hosts that the gateway refuses, also when an allowed domain matches them. |
| `ports` | `[80, 443]` | The ports that a link can use. |
| `port` | A free port | The port of the proxy on the gateway computer. |

The gateway compares names in these ways:

- `corp.internal` and `*.corp.internal` both match `corp.internal` and each subdomain of it.
- An IP address matches only that address.
- A `*` without a domain is not valid. The proxy never opens every host.
- Upper case and a final full stop in a host name do not change the match.

A link without a port uses port `80` for `http` and port `443` for `https`. Set `port` when a
firewall must allow the proxy. Without `port`, the gateway uses a free port that can change at
each start.

A page can load fonts, scripts, or images from hosts that are not on the list. The device loads
these hosts directly, as it does for any other page.

## Where private links work

The browser of the app sends only the allowed hosts to the proxy. The proxy asks for the gateway
token, and it refuses other hosts, denied domains, other ports, and the ports of the gateway.

The app opens private links in these places:

| App | Private links |
|---|---|
| Android | A browser in the app. It needs a current Android System WebView. |
| iOS and iPadOS 17 or later | A browser in the app. An earlier version opens links in Safari, as before. |
| macOS 14 or later, Windows, and Linux | A separate window of the desktop app. |
| The web app in a browser | Links open as before. A web page cannot set a proxy. |

The desktop app starts a relay on the loopback interface of your computer. The relay adds the
gateway token to the requests of the allowed hosts and sends the other hosts directly.

When the gateway runs on the same computer as the app, links open in the system browser, as
before. That computer can reach the same hosts as the gateway.

## Keep the token safe

The gateway token is the user name of the proxy. A person with the token can use the proxy, as
that person can use the gateway.

The device sends the token to the proxy without encryption. An `https` page stays encrypted from
the device to the company host. An `http` page goes to the gateway without encryption. Connect the
device to the gateway through a private network, for example your tailnet or a VPN.

## Fix a link that does not open

Find the problem that you see, then do its fix:

- **The link opens in the system browser.** Make sure that the domain and the port of the link are
  on the list. Make sure that the app is connected to that gateway. A gateway older than this
  feature has no proxy.
- **The page shows `403 Forbidden`.** The gateway refused the link. The host is a denied domain,
  the port is not in `ports`, or the gateway cannot find the host.
- **The page does not load.** The device cannot reach `port` on the gateway computer. The proxy
  uses the address of the gateway in the app. Open the port in the firewall. A tunnel that forwards
  only the gateway port, such as `cloudflared`, does not forward the proxy.
- **The app loses its connection while a private link is open on Android.** Remove the gateway host
  from `allowed_domains`. On Android, the proxy setting applies to each web view of the app while
  the browser is open.

## See also

- To read the proxy settings and send requests through the proxy from a program, read [Private links
  API](private-links-api.md).
- To install a gateway for your team, read [Running a gateway](gateway-service.md).
- To set the address in the pairing link, read [Gateway pairing
  address](configuration.md#gateway-pairing-address).
