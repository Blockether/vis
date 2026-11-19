import Capacitor
import Network
import UIKit
import WebKit

/// An in-app browser for private links: links to hosts that only the gateway can reach.
///
/// The browser has its own data store. It sends the allowed domains through the forward proxy
/// of the gateway and the other hosts direct. The proxy asks for the gateway token as the user
/// name. A web view can use a proxy from iOS 17. On an older iOS the app opens the link as before.
@objc(ProxyBrowserPlugin)
public class ProxyBrowserPlugin: CAPPlugin, CAPBridgedPlugin {
  public let identifier = "ProxyBrowserPlugin"
  public let jsName = "ProxyBrowser"
  public let pluginMethods: [CAPPluginMethod] = [
    CAPPluginMethod(name: "isAvailable", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "open", returnType: CAPPluginReturnPromise),
  ]

  @objc public func isAvailable(_ call: CAPPluginCall) {
    if #available(iOS 17.0, *) {
      call.resolve(["available": true])
    } else {
      call.resolve(["available": false])
    }
  }

  @objc public func open(_ call: CAPPluginCall) {
    guard #available(iOS 17.0, *) else {
      call.reject("This iOS cannot send a web view through a proxy.", "UNAVAILABLE")
      return
    }
    let domains = (call.getArray("allowedDomains", String.self) ?? []).compactMap(Self.domain)
    let denied = (call.getArray("deniedDomains", String.self) ?? []).compactMap(Self.domain)
    guard let link = call.getString("url"), let url = URL(string: link),
      ["http", "https"].contains(url.scheme?.lowercased() ?? ""),
      let host = call.getString("proxyHost"), !host.isEmpty,
      let port = call.getInt("proxyPort"), let proxyPort = NWEndpoint.Port(rawValue: UInt16(clamping: port)),
      let token = call.getString("token"), !token.isEmpty,
      !domains.isEmpty
    else {
      call.reject("The private link or the gateway proxy is not valid.")
      return
    }
    DispatchQueue.main.async { [weak self] in
      guard let presenter = self?.bridge?.viewController else {
        call.reject("No window for the browser.")
        return
      }
      let browser = ProxyBrowserController(
        url: url,
        proxy: ProxyBrowserRoute(host: host, port: proxyPort, token: token, domains: domains, denied: denied))
      let navigation = UINavigationController(rootViewController: browser)
      navigation.modalPresentationStyle = .fullScreen
      var top = presenter
      while let shown = top.presentedViewController { top = shown }
      top.present(navigation, animated: true)
      call.resolve()
    }
  }

  /// `*.example.com` and `example.com` both give `example.com`: a match domain covers each
  /// subdomain.
  private static func domain(_ pattern: String) -> String? {
    var key = pattern.trimmingCharacters(in: .whitespaces).lowercased()
    if key.hasPrefix("*.") { key.removeFirst(2) }
    while key.hasSuffix(".") { key.removeLast() }
    return key.isEmpty || key == "*" ? nil : key
  }
}

struct ProxyBrowserRoute {
  let host: String
  let port: NWEndpoint.Port
  let token: String
  let domains: [String]
  let denied: [String]
}

@available(iOS 17.0, *)
final class ProxyBrowserController: UIViewController, WKNavigationDelegate, WKUIDelegate {
  private let start: URL
  private let proxy: ProxyBrowserRoute
  private var web: WKWebView!
  private let progress = UIProgressView(progressViewStyle: .bar)
  private var observers: [NSKeyValueObservation] = []
  private lazy var back = UIBarButtonItem(
    image: UIImage(systemName: "chevron.backward"), style: .plain, target: self, action: #selector(goBack))
  private lazy var forward = UIBarButtonItem(
    image: UIImage(systemName: "chevron.forward"), style: .plain, target: self, action: #selector(goForward))

  init(url: URL, proxy: ProxyBrowserRoute) {
    start = url
    self.proxy = proxy
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) { nil }

  override func viewDidLoad() {
    super.viewDidLoad()
    view.backgroundColor = .systemBackground
    let store = WKWebsiteDataStore.nonPersistent()
    var config = ProxyConfiguration(
      httpCONNECTProxy: .hostPort(host: NWEndpoint.Host(proxy.host), port: proxy.port), tlsOptions: nil)
    config.matchDomains = proxy.domains
    // Denied domains go direct. The gateway refuses them in any case.
    config.excludedDomains = proxy.denied
    config.applyCredential(username: proxy.token, password: "vis")
    store.proxyConfigurations = [config]
    let configuration = WKWebViewConfiguration()
    configuration.websiteDataStore = store
    web = WKWebView(frame: .zero, configuration: configuration)
    web.navigationDelegate = self
    web.uiDelegate = self
    web.allowsBackForwardNavigationGestures = true
    web.translatesAutoresizingMaskIntoConstraints = false
    progress.translatesAutoresizingMaskIntoConstraints = false
    view.addSubview(web)
    view.addSubview(progress)
    NSLayoutConstraint.activate([
      web.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
      web.bottomAnchor.constraint(equalTo: view.bottomAnchor),
      web.leadingAnchor.constraint(equalTo: view.leadingAnchor),
      web.trailingAnchor.constraint(equalTo: view.trailingAnchor),
      progress.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
      progress.leadingAnchor.constraint(equalTo: view.leadingAnchor),
      progress.trailingAnchor.constraint(equalTo: view.trailingAnchor),
    ])
    navigationItem.leftBarButtonItem = UIBarButtonItem(
      barButtonSystemItem: .close, target: self, action: #selector(close))
    navigationItem.rightBarButtonItem = UIBarButtonItem(
      barButtonSystemItem: .refresh, target: self, action: #selector(reload))
    toolbarItems = [back, UIBarButtonItem(systemItem: .fixedSpace), forward]
    navigationController?.isToolbarHidden = false
    title = start.host
    observers = [
      web.observe(\.title) { [weak self] web, _ in
        if let text = web.title, !text.isEmpty { self?.title = text }
      },
      web.observe(\.estimatedProgress) { [weak self] web, _ in
        self?.progress.setProgress(Float(web.estimatedProgress), animated: true)
        self?.progress.isHidden = web.estimatedProgress >= 1
      },
      web.observe(\.canGoBack) { [weak self] web, _ in self?.back.isEnabled = web.canGoBack },
      web.observe(\.canGoForward) { [weak self] web, _ in self?.forward.isEnabled = web.canGoForward },
    ]
    back.isEnabled = false
    forward.isEnabled = false
    web.load(URLRequest(url: start))
  }

  @objc private func close() { dismiss(animated: true) }
  @objc private func reload() { web.reload() }
  @objc private func goBack() { web.goBack() }
  @objc private func goForward() { web.goForward() }

  /// mailto:, tel: and app links go to the app of the phone that opens them.
  func webView(
    _ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
    decisionHandler: @escaping (WKNavigationActionPolicy) -> Void
  ) {
    guard let url = action.request.url, let scheme = url.scheme?.lowercased() else {
      decisionHandler(.cancel)
      return
    }
    if ["http", "https", "about", "blob", "data"].contains(scheme) {
      decisionHandler(.allow)
      return
    }
    UIApplication.shared.open(url)
    decisionHandler(.cancel)
  }

  /// A link that opens a new window opens in this browser.
  func webView(
    _ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration,
    for action: WKNavigationAction, windowFeatures: WKWindowFeatures
  ) -> WKWebView? {
    if action.targetFrame == nil { webView.load(action.request) }
    return nil
  }

  /// The proxy of the gateway asks for the gateway token as the user name.
  func webView(
    _ webView: WKWebView, didReceive challenge: URLAuthenticationChallenge,
    completionHandler: @escaping (URLSession.AuthChallengeDisposition, URLCredential?) -> Void
  ) {
    let space = challenge.protectionSpace
    if space.isProxy(), space.host.caseInsensitiveCompare(proxy.host) == .orderedSame,
      challenge.previousFailureCount < 2
    {
      completionHandler(.useCredential, URLCredential(user: proxy.token, password: "vis", persistence: .forSession))
    } else {
      completionHandler(.performDefaultHandling, nil)
    }
  }
}
