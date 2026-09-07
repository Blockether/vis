import Capacitor
import SafariServices

/// System Safari view keeps the initiating app alive while its loopback receiver waits.
/// It is not a WKWebView and cannot inspect the provider's page, cookies or credentials.
@objc(OAuthLoopbackPlugin)
public class OAuthLoopbackPlugin: CAPPlugin, CAPBridgedPlugin, SFSafariViewControllerDelegate {
  public let identifier = "OAuthLoopbackPlugin"
  public let jsName = "OAuthLoopback"
  public let pluginMethods: [CAPPluginMethod] = [
    CAPPluginMethod(name: "authorize", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "reopen", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "cancel", returnType: CAPPluginReturnPromise),
  ]
  private var flowId: String?
  private var pending: CAPPluginCall?
  private var receiver: OAuthLoopback?
  private var browser: SFSafariViewController?

  @objc public func authorize(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      guard let self, self.pending == nil, let id = call.getString("flowId"), !id.isEmpty,
        let url = call.getString("authorizationUrl"), let redirect = call.getString("redirectUri"),
        let state = call.getString("state"), let deadline = call.getDouble("expiresAt"),
        let presenter = self.bridge?.viewController, presenter.presentedViewController == nil
      else {
        call.reject("Cannot start sign-in on this host.")
        return
      }
      do {
        let receiver = try OAuthLoopback(
          authorizationURL: url, redirectURI: redirect, state: state,
          expiresAt: Date(timeIntervalSince1970: deadline / 1000))
        self.flowId = id
        self.pending = call
        self.receiver = receiver
        receiver.start(
          ready: { [weak self] in
            guard let self, self.flowId == id else { return }
            let safari = SFSafariViewController(url: receiver.authorizationURL)
            safari.delegate = self
            safari.modalPresentationStyle = .fullScreen
            self.browser = safari
            presenter.present(safari, animated: true)
          },
          completion: { [weak self] result in
            guard let self, self.flowId == id else { return }
            self.finish(url: try? result.get())
          })
      } catch { call.reject("Cannot receive this sign-in callback.") }
    }
  }
  @objc public func reopen(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      guard let self, self.flowId == call.getString("flowId"), self.browser != nil else {
        call.reject("No active sign-in browser.")
        return
      }
      call.resolve()
    }
  }
  @objc public func cancel(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      if let self, self.flowId == call.getString("flowId") { self.finish(url: nil) }
      call.resolve()
    }
  }
  public func safariViewControllerDidFinish(_ controller: SFSafariViewController) {
    if browser === controller { finish(url: nil) }
  }
  private func finish(url: String?) {
    guard let call = pending else { return }
    pending = nil
    flowId = nil
    receiver?.cancel()
    receiver = nil
    let safari = browser
    browser = nil
    let complete = {
      if let url { call.resolve(["url": url]) } else { call.reject("Sign-in closed or expired.") }
    }
    if let safari, safari.presentingViewController != nil {
      safari.dismiss(animated: true, completion: complete)
    } else {
      complete()
    }
  }
  deinit { receiver?.cancel() }
}
