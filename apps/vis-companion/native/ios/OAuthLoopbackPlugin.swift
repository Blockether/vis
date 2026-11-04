import AuthenticationServices
import Capacitor

/// The system authentication browser shares Safari's cookies. A sign-in that an email link
/// finished in Safari is reused when the page opens again. Vis cannot read that page.
@objc(OAuthLoopbackPlugin)
public class OAuthLoopbackPlugin: CAPPlugin, CAPBridgedPlugin,
  ASWebAuthenticationPresentationContextProviding
{
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
  private var browser: ASWebAuthenticationSession?

  @objc public func authorize(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      guard let self, self.pending == nil, let id = call.getString("flowId"), !id.isEmpty,
        let url = call.getString("authorizationUrl"), let redirect = call.getString("redirectUri"),
        let state = call.getString("state"), let deadline = call.getDouble("expiresAt")
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
            self.openBrowser()
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
      guard let self, self.flowId == call.getString("flowId"), self.receiver != nil,
        self.openBrowser()
      else {
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
  public func presentationAnchor(for session: ASWebAuthenticationSession) -> ASPresentationAnchor {
    bridge?.viewController?.view.window ?? ASPresentationAnchor()
  }
  /// Closing the browser keeps the receiver until the flow expires or Vis cancels it.
  /// The redirect is an http loopback URL, so the session never matches its callback scheme.
  @discardableResult private func openBrowser() -> Bool {
    guard let receiver else { return false }
    browser?.cancel()
    var session: ASWebAuthenticationSession?
    session = ASWebAuthenticationSession(
      url: receiver.authorizationURL, callbackURLScheme: "vis-oauth-loopback"
    ) { [weak self] _, _ in
      DispatchQueue.main.async {
        if let self, self.browser === session { self.browser = nil }
      }
    }
    guard let session else { return false }
    session.presentationContextProvider = self
    session.prefersEphemeralWebBrowserSession = false
    browser = session
    if session.start() { return true }
    browser = nil
    return false
  }
  private func finish(url: String?) {
    guard let call = pending else { return }
    pending = nil
    flowId = nil
    receiver?.cancel()
    receiver = nil
    browser?.cancel()
    browser = nil
    if let url { call.resolve(["url": url]) } else { call.reject("Sign-in closed or expired.") }
  }
  deinit {
    receiver?.cancel()
    browser?.cancel()
  }
}
