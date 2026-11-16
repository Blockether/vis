import Capacitor
import UIKit

/// Shows the system edit menu, with Paste only, at a rectangle of the page.
/// The Paste of the system goes to the focused field of the page as a normal paste.
/// Vis does not read the pasteboard, so iOS asks for no permission.
@objc(PasteMenuPlugin)
public class PasteMenuPlugin: CAPPlugin, CAPBridgedPlugin {
  public let identifier = "PasteMenuPlugin"
  public let jsName = "PasteMenu"
  public let pluginMethods: [CAPPluginMethod] = [
    CAPPluginMethod(name: "show", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "move", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "hide", returnType: CAPPluginReturnPromise),
  ]
  /// A `UIEditMenuInteraction`. That type starts at iOS 16, and the app starts at iOS 15.
  private var interaction: UIInteraction?
  fileprivate var target = CGRect.zero
  fileprivate var isShown = false
  fileprivate var dismissedAt = Date.distantPast
  private var presentedAt = Date.distantPast

  @objc public func show(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      guard #available(iOS 16.0, *), let self, let view = self.bridge?.webView,
        let rect = Self.rect(of: call)
      else {
        call.reject("No system paste menu on this host.")
        return
      }
      let menu = self.menu(on: view)
      let now = Date()
      // The second tap of a double tap keeps the menu that the first tap opened.
      let isDoubleTap = now.timeIntervalSince(self.presentedAt) < 0.6
      // A later tap closes the menu, as in a system text field. The system can close it
      // when that tap starts, so the same tap must not open it again.
      if !isDoubleTap, self.isShown || now.timeIntervalSince(self.dismissedAt) < 0.5 {
        menu.dismissMenu()
        call.resolve()
        return
      }
      if self.isShown {
        call.resolve()
        return
      }
      self.presentedAt = now
      self.target = rect
      menu.presentEditMenu(
        with: UIEditMenuConfiguration(
          identifier: nil, sourcePoint: CGPoint(x: self.target.midX, y: self.target.minY)))
      call.resolve()
    }
  }
  /// The page can move the box while the menu shows. The menu goes to its new place.
  @objc public func move(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      if #available(iOS 16.0, *), let self, self.isShown, let rect = Self.rect(of: call),
        let menu = self.interaction as? UIEditMenuInteraction
      {
        self.target = rect
        menu.updateVisibleMenuPosition(animated: false)
      }
      call.resolve()
    }
  }
  @objc public func hide(_ call: CAPPluginCall) {
    DispatchQueue.main.async { [weak self] in
      if #available(iOS 16.0, *), let menu = self?.interaction as? UIEditMenuInteraction {
        menu.dismissMenu()
      }
      call.resolve()
    }
  }
  private static func rect(of call: CAPPluginCall) -> CGRect? {
    guard let x = call.getDouble("x"), let y = call.getDouble("y"),
      let width = call.getDouble("width"), let height = call.getDouble("height")
    else { return nil }
    return CGRect(x: x, y: y, width: width, height: height)
  }
  @available(iOS 16.0, *)
  private func menu(on view: UIView) -> UIEditMenuInteraction {
    if let menu = interaction as? UIEditMenuInteraction { return menu }
    let menu = UIEditMenuInteraction(delegate: self)
    view.addInteraction(menu)
    interaction = menu
    return menu
  }
}

@available(iOS 16.0, *)
extension PasteMenuPlugin: UIEditMenuInteractionDelegate {
  /// The web view offers Paste only while a field of the page has the focus and the
  /// pasteboard has content. Without Paste there is no menu.
  public func editMenuInteraction(
    _ interaction: UIEditMenuInteraction, menuFor configuration: UIEditMenuConfiguration,
    suggestedActions: [UIMenuElement]
  ) -> UIMenu? {
    let paste = Self.pasteCommands(in: suggestedActions)
    return paste.isEmpty ? nil : UIMenu(children: paste)
  }
  public func editMenuInteraction(
    _ interaction: UIEditMenuInteraction, targetRectFor configuration: UIEditMenuConfiguration
  ) -> CGRect { target }
  public func editMenuInteraction(
    _ interaction: UIEditMenuInteraction, willPresentMenuFor configuration: UIEditMenuConfiguration,
    animator: UIEditMenuInteractionAnimating
  ) { isShown = true }
  public func editMenuInteraction(
    _ interaction: UIEditMenuInteraction, willDismissMenuFor configuration: UIEditMenuConfiguration,
    animator: UIEditMenuInteractionAnimating
  ) {
    isShown = false
    dismissedAt = Date()
  }
  private static func pasteCommands(in elements: [UIMenuElement]) -> [UIMenuElement] {
    elements.flatMap { element -> [UIMenuElement] in
      if let command = element as? UICommand {
        return command.action == #selector(UIResponderStandardEditActions.paste(_:)) ? [command] : []
      }
      if let menu = element as? UIMenu { return pasteCommands(in: menu.children) }
      return []
    }
  }
}
