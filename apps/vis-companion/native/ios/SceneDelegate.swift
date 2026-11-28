import Capacitor
import UIKit

/// iOS 27 stops an app at launch when it does not use the scene life cycle.
/// UIKit loads Main.storyboard into `window` from the scene manifest in Info.plist.
/// Capacitor gets the URLs and the user activities that open the app.
class SceneDelegate: UIResponder, UIWindowSceneDelegate {
  var window: UIWindow?

  func scene(
    _ scene: UIScene, willConnectTo session: UISceneSession,
    options connectionOptions: UIScene.ConnectionOptions
  ) {
    // A fast turn of the phone is a shake, and a shake shows the "Undo Typing" alert
    // for the composer. The alert only interrupts.
    UIApplication.shared.applicationSupportsShakeToEdit = false
    SceneDelegateProxy.shared.scene(scene, willConnectTo: session, options: connectionOptions)
  }

  func scene(_ scene: UIScene, openURLContexts URLContexts: Set<UIOpenURLContext>) {
    SceneDelegateProxy.shared.scene(scene, openURLContexts: URLContexts)
  }

  func scene(_ scene: UIScene, continue userActivity: NSUserActivity) {
    SceneDelegateProxy.shared.scene(scene, continue: userActivity)
  }

  func sceneWillResignActive(_ scene: UIScene) {
    // Release WebKit's editor before UIKit starts the scene transition.
    window?.endEditing(true)
  }
}
