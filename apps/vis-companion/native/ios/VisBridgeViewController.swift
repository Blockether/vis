/// Pushes UIKit's own view size into the web layer.
///
/// WKWebView's layout viewport is not a reliable ruler on iOS: after a rotation
/// it keeps the previous orientation's size for several frames, and after the
/// app is resumed from a long suspension it can come back larger than the
/// screen and stay that way, because iOS then fires no viewport event at all.
/// Everything the web layer can measure — `innerHeight`, `100dvh`,
/// `visualViewport` — describes that same wrong box, so the shell renders
/// taller than the device and the composer and tab bar hang off the bottom.
///
/// No Capacitor plugin covers this: the orientation plugins report which way up
/// the device is (which `screen.orientation` already answers), not the size.
/// UIKit has the size, exactly and early — `viewWillTransition(to:)` is handed
/// the post-rotation size before a frame is drawn, its coordinator reports when
/// the animation is over, and `view.bounds` on activation is authoritative
/// while the webview's own numbers are still stale.
///
/// So this controller dispatches those points as the `visviewport` window
/// event; `src/lib/native-viewport.ts` consumes it and every rule in
/// `src/lib/viewport-metrics.ts` prefers it over the `screen`-derived guess.
/// The web layer works unchanged when the event never arrives (web, Android).
class VisBridgeViewController: CAPBridgeViewController {
    private var lastReported: CGSize = .zero

    override func viewDidLoad() {
        super.viewDidLoad()
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(visApplicationDidBecomeActive),
            name: UIApplication.didBecomeActiveNotification,
            object: nil
        )
    }

    deinit {
        NotificationCenter.default.removeObserver(self)
    }

    override func viewWillTransition(
        to size: CGSize,
        with coordinator: UIViewControllerTransitionCoordinator
    ) {
        super.viewWillTransition(to: size, with: coordinator)
        // The target size, one frame before the flip is drawn.
        report(phase: "rotate", size: size)
        coordinator.animate(alongsideTransition: nil) { [weak self] _ in
            guard let self = self else { return }
            self.report(phase: "settled", size: self.view.bounds.size)
        }
    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        // Split view, Stage Manager, any relayout the web layer is not told
        // about. Deduplicated so the rotation animation does not spam it.
        let size = view.bounds.size
        guard size != lastReported else { return }
        report(phase: "layout", size: size)
    }

    @objc private func visApplicationDidBecomeActive() {
        // Force a layout pass first: the resume bug is precisely the case where
        // the webview kept a box UIKit has already corrected.
        view.setNeedsLayout()
        view.layoutIfNeeded()
        report(phase: "resume", size: view.bounds.size)
    }

    private func report(phase: String, size: CGSize) {
        guard size.width > 0, size.height > 0 else { return }
        lastReported = size
        // Points, which are CSS pixels here: the webview cannot zoom.
        let width = Int(size.width.rounded())
        let height = Int(size.height.rounded())
        bridge?.triggerWindowJSEvent(
            eventName: "visviewport",
            data: "{\"phase\":\"\(phase)\",\"width\":\(width),\"height\":\(height)}"
        )
    }
}
