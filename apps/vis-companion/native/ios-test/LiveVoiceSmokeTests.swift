import AVFoundation
import Capacitor
import WebKit
import XCTest

@testable import App

@MainActor
final class LiveVoiceSmokeTests: XCTestCase {
  private struct Reply {
    let data: [String: Any]
    let error: String?
  }

  private func controller() async throws -> CAPBridgeViewController {
    #if !targetEnvironment(simulator)
      throw XCTSkip("Run these tests only in an owned simulator.")
    #else
      for _ in 0..<100 {
        let windows = UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }
          .flatMap { $0.windows }
        if let view = windows.compactMap({ $0.rootViewController as? CAPBridgeViewController })
          .first,
          view.bridge != nil, let webView = view.webView, !webView.isLoading
        {
          return view
        }
        try await Task.sleep(nanoseconds: 100_000_000)
      }
      return try XCTUnwrap(nil, "The app did not start its Capacitor bridge.")
    #endif
  }

  private func plugin() async throws -> LiveVoicePlugin {
    let view = try await controller()
    return try XCTUnwrap(view.bridge?.plugin(withName: "LiveVoice") as? LiveVoicePlugin)
  }

  private func invoke(
    _ plugin: LiveVoicePlugin, _ method: String,
    _ options: [String: Any] = [:]
  ) async -> Reply {
    await withCheckedContinuation { continuation in
      let call = CAPPluginCall(
        callbackId: UUID().uuidString, methodName: method, options: options,
        success: { result, _ in
          continuation.resume(returning: Reply(data: result?.data ?? [:], error: nil))
        },
        error: { error in
          continuation.resume(
            returning: Reply(data: [:], error: error?.message ?? "Missing error."))
        })!
      _ = plugin.perform(NSSelectorFromString("\(method):"), with: call)
    }
  }

  private var preparation: [String: Any] {
    [
      "sessionId": "simulator-no-audio", "sampleRate": 24000, "frameSamples": 480,
      "inputBufferFrames": 100, "outputBufferSamples": 24000,
    ]
  }

  func testRegisteredPluginRequiresPhysicalEchoCancellation() async throws {
    let plugin = try await plugin()
    XCTAssertEqual(
      Set(plugin.pluginMethods.map { $0.name }),
      Set([
        "getCapabilities", "prepare", "startCapture", "stopCapture", "acknowledgeCapture",
        "play", "stopOutput", "close",
      ]))
    let reply = await invoke(plugin, "getCapabilities")
    XCTAssertNil(reply.error)
    XCTAssertEqual(reply.data["platform"] as? String, "ios")
    XCTAssertEqual(reply.data["available"] as? Bool, false)
    XCTAssertEqual(reply.data["echoCancellation"] as? Bool, false)
    XCTAssertEqual(reply.data["requiresVerification"] as? Bool, true)
    XCTAssertEqual(
      reply.data["reason"] as? String, "Voice processing requires a physical iOS device.")
    if #available(iOS 17.0, *) {
      XCTAssertEqual(AVAudioApplication.shared.recordPermission, .denied)
    }
  }

  func testRejectedPreparationCannotStartCaptureOrPlayback() async throws {
    let plugin = try await plugin()
    let prepared = await invoke(plugin, "prepare", preparation)
    XCTAssertNotNil(prepared.error)
    let capture = await invoke(
      plugin, "startCapture",
      ["sessionId": "simulator-no-audio", "captureId": 1])
    XCTAssertNotNil(capture.error)
    let playback = await invoke(
      plugin, "play",
      [
        "sessionId": "simulator-no-audio",
        "outputId": 0, "frameId": 0, "pcm": "AAA=",
      ])
    XCTAssertNotNil(playback.error)
    if #available(iOS 17.0, *) {
      XCTAssertEqual(AVAudioApplication.shared.recordPermission, .denied)
    }
  }

  func testCloseIsIdempotentWithoutAnAudioOwner() async throws {
    let plugin = try await plugin()
    for _ in 0..<2 {
      let reply = await invoke(plugin, "close", ["sessionId": "simulator-no-audio"])
      XCTAssertNil(reply.error)
    }
    let reply = await invoke(
      plugin, "stopOutput",
      ["sessionId": "another-session", "outputId": 1])
    XCTAssertNotNil(reply.error)
  }

  func testWebViewReceivesNativeCapabilityAndPreparationResults() async throws {
    let view = try await controller()
    let webView = try XCTUnwrap(view.webView)
    let result: Any? = try await withCheckedThrowingContinuation { continuation in
      webView.callAsyncJavaScript(
        """
        const capabilities = await window.Capacitor.nativePromise('LiveVoice', 'getCapabilities', {});
        const preparation = await window.Capacitor.nativePromise('LiveVoice', 'prepare', options)
          .then(() => ({ rejected: false }), error => ({ rejected: true, message: error.message }));
        return { capabilities, preparation };
        """, arguments: ["options": preparation], in: nil, in: .page
      ) { result in
        switch result {
        case .success(let value): continuation.resume(returning: value)
        case .failure(let error): continuation.resume(throwing: error)
        }
      }
    }
    let reply = try XCTUnwrap(result as? [String: Any])
    let capabilities = try XCTUnwrap(reply["capabilities"] as? [String: Any])
    let preparation = try XCTUnwrap(reply["preparation"] as? [String: Any])
    XCTAssertEqual(capabilities["available"] as? Bool, false)
    XCTAssertEqual(capabilities["requiresVerification"] as? Bool, true)
    XCTAssertEqual(preparation["rejected"] as? Bool, true)
    XCTAssertEqual(
      preparation["message"] as? String,
      "The live audio command is unavailable or stale.")
  }
}
