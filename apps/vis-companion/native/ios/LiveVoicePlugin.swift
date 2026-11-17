import AVFoundation
import Capacitor
import UIKit
import WebKit

/// PCM transport. No recognition, synthesis or network requests occur here.
/// A session starts in the foreground and continues behind a locked screen.
@objc(LiveVoicePlugin)
public final class LiveVoicePlugin: CAPPlugin, CAPBridgedPlugin {
  public let identifier = "LiveVoicePlugin"
  public let jsName = "LiveVoice"
  public let pluginMethods: [CAPPluginMethod] = [
    CAPPluginMethod(name: "getCapabilities", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "prepare", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "startCapture", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "stopCapture", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "acknowledgeCapture", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "play", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "stopOutput", returnType: CAPPluginReturnPromise),
    CAPPluginMethod(name: "close", returnType: CAPPluginReturnPromise),
  ]
  private var sessionId: String?
  private var state: LiveVoiceState?
  private var engine: AVAudioEngine?
  private var player: AVAudioPlayerNode?
  private var observers: [NSObjectProtocol] = []
  private var permissionCall: CAPPluginCall?
  private var hasTap = false
  private var audioActive = false
  private var pipeline = UUID()
  private var capturePipeline = UUID()
  private var loggedCaptureBuffer = false

  private func diagnostic(_ message: @autoclosure () -> String) {
    #if DEBUG
      NSLog("[LiveVoice] %@", message())
    #endif
  }

  private func diagnosticRoute(_ event: String) {
    #if DEBUG
      let audio = AVAudioSession.sharedInstance()
      let inputs = audio.currentRoute.inputs.map { $0.portType.rawValue }.joined(separator: ",")
      let outputs = audio.currentRoute.outputs.map { $0.portType.rawValue }.joined(separator: ",")
      diagnostic(
        "\(event) inputs=\(inputs) outputs=\(outputs) rate=\(audio.sampleRate) engineRunning=\(engine?.isRunning ?? false)"
      )
    #endif
  }

  private let format = AVAudioFormat(standardFormatWithSampleRate: 24000, channels: 1)!

  private var capabilities: [String: Any] {
    #if targetEnvironment(simulator)
      return [
        "platform": "ios", "available": false, "echoCancellation": false,
        "requiresVerification": true, "reason": "Voice processing requires a physical iOS device.",
      ]
    #else
      if ProcessInfo.processInfo.isiOSAppOnMac {
        return [
          "platform": "ios", "available": false, "echoCancellation": false,
          "requiresVerification": true, "reason": "Live mobile audio requires an iOS device.",
        ]
      }
      return [
        "platform": "ios", "available": true, "echoCancellation": true,
        "requiresVerification": true,
      ]
    #endif
  }

  private var appActive: Bool { UIApplication.shared.applicationState == .active }

  public override func load() {
    // The voice session logic runs in the web view. From iOS 17, WebKit suspends
    // web content that is not visible unless the policy says otherwise.
    if #available(iOS 17.0, *) {
      bridge?.webView?.configuration.preferences.inactiveSchedulingPolicy = .none
    }
  }

  deinit {
    for observer in observers { NotificationCenter.default.removeObserver(observer) }
    if hasTap { engine?.inputNode.removeTap(onBus: 0) }
    player?.stop()
    engine?.stop()
    if audioActive {
      try? AVAudioSession.sharedInstance().setActive(false, options: .notifyOthersOnDeactivation)
    }
  }

  public override func shouldOverrideLoad(_ navigationAction: WKNavigationAction) -> NSNumber? {
    if navigationAction.targetFrame?.isMainFrame == true {
      fail("Live audio stopped because the page changed.")
    }
    return nil
  }

  @objc public func getCapabilities(_ call: CAPPluginCall) { call.resolve(capabilities) }

  private func command(_ call: CAPPluginCall, _ action: @escaping () throws -> Void) {
    DispatchQueue.main.async {
      do { try action() } catch {
        self.diagnostic("command rejected code=\((error as NSError).code)")
        call.reject("The live audio command is unavailable or stale.")
      }
    }
  }

  private func owner(_ call: CAPPluginCall) throws -> LiveVoiceState {
    guard let id = call.getString("sessionId"), id == sessionId, let state else {
      throw LiveVoiceState.Failure.stale
    }
    return state
  }

  private func integer(_ call: CAPPluginCall, _ key: String) throws -> Int {
    guard let n = call.getDouble(key), n.isFinite, n >= 0, n <= 9_007_199_254_740_991,
      n.rounded(.towardZero) == n
    else { throw LiveVoiceState.Failure.invalid }
    return Int(n)
  }

  @objc public func prepare(_ call: CAPPluginCall) {
    command(call) {
      guard self.sessionId == nil, let id = call.getString("sessionId"), !id.isEmpty,
        id.count <= 256, try self.integer(call, "sampleRate") == 24000,
        self.capabilities["available"] as? Bool == true, self.appActive
      else {
        throw LiveVoiceState.Failure.invalid
      }
      self.state = try LiveVoiceState(
        frameSamples: self.integer(call, "frameSamples"),
        inputBufferFrames: self.integer(call, "inputBufferFrames"),
        outputBufferSamples: self.integer(call, "outputBufferSamples"))
      self.sessionId = id
      self.observeLifecycle()
      self.diagnosticRoute("prepared")
      // No engine, active audio session or permission request exists until start or play.
      call.resolve(self.capabilities)
    }
  }

  private func observeLifecycle() {
    let center = NotificationCenter.default
    let owner = state
    for name in [
      AVAudioSession.interruptionNotification, AVAudioSession.mediaServicesWereResetNotification,
    ] {
      observers.append(
        center.addObserver(forName: name, object: nil, queue: .main) {
          [weak self] _ in
          guard let self, self.state === owner else { return }
          self.diagnostic("lifecycle notification=\(name.rawValue)")
          self.fail("Live audio stopped when the audio session changed.")
        })
    }
    observers.append(
      center.addObserver(
        forName: UIApplication.willResignActiveNotification, object: nil, queue: .main
      ) { [weak self] _ in
        guard let self, self.state === owner else { return }
        self.diagnostic("lifecycle notification=willResignActive")
        self.holdAudio()
      })
    observers.append(
      center.addObserver(
        forName: AVAudioSession.routeChangeNotification,
        object: nil, queue: .main
      ) { [weak self] note in
        guard let self, self.state === owner else { return }
        let reason = (note.userInfo?[AVAudioSessionRouteChangeReasonKey] as? NSNumber)?.uintValue
        self.diagnosticRoute("route changed reason=\(reason.map { String($0) } ?? "unknown")")
        // Our capture/output transitions change category without changing the physical route.
        if !LiveVoiceState.allowsRouteChange(reason, starting: self.permissionCall != nil) {
          self.fail("Live audio stopped because the audio route changed.")
        }
      })
  }

  /// iOS suspends an app that has no running audio. Keep a playback graph while
  /// the app is not in the foreground, so that the session continues.
  private func holdAudio() {
    guard sessionId != nil, let state, engine == nil, !state.capturing else { return }
    do { try configure(capture: false) } catch {
      diagnostic("background audio failed code=\((error as NSError).code)")
      fail("Live audio could not continue in the background.")
    }
  }

  @objc public func startCapture(_ call: CAPPluginCall) {
    command(call) { [self] in
      let state = try self.owner(call)
      let id = try self.integer(call, "captureId")
      guard !state.capturing, self.permissionCall == nil else {
        throw LiveVoiceState.Failure.invalid
      }
      self.diagnosticRoute("capture requested")
      try state.startCapture(id)
      let owner = self.sessionId
      self.permissionCall = call
      AVAudioSession.sharedInstance().requestRecordPermission { [weak self] allowed in
        DispatchQueue.main.async {
          guard let self, self.sessionId == owner, self.state === state,
            state.captureId == id, state.capturing, self.permissionCall === call
          else { return }
          let background = UIApplication.shared.applicationState == .background
          self.diagnostic(
            "permission allowed=\(allowed) background=\(background) audioActive=\(self.audioActive)"
          )
          guard allowed else {
            self.permissionCall = nil
            call.reject("Microphone access is unavailable.")
            self.fail("Microphone access is unavailable.")
            return
          }
          guard
            LiveVoiceState.allowsCaptureStart(
              background: background, audioActive: self.audioActive)
          else {
            self.permissionCall = nil
            call.reject("Voice cannot start behind a locked screen. Start voice again.")
            self.fail("Voice cannot start behind a locked screen. Start voice again.")
            return
          }
          do {
            try self.configure(capture: true, captureId: id)
            self.awaitCaptureStart(owner: owner, state: state, captureId: id, call: call)
          } catch LiveVoiceState.Failure.voiceProcessingUnavailable {
            self.permissionCall = nil
            call.reject("Echo cancellation could not be verified. Start voice again.")
            self.fail("Echo cancellation could not be verified. Start voice again.")
          } catch {
            self.diagnostic("capture startup failed code=\((error as NSError).code)")
            self.permissionCall = nil
            call.reject("Echo-cancelled capture is unavailable.")
            self.fail("Echo-cancelled capture is unavailable.")
          }
        }
      }
    }
  }

  private func awaitCaptureStart(
    owner: String?, state: LiveVoiceState, captureId: Int,
    call: CAPPluginCall, attempt: Int = 0
  ) {
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) { [weak self] in
      guard let self, self.sessionId == owner, self.state === state,
        state.capturing, state.captureId == captureId, self.permissionCall === call
      else { return }
      guard attempt < 10 else {
        self.fail("Microphone did not deliver audio. Check the audio route and start again.")
        return
      }
      // Voice processing can stop the graph while iOS changes its route.
      do {
        if let graph = self.engine {
          if try !self.captureReady(graph, starting: true) {
            self.diagnosticRoute("restart graph after route setup")
            graph.stop()
            try self.configureCaptureGraph(graph, captureId: captureId)
            graph.prepare()
            try graph.start()
            self.player?.play()
          }
          _ = try self.captureReady(graph, starting: true)
        }
      } catch LiveVoiceState.Failure.voiceProcessingUnavailable {
        self.fail("Echo cancellation could not be verified. Start voice again.")
        return
      } catch {
        self.fail("Microphone could not start after the audio route changed.")
        return
      }
      self.awaitCaptureStart(
        owner: owner, state: state, captureId: captureId,
        call: call, attempt: attempt + 1)
    }
  }

  private func stopEngine(closing: Bool = false) {
    diagnosticRoute("engine stopping")
    pipeline = UUID()
    capturePipeline = UUID()
    if hasTap {
      engine?.inputNode.removeTap(onBus: 0)
      hasTap = false
    }
    player?.stop()
    engine?.stop()
    engine?.reset()
    engine = nil
    player = nil
    if audioActive,
      LiveVoiceState.releasesAudioSession(foreground: appActive, closing: closing)
    {
      audioActive = false
      try? AVAudioSession.sharedInstance().setActive(false, options: .notifyOthersOnDeactivation)
    }
  }

  private func configure(capture: Bool, captureId: Int = 0) throws {
    diagnostic("configure capture=\(capture)")
    stopEngine()
    loggedCaptureBuffer = false
    let audio = AVAudioSession.sharedInstance()
    let headsetWasSelected = audio.currentRoute.outputs.contains {
      $0.portType == .bluetoothA2DP || $0.portType == .bluetoothHFP || $0.portType == .headphones
    }
    if capture {
      try audio.setCategory(
        .playAndRecord, mode: .voiceChat,
        options: [.defaultToSpeaker, .allowBluetoothHFP])
    } else {
      // Muting destroys the input graph rather than leaving a silent recording tap.
      try audio.setCategory(.playback, mode: .default)
    }
    try audio.setPreferredSampleRate(24000)
    try audio.setPreferredIOBufferDuration(0.02)
    try audio.setActive(true)
    audioActive = true
    if capture && headsetWasSelected {
      if let headset = audio.availableInputs?.first(where: {
        $0.portType == .bluetoothHFP || $0.portType == .headsetMic
      }) {
        try audio.setPreferredInput(headset)
      } else {
        throw LiveVoiceState.Failure.invalid
      }
    }
    diagnosticRoute("audio session active")
    let graph = AVAudioEngine()
    engine = graph
    if capture {
      try graph.inputNode.setVoiceProcessingEnabled(true)
      guard graph.inputNode.isVoiceProcessingEnabled else { throw LiveVoiceState.Failure.invalid }
      graph.inputNode.isVoiceProcessingBypassed = false
      graph.inputNode.isVoiceProcessingAGCEnabled = true
      guard !graph.inputNode.isVoiceProcessingBypassed else { throw LiveVoiceState.Failure.invalid }
      diagnostic("voice processing enabled and not bypassed")
    }
    let playback = AVAudioPlayerNode()
    graph.attach(playback)
    graph.connect(playback, to: graph.mainMixerNode, format: format)
    player = playback
    if capture { try configureCaptureGraph(graph, captureId: captureId) }
    graph.prepare()
    try graph.start()
    playback.play()
    if capture { _ = try captureReady(graph, starting: true) }
    diagnosticRoute("engine started")
  }

  private func configureCaptureGraph(_ graph: AVAudioEngine, captureId: Int) throws {
    let input = graph.inputNode
    let captureFormat = input.outputFormat(forBus: 0)
    guard !graph.isRunning, input.isVoiceProcessingEnabled,
      graph.outputNode.isVoiceProcessingEnabled, !input.isVoiceProcessingBypassed,
      captureFormat.sampleRate.isFinite, captureFormat.sampleRate > 0,
      captureFormat.channelCount > 0
    else { throw LiveVoiceState.Failure.voiceProcessingUnavailable }
    if hasTap {
      capturePipeline = UUID()
      input.removeTap(onBus: 0)
      hasTap = false
    }
    // Bluetooth can leave the automatic output connection in its previous A2DP format.
    // Voice processing requires matching input and output connection formats.
    graph.connect(graph.mainMixerNode, to: graph.outputNode, format: captureFormat)
    diagnostic(
      "connect capture IO rate=\(captureFormat.sampleRate) channels=\(captureFormat.channelCount)")
    try installCapture(graph, captureId: captureId)
  }

  private func captureReady(_ graph: AVAudioEngine, starting: Bool) throws -> Bool {
    let input = graph.inputNode
    let output = graph.outputNode
    let inputFormat = input.outputFormat(forBus: 0)
    let outputFormat = output.inputFormat(forBus: 0)
    let inputEnabled = input.isVoiceProcessingEnabled
    let outputEnabled = output.isVoiceProcessingEnabled
    let bypassed = input.isVoiceProcessingBypassed
    let running = graph.isRunning
    let readiness = LiveVoiceState.captureReadiness(
      running: running, starting: starting,
      inputEnabled: inputEnabled, outputEnabled: outputEnabled, bypassed: bypassed,
      inputRate: inputFormat.sampleRate, outputRate: outputFormat.sampleRate,
      inputChannels: inputFormat.channelCount, outputChannels: outputFormat.channelCount)
    if starting || readiness == .invalid {
      diagnostic(
        "capture readiness=\(readiness) starting=\(starting) running=\(running) inputVP=\(inputEnabled) outputVP=\(outputEnabled) bypassed=\(bypassed) inputRate=\(inputFormat.sampleRate) outputRate=\(outputFormat.sampleRate) inputChannels=\(inputFormat.channelCount) outputChannels=\(outputFormat.channelCount)"
      )
    }
    if readiness == .invalid { throw LiveVoiceState.Failure.voiceProcessingUnavailable }
    return readiness == .ready
  }

  private func installCapture(_ graph: AVAudioEngine, captureId: Int) throws {
    let input = graph.inputNode
    let inputFormat = input.outputFormat(forBus: 0)
    diagnostic(
      "install capture tap rate=\(inputFormat.sampleRate) channels=\(inputFormat.channelCount)")
    guard inputFormat.sampleRate > 0, inputFormat.channelCount > 0,
      let converter = AVAudioConverter(from: inputFormat, to: format)
    else {
      throw LiveVoiceState.Failure.invalid
    }
    let owner = sessionId
    let token = UUID()
    capturePipeline = token
    // Bound callback work waiting for the main queue, independently of bridge credits.
    let credits = DispatchSemaphore(value: 4)
    let overflow = NSLock()
    var failed = false
    input.installTap(onBus: 0, bufferSize: 1024, format: inputFormat) { [weak self] buffer, _ in
      guard credits.wait(timeout: .now()) == .success else {
        overflow.lock()
        let first = !failed
        failed = true
        overflow.unlock()
        if first {
          DispatchQueue.main.async { [weak self] in
            guard let self, self.sessionId == owner, self.capturePipeline == token else { return }
            self.fail("Microphone processing exceeded its queue limit.")
          }
        }
        return
      }
      let capacity = Int(ceil(Double(buffer.frameLength) * 24000 / inputFormat.sampleRate)) + 64
      guard capacity <= 4096,
        let converted = AVAudioPCMBuffer(
          pcmFormat: self?.format ?? inputFormat,
          frameCapacity: AVAudioFrameCount(capacity))
      else {
        DispatchQueue.main.async { [weak self] in
          defer { credits.signal() }
          guard let self, self.capturePipeline == token else { return }
          self.fail("Microphone conversion exceeded its frame limit.")
        }
        return
      }
      var supplied = false
      var error: NSError?
      let status = converter.convert(to: converted, error: &error) { _, outStatus in
        if supplied {
          outStatus.pointee = .noDataNow
          return nil
        }
        supplied = true
        outStatus.pointee = .haveData
        return buffer
      }
      var samples: [Int16] = []
      if let floats = converted.floatChannelData?[0], error == nil, status != .error {
        samples = (0..<Int(converted.frameLength)).map { index in
          let value = floats[index]
          return value.isFinite ? Int16(max(-32768, min(32767, (value * 32768).rounded()))) : 0
        }
      }
      let conversionFailed = error != nil || status == .error
      DispatchQueue.main.async { [weak self] in
        defer { credits.signal() }
        guard let self, self.sessionId == owner, self.capturePipeline == token,
          let state = self.state, state.capturing, state.captureId == captureId
        else { return }
        do {
          guard let graph = self.engine else {
            throw LiveVoiceState.Failure.voiceProcessingUnavailable
          }
          guard try self.captureReady(graph, starting: self.permissionCall != nil) else { return }
          if !self.loggedCaptureBuffer {
            self.loggedCaptureBuffer = true
            self.diagnostic(
              "first capture buffer convertedSamples=\(samples.count) conversionFailed=\(conversionFailed)"
            )
          }
          if conversionFailed { throw LiveVoiceState.Failure.invalid }
          for (sequence, data) in try state.capture(samples, id: captureId) {
            if sequence == 0 {
              self.diagnostic("first PCM frame emitted")
              let pending = self.permissionCall
              self.permissionCall = nil
              pending?.resolve()
            }
            self.notifyListeners(
              "capture",
              data: [
                "sessionId": owner!, "captureId": captureId,
                "sequence": sequence, "pcm": data.base64EncodedString(),
              ])
          }
        } catch LiveVoiceState.Failure.voiceProcessingUnavailable {
          self.fail("Echo cancellation could not be verified. Start voice again.")
        } catch { self.fail("Microphone transport exceeded its limit or failed conversion.") }
      }
    }
    hasTap = true
  }

  @objc public func stopCapture(_ call: CAPPluginCall) {
    command(call) {
      let state = try self.owner(call)
      if try state.stopCapture(self.integer(call, "captureId")) {
        self.diagnostic("capture stop requested")
        self.permissionCall?.reject("Microphone capture stopped.")
        self.permissionCall = nil
        self.stopEngine()
        // In the foreground, output starts lazily with a playback-only graph.
        if !self.appActive { self.holdAudio() }
      }
      call.resolve()
    }
  }

  @objc public func acknowledgeCapture(_ call: CAPPluginCall) {
    command(call) {
      try self.owner(call).acknowledge(
        self.integer(call, "sequence"),
        id: self.integer(call, "captureId"))
      call.resolve()
    }
  }

  @objc public func play(_ call: CAPPluginCall) {
    command(call) { [self] in
      let state = try self.owner(call)
      let id = try self.integer(call, "outputId")
      let frame = try self.integer(call, "frameId")
      guard let encoded = call.getString("pcm"), encoded.utf8.count <= 1280,
        let data = Data(base64Encoded: encoded)
      else { throw LiveVoiceState.Failure.invalid }
      do {
        let samples = try state.enqueue(data, id: id, frame: frame)
        if self.engine == nil {
          guard !state.capturing else { throw LiveVoiceState.Failure.invalid }
          try self.configure(capture: false)
        }
        guard let player = self.player,
          let buffer = AVAudioPCMBuffer(
            pcmFormat: self.format,
            frameCapacity: AVAudioFrameCount(samples.count)),
          let channel = buffer.floatChannelData?[0]
        else { throw LiveVoiceState.Failure.invalid }
        buffer.frameLength = AVAudioFrameCount(samples.count)
        for (index, sample) in samples.enumerated() { channel[index] = Float(sample) / 32768 }
        let owner = self.sessionId
        let token = self.pipeline
        player.scheduleBuffer(buffer, completionCallbackType: .dataPlayedBack) { [weak self] _ in
          DispatchQueue.main.async {
            guard let self, self.sessionId == owner, self.pipeline == token,
              let consumed = self.state?.consumed(id: id, frame: frame)
            else { return }
            self.notifyListeners(
              "played",
              data: [
                "sessionId": owner!, "outputId": id,
                "frameId": frame, "playedSamples": consumed,
              ])
          }
        }
        if !player.isPlaying { player.play() }
        call.resolve()
      } catch LiveVoiceState.Failure.stale {
        throw LiveVoiceState.Failure.stale
      } catch {
        self.fail("Live audio playback failed or exceeded its queue limit.")
        throw error
      }
    }
  }

  @objc public func stopOutput(_ call: CAPPluginCall) {
    command(call) {
      if try self.owner(call).stopOutput(self.integer(call, "outputId")) {
        self.player?.stop()
      }
      call.resolve()
    }
  }

  @objc public func close(_ call: CAPPluginCall) {
    command(call) {
      if let id = call.getString("sessionId"), id == self.sessionId { self.teardown() }
      call.resolve()
    }
  }

  private func fail(_ message: String) {
    guard let owner = sessionId else { return }
    diagnostic("failure: \(message)")
    teardown()
    notifyListeners("failure", data: ["sessionId": owner, "message": message])
  }

  private func teardown() {
    diagnosticRoute("teardown")
    sessionId = nil
    state = nil
    permissionCall?.reject("Live audio closed.")
    permissionCall = nil
    for observer in observers { NotificationCenter.default.removeObserver(observer) }
    observers.removeAll()
    stopEngine(closing: true)
  }
}
