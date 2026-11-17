import Foundation
import XCTest

@testable import LiveVoice

final class LiveVoiceStateTests: XCTestCase {
  private func state() throws -> LiveVoiceState {
    try LiveVoiceState(frameSamples: 480, inputBufferFrames: 100, outputBufferSamples: 24000)
  }

  // Regression: iOS reports reason 8 while the voice route starts.
  func testStartupRouteChangesDoNotHideDeviceLoss() {
    XCTAssertTrue(LiveVoiceState.allowsRouteChange(3, starting: false))
    XCTAssertTrue(LiveVoiceState.allowsRouteChange(8, starting: true))
    XCTAssertFalse(LiveVoiceState.allowsRouteChange(8, starting: false))
    for reason: UInt in [0, 1, 2, 4, 6, 7] {
      XCTAssertFalse(LiveVoiceState.allowsRouteChange(reason, starting: true))
    }
    XCTAssertFalse(LiveVoiceState.allowsRouteChange(nil, starting: true))
  }

  // Regression: a locked screen ended the voice session.
  func testLockedScreenKeepsTheAudioSession() {
    // iOS can refuse a new activation in the background.
    XCTAssertFalse(LiveVoiceState.releasesAudioSession(foreground: false, closing: false))
    XCTAssertTrue(LiveVoiceState.releasesAudioSession(foreground: true, closing: false))
    XCTAssertTrue(LiveVoiceState.releasesAudioSession(foreground: false, closing: true))
    // A live session can restart capture. A new session cannot start behind the lock.
    XCTAssertTrue(LiveVoiceState.allowsCaptureStart(background: true, audioActive: true))
    XCTAssertFalse(LiveVoiceState.allowsCaptureStart(background: true, audioActive: false))
    XCTAssertTrue(LiveVoiceState.allowsCaptureStart(background: false, audioActive: false))
  }

  // Regression: capture must not trust voice processing before route setup completes.
  func testCaptureReadinessWaitsOnlyDuringStartup() {
    XCTAssertEqual(readiness(running: false), .waiting)
    XCTAssertEqual(readiness(running: false, starting: false), .invalid)
    XCTAssertEqual(readiness(), .ready)
    XCTAssertEqual(readiness(starting: false), .ready)
  }

  func testCaptureReadinessRejectsLostProcessing() {
    XCTAssertEqual(readiness(inputEnabled: false), .invalid)
    XCTAssertEqual(readiness(outputEnabled: false), .invalid)
    XCTAssertEqual(readiness(bypassed: true), .invalid)
    XCTAssertEqual(readiness(starting: false, inputRate: 24000), .invalid)
    XCTAssertEqual(readiness(starting: false, inputChannels: 2), .invalid)
    for rate in [0.0, -1.0, Double.nan, Double.infinity] {
      XCTAssertEqual(readiness(inputRate: rate, outputRate: rate), .invalid)
    }
    XCTAssertEqual(readiness(inputChannels: 0, outputChannels: 0), .invalid)
    XCTAssertEqual(readiness(inputRate: 24000, outputRate: 24000), .ready)
  }

  // Regression: Bluetooth can retain an A2DP output format during HFP startup.
  func testBluetoothStartupWaitsForMatchingVoiceFormats() {
    XCTAssertEqual(
      readiness(inputRate: 16000, outputRate: 44100, outputChannels: 2), .waiting)
    XCTAssertEqual(
      readiness(running: false, inputRate: 16000, outputRate: 44100, outputChannels: 2),
      .waiting)
    XCTAssertEqual(
      readiness(starting: false, inputRate: 16000, outputRate: 44100, outputChannels: 2),
      .invalid)
    XCTAssertEqual(readiness(inputRate: 16000, outputRate: 16000), .ready)
    XCTAssertEqual(readiness(outputRate: 0), .invalid)
    XCTAssertEqual(readiness(outputChannels: 0), .invalid)
  }

  private func readiness(
    running: Bool = true, starting: Bool = true,
    inputEnabled: Bool = true, outputEnabled: Bool = true, bypassed: Bool = false,
    inputRate: Double = 48000, outputRate: Double = 48000,
    inputChannels: UInt32 = 1, outputChannels: UInt32 = 1
  ) -> LiveVoiceState.CaptureReadiness {
    LiveVoiceState.captureReadiness(
      running: running, starting: starting,
      inputEnabled: inputEnabled, outputEnabled: outputEnabled, bypassed: bypassed,
      inputRate: inputRate, outputRate: outputRate,
      inputChannels: inputChannels, outputChannels: outputChannels)
  }

  func testCanonicalLimits() throws {
    XCTAssertThrowsError(
      try LiveVoiceState(
        frameSamples: 481, inputBufferFrames: 100,
        outputBufferSamples: 24000))
    XCTAssertThrowsError(
      try LiveVoiceState(
        frameSamples: 480, inputBufferFrames: 101,
        outputBufferSamples: 24000))
    XCTAssertThrowsError(
      try LiveVoiceState(
        frameSamples: 480, inputBufferFrames: 100,
        outputBufferSamples: 24001))
  }

  func testCaptureFramesAreSequentialLittleEndian() throws {
    let state = try state()
    try state.startCapture(1)
    XCTAssertTrue(try state.capture(Array(repeating: -32768, count: 479), id: 1).isEmpty)
    let frames = try state.capture([32767] + Array(repeating: 1234, count: 480), id: 1)
    XCTAssertEqual(frames.map { $0.0 }, [0, 1])
    XCTAssertEqual(frames[0].1.count, 960)
    XCTAssertEqual(Array(frames[0].1.prefix(2)), [0, 128])
    XCTAssertEqual(Array(frames[0].1.suffix(2)), [255, 127])
  }

  func testCaptureCreditsBoundUnacknowledgedFrames() throws {
    let state = try state()
    try state.startCapture(1)
    for _ in 0..<100 { _ = try state.capture(Array(repeating: 0, count: 480), id: 1) }
    try state.acknowledge(0, id: 1)
    try state.acknowledge(0, id: 1)
    XCTAssertEqual(try state.capture(Array(repeating: 0, count: 480), id: 1).first?.0, 100)
    XCTAssertThrowsError(try state.capture(Array(repeating: 0, count: 480), id: 1))
  }

  func testCaptureGenerationRejectsStaleDataAndAcknowledgements() throws {
    let state = try state()
    try state.startCapture(1)
    _ = try state.capture([12], id: 1)
    XCTAssertTrue(try state.stopCapture(2))
    XCTAssertFalse(try state.stopCapture(2))
    XCTAssertThrowsError(try state.capture([12], id: 1))
    XCTAssertThrowsError(try state.startCapture(2))
    try state.startCapture(3)
    XCTAssertThrowsError(try state.stopCapture(2))
    XCTAssertThrowsError(try state.acknowledge(0, id: 1))
    XCTAssertTrue(try state.capture(Array(repeating: 0, count: 479), id: 3).isEmpty)
    XCTAssertEqual(try state.capture([0], id: 3).first?.0, 0)
  }

  func testCaptureRejectsUnboundedConversion() throws {
    let state = try state()
    try state.startCapture(1)
    XCTAssertThrowsError(try state.capture(Array(repeating: 0, count: 4097), id: 1))
  }

  func testPlaybackDoesNotAcknowledgeEnqueue() throws {
    let state = try state()
    XCTAssertEqual(try state.enqueue(Data([0, 128, 255, 127]), id: 0, frame: 0), [-32768, 32767])
    XCTAssertEqual(state.queuedSamples, 2)
    XCTAssertEqual(state.consumed(id: 0, frame: 0), 2)
    XCTAssertNil(state.consumed(id: 0, frame: 0))
    XCTAssertEqual(state.queuedSamples, 0)
  }

  func testPlaybackCreditsAreBounded() throws {
    let state = try state()
    for frame in 0..<50 {
      _ = try state.enqueue(Data(repeating: 0, count: 960), id: 0, frame: frame)
    }
    XCTAssertEqual(state.queuedSamples, 24000)
    XCTAssertThrowsError(try state.enqueue(Data([0, 0]), id: 0, frame: 50))
    XCTAssertEqual(state.consumed(id: 0, frame: 0), 480)
    _ = try state.enqueue(Data(repeating: 0, count: 960), id: 0, frame: 50)
    XCTAssertEqual(state.queuedSamples, 24000)
  }

  func testOutputStopDropsOldReceiptsAndPreservesNewWork() throws {
    let state = try state()
    _ = try state.enqueue(Data([0, 0]), id: 0, frame: 0)
    XCTAssertTrue(try state.stopOutput(1))
    XCTAssertNil(state.consumed(id: 0, frame: 0))
    XCTAssertThrowsError(try state.enqueue(Data([0, 0]), id: 0, frame: 1))
    _ = try state.enqueue(Data([0, 0]), id: 1, frame: 1)
    XCTAssertFalse(try state.stopOutput(1))
    XCTAssertThrowsError(try state.stopOutput(0))
    XCTAssertEqual(state.queuedSamples, 1)
    XCTAssertEqual(state.consumed(id: 1, frame: 1), 1)
  }

  func testInvalidAndDuplicateFramesCannotReachPlayback() throws {
    let state = try state()
    for data in [Data(), Data([0]), Data(repeating: 0, count: 962)] {
      XCTAssertThrowsError(try state.enqueue(data, id: 0, frame: 0))
    }
    XCTAssertThrowsError(try state.enqueue(Data([0, 0]), id: 1, frame: 0))
    _ = try state.enqueue(Data([0, 0]), id: 0, frame: 0)
    XCTAssertThrowsError(try state.enqueue(Data([0, 0]), id: 0, frame: 0))
  }
}
