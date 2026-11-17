import Foundation

/// Bounded PCM accounting. The owner serializes all calls.
final class LiveVoiceState {
  enum Failure: Error { case stale, overflow, invalid, voiceProcessingUnavailable }
  enum CaptureReadiness { case waiting, ready, invalid }

  static func captureReadiness(
    running: Bool, starting: Bool,
    inputEnabled: Bool, outputEnabled: Bool, bypassed: Bool,
    inputRate: Double, outputRate: Double,
    inputChannels: UInt32, outputChannels: UInt32
  ) -> CaptureReadiness {
    if !running { return starting ? .waiting : .invalid }
    guard inputEnabled, outputEnabled, !bypassed,
      inputRate.isFinite, inputRate > 0, outputRate.isFinite, outputRate > 0,
      inputChannels > 0, outputChannels > 0
    else { return .invalid }
    if inputRate != outputRate || inputChannels != outputChannels {
      return starting ? .waiting : .invalid
    }
    return .ready
  }

  static func allowsRouteChange(_ reason: UInt?, starting: Bool) -> Bool {
    // AVAudioSession categoryChange and routeConfigurationChange.
    reason == 3 || (starting && reason == 8)
  }
  let frameSamples: Int
  let inputBufferFrames: Int
  let outputBufferSamples: Int
  private(set) var captureId = 0
  private(set) var outputId = 0
  private(set) var capturing = false
  private(set) var sequence = 0
  private(set) var queuedSamples = 0
  private var pendingCapture = Set<Int>()
  private var pendingOutput: [Int: Int] = [:]
  private var partial: [Int16] = []
  private var lastFrameId = -1

  init(frameSamples: Int, inputBufferFrames: Int, outputBufferSamples: Int) throws {
    guard frameSamples == 480, inputBufferFrames == 100, outputBufferSamples == 24000 else {
      throw Failure.invalid
    }
    self.frameSamples = frameSamples
    self.inputBufferFrames = inputBufferFrames
    self.outputBufferSamples = outputBufferSamples
  }

  func startCapture(_ id: Int) throws {
    guard id > captureId else { throw Failure.stale }
    captureId = id
    capturing = true
    sequence = 0
    partial.removeAll(keepingCapacity: true)
    pendingCapture.removeAll(keepingCapacity: true)
  }

  @discardableResult func stopCapture(_ id: Int) throws -> Bool {
    guard id >= captureId else { throw Failure.stale }
    if id == captureId { return false }
    let wasCapturing = capturing
    captureId = id
    capturing = false
    partial.removeAll(keepingCapacity: true)
    pendingCapture.removeAll(keepingCapacity: true)
    return wasCapturing
  }

  func capture(_ samples: [Int16], id: Int) throws -> [(Int, Data)] {
    guard capturing, id == captureId else { throw Failure.stale }
    guard samples.count <= 4096 else { throw Failure.overflow }
    partial.append(contentsOf: samples)
    var frames: [(Int, Data)] = []
    while partial.count >= frameSamples {
      guard pendingCapture.count < inputBufferFrames else { throw Failure.overflow }
      let pcm = partial.prefix(frameSamples).flatMap { sample -> [UInt8] in
        let bits = UInt16(bitPattern: sample)
        return [UInt8(truncatingIfNeeded: bits), UInt8(truncatingIfNeeded: bits >> 8)]
      }
      partial.removeFirst(frameSamples)
      pendingCapture.insert(sequence)
      frames.append((sequence, Data(pcm)))
      sequence += 1
    }
    return frames
  }

  func acknowledge(_ sequence: Int, id: Int) throws {
    guard id == captureId else { throw Failure.stale }
    pendingCapture.remove(sequence)
  }

  func enqueue(_ data: Data, id: Int, frame: Int) throws -> [Int16] {
    guard id == outputId, frame > lastFrameId else { throw Failure.stale }
    guard !data.isEmpty, data.count % 2 == 0, data.count <= frameSamples * 2 else {
      throw Failure.invalid
    }
    let samples = data.count / 2
    guard queuedSamples + samples <= outputBufferSamples else { throw Failure.overflow }
    let bytes = [UInt8](data)
    let pcm = stride(from: 0, to: bytes.count, by: 2).map {
      Int16(bitPattern: UInt16(bytes[$0]) | UInt16(bytes[$0 + 1]) << 8)
    }
    pendingOutput[frame] = samples
    queuedSamples += samples
    lastFrameId = frame
    return pcm
  }

  func consumed(id: Int, frame: Int) -> Int? {
    guard id == outputId, let samples = pendingOutput.removeValue(forKey: frame) else { return nil }
    queuedSamples -= samples
    return samples
  }

  @discardableResult func stopOutput(_ id: Int) throws -> Bool {
    guard id >= outputId else { throw Failure.stale }
    if id == outputId { return false }
    outputId = id
    queuedSamples = 0
    pendingOutput.removeAll(keepingCapacity: true)
    return true
  }
}
