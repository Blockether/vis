import ActivityKit
import AppIntents
import Foundation

// The app target and the VisVoice widget extension both compile this file.

/// The route from a lock-screen button to the live voice plugin. The system runs
/// the intents below in the app process. Use it on the main thread only.
enum VisVoiceControl {
  /// Set by the live voice plugin while a session runs.
  static var handler: ((String) -> Void)?

  @available(iOS 17.0, *)
  @MainActor static func send(_ control: String) async {
    if let handler {
      handler(control)
      return
    }
    // No session owns the activity, because the app started again. Remove the dead controls.
    for activity in Activity<VisVoiceAttributes>.activities {
      await activity.end(nil, dismissalPolicy: .immediate)
    }
  }
}

@available(iOS 17.0, *)
struct VisVoiceAttributes: ActivityAttributes {
  /// The state that a locked phone shows. It holds no transcript and no session title.
  struct ContentState: Codable, Hashable {
    var state: String
    var muted: Bool
    var speaking: Bool
    var task: String?
    /// False when unmute needs a new microphone start, which needs the open app.
    var resumable: Bool

    init?(state: String?, muted: Bool, speaking: Bool, task: String?, resumable: Bool) {
      guard let state,
        ["connecting", "listening", "speaking", "muted", "waiting", "recording", "transcribing"]
          .contains(state),
        task == nil || ["running", "queued", "cancelling"].contains(task)
      else { return nil }
      self.state = state
      self.muted = muted
      self.speaking = speaking
      self.task = task
      self.resumable = resumable
    }

    var connecting: Bool { state == "connecting" }
    /// A dictation in the composer, not a voice session.
    var dictation: Bool { state == "recording" || state == "transcribing" }

    var title: String {
      switch state {
      case "listening": return "Listening"
      case "speaking": return "Speaking"
      case "muted": return "Microphone muted"
      case "waiting": return "Needs your answer"
      case "recording": return "Recording"
      case "transcribing": return "Transcribing"
      default: return "Connecting"
      }
    }

    var detail: String? {
      if state == "recording" { return "Stop puts your words in the message" }
      if state == "transcribing" { return "Your words go to the message" }
      if state == "waiting" { return "Open Vis to answer" }
      if state == "muted" { return resumable ? "Vis cannot hear you" : "Open Vis to unmute" }
      switch task {
      case "running": return "Task running"
      case "queued": return "Task queued"
      case "cancelling": return "Cancellation requested"
      default: return nil
      }
    }

    var symbol: String {
      switch state {
      case "listening": return "waveform"
      case "speaking": return "speaker.wave.2.fill"
      case "muted": return "mic.slash.fill"
      case "waiting": return "exclamationmark.bubble.fill"
      case "recording": return "record.circle"
      case "transcribing": return "text.bubble.fill"
      default: return "ellipsis"
      }
    }
  }

  var startedAt: Date
}

/// Mute, stop speech, end, or stop a dictation. It needs no unlock.
@available(iOS 17.0, *)
struct VisVoiceControlIntent: LiveActivityIntent {
  static var title: LocalizedStringResource = "Control Vis voice"
  static var isDiscoverable: Bool = false

  @Parameter(title: "Control") var control: String

  init() {}
  init(_ control: String) { self.control = control }

  @MainActor func perform() async throws -> some IntentResult {
    await VisVoiceControl.send(control)
    return .result()
  }
}

/// Unmute lets the audio of the open microphone through again.
@available(iOS 18.0, *)
struct VisVoiceUnmuteIntent: AudioRecordingIntent, LiveActivityIntent {
  static var title: LocalizedStringResource = "Unmute Vis voice"
  static var isDiscoverable: Bool = false

  @MainActor func perform() async throws -> some IntentResult {
    await VisVoiceControl.send("unmute")
    return .result()
  }
}
