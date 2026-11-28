import ActivityKit
import AppIntents
import SwiftUI
import WidgetKit

// The Live Activity of a voice session: the lock screen and the Dynamic Island.

@main
struct VisVoiceBundle: WidgetBundle {
  var body: some Widget { VisVoiceLiveActivity() }
}

struct VisVoiceLiveActivity: Widget {
  var body: some WidgetConfiguration {
    ActivityConfiguration(for: VisVoiceAttributes.self) { context in
      VStack(alignment: .leading, spacing: 12) {
        HStack {
          Text(context.state.dictation ? "Vis dictation" : "Vis voice")
            .font(.subheadline.weight(.medium))
          Spacer()
          VoiceTimer(start: context.attributes.startedAt)
        }
        .foregroundStyle(.white.opacity(0.65))
        HStack(spacing: 12) {
          VoiceIcon(state: context.state).font(.system(size: 30))
          VStack(alignment: .leading, spacing: 2) {
            Text(context.state.title).font(.system(size: 22, weight: .semibold))
            if let detail = context.state.detail {
              Text(detail).font(.system(size: 14)).foregroundStyle(.white.opacity(0.65))
            }
          }
        }
        TranscriptionProgress(state: context.state)
        VoiceControls(state: context.state)
      }
      .foregroundStyle(.white)
      .padding(16)
      .activityBackgroundTint(Color.black.opacity(0.8))
      .activitySystemActionForegroundColor(.white)
    } dynamicIsland: { context in
      DynamicIsland {
        DynamicIslandExpandedRegion(.leading) {
          VoiceIcon(state: context.state).font(.title2)
        }
        DynamicIslandExpandedRegion(.trailing) {
          VoiceTimer(start: context.attributes.startedAt)
        }
        DynamicIslandExpandedRegion(.center) {
          VStack(spacing: 2) {
            Text(context.state.title).font(.headline)
            if let detail = context.state.detail {
              Text(detail).font(.caption).foregroundStyle(.secondary)
            }
          }
        }
        DynamicIslandExpandedRegion(.bottom) {
          TranscriptionProgress(state: context.state)
          VoiceControls(state: context.state)
        }
      } compactLeading: {
        VoiceIcon(state: context.state)
      } compactTrailing: {
        VoiceTimer(start: context.attributes.startedAt).frame(maxWidth: 48)
      } minimal: {
        VoiceIcon(state: context.state)
      }
    }
  }
}

private let voiceAccent = Color(red: 1, green: 0.769, blue: 0.125)

private struct VoiceIcon: View {
  let state: VisVoiceAttributes.ContentState

  var body: some View {
    Image(systemName: state.symbol)
      .foregroundStyle(state.muted ? Color.gray : voiceAccent)
      .accessibilityLabel(state.title)
  }
}

/// The bar of a measured transcription phase.
private struct TranscriptionProgress: View {
  let state: VisVoiceAttributes.ContentState

  var body: some View {
    if state.state == "transcribing", let progress = state.progress {
      ProgressView(value: Double(progress), total: 100).tint(voiceAccent)
    }
  }
}

private struct VoiceTimer: View {
  let start: Date

  var body: some View {
    Text(timerInterval: start...Date.distantFuture, countsDown: false)
      .font(.subheadline)
      .monospacedDigit()
      .multilineTextAlignment(.trailing)
  }
}

/// Three controls in fixed positions. Only End works while the session connects.
/// A dictation has one control, Stop, and a transcription has none.
private struct VoiceControls: View {
  let state: VisVoiceAttributes.ContentState

  var body: some View {
    if state.state == "recording" {
      VoiceButton(
        title: "Stop", symbol: "stop.fill", tone: .end,
        intent: VisVoiceControlIntent("stop_recording"))
    } else if !state.dictation {
      sessionControls
    }
  }

  private var sessionControls: some View {
    HStack(spacing: 8) {
      if !state.muted {
        VoiceButton(
          title: "Mute", symbol: "mic.slash.fill", tone: .plain,
          intent: VisVoiceControlIntent("mute")
        )
        .disabled(state.connecting)
      } else if #available(iOS 18.0, *) {
        VoiceButton(
          title: "Unmute", symbol: "mic.fill", tone: .filled, intent: VisVoiceUnmuteIntent()
        )
        .disabled(state.connecting || !state.resumable)
      } else {
        VoiceButton(
          title: "Unmute", symbol: "mic.fill", tone: .filled,
          intent: VisVoiceControlIntent("unmute")
        )
        .disabled(state.connecting || !state.resumable)
      }
      VoiceButton(
        title: "Stop speech", symbol: "stop.fill", tone: .plain,
        intent: VisVoiceControlIntent("stop_speech")
      )
      .disabled(!state.speaking)
      VoiceButton(
        title: "End", symbol: "xmark", tone: .end, intent: VisVoiceControlIntent("end"))
    }
  }
}

private struct VoiceButton<Intent: AppIntent>: View {
  enum Tone { case plain, filled, end }

  let title: String
  let symbol: String
  let tone: Tone
  let intent: Intent
  @Environment(\.isEnabled) private var enabled

  private var foreground: Color {
    switch tone {
    case .plain: return .white
    case .filled: return .black
    case .end: return Color(red: 1, green: 0.42, blue: 0.4)
    }
  }

  private var background: Color {
    switch tone {
    case .plain: return .white.opacity(0.16)
    case .filled: return .white
    case .end: return Color(red: 0.35, green: 0.06, blue: 0.06)
    }
  }

  var body: some View {
    Button(intent: intent) {
      Label(title, systemImage: symbol)
        .font(.subheadline.weight(.semibold))
        .lineLimit(1)
        .minimumScaleFactor(0.8)
        .frame(maxWidth: .infinity, minHeight: 44)
        .foregroundStyle(foreground)
        .background(background, in: Capsule())
    }
    .buttonStyle(.plain)
    .opacity(enabled ? 1 : 0.45)
  }
}
