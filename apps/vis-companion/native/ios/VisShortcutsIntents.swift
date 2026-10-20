import AppIntents
import UIKit

// The App Intents of the Companion. `scripts/ios-prepare.mjs` puts this file
// after `VisGatewayHandshake` and the `VisShortcutsCore` sources, in one file of
// the app target. Each action only converts values: `VisShortcutsActions` does
// the work, with the operations and defaults of the Python SDK.

@available(iOS 16.0, *)
func visActions() -> VisShortcutsActions {
  VisShortcutsActions(
    protocolVersion: VisGatewayHandshake.protocolVersion,
    minGatewayProtocol: VisGatewayHandshake.minGatewayProtocol)
}

/// The error text that Shortcuts shows for a failed action.
@available(iOS 16.0, *)
struct VisIntentError: Error, CustomLocalizedStringResourceConvertible {
  let message: String
  var localizedStringResource: LocalizedStringResource { "\(message)" }
}

@available(iOS 16.0, *)
func visIntent<T>(_ work: () async throws -> T) async throws -> T {
  do {
    return try await work()
  } catch let error as VisIntentError {
    throw error
  } catch {
    throw VisIntentError(message: (error as? LocalizedError)?.errorDescription ?? "\(error)")
  }
}

// MARK: - Entities

@available(iOS 16.0, *)
struct VisMachineEntity: AppEntity {
  static var typeDisplayRepresentation: TypeDisplayRepresentation = "Vis Machine"
  static var defaultQuery = VisMachineQuery()

  let id: String
  let name: String

  init(_ connection: VisConnection) {
    id = connection.url
    name = connection.displayName
  }

  var displayRepresentation: DisplayRepresentation { DisplayRepresentation(title: "\(name)") }
}

@available(iOS 16.0, *)
struct VisMachineQuery: EntityQuery {
  func entities(for identifiers: [String]) async throws -> [VisMachineEntity] {
    identifiers.compactMap { VisConnections.find($0).map(VisMachineEntity.init) }
  }

  func suggestedEntities() async throws -> [VisMachineEntity] {
    VisConnections.all().map(VisMachineEntity.init)
  }
}

@available(iOS 16.0, *)
struct VisModelEntity: AppEntity {
  static var typeDisplayRepresentation: TypeDisplayRepresentation = "Vis Model"
  static var defaultQuery = VisModelQuery()

  /// The model as `provider/name`.
  let id: String
  @Property(title: "Provider") var provider: String
  @Property(title: "Name") var name: String
  @Property(title: "Label") var label: String
  @Property(title: "Billing") var billing: String
  @Property(title: "Is Available") var isAvailable: Bool
  @Property(title: "Is Default") var isDefault: Bool

  init(ref: VisModelRef, label: String? = nil, billing: String = "", isAvailable: Bool = true,
       isDefault: Bool = false) {
    id = ref.text
    self.provider = ref.provider
    self.name = ref.name
    self.label = label ?? ref.name
    self.billing = billing
    self.isAvailable = isAvailable
    self.isDefault = isDefault
  }

  init(_ model: VisModel) {
    self.init(
      ref: VisModelRef(provider: model.provider, name: model.name), label: model.label,
      billing: model.billing, isAvailable: model.isAvailable, isDefault: model.isDefault)
  }

  var displayRepresentation: DisplayRepresentation {
    DisplayRepresentation(title: "\(label)", subtitle: "\(provider)")
  }
}

@available(iOS 16.0, *)
struct VisModelQuery: EntityStringQuery {
  func entities(for identifiers: [String]) async throws -> [VisModelEntity] {
    identifiers.compactMap { VisModelRef($0).map { VisModelEntity(ref: $0) } }
  }

  func suggestedEntities() async throws -> [VisModelEntity] {
    try await visActions().models(isAvailable: true).map(VisModelEntity.init)
  }

  func entities(matching string: String) async throws -> [VisModelEntity] {
    let needle = string.lowercased()
    return try await suggestedEntities().filter {
      $0.id.lowercased().contains(needle) || $0.label.lowercased().contains(needle)
    }
  }
}

@available(iOS 16.0, *)
struct VisSessionEntity: AppEntity {
  static var typeDisplayRepresentation: TypeDisplayRepresentation = "Vis Session"
  static var defaultQuery = VisSessionQuery()

  let id: String
  @Property(title: "Title") var title: String

  init(id: String, title: String? = nil) {
    self.id = id
    self.title = title?.isEmpty == false ? title! : id
  }

  init(_ summary: VisSessionSummary) {
    self.init(id: summary.id, title: summary.title)
  }

  var displayRepresentation: DisplayRepresentation { DisplayRepresentation(title: "\(title)") }
}

@available(iOS 16.0, *)
struct VisSessionQuery: EntityStringQuery {
  func entities(for identifiers: [String]) async throws -> [VisSessionEntity] {
    identifiers.map { VisSessionEntity(id: $0) }
  }

  func suggestedEntities() async throws -> [VisSessionEntity] {
    try await visActions().searchSessions().map(VisSessionEntity.init)
  }

  func entities(matching string: String) async throws -> [VisSessionEntity] {
    try await visActions().searchSessions(query: string).map(VisSessionEntity.init)
  }
}

/// A submitted turn. The id is `session id` and `turn id`, joined by a space.
@available(iOS 16.0, *)
struct VisTurnEntity: AppEntity {
  static var typeDisplayRepresentation: TypeDisplayRepresentation = "Vis Turn"
  static var defaultQuery = VisTurnQuery()

  let id: String
  @Property(title: "Session ID") var sessionID: String
  @Property(title: "Turn ID") var turnID: String

  init(sessionID: String, turnID: String) {
    id = "\(sessionID) \(turnID)"
    self.sessionID = sessionID
    self.turnID = turnID
  }

  var displayRepresentation: DisplayRepresentation { DisplayRepresentation(title: "\(turnID)") }
}

@available(iOS 16.0, *)
struct VisTurnQuery: EntityQuery {
  func entities(for identifiers: [String]) async throws -> [VisTurnEntity] {
    identifiers.compactMap { id in
      let parts = id.split(separator: " ", maxSplits: 1).map(String.init)
      return parts.count == 2 ? VisTurnEntity(sessionID: parts[0], turnID: parts[1]) : nil
    }
  }
}

@available(iOS 16.0, *)
enum VisSessionMode: String, AppEnum {
  case new, last, chosen

  static var typeDisplayRepresentation: TypeDisplayRepresentation = "Vis Session Mode"
  static var caseDisplayRepresentations: [VisSessionMode: DisplayRepresentation] = [
    .new: "New session",
    .last: "Last session",
    .chosen: "Chosen session",
  ]

  func target(_ session: VisSessionEntity?) throws -> VisSessionTarget {
    switch self {
    case .new: return .new
    case .last: return .last
    case .chosen:
      guard let session else { throw VisIntentError(message: "Choose a session.") }
      return .id(session.id)
    }
  }
}

// MARK: - Intents

/// The Action Button entry: open the app and start a voice conversation.
@available(iOS 16.0, *)
struct TalkToVisIntent: AppIntent {
  static var title: LocalizedStringResource = "Talk to Vis"
  static var description = IntentDescription(
    "Open Vis in a session and start a voice conversation. Assign it to the Action Button.")
  static var openAppWhenRun = true

  @Parameter(title: "Session", default: .new) var mode: VisSessionMode
  @Parameter(title: "Chosen session") var session: VisSessionEntity?
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  @MainActor
  func perform() async throws -> some IntentResult {
    let url = try await visIntent {
      try visActions().talkURL(machine: machine?.id, session: try mode.target(session))
    }
    _ = await UIApplication.shared.open(url)
    return .result()
  }
}

/// `Session.run` of the SDK: send a request and return the answer.
@available(iOS 16.0, *)
struct AskVisIntent: AppIntent {
  static var title: LocalizedStringResource = "Ask Vis"
  static var description = IntentDescription(
    "Send a request to Vis and return the final answer. With a response schema, the answer is JSON that matches the schema.")

  @Parameter(title: "Request", requestValueDialog: "What should Vis do?") var request: String
  @Parameter(title: "Session", default: .new) var mode: VisSessionMode
  @Parameter(title: "Chosen session") var session: VisSessionEntity?
  @Parameter(title: "Model") var model: VisModelEntity?
  @Parameter(title: "Response schema", description: "A JSON Schema for a structured answer.",
             inputOptions: String.IntentInputOptions(multiline: true))
  var responseSchema: String?
  @Parameter(title: "Max corrections", default: 2, inclusiveRange: (0, 10)) var maxCorrections: Int
  @Parameter(title: "Timeout (seconds)", default: 300, inclusiveRange: (1, 3600)) var timeout: Int
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<String> & ProvidesDialog {
    let result = try await visIntent {
      try await visActions().run(
        request, machine: machine?.id, session: try mode.target(session), model: model?.id,
        responseSchema: responseSchema, maxCorrections: maxCorrections,
        timeout: TimeInterval(timeout))
    }
    let answer = result.value?.pythonText ?? result.text ?? ""
    return .result(value: answer, dialog: "\(answer)")
  }
}

/// `Session.send` of the SDK: submit a request and return its turn without a wait.
@available(iOS 16.0, *)
struct SendVisRequestIntent: AppIntent {
  static var title: LocalizedStringResource = "Send Vis Request"
  static var description = IntentDescription(
    "Submit a request to Vis without a wait. Use Wait for Vis Answer to get the result.")

  @Parameter(title: "Request") var request: String
  @Parameter(title: "Session", default: .new) var mode: VisSessionMode
  @Parameter(title: "Chosen session") var session: VisSessionEntity?
  @Parameter(title: "Model") var model: VisModelEntity?
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<VisTurnEntity> {
    let sent = try await visIntent {
      try await visActions().send(
        request, machine: machine?.id, session: try mode.target(session), model: model?.id)
    }
    return .result(value: VisTurnEntity(sessionID: sent.sessionID, turnID: sent.turnID))
  }
}

/// `Turn.wait` of the SDK: wait for a turn and return its final text.
@available(iOS 16.0, *)
struct WaitForVisAnswerIntent: AppIntent {
  static var title: LocalizedStringResource = "Wait for Vis Answer"
  static var description = IntentDescription("Wait for a submitted turn and return its final text.")

  @Parameter(title: "Turn") var turn: VisTurnEntity
  @Parameter(title: "Timeout (seconds)", default: 300, inclusiveRange: (1, 3600)) var timeout: Int
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<String> {
    let result = try await visIntent {
      try await visActions().wait(
        sessionID: turn.sessionID, turnID: turn.turnID, machine: machine?.id,
        timeout: TimeInterval(timeout))
    }
    return .result(value: result.text ?? "")
  }
}

/// `models` of the SDK.
@available(iOS 16.0, *)
struct GetVisModelsIntent: AppIntent {
  static var title: LocalizedStringResource = "Get Vis Models"
  static var description = IntentDescription("List the models of a machine.")

  @Parameter(title: "Provider") var provider: String?
  @Parameter(title: "Only available", default: true) var isAvailable: Bool
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<[VisModelEntity]> {
    let models = try await visIntent {
      try await visActions().models(
        machine: machine?.id, provider: provider, isAvailable: isAvailable ? true : nil)
    }
    return .result(value: models.map(VisModelEntity.init))
  }
}

/// `create_session` of the SDK. A model pins the session.
@available(iOS 16.0, *)
struct CreateVisSessionIntent: AppIntent {
  static var title: LocalizedStringResource = "Create Vis Session"
  static var description = IntentDescription("Create a session. A model pins it for each turn.")

  @Parameter(title: "Model") var model: VisModelEntity?
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<VisSessionEntity> {
    let id = try await visIntent {
      try await visActions().createSession(machine: machine?.id, model: model?.id)
    }
    return .result(value: VisSessionEntity(id: id))
  }
}

/// Recent sessions, or the sessions that match a search.
@available(iOS 16.0, *)
struct FindVisSessionsIntent: AppIntent {
  static var title: LocalizedStringResource = "Find Vis Sessions"
  static var description = IntentDescription("List recent sessions, or the sessions that match a search.")

  @Parameter(title: "Search") var query: String?
  @Parameter(title: "Limit", default: 20, inclusiveRange: (1, 100)) var limit: Int
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<[VisSessionEntity]> {
    let sessions = try await visIntent {
      try await visActions().searchSessions(machine: machine?.id, query: query ?? "", limit: limit)
    }
    return .result(value: sessions.map(VisSessionEntity.init))
  }
}

/// `Session.model` of the SDK: the model of the next turn as `provider/name`.
@available(iOS 16.0, *)
struct GetVisSessionModelIntent: AppIntent {
  static var title: LocalizedStringResource = "Get Vis Session Model"
  static var description = IntentDescription(
    "Return the model of the next turn as provider/name: the pin, else the default.")

  @Parameter(title: "Session", default: .last) var mode: VisSessionMode
  @Parameter(title: "Chosen session") var session: VisSessionEntity?
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult & ReturnsValue<String> {
    let model = try await visIntent {
      try await visActions().sessionModel(machine: machine?.id, session: try mode.target(session))
    }
    let provider = model.provider ?? model.defaultProvider
    let name = model.model ?? model.defaultModel
    return .result(value: provider.flatMap { p in name.map { "\(p)/\($0)" } } ?? "")
  }
}

/// `Session.set_model` of the SDK. No model clears the pin.
@available(iOS 16.0, *)
struct SetVisSessionModelIntent: AppIntent {
  static var title: LocalizedStringResource = "Set Vis Session Model"
  static var description = IntentDescription("Pin the model of a session. No model clears the pin.")

  @Parameter(title: "Model") var model: VisModelEntity?
  @Parameter(title: "Session", default: .last) var mode: VisSessionMode
  @Parameter(title: "Chosen session") var session: VisSessionEntity?
  @Parameter(title: "Machine") var machine: VisMachineEntity?

  func perform() async throws -> some IntentResult {
    _ = try await visIntent {
      try await visActions().setSessionModel(
        model?.id, machine: machine?.id, session: try mode.target(session))
    }
    return .result()
  }
}

// The payload has to reach the running app: the composer is in the webview.
@available(iOS 16.0, *)
struct SendToVisIntent: AppIntent {
  static var title: LocalizedStringResource = "Send to Vis"
  static var description = IntentDescription("Drop a link or a note into the Vis composer.")
  static var openAppWhenRun = true

  @Parameter(title: "Text or link", requestValueDialog: "What should Vis get?")
  var content: String

  @Parameter(title: "Title")
  var noteTitle: String?

  @MainActor
  func perform() async throws -> some IntentResult {
    let trimmed = content.trimmingCharacters(in: .whitespacesAndNewlines)
    guard !trimmed.isEmpty else { return .result() }

    var query = [URLQueryItem]()
    let isLink = !trimmed.contains(where: \.isWhitespace)
      && (trimmed.hasPrefix("http://") || trimmed.hasPrefix("https://"))
    query.append(URLQueryItem(name: isLink ? "url" : "text", value: trimmed))
    if let noteTitle, !noteTitle.isEmpty {
      query.append(URLQueryItem(name: "title", value: noteTitle))
    }

    // A nonce: running the shortcut twice on the same link must produce two
    // DIFFERENT URLs, or the deep-link dedupe (src/lib/deeplink.ts) drops one.
    query.append(URLQueryItem(name: "at", value: String(Int(Date().timeIntervalSince1970 * 1000))))

    var components = URLComponents()
    components.scheme = "vis"
    components.host = "share"
    components.queryItems = query
    if let url = components.url {
      _ = await UIApplication.shared.open(url)
    }
    return .result()
  }
}

@available(iOS 16.0, *)
struct VisShortcuts: AppShortcutsProvider {
  static var appShortcuts: [AppShortcut] {
    AppShortcut(
      intent: TalkToVisIntent(),
      phrases: [
        "Talk to \(.applicationName)",
        "Start a conversation in \(.applicationName)",
      ],
      shortTitle: "Talk to Vis",
      systemImageName: "waveform"
    )
    AppShortcut(
      intent: AskVisIntent(),
      phrases: ["Ask \(.applicationName)"],
      shortTitle: "Ask Vis",
      systemImageName: "text.bubble"
    )
    AppShortcut(
      intent: SendToVisIntent(),
      phrases: [
        "Send to \(.applicationName)",
        "Add to \(.applicationName)",
        "Send this to \(.applicationName)",
      ],
      shortTitle: "Send to Vis",
      systemImageName: "paperplane"
    )
  }
}
