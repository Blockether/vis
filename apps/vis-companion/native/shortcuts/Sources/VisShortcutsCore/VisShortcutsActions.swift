import Foundation

/// The session that a Shortcut action uses.
public enum VisSessionTarget: Equatable {
  /// A new session.
  case new
  /// The session that the last Shortcut action used on this machine, else a new one.
  case last
  /// The session with this id.
  case id(String)
}

/// A model as `provider/name`, the form that Shortcuts store.
///
/// Only the first slash separates the parts, because model names can hold slashes.
public struct VisModelRef: Equatable {
  public let provider: String
  public let name: String

  public init(provider: String, name: String) {
    self.provider = provider
    self.name = name
  }

  public init?(_ text: String?) {
    let trimmed = (text ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
    guard let slash = trimmed.firstIndex(of: "/") else { return nil }
    let provider = String(trimmed[..<slash])
    let name = String(trimmed[trimmed.index(after: slash)...])
    guard !provider.isEmpty, !name.isEmpty else { return nil }
    self.init(provider: provider, name: name)
  }

  public var text: String { "\(provider)/\(name)" }
}

/// A Shortcut action failed before it reached the gateway.
public struct VisShortcutsError: Error, LocalizedError, Equatable {
  public let message: String
  public init(_ message: String) { self.message = message }
  public var errorDescription: String? { message }
}

/// The result of `send`: the turn runs on, and `wait` reads its result later.
public struct VisSentTurn: Equatable {
  public let sessionID: String
  public let turnID: String
}

/// The Shortcut actions, with the same operations and defaults as the Python SDK.
///
/// The App Intents layer only converts parameters and results. Each action that
/// uses a session saves it as the last session of its machine.
public struct VisShortcutsActions {
  public static let lastSessionPrefix = "vis.shortcuts.lastSession."

  public let defaults: UserDefaults
  public let protocolVersion: Int
  public let minGatewayProtocol: Int
  public let urlSession: URLSession
  public var pollInterval: TimeInterval = 0.5

  public init(
    protocolVersion: Int, minGatewayProtocol: Int, defaults: UserDefaults = .standard,
    urlSession: URLSession = .shared
  ) {
    self.protocolVersion = protocolVersion
    self.minGatewayProtocol = minGatewayProtocol
    self.defaults = defaults
    self.urlSession = urlSession
  }

  /// The paired machine with this URL or gateway id, else the primary machine.
  public func connection(_ machine: String?) throws -> VisConnection {
    guard let connection = VisConnections.find(machine, in: defaults) else {
      if let machine, !machine.isEmpty {
        throw VisShortcutsError("No paired machine matches \(machine). Pair it in the Vis app.")
      }
      throw VisShortcutsError("No machine is paired. Pair a machine in the Vis app first.")
    }
    return connection
  }

  public func client(_ machine: String?) throws -> VisClient {
    let client = VisClient(
      connection: try connection(machine), protocolVersion: protocolVersion,
      minGatewayProtocol: minGatewayProtocol, urlSession: urlSession)
    client.pollInterval = pollInterval
    return client
  }

  static func machineKey(_ connection: VisConnection) -> String {
    if let id = connection.id, !id.isEmpty { return id }
    return connection.url
  }

  /// The id of the last session that a Shortcut action used on this machine.
  public func lastSession(_ connection: VisConnection) -> String? {
    defaults.string(forKey: Self.lastSessionPrefix + Self.machineKey(connection))
  }

  func remember(_ session: VisSession) {
    defaults.set(session.id, forKey: Self.lastSessionPrefix + Self.machineKey(session.client.connection))
  }

  static func pair(_ model: String?) throws -> (String?, String?) {
    guard let model, !model.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
      return (nil, nil)
    }
    guard let ref = VisModelRef(model) else {
      throw VisShortcutsError("Give the model as provider/name, for example openai/gpt-5.")
    }
    return (ref.provider, ref.name)
  }

  func session(_ client: VisClient, _ target: VisSessionTarget) async throws -> VisSession {
    switch target {
    case .id(let id):
      let trimmed = id.trimmingCharacters(in: .whitespacesAndNewlines)
      guard !trimmed.isEmpty else { throw VisShortcutsError("Give a session id.") }
      return client.session(trimmed)
    case .last:
      if let id = lastSession(client.connection) { return client.session(id) }
      return try await client.createSession()
    case .new:
      return try await client.createSession()
    }
  }

  /// The models of the machine, like the SDK's `models`.
  public func models(machine: String? = nil, provider: String? = nil, isAvailable: Bool? = nil)
    async throws -> [VisModel]
  {
    let provider = provider?.trimmingCharacters(in: .whitespacesAndNewlines)
    return try await client(machine).models(
      provider: provider?.isEmpty == false ? provider : nil, isAvailable: isAvailable)
  }

  /// Create a session, like the SDK's `create_session`. A model pins the session.
  public func createSession(machine: String? = nil, model: String? = nil) async throws -> String {
    let (provider, name) = try Self.pair(model)
    let session = try await client(machine).createSession(provider: provider, model: name)
    remember(session)
    return session.id
  }

  /// Submit a request and wait for its result, like the SDK's `Session.run`.
  ///
  /// A schema makes the answer one JSON value that matches it, with up to
  /// `maxCorrections` correction turns.
  public func run(
    _ request: String, machine: String? = nil, session target: VisSessionTarget = .new,
    model: String? = nil, responseSchema: String? = nil, maxCorrections: Int = 2,
    timeout: TimeInterval = 300
  ) async throws -> VisRunResult {
    let request = try Self.request(request)
    let (provider, name) = try Self.pair(model)
    let schema = responseSchema?.trimmingCharacters(in: .whitespacesAndNewlines)
    let contract = try schema.flatMap { $0.isEmpty ? nil : try VisResponseContract(schemaText: $0) }
    let session = try await session(client(machine), target)
    remember(session)
    return try await session.run(
      request, timeout: timeout, responseSchema: contract, maxCorrections: maxCorrections,
      provider: provider, model: name)
  }

  /// Submit a request without a wait, like the SDK's `Session.send`.
  public func send(
    _ request: String, machine: String? = nil, session target: VisSessionTarget = .new,
    model: String? = nil
  ) async throws -> VisSentTurn {
    let request = try Self.request(request)
    let (provider, name) = try Self.pair(model)
    let session = try await session(client(machine), target)
    remember(session)
    let turn = try await session.send(request, provider: provider, model: name)
    return VisSentTurn(sessionID: session.id, turnID: turn.id)
  }

  /// Wait for a turn and return its final text, like the SDK's `Turn.wait`.
  public func wait(
    sessionID: String, turnID: String, machine: String? = nil, timeout: TimeInterval = 300
  ) async throws -> VisRunResult {
    let session = try await session(client(machine), .id(sessionID))
    let turn = try await session.turn(turnID).wait(timeout: timeout)
    return VisRunResult(sessionID: session.id, turn: turn, text: visFinalAnswer(turn), value: nil)
  }

  /// Read the model of the next turn, like the SDK's `Session.model`.
  public func sessionModel(machine: String? = nil, session target: VisSessionTarget = .last)
    async throws -> VisSessionModel
  {
    try await session(client(machine), try Self.existing(target)).model()
  }

  /// Pin the model of a session, or clear the pin, like the SDK's `Session.set_model`.
  public func setSessionModel(
    _ model: String?, machine: String? = nil, session target: VisSessionTarget = .last
  ) async throws -> VisSessionModel {
    let (provider, name) = try Self.pair(model)
    let session = try await session(client(machine), try Self.existing(target))
    remember(session)
    return try await session.setModel(provider: provider, model: name)
  }

  /// Recent sessions, or the sessions that match `query`.
  public func searchSessions(machine: String? = nil, query: String = "", limit: Int = 20)
    async throws -> [VisSessionSummary]
  {
    try await client(machine).searchSessions(query: query, limit: limit)
  }

  /// The link that opens the app in a session and starts a voice conversation.
  ///
  /// `at` makes each link new, so the deep-link dedupe never drops a press.
  public func talkURL(
    machine: String? = nil, session target: VisSessionTarget = .new, now: Date = Date()
  ) throws -> URL {
    let connection = try connection(machine)
    let session: String
    switch target {
    case .new: session = "new"
    case .last: session = lastSession(connection) ?? "new"
    case .id(let id):
      session = id.trimmingCharacters(in: .whitespacesAndNewlines)
      guard !session.isEmpty else { throw VisShortcutsError("Give a session id.") }
    }
    var components = URLComponents()
    components.scheme = "vis"
    components.host = "talk"
    components.queryItems = [
      URLQueryItem(name: "machine", value: connection.url),
      URLQueryItem(name: "session", value: session),
      URLQueryItem(name: "at", value: String(Int(now.timeIntervalSince1970 * 1000))),
    ]
    guard let url = components.url else { throw VisShortcutsError("The talk link is invalid.") }
    return url
  }

  static func request(_ text: String) throws -> String {
    guard !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
      throw VisShortcutsError("Give a request for Vis.")
    }
    return text
  }

  static func existing(_ target: VisSessionTarget) throws -> VisSessionTarget {
    guard target != .new else { throw VisShortcutsError("Choose the last session or a session id.") }
    return target
  }
}
