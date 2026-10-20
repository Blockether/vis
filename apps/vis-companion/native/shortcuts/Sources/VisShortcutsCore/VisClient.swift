import Foundation

/// One machine that the Companion paired, as `src/lib/storage.ts` saves it.
public struct VisConnection: Codable, Equatable {
  public var url: String
  public var token: String?
  public var label: String?
  public var id: String?
  public var alts: [String]?

  public init(
    url: String, token: String? = nil, label: String? = nil, id: String? = nil,
    alts: [String]? = nil
  ) {
    self.url = url
    self.token = token
    self.label = label
    self.id = id
    self.alts = alts
  }

  /// The name that people see: the label, else the host of the URL.
  public var displayName: String {
    if let label, !label.isEmpty { return label }
    return URLComponents(string: url)?.host ?? url
  }

  /// The selected address first, then the other addresses of the same gateway.
  public var addresses: [String] {
    var seen = Set<String>()
    return ([url] + (alts ?? [])).filter { !$0.isEmpty && seen.insert($0).inserted }
  }
}

/// The Companion's saved machines. Capacitor Preferences keeps them in `UserDefaults`.
public enum VisConnections {
  public static let prefix = "CapacitorStorage."
  public static let connectionsKey = prefix + "vis.connections"
  public static let primaryKey = prefix + "vis.primaryConnection"
  public static let activeKey = prefix + "vis.activeConnection"

  public static func all(in defaults: UserDefaults = .standard) -> [VisConnection] {
    guard let text = defaults.string(forKey: connectionsKey), let data = text.data(using: .utf8)
    else { return [] }
    return (try? JSONDecoder().decode([VisConnection].self, from: data)) ?? []
  }

  /// The primary machine, as `getPrimaryConnection` chooses it in the app.
  public static func primary(in defaults: UserDefaults = .standard) -> VisConnection? {
    let connections = all(in: defaults)
    let stored = defaults.string(forKey: primaryKey)
    let active = defaults.string(forKey: activeKey)
    return connections.first { $0.url == stored } ?? connections.first { $0.url == active }
      ?? connections.first
  }

  /// The machine with this URL or gateway id, else the primary machine.
  public static func find(_ key: String?, in defaults: UserDefaults = .standard) -> VisConnection? {
    guard let key, !key.isEmpty else { return primary(in: defaults) }
    return all(in: defaults).first { $0.url == key || $0.id == key || $0.addresses.contains(key) }
  }
}

/// The gateway answered with an HTTP error, like the SDK's `GatewayError`.
public struct VisGatewayError: Error, LocalizedError, Equatable {
  public let status: Int
  public let type: String?
  public let message: String
  public var errorDescription: String? { "Vis gateway error \(status): \(message)" }
}

/// No usable answer reached the client, like the SDK's `TransportError`.
public struct VisTransportError: Error, LocalizedError, Equatable {
  public let message: String
  public var errorDescription: String? { message }
}

/// A wait ended before the turn finished, like the SDK's `VisTimeout`.
///
/// The turn still runs. Wait again with its `sessionID` and `turnID`.
public struct VisTimeout: Error, LocalizedError, Equatable {
  public let sessionID: String
  public let turnID: String
  public var errorDescription: String? {
    "turn wait timed out; turn was not cancelled (session \(sessionID), turn \(turnID))"
  }
}

/// A turn ended without a usable text answer.
public struct VisTurnError: Error, LocalizedError {
  public let turn: VisJSON
  public let problem: VisProblem
  public var errorDescription: String? {
    "\(problem.message) (turn \(turn["turn_id"]?.stringValue ?? "unknown"))"
  }
}

/// No valid structured result, like the SDK's `StructuredOutputError`.
///
/// `attempts` has one entry for each turn. `errors` lists the problems of the
/// last turn.
public struct VisStructuredOutputError: Error, LocalizedError {
  public struct Attempt {
    public let turn: VisJSON
    public let answer: String?
    public let errors: [VisProblem]
  }

  public let reason: String
  public let attempts: [Attempt]
  public var turn: VisJSON { attempts.last?.turn ?? .null }
  public var errors: [VisProblem] { attempts.last?.errors ?? [] }

  public var errorDescription: String? {
    "\(reason) (turn \(turn["turn_id"]?.stringValue ?? "unknown"), "
      + "status \(turn["status"]?.stringValue ?? "unknown"))\(visProblemLines(errors))"
  }
}

/// One model of the gateway pickers, like the SDK's `Model`.
public struct VisModel: Equatable {
  public let provider: String
  public let name: String
  public let label: String
  public let billing: String
  public let isAvailable: Bool
  public let isDefault: Bool
  public let isReasoning: Bool
  public let isToolCall: Bool
  public let isImageInput: Bool
  public let family: String?
  public let context: Int?

  init?(_ row: VisJSON) {
    guard let provider = row["provider"]?.stringValue, let name = row["name"]?.stringValue
    else { return nil }
    self.provider = provider
    self.name = name
    self.label = row["label"]?.stringValue ?? name
    self.billing = row["billing"]?.stringValue ?? ""
    self.isAvailable = row["is_available"]?.boolValue ?? false
    self.isDefault = row["is_default"]?.boolValue ?? false
    self.isReasoning = row["is_reasoning"]?.boolValue ?? false
    self.isToolCall = row["is_tool_call"]?.boolValue ?? false
    self.isImageInput = row["is_image_input"]?.boolValue ?? false
    self.family = row["family"]?.stringValue
    self.context = row["context"]?.doubleValue.map { Int($0) }
  }

  public static func == (left: VisModel, right: VisModel) -> Bool {
    left.provider == right.provider && left.name == right.name
  }
}

/// The model of the next turn of a session, like the SDK's `SessionModel`.
public struct VisSessionModel: Equatable {
  public let provider: String?
  public let model: String?
  public let isPinned: Bool
  public let defaultProvider: String?
  public let defaultModel: String?

  init(_ value: VisJSON) {
    let pinned = (value["model"]?["provider"]?.stringValue, value["model"]?["model"]?.stringValue)
    let fallback = (
      value["default"]?["provider"]?.stringValue, value["default"]?["model"]?.stringValue
    )
    isPinned = pinned.1 != nil
    (provider, model) = isPinned ? pinned : fallback
    (defaultProvider, defaultModel) = fallback
  }
}

/// One row of a session list.
public struct VisSessionSummary: Equatable {
  public let id: String
  public let title: String?
  public let agentName: String?
}

/// The result of `VisSession.run`.
///
/// `turn` is the last turn record in contract form. `text` is its final prose
/// answer. `value` is the validated answer when the run had a response schema.
public struct VisRunResult {
  public let sessionID: String
  public let turn: VisJSON
  public let text: String?
  public let value: VisJSON?
}

/// The final prose answer of a turn record, like the SDK's `_final_answer`.
public func visFinalAnswer(_ turn: VisJSON) -> String? {
  guard let content = turn["content"]?.arrayValue else { return nil }
  for block in content.reversed() where block["type"]?.stringValue == "prose" {
    guard let answer = block["markdown"]?.stringValue,
      !answer.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
    else { return nil }
    return answer
  }
  return nil
}

/// The problem of an unsuccessful turn, like the SDK's `_turn_problem`.
public func visTurnProblem(_ turn: VisJSON) -> VisProblem {
  let content = turn["content"]?.arrayValue ?? []
  let detail =
    content.reversed().first { $0["type"]?.stringValue == "error" && $0["message"]?.stringValue != nil
    }?["message"]?.stringValue ?? turn["error"]?.stringValue
  let message = "the turn ended with status \(turn["status"]?.stringValue ?? "unknown")"
  return VisProblem(source: "turn", parts: [], message: detail.map { "\(message): \($0)" } ?? message)
}

/// Turn states that end a wait, as in the SDK's `Turn.wait`.
public let visTerminalStatuses: Set<String> = [
  "completed", "failed", "cancelled", "suspended", "error",
]

/// A client for one paired gateway, like the SDK's `GatewayClient`.
///
/// It sends the protocol handshake headers and the bearer token on each
/// request. A connection failure moves to the next address of the machine.
public final class VisClient {
  public let connection: VisConnection
  public let protocolVersion: Int
  public let minGatewayProtocol: Int
  public let clientName: String
  public var timeout: TimeInterval
  /// The pause between two reads of a running turn.
  public var pollInterval: TimeInterval = 0.5
  let urlSession: URLSession
  private var working: String?

  public init(
    connection: VisConnection, protocolVersion: Int, minGatewayProtocol: Int,
    clientName: String = "vis-shortcuts", timeout: TimeInterval = 30,
    urlSession: URLSession = .shared
  ) {
    self.connection = connection
    self.protocolVersion = protocolVersion
    self.minGatewayProtocol = minGatewayProtocol
    self.clientName = clientName
    self.timeout = timeout
    self.urlSession = urlSession
  }

  static func isConnectionFailure(_ error: URLError) -> Bool {
    [
      .cannotConnectToHost, .cannotFindHost, .dnsLookupFailed, .networkConnectionLost,
      .notConnectedToInternet, .timedOut, .secureConnectionFailed,
    ].contains(error.code)
  }

  /// Send one request and return its JSON body, or `.null` for an empty body.
  public func call(
    _ method: String, _ path: String, query: [URLQueryItem] = [], body: VisJSON? = nil,
    timeout: TimeInterval? = nil
  ) async throws -> VisJSON {
    let addresses = [working].compactMap { $0 } + connection.addresses.filter { $0 != working }
    guard !addresses.isEmpty else { throw VisTransportError(message: "the machine has no address") }
    var lastFailure: Error?
    for address in addresses {
      guard var components = URLComponents(string: address.trimmingSuffix("/") + path) else {
        lastFailure = VisTransportError(message: "invalid gateway address \(address)")
        continue
      }
      if !query.isEmpty { components.queryItems = query }
      guard let url = components.url else { continue }
      var request = URLRequest(url: url, timeoutInterval: timeout ?? self.timeout)
      request.httpMethod = method
      request.setValue("application/json", forHTTPHeaderField: "Accept")
      request.setValue(String(protocolVersion), forHTTPHeaderField: "X-Vis-Protocol")
      request.setValue(String(minGatewayProtocol), forHTTPHeaderField: "X-Vis-Min-Gateway-Protocol")
      request.setValue(clientName, forHTTPHeaderField: "X-Vis-Client")
      if let token = connection.token, !token.isEmpty {
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
      }
      if let body {
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONSerialization.data(withJSONObject: body.foundationValue)
      }
      let data: Data
      let response: URLResponse
      do {
        (data, response) = try await urlSession.data(for: request)
      } catch let error as URLError where Self.isConnectionFailure(error) {
        lastFailure = VisTransportError(message: "could not reach \(address): \(error.localizedDescription)")
        continue
      } catch let error as URLError {
        throw VisTransportError(message: "request to \(address) failed: \(error.localizedDescription)")
      }
      working = address
      let status = (response as? HTTPURLResponse)?.statusCode ?? 0
      let text = String(decoding: data, as: UTF8.self)
      let value: VisJSON?
      if text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
        value = .null
      } else {
        value = try? VisJSON.parse(text)
      }
      guard (200..<300).contains(status) else {
        let error = value?["error"]
        throw VisGatewayError(
          status: status, type: error?["type"]?.stringValue,
          message: error?["message"]?.stringValue ?? error?.stringValue
            ?? HTTPURLResponse.localizedString(forStatusCode: status))
      }
      guard let value else {
        throw VisTransportError(message: "the gateway answered \(method) \(path) with invalid JSON")
      }
      return value
    }
    throw lastFailure ?? VisTransportError(message: "the gateway is not reachable")
  }

  /// Return the models that the gateway pickers show, in picker order.
  ///
  /// `provider` keeps the models of one provider. `isAvailable` true keeps the
  /// models of signed-in providers, and false keeps the others.
  public func models(provider: String? = nil, isAvailable: Bool? = nil) async throws -> [VisModel] {
    let data = try await call("GET", "/v1/models")
    guard let rows = data["models"]?.arrayValue else {
      throw VisTransportError(message: "the model list has no models field")
    }
    return rows.compactMap(VisModel.init).filter {
      (provider == nil || $0.provider == provider)
        && (isAvailable == nil || $0.isAvailable == isAvailable)
    }
  }

  /// Create a conversation and return its handle, like the SDK's `create_session`.
  ///
  /// Give `provider` and `model` together to pin the model before the first
  /// turn. `options` holds other session creation fields, such as `title`.
  public func createSession(
    provider: String? = nil, model: String? = nil, options: [(String, VisJSON)] = []
  ) async throws -> VisSession {
    guard (provider == nil) == (model == nil) else {
      throw VisSchemaError(message: "provider and model must be given together")
    }
    var body = options
    if let provider, let model {
      body += [("provider", .string(provider)), ("model", .string(model))]
    }
    let data = try await call("POST", "/v1/sessions", body: .object(body))
    guard let id = data["id"]?.stringValue else {
      throw VisTransportError(message: "the created session has no id")
    }
    return session(id)
  }

  /// A handle for an existing session. This sends no request.
  public func session(_ id: String) -> VisSession { VisSession(client: self, id: id) }

  /// Recent sessions for an empty `query`, else the sessions that match it.
  public func searchSessions(query: String = "", limit: Int = 20) async throws -> [VisSessionSummary] {
    let data = try await call(
      "GET", "/v1/sessions/actions/search",
      query: [URLQueryItem(name: "q", value: query), URLQueryItem(name: "limit", value: String(limit))])
    return (data["sessions"]?.arrayValue ?? []).compactMap { row in
      row["id"]?.stringValue.map {
        VisSessionSummary(id: $0, title: row["title"]?.stringValue, agentName: row["agent_name"]?.stringValue)
      }
    }
  }
}

/// One conversation on a gateway, like the SDK's `Session`.
public struct VisSession {
  public let client: VisClient
  public let id: String

  func path(_ suffix: String = "") -> String {
    "/v1/sessions/\(id.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed) ?? id)\(suffix)"
  }

  /// Read the session record.
  public func read() async throws -> VisJSON { try await client.call("GET", path()) }

  /// Submit a user message and return its turn handle, like the SDK's `send`.
  ///
  /// A return does not mean that the model finished. Use `VisTurn.wait`.
  /// `options` holds other turn fields. Each call has a new idempotency key
  /// unless you give one.
  public func send(
    _ request: String, provider: String? = nil, model: String? = nil,
    idempotencyKey: String? = nil, options: [(String, VisJSON)] = []
  ) async throws -> VisTurn {
    var body = options.filter { !["request", "idempotency_key", "provider", "model"].contains($0.0) }
    if let provider { body.append(("provider", .string(provider))) }
    if let model { body.append(("model", .string(model))) }
    body.append(("request", .string(request)))
    body.append(("idempotency_key", .string(idempotencyKey ?? UUID().uuidString.lowercased())))
    let data = try await client.call("POST", path("/turns"), body: .object(body))
    guard let turnID = data["turn_id"]?.stringValue else {
      throw VisTransportError(message: "the submitted turn has no turn_id")
    }
    return VisTurn(session: self, id: turnID)
  }

  /// Submit a request and wait for its result, like the SDK's `Session.run`.
  ///
  /// Without `responseSchema`, the result holds the turn record and its final
  /// text. With a schema, the request includes the schema, and the final answer
  /// must be one JSON value that matches it. An invalid answer starts up to
  /// `maxCorrections` correction turns in this session. Corrections reuse the
  /// provider and model.
  public func run(
    _ request: String, timeout: TimeInterval = 300, responseSchema: VisResponseContract? = nil,
    maxCorrections: Int = 2, provider: String? = nil, model: String? = nil,
    options: [(String, VisJSON)] = []
  ) async throws -> VisRunResult {
    guard timeout > 0, timeout.isFinite else {
      throw VisSchemaError(message: "timeout must be a positive finite number of seconds")
    }
    guard let contract = responseSchema else {
      let turn = try await send(request, provider: provider, model: model, options: options)
        .wait(timeout: timeout)
      return VisRunResult(sessionID: id, turn: turn, text: visFinalAnswer(turn), value: nil)
    }
    guard maxCorrections >= 0 else {
      throw VisSchemaError(message: "max_corrections must not be negative")
    }
    let followUp = options.filter { !["attachments", "idempotency_key"].contains($0.0) }
    let name = "Response schema"
    var attempts = [VisStructuredOutputError.Attempt]()
    var pending = try await send(
      contract.request(request), provider: provider, model: model, options: options)
    let deadline = Date().addingTimeInterval(timeout)
    while true {
      let turn = try await pending.wait(timeout: max(deadline.timeIntervalSinceNow, 0.001))
      if turn["status"]?.stringValue != "completed" {
        attempts.append(.init(turn: turn, answer: nil, errors: [visTurnProblem(turn)]))
        throw VisStructuredOutputError(reason: "\(name) turn did not complete", attempts: attempts)
      }
      let answer = visFinalAnswer(turn)
      let (value, problems) = contract.validate(answer)
      if let value {
        return VisRunResult(sessionID: id, turn: turn, text: answer, value: value)
      }
      attempts.append(.init(turn: turn, answer: answer, errors: problems))
      if attempts.count > maxCorrections {
        let count = attempts.count
        throw VisStructuredOutputError(
          reason: "\(name) answer is invalid after \(count) attempt\(count > 1 ? "s" : "")",
          attempts: attempts)
      }
      if deadline.timeIntervalSinceNow <= 0 {
        throw VisStructuredOutputError(
          reason: "\(name) answer is invalid and the timeout left no time for a correction",
          attempts: attempts)
      }
      pending = try await send(
        contract.correction(problems), provider: provider, model: model, options: followUp)
    }
  }

  /// Read the model of the next turn: the pin, else the default pair.
  public func model() async throws -> VisSessionModel {
    VisSessionModel(try await client.call("GET", path("/model")))
  }

  /// Pin the provider and model of the next turns, or clear the pin with nil.
  public func setModel(provider: String?, model: String?) async throws -> VisSessionModel {
    guard (provider == nil) == (model == nil) else {
      throw VisSchemaError(message: "provider and model must be given together")
    }
    let body: VisJSON = .object([
      ("provider", provider.map(VisJSON.string) ?? .null), ("model", model.map(VisJSON.string) ?? .null),
    ])
    return VisSessionModel(try await client.call("PATCH", path("/model"), body: body))
  }

  /// A handle for an existing turn of this session. This sends no request.
  public func turn(_ id: String) -> VisTurn { VisTurn(session: self, id: id) }
}

/// One submitted request, like the SDK's `Turn`.
public struct VisTurn {
  public let session: VisSession
  public let id: String

  var path: String {
    session.path("/turns/\(id.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed) ?? id)")
  }

  /// Fetch the current turn record, including its `status`.
  public func read(timeout: TimeInterval? = nil) async throws -> VisJSON {
    try await session.client.call("GET", path, timeout: timeout)
  }

  /// Request cancellation. This does not revert file edits or other side effects.
  public func cancel() async throws -> VisJSON {
    try await session.client.call("POST", path + "/cancel")
  }

  /// Poll until the turn finishes or suspends, and return its record.
  ///
  /// A failure status is data, not an error. `VisTimeout` reports that the
  /// deadline expired. The wait does not cancel the turn.
  public func wait(timeout: TimeInterval = 300) async throws -> VisJSON {
    let end = Date().addingTimeInterval(timeout)
    while true {
      let remaining = end.timeIntervalSinceNow
      if remaining <= 0 { throw VisTimeout(sessionID: session.id, turnID: id) }
      let data = try await read(timeout: min(remaining, session.client.timeout))
      if let status = data["status"]?.stringValue, visTerminalStatuses.contains(status) {
        return data
      }
      let pause = min(session.client.pollInterval, max(0, end.timeIntervalSinceNow))
      try await Task.sleep(nanoseconds: UInt64(pause * 1_000_000_000))
    }
  }
}

extension String {
  func trimmingSuffix(_ suffix: String) -> String {
    var text = self
    while text.hasSuffix(suffix) { text.removeLast(suffix.count) }
    return text
  }
}
