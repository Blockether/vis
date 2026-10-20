import XCTest

@testable import VisShortcutsCore

/// A fake gateway: each request goes to `StubGateway.handler`.
final class StubGateway: URLProtocol {
  struct Call {
    let method: String
    let url: URL
    let headers: [String: String]
    let body: VisJSON?
  }

  static var handler: ((Call) throws -> (Int, String)) = { _ in (404, "") }
  static var calls = [Call]()
  static var refused = Set<String>()

  static func session() -> URLSession {
    let configuration = URLSessionConfiguration.ephemeral
    configuration.protocolClasses = [StubGateway.self]
    return URLSession(configuration: configuration)
  }

  override class func canInit(with request: URLRequest) -> Bool { true }
  override class func canonicalRequest(for request: URLRequest) -> URLRequest { request }
  override func stopLoading() {}

  override func startLoading() {
    var data = request.httpBody
    if data == nil, let stream = request.httpBodyStream {
      stream.open()
      var buffer = Data()
      var chunk = [UInt8](repeating: 0, count: 4096)
      while stream.hasBytesAvailable {
        let count = stream.read(&chunk, maxLength: chunk.count)
        if count <= 0 { break }
        buffer.append(chunk, count: count)
      }
      stream.close()
      data = buffer
    }
    let body = data.flatMap { try? VisJSON.parse(String(decoding: $0, as: UTF8.self)) }
    let url = request.url!
    if StubGateway.refused.contains(url.host ?? "") {
      client?.urlProtocol(self, didFailWithError: URLError(.cannotConnectToHost))
      return
    }
    let call = Call(
      method: request.httpMethod ?? "GET", url: url,
      headers: request.allHTTPHeaderFields ?? [:], body: body)
    StubGateway.calls.append(call)
    do {
      let (status, text) = try StubGateway.handler(call)
      let response = HTTPURLResponse(url: url, statusCode: status, httpVersion: nil, headerFields: nil)!
      client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
      client?.urlProtocol(self, didLoad: Data(text.utf8))
      client?.urlProtocolDidFinishLoading(self)
    } catch {
      client?.urlProtocol(self, didFailWithError: error)
    }
  }
}

final class VisClientTests: XCTestCase {
  var client: VisClient!

  override func setUp() {
    StubGateway.calls = []
    StubGateway.refused = []
    client = VisClient(
      connection: VisConnection(url: "http://10.0.0.5:7890", token: "fixture-token"),
      protocolVersion: 18, minGatewayProtocol: 18, urlSession: StubGateway.session())
    client.pollInterval = 0.01
  }

  func turn(_ status: String, _ answer: String?) -> String {
    let content = answer.map { #"[{"type": "prose", "markdown": \#(VisJSON.string($0).pythonText)}]"# } ?? "[]"
    return #"{"turn_id": "t", "status": "\#(status)", "content": \#(content)}"#
  }

  /// A gateway that answers each submitted turn with the next answer of `answers`.
  func serve(answers: [String?], status: String = "completed", reads: Int = 1) {
    var submitted = 0
    var polled = 0
    StubGateway.handler = { call in
      switch (call.method, call.url.path) {
      case ("POST", "/v1/sessions"):
        return (200, #"{"id": "s-new"}"#)
      case ("POST", let path) where path.hasSuffix("/turns"):
        submitted += 1
        polled = 0
        return (200, #"{"turn_id": "t\#(submitted)"}"#)
      case ("GET", let path) where path.contains("/turns/"):
        polled += 1
        if polled < reads { return (200, self.turn("running", nil)) }
        return (200, self.turn(status, answers[min(submitted, answers.count) - 1]))
      default:
        return (404, #"{"error": {"type": "not_found", "message": "no route"}}"#)
      }
    }
  }

  func posts() -> [StubGateway.Call] {
    StubGateway.calls.filter { $0.method == "POST" && $0.url.path.hasSuffix("/turns") }
  }

  func testRequestsCarryTheTokenAndTheHandshake() async throws {
    StubGateway.handler = { _ in (200, #"{"models": []}"#) }
    _ = try await client.models()
    let headers = try XCTUnwrap(StubGateway.calls.first?.headers)
    XCTAssertEqual(headers["Authorization"], "Bearer fixture-token")
    XCTAssertEqual(headers["X-Vis-Protocol"], "18")
    XCTAssertEqual(headers["X-Vis-Min-Gateway-Protocol"], "18")
    XCTAssertEqual(headers["X-Vis-Client"], "vis-shortcuts")
  }

  func testModelsKeepPickerOrderAndFilterLikeTheSDK() async throws {
    StubGateway.handler = { _ in
      (
        200,
        #"{"models": [{"provider": "alpha", "name": "large", "label": "Large", "is_available": true, "is_default": true},"#
          + #"{"provider": "beta", "name": "small", "label": "Small", "is_available": false}]}"#
      )
    }
    let all = try await client.models()
    XCTAssertEqual(all.map(\.name), ["large", "small"])
    XCTAssertTrue(all[0].isDefault)
    let available = try await client.models(isAvailable: true)
    XCTAssertEqual(available.map(\.name), ["large"])
    let beta = try await client.models(provider: "beta")
    XCTAssertEqual(beta.map(\.provider), ["beta"])
  }

  func testCreateSessionPinsTheModel() async throws {
    serve(answers: [])
    let session = try await client.createSession(
      provider: "beta", model: "small", options: [("title", .string("From Shortcuts"))])
    XCTAssertEqual(session.id, "s-new")
    let body = try XCTUnwrap(StubGateway.calls.first?.body)
    XCTAssertEqual(body["provider"]?.stringValue, "beta")
    XCTAssertEqual(body["model"]?.stringValue, "small")
    XCTAssertEqual(body["title"]?.stringValue, "From Shortcuts")
    do {
      _ = try await client.createSession(provider: "beta")
      XCTFail("a provider without a model must fail")
    } catch {}
  }

  func testPlainRunReturnsTheFinalText() async throws {
    serve(answers: ["Plain answer"], reads: 3)
    let result = try await client.session("s1").run("Hello", provider: "beta", model: "small")
    XCTAssertEqual(result.text, "Plain answer")
    XCTAssertNil(result.value)
    XCTAssertEqual(result.sessionID, "s1")
    let sent = try XCTUnwrap(posts().first)
    XCTAssertEqual(sent.url.path, "/v1/sessions/s1/turns")
    XCTAssertEqual(sent.body?["request"]?.stringValue, "Hello")
    XCTAssertEqual(sent.body?["model"]?.stringValue, "small")
    XCTAssertNotNil(sent.body?["idempotency_key"]?.stringValue)
  }

  func testStructuredRunCorrectsAnInvalidAnswerInTheSameSession() async throws {
    serve(answers: [#"{"cents": "12"}"#, #"{"cents": 12}"#])
    let contract = try VisResponseContract(
      schemaText: #"{"type": "object", "properties": {"cents": {"type": "integer"}}, "required": ["cents"]}"#)
    let result = try await client.session("s1").run(
      "Quote", responseSchema: contract, model: "small",
      options: [("attachments", .array([.string("u1")]))])
    XCTAssertEqual(result.value?["cents"]?.doubleValue, 12)
    let sent = posts()
    XCTAssertEqual(sent.count, 2)
    XCTAssertEqual(sent[0].body?["request"]?.stringValue, contract.request("Quote"))
    let problem = VisProblem(
      source: "schema", parts: [.key("cents")], message: #"expected integer, got string (input: "12")"#)
    XCTAssertEqual(sent[1].body?["request"]?.stringValue, contract.correction([problem]))
    XCTAssertEqual(sent[1].body?["model"]?.stringValue, "small")
    XCTAssertNotNil(sent[0].body?["attachments"])
    XCTAssertNil(sent[1].body?["attachments"], "corrections do not resend attachments")
  }

  func testStructuredRunStopsAfterMaxCorrections() async throws {
    serve(answers: ["nope", "still no"])
    let contract = try VisResponseContract(schemaText: #"{"type": "integer"}"#)
    do {
      _ = try await client.session("s1").run("Count", responseSchema: contract, maxCorrections: 1)
      XCTFail("expected a structured output error")
    } catch let error as VisStructuredOutputError {
      XCTAssertEqual(error.attempts.count, 2)
      XCTAssertEqual(error.errors.map(\.message), ["the answer starts with text instead of a JSON value"])
      XCTAssertTrue(error.localizedDescription.hasPrefix("Response schema answer is invalid after 2 attempts"))
    }
    XCTAssertEqual(posts().count, 2)
  }

  func testStructuredRunReportsATurnThatDidNotComplete() async throws {
    serve(answers: [nil], status: "failed")
    let contract = try VisResponseContract(schemaText: #"{"type": "integer"}"#)
    do {
      _ = try await client.session("s1").run("Count", responseSchema: contract)
      XCTFail("expected a structured output error")
    } catch let error as VisStructuredOutputError {
      XCTAssertEqual(error.reason, "Response schema turn did not complete")
      XCTAssertEqual(error.errors.first?.message, "the turn ended with status failed")
    }
  }

  func testWaitTimesOutWithoutCancelling() async throws {
    serve(answers: ["late"], reads: 1_000_000)
    let turn = try await client.session("s1").send("Slow")
    do {
      _ = try await turn.wait(timeout: 0.05)
      XCTFail("expected a timeout")
    } catch let error as VisTimeout {
      XCTAssertEqual(error, VisTimeout(sessionID: "s1", turnID: "t1"))
    }
    XCTAssertFalse(StubGateway.calls.contains { $0.url.path.hasSuffix("/cancel") })
  }

  func testGatewayErrorsKeepTheirTypeAndMessage() async throws {
    StubGateway.handler = { _ in (400, #"{"error": {"type": "invalid_model", "message": "unknown model"}}"#) }
    do {
      _ = try await client.session("s1").send("Hi", provider: "x", model: "y")
      XCTFail("expected a gateway error")
    } catch let error as VisGatewayError {
      XCTAssertEqual(error, VisGatewayError(status: 400, type: "invalid_model", message: "unknown model"))
    }
  }

  func testAnUnreachableAddressMovesToTheNextAddress() async throws {
    client = VisClient(
      connection: VisConnection(url: "http://10.0.0.5:7890", alts: ["http://gateway.example.com"]),
      protocolVersion: 18, minGatewayProtocol: 18, urlSession: StubGateway.session())
    StubGateway.refused = ["10.0.0.5"]
    StubGateway.handler = { _ in (200, #"{"models": []}"#) }
    _ = try await client.models()
    _ = try await client.models()
    XCTAssertEqual(StubGateway.calls.map { $0.url.host }, ["gateway.example.com", "gateway.example.com"])
  }

  func testSessionModelReadsThePinOrTheDefault() async throws {
    StubGateway.handler = { call in
      call.method == "GET"
        ? (200, #"{"model": null, "default": {"provider": "alpha", "model": "large"}}"#)
        : (200, #"{"model": {"provider": "beta", "model": "small"}, "default": {"provider": "alpha", "model": "large"}}"#)
    }
    let current = try await client.session("s1").model()
    XCTAssertEqual(current.model, "large")
    XCTAssertFalse(current.isPinned)
    let pinned = try await client.session("s1").setModel(provider: "beta", model: "small")
    XCTAssertTrue(pinned.isPinned)
    XCTAssertEqual(pinned.provider, "beta")
  }

  func testSearchSessionsReadsRows() async throws {
    StubGateway.handler = { _ in (200, #"{"sessions": [{"id": "s1", "title": "Plan"}, {"id": "s2"}]}"#) }
    let rows = try await client.searchSessions(query: "plan", limit: 5)
    XCTAssertEqual(rows.map(\.id), ["s1", "s2"])
    XCTAssertEqual(rows.first?.title, "Plan")
    let query = URLComponents(url: StubGateway.calls[0].url, resolvingAgainstBaseURL: false)?.queryItems
    XCTAssertEqual(query, [URLQueryItem(name: "q", value: "plan"), URLQueryItem(name: "limit", value: "5")])
  }
}

final class VisConnectionsTests: XCTestCase {
  func testThePrimaryMachineComesFromTheCompanionStore() throws {
    let defaults = try XCTUnwrap(UserDefaults(suiteName: "vis-shortcuts-tests"))
    defaults.removePersistentDomain(forName: "vis-shortcuts-tests")
    XCTAssertNil(VisConnections.primary(in: defaults))
    defaults.set(
      #"[{"url": "http://10.0.0.5:7890", "token": "a", "label": "Desk"}, {"url": "https://gateway.example.com", "id": "gw-2"}]"#,
      forKey: VisConnections.connectionsKey)
    XCTAssertEqual(VisConnections.primary(in: defaults)?.label, "Desk")
    defaults.set("https://gateway.example.com", forKey: VisConnections.primaryKey)
    XCTAssertEqual(VisConnections.primary(in: defaults)?.id, "gw-2")
    XCTAssertEqual(VisConnections.find("gw-2", in: defaults)?.url, "https://gateway.example.com")
    XCTAssertEqual(VisConnections.find("http://10.0.0.5:7890", in: defaults)?.token, "a")
    XCTAssertEqual(VisConnections.all(in: defaults).first?.displayName, "Desk")
    XCTAssertEqual(VisConnections.all(in: defaults).last?.displayName, "gateway.example.com")
    defaults.removePersistentDomain(forName: "vis-shortcuts-tests")
  }
}
