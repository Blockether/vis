import XCTest

@testable import VisShortcutsCore

final class VisShortcutsActionsTests: XCTestCase {
  var defaults: UserDefaults!
  var actions: VisShortcutsActions!

  override func setUp() {
    StubGateway.calls = []
    StubGateway.refused = []
    defaults = UserDefaults(suiteName: "vis-shortcuts-actions-\(UUID().uuidString)")
    defaults.set(
      #"[{"url": "http://10.0.0.5:7890", "token": "fixture-token", "label": "Desk", "id": "gw-1"}]"#,
      forKey: VisConnections.connectionsKey)
    actions = VisShortcutsActions(
      protocolVersion: 18, minGatewayProtocol: 18, defaults: defaults,
      urlSession: StubGateway.session())
    actions.pollInterval = 0.01
  }

  func turn(_ status: String, _ answer: String?) -> String {
    let content = answer.map { #"[{"type": "prose", "markdown": \#(VisJSON.string($0).pythonText)}]"# } ?? "[]"
    return #"{"turn_id": "t", "status": "\#(status)", "content": \#(content)}"#
  }

  /// A gateway with one session `s1` and one turn `t` that ends with `answers` in order.
  func gateway(answers: [String]) {
    var left = answers
    var current = ""
    StubGateway.handler = { call in
      switch (call.method, call.url.path) {
      case ("POST", "/v1/sessions"): return (201, #"{"id": "s1"}"#)
      case ("POST", let path) where path.hasSuffix("/turns"):
        current = left.removeFirst()
        return (202, #"{"turn_id": "t"}"#)
      case ("GET", let path) where path.hasSuffix("/turns/t"): return (200, self.turn("completed", current))
      default: return (404, #"{"error": {"type": "not_found", "message": "no route"}}"#)
      }
    }
  }

  func testModelRefSplitsAtTheFirstSlash() {
    XCTAssertEqual(
      VisModelRef("openrouter/anthropic/claude"),
      VisModelRef(provider: "openrouter", name: "anthropic/claude"))
    XCTAssertNil(VisModelRef("gpt-5"))
    XCTAssertNil(VisModelRef("/gpt-5"))
    XCTAssertEqual(VisModelRef(" openai/gpt-5 ")?.text, "openai/gpt-5")
  }

  func testRunInANewSessionReturnsTheTextAndSavesTheSession() async throws {
    gateway(answers: ["Hello there"])
    let result = try await actions.run("Say hello", model: "openai/gpt-5")
    XCTAssertEqual(result.text, "Hello there")
    XCTAssertNil(result.value)
    XCTAssertEqual(result.sessionID, "s1")
    let submit = StubGateway.calls.first { $0.url.path.hasSuffix("/turns") }!
    XCTAssertEqual(submit.body?["request"]?.stringValue, "Say hello")
    XCTAssertEqual(submit.body?["provider"]?.stringValue, "openai")
    XCTAssertEqual(submit.body?["model"]?.stringValue, "gpt-5")
    XCTAssertEqual(defaults.string(forKey: VisShortcutsActions.lastSessionPrefix + "gw-1"), "s1")
  }

  func testRunInTheLastSessionCreatesNoSession() async throws {
    defaults.set("s9", forKey: VisShortcutsActions.lastSessionPrefix + "gw-1")
    gateway(answers: ["Again"])
    let result = try await actions.run("Continue", session: .last)
    XCTAssertEqual(result.sessionID, "s9")
    XCTAssertFalse(StubGateway.calls.contains { $0.url.path == "/v1/sessions" })
    XCTAssertTrue(StubGateway.calls.contains { $0.url.path == "/v1/sessions/s9/turns" })
  }

  func testLastSessionWithoutHistoryCreatesOne() async throws {
    gateway(answers: ["First"])
    let result = try await actions.run("Start", session: .last)
    XCTAssertEqual(result.sessionID, "s1")
  }

  func testRunWithASchemaCorrectsAndReturnsTheValue() async throws {
    gateway(answers: [#"{"city": 3}"#, #"{"city": "Paris"}"#])
    let schema = #"{"type": "object", "properties": {"city": {"type": "string"}}, "required": ["city"]}"#
    let result = try await actions.run("Capital of France?", session: .id("s1"), responseSchema: schema)
    XCTAssertEqual(result.value?.pythonText, #"{"city": "Paris"}"#)
    let submits = StubGateway.calls.filter { $0.url.path.hasSuffix("/turns") }
    XCTAssertEqual(submits.count, 2)
    XCTAssertTrue(submits[0].body?["request"]?.stringValue?.contains("Capital of France?") == true)
  }

  func testRunRefusesABadSchemaBeforeTheGateway() async {
    gateway(answers: [])
    do {
      _ = try await actions.run("Hi", responseSchema: #"{"type": "nope"}"#)
      XCTFail("expected a schema error")
    } catch {
      XCTAssertTrue(error is VisSchemaError, "\(error)")
    }
    XCTAssertTrue(StubGateway.calls.isEmpty)
  }

  func testSendThenWaitReturnTheFinalText() async throws {
    gateway(answers: ["Later"])
    let sent = try await actions.send("Work on it", session: .new)
    XCTAssertEqual(sent, VisSentTurn(sessionID: "s1", turnID: "t"))
    let result = try await actions.wait(sessionID: sent.sessionID, turnID: sent.turnID)
    XCTAssertEqual(result.text, "Later")
  }

  func testCreateSessionPinsTheModel() async throws {
    gateway(answers: [])
    let id = try await actions.createSession(model: "anthropic/claude-opus")
    XCTAssertEqual(id, "s1")
    let create = StubGateway.calls.first!
    XCTAssertEqual(create.body?["provider"]?.stringValue, "anthropic")
    XCTAssertEqual(create.body?["model"]?.stringValue, "claude-opus")
    XCTAssertEqual(actions.lastSession(try actions.connection(nil)), "s1")
  }

  func testInvalidInputFailsWithAClearMessage() async {
    for (model, request) in [("gpt-5", "Hi"), ("", "  ")] {
      do {
        _ = try await actions.run(request, model: model)
        XCTFail("expected an error")
      } catch let error as VisShortcutsError {
        XCTAssertFalse(error.message.isEmpty)
      } catch {
        XCTFail("unexpected \(error)")
      }
    }
    XCTAssertTrue(StubGateway.calls.isEmpty)
  }

  func testSessionModelNeedsAnExistingSession() async {
    do {
      _ = try await actions.sessionModel(session: .new)
      XCTFail("expected an error")
    } catch {
      XCTAssertTrue(error is VisShortcutsError)
    }
  }

  func testNoPairedMachine() {
    defaults.removeObject(forKey: VisConnections.connectionsKey)
    XCTAssertThrowsError(try actions.connection(nil)) { error in
      XCTAssertTrue((error as? VisShortcutsError)?.message.contains("Pair a machine") == true)
    }
  }

  func testTalkURL() throws {
    let now = Date(timeIntervalSince1970: 1.5)
    XCTAssertEqual(
      try actions.talkURL(now: now).absoluteString,
      "vis://talk?machine=http://10.0.0.5:7890&session=new&at=1500")
    defaults.set("s9", forKey: VisShortcutsActions.lastSessionPrefix + "gw-1")
    XCTAssertEqual(
      try actions.talkURL(session: .last, now: now).absoluteString,
      "vis://talk?machine=http://10.0.0.5:7890&session=s9&at=1500")
    XCTAssertTrue(try actions.talkURL(session: .id("abc")).absoluteString.contains("session=abc"))
  }
}
