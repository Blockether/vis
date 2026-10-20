import XCTest

@testable import VisShortcutsCore

/// The Shortcut actions against a real gateway with a scripted model.
///
/// `packages/vis-agent/tests/test_shortcuts_live.py` starts the gateway and runs
/// this suite with `VIS_SHORTCUTS_LIVE_URL` and `VIS_SHORTCUTS_LIVE_TOKEN`.
final class LiveGatewayTests: XCTestCase {
  var actions: VisShortcutsActions!

  override func setUpWithError() throws {
    let env = ProcessInfo.processInfo.environment
    guard let url = env["VIS_SHORTCUTS_LIVE_URL"], let token = env["VIS_SHORTCUTS_LIVE_TOKEN"] else {
      throw XCTSkip("set VIS_SHORTCUTS_LIVE_URL and VIS_SHORTCUTS_LIVE_TOKEN")
    }
    let defaults = UserDefaults(suiteName: "vis-shortcuts-live-\(UUID().uuidString)")!
    let machine = VisJSON.object([("url", .string(url)), ("token", .string(token)), ("label", .string("fixture"))])
    defaults.set(VisJSON.array([machine]).pythonText, forKey: VisConnections.connectionsKey)
    let protocolVersion = Int(env["VIS_SHORTCUTS_LIVE_PROTOCOL"] ?? "") ?? 18
    actions = VisShortcutsActions(
      protocolVersion: protocolVersion, minGatewayProtocol: protocolVersion, defaults: defaults)
  }

  func testTheSDKOperationsAgainstARealGateway() async throws {
    // The fixture provider is managed and has no sign-in, so it is not available.
    let models = try await actions.models()
    XCTAssertTrue(models.contains { $0.provider == "sdk-fixture" && $0.name == "sdk-test" }, "\(models)")

    let plain = try await actions.run("Say the plain fixture answer", session: .new, timeout: 120)
    XCTAssertEqual(plain.text, "Plain fixture answer")
    XCTAssertNil(plain.value)
    XCTAssertEqual(plain.turn["status"]?.stringValue, "completed")

    let schema = #"{"type": "object", "properties": {"city": {"type": "string"}}, "required": ["city"], "additionalProperties": false}"#
    let structured = try await actions.run(
      "Name the capital of France", session: .last, model: "sdk-fixture/sdk-test",
      responseSchema: schema, timeout: 120)
    XCTAssertEqual(structured.sessionID, plain.sessionID)
    XCTAssertEqual(structured.value?.pythonText, #"{"city": "Paris"}"#)

    let sent = try await actions.send("Say the plain fixture answer", session: .last)
    XCTAssertEqual(sent.sessionID, plain.sessionID)
    let waited = try await actions.wait(sessionID: sent.sessionID, turnID: sent.turnID, timeout: 120)
    XCTAssertEqual(waited.text, "Plain fixture answer")

    let pinned = try await actions.createSession(model: "sdk-fixture/sdk-test")
    XCTAssertNotEqual(pinned, plain.sessionID)
    let model = try await actions.sessionModel(session: .last)
    XCTAssertTrue(model.isPinned)
    XCTAssertEqual(model.provider, "sdk-fixture")
    XCTAssertEqual(model.model, "sdk-test")
    let cleared = try await actions.setSessionModel(nil, session: .id(pinned))
    XCTAssertFalse(cleared.isPinned)

    let sessions = try await actions.searchSessions(limit: 50)
    XCTAssertTrue(sessions.contains { $0.id == plain.sessionID }, "\(sessions)")
  }

  func testAStructuredAnswerThatStaysInvalidFails() async throws {
    let schema = #"{"type": "object", "properties": {"city": {"type": "boolean"}}, "required": ["city"]}"#
    do {
      _ = try await actions.run(
        "Name the capital of France", responseSchema: schema, maxCorrections: 1, timeout: 120)
      XCTFail("expected a structured output error")
    } catch let error as VisStructuredOutputError {
      XCTAssertEqual(error.attempts.count, 2)
      XCTAssertEqual(error.reason, "Response schema answer is invalid after 2 attempts")
    }
  }
}
