import Foundation
import XCTest

@testable import OAuthLoopback

final class OAuthLoopbackTests: XCTestCase {
  func receiver(_ redirect: String = "http://localhost:53692/callback", seconds: Double = 60) throws
    -> OAuthLoopback
  {
    var auth = URLComponents(string: "https://gateway.example.com/authorize")!
    auth.queryItems = [
      URLQueryItem(name: "state", value: "test-state"),
      URLQueryItem(name: "redirect_uri", value: redirect),
    ]
    return try OAuthLoopback(
      authorizationURL: auth.string!, redirectURI: redirect, state: "test-state",
      expiresAt: Date().addingTimeInterval(seconds))
  }
  func request(_ target: String, host: String = "localhost:53692", method: String = "GET") -> String
  {
    "\(method) \(target) HTTP/1.1\r\nHost: \(host)\r\n\r\n"
  }
  func testStateDestinationAndAmbiguousQueries() throws {
    let server = try receiver()
    XCTAssertEqual(
      server.callback(for: request("/callback?state=test-state&code=test-code&ignored=value")),
      "http://localhost:53692/callback?state=test-state&code=test-code")
    for target in [
      "/other?state=test-state&code=test-code", "/callback?state=wrong&code=test-code",
      "/callback?state=test-state&code=test-code&code=other",
      "/callback?state=test-state&code=test-code&error=",
      "/callback?state=test-state&code=", "/callback?state=test-state&code=test-code#fragment",
      "/callback?state=test-state&state=test-state&code=test-code",
    ] {
      XCTAssertNil(server.callback(for: request(target)))
    }
    XCTAssertNil(
      server.callback(
        for: request("/callback?state=test-state&code=test-code", host: "10.0.0.5:53692")))
    XCTAssertNil(
      server.callback(for: request("/callback?state=test-state&code=test-code", method: "POST")))
    XCTAssertEqual(
      server.callback(for: request("/callback?state=test-state&code=a%2Bb+c")),
      "http://localhost:53692/callback?state=test-state&code=a%2Bb%20c")
  }
  func testRejectsNonLoopbackAndUnboundedOrMismatchedAuthorization() throws {
    for redirect in [
      "http://10.0.0.5:53692/callback", "http://127.0.0.1:80/callback",
      "https://localhost:53692/callback",
      "http://user@localhost:53692/callback", "http://localhost:53692/callback?next=other",
    ] {
      XCTAssertThrowsError(try receiver(redirect))
    }
    XCTAssertThrowsError(try receiver(seconds: 901))
    XCTAssertThrowsError(try receiver(seconds: -1))
    XCTAssertThrowsError(
      try OAuthLoopback(
        authorizationURL:
          "https://gateway.example.com/authorize?state=test-state&redirect_uri=other",
        redirectURI: "http://localhost:53692/callback", state: "test-state",
        expiresAt: Date().addingTimeInterval(60)))
  }
  @MainActor func testFreshReturnOverRealIPv4AndIPv6Listener() async throws {
    // No provider session: fresh state and code cross the production socket receiver.
    let server = try receiver()
    let ready = expectation(description: "listener ready")
    let complete = expectation(description: "one return")
    complete.assertForOverFulfill = true
    server.start(
      ready: { ready.fulfill() },
      completion: { result in
        XCTAssertEqual(
          try? result.get(), "http://localhost:53692/callback?state=test-state&code=test-code",
          String(describing: result))
        complete.fulfill()
      })
    await fulfillment(of: [ready], timeout: 3)
    let config = URLSessionConfiguration.ephemeral
    config.timeoutIntervalForRequest = 3
    let session = URLSession(configuration: config)
    defer {
      session.invalidateAndCancel()
      server.cancel()
    }
    var invalid = URLRequest(
      url: URL(string: "http://[::1]:53692/callback?state=wrong&code=test-code")!)
    invalid.setValue("localhost:53692", forHTTPHeaderField: "Host")
    let (_, rejected) = try await session.data(for: invalid)
    XCTAssertEqual((rejected as? HTTPURLResponse)?.statusCode, 400)
    var valid = URLRequest(
      url: URL(string: "http://127.0.0.1:53692/callback?state=test-state&code=test-code")!)
    valid.setValue("localhost:53692", forHTTPHeaderField: "Host")
    let (body, response) = try await session.data(for: valid)
    XCTAssertEqual((response as? HTTPURLResponse)?.statusCode, 200)
    XCTAssertEqual(
      (response as? HTTPURLResponse)?.value(forHTTPHeaderField: "Cache-Control"), "no-store")
    XCTAssertFalse(String(decoding: body, as: UTF8.self).contains("test-code"))
    await fulfillment(of: [complete], timeout: 3)
  }
  @MainActor func testCancellationAndExpiryReleaseThePort() async throws {
    for seconds in [60.0, 0.1] {
      let server = try receiver(seconds: seconds)
      let ready = expectation(description: "ready")
      let done = expectation(description: "closed")
      server.start(
        ready: { ready.fulfill() },
        completion: { result in
          if case .success = result { XCTFail("No callback was submitted") }
          done.fulfill()
        })
      await fulfillment(of: [ready], timeout: 3)
      if seconds == 60 {
        server.cancel()
        server.cancel()
      }
      await fulfillment(of: [done], timeout: 3)
    }
  }
}
