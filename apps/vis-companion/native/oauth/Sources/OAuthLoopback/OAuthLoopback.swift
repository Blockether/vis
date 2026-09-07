import Darwin
import Foundation

/// A bounded, one-shot HTTP return on this device's loopback. Call on the main queue.
/// No provider endpoints, token exchange, logging, redirects or persisted callbacks.
public final class OAuthLoopback {
  public enum Failure: Error { case invalidRequest, unavailable, expired, cancelled }
  public let authorizationURL: URL
  private let redirect: URLComponents
  private let state: String
  private let expiresAt: Date
  private var listeners: [DispatchSourceRead] = []
  private var connections: [UUID: DispatchIO] = [:]
  private var timer: Timer?
  private var claimed = false
  private var done = false
  private var completion: ((Result<String, Failure>) -> Void)?

  public init(authorizationURL: String, redirectURI: String, state: String, expiresAt: Date) throws
  {
    guard authorizationURL.utf8.count <= 16384, redirectURI.utf8.count <= 2048,
      !state.isEmpty, state.utf8.count <= 1024,
      let redirect = URLComponents(string: redirectURI), redirect.scheme == "http",
      ["localhost", "127.0.0.1", "[::1]"].contains(redirect.host ?? ""),
      let port = redirect.port, (1024...65535).contains(port),
      redirect.user == nil, redirect.password == nil, redirect.query == nil,
      redirect.fragment == nil,
      !redirect.path.isEmpty, !redirect.percentEncodedPath.contains("%"),
      let auth = URLComponents(string: authorizationURL), let url = auth.url,
      auth.scheme == "https", auth.host != nil, auth.user == nil, auth.password == nil,
      auth.fragment == nil,
      Self.single(auth.queryItems, "state") == state,
      Self.single(auth.queryItems, "redirect_uri") == redirectURI,
      expiresAt > Date(), expiresAt.timeIntervalSinceNow <= 900
    else { throw Failure.invalidRequest }
    self.authorizationURL = url
    self.redirect = redirect
    self.state = state
    self.expiresAt = expiresAt
  }

  private static func single(_ items: [URLQueryItem]?, _ name: String) -> String? {
    let matches = (items ?? []).filter { $0.name == name }
    return matches.count == 1 ? matches[0].value : nil
  }

  /// Accept only the expected origin-form GET, Host, path, unique query and state.
  public func callback(for header: String) -> String? {
    guard header.utf8.count <= 8192, header.hasSuffix("\r\n\r\n"), !header.contains("\0") else {
      return nil
    }
    let lines = header.components(separatedBy: "\r\n")
    let request = lines[0].split(separator: " ", omittingEmptySubsequences: false)
    guard request.count == 3, request[0] == "GET",
      ["HTTP/1.0", "HTTP/1.1"].contains(String(request[2])),
      request[1].hasPrefix("/"), !request[1].hasPrefix("//")
    else { return nil }
    var headers: [String: String] = [:]
    for line in lines.dropFirst() where !line.isEmpty {
      guard let colon = line.firstIndex(of: ":"), !line.hasPrefix(" "), !line.hasPrefix("\t") else {
        return nil
      }
      let key = line[..<colon].lowercased()
      guard headers[key] == nil else { return nil }
      headers[key] = line[line.index(after: colon)...].trimmingCharacters(in: .whitespaces)
    }
    let expectedHost = "\(redirect.host!):\(redirect.port!)"
    guard headers["host"]?.lowercased() == expectedHost, headers["transfer-encoding"] == nil,
      headers["content-length"] == nil || headers["content-length"] == "0",
      var returned = URLComponents(string: "http://\(expectedHost)\(request[1])"),
      returned.fragment == nil, returned.percentEncodedPath == redirect.percentEncodedPath
    else { return nil }
    returned.percentEncodedQuery = returned.percentEncodedQuery?.replacingOccurrences(
      of: "+", with: "%20")
    let query = returned.queryItems ?? []
    guard Set(query.map { $0.name }).count == query.count,
      Self.single(query, "state") == state,
      query.contains(where: { $0.name == "code" }) != query.contains(where: { $0.name == "error" })
    else { return nil }
    let key = query.contains(where: { $0.name == "code" }) ? "code" : "error"
    guard let value = Self.single(query, key),
      !value.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
    else { return nil }
    var result = redirect
    result.queryItems = [
      URLQueryItem(name: "state", value: state), URLQueryItem(name: key, value: value),
    ]
    result.percentEncodedQuery = result.percentEncodedQuery?.replacingOccurrences(
      of: "+", with: "%2B")
    return result.string
  }

  /// Bind explicit IPv4/IPv6 loopback addresses, never a wildcard or hostname lookup.
  public func start(
    ready: @escaping () -> Void, completion: @escaping (Result<String, Failure>) -> Void
  ) {
    guard listeners.isEmpty, !done else {
      completion(.failure(.unavailable))
      return
    }
    self.completion = completion
    let host = redirect.host!
    let families =
      host == "localhost" ? [AF_INET, AF_INET6] : [host == "127.0.0.1" ? AF_INET : AF_INET6]
    for family in families {
      let fd = socket(family, SOCK_STREAM, 0)
      guard fd >= 0 else {
        finish(.failure(.unavailable))
        return
      }
      _ = fcntl(fd, F_SETFD, FD_CLOEXEC)
      _ = fcntl(fd, F_SETFL, O_NONBLOCK)
      var yes: Int32 = 1
      _ = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, socklen_t(MemoryLayout<Int32>.size))
      let bound: Int32
      if family == AF_INET {
        var address = sockaddr_in()
        address.sin_len = UInt8(MemoryLayout<sockaddr_in>.size)
        address.sin_family = sa_family_t(AF_INET)
        address.sin_port = UInt16(redirect.port!).bigEndian
        address.sin_addr = in_addr(s_addr: INADDR_LOOPBACK.bigEndian)
        bound = withUnsafePointer(to: &address) { ptr in
          ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
            Darwin.bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
          }
        }
      } else {
        var address = sockaddr_in6()
        address.sin6_len = UInt8(MemoryLayout<sockaddr_in6>.size)
        address.sin6_family = sa_family_t(AF_INET6)
        address.sin6_port = UInt16(redirect.port!).bigEndian
        address.sin6_addr = in6addr_loopback
        bound = withUnsafePointer(to: &address) { ptr in
          ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
            Darwin.bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in6>.size))
          }
        }
      }
      guard bound == 0, listen(fd, 8) == 0 else {
        Darwin.close(fd)
        finish(.failure(.unavailable))
        return
      }
      let source = DispatchSource.makeReadSource(fileDescriptor: fd, queue: .main)
      source.setEventHandler { [weak self] in
        guard let self, !self.done, !self.claimed else { return }
        let client = accept(fd, nil, nil)
        if client >= 0 { self.receive(client) }
      }
      source.setCancelHandler { Darwin.close(fd) }
      listeners.append(source)
      source.resume()
    }
    timer = Timer.scheduledTimer(
      withTimeInterval: max(0.001, expiresAt.timeIntervalSinceNow), repeats: false
    ) {
      [weak self] _ in self?.finish(.failure(.expired))
    }
    ready()
  }

  public func cancel() { finish(.failure(.cancelled)) }

  private func receive(_ fd: Int32) {
    guard !done, !claimed, connections.count < 8 else {
      Darwin.close(fd)
      return
    }
    _ = fcntl(fd, F_SETFD, FD_CLOEXEC)
    var yes: Int32 = 1
    _ = setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &yes, socklen_t(MemoryLayout<Int32>.size))
    let id = UUID()
    let channel = DispatchIO(type: .stream, fileDescriptor: fd, queue: .main) { _ in
      Darwin.close(fd)
    }
    channel.setLimit(lowWater: 1)
    connections[id] = channel
    let timeout = DispatchWorkItem { [weak self] in
      self?.connections.removeValue(forKey: id)?.close(flags: .stop)
    }
    DispatchQueue.main.asyncAfter(deadline: .now() + 5, execute: timeout)
    var received = Data()
    var replied = false
    channel.read(offset: 0, length: 8193, queue: .main) { [weak self] ended, bytes, error in
      guard let self, !self.done, self.connections[id] != nil, !replied else { return }
      if let bytes { received.append(Data(bytes)) }
      if received.count <= 8192, let end = received.range(of: Data("\r\n\r\n".utf8)) {
        replied = true
        let header = String(decoding: received[..<end.upperBound], as: UTF8.self)
        let input = self.claimed || Date() >= self.expiresAt ? nil : self.callback(for: header)
        if input != nil {
          self.claimed = true
          for listener in self.listeners { listener.cancel() }
        }
        let status = input == nil ? "400 Bad Request" : "200 OK"
        let body = input == nil ? "Invalid sign-in return." : "Sign-in received. Return to Vis."
        let reply =
          "HTTP/1.1 \(status)\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: \(body.utf8.count)\r\nCache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nContent-Security-Policy: default-src 'none'\r\nConnection: close\r\n\r\n\(body)"
        let data = Data(reply.utf8).withUnsafeBytes { DispatchData(bytes: $0) }
        channel.write(offset: 0, data: data, queue: .main) { [weak self] written, _, error in
          if written || error != 0 {
            timeout.cancel()
            channel.close(flags: .stop)
            self?.connections.removeValue(forKey: id)
            if let input { self?.finish(.success(input)) }
          }
        }
      } else if received.count > 8192 || ended || error != 0 {
        timeout.cancel()
        channel.close(flags: .stop)
        self.connections.removeValue(forKey: id)
      }
    }
  }

  private func finish(_ result: Result<String, Failure>) {
    guard !done else { return }
    done = true
    timer?.invalidate()
    timer = nil
    for listener in listeners { listener.cancel() }
    listeners.removeAll()
    for connection in connections.values { connection.close(flags: .stop) }
    connections.removeAll()
    let callback = completion
    completion = nil
    callback?(result)
  }
  deinit {
    timer?.invalidate()
    for listener in listeners { listener.cancel() }
    for connection in connections.values { connection.close(flags: .stop) }
  }
}
