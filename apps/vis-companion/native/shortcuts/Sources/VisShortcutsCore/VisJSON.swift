import Foundation

/// One JSON value that keeps member order and the integer or number kind.
///
/// The SDK reads answers with `json.loads` and strict hooks. This type and
/// `VisJSON.parse` give the same results: duplicate members, non-finite
/// numbers and the Python error messages.
public indirect enum VisJSON {
  case null
  case bool(Bool)
  /// An integer literal, kept as its exact digits.
  case integer(String)
  case number(Double)
  case string(String)
  case array([VisJSON])
  case object([(String, VisJSON)])

  /// The JSON Schema type name, as the SDK's `_json_type` gives it.
  public var typeName: String {
    switch self {
    case .null: return "null"
    case .bool: return "boolean"
    case .integer: return "integer"
    case .number: return "number"
    case .string: return "string"
    case .array: return "array"
    case .object: return "object"
    }
  }

  public var isScalar: Bool {
    switch self {
    case .array, .object: return false
    default: return true
    }
  }

  public var doubleValue: Double? {
    switch self {
    case .integer(let digits): return Double(digits)
    case .number(let value): return value
    default: return nil
    }
  }

  public var stringValue: String? {
    if case .string(let text) = self { return text }
    return nil
  }

  public var boolValue: Bool? {
    if case .bool(let flag) = self { return flag }
    return nil
  }

  public var arrayValue: [VisJSON]? {
    if case .array(let items) = self { return items }
    return nil
  }

  public var members: [(String, VisJSON)]? {
    if case .object(let members) = self { return members }
    return nil
  }

  public subscript(key: String) -> VisJSON? {
    members?.first(where: { $0.0 == key })?.1
  }

  /// Compact text in Python `json.dumps(value, ensure_ascii=False)` form.
  public var pythonText: String {
    switch self {
    case .null: return "null"
    case .bool(let flag): return flag ? "true" : "false"
    case .integer(let digits): return VisJSON.canonicalInteger(digits)
    case .number(let value): return VisJSON.pythonFloat(value)
    case .string(let text): return VisJSON.quote(text)
    case .array(let items):
      return "[" + items.map(\.pythonText).joined(separator: ", ") + "]"
    case .object(let members):
      return "{" + members.map { VisJSON.quote($0.0) + ": " + $0.1.pythonText }
        .joined(separator: ", ") + "}"
    }
  }

  /// A Foundation value for `JSONSerialization` request bodies.
  public var foundationValue: Any {
    switch self {
    case .null: return NSNull()
    case .bool(let flag): return flag
    case .integer(let digits):
      if let exact = Int(digits) { return exact }
      return Double(digits) ?? 0
    case .number(let value): return value
    case .string(let text): return text
    case .array(let items): return items.map(\.foundationValue)
    case .object(let members):
      var result = [String: Any]()
      for (name, value) in members { result[name] = value.foundationValue }
      return result
    }
  }

  static func canonicalInteger(_ digits: String) -> String {
    var text = digits
    let negative = text.hasPrefix("-")
    if negative { text.removeFirst() }
    while text.count > 1 && text.hasPrefix("0") { text.removeFirst() }
    if text == "0" { return "0" }
    return negative ? "-" + text : text
  }

  static func pythonFloat(_ value: Double) -> String {
    if value == value.rounded(), abs(value) < 1e16 {
      return String(format: "%.1f", value)
    }
    return "\(value)"
  }

  static func quote(_ text: String) -> String {
    var out = "\""
    for scalar in text.unicodeScalars {
      switch scalar {
      case "\"": out += "\\\""
      case "\\": out += "\\\\"
      case "\n": out += "\\n"
      case "\r": out += "\\r"
      case "\t": out += "\\t"
      case "\u{08}": out += "\\b"
      case "\u{0C}": out += "\\f"
      default:
        if scalar.value < 0x20 {
          out += String(format: "\\u%04x", scalar.value)
        } else {
          out.unicodeScalars.append(scalar)
        }
      }
    }
    return out + "\""
  }
}

/// Deep equality with JSON Schema rules: 1 equals 1.0, member order does not count.
public func visJSONEqual(_ left: VisJSON, _ right: VisJSON) -> Bool {
  switch (left, right) {
  case (.null, .null): return true
  case (.bool(let a), .bool(let b)): return a == b
  case (.string(let a), .string(let b)): return a == b
  case (.integer(let a), .integer(let b)):
    return VisJSON.canonicalInteger(a) == VisJSON.canonicalInteger(b)
  case (.integer, .number), (.number, .integer), (.number, .number):
    return left.doubleValue == right.doubleValue
  case (.array(let a), .array(let b)):
    return a.count == b.count && zip(a, b).allSatisfy { visJSONEqual($0, $1) }
  case (.object(let a), .object(let b)):
    guard a.count == b.count else { return false }
    return a.allSatisfy { member in
      b.contains { $0.0 == member.0 && visJSONEqual($0.1, member.1) }
    }
  default: return false
  }
}

/// A syntax error with the position fields of Python's `json.JSONDecodeError`.
public struct VisJSONSyntaxError: Error, Equatable {
  public let message: String
  /// Code point offset in the text.
  public let position: Int
  public let line: Int
  public let column: Int
}

/// A value that is valid JSON syntax but that the SDK refuses.
public struct VisJSONValueError: Error, Equatable {
  public let message: String
}

extension VisJSON {
  /// Parse one JSON text like the SDK's strict `json.loads`.
  ///
  /// Throws `VisJSONSyntaxError` for a syntax error and `VisJSONValueError`
  /// for a duplicate member, a non-finite number or a value nested too deeply.
  public static func parse(_ text: String) throws -> VisJSON {
    var parser = Parser(Array(text.unicodeScalars))
    return try parser.document()
  }

  struct Parser {
    let text: [Unicode.Scalar]
    var index = 0
    var depth = 0
    static let depthLimit = 500

    init(_ text: [Unicode.Scalar]) { self.text = text }

    func failure(_ message: String, at position: Int) -> VisJSONSyntaxError {
      var line = 1
      var lastNewline = -1
      for offset in 0..<min(position, text.count) where text[offset] == "\n" {
        line += 1
        lastNewline = offset
      }
      return VisJSONSyntaxError(
        message: message, position: position, line: line, column: position - lastNewline)
    }

    mutating func skipSpace() {
      while index < text.count, [" ", "\t", "\n", "\r"].contains(text[index]) { index += 1 }
    }

    mutating func document() throws -> VisJSON {
      skipSpace()
      let value = try value()
      skipSpace()
      if index != text.count { throw failure("Extra data", at: index) }
      return value
    }

    func starts(with word: String, at position: Int) -> Bool {
      let scalars = Array(word.unicodeScalars)
      guard position + scalars.count <= text.count else { return false }
      return Array(text[position..<(position + scalars.count)]) == scalars
    }

    mutating func value() throws -> VisJSON {
      guard index < text.count else { throw failure("Expecting value", at: index) }
      switch text[index] {
      case "\"":
        return .string(try string())
      case "{":
        return try nested { try $0.object() }
      case "[":
        return try nested { try $0.array() }
      default:
        if starts(with: "null", at: index) { index += 4; return .null }
        if starts(with: "true", at: index) { index += 4; return .bool(true) }
        if starts(with: "false", at: index) { index += 5; return .bool(false) }
        if starts(with: "NaN", at: index) {
          throw VisJSONValueError(message: "NaN is not valid JSON")
        }
        if starts(with: "Infinity", at: index) {
          throw VisJSONValueError(message: "Infinity is not valid JSON")
        }
        if starts(with: "-Infinity", at: index) {
          throw VisJSONValueError(message: "-Infinity is not valid JSON")
        }
        if let number = try number() { return number }
        throw failure("Expecting value", at: index)
      }
    }

    mutating func nested(_ body: (inout Parser) throws -> VisJSON) throws -> VisJSON {
      depth += 1
      defer { depth -= 1 }
      if depth > Parser.depthLimit {
        throw VisJSONValueError(message: "the JSON value is nested too deeply")
      }
      return try body(&self)
    }

    func isDigit(_ offset: Int) -> Bool {
      offset < text.count && ("0"..."9").contains(text[offset])
    }

    mutating func number() throws -> VisJSON? {
      var cursor = index
      if cursor < text.count, text[cursor] == "-" { cursor += 1 }
      guard isDigit(cursor) else { return nil }
      if text[cursor] == "0" {
        cursor += 1
      } else {
        while isDigit(cursor) { cursor += 1 }
      }
      var isInteger = true
      if cursor < text.count, text[cursor] == ".", isDigit(cursor + 1) {
        isInteger = false
        cursor += 1
        while isDigit(cursor) { cursor += 1 }
      }
      if cursor < text.count, text[cursor] == "e" || text[cursor] == "E" {
        var exponent = cursor + 1
        if exponent < text.count, text[exponent] == "+" || text[exponent] == "-" { exponent += 1 }
        if isDigit(exponent) {
          isInteger = false
          cursor = exponent
          while isDigit(cursor) { cursor += 1 }
        }
      }
      var literal = ""
      literal.unicodeScalars.append(contentsOf: text[index..<cursor])
      index = cursor
      if isInteger {
        if literal.count > 4300 {
          throw VisJSONValueError(
            message: "an integer with \(literal.count) characters is too long")
        }
        return .integer(literal)
      }
      guard let value = Double(literal), value.isFinite else {
        let shown = literal.count <= 40 ? literal : String(literal.prefix(39)) + "…"
        throw VisJSONValueError(
          message: "number \(shown) is outside the finite JSON number range")
      }
      return .number(value)
    }

    mutating func hex4(at position: Int) -> UInt32? {
      guard position + 4 <= text.count else { return nil }
      var value: UInt32 = 0
      for scalar in text[position..<(position + 4)] {
        guard let digit = Int(String(scalar), radix: 16) else { return nil }
        value = value * 16 + UInt32(digit)
      }
      return value
    }

    mutating func string() throws -> String {
      let start = index
      index += 1
      var out = String.UnicodeScalarView()
      while true {
        guard index < text.count else {
          throw failure("Unterminated string starting at", at: start)
        }
        let scalar = text[index]
        if scalar == "\"" {
          index += 1
          return String(out)
        }
        if scalar.value < 0x20 {
          throw failure("Invalid control character at", at: index)
        }
        if scalar != "\\" {
          out.append(scalar)
          index += 1
          continue
        }
        guard index + 1 < text.count else {
          throw failure("Unterminated string starting at", at: start)
        }
        let escape = text[index + 1]
        let simple: [Unicode.Scalar: Unicode.Scalar] = [
          "\"": "\"", "\\": "\\", "/": "/", "b": "\u{08}", "f": "\u{0C}", "n": "\n",
          "r": "\r", "t": "\t",
        ]
        if let mapped = simple[escape] {
          out.append(mapped)
          index += 2
          continue
        }
        guard escape == "u" else {
          throw failure("Invalid \\escape: '\(escape)'", at: index)
        }
        guard var code = hex4(at: index + 2) else {
          throw failure("Invalid \\uXXXX escape", at: index)
        }
        index += 6
        if (0xD800...0xDBFF).contains(code), index + 1 < text.count, text[index] == "\\",
          text[index + 1] == "u", let low = hex4(at: index + 2), (0xDC00...0xDFFF).contains(low)
        {
          code = 0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00)
          index += 6
        }
        out.append(Unicode.Scalar(code) ?? "\u{FFFD}")
      }
    }

    mutating func object() throws -> VisJSON {
      index += 1
      var members = [(String, VisJSON)]()
      var names = Set<String>()
      var duplicate: String?
      skipSpace()
      if index < text.count, text[index] == "}" {
        index += 1
        return .object(members)
      }
      while true {
        skipSpace()
        guard index < text.count, text[index] == "\"" else {
          if index < text.count, text[index] == "}", !members.isEmpty {
            throw failure("Illegal trailing comma before end of object", at: index - 1)
          }
          throw failure("Expecting property name enclosed in double quotes", at: index)
        }
        let name = try string()
        skipSpace()
        guard index < text.count, text[index] == ":" else {
          throw failure("Expecting ':' delimiter", at: index)
        }
        index += 1
        skipSpace()
        let member = try value()
        if names.contains(name), duplicate == nil { duplicate = name }
        names.insert(name)
        members.append((name, member))
        skipSpace()
        guard index < text.count else {
          throw failure("Expecting ',' delimiter", at: index)
        }
        if text[index] == "}" {
          index += 1
          // Python's object hook sees the members only after the whole object parsed.
          if let duplicate {
            throw VisJSONValueError(
              message: "object has duplicate member \(visBrief(.string(duplicate)))")
          }
          return .object(members)
        }
        guard text[index] == "," else {
          throw failure("Expecting ',' delimiter", at: index)
        }
        index += 1
      }
    }

    mutating func array() throws -> VisJSON {
      index += 1
      var items = [VisJSON]()
      skipSpace()
      if index < text.count, text[index] == "]" {
        index += 1
        return .array(items)
      }
      while true {
        skipSpace()
        if index < text.count, text[index] == "]", !items.isEmpty {
          throw failure("Illegal trailing comma before end of array", at: index - 1)
        }
        items.append(try value())
        skipSpace()
        guard index < text.count else {
          throw failure("Expecting ',' delimiter", at: index)
        }
        if text[index] == "]" {
          index += 1
          return .array(items)
        }
        guard text[index] == "," else {
          throw failure("Expecting ',' delimiter", at: index)
        }
        index += 1
      }
    }
  }
}

/// Cut text to `limit` code points with a final ellipsis, like the SDK's `_shorten`.
public func visShorten(_ text: String, _ limit: Int = 300) -> String {
  let scalars = Array(text.unicodeScalars)
  guard scalars.count > limit else { return text }
  var out = String.UnicodeScalarView()
  out.append(contentsOf: scalars[0..<(limit - 1)])
  return String(out) + "…"
}

/// A short JSON rendering of a value, like the SDK's `_brief`.
public func visBrief(_ value: VisJSON) -> String {
  visShorten(value.pythonText, 80)
}
