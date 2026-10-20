import Foundation

/// One step in a path to a value inside an answer.
public enum VisPathPart: Equatable {
  case key(String)
  case index(Int)
}

/// One problem with an answer, in the SDK's `StructuredOutputError.errors` form.
///
/// `path` is JSONPath-like, and `$` is the whole value. `source` is `turn`,
/// `answer`, `json` or `schema`.
public struct VisProblem: Equatable, CustomStringConvertible {
  public let path: String
  public let message: String
  public let source: String

  public init(source: String, parts: [VisPathPart], message: String) {
    self.path = visPath(parts)
    self.message = visShorten(message)
    self.source = source
  }

  public var description: String { "\(path): \(message)" }
}

private let identifierPattern = try! NSRegularExpression(pattern: "^[A-Za-z_][A-Za-z0-9_]*$")

private func isIdentifier(_ text: String) -> Bool {
  identifierPattern.firstMatch(in: text, range: NSRange(text.startIndex..., in: text)) != nil
}

/// Format a path like the SDK's `_path`: `$`, `.name`, `[0]` and `["odd key"]`.
public func visPath(_ parts: [VisPathPart]) -> String {
  var text = "$"
  for part in parts {
    switch part {
    case .index(let index): text += "[\(index)]"
    case .key(let name):
      text += isIdentifier(name) ? ".\(name)" : "[\(visBrief(.string(name)))]"
    }
  }
  return text
}

private let problemsShown = 10
private let problemLimit = 100

/// The problem list of correction prompts and errors, like the SDK's `_problem_lines`.
public func visProblemLines(_ problems: [VisProblem]) -> String {
  var lines = problems.map { "\n- \($0.path): \($0.message)" }
  let hidden = lines.count - problemsShown
  if hidden > 0 {
    lines.replaceSubrange(problemsShown..., with: ["\n- ...and \(hidden) more"])
  }
  return lines.joined()
}

/// A response schema that the contract cannot use.
public struct VisSchemaError: Error, Equatable, CustomStringConvertible {
  public let message: String
  public var description: String { message }
}

/// A JSON Schema 2020-12 failure before the SDK's problem formatting.
struct SchemaFailure {
  var path: [VisPathPart]
  var keyword: String?
  var expected: VisJSON?
  var instance: VisJSON
  /// The schema object that holds the failed keyword.
  var schema: VisJSON
  var message: String?
  var branches: [[SchemaFailure]] = []
}

/// A JSON Schema 2020-12 validator for self-contained answer schemas.
///
/// It covers the keywords that Pydantic and hand-written answer schemas use.
/// `format` is an annotation, as in the SDK's validator.
struct VisSchemaValidator {
  let root: VisJSON

  func failures(_ instance: VisJSON, _ schema: VisJSON, _ path: [VisPathPart]) -> [SchemaFailure] {
    if case .bool(let allowed) = schema {
      return allowed
        ? []
        : [SchemaFailure(path: path, keyword: nil, expected: nil, instance: instance, schema: schema)]
    }
    guard let members = schema.members else { return [] }
    var found = [SchemaFailure]()
    func fail(_ keyword: String, _ expected: VisJSON, message: String? = nil) {
      found.append(
        SchemaFailure(
          path: path, keyword: keyword, expected: expected, instance: instance, schema: schema,
          message: message))
    }
    for (keyword, expected) in members {
      switch keyword {
      case "$ref":
        if let reference = expected.stringValue, let target = resolve(reference) {
          found += failures(instance, target, path)
        }
      case "type":
        let names = expected.arrayValue?.compactMap(\.stringValue) ?? [expected.stringValue ?? ""]
        if !names.contains(where: { Self.isType(instance, $0) }) { fail(keyword, expected) }
      case "enum":
        if !(expected.arrayValue ?? []).contains(where: { visJSONEqual($0, instance) }) {
          fail(keyword, expected)
        }
      case "const":
        if !visJSONEqual(expected, instance) { fail(keyword, expected) }
      case "properties":
        guard let object = instance.members, let properties = expected.members else { continue }
        for (name, subschema) in properties {
          if let value = object.first(where: { $0.0 == name })?.1 {
            found += failures(value, subschema, path + [.key(name)])
          }
        }
      case "patternProperties":
        guard let object = instance.members, let patterns = expected.members else { continue }
        for (pattern, subschema) in patterns {
          for (name, value) in object where Self.search(pattern, name) {
            found += failures(value, subschema, path + [.key(name)])
          }
        }
      case "additionalProperties":
        guard let object = instance.members else { continue }
        let declared = schema["properties"]?.members?.map(\.0) ?? []
        let patterns = schema["patternProperties"]?.members?.map(\.0) ?? []
        let extras = object.filter { member in
          !declared.contains(member.0) && !patterns.contains { Self.search($0, member.0) }
        }
        if case .bool(false) = expected {
          if !extras.isEmpty {
            let names = extras.map { Self.pythonRepr($0.0) }.joined(separator: ", ")
            let verb = extras.count == 1 ? "was" : "were"
            fail(
              keyword, expected,
              message: "Additional properties are not allowed (\(names) \(verb) unexpected)")
          }
        } else {
          for (name, value) in extras {
            found += failures(value, expected, path + [.key(name)])
          }
        }
      case "required":
        guard let object = instance.members, let names = expected.arrayValue else { continue }
        let present = Set(object.map(\.0))
        if names.contains(where: { !present.contains($0.stringValue ?? "") }) {
          fail(keyword, expected)
        }
      case "dependentRequired":
        guard let object = instance.members, let rules = expected.members else { continue }
        let present = Set(object.map(\.0))
        for (name, needed) in rules where present.contains(name) {
          if (needed.arrayValue ?? []).contains(where: { !present.contains($0.stringValue ?? "") }) {
            fail(keyword, expected)
          }
        }
      case "propertyNames":
        guard let object = instance.members else { continue }
        for (name, _) in object { found += failures(.string(name), expected, path) }
      case "minProperties", "maxProperties":
        guard let object = instance.members, let bound = expected.doubleValue else { continue }
        let count = Double(object.count)
        if keyword == "minProperties" ? count < bound : count > bound { fail(keyword, expected) }
      case "prefixItems":
        guard let items = instance.arrayValue, let schemas = expected.arrayValue else { continue }
        for (offset, pair) in zip(items, schemas).enumerated() {
          found += failures(pair.0, pair.1, path + [.index(offset)])
        }
      case "items":
        guard let items = instance.arrayValue else { continue }
        let start = schema["prefixItems"]?.arrayValue?.count ?? 0
        guard items.count > start else { continue }
        if case .bool(false) = expected {
          fail(keyword, expected)
          continue
        }
        for offset in start..<items.count {
          found += failures(items[offset], expected, path + [.index(offset)])
        }
      case "contains":
        guard let items = instance.arrayValue else { continue }
        let matches = items.filter { failures($0, expected, path).isEmpty }.count
        let least = schema["minContains"]?.doubleValue ?? 1
        if Double(matches) < least { fail(keyword, expected) }
        if let most = schema["maxContains"]?.doubleValue, Double(matches) > most {
          fail("maxContains", .integer(String(Int(most))))
        }
      case "minItems", "maxItems":
        guard let items = instance.arrayValue, let bound = expected.doubleValue else { continue }
        let count = Double(items.count)
        if keyword == "minItems" ? count < bound : count > bound { fail(keyword, expected) }
      case "uniqueItems":
        guard expected.boolValue == true, let items = instance.arrayValue else { continue }
        var isUnique = true
        for left in 0..<items.count {
          for right in (left + 1)..<max(items.count, left + 1)
          where visJSONEqual(items[left], items[right]) {
            isUnique = false
          }
        }
        if !isUnique { fail(keyword, expected) }
      case "minLength", "maxLength":
        guard let text = instance.stringValue, let bound = expected.doubleValue else { continue }
        let count = Double(text.unicodeScalars.count)
        if keyword == "minLength" ? count < bound : count > bound { fail(keyword, expected) }
      case "pattern":
        guard let text = instance.stringValue, let pattern = expected.stringValue else { continue }
        if !Self.search(pattern, text) { fail(keyword, expected) }
      case "minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum":
        guard Self.isType(instance, "number"), let value = instance.doubleValue,
          let bound = expected.doubleValue
        else { continue }
        let isValid: Bool
        switch keyword {
        case "minimum": isValid = value >= bound
        case "maximum": isValid = value <= bound
        case "exclusiveMinimum": isValid = value > bound
        default: isValid = value < bound
        }
        if !isValid { fail(keyword, expected) }
      case "multipleOf":
        guard Self.isType(instance, "number"), let value = instance.doubleValue,
          let divisor = expected.doubleValue, divisor != 0
        else { continue }
        let quotient = value / divisor
        if quotient.isFinite && quotient.rounded(.towardZero) != quotient {
          fail(keyword, expected)
        }
      case "allOf":
        for subschema in expected.arrayValue ?? [] { found += failures(instance, subschema, path) }
      case "anyOf", "oneOf":
        let branches = (expected.arrayValue ?? []).map { failures(instance, $0, path) }
        let valid = branches.filter(\.isEmpty).count
        if valid == 0 {
          var failure = SchemaFailure(
            path: path, keyword: keyword, expected: expected, instance: instance, schema: schema)
          failure.branches = branches
          found.append(failure)
        } else if keyword == "oneOf" && valid > 1 {
          fail(keyword, expected)
        }
      case "not":
        if failures(instance, expected, path).isEmpty { fail(keyword, expected) }
      case "if":
        let thenSchema = schema["then"]
        let elseSchema = schema["else"]
        if failures(instance, expected, path).isEmpty {
          if let thenSchema { found += failures(instance, thenSchema, path) }
        } else if let elseSchema {
          found += failures(instance, elseSchema, path)
        }
      default:
        continue
      }
    }
    return found
  }

  /// Resolve a local reference such as `#`, `#/$defs/Item` or `#/definitions/Item`.
  func resolve(_ reference: String) -> VisJSON? {
    guard reference.hasPrefix("#") else { return nil }
    var node = root
    let pointer = reference.dropFirst()
    guard !pointer.isEmpty else { return node }
    for raw in pointer.split(separator: "/", omittingEmptySubsequences: false).dropFirst() {
      let token = String(raw).removingPercentEncoding ?? String(raw)
      let name = token.replacingOccurrences(of: "~1", with: "/")
        .replacingOccurrences(of: "~0", with: "~")
      if let items = node.arrayValue, let offset = Int(name), items.indices.contains(offset) {
        node = items[offset]
      } else if let next = node[name] {
        node = next
      } else {
        return nil
      }
    }
    return node
  }

  static func isType(_ value: VisJSON, _ name: String) -> Bool {
    switch (name, value) {
    case ("null", .null), ("boolean", .bool), ("string", .string), ("array", .array),
      ("object", .object), ("integer", .integer), ("number", .integer), ("number", .number):
      return true
    case ("integer", .number(let number)):
      return number.rounded() == number
    default:
      return false
    }
  }

  static func search(_ pattern: String, _ text: String) -> Bool {
    guard let regex = try? NSRegularExpression(pattern: pattern) else { return false }
    return regex.firstMatch(in: text, range: NSRange(text.startIndex..., in: text)) != nil
  }

  static func pythonRepr(_ text: String) -> String {
    let quote = text.contains("'") && !text.contains("\"") ? "\"" : "'"
    var out = quote
    for scalar in text.unicodeScalars {
      switch scalar {
      case "\\": out += "\\\\"
      case "\n": out += "\\n"
      case "\r": out += "\\r"
      case "\t": out += "\\t"
      default:
        if String(scalar) == quote {
          out += "\\" + quote
        } else {
          out.unicodeScalars.append(scalar)
        }
      }
    }
    return out + quote
  }

  /// The relevance order of jsonschema's `by_relevance`: a smaller key is a better match.
  static func isMoreRelevant(_ left: SchemaFailure, _ right: SchemaFailure) -> Bool {
    if left.path.count != right.path.count { return left.path.count > right.path.count }
    if left.path != right.path { return pathPrecedes(left.path, right.path) }
    let weak: Set<String> = ["anyOf", "oneOf"]
    let leftWeak = left.keyword.map(weak.contains) ?? false
    let rightWeak = right.keyword.map(weak.contains) ?? false
    if leftWeak != rightWeak { return leftWeak }
    return matchesType(left) && !matchesType(right)
  }

  static func isSameRelevance(_ left: SchemaFailure, _ right: SchemaFailure) -> Bool {
    !isMoreRelevant(left, right) && !isMoreRelevant(right, left)
  }

  static func pathPrecedes(_ left: [VisPathPart], _ right: [VisPathPart]) -> Bool {
    for (a, b) in zip(left, right) where a != b {
      switch (a, b) {
      case (.index(let x), .index(let y)): return x < y
      case (.key(let x), .key(let y)): return x < y
      case (.index, .key): return true
      case (.key, .index): return false
      }
    }
    return left.count < right.count
  }

  /// jsonschema's `_matches_type`: the value has a type that the failed schema names.
  static func matchesType(_ failure: SchemaFailure) -> Bool {
    guard let expected = failure.schema["type"] else { return false }
    let names = expected.arrayValue?.compactMap(\.stringValue) ?? [expected.stringValue ?? ""]
    return names.contains { isType(failure.instance, $0) }
  }

  /// Expand an anyOf or oneOf failure to every failure in its closest branch.
  ///
  /// This is the SDK's `_relevant_errors` with jsonschema's `best_match`: take
  /// the most relevant nested failure, and stop where the two best tie.
  static func relevant(_ failure: SchemaFailure) -> [SchemaFailure] {
    guard !failure.branches.isEmpty else { return [failure] }
    var best = failure
    var branch: Int?
    while true {
      let context = best.branches.enumerated().flatMap { offset, items in
        items.map { (offset, $0) }
      }
      guard !context.isEmpty else { break }
      var ranked = context
      // A stable insertion sort keeps the first of equal failures first, as `nsmallest` does.
      for index in ranked.indices.dropFirst() {
        var cursor = index
        while cursor > 0 && isMoreRelevant(ranked[cursor].1, ranked[cursor - 1].1) {
          ranked.swapAt(cursor, cursor - 1)
          cursor -= 1
        }
      }
      if ranked.count >= 2 && isSameRelevance(ranked[0].1, ranked[1].1) { break }
      if branch == nil { branch = ranked[0].0 }
      best = ranked[0].1
    }
    guard let branch else { return [failure] }
    return failure.branches[branch].flatMap(relevant)
  }

  /// Describe one failure without whole objects or arrays, like `_schema_problems`.
  static func problems(_ failure: SchemaFailure, showInput: Bool) -> [VisProblem] {
    let value = failure.instance
    if failure.keyword == "required", let object = value.members,
      let names = failure.expected?.arrayValue
    {
      let present = Set(object.map(\.0))
      return names.compactMap(\.stringValue).filter { !present.contains($0) }.map {
        VisProblem(
          source: "schema", parts: failure.path + [.key($0)],
          message: "required property is missing")
      }
    }
    var message: String
    switch failure.keyword {
    case "additionalProperties"?, "unevaluatedProperties"?:
      message = failure.message ?? "does not satisfy \(failure.keyword!)"
    case nil:
      message = "no value is allowed here"
    case "type"?:
      let expected = failure.expected
      let types = expected?.arrayValue?.compactMap(\.stringValue).joined(separator: " or ")
        ?? expected?.stringValue ?? ""
      message = "expected \(types), got \(value.typeName)"
    case let keyword?:
      message = "does not satisfy \(keyword) \(visBrief(failure.expected ?? .null))"
    }
    if showInput && value.isScalar && failure.keyword != nil {
      message += " (input: \(visBrief(value)))"
    }
    return [VisProblem(source: "schema", parts: failure.path, message: message)]
  }
}

/// Prompts and checks for one response schema, like the SDK's `_ResponseContract`.
///
/// Give the JSON Schema that the SDK takes from `response_model`. The request
/// and correction prompts are the SDK's prompts, with the same schema text.
public struct VisResponseContract {
  public let schema: VisJSON
  /// The schema in Python `json.dumps(schema, ensure_ascii=False)` form.
  public let schemaText: String
  public let showInput: Bool
  let validator: VisSchemaValidator

  public init(schemaText text: String, showInput: Bool = true) throws {
    let schema: VisJSON
    do {
      schema = try VisJSON.parse(text)
    } catch let error as VisJSONSyntaxError {
      throw VisSchemaError(
        message:
          "response schema is not valid JSON: \(error.message) at line \(error.line), column \(error.column)"
      )
    } catch let error as VisJSONValueError {
      throw VisSchemaError(message: "response schema is not valid JSON: \(error.message)")
    }
    try self.init(schema: schema, showInput: showInput)
  }

  public init(schema: VisJSON, showInput: Bool = true) throws {
    switch schema {
    case .object, .bool: break
    default:
      throw VisSchemaError(message: "response schema must be a JSON object")
    }
    try Self.check(schema)
    if let reference = Self.externalReferences(schema).first {
      throw VisSchemaError(
        message:
          "response schema must be self-contained; unsupported reference \(VisSchemaValidator.pythonRepr(reference))"
      )
    }
    self.schema = schema
    self.schemaText = schema.pythonText
    self.showInput = showInput
    self.validator = VisSchemaValidator(root: schema)
  }

  static let typeNames: Set<String> = [
    "null", "boolean", "object", "array", "number", "string", "integer",
  ]

  /// Refuse the schema mistakes that the SDK's `check_schema` refuses most often.
  static func check(_ schema: VisJSON) throws {
    guard let members = schema.members else { return }
    for (keyword, value) in members {
      switch keyword {
      case "type":
        let names = value.arrayValue?.map(\.stringValue) ?? [value.stringValue]
        if names.isEmpty || names.contains(where: { $0 == nil || !typeNames.contains($0!) }) {
          throw VisSchemaError(
            message: "response schema is not a valid JSON Schema: \(visBrief(value)) is not a valid type")
        }
      case "required":
        guard let names = value.arrayValue, names.allSatisfy({ $0.stringValue != nil }) else {
          throw VisSchemaError(
            message: "response schema is not a valid JSON Schema: required must be a list of names")
        }
      case "properties", "$defs", "definitions", "patternProperties", "dependentSchemas":
        guard let entries = value.members else {
          throw VisSchemaError(
            message: "response schema is not a valid JSON Schema: \(keyword) must be an object")
        }
        for (_, subschema) in entries { try check(subschema) }
      case "items", "additionalProperties", "not", "if", "then", "else", "contains",
        "propertyNames":
        try check(value)
      case "allOf", "anyOf", "oneOf", "prefixItems":
        guard let items = value.arrayValue, !items.isEmpty else {
          throw VisSchemaError(
            message: "response schema is not a valid JSON Schema: \(keyword) must be a non-empty list")
        }
        for subschema in items { try check(subschema) }
      default:
        continue
      }
    }
  }

  static func externalReferences(_ node: VisJSON) -> [String] {
    switch node {
    case .object(let members):
      return members.flatMap { name, value -> [String] in
        if name == "$ref" || name == "$dynamicRef", let reference = value.stringValue {
          return reference.hasPrefix("#") ? [] : [reference]
        }
        return externalReferences(value)
      }
    case .array(let items):
      return items.flatMap(externalReferences)
    default:
      return []
    }
  }

  /// The first prompt: the request, then the answer rules and the schema.
  public func request(_ request: String) -> String {
    "\(request)\n\n"
      + "Return the final answer as exactly one JSON value matching the schema. "
      + "Do not include Markdown fences, commentary, or text outside the JSON. "
      + "Match this JSON Schema:\n\(schemaText)"
  }

  /// A correction prompt that lists each problem and repeats the schema.
  public func correction(_ problems: [VisProblem]) -> String {
    "Your previous final answer is not a valid structured result "
      + "($ is the whole JSON value):\(visProblemLines(problems))\n\n"
      + "Answer the previous request again with exactly one corrected JSON value. "
      + "Do not include Markdown fences, commentary, or text outside the JSON. "
      + "Keep completed work; repeat tools or file changes only if the correction "
      + "requires them. Match this JSON Schema:\n\(schemaText)"
  }

  /// Return the parsed answer, or no value and the answer's problems.
  public func validate(_ answer: String?) -> (value: VisJSON?, problems: [VisProblem]) {
    guard let answer else {
      return (nil, [VisProblem(source: "answer", parts: [], message: "the turn has no final text answer")])
    }
    let payload: VisJSON
    do {
      payload = try VisJSON.parse(answer)
    } catch let error as VisJSONSyntaxError {
      return (nil, [VisProblem(source: "json", parts: [], message: Self.syntaxMessage(answer, error))])
    } catch let error as VisJSONValueError {
      return (nil, [VisProblem(source: "json", parts: [], message: error.message)])
    } catch {
      return (nil, [VisProblem(source: "json", parts: [], message: "\(error)")])
    }
    var problems = [VisProblem]()
    var seen = Set<String>()
    var counted = 0
    outer: for failure in validator.failures(payload, schema, []) {
      for leaf in VisSchemaValidator.relevant(failure) {
        for problem in VisSchemaValidator.problems(leaf, showInput: showInput) {
          counted += 1
          if counted > problemLimit { break outer }
          if seen.insert(problem.path + "\u{0}" + problem.message).inserted {
            problems.append(problem)
          }
        }
      }
    }
    return problems.isEmpty ? (payload, []) : (nil, problems)
  }

  static func syntaxMessage(_ answer: String, _ error: VisJSONSyntaxError) -> String {
    let scalars = Array(answer.unicodeScalars)
    let start = scalars.firstIndex { !$0.properties.isWhitespace } ?? scalars.count
    if Array(scalars[start...].prefix(3)) == Array("```".unicodeScalars) {
      return "the answer is wrapped in a Markdown code fence; send only the JSON value"
    }
    let place = "line \(error.line), column \(error.column)"
    if error.message == "Extra data" {
      return "text follows the JSON value at \(place); send exactly one JSON value"
    }
    if error.message == "Expecting value" && error.position == start {
      return "the answer starts with text instead of a JSON value"
    }
    return "not valid JSON: \(error.message) at \(place)"
  }
}
