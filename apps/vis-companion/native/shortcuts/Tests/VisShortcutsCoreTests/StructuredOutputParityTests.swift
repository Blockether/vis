import XCTest

@testable import VisShortcutsCore

/// The Swift response contract must give the SDK's prompts and problems.
///
/// `scripts/sdk_parity.py` writes the fixture from the Python SDK.
final class StructuredOutputParityTests: XCTestCase {
  func fixture() throws -> VisJSON {
    let url = try XCTUnwrap(
      Bundle.module.url(forResource: "sdk-parity", withExtension: "json", subdirectory: "Fixtures"))
    return try VisJSON.parse(String(contentsOf: url, encoding: .utf8))
  }

  func testPromptsAndProblemsMatchTheSDK() throws {
    let cases = try XCTUnwrap(fixture()["cases"]?.arrayValue)
    XCTAssertFalse(cases.isEmpty)
    for item in cases {
      let model = item["model"]?.stringValue ?? "?"
      let contract = try VisResponseContract(schema: XCTUnwrap(item["schema"]))
      XCTAssertEqual(contract.schemaText, item["schema_text"]?.stringValue, model)
      XCTAssertEqual(
        contract.request("Quote a 1000 gram parcel."), item["request"]?.stringValue, model)
      for row in item["answers"]?.arrayValue ?? [] {
        let answer = row["answer"]?.stringValue
        let (value, problems) = contract.validate(answer)
        let label = "\(model): \(answer ?? "nil")"
        XCTAssertEqual(value != nil, row["is_valid"]?.boolValue, label)
        let expected = (row["problems"]?.arrayValue ?? []).map {
          "\($0["source"]?.stringValue ?? "") \($0["path"]?.stringValue ?? ""): \($0["message"]?.stringValue ?? "")"
        }
        XCTAssertEqual(problems.map { "\($0.source) \($0.path): \($0.message)" }, expected, label)
        if let correction = row["correction"]?.stringValue {
          XCTAssertEqual(contract.correction(problems), correction, label)
        }
      }
    }
  }

  func testProblemLinesShowTenThenACount() throws {
    let lines = try XCTUnwrap(fixture()["problem_lines"])
    let problems = (lines["problems"]?.arrayValue ?? []).map {
      VisProblem(
        source: "schema", parts: [.key(String(($0["path"]?.stringValue ?? "").dropFirst(2)))],
        message: $0["message"]?.stringValue ?? "")
    }
    XCTAssertEqual(visProblemLines(problems), lines["text"]?.stringValue)
  }

  func testSchemaErrorsAreRefusedBeforeAnyTurn() {
    XCTAssertThrowsError(try VisResponseContract(schemaText: "{\"type\": \"strin\"}"))
    XCTAssertThrowsError(try VisResponseContract(schemaText: "[1]"))
    XCTAssertThrowsError(try VisResponseContract(schemaText: "{\"type\": \"object\""))
    XCTAssertThrowsError(
      try VisResponseContract(schemaText: "{\"$ref\": \"https://example.com/schema.json\"}")
    ) { error in
      XCTAssertEqual(
        (error as? VisSchemaError)?.message,
        "response schema must be self-contained; unsupported reference 'https://example.com/schema.json'")
    }
  }

  func testHandWrittenSchemasUseTheCommonKeywords() throws {
    let contract = try VisResponseContract(
      schemaText: """
        {"type": "object", "additionalProperties": false,
         "properties": {"city": {"type": "string", "pattern": "^[A-Z]"},
                        "days": {"type": "array", "items": {"type": "integer"}, "uniqueItems": true},
                        "mode": {"oneOf": [{"const": "walk"}, {"const": "drive"}]}},
         "required": ["city"]}
        """)
    XCTAssertNotNil(contract.validate(#"{"city": "Oslo", "days": [1, 2], "mode": "walk"}"#).value)
    let problems = contract.validate(#"{"city": "oslo", "days": [1, 1], "mode": "fly", "x": 1}"#)
      .problems.map(\.description)
    XCTAssertEqual(
      problems,
      [
        "$: Additional properties are not allowed ('x' was unexpected)",
        #"$.city: does not satisfy pattern "^[A-Z]" (input: "oslo")"#,
        "$.days: does not satisfy uniqueItems true",
        #"$.mode: does not satisfy oneOf [{"const": "walk"}, {"const": "drive"}] (input: "fly")"#,
      ])
  }

  func testJSONTextKeepsOrderAndPythonFormatting() throws {
    let value = try VisJSON.parse(#"{"b": 1.0, "a": [true, null, "é\n"], "c": 10000000000000000.0}"#)
    XCTAssertEqual(value.pythonText, #"{"b": 1.0, "a": [true, null, "é\n"], "c": 1e+16}"#)
  }
}
