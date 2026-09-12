import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { describe, expect, it } from 'vitest';

const prepare = readFileSync(new URL('./ios-prepare.mjs', import.meta.url), 'utf8');
const controller = prepare.match(/const shareControllerSource = `([\s\S]*?)\n`;/)?.[1] ?? '';
const methods = controller.slice(
  controller.indexOf('    private func stageableType('),
  controller.indexOf('    private func load('),
);
const stagedFile = controller.match(/    private struct StagedFile \{[\s\S]*?\n    \}/)?.[0];
const sizeLimit = controller.match(/    private static let maxFileBytes[^\n]*/)?.[0];

describe.skipIf(process.platform !== 'darwin')('iOS share file intake', () => {
  // User regression: sharing PLAN.md back to Vis only showed a success notice.
  // Execute the generated controller's real staging methods with Apple's providers,
  // without requiring UIKit, a signed app, or a running simulator.
  it('stages named text files and logs without converting selected prose to a file', () => {
    expect(stagedFile).toBeTruthy();
    expect(sizeLimit).toBeTruthy();
    const swift = `
import Foundation
import UniformTypeIdentifiers

final class Intake {
${stagedFile?.replace('private struct', 'struct')}
${sizeLimit}
${methods.replace('private func stage(', 'func stage(')}
}

func check() async throws {
    let directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: directory) }
    let intake = Intake()
    let bytes = Data("Shared file contents".utf8)
    for (index, name) in ["PLAN.md", "vis-diagnostics.jsonl", "vis-diagnostics.jsonl.gz"].enumerated() {
        let source = directory.appendingPathComponent(name)
        try bytes.write(to: source)
        let provider = NSItemProvider(contentsOf: source)!
        guard let staged = await intake.stage(provider, into: directory.appendingPathComponent("staged"), index: index),
              staged.name == name, try Data(contentsOf: staged.url) == bytes else {
            throw NSError(domain: "Share regression: failed to stage \\(name)", code: 1)
        }
    }
    let document = NSItemProvider(item: bytes as NSData, typeIdentifier: UTType.pdf.identifier)
    document.suggestedName = "report.pdf"
    guard let represented = await intake.stage(document, into: directory.appendingPathComponent("staged"), index: 3),
          represented.name == "report.pdf", try Data(contentsOf: represented.url) == bytes else {
        throw NSError(domain: "File representation was lost", code: 1)
    }
    let prose = NSItemProvider(item: "Selected words" as NSString, typeIdentifier: UTType.plainText.identifier)
    guard await intake.stage(prose, into: directory, index: 4) == nil else {
        throw NSError(domain: "Selected prose became a file", code: 1)
    }
    print("Files preserved; selected prose stays text")
}

Task {
    do { try await check(); exit(0) }
    catch { print(error); exit(1) }
}
dispatchMain()
`;
    const result = spawnSync('xcrun', ['swift', '-e', swift], {
      encoding: 'utf8',
      timeout: 30_000,
    });
    expect(result.status, result.stdout + result.stderr).toBe(0);
    expect(result.stdout).toContain('Files preserved; selected prose stays text');
  }, 35_000);
});
