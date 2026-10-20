// swift-tools-version: 5.9
import PackageDescription

let package = Package(
  name: "VisShortcutsCore", platforms: [.macOS(.v12), .iOS(.v15)],
  products: [.library(name: "VisShortcutsCore", targets: ["VisShortcutsCore"])],
  targets: [
    .target(name: "VisShortcutsCore"),
    .testTarget(
      name: "VisShortcutsCoreTests", dependencies: ["VisShortcutsCore"],
      resources: [.copy("Fixtures")]),
  ])
