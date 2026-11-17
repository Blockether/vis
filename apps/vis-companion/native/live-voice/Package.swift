// swift-tools-version: 5.9
import PackageDescription

let package = Package(
  name: "LiveVoice", platforms: [.macOS(.v12), .iOS(.v15)],
  products: [.library(name: "LiveVoice", targets: ["LiveVoice"])],
  targets: [
    .target(name: "LiveVoice"),
    .testTarget(name: "LiveVoiceTests", dependencies: ["LiveVoice"]),
  ])
