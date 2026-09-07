// swift-tools-version: 5.9
import PackageDescription

let package = Package(
  name: "OAuthLoopback", platforms: [.macOS(.v12), .iOS(.v15)],
  products: [.library(name: "OAuthLoopback", targets: ["OAuthLoopback"])],
  targets: [
    .target(name: "OAuthLoopback"),
    .testTarget(name: "OAuthLoopbackTests", dependencies: ["OAuthLoopback"]),
  ])
