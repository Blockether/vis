#!/usr/bin/env node
/**
 * Prepare the generated iOS project around Capacitor's stock bridge controller.
 *
 * This used to install a tracked `CAPBridgeViewController` subclass
 * (`native/ios/VisBridgeViewController.swift`) that pushed UIKit's view size into
 * the web layer. That is gone: the app rides the plain Capacitor host, and a
 * rotation is handled where it is cheap — the web layer simply stops measuring
 * for the length of the flip (`src/lib/viewport.ts`, and the `ResizeObserver`s in
 * `SessionScreen`/`ChatContent` that skip while it is open).
 *
 * Deleting the Swift source is not enough on its own. `ios/` is gitignored but it
 * is NOT regenerated on a machine that already has it: an existing checkout still
 * carries the stamped class in `AppDelegate.swift` and a `Main.storyboard` whose
 * root controller is `customClass="VisBridgeViewController"`. Leaving that behind
 * means the storyboard names a class that no longer exists — a crash on launch.
 *
 * The hook removes that obsolete bridge and restores `CAPBridgeViewController`.
 * It also stamps the small native lifecycle behavior that must happen before the
 * asynchronous JavaScript bridge can react: ending keyboard editing synchronously
 * as the app resigns active. Every operation is idempotent.
 *
 * Usage:
 *   node scripts/ios-prepare.mjs
 *   node scripts/ios-prepare.mjs --check   # exit 1 if the project needs preparation
 */
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const check = process.argv.slice(2).includes('--check');

const BEGIN = '// vis:viewport-bridge:begin';
const CUSTOM_CLASS = 'VisBridgeViewController';

const appDir = join(root, 'ios', 'App', 'App');
const delegate = join(appDir, 'AppDelegate.swift');
const storyboard = join(appDir, 'Base.lproj', 'Main.storyboard');
const infoPlist = join(appDir, 'Info.plist');
const bundleId = JSON.parse(readFileSync(join(root, 'capacitor.config.json'), 'utf8')).appId;
// The share extension is a SEPARATE process with a SEPARATE container, so a file
// it was handed can only reach the app through a container both are entitled to.
// One group, named after the app, carried by the app target and the extension.
const appGroup = `group.${bundleId}`;

const appIconSource = join(root, 'native-assets', 'ios', 'AppIcon-512@2x.png');
const appIconTarget = join(appDir, 'Assets.xcassets', 'AppIcon.appiconset', 'AppIcon-512@2x.png');
const appIcon = readFileSync(appIconSource);
const appIconOk = existsSync(appIconTarget) && readFileSync(appIconTarget).equals(appIcon);

// ── Launch screen ─────────────────────────────────────────────────────────────────────
//
// Capacitor's launch screen aspect-fills ONE 2732² image carrying its own logo, so the mark
// on screen is both the wrong mark and a size that depends on which scale the asset catalog
// serves the device. The tracked one is the picture the web layer paints a frame later
// (`App.tsx` `<Splash/>`: the mark at `h-16` = 64 pt on `--bg`), drawn at its intrinsic size
// on the same colour — so the handover from launch image to first paint moves nothing.
//
// It is declared with `UILaunchScreen`, NOT `UILaunchStoryboardName`, because SplashBoard
// composes a storyboard into one bitmap per orientation and refuses over a budget:
//   XBLaunchStoryboardErrorDomain Code=6 "Unable to generate launch image"
//   Estimated size (29900800) is over limit (25000000)
// measured on an iPhone 17 Pro simulator, whose screen alone exceeds it — the app then
// launches on BLACK. The same build on an iPhone 17e (a slightly smaller screen) composed
// fine and showed the mark, and with `UILaunchScreen` both devices show it. The storyboard
// is still rewritten below: the Xcode target lists it as a resource, so the bundle carries
// one either way, and it must not be the one with somebody else's logo in it.
const splashDir = join(appDir, 'Assets.xcassets', 'Splash.imageset');
const splashImages = [
  ['Splash-mark.png', 'splash-mark.png'],
  ['Splash-mark@2x.png', 'splash-mark@2x.png'],
  ['Splash-mark@3x.png', 'splash-mark@3x.png'],
];
const splashContents = `{
  "images": [
    {
      "idiom": "universal",
      "filename": "splash-mark.png",
      "scale": "1x"
    },
    {
      "idiom": "universal",
      "filename": "splash-mark@2x.png",
      "scale": "2x"
    },
    {
      "idiom": "universal",
      "filename": "splash-mark@3x.png",
      "scale": "3x"
    }
  ],
  "info": {
    "version": 1,
    "author": "vis"
  }
}
`;
const splashStale = existsSync(splashDir)
  ? readdirSync(splashDir).filter((name) => name.startsWith('splash-2732'))
  : [];
const splashFilesOk =
  splashStale.length === 0 &&
  existsSync(join(splashDir, 'Contents.json')) &&
  readFileSync(join(splashDir, 'Contents.json'), 'utf8') === splashContents &&
  splashImages.every(
    ([source, target]) =>
      existsSync(join(splashDir, target)) &&
      readFileSync(join(splashDir, target)).equals(readFileSync(join(root, 'native-assets', 'ios', source))),
  );

// The colour lives in the asset catalog because `UILaunchScreen` names it there — the one
// place both halves of the launch screen (colour + mark) can be read by the system without
// a nib.
const splashColorDir = join(appDir, 'Assets.xcassets', 'SplashBackground.colorset');
const splashColorContents = `{
  "colors": [
    {
      "idiom": "universal",
      "color": {
        "color-space": "srgb",
        "components": {
          "red": "0.98039215686274506",
          "green": "0.95294117647058818",
          "blue": "0.92156862745098034",
          "alpha": "1.000"
        }
      }
    }
  ],
  "info": {
    "version": 1,
    "author": "vis"
  }
}
`;
const splashColorOk =
  existsSync(join(splashColorDir, 'Contents.json')) &&
  readFileSync(join(splashColorDir, 'Contents.json'), 'utf8') === splashColorContents;

const launchBoard = join(appDir, 'Base.lproj', 'LaunchScreen.storyboard');
const launchBoardSource = `<?xml version="1.0" encoding="UTF-8"?>
<document type="com.apple.InterfaceBuilder3.CocoaTouch.Storyboard.XIB" version="3.0" toolsVersion="17132" targetRuntime="iOS.CocoaTouch" propertyAccessControl="none" useAutolayout="YES" launchScreen="YES" useTraitCollections="YES" useSafeAreas="YES" colorMatched="YES" initialViewController="01J-lp-oVM">
    <device id="retina4_7" orientation="portrait" appearance="light"/>
    <dependencies>
        <deployment identifier="iOS"/>
        <plugIn identifier="com.apple.InterfaceBuilder.IBCocoaTouchPlugin" version="17105"/>
        <capability name="documents saved in the Xcode 8 format" minToolsVersion="8.0"/>
    </dependencies>
    <scenes>
        <!--View Controller-->
        <scene sceneID="EHf-IW-A2E">
            <objects>
                <viewController id="01J-lp-oVM" sceneMemberID="viewController">
                    <imageView key="view" userInteractionEnabled="NO" contentMode="center" horizontalHuggingPriority="251" verticalHuggingPriority="251" image="Splash" id="snD-IY-ifK">
                        <rect key="frame" x="0.0" y="0.0" width="375" height="667"/>
                        <autoresizingMask key="autoresizingMask"/>
                        <color key="backgroundColor" red="0.98039215686274506" green="0.95294117647058818" blue="0.92156862745098034" alpha="1" colorSpace="custom" customColorSpace="sRGB"/>
                    </imageView>
                </viewController>
                <placeholder placeholderIdentifier="IBFirstResponder" id="iYj-Kq-Ea1" userLabel="First Responder" sceneMemberID="firstResponder"/>
            </objects>
            <point key="canvasLocation" x="53" y="375"/>
        </scene>
    </scenes>
    <resources>
        <image name="Splash" width="72" height="64"/>
    </resources>
</document>
`;
const launchBoardOk = existsSync(launchBoard) && readFileSync(launchBoard, 'utf8') === launchBoardSource;
const splashOk = splashFilesOk && splashColorOk && launchBoardOk;

const die = (msg) => {
  console.error(`\n\u2717 ${msg}\n`);
  process.exit(1);
};

// A machine that only builds Android (or web) has no ios/ at all; that is not a
// failure, it just has nothing to clean.
if (!existsSync(delegate)) {
  if (check) die('no ios/App/App/AppDelegate.swift — run `npm run add:ios` first');
  console.log('\u00b7 ios: no ios/ project — nothing to clean');
  process.exit(0);
}

// ── 1. generated AppDelegate hygiene + pre-background keyboard release ─────────

const currentDelegate = readFileSync(delegate, 'utf8');
const cleanedDelegate = currentDelegate.includes(BEGIN)
  ? `${currentDelegate.slice(0, currentDelegate.indexOf(BEGIN)).trimEnd()}\n`
  : currentDelegate;
const KEYBOARD_RELEASE = 'window?.endEditing(true)';
const WILL_RESIGN_ACTIVE = '    func applicationWillResignActive(_ application: UIApplication) {\n';
if (!cleanedDelegate.includes(KEYBOARD_RELEASE) && !cleanedDelegate.includes(WILL_RESIGN_ACTIVE)) {
  die('AppDelegate.swift has no applicationWillResignActive method to stamp');
}
const preparedDelegate = cleanedDelegate.includes(KEYBOARD_RELEASE)
  ? cleanedDelegate
  : cleanedDelegate.replace(
      WILL_RESIGN_ACTIVE,
      `${WILL_RESIGN_ACTIVE}        // Release WebKit's editor before UIKit starts the scene transition.\n        ${KEYBOARD_RELEASE}\n`,
    );
const delegateOk = preparedDelegate === currentDelegate;

// ── 2. the storyboard, back to Capacitor's own controller ────────────────────

const currentBoard = readFileSync(storyboard, 'utf8');
const cleanedBoard = currentBoard.replace(
  /customClass="VisBridgeViewController" customModule="App" customModuleProvider="target"/,
  'customClass="CAPBridgeViewController" customModule="Capacitor"',
);
const boardOk = !currentBoard.includes(CUSTOM_CLASS);
if (!boardOk && cleanedBoard === currentBoard) {
  die(`Main.storyboard references ${CUSTOM_CLASS} in a shape this script cannot rewrite`);
}

// ── 3. capabilities that Capacitor's generated Info.plist does not carry ──────
//
// `ios/` is gitignored and CI creates it from scratch. Every runtime-sensitive
// plist entry therefore belongs here, not in a hand-edited local Xcode project.
const plistEntries = [
  [
    'UIBackgroundModes',
    `\t<key>UIBackgroundModes</key>
\t<array>
\t\t<string>audio</string>
\t</array>`,
  ],
  [
    'NSAppTransportSecurity',
    `\t<key>NSAppTransportSecurity</key>
\t<dict>
\t\t<key>NSAllowsArbitraryLoadsInWebContent</key>
\t\t<true/>
\t\t<key>NSAllowsLocalNetworking</key>
\t\t<true/>
\t</dict>`,
  ],
  [
    'NSLocalNetworkUsageDescription',
    `\t<key>NSLocalNetworkUsageDescription</key>
\t<string>Vis connects to your gateway running on your local network, Tailscale, or a tunnel.</string>`,
  ],
  [
    'NSCameraUsageDescription',
    `\t<key>NSCameraUsageDescription</key>
\t<string>Take photos to attach to a Vis conversation and scan gateway pairing QR codes.</string>`,
  ],
  [
    'NSMicrophoneUsageDescription',
    `\t<key>NSMicrophoneUsageDescription</key>
\t<string>Dictate messages to Vis by voice.</string>`,
  ],
  [
    'NSPhotoLibraryUsageDescription',
    `\t<key>NSPhotoLibraryUsageDescription</key>
\t<string>Attach images from your photo library to a Vis conversation.</string>`,
  ],
  [
    'NSPhotoLibraryAddUsageDescription',
    `\t<key>NSPhotoLibraryAddUsageDescription</key>
\t<string>Save images shared from a Vis conversation.</string>`,
  ],
  [
    'ITSAppUsesNonExemptEncryption',
    `\t<key>ITSAppUsesNonExemptEncryption</key>
\t<false/>`,
  ],
  [
    'CFBundleURLTypes',
    `\t<key>CFBundleURLTypes</key>
\t<array>
\t\t<dict>
\t\t\t<key>CFBundleURLName</key>
\t\t\t<string>${bundleId}</string>
\t\t\t<key>CFBundleTypeRole</key>
\t\t\t<string>Editor</string>
\t\t\t<key>CFBundleURLSchemes</key>
\t\t\t<array>
\t\t\t\t<string>vis</string>
\t\t\t</array>
\t\t</dict>
\t</array>`,
  ],
  [
    'UILaunchScreen',
    `\t<key>UILaunchScreen</key>
\t<dict>
\t\t<key>UIColorName</key>
\t\t<string>SplashBackground</string>
\t\t<key>UIImageName</key>
\t\t<string>Splash</string>
\t</dict>`,
  ],
];

const currentPlist = readFileSync(infoPlist, 'utf8');
// Capacitor's scaffold points at the storyboard; `UILaunchScreen` only wins once that key is
// gone, so the removal is part of being prepared rather than a one-off cleanup.
const launchStoryboardEntry = /[\t ]*<key>UILaunchStoryboardName<\/key>\n[\t ]*<string>[^<]*<\/string>\n/;
const launchStoryboardStale = launchStoryboardEntry.test(currentPlist);
const missingPlistEntries = plistEntries.filter(([key]) => !currentPlist.includes(`<key>${key}</key>`));
const plistOk = missingPlistEntries.length === 0 && !launchStoryboardStale;
let preparedPlist = currentPlist.replace(launchStoryboardEntry, '');
if (missingPlistEntries.length > 0) {
  const at = preparedPlist.lastIndexOf('</dict>');
  if (at < 0) die('Info.plist has no root </dict>');
  const additions = `${missingPlistEntries.map(([, xml]) => xml).join('\n')}\n`;
  preparedPlist = preparedPlist.slice(0, at) + additions + preparedPlist.slice(at);
}

// ── 4. share extension + App Intents, and the Xcode target that builds them ───
//
// "Share → Vis" from Safari, and "Send to Vis" from Shortcuts/Siri, both end up
// opening `vis://share?url=…&text=…&title=…` — the exact URL the Android
// activity synthesises, so `src/lib/share-intake.ts` is the only place that
// knows what a shared payload means.
//
// A share sheet entry is a SEPARATE bundle: iOS only offers apps that ship an
// `com.apple.share-services` extension, which means a second Xcode target. App
// Intents are just Swift in the app target, but that file still has to reach the
// Sources phase. `ios/` is gitignored and CI recreates it from scratch, so both
// are stamped into project.pbxproj here rather than clicked into Xcode once.

const shareDir = join(root, 'ios', 'App', 'VisShare');
const shareController = join(shareDir, 'ShareViewController.swift');
const sharePlist = join(shareDir, 'Info.plist');
const shareEntitlements = join(shareDir, 'VisShare.entitlements');
const appEntitlements = join(appDir, 'App.entitlements');
const shortcutsSwift = join(appDir, 'VisShortcuts.swift');
const pbxprojPath = join(root, 'ios', 'App', 'App.xcodeproj', 'project.pbxproj');

// `UIApplication.shared` is unavailable to an extension by design, and
// `extensionContext.open` is documented for widgets — it answers false here.
// So the app is opened through the responder chain, which has two traps, both
// reproduced in the iOS 26 Simulator: `openURL:` is dead (UIKit force-returns NO
// and logs "BUG IN CLIENT OF UIKIT", so the share vanished into a white sheet),
// and `UIScene` answers the modern selector earlier in the chain but aborts the
// extension when called. Only `UIApplication`, only
// `openURL:options:completionHandler:`, invoked through its IMP.
const shareControllerSource = `import UIKit
import ObjectiveC
import UniformTypeIdentifiers

/// The whole extension: pull the link, the text or the FILES out of the share,
/// hand them to the app as a vis://share URL, and get off the screen. No compose
/// UI — the composer inside Vis is the compose UI, and a second one would only be
/// a slower way to type the same prompt.
///
/// A file cannot be handed over as a path: the extension's container is not the
/// app's, and the URL a provider hands out dies with the extension. Every
/// attachment is COPIED into the shared App Group container, whose absolute
/// file:// URL both processes can read; the app deletes it once the bytes are in
/// the composer, and a share the user abandoned is purged on the next one.
final class ShareViewController: UIViewController {
    private static let appGroup = "${appGroup}"
    private static let maxFiles = 8
    private static let maxFileBytes = 64 * 1024 * 1024
    private static let staleAfter: TimeInterval = 7 * 24 * 60 * 60

    private struct StagedFile {
        let url: URL
        let name: String
        let type: String
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .clear
    }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        Task { await handoff() }
    }

    private func handoff() async {
        var link: String?
        var text: String?
        var title: String?
        var files = [StagedFile]()
        let staging = stagingDirectory()

        for case let item as NSExtensionItem in extensionContext?.inputItems ?? [] {
            if title == nil, let subject = item.attributedTitle?.string, !subject.isEmpty {
                title = subject
            }
            for provider in item.attachments ?? [] {
                // Files are claimed FIRST: a file conforms to public.url as well,
                // so the link branch below would otherwise turn a shared voice memo
                // into a file:// address the app is not allowed to read.
                if let staging, files.count < Self.maxFiles,
                   let staged = await stage(provider, into: staging, index: files.count) {
                    files.append(staged)
                    continue
                }
                if link == nil, provider.hasItemConformingToTypeIdentifier(UTType.url.identifier),
                   !provider.hasItemConformingToTypeIdentifier(UTType.fileURL.identifier) {
                    link = (await load(provider, UTType.url.identifier) as? URL)?.absoluteString
                }
                if text == nil, provider.hasItemConformingToTypeIdentifier(UTType.plainText.identifier) {
                    text = await load(provider, UTType.plainText.identifier) as? String
                }
            }
            // A web page share puts the selection here, not in an attachment.
            if text == nil, let selected = item.attributedContentText?.string, !selected.isEmpty {
                text = selected
            }
        }

        var query = [URLQueryItem]()
        if let link, !link.isEmpty { query.append(URLQueryItem(name: "url", value: link)) }
        if let text, !text.isEmpty, text != link { query.append(URLQueryItem(name: "text", value: text)) }
        if let title, !title.isEmpty, title != link { query.append(URLQueryItem(name: "title", value: title)) }

        // file/name/type are INDEX ALIGNED — the app pairs them by position, so a
        // type it could not determine is still sent, as an empty string.
        for file in files {
            query.append(URLQueryItem(name: "file", value: file.url.absoluteString))
            query.append(URLQueryItem(name: "name", value: file.name))
            query.append(URLQueryItem(name: "type", value: file.type))
        }

        // A nonce: sharing the SAME page twice must produce two DIFFERENT URLs,
        // or the app's deep-link dedupe (src/lib/deeplink.ts) swallows the second.
        if (!query.isEmpty) {
            query.append(URLQueryItem(name: "at", value: String(Int(Date().timeIntervalSince1970 * 1000))))
        }

        var components = URLComponents()
        components.scheme = "vis"
        components.host = "share"
        components.queryItems = query.isEmpty ? nil : query

        if let url = components.url, !query.isEmpty {
            await open(url)
        }
        extensionContext?.completeRequest(returningItems: nil)
    }

    /// This share's own folder inside the App Group container. Nil when the group
    /// is unavailable — a build signed without the capability still hands over its
    /// link and its text, and simply carries no files.
    private func stagingDirectory() -> URL? {
        let manager = FileManager.default
        guard let container = manager.containerURL(forSecurityApplicationGroupIdentifier: Self.appGroup) else {
            return nil
        }
        let root = container.appendingPathComponent("Library/Caches/VisShare", isDirectory: true)
        purge(root)
        let directory = root.appendingPathComponent(UUID().uuidString, isDirectory: true)
        do {
            try manager.createDirectory(at: directory, withIntermediateDirectories: true)
        } catch {
            return nil
        }
        return directory
    }

    /// A share the user cancelled leaves its copy behind and nobody else will ever
    /// delete it, so the container is swept before staging the next one.
    private func purge(_ root: URL) {
        let manager = FileManager.default
        let cutoff = Date().addingTimeInterval(-Self.staleAfter)
        let keys: [URLResourceKey] = [.contentModificationDateKey]
        let entries = (try? manager.contentsOfDirectory(at: root, includingPropertiesForKeys: keys)) ?? []
        for entry in entries {
            let modified = (try? entry.resourceValues(forKeys: Set(keys)))?.contentModificationDate
            if let modified, modified > cutoff { continue }
            try? manager.removeItem(at: entry)
        }
    }

    /// The type to copy the attachment AS. Media first — a photo registers both
    /// public.jpeg and public.file-url — then any concrete file that is neither
    /// text nor a link, because those two belong in the message, not on a chip.
    private func stageableType(_ provider: NSItemProvider) -> UTType? {
        let media: [UTType] = [.image, .movie, .audio, .pdf]
        let types = provider.registeredTypeIdentifiers.compactMap { UTType($0) }
        if let match = types.first(where: { candidate in media.contains { candidate.conforms(to: $0) } }) {
            return match
        }
        return types.first { $0.conforms(to: .data) && !$0.conforms(to: .text) && !$0.conforms(to: .url) }
    }

    private func stage(_ provider: NSItemProvider, into directory: URL, index: Int) async -> StagedFile? {
        guard let type = stageableType(provider) else { return nil }
        let folder = directory.appendingPathComponent(String(index), isDirectory: true)
        let name = fileName(provider, type)
        return await withCheckedContinuation { (continuation: CheckedContinuation<StagedFile?, Never>) in
            // The URL is valid only INSIDE this block, so the copy happens here —
            // after the continuation resumes the temporary file is already gone.
            _ = provider.loadFileRepresentation(forTypeIdentifier: type.identifier) { url, _ in
                guard let url else {
                    continuation.resume(returning: nil)
                    return
                }
                let manager = FileManager.default
                let size = (try? url.resourceValues(forKeys: [.fileSizeKey]))?.fileSize ?? 0
                guard size <= Self.maxFileBytes else {
                    continuation.resume(returning: nil)
                    return
                }
                do {
                    try manager.createDirectory(at: folder, withIntermediateDirectories: true)
                    let destination = folder.appendingPathComponent(name)
                    try? manager.removeItem(at: destination)
                    try manager.copyItem(at: url, to: destination)
                    let staged = StagedFile(url: destination, name: name, type: type.preferredMIMEType ?? "")
                    continuation.resume(returning: staged)
                } catch {
                    continuation.resume(returning: nil)
                }
            }
        }
    }

    /// The name the human recognises on the chip: what the sharing app called the
    /// file, with the extension restored from the type — a photo often arrives as
    /// "IMG_0001" with none, and the app types an attachment by its extension when
    /// the platform reports no media type.
    private func fileName(_ provider: NSItemProvider, _ type: UTType) -> String {
        let suggested = provider.suggestedName ?? ""
        let base = suggested.isEmpty ? "shared" : (suggested as NSString).lastPathComponent
        let ext = type.preferredFilenameExtension ?? ""
        if ext.isEmpty || (base as NSString).pathExtension.lowercased() == ext.lowercased() { return base }
        return (base as NSString).appendingPathExtension(ext) ?? base
    }

    private func load(_ provider: NSItemProvider, _ type: String) async -> Any? {
        await withCheckedContinuation { continuation in
            provider.loadItem(forTypeIdentifier: type, options: nil) { value, _ in
                continuation.resume(returning: value)
            }
        }
    }

    @MainActor
    private func open(_ url: URL) async {
        let opened = await withCheckedContinuation { (continuation: CheckedContinuation<Bool, Never>) in
            guard let context = extensionContext else {
                continuation.resume(returning: false)
                return
            }
            context.open(url) { success in continuation.resume(returning: success) }
        }
        guard !opened else { return }
        openViaResponderChain(url)
    }

    /// Reaches UIApplication through the responder chain. openURL: is dead since
    /// iOS 18 (UIKit force-returns NO) and UIScene answers the modern selector
    /// earlier in the chain but aborts the extension, so this matches the
    /// application class exactly and calls the modern selector through its IMP.
    @MainActor
    private func openViaResponderChain(_ url: URL) {
        let modern = NSSelectorFromString("openURL:options:completionHandler:")
        guard let application = NSClassFromString("UIApplication") else { return }
        var responder: UIResponder? = self
        while let current = responder {
            if current.isKind(of: application),
               let method = class_getInstanceMethod(type(of: current), modern) {
                typealias Open = @convention(c) (AnyObject, Selector, NSURL, NSDictionary, AnyObject?) -> Void
                let call = unsafeBitCast(method_getImplementation(method), to: Open.self)
                call(current, modern, url as NSURL, NSDictionary(), nil)
                return
            }
            responder = current.next
        }
    }
}
`;

const sharePlistSource = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>CFBundleDevelopmentRegion</key>
\t<string>$(DEVELOPMENT_LANGUAGE)</string>
\t<key>CFBundleDisplayName</key>
\t<string>Vis</string>
\t<key>CFBundleExecutable</key>
\t<string>$(EXECUTABLE_NAME)</string>
\t<key>CFBundleIdentifier</key>
\t<string>$(PRODUCT_BUNDLE_IDENTIFIER)</string>
\t<key>CFBundleInfoDictionaryVersion</key>
\t<string>6.0</string>
\t<key>CFBundleName</key>
\t<string>$(PRODUCT_NAME)</string>
\t<key>CFBundlePackageType</key>
\t<string>XPC!</string>
\t<key>CFBundleShortVersionString</key>
\t<string>$(MARKETING_VERSION)</string>
\t<key>CFBundleVersion</key>
\t<string>$(CURRENT_PROJECT_VERSION)</string>
\t<key>NSExtension</key>
\t<dict>
\t\t<key>NSExtensionAttributes</key>
\t\t<dict>
\t\t\t<key>NSExtensionActivationRule</key>
\t\t\t<dict>
\t\t\t\t<key>NSExtensionActivationSupportsWebURLWithMaxCount</key>
\t\t\t\t<integer>1</integer>
\t\t\t\t<key>NSExtensionActivationSupportsWebPageWithMaxCount</key>
\t\t\t\t<integer>1</integer>
\t\t\t\t<key>NSExtensionActivationSupportsText</key>
\t\t\t\t<true/>
\t\t\t\t<key>NSExtensionActivationSupportsFileWithMaxCount</key>
\t\t\t\t<integer>8</integer>
\t\t\t\t<key>NSExtensionActivationSupportsImageWithMaxCount</key>
\t\t\t\t<integer>8</integer>
\t\t\t\t<key>NSExtensionActivationSupportsMovieWithMaxCount</key>
\t\t\t\t<integer>8</integer>
\t\t\t</dict>
\t\t</dict>
\t\t<key>NSExtensionPointIdentifier</key>
\t\t<string>com.apple.share-services</string>
\t\t<key>NSExtensionPrincipalClass</key>
\t\t<string>$(PRODUCT_MODULE_NAME).ShareViewController</string>
\t</dict>
</dict>
</plist>
`;

// The App Group is a CAPABILITY: BOTH signatures have to carry it, or
// `containerURL(forSecurityApplicationGroupIdentifier:)` answers nil and a shared
// file has nowhere to land. The app's own file also carries `aps-environment`,
// whose value only the release script knows (development vs production), so this
// writes it only when it is ABSENT — scripts/ios-release.mjs owns it after that.
const appGroupEntitlements = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>com.apple.security.application-groups</key>
\t<array>
\t\t<string>${appGroup}</string>
\t</array>
</dict>
</plist>
`;

// App Intents live in the APP target, so `perform()` runs in the app process
// with the app already foregrounded (`openAppWhenRun`). Opening our own scheme
// from there is the same doorway the share extension and a pasted link use.
const shortcutsSource = `import AppIntents
import UIKit

@available(iOS 16.0, *)
struct SendToVisIntent: AppIntent {
    static var title: LocalizedStringResource = "Send to Vis"
    static var description = IntentDescription("Drop a link or a note into the Vis composer.")
    // The payload has to reach the running app: the composer is in the webview.
    static var openAppWhenRun = true

    @Parameter(title: "Text or link", requestValueDialog: "What should Vis get?")
    var content: String

    @Parameter(title: "Title")
    var noteTitle: String?

    @MainActor
    func perform() async throws -> some IntentResult {
        let trimmed = content.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return .result() }

        var query = [URLQueryItem]()
        let isLink = !trimmed.contains(where: \\.isWhitespace)
            && (trimmed.hasPrefix("http://") || trimmed.hasPrefix("https://"))
        query.append(URLQueryItem(name: isLink ? "url" : "text", value: trimmed))
        if let noteTitle, !noteTitle.isEmpty {
            query.append(URLQueryItem(name: "title", value: noteTitle))
        }

        // A nonce: running the shortcut twice on the same link must produce two
        // DIFFERENT URLs, or the deep-link dedupe (src/lib/deeplink.ts) drops one.
        query.append(URLQueryItem(name: "at", value: String(Int(Date().timeIntervalSince1970 * 1000))))

        var components = URLComponents()
        components.scheme = "vis"
        components.host = "share"
        components.queryItems = query
        if let url = components.url {
            _ = await UIApplication.shared.open(url)
        }
        return .result()
    }
}

@available(iOS 16.0, *)
struct VisShortcuts: AppShortcutsProvider {
    static var appShortcuts: [AppShortcut] {
        AppShortcut(
            intent: SendToVisIntent(),
            phrases: [
                "Send to \\(.applicationName)",
                "Add to \\(.applicationName)",
                "Send this to \\(.applicationName)",
            ],
            shortTitle: "Send to Vis",
            systemImageName: "paperplane"
        )
    }
}
`;

// ── 5. the icon badge ────────────────────────────────────────────────────────
// No gateway can count it: `aps.badge` is ABSOLUTE, and this device is paired
// with several machines that each know only their own sessions, so the last
// push to land would overwrite everyone else's number. Two native pieces that
// can see the whole device own it instead — a notification service extension
// that runs inside every arriving alert (`mutable-content` in the payload asks
// for it), and one plugin verb the app calls whenever it knows better.
const notifyDir = join(root, 'ios', 'App', 'VisNotify');
const notifyService = join(notifyDir, 'NotificationService.swift');
const notifyPlist = join(notifyDir, 'Info.plist');
const badgeSwift = join(appDir, 'VisBadge.swift');
const capConfig = join(appDir, 'capacitor.config.json');

const notifyServiceSource = `import UserNotifications

/// The badge, decided on the device.
///
/// APNs never counts anything, so this extension does: it runs inside every
/// alert Vis delivers, counts the alerts still waiting in Notification Center
/// — that is exactly the answers the reader has not dealt with — and adds the
/// one being delivered now, which is not in there yet.
///
/// Nothing else about the alert is touched. The app writes the same number
/// from the other side while it is running (src/lib/badge.ts), and keeps the
/// tray honest by dropping the alerts of sessions that have since been read.
final class NotificationService: UNNotificationServiceExtension {
  private var handler: ((UNNotificationContent) -> Void)?
  private var pending: UNMutableNotificationContent?

  override func didReceive(
    _ request: UNNotificationRequest,
    withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void
  ) {
    handler = contentHandler
    guard let content = request.content.mutableCopy() as? UNMutableNotificationContent else {
      contentHandler(request.content)
      return
    }
    pending = content
    UNUserNotificationCenter.current().getDeliveredNotifications { delivered in
      content.badge = NSNumber(value: delivered.count + 1)
      contentHandler(content)
    }
  }

  /// iOS gives an extension seconds, then delivers whatever it holds. Handing
  /// back the copy being mutated keeps the alert; only the count is lost.
  override func serviceExtensionTimeWillExpire() {
    guard let handler = handler, let pending = pending else { return }
    handler(pending)
  }
}
`;

const notifyPlistSource = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>CFBundleDevelopmentRegion</key>
\t<string>$(DEVELOPMENT_LANGUAGE)</string>
\t<key>CFBundleDisplayName</key>
\t<string>Vis</string>
\t<key>CFBundleExecutable</key>
\t<string>$(EXECUTABLE_NAME)</string>
\t<key>CFBundleIdentifier</key>
\t<string>$(PRODUCT_BUNDLE_IDENTIFIER)</string>
\t<key>CFBundleInfoDictionaryVersion</key>
\t<string>6.0</string>
\t<key>CFBundleName</key>
\t<string>$(PRODUCT_NAME)</string>
\t<key>CFBundlePackageType</key>
\t<string>XPC!</string>
\t<key>CFBundleShortVersionString</key>
\t<string>$(MARKETING_VERSION)</string>
\t<key>CFBundleVersion</key>
\t<string>$(CURRENT_PROJECT_VERSION)</string>
\t<key>NSExtension</key>
\t<dict>
\t\t<key>NSExtensionPointIdentifier</key>
\t\t<string>com.apple.usernotifications.service</string>
\t\t<key>NSExtensionPrincipalClass</key>
\t\t<string>$(PRODUCT_MODULE_NAME).NotificationService</string>
\t</dict>
</dict>
</plist>
`;

// The app's half. `UNUserNotificationCenter.setBadgeCount` is the only way to
// write the icon badge and no Capacitor plugin this app ships exposes it, so
// this is the whole native surface: one verb, no state. Registered by name in
// `capacitor.config.json`'s `packageClassList`, which is how Capacitor finds a
// plugin that lives in the app target rather than in a package.
const badgeSource = `import Capacitor
import UIKit
import UserNotifications

@objc(VisBadgePlugin)
public class VisBadgePlugin: CAPPlugin, CAPBridgedPlugin {
  public let identifier = "VisBadgePlugin"
  public let jsName = "VisBadge"
  public let pluginMethods: [CAPPluginMethod] = [
    CAPPluginMethod(name: "set", returnType: CAPPluginReturnPromise)
  ]

  @objc public func set(_ call: CAPPluginCall) {
    let count = max(0, call.getInt("count") ?? 0)
    if #available(iOS 16.0, *) {
      UNUserNotificationCenter.current().setBadgeCount(count) { _ in call.resolve() }
    } else {
      DispatchQueue.main.async {
        UIApplication.shared.applicationIconBadgeNumber = count
        call.resolve()
      }
    }
  }
}
`;

const fileOk = (path, contents) => existsSync(path) && readFileSync(path, 'utf8') === contents;
const shareFilesOk =
  fileOk(shareController, shareControllerSource)
  && fileOk(sharePlist, sharePlistSource)
  && fileOk(shareEntitlements, appGroupEntitlements)
  && fileOk(shortcutsSwift, shortcutsSource);
const notifyFilesOk =
  fileOk(notifyService, notifyServiceSource)
  && fileOk(notifyPlist, notifyPlistSource)
  && fileOk(badgeSwift, badgeSource);

// `cap sync` rewrites this file from the INSTALLED packages, so a plugin class
// that lives in the app target is dropped from it every time. Putting it back
// is exactly what this hook is for — it runs as `postsync`.
const capConfigJson = existsSync(capConfig) ? JSON.parse(readFileSync(capConfig, 'utf8')) : null;
const capConfigOk = !capConfigJson || (capConfigJson.packageClassList ?? []).includes('VisBadgePlugin');

let project = existsSync(pbxprojPath) ? readFileSync(pbxprojPath, 'utf8') : '';
if (!project) die('no ios/App/App.xcodeproj/project.pbxproj — run `npm run add:ios` first');
const projectBefore = project;

// Stable, obviously-ours object ids: a regenerated project never mints these, so
// re-running is a no-op instead of a second copy of the target.
const objectId = (n) => `5A11E5${n.toString(16).toUpperCase().padStart(18, '0')}`;
const ids = {
  extSwiftRef: objectId(1),
  extPlistRef: objectId(2),
  appexRef: objectId(3),
  group: objectId(4),
  sources: objectId(5),
  frameworks: objectId(6),
  resources: objectId(7),
  target: objectId(8),
  configList: objectId(9),
  debug: objectId(10),
  release: objectId(11),
  extSwiftBuild: objectId(12),
  embedPhase: objectId(13),
  embedBuild: objectId(14),
  dependency: objectId(15),
  proxy: objectId(16),
  shortcutsRef: objectId(17),
  shortcutsBuild: objectId(18),
};

const notifyIds = {
  swiftRef: objectId(19),
  plistRef: objectId(20),
  appexRef: objectId(21),
  group: objectId(22),
  sources: objectId(23),
  frameworks: objectId(24),
  resources: objectId(25),
  target: objectId(26),
  configList: objectId(27),
  debug: objectId(28),
  release: objectId(29),
  swiftBuild: objectId(30),
  embedBuild: objectId(31),
  dependency: objectId(32),
  proxy: objectId(33),
  badgeRef: objectId(34),
  badgeBuild: objectId(35),
};

const projectOk = project.includes(ids.target) && project.includes(ids.shortcutsRef);
const notifyProjectOk = project.includes(notifyIds.target) && project.includes(notifyIds.badgeRef);

const after = (pattern, addition, what) => {
  const match = pattern.exec(project);
  if (!match) die(`project.pbxproj: no ${what} to stamp an extension onto`);
  const at = match.index + match[0].length;
  project = project.slice(0, at) + addition + project.slice(at);
};
const before = (marker, addition) => {
  const at = project.indexOf(marker);
  if (at < 0) die(`project.pbxproj has no ${marker}`);
  project = project.slice(0, at) + addition + project.slice(at);
};

const projectObject = /([0-9A-Fa-f]{24}) \/\* Project object \*\//.exec(project)?.[1];
if (!projectObject) die('project.pbxproj has no Project object');
// An extension ships inside the app, so it carries the app's version pair —
// scripts/ios-release.mjs rewrites every occurrence of both before an archive.
const marketing = /MARKETING_VERSION = ([^;]+);/.exec(project)?.[1] ?? '1.0';
const buildVersion = /CURRENT_PROJECT_VERSION = ([^;]+);/.exec(project)?.[1] ?? '1';

const shareTarget = { name: 'VisShare', bundleSuffix: 'share', entitlements: 'VisShare/VisShare.entitlements' };
const notifyTarget = { name: 'VisNotify', bundleSuffix: 'notify' };

// The share extension DOES need an entitlement of its own — the App Group that
// carries a shared file to the app. The notification service does not: the host's
// aps-environment on an app extension is rejected outright, and that extension is
// entitled by the notification it is handed, not by push.
const extensionSettings = ({ name, bundleSuffix, entitlements }, configId, configName, extra) => `\t\t${configId} /* ${configName} */ = {
\t\t\tisa = XCBuildConfiguration;
\t\t\tbuildSettings = {
${entitlements ? `\t\t\t\tCODE_SIGN_ENTITLEMENTS = ${entitlements};\n` : ''}\t\t\t\tCODE_SIGN_STYLE = Automatic;
\t\t\t\tCURRENT_PROJECT_VERSION = ${buildVersion};
\t\t\t\tGENERATE_INFOPLIST_FILE = NO;
\t\t\t\tINFOPLIST_FILE = ${name}/Info.plist;
\t\t\t\tIPHONEOS_DEPLOYMENT_TARGET = 15.0;
\t\t\t\tLD_RUNPATH_SEARCH_PATHS = (
\t\t\t\t\t"$(inherited)",
\t\t\t\t\t"@executable_path/Frameworks",
\t\t\t\t\t"@executable_path/../../Frameworks",
\t\t\t\t);
\t\t\t\tMARKETING_VERSION = ${marketing};
\t\t\t\tPRODUCT_BUNDLE_IDENTIFIER = ${bundleId}.${bundleSuffix};
\t\t\t\tPRODUCT_NAME = "$(TARGET_NAME)";
\t\t\t\tSKIP_INSTALL = YES;
${extra}\t\t\t\tSWIFT_VERSION = 5.0;
\t\t\t\tTARGETED_DEVICE_FAMILY = "1,2";
\t\t\t};
\t\t\tname = ${configName};
\t\t};
`;

if (!projectOk) {

  // App target first: the additions below carry the same shapes (an empty
  // `dependencies`, a `Resources` phase) and would otherwise be matched instead.
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* AppDelegate\.swift in Sources \*\/,/,
    `\n\t\t\t\t${ids.shortcutsBuild} /* VisShortcuts.swift in Sources */,`,
    'App Sources phase',
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* AppDelegate\.swift \*\/,/,
    `\n\t\t\t\t${ids.shortcutsRef} /* VisShortcuts.swift */,`,
    'App group',
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* Resources \*\/,/,
    `\n\t\t\t\t${ids.embedPhase} /* Embed Foundation Extensions */,`,
    'App buildPhases',
  );
  project = project.replace(
    /\n(\s*)dependencies = \(\n\s*\);/,
    `\n$1dependencies = (\n$1\t${ids.dependency} /* PBXTargetDependency */,\n$1);`,
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* App\.app \*\/,/,
    `\n\t\t\t\t${ids.appexRef} /* VisShare.appex */,`,
    'Products group',
  );
  after(/\n(\s*)targets = \(/, `\n\t\t\t\t${ids.target} /* VisShare */,`, 'targets list');
  after(/TargetAttributes = \{/, `\n\t\t\t\t\t${ids.target} = {\n\t\t\t\t\t\tProvisioningStyle = Automatic;\n\t\t\t\t\t};`, 'TargetAttributes');
  // The group that holds the extension's own sources.
  before(
    '/* End PBXGroup section */',
    `\t\t${ids.group} /* VisShare */ = {
\t\t\tisa = PBXGroup;
\t\t\tchildren = (
\t\t\t\t${ids.extSwiftRef} /* ShareViewController.swift */,
\t\t\t\t${ids.extPlistRef} /* Info.plist */,
\t\t\t);
\t\t\tpath = VisShare;
\t\t\tsourceTree = "<group>";
\t\t};
`,
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* Products \*\/,/,
    `\n\t\t\t\t${ids.group} /* VisShare */,`,
    'main group',
  );

  before(
    '/* End PBXBuildFile section */',
    `\t\t${ids.extSwiftBuild} /* ShareViewController.swift in Sources */ = {isa = PBXBuildFile; fileRef = ${ids.extSwiftRef} /* ShareViewController.swift */; };
\t\t${ids.embedBuild} /* VisShare.appex in Embed Foundation Extensions */ = {isa = PBXBuildFile; fileRef = ${ids.appexRef} /* VisShare.appex */; settings = {ATTRIBUTES = (RemoveHeadersOnCopy, ); }; };
\t\t${ids.shortcutsBuild} /* VisShortcuts.swift in Sources */ = {isa = PBXBuildFile; fileRef = ${ids.shortcutsRef} /* VisShortcuts.swift */; };
`,
  );
  before(
    '/* End PBXFileReference section */',
    `\t\t${ids.appexRef} /* VisShare.appex */ = {isa = PBXFileReference; explicitFileType = "wrapper.app-extension"; includeInIndex = 0; path = VisShare.appex; sourceTree = BUILT_PRODUCTS_DIR; };
\t\t${ids.extSwiftRef} /* ShareViewController.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = ShareViewController.swift; sourceTree = "<group>"; };
\t\t${ids.extPlistRef} /* Info.plist */ = {isa = PBXFileReference; lastKnownFileType = text.plist.xml; path = Info.plist; sourceTree = "<group>"; };
\t\t${ids.shortcutsRef} /* VisShortcuts.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = VisShortcuts.swift; sourceTree = "<group>"; };
`,
  );
  before(
    '/* End PBXFrameworksBuildPhase section */',
    `\t\t${ids.frameworks} /* Frameworks */ = {
\t\t\tisa = PBXFrameworksBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* Begin PBXFileReference section */',
    `/* Begin PBXCopyFilesBuildPhase section */
\t\t${ids.embedPhase} /* Embed Foundation Extensions */ = {
\t\t\tisa = PBXCopyFilesBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tdstPath = "";
\t\t\tdstSubfolderSpec = 13;
\t\t\tfiles = (
\t\t\t\t${ids.embedBuild} /* VisShare.appex in Embed Foundation Extensions */,
\t\t\t);
\t\t\tname = "Embed Foundation Extensions";
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
/* End PBXCopyFilesBuildPhase section */

`,
  );
  before(
    '/* End PBXNativeTarget section */',
    `\t\t${ids.target} /* VisShare */ = {
\t\t\tisa = PBXNativeTarget;
\t\t\tbuildConfigurationList = ${ids.configList} /* Build configuration list for PBXNativeTarget "VisShare" */;
\t\t\tbuildPhases = (
\t\t\t\t${ids.sources} /* Sources */,
\t\t\t\t${ids.frameworks} /* Frameworks */,
\t\t\t\t${ids.resources} /* Resources */,
\t\t\t);
\t\t\tbuildRules = (
\t\t\t);
\t\t\tdependencies = (
\t\t\t);
\t\t\tname = VisShare;
\t\t\tproductName = VisShare;
\t\t\tproductReference = ${ids.appexRef} /* VisShare.appex */;
\t\t\tproductType = "com.apple.product-type.app-extension";
\t\t};
`,
  );
  before(
    '/* End PBXResourcesBuildPhase section */',
    `\t\t${ids.resources} /* Resources */ = {
\t\t\tisa = PBXResourcesBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* End PBXSourcesBuildPhase section */',
    `\t\t${ids.sources} /* Sources */ = {
\t\t\tisa = PBXSourcesBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t\t${ids.extSwiftBuild} /* ShareViewController.swift in Sources */,
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* Begin XCBuildConfiguration section */',
    `/* Begin PBXContainerItemProxy section */
\t\t${ids.proxy} /* PBXContainerItemProxy */ = {
\t\t\tisa = PBXContainerItemProxy;
\t\t\tcontainerPortal = ${projectObject} /* Project object */;
\t\t\tproxyType = 1;
\t\t\tremoteGlobalIDString = ${ids.target};
\t\t\tremoteInfo = VisShare;
\t\t};
/* End PBXContainerItemProxy section */

/* Begin PBXTargetDependency section */
\t\t${ids.dependency} /* PBXTargetDependency */ = {
\t\t\tisa = PBXTargetDependency;
\t\t\ttarget = ${ids.target} /* VisShare */;
\t\t\ttargetProxy = ${ids.proxy} /* PBXContainerItemProxy */;
\t\t};
/* End PBXTargetDependency section */

`,
  );
  before(
    '/* End XCBuildConfiguration section */',
    extensionSettings(shareTarget, ids.debug, 'Debug', '\t\t\t\tSWIFT_ACTIVE_COMPILATION_CONDITIONS = DEBUG;\n')
      + extensionSettings(shareTarget, ids.release, 'Release', '\t\t\t\tSWIFT_ACTIVE_COMPILATION_CONDITIONS = "";\n'),
  );
  before(
    '/* End XCConfigurationList section */',
    `\t\t${ids.configList} /* Build configuration list for PBXNativeTarget "VisShare" */ = {
\t\t\tisa = XCConfigurationList;
\t\t\tbuildConfigurations = (
\t\t\t\t${ids.debug} /* Debug */,
\t\t\t\t${ids.release} /* Release */,
\t\t\t);
\t\t\tdefaultConfigurationIsVisible = 0;
\t\t\tdefaultConfigurationName = Release;
\t\t};
`,
  );
}

// The same target once more, for notifications — with two differences that
// matter: the app's dependency list and the Embed Foundation Extensions phase
// already exist, so this one JOINS them instead of stamping a second copy, and
// the badge plugin is a plain Swift file compiled into the APP target.
if (!notifyProjectOk) {
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* AppDelegate\.swift in Sources \*\/,/,
    `\n\t\t\t\t${notifyIds.badgeBuild} /* VisBadge.swift in Sources */,`,
    'App Sources phase',
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* AppDelegate\.swift \*\/,/,
    `\n\t\t\t\t${notifyIds.badgeRef} /* VisBadge.swift */,`,
    'App group',
  );
  after(
    new RegExp(`\\n(\\s*)${ids.dependency} \\/\\* PBXTargetDependency \\*\\/,`),
    `\n\t\t\t\t${notifyIds.dependency} /* PBXTargetDependency */,`,
    'App dependencies list',
  );
  after(
    new RegExp(`\\n(\\s*)${ids.embedBuild} \\/\\* VisShare\\.appex in Embed Foundation Extensions \\*\\/,`),
    `\n\t\t\t\t${notifyIds.embedBuild} /* VisNotify.appex in Embed Foundation Extensions */,`,
    'Embed Foundation Extensions phase',
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* App\.app \*\/,/,
    `\n\t\t\t\t${notifyIds.appexRef} /* VisNotify.appex */,`,
    'Products group',
  );
  after(/\n(\s*)targets = \(/, `\n\t\t\t\t${notifyIds.target} /* VisNotify */,`, 'targets list');
  after(
    /TargetAttributes = \{/,
    `\n\t\t\t\t\t${notifyIds.target} = {\n\t\t\t\t\t\tProvisioningStyle = Automatic;\n\t\t\t\t\t};`,
    'TargetAttributes',
  );
  before(
    '/* End PBXGroup section */',
    `\t\t${notifyIds.group} /* VisNotify */ = {
\t\t\tisa = PBXGroup;
\t\t\tchildren = (
\t\t\t\t${notifyIds.swiftRef} /* NotificationService.swift */,
\t\t\t\t${notifyIds.plistRef} /* Info.plist */,
\t\t\t);
\t\t\tpath = VisNotify;
\t\t\tsourceTree = "<group>";
\t\t};
`,
  );
  after(
    /\n(\s*)[0-9A-Fa-f]{24} \/\* Products \*\/,/,
    `\n\t\t\t\t${notifyIds.group} /* VisNotify */,`,
    'main group',
  );
  before(
    '/* End PBXBuildFile section */',
    `\t\t${notifyIds.swiftBuild} /* NotificationService.swift in Sources */ = {isa = PBXBuildFile; fileRef = ${notifyIds.swiftRef} /* NotificationService.swift */; };
\t\t${notifyIds.embedBuild} /* VisNotify.appex in Embed Foundation Extensions */ = {isa = PBXBuildFile; fileRef = ${notifyIds.appexRef} /* VisNotify.appex */; settings = {ATTRIBUTES = (RemoveHeadersOnCopy, ); }; };
\t\t${notifyIds.badgeBuild} /* VisBadge.swift in Sources */ = {isa = PBXBuildFile; fileRef = ${notifyIds.badgeRef} /* VisBadge.swift */; };
`,
  );
  before(
    '/* End PBXFileReference section */',
    `\t\t${notifyIds.appexRef} /* VisNotify.appex */ = {isa = PBXFileReference; explicitFileType = "wrapper.app-extension"; includeInIndex = 0; path = VisNotify.appex; sourceTree = BUILT_PRODUCTS_DIR; };
\t\t${notifyIds.swiftRef} /* NotificationService.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = NotificationService.swift; sourceTree = "<group>"; };
\t\t${notifyIds.plistRef} /* Info.plist */ = {isa = PBXFileReference; lastKnownFileType = text.plist.xml; path = Info.plist; sourceTree = "<group>"; };
\t\t${notifyIds.badgeRef} /* VisBadge.swift */ = {isa = PBXFileReference; lastKnownFileType = sourcecode.swift; path = VisBadge.swift; sourceTree = "<group>"; };
`,
  );
  before(
    '/* End PBXFrameworksBuildPhase section */',
    `\t\t${notifyIds.frameworks} /* Frameworks */ = {
\t\t\tisa = PBXFrameworksBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* End PBXNativeTarget section */',
    `\t\t${notifyIds.target} /* VisNotify */ = {
\t\t\tisa = PBXNativeTarget;
\t\t\tbuildConfigurationList = ${notifyIds.configList} /* Build configuration list for PBXNativeTarget "VisNotify" */;
\t\t\tbuildPhases = (
\t\t\t\t${notifyIds.sources} /* Sources */,
\t\t\t\t${notifyIds.frameworks} /* Frameworks */,
\t\t\t\t${notifyIds.resources} /* Resources */,
\t\t\t);
\t\t\tbuildRules = (
\t\t\t);
\t\t\tdependencies = (
\t\t\t);
\t\t\tname = VisNotify;
\t\t\tproductName = VisNotify;
\t\t\tproductReference = ${notifyIds.appexRef} /* VisNotify.appex */;
\t\t\tproductType = "com.apple.product-type.app-extension";
\t\t};
`,
  );
  before(
    '/* End PBXResourcesBuildPhase section */',
    `\t\t${notifyIds.resources} /* Resources */ = {
\t\t\tisa = PBXResourcesBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* End PBXSourcesBuildPhase section */',
    `\t\t${notifyIds.sources} /* Sources */ = {
\t\t\tisa = PBXSourcesBuildPhase;
\t\t\tbuildActionMask = 2147483647;
\t\t\tfiles = (
\t\t\t\t${notifyIds.swiftBuild} /* NotificationService.swift in Sources */,
\t\t\t);
\t\t\trunOnlyForDeploymentPostprocessing = 0;
\t\t};
`,
  );
  before(
    '/* End PBXContainerItemProxy section */',
    `\t\t${notifyIds.proxy} /* PBXContainerItemProxy */ = {
\t\t\tisa = PBXContainerItemProxy;
\t\t\tcontainerPortal = ${projectObject} /* Project object */;
\t\t\tproxyType = 1;
\t\t\tremoteGlobalIDString = ${notifyIds.target};
\t\t\tremoteInfo = VisNotify;
\t\t};
`,
  );
  before(
    '/* End PBXTargetDependency section */',
    `\t\t${notifyIds.dependency} /* PBXTargetDependency */ = {
\t\t\tisa = PBXTargetDependency;
\t\t\ttarget = ${notifyIds.target} /* VisNotify */;
\t\t\ttargetProxy = ${notifyIds.proxy} /* PBXContainerItemProxy */;
\t\t};
`,
  );
  before(
    '/* End XCBuildConfiguration section */',
    extensionSettings(notifyTarget, notifyIds.debug, 'Debug', '\t\t\t\tSWIFT_ACTIVE_COMPILATION_CONDITIONS = DEBUG;\n')
      + extensionSettings(notifyTarget, notifyIds.release, 'Release', '\t\t\t\tSWIFT_ACTIVE_COMPILATION_CONDITIONS = "";\n'),
  );
  before(
    '/* End XCConfigurationList section */',
    `\t\t${notifyIds.configList} /* Build configuration list for PBXNativeTarget "VisNotify" */ = {
\t\t\tisa = XCConfigurationList;
\t\t\tbuildConfigurations = (
\t\t\t\t${notifyIds.debug} /* Debug */,
\t\t\t\t${notifyIds.release} /* Release */,
\t\t\t);
\t\t\tdefaultConfigurationIsVisible = 0;
\t\t\tdefaultConfigurationName = Release;
\t\t};
`,
  );
}

const shareOk = shareFilesOk && projectOk;
const badgeOk = notifyFilesOk && notifyProjectOk && capConfigOk;

if (check) {
  if (delegateOk && boardOk && plistOk && appIconOk && shareOk && badgeOk && splashOk) {
    console.log('· ios: prepared stock Capacitor host with required app capabilities, branded icon and launch screen, share extension, Shortcuts and the badge extension');
    process.exit(0);
  }
  const missing = missingPlistEntries.map(([key]) => key).join(', ');
  die(
    !appIconOk
      ? 'ios: generated AppIcon is not the tracked Vis icon — run `node scripts/ios-prepare.mjs`'
      : !delegateOk
        ? 'ios: AppDelegate needs native hygiene — run `node scripts/ios-prepare.mjs`'
        : !boardOk
          ? 'ios: stale viewport bridge — run `node scripts/ios-prepare.mjs`'
          : !shareOk
            ? 'ios: no share extension / Shortcuts target — run `node scripts/ios-prepare.mjs`'
            : !badgeOk
              ? 'ios: no VisNotify badge extension / VisBadge plugin — run `node scripts/ios-prepare.mjs`'
              : !splashOk
                ? 'ios: launch screen still shows Capacitor\'s splash — run `node scripts/ios-prepare.mjs`'
              : `ios: Info.plist is missing ${missing} — run \`node scripts/ios-prepare.mjs\``,
  );
}

if (!appIconOk) copyFileSync(appIconSource, appIconTarget);
if (!splashFilesOk) {
  mkdirSync(splashDir, { recursive: true });
  for (const name of splashStale) rmSync(join(splashDir, name));
  for (const [source, target] of splashImages) {
    copyFileSync(join(root, 'native-assets', 'ios', source), join(splashDir, target));
  }
  writeFileSync(join(splashDir, 'Contents.json'), splashContents);
}
if (!splashColorOk) {
  mkdirSync(splashColorDir, { recursive: true });
  writeFileSync(join(splashColorDir, 'Contents.json'), splashColorContents);
}
if (!launchBoardOk) writeFileSync(launchBoard, launchBoardSource);

if (!delegateOk) writeFileSync(delegate, preparedDelegate);
if (!boardOk) writeFileSync(storyboard, cleanedBoard);
if (!plistOk) writeFileSync(infoPlist, preparedPlist);

if (!shareFilesOk) {
  mkdirSync(shareDir, { recursive: true });
  writeFileSync(shareController, shareControllerSource);
  writeFileSync(sharePlist, sharePlistSource);
  writeFileSync(shareEntitlements, appGroupEntitlements);
  writeFileSync(shortcutsSwift, shortcutsSource);
}
// The app's file may already carry aps-environment from scripts/ios-release.mjs —
// the group is ADDED to it, never written over it.
if (!existsSync(appEntitlements) || !readFileSync(appEntitlements, 'utf8').includes(appGroup)) {
  const existing = existsSync(appEntitlements) ? readFileSync(appEntitlements, 'utf8') : '';
  const group = `\t<key>com.apple.security.application-groups</key>\n\t<array>\n\t\t<string>${appGroup}</string>\n\t</array>\n</dict>`;
  writeFileSync(appEntitlements, existing.includes('</dict>') ? existing.replace('</dict>', group) : appGroupEntitlements);
}
if (!notifyFilesOk) {
  mkdirSync(notifyDir, { recursive: true });
  writeFileSync(notifyService, notifyServiceSource);
  writeFileSync(notifyPlist, notifyPlistSource);
  writeFileSync(badgeSwift, badgeSource);
}
if (!capConfigOk && capConfigJson) {
  capConfigJson.packageClassList = [...(capConfigJson.packageClassList ?? []), 'VisBadgePlugin'];
  writeFileSync(capConfig, `${JSON.stringify(capConfigJson, null, '\t')}\n`);
}
// The app has to be entitled to the SAME group as the extension, or it cannot read
// the file it was just handed. scripts/ios-release.mjs points the app target at
// this very file for push, and looks for exactly this line before adding it.
if (!project.includes('CODE_SIGN_ENTITLEMENTS = App/App.entitlements')) {
  const idPattern = bundleId.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  project = project.replaceAll(
    new RegExp(`(\\n(\\s*)PRODUCT_BUNDLE_IDENTIFIER = "?${idPattern}"?;)`, 'g'),
    '$1\n$2CODE_SIGN_ENTITLEMENTS = App/App.entitlements;',
  );
}
// An `ios/` stamped by an EARLIER version of this script already carries the
// extension target, so the block above is skipped — the group entitlement is
// still put on it here, where it is missing.
if (!project.includes(`CODE_SIGN_ENTITLEMENTS = ${shareTarget.entitlements}`)) {
  const sharePattern = `${bundleId}.${shareTarget.bundleSuffix}`.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  project = project.replaceAll(
    new RegExp(`(\\n(\\s*)PRODUCT_BUNDLE_IDENTIFIER = "?${sharePattern}"?;)`, 'g'),
    `$1\n$2CODE_SIGN_ENTITLEMENTS = ${shareTarget.entitlements};`,
  );
}
if (project !== projectBefore) writeFileSync(pbxprojPath, project);

console.log(
  `· ios: ${delegateOk ? 'AppDelegate already prepared' : 'prepared AppDelegate'}; ${
    boardOk ? 'stock Capacitor bridge' : 'removed the viewport bridge'
  }; ${plistOk ? 'app capabilities already present' : `stamped ${missingPlistEntries.map(([key]) => key).join(', ')}`
  }; ${appIconOk ? 'branded icon already present' : 'stamped branded app icon'}; ${
    shareOk ? 'share extension + Shortcuts already present' : 'stamped VisShare extension + App Intents'
  }; ${badgeOk ? 'badge extension already present' : 'stamped VisNotify extension + VisBadge plugin'}; ${
    splashOk ? 'branded launch screen already present' : 'stamped the Vis launch screen'
  }`,
);
