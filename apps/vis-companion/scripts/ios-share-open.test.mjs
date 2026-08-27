import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// `ios-prepare.mjs` stamps the generated Xcode project the moment it is
// imported, so the Swift it embeds is read as text instead.
const prepare = readFileSync(join(dirname(fileURLToPath(import.meta.url)), 'ios-prepare.mjs'), 'utf8');
const shareController = prepare.match(/const shareControllerSource = `([\s\S]*?)\n`;/)?.[1] ?? '';
const sharePlist = prepare.match(/const sharePlistSource = `([\s\S]*?)\n`;/)?.[1] ?? '';
const entitlements = prepare.match(/const appGroupEntitlements = `([\s\S]*?)\n`;/)?.[1] ?? '';
const release = readFileSync(join(dirname(fileURLToPath(import.meta.url)), 'ios-release.mjs'), 'utf8');

describe('VisShare share extension', () => {
  it('embeds a share controller', () => {
    expect(shareController).toContain('final class ShareViewController: UIViewController');
  });

  // The shipped bug: sharing a Safari link showed a white sheet and then
  // nothing. Verified in the iOS 26 Simulator: `extensionContext.open` answers
  // false for a share extension, and the legacy `openURL:` fallback is dead —
  // UIKit force-returns NO and logs "BUG IN CLIENT OF UIKIT", so the app was
  // never launched. Only `openURL:options:completionHandler:` still opens it.
  it('opens the app with the modern selector, never the dead openURL:', () => {
    expect(shareController).toContain('NSSelectorFromString("openURL:options:completionHandler:")');
    expect(shareController).not.toContain('NSSelectorFromString("openURL:")');
    expect(shareController).not.toContain('.perform(');
  });

  // `UIScene` sits earlier in the responder chain and answers the same selector,
  // but calling it aborts the extension, so the walk must match the application
  // class exactly rather than asking who responds.
  it('calls UIApplication only, through its IMP', () => {
    expect(shareController).toContain('NSClassFromString("UIApplication")');
    expect(shareController).toContain('isKind(of: application)');
    expect(shareController).toContain('class_getInstanceMethod');
    expect(shareController).toContain('import ObjectiveC');
    expect(shareController).not.toContain('responds(to:');
  });

  // Sharing the same page twice must produce two different URLs or the app's
  // deep-link dedupe swallows the second share.
  it('keeps the documented call first and the nonce on the URL', () => {
    expect(shareController).toContain('context.open(url)');
    expect(shareController).toContain('URLQueryItem(name: "at"');
  });
  // A shared FILE conforms to public.url as well, so the link branch used to win:
  // a voice memo arrived as a file:// address in another process's container,
  // which the app is not allowed to read, and nothing was attached.
  it('claims files before the link, and never treats a file as one', () => {
    expect(shareController.indexOf('await stage(provider')).toBeGreaterThan(-1);
    expect(shareController.indexOf('await stage(provider'))
      .toBeLessThan(shareController.indexOf('load(provider, UTType.url.identifier)'));
    expect(shareController).toContain('!provider.hasItemConformingToTypeIdentifier(UTType.fileURL.identifier)');
  });

  // The extension's container is not the app's: the only path both processes can
  // read is inside the App Group, and the provider's own URL dies with the block.
  it('copies a shared file into the App Group container', () => {
    expect(shareController).toContain('containerURL(forSecurityApplicationGroupIdentifier: Self.appGroup)');
    expect(shareController).toContain('Library/Caches/VisShare');
    expect(shareController).toContain('loadFileRepresentation(forTypeIdentifier:');
    expect(shareController).toContain('try manager.copyItem(at: url, to: destination)');
    expect(shareController).toContain('Date().addingTimeInterval(-Self.staleAfter)');
  });

  // file/name/type are read back by position in src/lib/share-intake.ts, so a
  // missing media type is still sent — as an empty string, never as a gap.
  it('hands the app file, name and type index aligned', () => {
    expect(shareController).toContain('URLQueryItem(name: "file", value: file.url.absoluteString)');
    expect(shareController).toContain('URLQueryItem(name: "name", value: file.name)');
    expect(shareController).toContain('URLQueryItem(name: "type", value: file.type)');
    expect(shareController).toContain('type.preferredMIMEType ?? ""');
  });

  // Regression, issue vis_session_id#3d6dc388-a21c-4005-b498-87c02668cb34:
  // a local index.html conforms to public.text, so the generic file branch
  // rejected it and sharing the file produced no attachment.
  it('stages a local HTML document as a file before excluding shared text', () => {
    expect(shareController).toContain('let documents: [UTType] = [.pdf, .html]');
    expect(shareController).toContain('candidate in documents.contains');
    expect(shareController.indexOf('candidate in documents.contains'))
      .toBeLessThan(shareController.indexOf('!$0.conforms(to: .text)'));
  });

  // iOS offers the extension only for what its activation rule claims: without
  // these three keys the share sheet shows Vis for a link and hides it for a memo.
  it('is offered for files, images and movies as well as links', () => {
    for (const key of ['File', 'Image', 'Movie']) {
      expect(sharePlist).toContain(`<key>NSExtensionActivationSupports${key}WithMaxCount</key>`);
    }
  });

  // Both signatures must carry the SAME group or the container is nil at runtime.
  it('entitles both targets to one App Group', () => {
    expect(prepare).toContain('const appGroup = `group.${bundleId}`;');
    expect(entitlements).toContain('<key>com.apple.security.application-groups</key>');
    expect(entitlements).toContain('<string>${appGroup}</string>');
    expect(prepare).toContain("entitlements: 'VisShare/VisShare.entitlements'");
    expect(prepare).toContain('CODE_SIGN_ENTITLEMENTS = App/App.entitlements');
    expect(release).toContain('<string>group.${appBundleId}</string>');
    // The release script adds the push entitlement only when the app target has
    // none — it must look for the APP's file, not for any entitlement at all.
    expect(release).toContain("project.includes('CODE_SIGN_ENTITLEMENTS = App/App.entitlements')");
  });
});
