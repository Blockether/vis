import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// `ios-prepare.mjs` stamps the generated Xcode project the moment it is
// imported, so the Swift it embeds is read as text instead.
const prepare = readFileSync(join(dirname(fileURLToPath(import.meta.url)), 'ios-prepare.mjs'), 'utf8');
const shareController = prepare.match(/const shareControllerSource = `([\s\S]*?)\n`;/)?.[1] ?? '';

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
});
