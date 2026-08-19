import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

// `android-prepare.mjs` stamps the generated Gradle project the moment it is
// imported, so the Java and the manifest filters it embeds are read as text.
const prepare = readFileSync(join(dirname(fileURLToPath(import.meta.url)), 'android-prepare.mjs'), 'utf8');
const mainActivity = prepare.match(/const mainActivity = `([\s\S]*?)\n`;/)?.[1] ?? '';

describe('Android share target', () => {
  it('embeds a MainActivity that rewrites a share into a vis:// link', () => {
    expect(mainActivity).toContain('public class MainActivity extends BridgeActivity');
    expect(mainActivity).toContain('setIntent(asShareLink(getIntent()));');
    expect(mainActivity).toContain('public void onNewIntent(Intent intent)');
  });

  // The shipped bug: a shared voice memo arrived as its `content://` URI in the
  // TEXT of the message. That URI is a permission grant to the intent that
  // carried it — the webview holds no such grant, so the app could open nothing.
  it('copies the shared bytes instead of forwarding a content:// URI', () => {
    expect(mainActivity).toContain('Intent.EXTRA_STREAM');
    expect(mainActivity).toContain('private File stageShared(Uri stream, int index)');
    expect(mainActivity).toContain('Uri.fromFile(staged).toString()');
    expect(mainActivity).not.toContain('share.appendQueryParameter("text", stream.toString())');
  });

  // file/name/type are read back BY POSITION in src/lib/share-intake.ts.
  it('hands over file, name and type index aligned', () => {
    expect(mainActivity).toContain('share.appendQueryParameter("file"');
    expect(mainActivity).toContain('share.appendQueryParameter("name"');
    expect(mainActivity).toContain('share.appendQueryParameter("type"');
  });

  // Multi-select share sends SEND_MULTIPLE; a copy nobody came back for is the
  // app's own litter, so staging sweeps before it writes.
  it('accepts a multi-file share and sweeps what was abandoned', () => {
    expect(mainActivity).toContain('Intent.ACTION_SEND_MULTIPLE');
    expect(mainActivity).toContain('purgeStaged();');
    expect(mainActivity).toContain('STAGED_MAX_AGE_MS');
    expect(mainActivity).toContain('MAX_STAGED_BYTES');
  });

  // An app offered in the sheet that refuses what it was handed is worse than one
  // that is not offered, so the filters name exactly what the composer carries.
  it('claims text, images, audio, video and PDF — never */*', () => {
    expect(prepare).toContain(
      "const SHARE_MEDIA_TYPES = ['text/plain', 'image/*', 'audio/*', 'video/*', 'application/pdf'];",
    );
    expect(prepare).toContain("shareFilter('SEND', SHARE_MEDIA_TYPES)");
    expect(prepare).toContain("shareFilter('SEND_MULTIPLE', SHARE_MEDIA_TYPES)");
    expect(prepare).not.toContain('android:mimeType="*/*"');
  });

  // Stamped, never appended to: widening the sheet on an existing checkout would
  // otherwise leave the previous text-only block beside the new filters.
  it('replaces an earlier filter block rather than adding to it', () => {
    expect(prepare).toContain('const shareManifestBefore = shareManifest;');
    expect(prepare).toMatch(/shareManifest = shareManifest\.replace\(/);
    expect(prepare).toContain('action\\.SEND|action\\.SEND_MULTIPLE|action\\.PROCESS_TEXT');
  });
});
