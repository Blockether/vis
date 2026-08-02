#!/usr/bin/env node
/**
 * Make the generated `android/` project releasable, from secrets that are NOT in the repo.
 *
 * `android/` is gitignored — Capacitor regenerates it from scratch with `cap add android`,
 * and a regenerated project has: no google-services.json, no signing config, and
 * versionCode 1 / versionName "1.0" hardcoded in app/build.gradle. Every one of those is
 * fatal for a Play upload (unsigned, or "version code 1 already used"). So this script
 * stamps all three in, idempotently, right before a build:
 *
 *   1. google-services.json  → android/app/            (Firebase push config; optional)
 *   2. upload keystore       → android/app/vis-upload.keystore + keystore.properties
 *                              (0600, materialised from the login keychain per build and
 *                               shredded by `--clean`; never committed, never in Downloads)
 *   3. versionName/versionCode → app/build.gradle
 *      versionName = repo-root VIS_VERSION, versionCode = `git rev-list --count HEAD`,
 *      the SAME pair scripts/ios-release.mjs uses, so a build number identifies one commit
 *      on both stores and two uploads can never collide.
 *   4. cleartext HTTP        → res/xml/network_security_config.xml + AndroidManifest
 *      (API 28+ blocks http:// by default; the gateway is a bare LAN/tailnet IP)
 *
 * Usage:
 *   node scripts/android-prepare.mjs
 *   node scripts/android-prepare.mjs --build 4711
 *   node scripts/android-prepare.mjs --file google-services.json
 *   node scripts/android-prepare.mjs --clean        # shred the materialised keystore
 *   node scripts/android-prepare.mjs --check        # verify branded launcher assets after sync
 */
import { spawnSync } from 'node:child_process';
import { cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { syncPackageVersion } from './version.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repoRoot = resolve(root, '..', '..');
const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};
const has = (name) => args.includes(`--${name}`);

const die = (msg) => {
  console.error(`\n\u2717 ${msg}\n`);
  process.exit(1);
};

// `security -w` prints hex for anything that is not plain printable ASCII.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);

const keychain = (service, account) => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], { encoding: 'utf8' });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};
// Env first so CI (which has no keychain) injects the same values as GitHub secrets.
const secret = (envName, service, account) => process.env[envName]?.trim() || keychain(service, account);

const home = process.env.HOME ?? '';
const expand = (p) => resolve(p.replace(/^~/, home));

const androidDir = resolve(root, 'android');
const androidApp = join(androidDir, 'app');
const keystorePath = join(androidApp, 'vis-upload.keystore');
const keystoreProps = join(androidDir, 'keystore.properties');

if (has('clean')) {
  for (const p of [keystorePath, keystoreProps]) {
    if (existsSync(p)) {
      rmSync(p, { force: true });
      console.log(`\u00b7 shredded ${p}`);
    }
  }
  process.exit(0);
}

if (!existsSync(androidApp)) die('no android/app — run `npm run add:android` first');

// `android/` is absent in clean CI and Capacitor's fresh scaffold contains its
// generic launcher. Always overlay the tracked Vis icon set before Gradle runs.
const nativeIconAssets = join(root, 'native-assets', 'android', 'res');
const androidResources = join(androidApp, 'src', 'main', 'res');
if (!existsSync(nativeIconAssets)) die(`no tracked Android icons at ${nativeIconAssets}`);

const assetFiles = (dir, prefix = '') =>
  readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const relative = join(prefix, entry.name);
    return entry.isDirectory() ? assetFiles(join(dir, entry.name), relative) : [relative];
  });

const launcherAssets = assetFiles(nativeIconAssets);
const mismatchedLauncherAssets = () =>
  launcherAssets.filter((relative) => {
    const source = join(nativeIconAssets, relative);
    const target = join(androidResources, relative);
    return !existsSync(target) || !readFileSync(source).equals(readFileSync(target));
  });

if (has('check')) {
  const mismatched = mismatchedLauncherAssets();
  if (mismatched.length) {
    die(`Android launcher assets differ from tracked Vis branding: ${mismatched.join(', ')}`);
  }
  console.log('✓ launcher icons match tracked Vis branding');
  process.exit(0);
}

cpSync(nativeIconAssets, androidResources, { recursive: true, force: true });
console.log('✓ launcher icons → android/app/src/main/res  (tracked Vis branding)');

const appId = JSON.parse(readFileSync(resolve(root, 'capacitor.config.json'), 'utf8')).appId;

// ── 1. google-services.json (push config; optional) ───────────────────────────────────

const explicit = flag('file');
const fallback = `${home}/.vis/fcm/google-services.json`;
const services =
  (explicit && readFileSync(expand(explicit), 'utf8')) ||
  secret('VIS_ANDROID_GOOGLE_SERVICES', 'vis-fcm', 'google_services') ||
  (existsSync(fallback) && readFileSync(fallback, 'utf8')) ||
  undefined;

if (!services) {
  console.log('\n\u00b7 no google-services.json (keychain vis-fcm/google_services, ~/.vis/fcm/, or --file)');
  console.log('  Android push stays off; everything else builds normally.');
  console.log('  Firebase console \u25b8 Project settings \u25b8 your Android app \u25b8 download, then:');
  console.log('    npm run secrets android <google-services.json>\n');
} else {
  let parsed;
  try {
    parsed = JSON.parse(services);
  } catch {
    die('that google-services.json is not valid JSON');
  }
  const packages = (parsed.client ?? []).map((c) => c?.client_info?.android_client_info?.package_name).filter(Boolean);
  if (!packages.length) die('that JSON has no client[].client_info.android_client_info.package_name — wrong file?');
  if (!packages.includes(appId)) {
    die(`google-services.json is for ${packages.join(', ')}, but this app is ${appId}\n  Add an Android app with that exact package name in the Firebase console.`);
  }
  writeFileSync(join(androidApp, 'google-services.json'), services, { mode: 0o600 });
  console.log(`\u2713 google-services.json \u2192 android/app  (${appId}, project ${parsed.project_info?.project_id ?? '?'})`);
}

// ── 2. upload keystore ────────────────────────────────────────────────────────────────
// Stored base64 because a JKS is binary and the keychain holds text. Play App Signing
// re-signs with Google's own key, so THIS key only proves "the upload came from us" —
// but losing it still means opening a support ticket, hence: generate once, keep it in
// the keychain, never in the repo, never in Downloads.

const ksB64 = secret('VIS_ANDROID_KEYSTORE', 'vis-android', 'keystore');
const storePassword = secret('VIS_ANDROID_KEYSTORE_PASSWORD', 'vis-android', 'keystore_password');
const keyAlias = secret('VIS_ANDROID_KEY_ALIAS', 'vis-android', 'key_alias') ?? 'upload';
const keyPassword = secret('VIS_ANDROID_KEY_PASSWORD', 'vis-android', 'key_password') ?? storePassword;

if (ksB64 && storePassword) {
  writeFileSync(keystorePath, Buffer.from(ksB64, 'base64'), { mode: 0o600 });
  writeFileSync(
    keystoreProps,
    `storeFile=${keystorePath}\nstorePassword=${storePassword}\nkeyAlias=${keyAlias}\nkeyPassword=${keyPassword}\n`,
    { mode: 0o600 },
  );
  console.log(`\u2713 upload keystore \u2192 android/app/vis-upload.keystore  (alias ${keyAlias})`);
} else if (existsSync(keystoreProps)) {
  console.log('\u00b7 keystore.properties already present — leaving it alone');
} else {
  console.log('\n\u00b7 no upload keystore (keychain vis-android/keystore). Release builds will be UNSIGNED');
  console.log('  and Play will reject them. Create one once:');
  console.log('    npm run secrets keystore create        # generate a fresh one');
  console.log('    npm run secrets keystore adopt         # import the one android/keystore.properties points at\n');
}

// ── 3. SDK location ───────────────────────────────────────────────────────────────────
// Gradle refuses to build without one, and `android/` is regenerated by Capacitor, so
// local.properties has to be written here rather than kept by hand. ANDROID_HOME wins
// (that is what CI's setup-android exports); otherwise take the usual install locations,
// including Homebrew's android-commandlinetools, which is what this machine has.
const sdkDir = [
  process.env.ANDROID_HOME,
  process.env.ANDROID_SDK_ROOT,
  expand('~/Library/Android/sdk'),
  '/opt/homebrew/share/android-commandlinetools',
  '/usr/local/share/android-commandlinetools',
  '/usr/local/share/android-sdk',
].find((p) => p && existsSync(p));

if (sdkDir) {
  writeFileSync(join(androidDir, 'local.properties'), `sdk.dir=${sdkDir}\n`);
  console.log(`\u2713 sdk.dir \u2192 ${sdkDir}`);
} else {
  console.log('\n\u00b7 no Android SDK found (ANDROID_HOME unset, no ~/Library/Android/sdk).');
  console.log('  Install one: `brew install --cask android-commandlinetools` or Android Studio,');
  console.log('  then re-run. Gradle cannot build a release bundle without it.\n');
}

// ── 4. minSdk floor in variables.gradle ──────────────────────────────────────────────
// Capacitor writes minSdkVersion 24, but @capacitor/barcode-scanner pulls in
// io.ionic.libs:ionbarcode-android, which declares 26 — the manifest merger then fails the
// RELEASE build only, long after a debug run looked fine. Raising the floor here (not by
// hand in the gitignored android/) keeps it fixed across every `npx cap sync`.
const MIN_SDK = 26;
const varsPath = join(androidDir, 'variables.gradle');
if (existsSync(varsPath)) {
  const vars = readFileSync(varsPath, 'utf8');
  const current = Number(/minSdkVersion\s*=\s*(\d+)/.exec(vars)?.[1] ?? 0);
  if (current && current < MIN_SDK) {
    writeFileSync(varsPath, vars.replace(/minSdkVersion\s*=\s*\d+/, `minSdkVersion = ${MIN_SDK}`));
    console.log(`\u2713 variables.gradle  minSdkVersion ${current} \u2192 ${MIN_SDK} (barcode scanner requires it)`);
  }
}

// ── 5. signing config + versions in app/build.gradle ──────────────────────────────────

const capture = (cmd, cmdArgs) => {
  const res = spawnSync(cmd, cmdArgs, { encoding: 'utf8', cwd: repoRoot });
  return res.status === 0 ? res.stdout.trim() : undefined;
};

// Repo-root VIS_VERSION is the source of truth; npm metadata mirrors it.
const versionName = syncPackageVersion();
const versionCode = flag('build') ?? capture('git', ['rev-list', '--count', 'HEAD']) ?? '1';

const gradlePath = join(androidApp, 'build.gradle');
let gradle = readFileSync(gradlePath, 'utf8');
const before = gradle;

// Load block — Capacitor's template has no notion of a keystore.
if (!gradle.includes('keystorePropsFile')) {
  gradle = gradle.replace(
    /^(apply plugin: 'com\.android\.application'\n)/m,
    `$1
// Injected by scripts/android-prepare.mjs — the properties file is materialised per build
// from the login keychain and is gitignored; a missing file just means an unsigned build.
def keystorePropsFile = rootProject.file("keystore.properties")
def keystoreProps = new Properties()
if (keystorePropsFile.exists()) {
    keystoreProps.load(new FileInputStream(keystorePropsFile))
}
`,
  );
}

if (!gradle.includes('signingConfigs {')) {
  gradle = gradle.replace(
    /^(\s*)buildTypes \{/m,
    `$1signingConfigs {
$1    release {
$1        if (keystorePropsFile.exists()) {
$1            storeFile file(keystoreProps['storeFile'])
$1            storePassword keystoreProps['storePassword']
$1            keyAlias keystoreProps['keyAlias']
$1            keyPassword keystoreProps['keyPassword']
$1        }
$1    }
$1}
$1buildTypes {`,
  );
}

if (!/release \{\s*\n\s*signingConfig signingConfigs\.release/.test(gradle)) {
  gradle = gradle.replace(/(buildTypes \{\s*\n(\s*)release \{\n)/m, `$1$2    signingConfig signingConfigs.release\n`);
}

gradle = gradle.replace(/versionCode\s+\d+/, `versionCode ${versionCode}`).replace(/versionName\s+"[^"]*"/, `versionName "${versionName}"`);

if (gradle !== before) writeFileSync(gradlePath, gradle);
console.log(`\u2713 app/build.gradle  versionName ${versionName}, versionCode ${versionCode}, signingConfig release`);

/**
 * 4. Cleartext HTTP to the gateway.
 *
 * The gateway is reached over plain HTTP at a bare IP (LAN 192.168.x, Tailscale 100.64/10).
 * Since API 28 Android's default network security config sets cleartextTrafficPermitted=false,
 * so every fetch/EventSource to http://<ip>:7890 fails locally with a bare "Load failed" —
 * the exact Android twin of the iOS ATS block fixed in ios/App/App/Info.plist.
 * Android's NSC has no CIDR support (only literal <domain> entries), and the gateway IP is
 * discovered at runtime, so a base-config opt-in is the only workable form.
 */
const xmlDir = join(root, 'android', 'app', 'src', 'main', 'res', 'xml');
const nscPath = join(xmlDir, 'network_security_config.xml');
const nsc = `<?xml version="1.0" encoding="utf-8"?>
<network-security-config>
    <base-config cleartextTrafficPermitted="true">
        <trust-anchors>
            <certificates src="system" />
        </trust-anchors>
    </base-config>
</network-security-config>
`;
mkdirSync(xmlDir, { recursive: true });
if (!existsSync(nscPath) || readFileSync(nscPath, 'utf8') !== nsc) writeFileSync(nscPath, nsc);

const manifestPath = join(root, 'android', 'app', 'src', 'main', 'AndroidManifest.xml');
let manifest = readFileSync(manifestPath, 'utf8');
const manifestBefore = manifest;
if (!manifest.includes('android:usesCleartextTraffic')) {
  manifest = manifest.replace(/<application\b/, '<application\n        android:usesCleartextTraffic="true"');
}
if (!manifest.includes('android:networkSecurityConfig')) {
  manifest = manifest.replace(/<application\b/, '<application\n        android:networkSecurityConfig="@xml/network_security_config"');
}
if (manifest !== manifestBefore) writeFileSync(manifestPath, manifest);
console.log('\u2713 cleartext HTTP  network_security_config.xml + AndroidManifest (LAN + tailnet gateway)');

/**
 * 6. System share target.
 *
 * "Share → Vis" from a browser, a feed reader, or a text selection has to reach
 * the web layer, and the ONLY channel Capacitor gives us is `appUrlOpen` /
 * `getLaunchUrl`, both of which read `intent.getData()` on an ACTION_VIEW
 * intent. An ACTION_SEND intent carries its payload in extras and has no data
 * URI at all, so the bridge simply never sees it.
 *
 * So the activity rewrites the intent into the same `vis://share?…` URL the iOS
 * share extension and the Shortcuts action produce, BEFORE Capacitor looks at
 * it: one shape on the wire, one code path in `src/lib/share-intake.ts`.
 * `onCreate` covers the cold start (app not running — the launch intent IS the
 * share) and `onNewIntent` the warm one (`launchMode=singleTask`).
 *
 * Stamped here, never hand-edited: `android/` is gitignored and CI recreates it
 * from scratch on every build.
 */
const SHARE_MARKER = 'vis:share-target';
const javaPackageDir = join(androidApp, 'src', 'main', 'java', ...appId.split('.'));
const mainActivityPath = join(javaPackageDir, 'MainActivity.java');
const mainActivity = `package ${appId};

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.TextUtils;
import com.getcapacitor.BridgeActivity;
import java.util.ArrayList;

// ${SHARE_MARKER} — stamped by scripts/android-prepare.mjs; edit that, not this file.
public class MainActivity extends BridgeActivity {

    private static final String PROCESS_TEXT = "android.intent.action.PROCESS_TEXT";
    private static final String EXTRA_PROCESS_TEXT = "android.intent.extra.PROCESS_TEXT";

    @Override
    public void onCreate(Bundle savedInstanceState) {
        // Before super: the bridge reads the launch intent while it starts up,
        // and a cold-start share must already look like a vis:// link by then.
        setIntent(asShareLink(getIntent()));
        super.onCreate(savedInstanceState);
    }

    @Override
    public void onNewIntent(Intent intent) {
        Intent rewritten = asShareLink(intent);
        setIntent(rewritten);
        super.onNewIntent(rewritten);
    }

    /** ACTION_SEND / ACTION_SEND_MULTIPLE / PROCESS_TEXT → vis://share?… ; anything else untouched. */
    private static Intent asShareLink(Intent intent) {
        if (intent == null) {
            return null;
        }
        String action = intent.getAction();
        if (action == null) {
            return intent;
        }
        String text = null;
        String title = null;
        if (Intent.ACTION_SEND.equals(action)) {
            text = string(intent.getCharSequenceExtra(Intent.EXTRA_TEXT));
            title = string(intent.getCharSequenceExtra(Intent.EXTRA_SUBJECT));
            if (text == null) {
                // An image/file share: the stream URI is all we can honestly
                // forward, and a link to it still beats dropping the share.
                android.os.Parcelable stream = intent.getParcelableExtra(Intent.EXTRA_STREAM);
                text = stream == null ? null : stream.toString();
            }
        } else if (Intent.ACTION_SEND_MULTIPLE.equals(action)) {
            ArrayList<CharSequence> parts = intent.getCharSequenceArrayListExtra(Intent.EXTRA_TEXT);
            title = string(intent.getCharSequenceExtra(Intent.EXTRA_SUBJECT));
            if (parts != null && !parts.isEmpty()) {
                text = TextUtils.join("\\n", parts);
            } else {
                ArrayList<android.os.Parcelable> streams = intent.getParcelableArrayListExtra(Intent.EXTRA_STREAM);
                if (streams != null && !streams.isEmpty()) {
                    StringBuilder joined = new StringBuilder();
                    for (android.os.Parcelable stream : streams) {
                        if (joined.length() > 0) {
                            joined.append('\\n');
                        }
                        joined.append(stream.toString());
                    }
                    text = joined.toString();
                }
            }
        } else if (PROCESS_TEXT.equals(action)) {
            text = string(intent.getCharSequenceExtra(EXTRA_PROCESS_TEXT));
        } else {
            return intent;
        }
        if (text == null && title == null) {
            return intent;
        }
        Uri.Builder share = new Uri.Builder().scheme("vis").authority("share");
        // A bare link goes in \`url\` so the web layer can treat it as one; prose
        // stays prose. Sharing from a browser sends the URL as EXTRA_TEXT.
        if (isLink(text)) {
            share.appendQueryParameter("url", text);
        } else if (text != null) {
            share.appendQueryParameter("text", text);
        }
        if (title != null) {
            share.appendQueryParameter("title", title);
        }
        // A nonce: sharing the SAME page twice must produce two DIFFERENT URLs,
        // or the app's deep-link dedupe (src/lib/deeplink.ts) swallows the second.
        share.appendQueryParameter("at", Long.toString(System.currentTimeMillis()));
        Intent out = new Intent(intent);
        out.setAction(Intent.ACTION_VIEW);
        out.setData(share.build());
        return out;
    }

    private static String string(CharSequence value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.toString().trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private static boolean isLink(String value) {
        if (value == null || value.contains(" ") || value.contains("\\n")) {
            return false;
        }
        String lower = value.toLowerCase();
        return lower.startsWith("http://") || lower.startsWith("https://");
    }
}
`;
mkdirSync(javaPackageDir, { recursive: true });
if (!existsSync(mainActivityPath) || readFileSync(mainActivityPath, 'utf8') !== mainActivity) {
  writeFileSync(mainActivityPath, mainActivity);
}

// The filters that put Vis in the system share sheet. `text/plain` is what a
// browser, a reader and a text selection all send; `*/*` would also claim every
// binary share we cannot do anything useful with.
const shareFilters = `
            <intent-filter>
                <action android:name="android.intent.action.SEND" />
                <category android:name="android.intent.category.DEFAULT" />
                <data android:mimeType="text/plain" />
            </intent-filter>
            <intent-filter>
                <action android:name="android.intent.action.SEND_MULTIPLE" />
                <category android:name="android.intent.category.DEFAULT" />
                <data android:mimeType="text/plain" />
            </intent-filter>
            <intent-filter>
                <action android:name="android.intent.action.PROCESS_TEXT" />
                <category android:name="android.intent.category.DEFAULT" />
                <data android:mimeType="text/plain" />
            </intent-filter>
`;
let shareManifest = readFileSync(manifestPath, 'utf8');
const shareManifestBefore = shareManifest;
if (!shareManifest.includes('android.intent.action.SEND')) {
  const at = shareManifest.indexOf('</activity>');
  if (at < 0) die('AndroidManifest.xml has no </activity> to attach the share filters to');
  shareManifest = shareManifest.slice(0, at) + shareFilters.replace(/^\n/, '') + '        ' + shareManifest.slice(at);
}
if (shareManifest !== shareManifestBefore) writeFileSync(manifestPath, shareManifest);
console.log('\u2713 share target   MainActivity SEND/PROCESS_TEXT \u2192 vis://share + AndroidManifest filters');
