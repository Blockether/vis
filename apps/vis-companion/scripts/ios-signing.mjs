// App Store signing assets: profiles from App Store Connect, manual signing in the project.
//
// WHY this exists at all. Automatic signing looks free until you count what it
// spends: `xcodebuild archive -allowProvisioningUpdates` signs the archive with a
// *development* identity and, on a throwaway CI runner whose keychain starts empty,
// mints a BRAND NEW "Apple Development" certificate every single run. The account
// caps those, and on the twelfth one the whole release stops with
//
//   error: Choose a certificate to revoke. Your account has reached the maximum
//   number of certificates.  (in target 'VisShare' from project 'App')
//
// which no amount of retrying fixes — the previous runs' certificates are garbage
// (their private keys died with the runners) yet they hold every slot.
//
// So the release signs MANUALLY with the distribution certificate the workflow
// already imports, and the archive never asks the portal for a development
// identity again. Manual signing is all-or-nothing: every bundle in the archive
// needs a named profile, the share extension included, so the profiles are
// fetched — and created when missing — through the App Store Connect API with the
// key the release already holds. No extra secret, nothing to click, no new
// certificate ever.

import { execFileSync } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import crypto from "node:crypto";

const API = "https://api.appstoreconnect.apple.com";

/**
 * Mint the short-lived ES256 token App Store Connect authenticates with.
 *
 * @param {object} options
 * @param {string} options.keyId App Store Connect key id (the JWT `kid`)
 * @param {string} options.issuerId issuer id (the JWT `iss`)
 * @param {string} options.privateKey the .p8 contents
 * @param {number} [options.now] epoch millis, for tests
 * @param {number} [options.lifetimeSeconds] token lifetime; Apple rejects over 20 minutes
 * @returns {string} a signed JWT
 */
export function ascJwt({
  keyId,
  issuerId,
  privateKey,
  now = Date.now(),
  lifetimeSeconds = 600,
}) {
  const b64 = (value) =>
    Buffer.from(
      typeof value === "string" ? value : JSON.stringify(value),
    ).toString("base64url");
  const issuedAt = Math.floor(now / 1000);
  const head = b64({ alg: "ES256", kid: keyId, typ: "JWT" });
  const body = b64({
    iss: issuerId,
    iat: issuedAt,
    exp: issuedAt + lifetimeSeconds,
    aud: "appstoreconnect-v1",
  });
  // ES256 is a RAW r‖s pair; the DER encoding OpenSSL defaults to is rejected.
  const signature = crypto.sign("sha256", Buffer.from(`${head}.${body}`), {
    key: crypto.createPrivateKey(privateKey),
    dsaEncoding: "ieee-p1363",
  });
  return `${head}.${body}.${signature.toString("base64url")}`;
}

/**
 * Choose the profile a bundle id should be signed with.
 *
 * App Store Connect keeps expired and invalidated profiles around forever, and a
 * bundle id can carry several, so prefer an ACTIVE one and, among those, the one
 * that expires last.
 *
 * @param {{data: any[], included?: any[]}} payload a `/v1/profiles?include=bundleId` response
 * @param {object} options
 * @param {string} options.bundleId the bundle identifier to match
 * @param {string} [options.profileType] Apple's profile type
 * @returns {any | undefined} the winning profile resource
 */
export function pickProfile(
  payload,
  { bundleId, profileType = "IOS_APP_STORE" },
) {
  const identifiers = Object.fromEntries(
    (payload.included ?? [])
      .filter((resource) => resource.type === "bundleIds")
      .map((resource) => [resource.id, resource.attributes.identifier]),
  );
  return (payload.data ?? [])
    .filter((profile) => {
      const owner = profile.relationships?.bundleId?.data?.id;
      return (
        identifiers[owner] === bundleId &&
        profile.attributes.profileType === profileType &&
        profile.attributes.profileState === "ACTIVE"
      );
    })
    .sort((a, b) =>
      String(b.attributes.expirationDate ?? "").localeCompare(
        String(a.attributes.expirationDate ?? ""),
      ),
    )[0];
}

/**
 * Sign the archive by hand: manual style, distribution identity, named profile.
 *
 * Only the target-level configurations of the given `configuration` are touched,
 * and only those whose PRODUCT_BUNDLE_IDENTIFIER we have a profile for — the
 * project-level defaults and every Debug build keep automatic signing, so opening
 * the same project in Xcode and running onto a phone still works.
 *
 * Idempotent: the settings it writes are the ones it strips first, so stamping an
 * already-stamped project changes nothing.
 *
 * @param {string} pbxproj contents of project.pbxproj
 * @param {object} options
 * @param {string} options.teamId Apple Developer team
 * @param {Record<string, string>} options.profileNames profile name per bundle id
 * @param {string} [options.identity] CODE_SIGN_IDENTITY to force
 * @param {string} [options.configuration] build configuration to stamp
 * @returns {{text: string, stamped: string[]}} the new project and the bundle ids changed
 */
export function stampManualSigning(
  pbxproj,
  {
    teamId,
    profileNames,
    identity = "Apple Distribution",
    configuration = "Release",
  },
) {
  const stamped = [];
  const owned =
    /^\t{4}(CODE_SIGN_STYLE|CODE_SIGN_IDENTITY|DEVELOPMENT_TEAM|PROVISIONING_PROFILE|PROVISIONING_PROFILE_SPECIFIER) = [^\n]*\n/gm;
  const text = pbxproj.replaceAll(
    /(\n\t\t[0-9A-Fa-f]{24} \/\* (\w+) \*\/ = \{\n\t\t\tisa = XCBuildConfiguration;\n)([\s\S]*?)(\n\t\t\};)/g,
    (whole, header, configName, body, tail) => {
      if (configName !== configuration) return whole;
      const bundleId = body.match(
        /\n\t{4}PRODUCT_BUNDLE_IDENTIFIER = "?([^";]+)"?;/,
      )?.[1];
      const profileName = bundleId ? profileNames[bundleId]?.trim() : undefined;
      if (!bundleId || !profileName) return whole;
      // A literal replacement would read `$&` and friends inside a profile name;
      // Apple allows neither today, but nothing guarantees it forever.
      const insert =
        `\t\t\t\tCODE_SIGN_IDENTITY = "${identity}";\n` +
        "\t\t\t\tCODE_SIGN_STYLE = Manual;\n" +
        `\t\t\t\tDEVELOPMENT_TEAM = ${teamId};\n` +
        `\t\t\t\tPROVISIONING_PROFILE_SPECIFIER = "${profileName}";\n`;
      const settings = body
        .replaceAll(owned, "")
        .replace(
          /(^|\n)\t\t\tbuildSettings = \{\n/,
          (match) => `${match}${insert}`,
        );
      // A configuration whose settings block we could not find is left exactly as
      // it was, and never reported as signed.
      if (!settings.includes("CODE_SIGN_STYLE = Manual;")) return whole;
      stamped.push(bundleId);
      return `${header}${settings}${tail}`;
    },
  );
  return { text, stamped };
}

const request = async (token, path, init = {}) => {
  const res = await fetch(`${API}${path}`, {
    ...init,
    headers: {
      Authorization: `Bearer ${token}`,
      ...(init.body ? { "Content-Type": "application/json" } : {}),
      ...init.headers,
    },
  });
  const payload = res.status === 204 ? {} : await res.json();
  if (!res.ok) {
    const detail = (payload.errors ?? [])
      .map((error) => `${error.title}: ${error.detail}`)
      .join("; ");
    throw new Error(
      `App Store Connect ${init.method ?? "GET"} ${path} → ${res.status} ${detail}`,
    );
  }
  return payload;
};

// Apple only accepts letters, digits and spaces in the names of things it stores.
const plainName = (value) => value.replaceAll(/[^A-Za-z0-9 ]+/g, " ").trim();

/**
 * Find — or create — the App Store profile of every bundle id, ready to install.
 *
 * A profile is created only when the bundle id has none, and the bundle id itself
 * is registered only when the portal does not know it yet: the share extension
 * ships under `<app>.share`, which nobody had ever registered by hand.
 *
 * @param {object} options
 * @param {string} options.keyId App Store Connect key id
 * @param {string} options.issuerId App Store Connect issuer id
 * @param {string} options.privateKey the .p8 contents
 * @param {string[]} options.bundleIds every bundle id in the archive
 * @param {(message: string) => void} [options.log] progress reporter
 * @returns {Promise<Record<string, {name: string, uuid: string, content: string}>>} profile per bundle id
 */
export async function ensureProfiles({
  keyId,
  issuerId,
  privateKey,
  bundleIds,
  log = () => {},
}) {
  const token = ascJwt({ keyId, issuerId, privateKey });
  // `bundleId` belongs in the fieldset as well as in `include`: App Store Connect
  // serialises a profile's relationships only when the sparse fieldset names them,
  // so `include=bundleId` on its own returns the bundle ids under `included` and
  // still leaves every profile unattached to one. Nothing then matches, the release
  // tries to CREATE the profile the portal already has, and Apple answers 409
  // "Multiple profiles found with the name …" — after which signing falls back to
  // automatic and the export dies on a cloud signing permission error.
  const fields =
    "fields[profiles]=name,uuid,profileType,profileState,expirationDate,profileContent,bundleId&include=bundleId&limit=200";
  let profiles = await request(token, `/v1/profiles?${fields}`);
  const resolved = {};

  for (const bundleId of bundleIds) {
    let profile = pickProfile(profiles, { bundleId });
    if (!profile) {
      log(`· no App Store profile for ${bundleId} — creating one`);
      const registered = await request(
        token,
        `/v1/bundleIds?filter[identifier]=${encodeURIComponent(bundleId)}&limit=200`,
      );
      let bundle = registered.data.find(
        (item) => item.attributes.identifier === bundleId,
      );
      if (!bundle) {
        log(`· registering bundle id ${bundleId}`);
        bundle = (
          await request(token, "/v1/bundleIds", {
            method: "POST",
            body: JSON.stringify({
              data: {
                type: "bundleIds",
                attributes: {
                  identifier: bundleId,
                  name: plainName(bundleId),
                  platform: "IOS",
                },
              },
            }),
          })
        ).data;
      }
      const certificates = await request(
        token,
        "/v1/certificates?filter[certificateType]=IOS_DISTRIBUTION&limit=200",
      );
      if (certificates.data.length === 0) {
        throw new Error(
          "no IOS_DISTRIBUTION certificate in the account — a distribution profile cannot be created",
        );
      }
      profile = (
        await request(token, "/v1/profiles", {
          method: "POST",
          body: JSON.stringify({
            data: {
              type: "profiles",
              attributes: {
                name: `${plainName(bundleId)} App Store`,
                profileType: "IOS_APP_STORE",
              },
              relationships: {
                bundleId: { data: { id: bundle.id, type: "bundleIds" } },
                certificates: {
                  data: certificates.data.map((certificate) => ({
                    id: certificate.id,
                    type: "certificates",
                  })),
                },
              },
            },
          }),
        })
      ).data;
      // The list is stale the moment we add to it, and a later bundle id may match
      // what we just created (nothing does today, but nothing should have to know).
      profiles = await request(token, `/v1/profiles?${fields}`);
    }
    resolved[bundleId] = {
      name: profile.attributes.name,
      uuid: profile.attributes.uuid,
      content: profile.attributes.profileContent,
    };
  }
  return resolved;
}

/**
 * Put a profile where xcodebuild looks for it.
 *
 * Xcode 16 reads `~/Library/Developer/Xcode/UserData/Provisioning Profiles`;
 * older toolchains and `xcodebuild -exportArchive` still read the MobileDevice
 * path, so write both and let whichever is asked answer.
 *
 * @param {object} profile
 * @param {string} profile.uuid the profile's UUID (its file name)
 * @param {string} profile.content base64 `.mobileprovision`
 * @param {string} [home] home directory, for tests
 * @returns {string[]} the paths written
 */
export function installProfile({ uuid, content }, home = homedir()) {
  const bytes = Buffer.from(content, "base64");
  return [
    join(home, "Library", "MobileDevice", "Provisioning Profiles"),
    join(
      home,
      "Library",
      "Developer",
      "Xcode",
      "UserData",
      "Provisioning Profiles",
    ),
  ].map((dir) => {
    mkdirSync(dir, { recursive: true });
    const path = join(dir, `${uuid}.mobileprovision`);
    writeFileSync(path, bytes, { mode: 0o600 });
    return path;
  });
}

/**
 * The generic distribution identity this keychain can sign with, if any.
 *
 * Manual signing cannot invent one: without the certificate's PRIVATE KEY in a
 * keychain — the workflow imports it, a laptop usually has not — the release has
 * to fall back to automatic signing rather than fail at the codesign step.
 *
 * The NAME matters as much as the presence. A certificate created as
 * IOS_DISTRIBUTION is issued to "iPhone Distribution: …", the modern DISTRIBUTION
 * type to "Apple Distribution: …", and codesign matches CODE_SIGN_IDENTITY by
 * prefix — so guessing the wrong generic name finds no identity at all. Read it
 * back from the keychain instead of assuming.
 *
 * @param {string} [output] `security find-identity` output; for tests
 * @returns {string|undefined} "Apple Distribution", "iPhone Distribution", or none
 */
export function distributionIdentity(output) {
  let text = output;
  if (text === undefined) {
    if (process.platform !== "darwin") return undefined;
    try {
      text = execFileSync(
        "security",
        ["find-identity", "-v", "-p", "codesigning"],
        { encoding: "utf8" },
      );
    } catch {
      return undefined;
    }
  }
  return text.match(/"(Apple Distribution|iPhone Distribution):/)?.[1];
}
