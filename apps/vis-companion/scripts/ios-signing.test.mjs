import crypto from "node:crypto";
import { describe, expect, it } from "vitest";
import { mkdtempSync, readFileSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  ascJwt,
  distributionIdentity,
  ensureProfiles,
  installProfile,
  pickProfile,
  stampManualSigning,
} from "./ios-signing.mjs";

// A project.pbxproj in miniature: the two project-level configurations (no bundle
// id of their own), the app target's pair, and the share extension's pair.
const config = (id, name, settings) => `
		${id} /* ${name} */ = {
			isa = XCBuildConfiguration;
			buildSettings = {
${settings.map((line) => `\t\t\t\t${line}`).join("\n")}
			};
			name = ${name};
		};`;

const project = [
  "// !$*UTF8*$!",
  "/* Begin XCBuildConfiguration section */",
  config("504EC3141FED79650016851F", "Debug", [
    'CODE_SIGN_IDENTITY = "iPhone Developer";',
  ]),
  config("504EC3151FED79650016851F", "Release", [
    'CODE_SIGN_IDENTITY = "iPhone Developer";',
  ]),
  config("504EC3171FED79650016851F", "Debug", [
    "CODE_SIGN_STYLE = Automatic;",
    "PRODUCT_BUNDLE_IDENTIFIER = com.blockether.viscompanion;",
  ]),
  config("504EC3181FED79650016851F", "Release", [
    "CODE_SIGN_STYLE = Automatic;",
    "PRODUCT_BUNDLE_IDENTIFIER = com.blockether.viscompanion;",
  ]),
  config("5A11E500000000000000000B", "Release", [
    "CODE_SIGN_STYLE = Automatic;",
    "PRODUCT_BUNDLE_IDENTIFIER = com.blockether.viscompanion.share;",
  ]),
  "/* End XCBuildConfiguration section */",
].join("\n");

const profileNames = {
  "com.blockether.viscompanion": "Vis Companion App Store",
  "com.blockether.viscompanion.share": "Vis Companion Share App Store",
};

const stamp = (text = project) =>
  stampManualSigning(text, { teamId: "JSZTFUBUBB", profileNames });

// The build that failed did so because ONE bundle was left to sign itself.
describe("stampManualSigning", () => {
  it("names a profile for every shipped bundle, extension included", () => {
    const { text, stamped } = stamp();
    expect(stamped).toEqual([
      "com.blockether.viscompanion",
      "com.blockether.viscompanion.share",
    ]);
    expect(text).toContain(
      'PROVISIONING_PROFILE_SPECIFIER = "Vis Companion App Store";',
    );
    expect(text).toContain(
      'PROVISIONING_PROFILE_SPECIFIER = "Vis Companion Share App Store";',
    );
    expect(text.match(/CODE_SIGN_STYLE = Manual;/g)).toHaveLength(2);
    expect(text).toContain('CODE_SIGN_IDENTITY = "Apple Distribution";');
    expect(text).toContain("DEVELOPMENT_TEAM = JSZTFUBUBB;");
  });

  it("leaves automatic signing to Debug and to the project defaults", () => {
    const { text } = stamp();
    const debugAppConfig = text.slice(
      text.indexOf("504EC3171FED79650016851F"),
      text.indexOf("504EC3181FED79650016851F"),
    );
    expect(debugAppConfig).toContain("CODE_SIGN_STYLE = Automatic;");
    expect(debugAppConfig).not.toContain("CODE_SIGN_STYLE = Manual;");
    // The project-level Release configuration owns no bundle id and is untouched.
    const projectRelease = text.slice(
      text.indexOf("504EC3151FED79650016851F"),
      text.indexOf("504EC3171FED79650016851F"),
    );
    expect(projectRelease).toContain(
      'CODE_SIGN_IDENTITY = "iPhone Developer";',
    );
  });

  it("is idempotent, so a re-run never stacks a second copy", () => {
    const once = stamp().text;
    const twice = stamp(once).text;
    expect(twice).toBe(once);
    expect(twice.match(/PROVISIONING_PROFILE_SPECIFIER/g)).toHaveLength(2);
  });

  it("skips a bundle it has no profile for rather than mis-signing it", () => {
    const { text, stamped } = stampManualSigning(project, {
      teamId: "JSZTFUBUBB",
      profileNames: {
        "com.blockether.viscompanion": "Vis Companion App Store",
      },
    });
    expect(stamped).toEqual(["com.blockether.viscompanion"]);
    expect(text.match(/CODE_SIGN_STYLE = Manual;/g)).toHaveLength(1);
    expect(text).toContain(
      "PRODUCT_BUNDLE_IDENTIFIER = com.blockether.viscompanion.share;",
    );
  });
});

describe("distributionIdentity", () => {
  // Verbatim `security find-identity -v -p codesigning` output.
  const listing = (...names) =>
    `${names
      .map((name, i) => `  ${i + 1}) ${"0".repeat(40)} "${name}"`)
      .join("\n")}\n     ${names.length} valid identities found\n`;

  it("reads the generic name back rather than assuming one", () => {
    expect(
      distributionIdentity(
        listing(
          "Apple Development: Someone (ABC123)",
          "Apple Distribution: Blockether (JSZTFUBUBB)",
        ),
      ),
    ).toBe("Apple Distribution");
  });

  // A certificate created as IOS_DISTRIBUTION is issued to the legacy name, and
  // CODE_SIGN_IDENTITY = "Apple Distribution" would then match nothing.
  it("recognises the legacy iPhone Distribution certificate", () => {
    expect(
      distributionIdentity(
        listing("iPhone Distribution: Blockether (JSZTFUBUBB)"),
      ),
    ).toBe("iPhone Distribution");
  });

  it("finds nothing in a keychain that can only sign for development", () => {
    expect(
      distributionIdentity(listing("Apple Development: Someone (ABC123)")),
    ).toBeUndefined();
    expect(
      distributionIdentity("     0 valid identities found\n"),
    ).toBeUndefined();
  });
});

describe("pickProfile", () => {
  const payload = {
    data: [
      {
        id: "p-old",
        attributes: {
          name: "old",
          profileType: "IOS_APP_STORE",
          profileState: "ACTIVE",
          expirationDate: "2027-01-01T00:00:00.000+0000",
        },
        relationships: { bundleId: { data: { id: "b-app" } } },
      },
      {
        id: "p-new",
        attributes: {
          name: "new",
          profileType: "IOS_APP_STORE",
          profileState: "ACTIVE",
          expirationDate: "2027-08-01T00:00:00.000+0000",
        },
        relationships: { bundleId: { data: { id: "b-app" } } },
      },
      {
        id: "p-invalid",
        attributes: {
          name: "invalid",
          profileType: "IOS_APP_STORE",
          profileState: "INVALID",
          expirationDate: "2028-01-01T00:00:00.000+0000",
        },
        relationships: { bundleId: { data: { id: "b-app" } } },
      },
      {
        id: "p-dev",
        attributes: {
          name: "development",
          profileType: "IOS_APP_DEVELOPMENT",
          profileState: "ACTIVE",
          expirationDate: "2029-01-01T00:00:00.000+0000",
        },
        relationships: { bundleId: { data: { id: "b-app" } } },
      },
    ],
    included: [
      {
        type: "bundleIds",
        id: "b-app",
        attributes: { identifier: "com.blockether.viscompanion" },
      },
      {
        type: "bundleIds",
        id: "b-share",
        attributes: { identifier: "com.blockether.viscompanion.share" },
      },
    ],
  };

  it("takes the live App Store profile that expires last", () => {
    expect(
      pickProfile(payload, { bundleId: "com.blockether.viscompanion" })?.id,
    ).toBe("p-new");
  });

  it("reports nothing for a bundle id with no profile of its own", () => {
    expect(
      pickProfile(payload, { bundleId: "com.blockether.viscompanion.share" }),
    ).toBeUndefined();
  });
});

describe("ascJwt", () => {
  const { privateKey, publicKey } = crypto.generateKeyPairSync("ec", {
    namedCurve: "P-256",
  });
  const token = ascJwt({
    keyId: "KEY123",
    issuerId: "ISSUER-1",
    privateKey: privateKey.export({ type: "pkcs8", format: "pem" }),
    now: 1_700_000_000_000,
  });
  const [head, body, signature] = token.split(".");
  const decode = (part) =>
    JSON.parse(Buffer.from(part, "base64url").toString("utf8"));

  it("claims exactly what App Store Connect checks", () => {
    expect(decode(head)).toEqual({ alg: "ES256", kid: "KEY123", typ: "JWT" });
    expect(decode(body)).toEqual({
      iss: "ISSUER-1",
      iat: 1_700_000_000,
      exp: 1_700_000_600,
      aud: "appstoreconnect-v1",
    });
  });

  it("signs as a raw r‖s pair, which is the only encoding Apple accepts", () => {
    const raw = Buffer.from(signature, "base64url");
    expect(raw).toHaveLength(64);
    expect(
      crypto.verify(
        "sha256",
        Buffer.from(`${head}.${body}`),
        {
          key: publicKey,
          dsaEncoding: "ieee-p1363",
        },
        raw,
      ),
    ).toBe(true);
  });
});

describe("ensureProfiles", () => {
  const { privateKey } = crypto.generateKeyPairSync("ec", {
    namedCurve: "P-256",
    privateKeyEncoding: { type: "pkcs8", format: "pem" },
    publicKeyEncoding: { type: "spki", format: "pem" },
  });

  const bundleIds = [
    "com.blockether.viscompanion",
    "com.blockether.viscompanion.share",
  ];

  // The portal as it really answers. `include=bundleId` lists the bundle ids under
  // `included`, but a profile's own `relationships.bundleId` is serialised only when
  // the sparse fieldset asks for that relationship too — ask for `include` alone and
  // every profile comes back unattached, matching nothing. What that silence costs is
  // the 409 below: the profile Apple already has cannot be created a second time.
  const portal = () => {
    const calls = [];
    const reply = (status, body) => ({
      ok: status < 300,
      status,
      json: async () => body,
    });
    const fetch = async (url, init = {}) => {
      const { pathname, searchParams } = new URL(url);
      const method = init.method ?? "GET";
      calls.push(`${method} ${pathname}`);
      if (method === "GET" && pathname === "/v1/profiles") {
        const linked = (searchParams.get("fields[profiles]") ?? "")
          .split(",")
          .includes("bundleId");
        return reply(200, {
          data: bundleIds.map((identifier, index) => ({
            id: `p-${index}`,
            attributes: {
              name: `${identifier} App Store`,
              uuid: `uuid-${index}`,
              profileType: "IOS_APP_STORE",
              profileState: "ACTIVE",
              expirationDate: "2027-01-01T00:00:00.000+0000",
              profileContent: "cHJvZmlsZQ==",
            },
            ...(linked
              ? { relationships: { bundleId: { data: { id: `b-${index}` } } } }
              : {}),
          })),
          included: bundleIds.map((identifier, index) => ({
            type: "bundleIds",
            id: `b-${index}`,
            attributes: { identifier },
          })),
        });
      }
      if (method === "POST" && pathname === "/v1/profiles") {
        return reply(409, {
          errors: [
            {
              title: "There is a problem with the request entity",
              detail:
                "Multiple profiles found with the name 'com blockether viscompanion App Store'.  Please remove the duplicate profiles and try again.",
            },
          ],
        });
      }
      if (pathname === "/v1/certificates") {
        return reply(200, {
          data: [{ id: "c-dist", attributes: { certificateType: "IOS_DISTRIBUTION" } }],
        });
      }
      return reply(200, { data: [] });
    };
    return { calls, fetch };
  };

  it("signs with the profiles the portal already has, creating none", async () => {
    const { calls, fetch } = portal();
    const original = globalThis.fetch;
    globalThis.fetch = fetch;
    try {
      const resolved = await ensureProfiles({
        keyId: "KEYID",
        issuerId: "ISSUERID",
        privateKey,
        bundleIds,
      });
      expect(Object.keys(resolved)).toEqual(bundleIds);
      expect(resolved["com.blockether.viscompanion"].uuid).toBe("uuid-0");
      expect(resolved["com.blockether.viscompanion.share"].uuid).toBe("uuid-1");
    } finally {
      globalThis.fetch = original;
    }
    expect(calls).not.toContain("POST /v1/profiles");
  });
});

describe("installProfile", () => {
  it("writes the profile only where Xcode 16 and later read it", () => {
    const home = mkdtempSync(join(tmpdir(), "vis-home-"));
    const written = installProfile(
      { uuid: "uuid-0", content: Buffer.from("profile").toString("base64") },
      home,
    );
    expect(written).toEqual([
      join(
        home,
        "Library",
        "Developer",
        "Xcode",
        "UserData",
        "Provisioning Profiles",
        "uuid-0.mobileprovision",
      ),
    ]);
    expect(readFileSync(written[0], "utf8")).toBe("profile");
    // A second copy under Library/MobileDevice is not read, and Xcode deletes the
    // one it DID choose mid-archive while both exist.
    expect(readdirSync(join(home, "Library"))).toEqual(["Developer"]);
  });
});
