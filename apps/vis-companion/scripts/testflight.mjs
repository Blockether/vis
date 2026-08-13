#!/usr/bin/env node
/**
 * Put a build in front of PUBLIC TestFlight testers, headlessly.
 *
 * Uploading an .ipa (scripts/ios-release.mjs) only makes a build exist. Nobody outside the
 * team can install it until three separate things happen in App Store Connect, and this
 * script is those three things:
 *
 *   1. an EXTERNAL beta group with a PUBLIC LINK exists  (anyone with the URL can join,
 *      no email invite, up to 10 000 testers)
 *   2. the build passes BETA APP REVIEW — mandatory for external testers, not for internal
 *      ones; that is the whole reason "it works for me" builds are invisible to the public
 *   3. the build is LINKED to that group
 *
 * Steps 2 and 3 both need a build in `processingState: VALID`, which is minutes after the
 * upload returns — hence the polling.
 *
 * Beta App Review will reject the submission unless the app's beta metadata is filled in
 * once: a beta localization (feedback email + description) and a review contact. Those are
 * per-app, not per-build, so this script checks them, fills them when the corresponding
 * flags are given, and otherwise tells you exactly which field is missing rather than
 * letting Apple fail the submission with a generic error.
 *
 * Usage:
 *   node scripts/testflight.mjs                       # newest build, group "Public"
 *   node scripts/testflight.mjs --build 2709
 *   node scripts/testflight.mjs --group "Public Beta"
 *   node scripts/testflight.mjs --no-review           # link only (group already approved)
 *   node scripts/testflight.mjs --contact-email me@x.dev --contact-first Ada \
 *                               --contact-last L --contact-phone +48123 \
 *                               --feedback-email me@x.dev --description "…"
 *
 * Credentials: env first (VIS_ASC_KEY_ID / VIS_ASC_ISSUER_ID / VIS_ASC_KEY_PATH), then the
 * macOS login keychain (`npm run secrets asc …`). Never a file in this repo.
 */
import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { appIdFor, asc, ascToken, waitForBuild } from "./asc.mjs";

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");

// "already exists" / "already submitted" are the normal state of a re-run, not a failure.
// A build that is already IN Beta App Review answers "not in a valid processing state" to a
// second submission — same thing, different wording, and equally not an error for a re-run.
const isDuplicate = (err) =>
  err.status === 409 ||
  /already/i.test(err.message) ||
  /not in a valid processing state/i.test(err.message);

/** Every TestFlight tester audience, cheapest first. The App Store itself is not one of them. */
export const TESTFLIGHT_AUDIENCES = ['internal', 'public'];

/**
 * Which testers a build must reach, decided BEFORE anything is built or uploaded.
 *
 * An upload reaches INTERNAL groups by itself — App Store Connect hands them every processed
 * build — while EXTERNAL, public-link testers see nothing until the build is submitted for
 * Beta App Review and LINKED here. Asking for nothing therefore used to mean "team only",
 * which is exactly how the public link sat on build 4042 while the team group already had
 * 4075 and Play served 4090 on every track. Asking for nothing now means EVERY tester
 * audience — the same rule Play follows with internal + alpha + beta — and `--audience
 * internal` is how a build stays inside the team.
 *
 * `internal` is never removable: Apple gives internal groups the build whether or not this
 * release asks for it, so a plan that omitted it would be a lie about what testers can see.
 */
export const planDistribution = ({ audiences, group, review = true } = {}) => {
  const asked = (Array.isArray(audiences) ? audiences : [audiences])
    .flatMap((v) => (v == null ? [] : String(v).split(',')))
    .map((v) => v.trim())
    .filter(Boolean);
  const unknown = asked.find((a) => !TESTFLIGHT_AUDIENCES.includes(a));
  if (unknown) throw new Error(`unknown audience "${unknown}" (${TESTFLIGHT_AUDIENCES.join(' | ')})`);
  // A named group is a public group: there is nothing else to name.
  const isPublic = asked.length ? asked.includes('public') || Boolean(group) : true;
  return {
    audiences: isPublic ? [...TESTFLIGHT_AUDIENCES] : ['internal'],
    isPublic,
    group: group ?? 'Public',
    review: isPublic && review,
  };
};

/**
 * Every beta group that must carry this build, named group FIRST.
 *
 * A tester sees the newest build of the group their invitation belongs to, so a build that only
 * reaches the named group leaves every other invitation serving whatever it last got.
 *
 * An INTERNAL group is never a target: App Store Connect answers `422 Builds cannot be assigned
 * to this internal group` — internal testers get every build automatically — and that refusal
 * failed the whole release job after the build had already shipped to TestFlight.
 */
export const linkTargets = (groups, namedId) => [
  namedId,
  ...groups
    .filter((g) => g.id && g.id !== namedId && !g.attributes?.isInternalGroup)
    .map((g) => g.id),
];

/** Beta App Review refuses a submission when these are unset; they are per-app and set once. */
const ensureBetaMeta = async (
  api,
  appId,
  { locale = "en-US", feedbackEmail, description, contact = {}, log },
) => {
  const missing = [];

  const locs = await api(
    "GET",
    `/v1/apps/${appId}/betaAppLocalizations?limit=50`,
  );
  const mine = locs.data?.find((l) => l.attributes?.locale === locale);
  const wantLoc = {
    ...(feedbackEmail ? { feedbackEmail } : {}),
    ...(description ? { description } : {}),
  };
  if (mine && Object.keys(wantLoc).length) {
    await api("PATCH", `/v1/betaAppLocalizations/${mine.id}`, {
      data: { type: "betaAppLocalizations", id: mine.id, attributes: wantLoc },
    });
    log(`beta localization ${locale} updated`);
  } else if (!mine) {
    if (feedbackEmail) {
      await api("POST", "/v1/betaAppLocalizations", {
        data: {
          type: "betaAppLocalizations",
          attributes: {
            locale,
            feedbackEmail,
            ...(description ? { description } : {}),
          },
          relationships: { app: { data: { type: "apps", id: appId } } },
        },
      });
      log(`beta localization ${locale} created`);
    } else {
      missing.push(
        "beta localization (pass --feedback-email, and ideally --description)",
      );
    }
  }

  // The beta review detail's id IS the app id — it is a to-one child of the app.
  const wantContact = {
    ...(contact.first ? { contactFirstName: contact.first } : {}),
    ...(contact.last ? { contactLastName: contact.last } : {}),
    ...(contact.email ? { contactEmail: contact.email } : {}),
    ...(contact.phone ? { contactPhone: contact.phone } : {}),
    ...(contact.demoRequired === undefined
      ? {}
      : { demoAccountRequired: contact.demoRequired }),
    ...(contact.notes ? { notes: contact.notes } : {}),
  };
  if (Object.keys(wantContact).length) {
    await api("PATCH", `/v1/betaAppReviewDetails/${appId}`, {
      data: {
        type: "betaAppReviewDetails",
        id: appId,
        attributes: wantContact,
      },
    });
    log("beta review contact updated");
  } else {
    const detail = await api("GET", `/v1/apps/${appId}/betaAppReviewDetail`);
    const a = detail.data?.attributes ?? {};
    if (
      !a.contactEmail ||
      !a.contactFirstName ||
      !a.contactLastName ||
      !a.contactPhone
    ) {
      missing.push(
        "beta review contact (pass --contact-first/--contact-last/--contact-email/--contact-phone)",
      );
    }
  }
  return missing;
};

/** The external, public-link group every public tester joins. Created once, reused after. */
const ensurePublicGroup = async (
  api,
  appId,
  name,
  { publicLink = true, limit, log },
) => {
  const groups = await api("GET", `/v1/apps/${appId}/betaGroups?limit=200`);
  let group = groups.data?.find((g) => g.attributes?.name === name);
  if (!group) {
    const created = await api("POST", "/v1/betaGroups", {
      data: {
        type: "betaGroups",
        attributes: {
          name,
          publicLinkEnabled: publicLink,
          publicLinkLimitEnabled: limit !== undefined,
          ...(limit !== undefined ? { publicLinkLimit: limit } : {}),
        },
        relationships: { app: { data: { type: "apps", id: appId } } },
      },
    });
    group = created.data;
    log(`created external beta group "${name}"`);
  } else if (publicLink && !group.attributes?.publicLinkEnabled) {
    const patched = await api("PATCH", `/v1/betaGroups/${group.id}`, {
      data: {
        type: "betaGroups",
        id: group.id,
        attributes: { publicLinkEnabled: true },
      },
    });
    group = patched.data;
    log(`enabled the public link on "${name}"`);
  }
  return group;
};

/**
 * Ship `build` to the public group. Every failure is returned, never thrown: the build is
 * already uploaded by the time this runs, so nothing here may take a release down.
 */
export const distribute = async ({
  keyId,
  issuerId,
  keyPem,
  bundleId,
  build,
  group = "Public",
  publicLink = true,
  publicLinkLimit,
  review = true,
  meta = {},
  timeoutMs = 60 * 60 * 1000,
  log = (m) => console.log(`· ${m}`),
}) => {
  if (!keyId || !issuerId || !keyPem)
    return {
      ok: false,
      reason: "no App Store Connect API key (npm run secrets asc …)",
    };
  const credentials = { keyId, issuerId, keyPem };
  // A token MINT, not a token: `asc` signs a fresh JWT per attempt and retries Apple's
  // transient failures itself, so nothing in this file wraps a request (scripts/asc.mjs).
  const mint = () => ascToken(credentials);
  const api = (method, path, body) => asc(mint, method, path, body);
  try {
    const appId = await appIdFor(mint, bundleId);
    if (!appId)
      return {
        ok: false,
        reason: `no app with bundle id ${bundleId} in this API key's team`,
      };

    // A build must be VALID before it can be reviewed or linked; PROCESSING is rejected.
    // Apple's JWTs expire after 20 minutes, while build ingestion can take much longer.
    const found = await waitForBuild(mint, {
      appId,
      build,
      timeoutMs,
      requireValid: true,
      log,
    });
    if (!found?.id)
      return {
        ok: false,
        reason: `build ${build} not visible in App Store Connect yet`,
      };
    if (found.state !== "VALID")
      return {
        ok: false,
        reason: `build ${build} is still ${found.state} — re-run with --build ${build} once it is VALID`,
      };

    // Apple validates the beta metadata against the app's PRIMARY locale, not en-US: with only an
    // en-US localization on an en-GB app, `betaAppReviewSubmissions` fails with the misleading
    // "betaAppLocalizations not found for this app". Ask the app which locale it wants.
    const primaryLocale = (await api("GET", `/v1/apps/${appId}`))?.data
      ?.attributes?.primaryLocale;
    const missing = await ensureBetaMeta(api, appId, {
      locale: primaryLocale || "en-US",
      ...meta,
      log,
    });
    if (review && missing.length) {
      return {
        ok: false,
        reason: `beta metadata incomplete: ${missing.join("; ")}`,
      };
    }

    const grp = await ensurePublicGroup(api, appId, group, {
      publicLink,
      limit: publicLinkLimit,
      log,
    });

    if (review) {
      try {
        await api("POST", "/v1/betaAppReviewSubmissions", {
          data: {
            type: "betaAppReviewSubmissions",
            relationships: {
              build: { data: { type: "builds", id: found.id } },
            },
          },
        });
        log(
          "submitted for Beta App Review (Apple usually answers within a day)",
        );
      } catch (err) {
        if (!isDuplicate(err)) throw err;
        log("already submitted for Beta App Review");
      }
    }

    // EVERY invitation must serve the SAME build. TestFlight shows a tester the newest build of
    // the group their link belongs to, so linking only the named group leaves every other public
    // link handing out whatever it last got — an old invitation served 0.1.14 (build 2804) while
    // Public was on 0.1.32 (build 3774).
    const everyGroup =
      (await api("GET", `/v1/betaGroups?filter[app]=${appId}&limit=200`))
        ?.data ?? [];
    for (const id of linkTargets(everyGroup, grp.id)) {
      const name = everyGroup.find((g) => g.id === id)?.attributes?.name ?? id;
      // Linking is idempotent server-side, but a duplicate POST still 409s on some paths.
      try {
        await api("POST", `/v1/betaGroups/${id}/relationships/builds`, {
          data: [{ type: "builds", id: found.id }],
        });
        log(`linked build ${build} to "${name}"`);
      } catch (err) {
        if (!isDuplicate(err)) throw err;
        log(`build ${build} was already in "${name}"`);
      }
    }

    const fresh = await api("GET", `/v1/betaGroups/${grp.id}`);
    return {
      ok: true,
      buildId: found.id,
      groupId: grp.id,
      publicLink: fresh.data?.attributes?.publicLink,
    };
  } catch (err) {
    return { ok: false, reason: err.message };
  }
};

// ── standalone CLI ────────────────────────────────────────────────────────────────────

if (
  process.argv[1] &&
  resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))
) {
  const args = process.argv.slice(2);
  const flag = (name) => {
    const i = args.indexOf(`--${name}`);
    return i === -1 ? undefined : args[i + 1];
  };
  const has = (name) => args.includes(`--${name}`);

  // Keychain-first, same rule as ios-release.mjs: env wins so CI can inject, otherwise the
  // macOS login keychain, never a dotfile in the repo.
  const unhex = (s) =>
    /^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0
      ? Buffer.from(s, "hex").toString("utf8")
      : s;
  const keychain = (account) => {
    if (process.platform !== "darwin") return undefined;
    const res = spawnSync(
      "security",
      ["find-generic-password", "-s", "vis-ios", "-a", account, "-w"],
      { encoding: "utf8" },
    );
    return res.status === 0 && res.stdout.trim()
      ? unhex(res.stdout.trim())
      : undefined;
  };
  const secret = (envName, account) =>
    process.env[envName]?.trim() || keychain(account);

  const build =
    flag("build") ??
    spawnSync("git", ["rev-list", "--count", "HEAD"], {
      encoding: "utf8",
      cwd: resolve(appDir, "..", ".."),
    }).stdout.trim();
  const bundleId =
    flag("bundle") ??
    JSON.parse(readFileSync(join(appDir, "capacitor.config.json"), "utf8"))
      .appId;

  const res = await distribute({
    keyId: secret("VIS_ASC_KEY_ID", "asc_key_id"),
    issuerId: secret("VIS_ASC_ISSUER_ID", "asc_issuer_id"),
    keyPem: process.env.VIS_ASC_KEY_PATH
      ? readFileSync(process.env.VIS_ASC_KEY_PATH, "utf8")
      : keychain("asc_key"),
    bundleId,
    build,
    group: flag("group") ?? "Public",
    publicLink: !has("no-public-link"),
    publicLinkLimit: flag("limit") ? Number(flag("limit")) : undefined,
    review: !has("no-review"),
    timeoutMs: Number(flag("timeout") ?? 60 * 60 * 1000),
    meta: {
      feedbackEmail: flag("feedback-email"),
      description: flag("description"),
      contact: {
        first: flag("contact-first"),
        last: flag("contact-last"),
        email: flag("contact-email"),
        phone: flag("contact-phone"),
        notes: flag("review-notes"),
        demoRequired: has("demo-required")
          ? true
          : has("no-demo-required")
            ? false
            : undefined,
      },
    },
  });

  if (!res.ok) {
    console.error(`\n✗ ${res.reason}\n`);
    process.exit(1);
  }
  console.log(`\n✓ build ${build} is on public TestFlight.`);
  console.log(
    res.publicLink
      ? `  Public link: ${res.publicLink}\n`
      : "  Public link appears once Beta App Review approves the build.\n",
  );
}
