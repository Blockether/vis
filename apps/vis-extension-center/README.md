# Vis Extension Center

A separate Cloudflare Worker application backed by D1. It serves complete HTML for
catalog and detail pages; JavaScript enhances filtering, navigation, copying commands
and repository submission. No Vis gateway, Python web server or browser database keys.

## Vis light, shared with the docs

`build.mjs` copies the canonical [documentation stylesheet](../../resources/vis-docs/assets/theme.css),
font files and Blockether mark byte-for-byte into `dist/assets/`. There is no alternate
theme or font dependency. `web/style.css` adds catalog controls, not another page theme.
The Worker and browser use the same HTML renderer in `web/render.js`.

The public documentation can link this separate app through the repository variable
`VIS_EXTENSION_CENTER_URL`. Set it to the deployed HTTPS Worker URL, then rebuild the
docs site. That link is public-site-only: the application is neither a `doc()` entry
nor a page served by the embedded documentation. There is no invented deployment URL.

## Local development

Requires Node 22.12+ and npm. From this directory:

```sh
npm ci
npm run db:init
npm run dev
```

Open <http://127.0.0.1:4178>. Wrangler runs the real Worker runtime and a local D1
instance under `.wrangler/`. The catalog starts empty. These commands never create
Cloudflare resources or deploy the app. `npm run build` rebuilds generated assets.

Submissions fail closed until Turnstile is configured. Supply a public
`TURNSTILE_SITE_KEY` and a server-only `TURNSTILE_SECRET_KEY` in ignored `.dev.vars`.
Use a widget configured for the serving hostname. The Worker verifies success,
hostname and action on both steps. Test keys alone are not proof of that protection;
tests replace the verification HTTP response, including failure and replay cases.
Optional server-only `GITHUB_TOKEN` raises the GitHub API quota. Never put either secret
in assets, source control, screenshots or client configuration.

## Repository submission and moderation

1. Keep `pyproject.toml` and `extension.py` together in a public GitHub repository.
   Follow the [project contract](../../resources/vis-docs/extending.md#extension-center-projects).
2. Choose **Add a repository**, enter `https://github.com/owner/repository`, and leave
   **Project folder** empty for root or provide a folder such as `extensions/greeting`.
3. Complete the anti-spam check and choose **Review repository**.
4. Review the manifest and linked source at the resolved commit. Complete the second
   anti-spam check and choose **Submit for review**. Retain the submission reference.
5. A maintainer reviews the pending entry before approving it. Nothing publishes automatically.

The Worker reads metadata only from `api.github.com`, rejects redirects, enforces
body/response/time limits and pins all source requests to a full commit SHA. It never
fetches archives, executes code or trusts caller-supplied catalog metadata. Display-field
validation is deliberately not a second Python packaging implementation: the SDK remains
authoritative for PEP 440/508, runtime compatibility and installation. Moderators should
check the manifest with that SDK before approval; listing is not an endorsement.

Only the `extensions` table is public. Pending entries live in `submissions`, without
an anonymous read API. A repository/folder pair identifies a listing; repeated submissions
of the same commit are idempotent. Updates also require approval and preserve the original
addition date. GitHub stars, license and dates are checked snapshots, not live counters.

Moderation uses Cloudflare account authentication through Wrangler, not a public admin
endpoint or a browser token. These commands target the **local** database by default:

```sh
npm run moderate -- list
npm run moderate -- approve SUBMISSION_ID
npm run moderate -- reject SUBMISSION_ID
```

Replace `SUBMISSION_ID` with the full 24-character reference. Review before approving
or rejecting. Append `--remote` only when intentionally moderating the deployed catalog.
Approval writes the public entry before deleting the pending one; interruption leaves an
idempotently repeatable approval, never deletes an unapproved entry. Catalog cache may
show the previous snapshot for up to 60 seconds. There is no automatic refresh job.

## Production deployment through GitHub Actions

`.github/workflows/extension-center.yml` verifies pull requests and deploys pushes to
`main` when this app, shared documentation assets or the workflow changes. Manual dispatch
is also available. Deployments are serialized and never cancelled midway. Dependency
installation and tests receive no deployment credentials; pull requests cannot deploy.

Configure a GitHub environment named `extension-center`, restricted to the `main` branch:

- Secrets: `CLOUDFLARE_API_TOKEN` (scoped to the target account, Workers and D1 operations)
  and `CLOUDFLARE_ACCOUNT_ID`. Do not reuse an unrelated service's credentials.
- Variables: `CENTER_D1_DATABASE_ID`, `CENTER_TURNSTILE_SITE_KEY`, and a unique
  `CENTER_RATE_LIMIT_NAMESPACE`. Keep the actual account/resource configuration out of
  this public repository. `deploy.mjs` creates an ignored configuration containing only
  explicitly allowed public values; missing values or Turnstile test keys fail deployment.
- Create D1 named `vis-extension-center` separately. CI applies the idempotent `schema.sql`
  before deploying; it never creates or deletes a database. Schema changes require review.
- Configure Turnstile for the final hostname and provision `TURNSTILE_SECRET_KEY` directly
  as a Worker secret. Optional `GITHUB_TOKEN` also belongs only in Worker secrets. CI checks
  that the Turnstile secret exists, without reading its value, before deploying.
- Set repository variable `VIS_EXTENSION_CENTER_URL` to the actual HTTPS URL. CI checks
  the deployed HTML and public catalog at that URL. Dispatch the docs workflow after first
  deployment to publish the navigation link. The docs workflow itself does not deploy Workers.

Keep the account on Workers Free if a hard usage ceiling is required; quotas are shared.
The supplied rate-limit binding permits 10 write attempts per minute per IP per Cloudflare
location, not a global quota. Missing production configuration fails rather than silently
reporting a successful deployment. Never commit `.dev.vars`, `.env` or `.deployment.json`.

D1 is reachable only through a Worker binding. Public reads share one cached catalog
snapshot for 60 seconds regardless of filters; detail lookups use the primary key.
Static assets bypass the Worker. Writes require same-origin JSON, bounded inputs,
Turnstile and a rate-limit check. Search and sorting do not produce new database scans.

The current Free allowances are 100,000 Worker requests/day, 10 ms CPU/request,
5 million D1 rows read/day, 100,000 written/day, and 500 MB/database (5 GB/account).
D1 stops queries at Free quotas rather than charging for overages. Large catalogs or
manifests still need live CPU/usage monitoring; local tests do not prove hosted capacity.
See [Workers pricing](https://developers.cloudflare.com/workers/platform/pricing/),
[D1 pricing](https://developers.cloudflare.com/d1/platform/pricing/),
[D1 limits](https://developers.cloudflare.com/d1/platform/limits/) and
[Turnstile plans](https://developers.cloudflare.com/turnstile/plans/).

## Installation and verification

GitHub installs require Git on PATH, explicit `--trust`, and use the reviewed revision.
Local projects are linked; Vis prepares dependencies on startup or reload.

```sh
vis-agent extension install ./examples/vis-greeter --project --trust
npm test
npm run lint
npm run build
npx wrangler deploy --dry-run
```

Tests run the actual Worker and local D1 with only GitHub/Turnstile HTTP replaced. They
cover SSR, cache, pinned previews, moderation isolation, duplicate submissions, anti-spam
failures, rate limits, input bounds and metadata escaping. UI tests cover filtering,
routing, keyboard/mobile navigation and asynchronous form behavior. The SDK, JVM and
native installation suites remain the authority for executing installed extensions.

`web/catalog.fixture.json` contains test/review examples, not live repositories. Preview
artifacts use the real Worker-rendered HTML and built assets with labeled API fixtures.
The pinned `sharp` override fixes the image-decoder advisory in Wrangler's local tooling;
no image decoding library is included in the production Worker.
