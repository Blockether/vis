# Vis docs

One Cloudflare Worker serves the public documentation and Extension Center on the same
origin. `/` and `/*.html` are generated documentation assets; `/extensions/` and
`/extensions/<id>` are Worker-rendered catalog pages, and `/api/*` is its API.
JavaScript enhances filtering, navigation, copying commands and repository submission.
There is no Vis gateway, Python web server or browser database credential.

## One renderer and Vis light stylesheet

`build.mjs` invokes the engine's documentation renderer, using its dependency pins and
canonical Markdown resources. The [documentation stylesheet](../../resources/vis-docs/assets/theme.css),
font and highlighter are copied byte-for-byte. Static pages use external CSS and scripts,
so the site can enforce a Content Security Policy without unsafe inline execution.
`web/style.css` adds catalog controls, not another page theme. The Worker and browser
share the catalog renderer in `web/render.js`; all documentation links stay on this origin.

The catalog link is public-site-only. It is neither a `doc()` entry nor a page served
by the embedded documentation. Local/live documentation rendering remains supported.

## Local development

Requires Node 22.12+, npm, Clojure CLI and the repository-pinned GraalVM CE. Prepare the
repository dependencies with `clojure -X:deps prep` at the repository root, then from this directory:

```sh
npm ci
npm run db:init
npm run dev
```

Open <http://127.0.0.1:4178> for docs and <http://127.0.0.1:4178/extensions/> for
the catalog. Wrangler runs the real Worker runtime and a local D1 instance under
`.wrangler/`. The catalog starts empty. These commands never create Cloudflare resources
or deploy the app. `npm run build` rebuilds both documentation and catalog assets.

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

`.github/workflows/docs.yml` verifies pull requests and deploys pushes to `main` when
this app, documentation content/renderer, dependency pins or the workflow changes.
Manual dispatch is also available. Deployments are serialized and never cancelled midway.
Installation, build and tests receive no deployment credentials; pull requests cannot deploy.
There is no GitHub Pages publishing job.

Configure a GitHub environment named `docs`, restricted to the `main` branch:

- Secrets: `CLOUDFLARE_API_TOKEN` (scoped to the intended account/zone for Worker, D1 and
  custom-domain deployment) and `CLOUDFLARE_ACCOUNT_ID`. Never use a Global API Key.
- Variables: `DOCS_HOSTNAME` (hostname only, such as `gateway.example.com`),
  `DOCS_D1_DATABASE_ID`, `DOCS_TURNSTILE_SITE_KEY`, and `DOCS_RATE_LIMIT_NAMESPACE`.
  Use the existing catalog database ID and limiter namespace when replacing that app.
  Actual account/resource configuration stays out of this public repository.
- Create the `vis-docs` Worker before the first deployment and provision
  `TURNSTILE_SECRET_KEY` directly as its secret. Optional `GITHUB_TOKEN` also belongs
  only in Worker secrets. A new Worker does not inherit the previous Worker's secrets.
  CI checks secret names without reading their values.
- Add the final hostname to the production Turnstile widget. Keep the previous hostname
  allowed while the old app is still serving users.
- Keep the existing D1 named `vis-extension-center`; renaming the application does not
  create or replace its database. CI applies idempotent `schema.sql`, never creates or
  deletes D1. Schema changes require review.
- The domain must belong to an active Cloudflare zone. `deploy.mjs` generates an ignored
  configuration with a Worker Custom Domain, which manages DNS and the exact hostname's
  certificate. It disables the new Worker's `workers.dev` and preview URLs. Do not add
  a competing CNAME manually. Missing values and Turnstile test keys fail deployment.
- CI verifies documentation, the authoring guide, catalog, CSS, highlighter and API on
  the configured HTTPS origin. Response bodies and secrets are never printed.

For a registrar-managed DNS zone, import and verify all existing records before changing
nameservers. Disable old DNSSEC and verify old DS records have cleared first; re-enable
DNSSEC with the new provider after activation. This affects the entire domain, including
email and unrelated services. Keep those existing records DNS-only during the DNS move.

Test the new site and update public links before unpublishing the old GitHub Pages site
or retiring the previous catalog Worker. Those external cutover actions are separate from
this code change; the workflow does not delete either service or change registrar settings.
The relay remains a separate Worker with its own Custom Domain and deployment lifecycle.

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

Tests run the actual Worker and local D1 with GitHub/Turnstile HTTP fixtures. A separate
Wrangler harness loads the production routing, static assets and headers, including the
home-page rewrite and documentation availability when the catalog database has no schema.
Tests validate generated links/assets, shared CSS, CSP, SSR, cache, pinned previews,
moderation, input bounds and metadata escaping. UI tests cover same-origin routing,
keyboard/mobile navigation and asynchronous forms. SDK, JVM and native installation
suites remain the authority for executing installed extensions.

`web/catalog.fixture.json` contains test/review examples, not live repositories. Preview
artifacts use the real Worker-rendered HTML and built assets with labeled API fixtures.
The pinned `sharp` override fixes the image-decoder advisory in Wrangler's local tooling;
no image decoding library is included in the production Worker.
