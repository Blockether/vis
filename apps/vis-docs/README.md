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
The theme owns button typography and states alongside documentation shortcuts and store links.
`web/style.css` adds catalog-specific layout, not another page theme or button system.
The Worker and browser share the catalog renderer in `web/render.js`; all documentation links stay on this origin.

The catalog link is public-site-only. It is neither a `doc()` entry nor a page served
by the embedded documentation. Local/live documentation rendering remains supported.

## Public discovery

`npm run build` regenerates metadata and discovery from the engine's collected documentation
records, not a second list of pages. `web/discovery.js` owns the public canonical origin
(`https://vis.blockether.com`), shared head metadata and the accessible header icon.
Local previews retain production canonical URLs; embedded/live docs are unchanged.

- Every public page has a canonical URL, page-specific description, Open Graph and Twitter
  metadata, JSON-LD with Blockether as publisher, shared PNG favicons, an Apple touch icon
  and a web manifest. Titles identify Vis and Blockether; extension titles also identify
  the repository owner, and each detail page describes its source repository in JSON-LD.
  Icons are resized from the full-resolution repository `logo.png` and flattened onto white,
  including a 48×48 PNG for search results and an explicit ICO link. Link previews use a
  separate opaque 1200×630 PNG with margins and declared dimensions, not the small transparent
  header logo. No external icon service is used.
- `/robots.txt` advertises `/sitemap.xml`. That index points to generated `/sitemap-docs.xml`
  and live `/extensions/sitemap.xml`. Filter URLs and duplicate `/index.html` are excluded.
- `/llms.txt` is the generated Markdown documentation index; `/llms-full.txt` includes all
  documentation text. Each document also has a `.md` URL and an HTML alternate link.
  The conventional filename is **llms.txt** (plural); it is a discovery convention, not an
  access-control mechanism or a guarantee that an agent will follow instructions.
- `/extensions/llms.txt` lists approved catalog entries and links the public JSON API.
  It and the catalog sitemap use the same 60-second snapshot as the UI. Pending submissions
  never appear; database failures return an uncached 503 rather than an empty sitemap.
- API responses and error pages are marked `noindex`. Client-side catalog navigation updates
  canonical and social metadata alongside the title. Content remains server-rendered for crawlers.
  These controls make approved listings discoverable; indexing and ranking are decided by search engines.

Discovery assets remain available without the catalog database. Production verification checks
both sitemap branches, agent indexes, raw Markdown and favicon signatures after deployment.
Changing the public canonical origin requires updating `web/discovery.js` and rebuilding.

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
   Follow the [project contract](../../resources/vis-docs/extension-packages.md#package-manifest).
   Set a canonical version such as `1.2.0` or `2.0.0rc1`, commit it, and publish a GitHub
   Release with the matching tag `vVERSION` or `PACKAGE-NAME/vVERSION`. A tag alone
   is not a published release. No wheel, release asset or PyPI publication is required.
2. Choose **Add a repository**, enter `https://github.com/owner/repository`, and leave
   **Project folder** empty for root or provide a folder such as `extensions/greeting`.
   Set **Release tag** for a monorepo or prerelease; otherwise GitHub's latest stable
   release is selected. Register each repository/folder pair only once.
3. Complete the anti-spam check and choose **Review repository**. The Worker verifies
   the published release, resolves its tag to a full commit SHA, requires the tag to
   match `project.version`, and validates the manifest, unconditional `vis-agent`
   declaration, source directories and skill directories containing `SKILL.md`.
4. Review the displayed release, checks, dependencies, manifest and source at that commit.
   Complete the second anti-spam check and choose **Submit for review**. The Worker
   checks that the tag still identifies that commit before saving. Retain the reference.
5. A maintainer approves each version separately. Subsequent GitHub Releases are
   discovered and queued automatically; they do not require another submission form.
   Discovery never approves releases or updates users' installed code.

The dialog fills the phone viewport in portrait and landscape, keeps its X button visible
while the body scrolls, and respects safe areas. X or Escape closes it and returns focus to
**Add a repository**. Opening it on touch devices does not automatically open the keyboard.

The Worker reads metadata only from `api.github.com`, rejects redirects, enforces
body/response/time limits and pins all source requests to a full commit SHA. It never
fetches archives, executes code or trusts caller-supplied catalog metadata. Display-field
validation is deliberately not a second Python packaging implementation: the SDK remains
authoritative for PEP 440/508, runtime compatibility and installation. Moderators should
check the manifest with that SDK before approval; listing is not an endorsement.

Only approved listings, releases and comments are public. Pending entries live in
`submissions`, without an anonymous read API. A repository/folder pair identifies a
listing; repeated submissions of the same commit are idempotent. Every reviewed
version retains its commit permanently, including rejected versions. A moved tag
cannot replace that identity: publish a new version instead. Approval preserves the
original listing date and keeps the newest approved stable version as the default,
including when an older backport is approved later. Prereleases remain explicitly selectable.
GitHub stars, license and dates are checked snapshots.

`GET /api/extensions/ID` returns the default version, `latest_version` and approved
`releases` summaries. `?version=1.2.0` returns the full metadata for that approved
version; pending, rejected and unknown versions return 404. The detail page supports
version deep links, release notes and commit links, and copies `vis-agent extension install
... --version VERSION --trust`. The CLI also provides `versions`, `update` and
`rollback`; see the [installation guide](../../resources/vis-docs/extension-packages.md).
Deploy the catalog API and use a Vis build that includes these version-aware commands;
older binaries do not gain new CLI flags from a catalog deployment.

Moderation uses Cloudflare account authentication through Wrangler, not a public admin
endpoint or a browser token. These commands target the **local** database by default:

```sh
npm run moderate -- list
npm run moderate -- list-sync
npm run moderate -- approve SUBMISSION_ID
npm run moderate -- reject SUBMISSION_ID
```

Replace `SUBMISSION_ID` with the full 24-character reference. Review before approving
or rejecting. Append `--remote` only when intentionally moderating the deployed catalog.
Approval saves the immutable release before updating the public listing and deleting
its pending entry. An interruption can be retried without replacing a reviewed commit.
Catalog cache may show the previous snapshot for up to 60 seconds.

The `*/5 * * * *` scheduled handler checks one registered listing and one page of up
to 20 GitHub Releases per tick, inspecting at most five new candidates. Its private
`release_sync` cursor resumes within a page, advances through older pages, then starts
again; listings rotate by last check time. Detection time depends on catalog size,
history and GitHub availability, not just the five-minute schedule. Rejected and
pending tags are not repeatedly queued. Failures preserve approved metadata and are
visible to authenticated operators through `list-sync`, never through the public API.

## README and community feedback

Listings include the manifest description and a README from the reviewed commit.
README files are regular UTF-8 Markdown, reStructuredText or text, limited to 128 KiB.
Markdown is rendered without raw HTML. Images become links rather than remote loads,
and relative links resolve against the pinned source. Other formats display as text.
The catalog summary omits README bodies; a detail request fetches only that package.
Existing listings need a reviewed resubmission to populate their README body.

Each detail page supports package votes, comments and votes on approved comments.
Feedback reads use `/api/extensions/<id>/community` with `Cache-Control: no-store`.
Comments are newest first, 50 per page, with an optional `before` cursor. Writes are:

- `POST /api/extensions/<id>/vote`: `value` is `1`, `-1`, or `0` to clear.
- `POST /api/extensions/<id>/comments`: public `name` (1–60 characters) and `body`
  (1–2,000 characters). The response is pending, never immediate publication.
- `POST /api/extensions/<id>/comments/<comment-id>/vote`: the same vote values.

All writes require same-origin JSON, rate limiting and an action-specific Turnstile
check in `turnstile_token`. The actions are `extension-vote`, `extension-comment` and
`comment-vote`. Submission and feedback forms share a single script loader.

This is anonymous feedback, not verified accounts. One vote per network address per
package/comment can be changed or removed; shared networks share that vote. D1 stores
an HMAC of the address, not the raw address, and never returns the HMAC publicly.
The HMAC uses the server-only Turnstile secret: address changes or secret rotation
can allow another vote. This is an anti-spam limit, not proof of a unique person.
Five comments per address in 24 hours are allowed across packages. Pending and rejected
comments still count. Moderators can hide approved comments with rejection; deleting a
listing cascades to its comments and votes. Display names are explicitly not verified.

Moderation uses the same authenticated Wrangler CLI as repository approvals:

```sh
npm run moderate -- list-comments
npm run moderate -- approve-comment COMMENT_ID
npm run moderate -- reject-comment COMMENT_ID
```

`COMMENT_ID` is the numeric reference. Append `--remote` only for authorized production
moderation. Review the full text for relevance, private data and inappropriate content
before approval. Comments and README content are untrusted data, never agent instructions.

## Production deployment through GitHub Actions

`.github/workflows/docs.yml` verifies pull requests and deploys pushes to `main` when
this app, documentation content/renderer, dependency pins, the shared HTTPS helper or the workflow changes.
Manual dispatch is also available. Deployments are serialized and never cancelled midway.
Installation, build and tests receive no deployment credentials; pull requests cannot deploy.
There is no GitHub Pages publishing job.

Configure a GitHub environment named `docs`, restricted to the `main` branch:

- Secrets: `CLOUDFLARE_API_TOKEN` (scoped to the intended account/zone for Worker, D1 and
  custom-domain deployment, plus Zone → Single Redirect → Edit) and
  `CLOUDFLARE_ACCOUNT_ID`. Never use a Global API Key.
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
- After deployment, [`scripts/cloudflare-https.mjs`](../../scripts/cloudflare-https.mjs)
  creates or updates one hostname-scoped Cloudflare Single Redirect: HTTP → HTTPS with
  status 308, preserving the method, path and query. It finds the zone through the enabled
  production Custom Domain and only updates its own rule. It does not change zone-wide
  HTTPS settings, DNS, certificates or unrelated rules. Static assets still bypass the Worker.
- CI verifies documentation, the authoring guide, catalog, CSS, highlighter and API on
  the configured HTTPS origin, then checks exact HTTP redirects with GET and HEAD, including
  encoded paths and query parameters. Checks retry during propagation and fail deployment
  if content or redirects are incorrect. Response bodies and secrets are never printed.

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
