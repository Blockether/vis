# vis-companion-relay

The smallest piece of infrastructure that lets a gateway **you do not run** wake a
phone running a companion **you signed**.

APNs binds a topic to the Apple team that owns it. A key minted by anyone else,
aimed at someone else's bundle id, is refused forever (`403 InvalidProviderToken`
/ `TopicDisallowed`); FCM answers `403 SENDER_ID_MISMATCH` to the same mistake.
So the signing key can only live on infrastructure the app's publisher controls —
and a self-hosted gateway must be handed a *capability* instead of a *credential*.

That capability is a **grant**.

```
app     -> POST   /v1/grants        {device_token}   => an opaque grant
app     -> hands the grant to a gateway on "notify this device"
gateway -> POST   /v1/push          Bearer <grant>   => the relay signs and sends
app     -> DELETE /v1/grants/<grant>                 => revoked, alone
```

What that buys, versus copying a `.p8` onto every self-hoster's box:

- **No gateway holds a signing key.** Leaking one gateway leaks one grant.
- **Revocation is per holder.** Deleting a grant mutes exactly one gateway; revoking
  an APNs key mutes push for *everyone* until every box is re-keyed.
- **The gateway never learns the device token**, so a gateway you do not trust
  cannot fingerprint — or resell — the device it notifies.
- Encrypt the alert body app-side (`is_mutable` + a Notification Service Extension)
  and the relay learns *when*, never *what*.

The relay holds no user account, no session, no transcript, and no gateway
credential. If it is down for an hour, alerts are late; nothing else breaks.

## Routes

| route | who calls it | body | answers |
| --- | --- | --- | --- |
| `GET /healthz` | anyone | — | `200` provider availability, topic, project id |
| `POST /v1/grants` | the app | `{device_token, platform?, environment?, label?}` | `201 {grant, relay_url, platform, environment, created_at}` |
| `POST /v1/push` | a gateway | `Authorization: Bearer <grant>` + `{title, body, data?, thread_id?, collapse_id?, badge?, is_mutable?}` | `200 {is_delivered:true, environment}` |
| `DELETE /v1/grants/<grant>` | the app | — | `200 {is_revoked}` |

`platform` is `ios`, `ipados` or `android`. A grant is a bearer secret: it is stored
as `sha256(grant)`, so a dump of the database cannot push anything.

Failure verdicts a gateway must act on:

| status | meaning | gateway should |
| --- | --- | --- |
| `404 unknown_grant` | revoked, or never existed | forget the device |
| `410` (`is_revoked: true`) | the provider says the token is dead — the relay already deleted the grant | forget the device |
| `429 rate_limited` | over quota, `reset_at` says when | back off |
| `502` | the provider refused this one send | retry later |
| `503 provider_unconfigured` | this relay has no key for that platform | report unavailable |

APNs `410`/`BadDeviceToken`/`Unregistered` and FCM `UNREGISTERED`/`INVALID_ARGUMENT`
delete the grant server-side, so a dead device stops costing pushes without anyone
cleaning up. An APNs send that misses on one environment is retried once against the
other, and the grant remembers which one worked.

## Deploy (Cloudflare Workers free plan — $0, no card)

Workers is the runtime chosen because **APNs requires HTTP/2** and the Workers
fetch stack speaks it; most free runtimes do not. Free plan: 100k requests/day,
D1 5M row reads and 100k row writes/day. One push is one request and one row read.

```bash
cd apps/vis-companion-relay
npm install
npx wrangler d1 create vis-companion-relay   # paste the id into wrangler.jsonc
npm run db:apply                             # create the tables
npx wrangler secret put APNS_KEY_P8          # the .p8 PEM, whole file
npx wrangler secret put FCM_SERVICE_ACCOUNT  # the service-account JSON
npm run deploy
```

Public configuration lives in `wrangler.jsonc` `vars` (`APNS_KEY_ID`,
`APNS_TEAM_ID`, `APNS_TOPIC`, `APNS_DEFAULT_ENV`, and the quota knobs). **Key
material is a secret, never a var.** Put production on a custom domain — a
`workers.dev` subdomain is documented by Cloudflare as hobby-grade.

> `wrangler dev` runs workerd locally, which has **no HTTP/2**, so an APNs call
> fails there with the identical code that works when deployed
> (`cloudflare/workerd#4841`). `npm run dev` therefore uses `--remote`.

D1 is used for grants rather than KV on purpose: free KV allows 1,000 writes/day,
which breaks at 1,000 registrations.

## Continuous deployment

`.github/workflows/relay.yml` typechecks, tests, and — on `main` — deploys this
Worker on **every commit that touches `apps/vis-companion-relay/**`**, and on no
other commit. The Worker's source is its deployment; a relay commit that has not
shipped is a lie.

Configure it once, in GitHub, so nothing deployment-specific is committed:

| where | name | what |
| --- | --- | --- |
| secret | `CLOUDFLARE_API_TOKEN` | Workers Scripts:Edit + D1:Edit |
| secret | `CLOUDFLARE_ACCOUNT_ID` | |
| secret | `CLOUDFLARE_D1_DATABASE_ID` | id from `wrangler d1 create` |
| variable | `APNS_KEY_ID`, `APNS_TEAM_ID`, `APNS_TOPIC` | omit all three for FCM-only |
| variable | `APNS_DEFAULT_ENV` | `production` (default) or `sandbox` |
| variable | `RELAY_HEALTHCHECK_URL` | optional `<relay>/healthz`, curled after deploy |

**CI never sees key material.** `APNS_KEY_P8` and `FCM_SERVICE_ACCOUNT` are set
once with `wrangler secret put` and live only in Cloudflare; `wrangler deploy`
leaves a Worker's secrets untouched. The run stamps the D1 id into the checkout's
`wrangler.jsonc`, re-applies `schema.sql` (idempotent, so a fresh account needs no
manual step), deploys, and curls `/healthz`. Without the three Cloudflare values
the job verifies and stands down with a notice, so a fork stays green.

## Abuse, quotas and cost

Every route is public — nobody authenticates to *ask* for a grant, and a gateway
only ever proves it already holds one. So the design question is not who may
call, it is what an unwelcome caller costs.

| a stranger can | and it costs |
|---|---|
| flood `/v1/push` with invented grants | `IP_PUSH_RATE_LIMIT` (600/h) per address, then `429` **without** a grant lookup and **without** a counter write |
| guess a grant | 256 bits of entropy, stored as `sha256(grant)`; unguessable, and a database dump contains no usable capability |
| mint grants for junk device tokens | `GRANT_RATE_LIMIT` (30/h) per address, ≤ `MAX_GRANTS_PER_DEVICE` rows per token, and the hourly cron deletes any grant never pushed to within `UNUSED_GRANT_TTL_MS` (30 days) |
| POST a huge body | refused `413` on `content-length` before parsing, and again on what actually arrived |
| stuff a payload | title/body/each value capped at 4 KiB, at most 32 custom data keys |
| steal a grant off a gateway | pushes to that one phone, ≤ `PUSH_RATE_LIMIT` (120/h), until the owner deletes it — a stolen `.p8` is none of those things |

What a stranger **cannot** get at any volume: a device token (never returned),
an alert body (encrypt it app-side and the relay cannot read it either), a
session, or your signing key.

Volumetric DDoS is Cloudflare's problem, not yours — the Worker is behind their
edge, and unmetered DDoS mitigation is on the free plan. What is *your* problem
is an application-layer flood spending the daily budget: Workers requests, and
D1 rows read/written. Every route above answers a refused request from one
indexed read and no write, so a flood is throttled at the edge of your own
quota rather than inside it. Two things worth doing anyway:

- Add a WAF **rate-limiting rule** on `/v1/*` (per IP, e.g. 20 requests / 10 s).
  It stops the flood before it reaches the Worker at all, which the in-Worker
  quota by definition cannot.
- If you ever move to the paid Workers plan, set a **spend limit**. On the free
  plan a flood degrades the relay for a day; it cannot generate a bill.

The knobs are plain `vars` in `wrangler.jsonc` — tighten them and redeploy; the
cron (`triggers.crons`) runs the sweep hourly and needs no configuration.

## Pointing a gateway at it

```bash
export VIS_PUSH_RELAY_URL=https://push.example.com
# or, permanently:
echo '{:url "https://push.example.com"}' > ~/.vis/relay.edn
```

Then register the device with a `grant` instead of a `token`
(`POST /v1/devices`). `GET /v1/devices` reports `push.relay`. A gateway that has
its own APNs/FCM credentials keeps using them; the relay is only consulted for
devices registered with a grant. See `src/com/blockether/vis/internal/gateway/relay.clj`
and `resources/vis-docs/gateway.md`.

## Tests

```bash
npm run typecheck
npm test
```

`test/relay.test.ts` runs the real router against a `node:sqlite` D1 shim and a fake
provider `fetch`, so grant lifecycle, quota, dead-token cleanup and JWT shape are
covered without touching Apple, Google, or Cloudflare.
