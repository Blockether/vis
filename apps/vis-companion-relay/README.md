# vis-companion-relay

A Cloudflare Worker that lets a gateway **you** run wake a phone running a
companion **somebody else** signed — without ever holding their signing key.

APNs binds a topic to the Apple team that owns it. A key minted by anyone else,
aimed at someone else's bundle id, is refused forever (`403
InvalidProviderToken` / `TopicDisallowed`). So the key has to live on
infrastructure the app's publisher runs, and every gateway gets a **capability**
instead of a credential:

```
app     -> POST /v1/grants  {device_token}   => an opaque, sealed grant
app     -> hands that grant to a gateway on "notify this device"
gateway -> POST /v1/push    Bearer <grant>   => the relay signs and sends
```

## It stores nothing

There is **no database**. No D1, no KV, no Durable Object, no cron, no queue.

A grant is not a row key — it *is* the record, AES-256-GCM sealed under a key
that exists only as a Worker secret (`src/seal.ts`):

```
vg1.<base64url( iv(12) || AES-GCM({device token, platform, environment, expiry}) )>
```

That one decision is most of the security story:

- **Nothing is at rest to steal.** The old design kept a row per grant — every
  user's push token, in one file, forever. Deleting the table deleted the
  breach. A dump of this service's storage is a dump of nothing.
- **A grant cannot be forged or retargeted.** GCM authenticates every byte, and
  `additionalData` pins the format version; you cannot edit the device token,
  the platform, or the expiry inside one.
- **The gateway still cannot read the device token.** It holds ciphertext, not
  an encoding.
- **A grant expires by itself** (`GRANT_TTL_DAYS`, default 90) because the
  expiry travels *inside* it. No calendar, no sweeper, no list of anybody.
- **Nothing accumulates**, so signing up a million times exhausts no storage
  quota and there is no table an attacker can make expensive.

What that costs: revoking one grant needed the row. Revocation is now expiry —
and, for *everything, now*, rotating `RELAY_SEAL_KEY`.

## Routes

| route | who calls it | answer |
| --- | --- | --- |
| `GET /healthz` | anyone | provider availability, topic, `is_accepting_grants` |
| `POST /v1/grants` | the app | `201 {grant, relay_url, platform, environment, expires_at}` |
| `POST /v1/push` | a gateway, `Authorization: Bearer <grant>` | `200 {is_delivered:true}` |

Push verdicts a gateway acts on: `404` the grant is forged, expired, or sealed
under a rotated-away key; `410` the provider says that device is gone — both
mean *forget this device*. `429` over a limit, `502` the provider failed,
`503` this relay cannot sign for that platform.

```bash
curl -sS -X POST "$RELAY/v1/grants" -H 'content-type: application/json' \
  -d '{"device_token":"<64 hex from APNs>","platform":"ios"}'

curl -sS -X POST "$RELAY/v1/push" -H "authorization: Bearer $GRANT" \
  -H 'content-type: application/json' \
  -d '{"title":"vis","body":"needs your input","data":{"session_id":"abc"}}'
```

## What an unwelcome caller costs

Every route is public — nobody authenticates to *ask* for a grant — so the
question is never "is this caller allowed" but "what does this cost me".

| a stranger can | and it costs |
| --- | --- |
| flood `/v1/push` with invented grants | `PUSH_ADDRESS_LIMIT` (60/min per address) is checked **before** the body is read and before a byte is decrypted. The counter lives at the Cloudflare edge: a refusal performs no storage operation at all |
| guess a grant | AES-256-GCM. There is nothing to guess and nothing to look up |
| mint junk grants | `MINT_LIMIT`, 5/min per address — and a grant is a string the relay immediately forgets, so junk grants occupy nothing |
| mint many grants for one phone | nothing: `PUSH_DEVICE_LIMIT` is keyed by a **hash of the device token**, so all the grants for one phone share one 20/min budget |
| POST a 100 MB body | `413` from `content-length` before parsing, and again on what actually arrived (a chunked body declares no length) |
| stuff a payload | 16 KiB per request (`MAX_REQUEST_BYTES`, which may only *tighten* it), 4 KiB per field, ≤32 data keys — and the **provider's** 4 KiB is measured here, before the round trip: a too-long preview is trimmed (`is_truncated`), a too-long `data` map is `413 payload_too_large` |
| put `../` or a URL in a device token | refused at mint time by platform-specific alphabets (Apple: hex; Google: url-safe base64 + `:`), and the APNs path is `encodeURIComponent`-escaped anyway |
| make the relay hang on a provider | every provider call carries a 10 s `AbortSignal.timeout` |
| steal a grant off a gateway | pushes to that one phone, ≤20/min, until it expires. A stolen `.p8` is none of those things |

**Volumetric DDoS is Cloudflare's problem, not yours** — unmetered DDoS
mitigation is on the free plan. What Cloudflare will *not* do for you is the
size of a single request: the body limit belongs to your **account plan** (100
MB on Free) and no setting lowers it, and a `workers.dev` subdomain is not a
zone, so WAF custom rules and rate limiting rules never run in front of the
Worker. Every cap in the table above is therefore enforced by `src/index.ts`
itself, before the body is pulled. Put the relay on a **custom domain** and the
dashboard rules do apply — a WAF custom rule on `http.request.body.size` then
refuses a flood without invoking the Worker at all. If you move to paid
Workers, set a **spend limit**; on the free plan the worst case is a degraded
relay for a day, and it cannot become a bill.

What a stranger cannot obtain at any volume: a device token (never returned by
any route), an alert body (encrypt it app-side and even the relay operator
cannot read it), a session, or your signing key.

## Deploy

```bash
npm install
npx wrangler secret put RELAY_SEAL_KEY        # openssl rand -base64 32
npx wrangler secret put APNS_KEY_P8           # the AuthKey_XXXX.p8, whole file
npx wrangler secret put FCM_SERVICE_ACCOUNT   # the service-account JSON
npm run deploy
```

Public configuration lives in `wrangler.jsonc` `vars`: `APNS_KEY_ID`,
`APNS_TEAM_ID`, `APNS_TOPIC`, `APNS_DEFAULT_ENV`, `GRANT_TTL_DAYS`,
`MAX_REQUEST_BYTES`. Set the Apple three for iOS, `FCM_SERVICE_ACCOUNT` for
Android; either alone is fine — `/healthz` reports which are live, and under
`limits` the caps this relay enforces.

Then point a gateway at it:

```bash
export VIS_PUSH_RELAY_URL=https://push.example.com
```

The companion needs no configuration for this: a gateway publishes its relay as
`push.relay.url`, and the app mints its grant at whatever address that machine
names — over `https` only, and never at an address baked into the app. So running
your own relay is a deploy and one environment variable, and it serves the
companion build whose signing key you put in it.

`npm run dev` deliberately uses `--remote`: local workerd has no HTTP/2 and APNs
will fail there with code that works deployed.

### Rotating the seal

```bash
npx wrangler secret put RELAY_SEAL_KEY_PREVIOUS   # the current value
npx wrangler secret put RELAY_SEAL_KEY            # a new one
```

Both keys open a grant; only the first seals a new one. Delete
`RELAY_SEAL_KEY_PREVIOUS` once every app has re-registered — or immediately, if
what you want is to invalidate every grant in existence at once.

## Continuous deployment

`.github/workflows/relay.yml` runs on any commit touching
`apps/vis-companion-relay/**` — and on no other commit.

1. **verify** (also on PRs): `npm ci`, `npm run typecheck`, `npm test`.
2. **deploy** (main only): stands down with a `::notice` unless
   `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` are set, then
   `wrangler deploy` with the `vars` below, then `curl /healthz` (with retries)
   and a check that the deployed Worker is really accepting grants and can sign
   for at least one provider.

| where | name |
| --- | --- |
| secret | `CLOUDFLARE_API_TOKEN` (Workers Scripts:Edit), `CLOUDFLARE_ACCOUNT_ID` |
| variable | `APNS_KEY_ID`, `APNS_TEAM_ID`, `APNS_TOPIC`, `APNS_DEFAULT_ENV`, `GRANT_TTL_DAYS`, `RELAY_HEALTHCHECK_URL` |

CI never sees key material: `wrangler deploy` does not touch a Worker's secrets,
so `RELAY_SEAL_KEY`, `APNS_KEY_P8` and `FCM_SERVICE_ACCOUNT` stay in Cloudflare
and out of GitHub.

## Tests

```bash
npm run typecheck
npm test
```

The suite drives the real router with a fake provider `fetch` and fake rate
limiters, and verifies real ES256/RS256 signatures with WebCrypto: no network,
no Cloudflare account, no emulator.
