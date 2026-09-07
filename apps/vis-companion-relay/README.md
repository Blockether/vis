# vis-companion-relay

A Cloudflare Worker that delivers push notifications for Companion gateways.
The publisher keeps APNs and FCM credentials in the Worker. Each gateway uses
a delivery grant instead of receiving those credentials.

APNs credentials must match the app's Apple team and topic. A gateway cannot
use an unrelated team's key for the store-distributed app:

```
app     -> POST /v1/grants  {device_token}   => encrypted delivery grant
app     -> registers the grant with its paired gateway
gateway -> POST /v1/push    Bearer <grant>   => relay authenticates and sends
```

## Grant storage

The relay has no device-token database. It uses no D1, KV, Durable Object, cron
or queue. Each grant contains the device token, platform, environment and
expiry, encrypted with AES-256-GCM using a Worker secret (`src/seal.ts`):

```
vg1.<base64url( iv(12) || AES-GCM({device token, platform, environment, expiry}) )>
```

- GCM authenticates the encrypted fields; `additionalData` authenticates the
  format version. Modified grants are rejected.
- Gateways receive the encrypted grant, not the device token.
- Grants contain their expiry (`GRANT_TTL_DAYS`, default 90), so the relay can
  validate expiry without stored records or a cleanup job.
- Individual grants cannot be revoked. They expire, or the operator can rotate
  `RELAY_SEAL_KEY` to invalidate grants encrypted with the old key.

## No OAuth callbacks

This service delivers notifications only. MCP and model-provider sign-in must not
send authorization codes, state, PKCE verifiers or provider tokens through it.
Callbacks return to the initiating client and go directly to its paired gateway;
device authorization talks directly to the provider. There is no shared HTTPS
callback or browser fallback here. OAuth paths are ordinary unknown routes (404).

## Routes

| route | who calls it | answer |
| --- | --- | --- |
| `GET /healthz` | anyone | provider availability, topic, `is_accepting_grants` |
| `POST /v1/grants` | the app | `201 {grant, relay_url, platform, environment, expires_at}` |
| `POST /v1/push` | a gateway, `Authorization: Bearer <grant>` | `200 {is_delivered:true}` |

Gateways remove device registrations after `404` (invalid or expired grant)
or `410` (provider reports an unavailable device). `429` indicates a rate
limit, `502` a provider failure, and `503` missing relay credentials for the
requested platform.

```bash
curl -sS -X POST "$RELAY/v1/grants" -H 'content-type: application/json' \
  -d '{"device_token":"<64 hex from APNs>","platform":"ios"}'

curl -sS -X POST "$RELAY/v1/push" -H "authorization: Bearer $GRANT" \
  -H 'content-type: application/json' \
  -d '{"title":"vis","body":"needs your input","data":{"session_id":"abc"}}'
```

## Request limits

Grant creation is public. Push delivery requires a valid grant. The Worker
applies these limits:

| Check | Limit or behavior |
| --- | --- |
| Pushes per client address | `PUSH_ADDRESS_LIMIT`: 60/min, checked before reading or decrypting the body |
| Grant creation | `MINT_LIMIT`: 5/min per address |
| Pushes per device | `PUSH_DEVICE_LIMIT`: 20/min, keyed by device-token hash across all grants |
| Request body | 16 KiB maximum; `MAX_REQUEST_BYTES` may lower it |
| Payload fields | 4 KiB per field and at most 32 data keys |
| Provider payload | 4 KiB; long previews are shortened with `is_truncated`, oversized data returns `413 payload_too_large` |
| Device tokens | Platform-specific format validation at grant creation |
| Provider requests | 10-second timeout |

Cloudflare's infrastructure protections do not replace application limits.
On `workers.dev`, the Worker enforces body limits itself. A custom domain can
also use WAF rules to reject oversized requests before Worker execution.
Review Cloudflare's current limits and pricing before deployment and configure
a spending limit where available.

Treat grants as credentials: anyone holding one can send notifications to its
device until expiry or key rotation. The relay does not expose sessions or
return device tokens. It does receive notification titles, bodies and data
when forwarding pushes; those payloads are not end-to-end encrypted.

## Deploy

```bash
npm install
npx wrangler secret put RELAY_SEAL_KEY        # openssl rand -base64 32
npx wrangler secret put APNS_KEY_P8           # the AuthKey_XXXX.p8, whole file
npx wrangler secret put FCM_SERVICE_ACCOUNT   # the service-account JSON
npm run deploy
```

Public configuration is in `wrangler.jsonc` under `vars`: `APNS_KEY_ID`,
`APNS_TEAM_ID`, `APNS_TOPIC`, `APNS_DEFAULT_ENV`, `GRANT_TTL_DAYS` and
`MAX_REQUEST_BYTES`. Configure APNs for iOS, FCM for Android, or both.
`/healthz` reports provider availability and active limits.

`FCM_SERVICE_ACCOUNT` must contain the service-account JSON. Invalid JSON makes
`fcm.is_available` false in `/healthz`; check that field after configuration.
If macOS Keychain returns a hex-encoded value, decode it before passing it to
Wrangler:

```bash
security find-generic-password -s vis-fcm -a service_account -w \
  | python3 -c "import sys; sys.stdout.write(bytes.fromhex(sys.stdin.read().strip()).decode())" \
  | npx wrangler secret put FCM_SERVICE_ACCOUNT
```

Then point a gateway at it:

```bash
export VIS_PUSH_RELAY_URL=https://push.example.com
```

The store app includes its publisher's relay URL. It obtains a grant there
and registers `{grant, relay_url}` with the paired gateway using HTTPS.
Gateways also use the publisher's relay by default; `VIS_PUSH_RELAY_URL`
overrides that choice. A self-hosted relay needs credentials for the app build
it serves and a matching relay URL in that build.

`npm run dev` uses `--remote` because local workerd lacks the HTTP/2 support
required by APNs.

### Rotating encryption keys

```bash
npx wrangler secret put RELAY_SEAL_KEY_PREVIOUS   # the current value
npx wrangler secret put RELAY_SEAL_KEY            # a new one
```

The relay accepts grants encrypted with either key and creates new grants
with `RELAY_SEAL_KEY`. Remove `RELAY_SEAL_KEY_PREVIOUS` after devices
re-register, or immediately to invalidate grants using that key.

## Continuous deployment

`.github/workflows/relay.yml` runs on any commit touching
`apps/vis-companion-relay/**` — and on no other commit.

1. **verify** (also on PRs): `npm ci`, `npm run typecheck`, `npm test`.
2. **deploy** (main only): skips with a `::notice` unless
   `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` are configured. It runs
   `wrangler deploy`, then checks `/healthz` to confirm grant acceptance and
   credentials for at least one provider.

| where | name |
| --- | --- |
| secret | `CLOUDFLARE_API_TOKEN` (Workers Scripts:Edit), `CLOUDFLARE_ACCOUNT_ID` |
| variable | `APNS_KEY_ID`, `APNS_TEAM_ID`, `APNS_TOPIC`, `APNS_DEFAULT_ENV`, `GRANT_TTL_DAYS`, `RELAY_HEALTHCHECK_URL` |

`wrangler deploy` preserves existing Worker secrets. `RELAY_SEAL_KEY`,
`APNS_KEY_P8` and `FCM_SERVICE_ACCOUNT` remain in Cloudflare, not GitHub CI.

## Tests

```bash
npm run typecheck
npm test
```

Tests run the router with mocked provider requests and rate limiters.
WebCrypto verifies ES256/RS256 signatures. The suite requires no network,
Cloudflare account or emulator.
