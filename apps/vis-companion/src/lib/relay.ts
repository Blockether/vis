// Being woken by a machine that cannot sign a push.
//
// A gateway pushes with ITS OWN Apple or Firebase credentials, and most machines
// have none: signing keys belong to whoever built the app, not to every laptop
// the phone is paired with. Such a machine is not condemned to silence — it can
// reach this device through the relay, but only by presenting a capability this
// DEVICE minted for itself. That is the grant: it names the device, carries its
// own expiry sealed inside it, and tells the holder nothing else.
//
// Two rules keep the arrangement honest, and both are enforced here rather than
// in the screens that call it:
//
//   * A machine that CAN sign its own pushes never receives a grant. Routing it
//     through the relay would show a third party the title and body of alerts
//     that never had to leave the machine.
//   * A machine that cannot sign gets the grant INSTEAD of the push token, never
//     as well. The raw token would be dead weight there, and a device list that
//     shows a token implies a delivery path that does not exist.
//
// Minting is deliberately rate-limited at the relay, so the grant is cached by
// `storage` and reused across machines and launches; see `relayGrants`.

import {
  getRelayGrant,
  relayGrants,
  setRelayGrant,
  type RelayGrant,
} from "./storage";
import type { PushDeviceInput, PushStatus } from "./types";

/** How this device named itself to one gateway. */
export interface PushIdentity {
  kind: "token" | "grant";
  value: string;
}

/** How a grant is obtained; injected so tests never reach the network. */
export type MintGrant = (
  relayUrl: string,
  device: PushDeviceInput,
) => Promise<RelayGrant>;

/**
 * Re-mint this long before expiry.
 *
 * A phone that is opened rarely is exactly the phone whose grant lapses between
 * launches, and the sweep that would have renewed it only runs when the app is
 * opened. Renewing a week early costs one request and removes that window.
 */
const RENEW_BEFORE_MS = 7 * 24 * 60 * 60 * 1000;

/**
 * The relay one gateway needs to reach THIS device, or null when it can sign
 * pushes to it itself.
 *
 * Push has two disjoint halves and this device only lives in one of them: a
 * gateway holding Firebase credentials and no APNs key can sign for a Pixel and
 * not for an iPhone, so the verdict is per platform, never the summary flag.
 */
function signsItself(push: PushStatus, platform: string): boolean {
  return Boolean(
    platform === "android" ? push.fcm?.is_available : push.apns?.is_available,
  );
}

export function relayUrlFor(
  push: PushStatus | undefined,
  platform: string,
): string | null {
  if (!push) return null;
  if (signsItself(push, platform)) return null;
  const url = push.relay?.url;
  // This device's push token travels to whatever address the gateway names here,
  // so a paired machine may send us to a relay over TLS and nowhere else.
  if (!push.relay?.is_available || !url || !url.startsWith("https://"))
    return null;
  return url;
}

/**
 * The relay a machine named and this device REFUSED, or null.
 *
 * Anyone may run their own relay — the address is configuration on the machine,
 * never a constant in this app — and the first way that goes wrong is an `http`
 * address, which would put a permanent right to push to this phone on the wire.
 * Such a machine holds no credentials AND has a relay we will not use, and
 * "missing push credentials" would send its operator looking for a signing key
 * they do not need. Name the address instead: only they can change it.
 */
export function refusedRelayUrl(
  push: PushStatus | undefined,
  platform: string,
): string | null {
  if (!push || signsItself(push, platform)) return null;
  const url = push.relay?.url;
  if (!url) return null;
  return url.startsWith("https://") && !push.relay?.is_insecure ? null : url;
}

/** A relay address as a person reads it: the host, never the path. */
export function relayHost(url: string | null | undefined): string | null {
  if (!url) return null;
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

/** `POST /v1/grants`: hand the relay this device's token, get back a capability. */
export const mintGrant: MintGrant = async (relayUrl, device) => {
  const response = await fetch(`${relayUrl.replace(/\/+$/, "")}/v1/grants`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      device_token: device.token,
      platform: device.platform,
      environment: device.environment,
    }),
  });
  if (!response.ok)
    throw new Error(
      `The relay refused a grant for this device (${response.status}).`,
    );
  const body = (await response.json()) as {
    grant?: unknown;
    expires_at?: unknown;
  };
  if (typeof body.grant !== "string" || !body.grant) {
    throw new Error("The relay returned no grant.");
  }
  return {
    token: String(device.token ?? ""),
    grant: body.grant,
    expires_at:
      typeof body.expires_at === "number" ? body.expires_at : undefined,
  };
};

function isUsable(
  cached: RelayGrant | null,
  token: string,
  now: number,
): boolean {
  if (!cached || cached.token !== token) return false;
  return (
    cached.expires_at === undefined || cached.expires_at - now > RENEW_BEFORE_MS
  );
}

/** The grant this device presents to one relay, minting only when it must. */
export async function grantFor(
  relayUrl: string,
  device: PushDeviceInput,
  mint: MintGrant = mintGrant,
  now: number = Date.now(),
): Promise<string> {
  const token = String(device.token ?? "");
  const cached = await getRelayGrant(relayUrl);
  if (cached && isUsable(cached, token, now)) return cached.grant;
  const minted = await mint(relayUrl, device);
  await setRelayGrant(relayUrl, minted);
  return minted.grant;
}

/** One gateway, reduced to what push registration needs of it. */
export interface PushGateway {
  status: () => Promise<PushStatus>;
  register: (input: PushDeviceInput) => Promise<unknown>;
  unregister: (id: string) => Promise<unknown>;
}

/**
 * Register this device with one gateway, through its relay when it needs one.
 *
 * A relay that refuses to mint is a FAILED registration, never a fallback to the
 * raw token: the machine has no key to use that token with, so registering it
 * would leave a device row that can never be delivered to — the exact appearance
 * of working that made silent notifications so hard to explain. The sweep retries
 * on the next launch or wake.
 */
export async function registerForPush(
  device: PushDeviceInput,
  gateway: PushGateway,
  mint: MintGrant = mintGrant,
  now: number = Date.now(),
): Promise<PushIdentity> {
  const relayUrl = relayUrlFor(
    await gateway.status(),
    String(device.platform ?? ""),
  );
  if (!relayUrl) {
    await gateway.register(device);
    return { kind: "token", value: String(device.token ?? "") };
  }
  const grant = await grantFor(relayUrl, device, mint, now);
  await gateway.register({ ...device, token: undefined, grant });
  return { kind: "grant", value: grant };
}

/**
 * Every id a gateway may have filed this device under: its OS push token, and
 * any grant this device minted.
 *
 * Both halves matter. Turning notifications off has to remove whichever one that
 * machine is holding, and the settings screen has to recognise its own row in a
 * device list where a relayed registration appears as the grant.
 */
export async function registeredIds(token: string): Promise<string[]> {
  const ids = token ? [token] : [];
  for (const held of await relayGrants()) {
    if (!ids.includes(held.grant)) ids.push(held.grant);
  }
  return ids;
}

/** Take this device off one gateway, under every name it may be filed under. */
export async function unregisterFromPush(
  token: string,
  gateway: Pick<PushGateway, "unregister">,
): Promise<void> {
  for (const id of await registeredIds(token)) {
    await gateway.unregister(id);
  }
}
