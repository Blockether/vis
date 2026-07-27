/**
 * Gateway ↔ app version compatibility — the browser half of
 * `com.blockether.vis.internal.gateway.protocol`.
 *
 * The companion updates on its own clock: a phone keeps a cached web build for
 * weeks while the gateway behind it gets `brew upgrade`d, or the reverse. So
 * both halves publish two numbers next to their human release version — the
 * wire `protocol` they speak, and the oldest counterpart they still speak to —
 * and compatibility is a pure comparison, never feature sniffing.
 *
 * Keep the numbers and the copy in lockstep with `protocol.clj`: the gateway
 * renders the SAME verdict for the TUI, so a user who sees the screen on their
 * phone and in their terminal must read the same explanation.
 */

import type { GatewayProtocol } from './types';

/** Wire protocol number THIS app build speaks. Bump on a breaking wire change. */
export const APP_PROTOCOL = 2;

/** Oldest gateway protocol this app accepts: only the current wire contract. */
export const APP_MIN_GATEWAY_PROTOCOL = 2;

/** How this app names itself in the handshake. */
export const APP_NAME = 'vis-companion';

/** Human release version, injected from package.json at build time. */
export const APP_VERSION = __VIS_APP_VERSION__;

/**
 * Stamped on EVERY gateway request so the gateway can judge us without a
 * separate negotiation round-trip — and answer 426 with a real explanation
 * instead of feeding us a shape we cannot read.
 */
export const PROTOCOL_HEADERS: Readonly<Record<string, string>> = {
  'X-Vis-Protocol': String(APP_PROTOCOL),
  'X-Vis-Min-Gateway-Protocol': String(APP_MIN_GATEWAY_PROTOCOL),
  'X-Vis-Client': APP_NAME,
  'X-Vis-Client-Version': APP_VERSION,
};

export type CompatReason = 'ok' | 'client-too-old' | 'gateway-too-old' | 'unknown';

export type { GatewayProtocol };

export interface Compat {
  isCompatible: boolean;
  reason: CompatReason;
  /** Which half the user must update, or null when nothing is out of date. */
  upgrade: 'client' | 'gateway' | null;
  gatewayProtocol: number | null;
  gatewayMinClient: number | null;
  gatewayVersion: string | null;
  clientProtocol: number;
  clientMinGateway: number;
  clientVersion: string;
  title: string;
  summary: string;
  remedy: string[];
}

function asInt(value: unknown): number | null {
  if (typeof value === 'number' && Number.isFinite(value)) return Math.trunc(value);
  if (typeof value === 'string' && value.trim() !== '') {
    const n = Number(value);
    return Number.isFinite(n) ? Math.trunc(n) : null;
  }
  return null;
}

/**
 * Judge one gateway from its advertised `protocol` block. An unversioned gateway
 * is unsupported; this app drives only the current daemon wire contract.
 */
export function compatOf(block?: GatewayProtocol | null): Compat {
  const gatewayProtocol = asInt(block?.protocol);
  const gatewayMinClient = asInt(block?.min_client) ?? gatewayProtocol;
  const gatewayVersion = typeof block?.version === 'string' ? block.version : null;

  const reason: CompatReason =
    gatewayProtocol === null
      ? 'unknown'
      : APP_PROTOCOL < (gatewayMinClient ?? gatewayProtocol)
        ? 'client-too-old'
        : gatewayProtocol < APP_MIN_GATEWAY_PROTOCOL
          ? 'gateway-too-old'
          : 'ok';

  const base = {
    isCompatible: reason === 'ok',
    reason,
    gatewayProtocol,
    gatewayMinClient,
    gatewayVersion,
    clientProtocol: APP_PROTOCOL,
    clientMinGateway: APP_MIN_GATEWAY_PROTOCOL,
    clientVersion: APP_VERSION,
  };

  if (reason === 'client-too-old') {
    return {
      ...base,
      upgrade: 'client',
      title: 'Update this app',
      summary:
        `The gateway speaks protocol ${gatewayProtocol} and no longer serves clients ` +
        `below protocol ${gatewayMinClient}. This app speaks protocol ${APP_PROTOCOL}.`,
      remedy: [
        'Reload this page to pick up the build the gateway ships.',
        'On iOS or Android, update the Vis Companion app from the store.',
      ],
    };
  }

  if (reason === 'gateway-too-old') {
    return {
      ...base,
      upgrade: 'gateway',
      title: 'Update the gateway',
      summary:
        `This app needs gateway protocol ${APP_MIN_GATEWAY_PROTOCOL} or newer, but the ` +
        `gateway speaks protocol ${gatewayProtocol}.`,
      remedy: [
        'Update Vis on the machine hosting the gateway.',
        'Restart it: vis gateway stop && vis gateway start',
      ],
    };
  }

  if (reason === 'unknown') {
    return {
      ...base,
      upgrade: 'gateway',
      title: 'Update the gateway',
      summary: 'The gateway did not advertise the current Vis wire protocol and is unsupported.',
      remedy: [
        'Update Vis on the machine hosting the gateway.',
        'Restart it: vis gateway stop && vis gateway start',
      ],
    };
  }

  return {
    ...base,
    upgrade: null,
    title: 'Versions match',
    summary: `Gateway and app both speak protocol ${gatewayProtocol}.`,
    remedy: [],
  };
}
