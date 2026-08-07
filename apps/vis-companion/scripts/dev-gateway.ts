import { readFileSync, readdirSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

export interface DevGatewayConnection {
  url: string;
  token: string;
}

interface RegistryEntry extends DevGatewayConnection {
  createdAt: number;
}

interface DiscoveryOptions {
  registryDir?: string;
  probeGateway?: (connection: DevGatewayConnection) => boolean | Promise<boolean>;
}

function patternKey(key: string): string {
  return key.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function ednString(source: string, key: string): string | null {
  const match = source.match(
    new RegExp(`:${patternKey(key)}\\s+("(?:\\\\.|[^"\\\\])*")(?=\\s|,|\\})`, 'u'),
  );
  if (!match) return null;
  try {
    const value: unknown = JSON.parse(match[1]);
    return typeof value === 'string' ? value : null;
  } catch {
    return null;
  }
}

function ednInteger(source: string, key: string): number | null {
  const match = source.match(new RegExp(`:${patternKey(key)}\\s+(-?\\d+)(?=\\s|,|\\})`, 'u'));
  if (!match) return null;
  const value = Number(match[1]);
  return Number.isSafeInteger(value) ? value : null;
}

function browserHost(host: string): string {
  if (host === '0.0.0.0' || host === 'localhost') return '127.0.0.1';
  if (host === '::') return '[::1]';
  return host.includes(':') && !host.startsWith('[') ? `[${host}]` : host;
}

function registryEntry(source: string): RegistryEntry | null {
  const host = ednString(source, 'host');
  const token = ednString(source, 'secret');
  const port = ednInteger(source, 'port');
  const createdAt = ednInteger(source, 'created-at');
  if (!host || !token || !port || port > 65_535 || createdAt === null) return null;
  return {
    url: `http://${browserHost(host)}:${port}`,
    token,
    createdAt,
  };
}

async function gatewayResponds(connection: DevGatewayConnection): Promise<boolean> {
  try {
    const response = await fetch(`${connection.url}/healthz`, {
      headers: { Authorization: `Bearer ${connection.token}` },
      signal: AbortSignal.timeout(1_500),
    });
    return response.ok;
  } catch {
    return false;
  }
}

/** Read and authenticate every reachable daemon without ever logging bearer secrets. */
export async function discoverDevGatewayConnections(
  options: DiscoveryOptions = {},
): Promise<DevGatewayConnection[]> {
  const directory = options.registryDir ?? join(homedir(), '.vis', 'gateway', 'registry');
  const probeGateway = options.probeGateway ?? gatewayResponds;
  let filenames: string[];
  try {
    filenames = readdirSync(directory).filter((name) => name.endsWith('.edn'));
  } catch {
    return [];
  }

  const candidates = filenames.flatMap((filename) => {
    try {
      const entry = registryEntry(readFileSync(join(directory, filename), 'utf8'));
      return entry ? [entry] : [];
    } catch {
      return [];
    }
  });
  const entries = (
    await Promise.all(
      candidates.map(async (entry) => ((await probeGateway(entry)) ? entry : null)),
    )
  ).filter((entry): entry is RegistryEntry => entry !== null);
  entries.sort((a, b) => b.createdAt - a.createdAt || a.url.localeCompare(b.url));

  const unique = new Map<string, DevGatewayConnection>();
  for (const { url, token } of entries) {
    if (!unique.has(url)) unique.set(url, { url, token });
  }
  return [...unique.values()];
}

function scriptLiteral(value: unknown): string {
  return JSON.stringify(value)
    .replaceAll('<', '\\u003c')
    .replaceAll('\u2028', '\\u2028')
    .replaceAll('\u2029', '\\u2029');
}

/**
 * Seed both the synchronous web mirror and Capacitor Preferences before React imports.
 *
 * The registry knows an address and a token; everything else about a paired
 * machine — the name a human typed into its header, its id, its pinned address —
 * is owned by the APP and only lives in storage. So the seed MERGES onto what is
 * already there, per URL, instead of writing the registry over it: a rename that
 * survived the click has to survive the next page load too.
 */
export function devConnectionStorageScript(connections: DevGatewayConnection[]): string {
  const primary = connections[0]?.url ?? '';
  return `(() => {
  const seeded = ${scriptLiteral(connections)};
  let stored = [];
  try {
    const raw = localStorage.getItem('vis.connections');
    if (raw) stored = JSON.parse(raw);
  } catch {}
  const kept = new Map(
    (Array.isArray(stored) ? stored : [])
      .filter((conn) => conn && typeof conn.url === 'string')
      .map((conn) => [conn.url, conn]),
  );
  const merged = JSON.stringify(seeded.map((conn) => ({ ...kept.get(conn.url), ...conn })));
  const values = {
    'vis.connections': merged,
    'CapacitorStorage.vis.connections': merged,
    'vis.primaryConnection': ${scriptLiteral(primary)},
    'CapacitorStorage.vis.primaryConnection': ${scriptLiteral(primary)},
    'vis.activeConnection': ${scriptLiteral(primary)},
    'CapacitorStorage.vis.activeConnection': ${scriptLiteral(primary)},
  };
  for (const [key, value] of Object.entries(values)) localStorage.setItem(key, value);
})();`;
}
