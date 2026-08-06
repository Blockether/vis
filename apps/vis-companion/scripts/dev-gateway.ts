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

/** Seed both the synchronous web mirror and Capacitor Preferences before React imports. */
export function devConnectionStorageScript(connections: DevGatewayConnection[]): string {
  const encoded = JSON.stringify(connections);
  const primary = connections[0]?.url ?? '';
  const values = {
    'vis.connections': encoded,
    'CapacitorStorage.vis.connections': encoded,
    'vis.primaryConnection': primary,
    'CapacitorStorage.vis.primaryConnection': primary,
    'vis.activeConnection': primary,
    'CapacitorStorage.vis.activeConnection': primary,
  };
  return `for (const [key, value] of Object.entries(${scriptLiteral(values)})) localStorage.setItem(key, value);`;
}
