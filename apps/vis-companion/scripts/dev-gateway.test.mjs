import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, describe, expect, it } from 'vitest';
import {
  devConnectionStorageScript,
  discoverDevGatewayConnections,
} from './dev-gateway.ts';

const tempDirs = [];

function registryDir() {
  const dir = mkdtempSync(join(tmpdir(), 'vis-companion-dev-gateway-'));
  tempDirs.push(dir);
  return dir;
}

function writeRegistry(dir, name, fields) {
  const entry = `{:db ${JSON.stringify(fields.db)}, :created-at ${fields.createdAt}, :pid ${fields.pid}, :port ${fields.port}, :host ${JSON.stringify(fields.host)}, :secret ${JSON.stringify(fields.secret)}}`;
  writeFileSync(join(dir, `${name}.edn`), entry);
}

afterEach(() => {
  for (const dir of tempDirs.splice(0)) rmSync(dir, { recursive: true, force: true });
});

describe('companion dev gateway discovery', () => {
  it('turns every live registry into an authenticated browser connection, newest first', async () => {
    const dir = registryDir();
    writeRegistry(dir, 'older', {
      db: '/tmp/older/vis.db',
      createdAt: 10,
      pid: 101,
      port: 7890,
      host: '0.0.0.0',
      secret: 'older-token',
    });
    writeRegistry(dir, 'newer', {
      db: '/tmp/newer/vis.db',
      createdAt: 20,
      pid: 202,
      port: 8790,
      host: '::',
      secret: 'newer-token',
    });
    writeRegistry(dir, 'stale', {
      db: '/tmp/stale/vis.db',
      createdAt: 30,
      pid: 303,
      port: 9790,
      host: '127.0.0.1',
      secret: 'stale-token',
    });

    expect(
      await discoverDevGatewayConnections({
        registryDir: dir,
        probeGateway: (connection) => !connection.url.endsWith(':9790'),
      }),
    ).toEqual([
      { url: 'http://[::1]:8790', token: 'newer-token' },
      { url: 'http://127.0.0.1:7890', token: 'older-token' },
    ]);
  });

  it('seeds both storage mirrors before the application reads its first frame', () => {
    const connections = [
      { url: 'http://127.0.0.1:7890', token: 'token-one' },
      { url: 'http://127.0.0.1:8790', token: 'token-two' },
    ];
    const values = new Map();
    const localStorage = {
      setItem(key, value) {
        values.set(key, value);
      },
    };

    new Function('localStorage', devConnectionStorageScript(connections))(localStorage);

    const encoded = JSON.stringify(connections);
    expect(values.get('vis.connections')).toBe(encoded);
    expect(values.get('CapacitorStorage.vis.connections')).toBe(encoded);
    expect(values.get('vis.primaryConnection')).toBe(connections[0].url);
    expect(values.get('CapacitorStorage.vis.primaryConnection')).toBe(connections[0].url);
    expect(values.get('vis.activeConnection')).toBe(connections[0].url);
    expect(values.get('CapacitorStorage.vis.activeConnection')).toBe(connections[0].url);
  });

  it('clears stale dev pairings when no registered gateway answers', () => {
    const values = new Map([
      ['vis.connections', '[{"url":"http://127.0.0.1:9999","token":"old"}]'],
      ['vis.primaryConnection', 'http://127.0.0.1:9999'],
    ]);
    const localStorage = {
      setItem(key, value) {
        values.set(key, value);
      },
    };

    new Function('localStorage', devConnectionStorageScript([]))(localStorage);

    expect(values.get('vis.connections')).toBe('[]');
    expect(values.get('CapacitorStorage.vis.connections')).toBe('[]');
    expect(values.get('vis.primaryConnection')).toBe('');
    expect(values.get('vis.activeConnection')).toBe('');
  });
  // Regression, turn 37: renaming a machine in its own header saved the label,
  // and the next page load put the address back — the dev seed rewrote
  // `vis.connections` from the registry alone and dropped every field the app
  // owns.
  it('keeps the name the human gave a machine', () => {
    const values = new Map([
      [
        'vis.connections',
        JSON.stringify([
          {
            url: 'http://127.0.0.1:7890',
            token: 'stale-token',
            label: 'tower',
            id: 'be2c15686eaef0f4',
          },
        ]),
      ],
    ]);
    const localStorage = {
      getItem(key) {
        return values.has(key) ? values.get(key) : null;
      },
      setItem(key, value) {
        values.set(key, value);
      },
    };

    new Function(
      'localStorage',
      devConnectionStorageScript([
        { url: 'http://127.0.0.1:7890', token: 'fresh-token' },
      ]),
    )(localStorage);

    expect(JSON.parse(values.get('vis.connections'))).toEqual([
      {
        url: 'http://127.0.0.1:7890',
        token: 'fresh-token',
        label: 'tower',
        id: 'be2c15686eaef0f4',
      },
    ]);
  });
});
