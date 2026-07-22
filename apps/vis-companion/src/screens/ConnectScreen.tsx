import { useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import { removeConnection } from '../lib/storage';
import { scanQr } from '../lib/scan';
import { Banner, Button, Card, Input, Section } from '../components/ui';

interface Props {
  conns: GatewayConn[];
  active: GatewayConn | null;
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  onSelect: (url: string) => Promise<void>;
  onChanged: () => Promise<void>;
}

export function ConnectScreen({ conns, active, onAdd, onSelect, onChanged }: Props) {
  const [payload, setPayload] = useState('');
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [msg, setMsg] = useState<{ kind: 'ok' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);

  async function tryConn(conn: GatewayConn) {
    setBusy(true);
    setMsg(null);
    try {
      const client = new GatewayClient(conn);
      await client.ping(); // throws GatewayError on 401 (reachable, needs token)
      await onAdd(conn);
      setMsg({ kind: 'ok', text: `paired with ${conn.label ?? conn.url}` });
      setPayload('');
      setUrl('');
      setToken('');
    } catch (e) {
      // Still store it so the user can retry once on the network (e.g. bring up
      // Tailscale / cloudflared), but report the failure.
      await onAdd(conn, false);
      setMsg({ kind: 'err', text: `saved, but not reachable yet: ${(e as Error).message}` });
    } finally {
      setBusy(false);
    }
  }

  async function addFromPayload() {
    const conn = parsePairing(payload);
    if (!conn) {
      setMsg({ kind: 'err', text: 'not a vis:// pairing link or gateway URL' });
      return;
    }
    await tryConn(conn);
  }

  async function addManual() {
    if (!/^https?:\/\//i.test(url.trim())) {
      setMsg({ kind: 'err', text: 'url must start with http:// or https://' });
      return;
    }
    const u = url.trim();
    await tryConn({ url: u, token: token.trim() || undefined, label: hostOf(u) });
  }

  async function scan() {
    const raw = await scanQr();
    if (!raw) {
      setMsg({ kind: 'err', text: 'scan cancelled or camera unavailable' });
      return;
    }
    const conn = parsePairing(raw);
    if (!conn) {
      setMsg({ kind: 'err', text: 'QR is not a vis pairing code' });
      return;
    }
    await tryConn(conn);
  }

  return (
    <div className="space-y-6 p-4">
      <Section title="Pair a gateway">
        <p className="px-1 text-sm text-white/50">
          Run <code className="text-accent">vis gateway pair</code> on the machine, then
          scan the QR or paste the <code className="text-accent">vis://</code> link. The
          gateway can be on LAN, a Tailscale <span className="text-white/70">100.x</span>{' '}
          host, or a cloudflared tunnel URL.
        </p>
        <Card className="space-y-3">
          <Input
            placeholder="vis://gateway?url=…&token=…"
            value={payload}
            onChange={(e) => setPayload(e.target.value)}
          />
          <div className="flex gap-2">
            <Button className="flex-1" onClick={addFromPayload} disabled={busy || !payload}>
              Pair from link
            </Button>
            <Button variant="ghost" onClick={scan} disabled={busy}>
              Scan QR
            </Button>
          </div>
        </Card>
      </Section>

      <Section title="Manual (URL + token)">
        <Card className="space-y-3">
          <Input
            placeholder="http://100.64.0.10:7890  ·  https://name.trycloudflare.com"
            value={url}
            onChange={(e) => setUrl(e.target.value)}
            autoCapitalize="none"
            autoCorrect="off"
          />
          <Input
            placeholder="bearer token (blank for loopback)"
            value={token}
            onChange={(e) => setToken(e.target.value)}
            autoCapitalize="none"
            autoCorrect="off"
          />
          <Button onClick={addManual} disabled={busy || !url}>
            Connect
          </Button>
        </Card>
      </Section>

      {msg && <Banner kind={msg.kind === 'ok' ? 'ok' : 'err'}>{msg.text}</Banner>}

      {conns.length > 0 && (
        <Section title="Saved gateways">
          <div className="space-y-2">
            {conns.map((c) => {
              const on = active?.url === c.url;
              return (
                <Card key={c.url} className="flex items-center justify-between gap-3">
                  <button
                    className="min-w-0 flex-1 text-left"
                    onClick={() => onSelect(c.url)}
                  >
                    <div className="flex items-center gap-2">
                      <span className={`h-2 w-2 rounded-full ${on ? 'bg-ok' : 'bg-white/20'}`} />
                      <span className="truncate text-sm text-white/90">{c.label ?? c.url}</span>
                    </div>
                    <div className="truncate pl-4 font-mono text-xs text-white/35">{c.url}</div>
                  </button>
                  <Button
                    variant="danger"
                    className="px-3 py-1.5 text-xs"
                    onClick={async () => {
                      await removeConnection(c.url);
                      await onChanged();
                    }}
                  >
                    Remove
                  </Button>
                </Card>
              );
            })}
          </div>
        </Section>
      )}
    </div>
  );
}

function hostOf(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}
