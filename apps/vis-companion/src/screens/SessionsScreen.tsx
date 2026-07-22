import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { Session } from '../lib/types';
import { Banner, Button, Card, Input } from '../components/ui';

export function SessionsScreen({
  client,
  onOpen,
}: {
  client: GatewayClient;
  onOpen: (sid: string) => void;
}) {
  const [sessions, setSessions] = useState<Session[] | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const [title, setTitle] = useState('');
  const [busy, setBusy] = useState(false);

  const load = useCallback(async () => {
    setErr(null);
    try {
      setSessions(await client.listSessions());
    } catch (e) {
      setErr((e as Error).message);
      setSessions([]);
    }
  }, [client]);

  useEffect(() => {
    void load();
  }, [load]);

  async function create() {
    setBusy(true);
    try {
      const s = await client.createSession({ title: title.trim() || undefined });
      setTitle('');
      await load();
      if (s.id) onOpen(s.id);
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  return (
    <div className="space-y-4 p-4">
      <Card className="space-y-3">
        <Input
          placeholder="New session title (optional)"
          value={title}
          onChange={(e) => setTitle(e.target.value)}
        />
        <Button onClick={create} disabled={busy}>
          New session
        </Button>
      </Card>

      {err && <Banner kind="err">{err}</Banner>}

      {sessions === null ? (
        <p className="p-2 text-sm text-white/40">loading…</p>
      ) : sessions.length === 0 ? (
        <p className="p-2 text-sm text-white/40">No sessions yet — create one above.</p>
      ) : (
        <div className="space-y-2">
          {sessions.map((s) => (
            <button key={s.id} className="w-full text-left" onClick={() => onOpen(s.id)}>
              <Card className="hover:border-accent/50">
                <div className="truncate text-sm text-white/90">
                  {s.title || <span className="text-white/40">untitled</span>}
                </div>
                <div className="mt-1 flex items-center gap-2 font-mono text-xs text-white/35">
                  <span className="truncate">{s.id}</span>
                  {s.channel && <span className="text-white/25">· {s.channel}</span>}
                </div>
              </Card>
            </button>
          ))}
        </div>
      )}

      <div className="pt-1">
        <Button variant="ghost" onClick={load} className="w-full">
          Refresh
        </Button>
      </div>
    </div>
  );
}
