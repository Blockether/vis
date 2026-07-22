import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { GatewayTheme, Toggle, ToggleGroup } from '../lib/types';
import { applyGatewayTheme } from '../lib/theme';
import { Banner, Card, Section } from '../components/ui';

export function SettingsScreen({ client }: { client: GatewayClient }) {
  const [groups, setGroups] = useState<ToggleGroup[] | null>(null);
  const [theme, setTheme] = useState<GatewayTheme | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const [pending, setPending] = useState<string | null>(null);

  const load = useCallback(async () => {
    setErr(null);
    try {
      const settings = await client.settings();
      setGroups(settings.groups ?? []);
      try {
        const activeTheme = await client.theme();
        setTheme(activeTheme);
        applyGatewayTheme(activeTheme);
      } catch (e) {
        // Keep the existing settings usable while an older daemon awaits restart.
        setTheme(null);
        setErr(`Theme sync unavailable: ${(e as Error).message}`);
      }
    } catch (e) {
      setErr((e as Error).message);
      setGroups([]);
    }
  }, [client]);

  useEffect(() => {
    void load();
  }, [load]);

  function patch(updated: Toggle) {
    setGroups((gs) =>
      gs?.map((g) => ({
        ...g,
        toggles: g.toggles.map((t) => (t.id === updated.id ? updated : t)),
      })) ?? null,
    );
  }

  async function chooseTheme(id: string) {
    setPending(`theme:${id}`);
    try {
      const updated = await client.setTheme(id);
      setTheme(updated);
      applyGatewayTheme(updated);
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function flip(t: Toggle) {
    setPending(t.id);
    try {
      patch(await client.setSetting(t.id, 'toggle'));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function pick(t: Toggle, value: string) {
    setPending(t.id);
    try {
      patch(await client.setSetting(t.id, 'value', value));
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  return (
    <div className="space-y-6 p-4">
      {err && <Banner kind="err">{err}</Banner>}

      {theme && (
        <Section title="Theme — shared with TUI">
          <Card className="space-y-3">
            <div>
              <div className="text-sm text-white/90">{theme.display_name}</div>
              <div className="mt-0.5 text-xs text-white/40">
                Stored by the gateway. The TUI and companion use the same palette.
              </div>
            </div>
            <div className="grid grid-cols-2 gap-2">
              {theme.themes.map((choice) => (
                <button
                  key={choice.id}
                  disabled={pending !== null}
                  onClick={() => chooseTheme(choice.id)}
                  className={`rounded-lg border px-3 py-2 text-left text-xs transition disabled:opacity-50 ${
                    choice.id === theme.id
                      ? 'border-accent bg-accent/15 text-accent'
                      : 'border-edge text-white/60 hover:bg-panel-2'
                  }`}
                >
                  <span className="block font-medium">{choice.display_name}</span>
                  <span className="font-mono text-[10px] opacity-60">{choice.mode}</span>
                </button>
              ))}
            </div>
          </Card>
        </Section>
      )}

      {groups === null ? (
        <p className="p-2 text-sm text-white/40">loading…</p>
      ) : groups.length === 0 ? (
        <p className="p-2 text-sm text-white/40">No settings exposed by this gateway.</p>
      ) : (
        groups.map((g) => (
          <Section key={g.id} title={g.title}>
            <div className="space-y-2">
              {g.toggles.map((t) => (
                <Card key={t.id} className="space-y-2">
                  <div className="flex items-center justify-between gap-3">
                    <div className="min-w-0">
                      <div className="truncate text-sm text-white/90">{t.label}</div>
                      {t.description && (
                        <div className="mt-0.5 text-xs text-white/40">{t.description}</div>
                      )}
                    </div>
                    {t.type === 'boolean' && (
                      <Switch
                        on={!!t.enabled}
                        disabled={pending === t.id}
                        onClick={() => flip(t)}
                      />
                    )}
                  </div>

                  {t.type === 'enum' && t.choices && (
                    <div className="flex flex-wrap gap-1.5">
                      {t.choices.map((c) => (
                        <button
                          key={c}
                          disabled={pending === t.id}
                          onClick={() => pick(t, c)}
                          className={`rounded-md border px-2.5 py-1 font-mono text-xs transition ${
                            t.value === c
                              ? 'border-accent bg-accent/15 text-accent'
                              : 'border-edge text-white/55 hover:bg-panel-2'
                          }`}
                        >
                          {c}
                        </button>
                      ))}
                    </div>
                  )}
                </Card>
              ))}
            </div>
          </Section>
        ))
      )}
    </div>
  );
}

function Switch({
  on,
  disabled,
  onClick,
}: {
  on: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  return (
    <button
      role="switch"
      aria-checked={on}
      disabled={disabled}
      onClick={onClick}
      className={`relative h-6 w-11 shrink-0 rounded-full transition-colors disabled:opacity-50 ${
        on ? 'bg-accent' : 'bg-edge'
      }`}
    >
      <span
        className={`absolute top-0.5 h-5 w-5 rounded-full bg-white transition-transform ${
          on ? 'translate-x-5' : 'translate-x-0.5'
        }`}
      />
    </button>
  );
}
