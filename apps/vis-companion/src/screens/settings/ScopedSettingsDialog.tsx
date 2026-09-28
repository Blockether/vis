import { useEffect, useMemo, useRef, useState } from 'react';
import type { GatewayClient } from '../../lib/gateway';
import type { SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { Banner, DialogFrame, Input, Modal, Text } from '../../components/ui';
import { McpServersPanel, SettingRow } from './MachineSettings';
import { SettingsPanel } from './SettingsLayout';

type ScopedSettingsProps = {
  client: GatewayClient;
  target: SettingsTarget;
  onClose: () => void;
};

/** A new owner gets new form and request state, never a previous owner's catalog. */
export function ScopedSettingsDialog(props: ScopedSettingsProps) {
  return <ScopedSettingsContent key={`${props.target.scope}:${props.target.target_id ?? ''}`} {...props} />;
}

function ScopedSettingsContent({ client, target, onClose }: ScopedSettingsProps) {
  const { scope, target_id, label } = target;
  const owner = useMemo(() => ({ scope, target_id }), [scope, target_id]);
  const [data, setData] = useState<SettingsResponse | null>(() => client.cachedSettings(owner));
  const [pending, setPending] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [search, setSearch] = useState('');
  const epoch = useRef(0);
  const saving = useRef(false);
  useEffect(() => {
    const controller = new AbortController();
    let reading = false;
    const refresh = async () => {
      if (reading || saving.current) return;
      reading = true;
      const version = epoch.current;
      try {
        const next = await client.settings(controller.signal, owner);
        if (!controller.signal.aborted && version === epoch.current) setData(next);
      } catch (err) {
        if (!controller.signal.aborted) setError((err as Error).message);
      } finally { reading = false; }
    };
    void refresh();
    const timer = window.setInterval(() => void refresh(), 3000);
    return () => { controller.abort(); window.clearInterval(timer); };
  }, [client, owner]);
  const save = async (toggle: Toggle, action: 'toggle' | 'value' | 'inherit', value?: string) => {
    if (saving.current) return false;
    saving.current = true;
    epoch.current += 1;
    setPending(toggle.id);
    setError(null);
    try {
      await client.setSetting(toggle.id, action, value, owner);
      setData(await client.settings(undefined, owner));
      return true;
    } catch (err) { setError((err as Error).message); return false; }
    finally { saving.current = false; setPending(null); }
  };
  const needle = search.toLowerCase().trim();
  const groups = (data?.groups ?? []).map((group) => ({ ...group, toggles: group.toggles.filter((toggle) => `${toggle.label} ${toggle.description ?? ''}`.toLowerCase().includes(needle)) })).filter((group) => group.toggles.length);
  return (
    <Modal onDismiss={onClose}>
      <DialogFrame title={`${scope[0].toUpperCase()}${scope.slice(1)} settings`} subtitle={label ?? data?.label ?? target_id} onClose={onClose}>
        <div className="min-h-0 overflow-y-auto">
          <div className="p-3"><Input aria-label="Search settings" placeholder="Search settings" value={search} onChange={(event) => setSearch(event.target.value)} /></div>
          {error && <Banner kind="err">{error}</Banner>}
          {!data && !error && <Text variant="description">Loading settings…</Text>}
          {data && groups.length === 0 && <Text variant="description">No matching settings.</Text>}
          {groups.map((group) => <SettingsPanel key={group.id} title={group.title}>
            <div className="divide-y divide-dialog-edge">{group.toggles.map((toggle) => <SettingRow key={toggle.id} toggle={toggle} busy={pending !== null}
              onToggle={() => void save(toggle, 'toggle')} onPick={(value) => save(toggle, 'value', value)} onInherit={() => void save(toggle, 'inherit')} />)}</div>
          </SettingsPanel>)}
          {!needle && <McpServersPanel client={client} target={owner} />}
        </div>
      </DialogFrame>
    </Modal>
  );
}
