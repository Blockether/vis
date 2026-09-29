import { useEffect, useMemo, useRef, useState } from 'react';
import { CloseIcon, SearchIcon } from '../../components/icons';
import type { GatewayClient } from '../../lib/gateway';
import type { SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { Banner, Button, DialogFrame, IconButton, Input, Modal, Text } from '../../components/ui';
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
  const searchInput = useRef<HTMLInputElement>(null);
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
  const groups = (data?.groups ?? [])
    .map((group) => ({
      ...group,
      toggles: group.title.toLowerCase().includes(needle)
        ? group.toggles
        : group.toggles.filter((toggle) =>
            `${toggle.label} ${toggle.description ?? ''}`.toLowerCase().includes(needle),
          ),
    }))
    .filter((group) => group.toggles.length);
  const matches = groups.reduce((count, group) => count + group.toggles.length, 0);
  const clearSearch = () => {
    setSearch('');
    searchInput.current?.focus();
  };

  return (
    <Modal size="fit-roomy" onDismiss={onClose}>
      <DialogFrame title={`${scope[0].toUpperCase()}${scope.slice(1)} settings`} subtitle={label ?? data?.label ?? target_id} onClose={onClose}>
        <div className="min-h-0 overflow-y-auto">
          <div className="sticky top-0 z-10 border-b border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4">
            <Input
              ref={searchInput}
              type="search"
              icon={<SearchIcon className="size-4" />}
              action={search && (
                <IconButton label="Clear settings search" variant="quiet" onClick={clearSearch}>
                  <CloseIcon className="size-3.5" />
                </IconButton>
              )}
              aria-label="Search settings"
              placeholder="Search settings"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              onKeyDown={(event) => {
                if (event.key === 'Escape' && search) {
                  event.preventDefault();
                  event.stopPropagation();
                  clearSearch();
                }
              }}
            />
            {data && needle && matches > 0 && (
              <Text as="p" variant="meta" role="status" className="mt-2">
                {matches} {matches === 1 ? 'setting' : 'settings'} found · Clear search to view MCP servers
              </Text>
            )}
          </div>
          {error && <div className="p-3"><Banner kind="err">{error}</Banner></div>}
          {!data && !error && <div className="p-6 text-center"><Text variant="description">Loading settings…</Text></div>}
          {data && needle && matches === 0 && (
            <div role="status" className="px-4 py-8 text-center sm:py-10">
              <SearchIcon className="mx-auto size-5 text-dialog-hint" />
              <Text as="h3" variant="section" className="mt-3 break-words">No settings match “{search.trim()}”</Text>
              <Text as="p" variant="description" className="mx-auto mt-1 max-w-sm">
                Try a different name or description, or clear your search to browse settings and MCP servers.
              </Text>
              <Button type="button" variant="secondary" density="panel" className="mt-4" onClick={clearSearch}>
                Clear search
              </Button>
            </div>
          )}
          {(groups.length > 0 || !needle) && (
            <div className="divide-y divide-dialog-edge">
              {groups.map((group) => <SettingsPanel key={group.id} title={group.title}>
                <div className="divide-y divide-dialog-edge">{group.toggles.map((toggle) => <SettingRow key={toggle.id} toggle={toggle} busy={pending !== null}
                  onToggle={() => void save(toggle, 'toggle')} onPick={(value) => save(toggle, 'value', value)} onInherit={() => void save(toggle, 'inherit')} />)}</div>
              </SettingsPanel>)}
              {!needle && <McpServersPanel client={client} target={owner} />}
            </div>
          )}
        </div>
      </DialogFrame>
    </Modal>
  );
}
