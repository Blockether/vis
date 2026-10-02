import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import { SearchIcon } from '../../components/icons';
import { Banner, Button, Chip, CloseButton, Input, Select, SettingsHeader, Text } from '../../components/ui';
import type { GatewayClient } from '../../lib/gateway';
import {
  formatSettingValue,
  matchingGroups,
  previewSetting,
  SETTINGS_CATEGORIES,
  settingValue,
  type SettingsCategory,
} from '../../lib/settings-model';
import type { SettingsTarget } from '../../lib/types';
import { SettingField } from './SettingField';
import { SettingsPanel } from './SettingsLayout';
import { SettingsTransfer } from './SettingsTransfer';
import { useSettingsDraft } from './useSettingsDraft';

export type SettingsLeaveGuard = (leave: () => void) => void;
interface SettingsEditorProps {
  client: GatewayClient;
  target: SettingsTarget;
  contextSessionId?: string;
  category?: SettingsCategory;
  onCategoryChange?: (category: SettingsCategory) => void;
  onLeaveGuard?: (guard: SettingsLeaveGuard | null) => void;
  onTargetChange?: (target: SettingsTarget) => void;
  resources?: (category: SettingsCategory, search: string) => ReactNode;
}
export function SettingsEditor({
  client,
  target,
  contextSessionId,
  category: chosenCategory,
  onCategoryChange,
  onLeaveGuard,
  onTargetChange,
  resources,
}: SettingsEditorProps) {
  const [category, setCategory] = useState<SettingsCategory>('basic');
  const [search, setSearch] = useState('');
  const [modified, setModified] = useState(false);
  const [rawDrafts, setRawDrafts] = useState<Record<string, string>>({});
  const [rawErrors, setRawErrors] = useState<Record<string, string>>({});
  const [permissionReview, setPermissionReview] = useState(false);
  const [leaveAction, setLeaveAction] = useState<(() => void) | null>(null);
  const searchInput = useRef<HTMLInputElement>(null);
  const clearRaw = () => {
    setRawDrafts({});
    setRawErrors({});
    setPermissionReview(false);
  };
  const draft = useSettingsDraft(
    client,
    target,
    contextSessionId,
    Object.keys(rawDrafts).length > 0,
    clearRaw,
  );
  const selectedCategory = chosenCategory ?? category;
  const selectCategory = onCategoryChange ?? setCategory;
  const discard = () => {
    draft.discard();
    clearRaw();
  };
  const requestLeave = useCallback<SettingsLeaveGuard>(
    (leave) => {
      if (draft.busy) return;
      if (draft.dirty) setLeaveAction(() => leave);
      else leave();
    },
    [draft.busy, draft.dirty],
  );
  useEffect(() => {
    onLeaveGuard?.(requestLeave);
    return () => onLeaveGuard?.(null);
  }, [onLeaveGuard, requestLeave]);
  useEffect(() => {
    const warn = (event: BeforeUnloadEvent) => {
      if (draft.dirty || draft.busy) {
        event.preventDefault();
        event.returnValue = '';
      }
    };
    window.addEventListener('beforeunload', warn);
    return () => window.removeEventListener('beforeunload', warn);
  }, [draft.dirty, draft.busy]);
  const allSettings = draft.data?.groups.flatMap((group) => group.toggles) ?? [];
  const groups = draft.data ? matchingGroups(draft.data, selectedCategory, search, modified) : [];
  const permissionChanges = Object.keys(draft.changes).some(
    (id) => allSettings.find((setting) => setting.id === id)?.schema,
  );
  const count = Object.keys(draft.changes).length;
  const apply = async () => {
    await draft.apply();
  };
  const owners = [...(draft.data?.lineage ?? [target])];
  if (contextSessionId && !owners.some((owner) => owner.scope === 'session'))
    owners.push({ scope: 'session', target_id: contextSessionId, label: 'Current session' });
  const stage: typeof draft.stage = (setting, change) => {
    draft.stage(setting, change);
    setPermissionReview(false);
  };
  return (
    <div className="flex min-h-0 min-w-0 flex-1 flex-col">
      <div className="shrink-0 space-y-3 border-b border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4">
        <div className="flex flex-wrap items-center gap-2">
          <Text variant="label">
            Editing:{' '}
            {target.label ??
              (target.scope === 'global'
                ? 'Machine'
                : `${target.scope[0].toUpperCase()}${target.scope.slice(1)}`)}
          </Text>
          {onTargetChange && owners.length > 1 && (
            <Select
              aria-label="Settings scope"
              value={`${target.scope}:${target.target_id ?? ''}`}
              options={owners.map((owner) => ({
                value: `${owner.scope}:${owner.target_id ?? ''}`,
                label: owner.label ?? `${owner.scope}: ${owner.target_id ?? ''}`,
              }))}
              disabled={draft.busy}
              onValueChange={(value) => {
                const owner = owners.find(
                  (owner) => `${owner.scope}:${owner.target_id ?? ''}` === value,
                );
                if (owner) requestLeave(() => onTargetChange(owner));
              }}
            />
          )}
          <Chip>{target.scope}</Chip>
        </div>
        <Text as="p" variant="description">
          Default → Machine → Project → Group → Session. A setting uses the nearest value. Device
          appearance and audio stay on this device.
        </Text>
        <div className="flex flex-wrap gap-2">
          <Input
            ref={searchInput}
            type="search"
            icon={<SearchIcon className="size-4" />}
            className="min-w-0 flex-1"
            aria-label="Search settings"
            placeholder="Search all settings and integrations"
            value={search}
            action={
              search && (
                <CloseButton
                  label="Clear settings search"
                  onClick={() => {
                    setSearch('');
                    searchInput.current?.focus();
                  }}
                />
              )
            }
            onChange={(event) => setSearch(event.target.value)}
            onKeyDown={(event) => {
              if (event.key === 'Escape' && search) {
                event.preventDefault();
                event.stopPropagation();
                setSearch('');
              }
            }}
          />
          <Select
            aria-label="Settings category"
            value={selectedCategory}
            onValueChange={(value) => selectCategory(value as SettingsCategory)}
            options={SETTINGS_CATEGORIES.map(([value, label]) => ({ value, label }))}
          />
          <Button
            variant="secondary"
            density="panel"
            aria-pressed={modified}
            onClick={() => setModified((value) => !value)}
          >
            Set here only
          </Button>
        </div>
        {search && (
          <Text as="p" variant="meta" role="status">
            {groups.reduce((total, group) => total + group.toggles.length, 0)} settings found across
            all categories
          </Text>
        )}
      </div>
      <div className="min-h-0 flex-1 overflow-y-auto overscroll-contain">
        {draft.error && (
          <div className="p-3">
            <Banner kind="err">{draft.error}</Banner>
          </div>
        )}
        {draft.latest && (
          <div className="space-y-2 p-3">
            <Banner kind="warn">
              Settings changed in another client. Your draft is kept. Review the current values
              before applying again.
            </Banner>
            <details>
              <summary className="cursor-pointer text-sm">Changed settings</summary>
              <ul className="space-y-1 py-2 text-sm">
                {allSettings
                  .filter((setting) => {
                    const next = draft.latest?.groups
                      .flatMap((group) => group.toggles)
                      .find((next) => next.id === setting.id);
                    return (
                      next &&
                      JSON.stringify(settingValue(next)) !== JSON.stringify(settingValue(setting))
                    );
                  })
                  .map((setting) => (
                    <li key={setting.id}>
                      {setting.label}: {formatSettingValue(settingValue(setting))} →{' '}
                      {formatSettingValue(
                        settingValue(
                          draft
                            .latest!.groups.flatMap((group) => group.toggles)
                            .find((next) => next.id === setting.id)!,
                        ),
                      )}
                    </li>
                  ))}
              </ul>
            </details>
            <Button variant="secondary" density="panel" onClick={draft.reviewLatest}>
              Review latest and keep draft
            </Button>
          </div>
        )}
        {!draft.data && !draft.error && (
          <div className="p-6" role="status">
            <Text as="p" variant="description">Loading settings…</Text>
          </div>
        )}
        {draft.data && (
          <SettingsTransfer
            data={draft.data}
            hasDrafts={draft.dirty}
            disabled={draft.busy}
            stage={stage}
          />
        )}
        {draft.data && groups.length === 0 && (
          <div className="p-6 text-center" role="status">
            <Text as="p" variant="description">
              {search
                ? `No settings match “${search.trim()}”`
                : modified
                  ? 'No settings are set here in this category.'
                  : 'No configuration fields in this category.'}
            </Text>
            {search.trim() && (
              <Button
                variant="secondary"
                density="panel"
                className="mt-3"
                onClick={() => setSearch('')}
              >
                Clear search
              </Button>
            )}
          </div>
        )}
        <SettingsHeader>
          <Text as="h3" variant="section" className="min-w-0 flex-auto truncate">
            {search.trim()
              ? 'Search results'
              : (SETTINGS_CATEGORIES.find(([id]) => id === selectedCategory)?.[1] ?? 'Settings')}
          </Text>
        </SettingsHeader>
        <div className="divide-y divide-dialog-edge">
          {groups.map((group) => (
            <SettingsPanel key={group.id} title={group.title} headingLevel={4}>
              <div className="divide-y divide-dialog-edge">
                {group.toggles.map((setting) => {
                  const change = draft.changes[setting.id];
                  const preview = previewSetting(setting, change);
                  const fieldError = rawErrors[setting.id] ?? draft.fieldErrors[setting.id];
                  return (
                    <div
                      key={setting.id}
                      className="space-y-2 px-3 py-3 sm:px-4"
                      data-setting-id={setting.id}
                    >
                      <div
                        className={`flex min-w-0 gap-3 ${['boolean', 'enum'].includes(setting.type) ? 'items-start justify-between' : 'flex-col'}`}
                      >
                        <div className="min-w-0 flex-1">
                          <div className="flex flex-wrap items-center gap-2">
                            <Text variant="label">{setting.label}</Text>
                            {setting.is_experimental && <Chip>Experimental</Chip>}
                            {change && <Chip>Draft</Chip>}
                          </div>
                          {setting.description && (
                            <Text as="p" variant="description" className="mt-1 break-words">
                              {setting.description}
                            </Text>
                          )}
                        </div>
                        <div
                          className={
                            ['boolean', 'enum'].includes(setting.type)
                              ? 'shrink-0 self-center'
                              : 'min-w-0'
                          }
                        >
                          <SettingField
                            setting={setting}
                            value={settingValue(preview)}
                            disabled={draft.busy}
                            raw={rawDrafts[setting.id]}
                            onChange={(value, keepRaw) => {
                              if (!keepRaw) {
                                setRawDrafts((current) => {
                                  const next = { ...current };
                                  delete next[setting.id];
                                  return next;
                                });
                                setRawErrors((current) => {
                                  const next = { ...current };
                                  delete next[setting.id];
                                  return next;
                                });
                              }
                              setRawErrors((current) => {
                                const next = { ...current };
                                if (
                                  setting.id === 'agent_name' &&
                                  typeof value === 'string' &&
                                  !value.trim()
                                )
                                  next[setting.id] = 'Enter a nonempty agent name.';
                                else delete next[setting.id];
                                return next;
                              });
                              stage(setting, { id: setting.id, action: 'value', value });
                            }}
                            onRawChange={(text, error) => {
                              setRawDrafts((current) => ({ ...current, [setting.id]: text }));
                              setRawErrors((current) => {
                                const next = { ...current };
                                if (error) next[setting.id] = error;
                                else delete next[setting.id];
                                return next;
                              });
                            }}
                          />
                        </div>
                      </div>
                      <div className="flex flex-wrap items-center justify-between gap-2">
                        <Text variant="meta">
                          {preview.is_override
                            ? 'Set here'
                            : `Inherited from ${preview.source ?? 'default'}`}{' '}
                          · Applies{' '}
                          {setting.applies === 'next_call'
                            ? 'on the next tool call'
                            : setting.applies === 'immediate'
                              ? 'immediately'
                              : setting.applies === 'restart'
                                ? 'after restart'
                                : setting.applies === 'reload'
                                  ? 'after reload'
                                  : 'on the next turn'}
                        </Text>
                        <Button
                          variant="secondary"
                          density="panel"
                          disabled={draft.busy}
                          onClick={() => {
                            stage(
                              setting,
                              preview.is_override
                                ? { id: setting.id, action: 'inherit' }
                                : { id: setting.id, action: 'value', value: settingValue(preview) },
                            );
                          }}
                        >
                          {' '}
                          {preview.is_override ? 'Use inherited value' : 'Set value here'}
                        </Button>
                      </div>
                      {setting.is_override && (
                        <Text as="p" variant="meta">
                          Without this override: {formatSettingValue(setting.inherited_value)} from{' '}
                          {setting.inherited_source ?? 'default'}
                        </Text>
                      )}
                      {setting.overridden_by && (
                        <Banner kind="warn">
                          This session uses its {setting.overridden_by.scope} override. Changing
                          this ancestor does not replace that value.
                          {onTargetChange && (
                            <Button
                              variant="secondary"
                              density="panel"
                              onClick={() => {
                                const owner = owners.find(
                                  (owner) => owner.scope === setting.overridden_by?.scope,
                                );
                                if (owner) requestLeave(() => onTargetChange(owner));
                              }}
                            >
                              Open winning scope
                            </Button>
                          )}
                        </Banner>
                      )}
                      {fieldError && (
                        <Banner kind="err">
                          <Text as="p" variant="description" tone="inherit" role="alert">{fieldError}</Text>
                        </Banner>
                      )}
                    </div>
                  );
                })}
              </div>
            </SettingsPanel>
          ))}
        </div>
        {draft.data && resources?.(selectedCategory, search)}
      </div>
      <div
        className="shrink-0 space-y-2 border-t border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4"
        aria-live="polite"
      >
        {leaveAction && (
          <div className="space-y-2">
            <Banner kind="warn">You have unsaved changes. Discard them before leaving?</Banner>
            <div className="flex gap-2">
              <Button variant="secondary" density="panel" onClick={() => setLeaveAction(null)}>
                Keep editing
              </Button>
              <Button
                variant="secondary"
                density="panel"
                onClick={() => {
                  discard();
                  setLeaveAction(null);
                  leaveAction();
                }}
              >
                Discard and leave
              </Button>
            </div>
          </div>
        )}
        {count > 0 && (
          <details>
            <summary className="cursor-pointer text-sm">
              Review {count} {count === 1 ? 'change' : 'changes'}
            </summary>
            <ul className="max-h-40 space-y-2 overflow-y-auto py-2 text-sm">
              {Object.values(draft.changes).map((change) => {
                const setting = allSettings.find((setting) => setting.id === change.id);
                return (
                  <li key={change.id}>
                    <strong>{setting?.label ?? change.id}</strong>:{' '}
                    {formatSettingValue(setting && settingValue(setting))} →{' '}
                    {change.action === 'inherit'
                      ? `Inherited from ${setting?.inherited_source ?? 'default'}`
                      : formatSettingValue(change.value)}
                    {(typeof change.value === 'object' || typeof setting?.value === 'object') && (
                      <pre className="whitespace-pre-wrap break-all text-xs">
                        {JSON.stringify(
                          {
                            before: setting?.value,
                            after:
                              change.action === 'inherit' ? setting?.inherited_value : change.value,
                          },
                          null,
                          2,
                        )}
                      </pre>
                    )}
                  </li>
                );
              })}
            </ul>
          </details>
        )}
        {permissionChanges && (
          <label className="flex items-start gap-2 text-sm">
            <input
              type="checkbox"
              checked={permissionReview}
              onChange={(event) => setPermissionReview(event.target.checked)}
              disabled={draft.busy}
            />
            I reviewed the permission changes. They apply on the next turn. Running calls keep their
            current permissions.
          </label>
        )}
        <div className="flex flex-wrap items-center justify-between gap-3">
          <Text variant="description" role="status">
            {draft.busy
              ? 'Applying changes…'
              : draft.saved
                ? 'Changes applied.'
                : draft.dirty
                  ? `${count} changes not yet applied`
                  : 'No unsaved changes'}
            {!draft.loaded && draft.data ? ' · Offline or reconnecting' : ''}
          </Text>
          <div className="flex gap-2">
            <Button
              variant="secondary"
              density="panel"
              disabled={!draft.dirty || draft.busy}
              onClick={discard}
            >
              Discard changes
            </Button>
            <Button
              density="panel"
              disabled={
                !count ||
                draft.busy ||
                !draft.loaded ||
                !draft.data?.revision ||
                !!draft.latest ||
                Object.keys(rawErrors).length > 0 ||
                (permissionChanges && !permissionReview)
              }
              onClick={() => void apply()}
            >
              Apply changes
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
