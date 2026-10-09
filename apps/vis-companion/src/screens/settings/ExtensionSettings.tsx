import { useContext, useId, useState, type ReactNode } from 'react';
import { RefreshIcon } from '../../components/icons';
import { Banner, IconButton, Text } from '../../components/ui';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { ExtensionReload, SettingsTarget, Toggle, ToggleGroup } from '../../lib/types';
import { SettingsNested, SettingsPanel } from './SettingsLayout';

/** Extension sections stand under Extensions. Older gateways do not mark them. */
export function isExtensionGroup(group: ToggleGroup): boolean {
  return Boolean(group.extension);
}

/** Where an extension is installed. Built-in extensions ship with Vis and need no scope. */
export function extensionScope(group: ToggleGroup): 'project' | 'global' | undefined {
  const origin = group.extension?.origin;
  return origin === 'project' || origin === 'global' ? origin : undefined;
}

/** A failed load keeps its section visible, even when it has no settings. */
export function hasExtensionNotice(group: ToggleGroup): boolean {
  return Boolean(group.extension && group.extension.status !== 'loaded');
}

/**
 * The reload report: each scanned scope names its directory and what it loaded. A machine
 * reload says that project extensions need their own reload. Issue #348.
 */
export function extensionReloadStatus(result: ExtensionReload, global: boolean): string {
  const scopes = result.scopes ?? [];
  const lines = scopes.length
    ? scopes.map((entry) => {
        const label = entry.scope === 'project' ? 'project extensions' : 'machine extensions';
        const dir = entry.dirs.join(', ');
        if (entry.loaded + entry.failed === 0) return `No ${label} found in ${dir}.`;
        const shown = entry.extensions.slice(0, 8);
        const more = entry.extensions.length - shown.length;
        const names = shown.length ? ` (${shown.join(', ')}${more > 0 ? ` and ${more} more` : ''})` : '';
        const title = label.charAt(0).toUpperCase() + label.slice(1);
        return `${title} (${dir}): ${entry.loaded} loaded${names}, ${entry.failed} failed.`;
      })
    : [`${result.loaded} loaded, ${result.failed} failed.`];
  if (global) lines.push('Project extensions were not reloaded. Use Project settings → Reload or /reload.');
  if (result.failed > 0) lines.push('Each failed extension shows its error.');
  return lines.join(' ');
}

/** The load error of one extension section, or nothing while it loads normally. */
export function ExtensionNotice({ group }: { group: ToggleGroup }) {
  const extension = group.extension;
  if (!extension || extension.status === 'loaded') return null;
  const stale = extension.status === 'stale';
  return (
    <div className="p-3 sm:p-4">
      <Banner kind={stale ? 'warn' : 'err'}>
        {stale ? 'Reload failed. Vis uses the last loaded version.' : 'Extension failed to load.'}
        {extension.error ? ` ${extension.error}` : ''}
      </Banner>
    </div>
  );
}

/**
 * The row that names an extension: its label is the heading, with the install scope beside it.
 * The extension's Auto/On/Off choice is this row, so the name shows once.
 */
export interface SettingHead {
  id: string;
  level: 4 | 5 | 6;
  scope?: 'project' | 'global';
}

/** The Auto/On/Off choice of an optional tool extension. The gateway names it after the extension. */
export function isEngineToggle(toggle: Toggle): boolean {
  return toggle.id.startsWith('engines_');
}

/** A skill that the extension packages. The gateway gives its row a `skills_` id. */
function isSkillToggle(toggle: Toggle): boolean {
  return toggle.id.startsWith('skills_');
}

/** A packaged skill repeats its extension's name, for example `vis-spel/browser` under `vis-spel`. */
function memberLabel(group: ToggleGroup, label: string): string {
  const prefix = `${group.title}/`;
  return label.startsWith(prefix) && label.length > prefix.length ? label.slice(prefix.length) : label;
}

/**
 * One extension under the Extensions heading. Its first row names it and shows its scope: the
 * Auto/On/Off choice, or only the name when the extension has no such choice. A load error
 * follows, then the other settings one step in, and last its packaged skills under Skills.
 * The indent draws the depth.
 */
function ExtensionGroup({
  group,
  headingLevel,
  renderSetting,
}: {
  group: ToggleGroup;
  headingLevel: 4 | 5 | 6;
  renderSetting: (toggle: Toggle, head?: SettingHead) => ReactNode;
}) {
  const headingId = useId();
  const skillsId = useId();
  const scope = extensionScope(group);
  const engine = group.toggles.find(isEngineToggle);
  const members = group.toggles.filter((toggle) => toggle !== engine && !isSkillToggle(toggle));
  const skills = group.toggles.filter(isSkillToggle);
  const memberRow = (toggle: Toggle) => renderSetting({ ...toggle, label: memberLabel(group, toggle.label) });
  return (
    <section aria-labelledby={headingId} className="min-w-0 divide-y divide-dialog-edge">
      {engine ? (
        renderSetting(engine, { id: headingId, level: headingLevel, scope })
      ) : (
        <div className="flex min-w-0 items-baseline gap-3 px-3 py-2 sm:px-4">
          <Text
            as={headingLevel === 4 ? 'h4' : headingLevel === 5 ? 'h5' : 'h6'}
            id={headingId}
            variant="label"
            className="min-w-0 flex-auto truncate"
          >
            {group.title}
          </Text>
          {scope && <Text variant="meta">{scope}</Text>}
        </div>
      )}
      <ExtensionNotice group={group} />
      {(members.length > 0 || skills.length > 0) && (
        <div className="divide-y divide-dialog-edge ps-3 sm:ps-4">
          {members.map(memberRow)}
          {skills.length > 0 && (
            <section aria-labelledby={skillsId} className="min-w-0">
              <div className="px-3 pt-2 sm:px-4">
                <Text role="heading" aria-level={headingLevel + 1} id={skillsId} variant="meta" className="block">
                  Skills
                </Text>
              </div>
              <div className="divide-y divide-dialog-edge">{skills.map(memberRow)}</div>
            </section>
          )}
        </div>
      )}
    </section>
  );
}

function actionError(error: unknown): string {
  const type = error instanceof GatewayError && error.status === 404
    ? (error.body as { error?: { type?: unknown } } | undefined)?.error?.type
    : undefined;
  return type === 'not-found'
    ? 'This gateway does not support extension reload. Update Vis on that machine.'
    : (error as Error).message;
}

/**
 * Every extension section stands under one Extensions heading. Its reload button runs trusted
 * extension code where these settings apply, then reads the settings catalog again.
 */
export function ExtensionsPanel({
  client,
  target,
  groups,
  hasActions = true,
  onRefresh,
  renderSetting,
}: {
  client: GatewayClient;
  target?: SettingsTarget;
  /** Extension sections in catalog order. */
  groups: ToggleGroup[];
  /** A search shows only the matching sections, without the reload button. */
  hasActions?: boolean;
  onRefresh: () => Promise<unknown>;
  /** One setting row; the dialog that saves it draws it. `head` marks the row that names its extension. */
  renderSetting: (toggle: Toggle, head?: SettingHead) => ReactNode;
}) {
  const [busy, setBusy] = useState(false);
  const [status, setStatus] = useState<string | null>(null);
  const scoped = Boolean(target && target.scope !== 'global');
  const headingLevel = scoped ? 3 : 4;
  // Inside a section band the Extensions heading is one level lower, and so are its extensions.
  const isNested = useContext(SettingsNested);
  const groupLevel = ((isNested ? headingLevel + 1 : headingLevel) + 1) as 4 | 5 | 6;
  const reload = async () => {
    setBusy(true);
    setStatus('Reloading…');
    try {
      const result = await client.reloadExtensions(target);
      await onRefresh();
      setStatus(extensionReloadStatus(result, !scoped));
    } catch (error) {
      setStatus(actionError(error));
    } finally {
      setBusy(false);
    }
  };

  return (
    <SettingsPanel
      title="Extensions"
      headingLevel={headingLevel}
      /* The reload result stands as plain text in the header, beside its button. */
      meta={hasActions && status ? <span role="status">{status}</span> : undefined}
      action={
        hasActions ? (
          <IconButton
            variant="quiet"
            align="trailing"
            label="Reload extensions"
            disabled={busy}
            onClick={() => void reload()}
          >
            <RefreshIcon isBusy={busy} className="size-4" />
          </IconButton>
        ) : undefined
      }
    >
      {groups.map((group) => (
        <ExtensionGroup
          key={group.id}
          group={group}
          headingLevel={groupLevel}
          renderSetting={renderSetting}
        />
      ))}
    </SettingsPanel>
  );
}
