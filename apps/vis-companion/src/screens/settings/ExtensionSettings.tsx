import { useId, useState, type ReactNode } from 'react';
import { RefreshIcon } from '../../components/icons';
import { Banner, IconButton, Text } from '../../components/ui';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { SettingsTarget, ToggleGroup } from '../../lib/types';
import { SettingsPanel } from './SettingsLayout';

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
 * One extension under the Extensions heading: its name, its scope, a load error and its
 * settings. The left rail draws the depth, so the name never reads as a band of its own.
 */
function ExtensionGroup({
  group,
  headingLevel,
  children,
}: {
  group: ToggleGroup;
  headingLevel: 4 | 5;
  children: ReactNode;
}) {
  const headingId = useId();
  const scope = extensionScope(group);
  return (
    <section
      aria-labelledby={headingId}
      className="min-w-0 divide-y divide-dialog-edge border-l-2 border-dialog-edge"
    >
      <header className="flex min-w-0 items-baseline gap-3 px-3 pb-1.5 pt-3 sm:px-4">
        <Text
          as={headingLevel === 4 ? 'h4' : 'h5'}
          id={headingId}
          variant="section"
          className="min-w-0 flex-auto truncate"
        >
          {group.title}
        </Text>
        {scope && <Text variant="meta">{scope}</Text>}
      </header>
      <ExtensionNotice group={group} />
      {children}
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
  renderSettings,
}: {
  client: GatewayClient;
  target?: SettingsTarget;
  /** Extension sections in catalog order. */
  groups: ToggleGroup[];
  /** A search shows only the matching sections, without the reload button. */
  hasActions?: boolean;
  onRefresh: () => Promise<unknown>;
  /** The setting rows of one extension; the dialog that saves them draws them. */
  renderSettings: (group: ToggleGroup) => ReactNode;
}) {
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState<{ kind: 'ok' | 'warn' | 'err'; text: string } | null>(null);
  const scoped = Boolean(target && target.scope !== 'global');
  const headingLevel = scoped ? 3 : 4;

  const reload = async () => {
    setBusy(true);
    setResult(null);
    try {
      const counts = await client.reloadExtensions(target);
      await onRefresh();
      setResult({
        kind: counts.failed > 0 ? 'warn' : 'ok',
        text: `${counts.loaded} loaded, ${counts.failed} failed.${
          counts.failed > 0 ? ' Each failed extension shows its error.' : ''
        }`,
      });
    } catch (error) {
      setResult({ kind: 'err', text: actionError(error) });
    } finally {
      setBusy(false);
    }
  };

  return (
    <SettingsPanel
      title="Extensions"
      headingLevel={headingLevel}
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
      {hasActions && result && (
        <div className="px-4 py-3">
          <Banner kind={result.kind}>{result.text}</Banner>
        </div>
      )}
      {groups.map((group) => (
        <ExtensionGroup key={group.id} group={group} headingLevel={headingLevel === 3 ? 4 : 5}>
          {renderSettings(group)}
        </ExtensionGroup>
      ))}
    </SettingsPanel>
  );
}
