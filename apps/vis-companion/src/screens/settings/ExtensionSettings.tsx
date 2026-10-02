import { useState } from 'react';
import { Banner, Button, Text } from '../../components/ui';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { SettingsTarget, ToggleGroup } from '../../lib/types';
import { SettingsPanel } from './SettingsLayout';

/** Where an extension section's code comes from. Built-in sections need no label. */
export function extensionMeta(group: ToggleGroup): string | undefined {
  const extension = group.extension;
  if (!extension || extension.origin === 'built_in') return undefined;
  const origin = extension.origin === 'project' ? 'Project extension' : 'Machine extension';
  return extension.path ? `${origin} · ${extension.path}` : origin;
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

function actionError(error: unknown): string {
  const type = error instanceof GatewayError && error.status === 404
    ? (error.body as { error?: { type?: unknown } } | undefined)?.error?.type
    : undefined;
  return type === 'not-found'
    ? 'This gateway does not support extension reload. Update Vis on that machine.'
    : (error as Error).message;
}

/**
 * Refresh reads the settings catalog again and runs no extension code. Reload runs
 * trusted extension code where these settings apply, then reads the catalog.
 */
export function ExtensionsPanel({
  client,
  target,
  onRefresh,
}: {
  client: GatewayClient;
  target?: SettingsTarget;
  onRefresh: () => Promise<unknown>;
}) {
  const [busy, setBusy] = useState<'refresh' | 'reload' | null>(null);
  const [result, setResult] = useState<{ kind: 'ok' | 'warn' | 'err'; text: string } | null>(null);
  const scoped = Boolean(target && target.scope !== 'global');

  const run = async (reload: boolean) => {
    setBusy(reload ? 'reload' : 'refresh');
    setResult(null);
    try {
      const counts = reload ? await client.reloadExtensions(target) : null;
      await onRefresh();
      setResult(
        counts
          ? {
              kind: counts.failed > 0 ? 'warn' : 'ok',
              text: `${counts.loaded} loaded, ${counts.failed} failed.${
                counts.failed > 0 ? ' Each failed extension shows its error.' : ''
              }`,
            }
          : { kind: 'ok', text: 'List refreshed. No extension code ran.' },
      );
    } catch (error) {
      setResult({ kind: 'err', text: actionError(error) });
    } finally {
      setBusy(null);
    }
  };

  return (
    <SettingsPanel title="Extensions" headingLevel={scoped ? 3 : 4}>
      <div className="flex flex-col gap-3 px-4 py-3">
        <Text as="p" variant="description">
          Refresh list reads the settings again. Reload extensions runs trusted{' '}
          {scoped ? 'machine and project' : 'machine'} extension code again. Stored settings stay
          unchanged.
        </Text>
        <div className="flex flex-wrap gap-2">
          <Button type="button" variant="secondary" density="panel" disabled={busy !== null}
            onClick={() => void run(false)}>
            {busy === 'refresh' ? 'Refreshing…' : 'Refresh list'}
          </Button>
          <Button type="button" variant="secondary" density="panel" disabled={busy !== null}
            onClick={() => void run(true)}>
            {busy === 'reload' ? 'Reloading…' : 'Reload extensions'}
          </Button>
        </div>
        {result && <Banner kind={result.kind}>{result.text}</Banner>}
      </div>
    </SettingsPanel>
  );
}
