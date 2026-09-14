import { lazy, Suspense, useEffect, useState } from 'react';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';
import { onWake } from '../lib/wake';
import { IconButton } from './ui';
import { ImproveIcon } from './icons';

const ImproveDialog = lazy(async () => ({
  default: (await import('../screens/ImproveScreen')).ImproveDialog,
}));

/** The global entry is present only when a paired machine has Improve enabled. */
export function ImproveLauncher({
  gateways,
  primaryUrl,
  refreshKey,
}: {
  gateways: GatewayConn[];
  primaryUrl?: string;
  refreshKey?: boolean;
}) {
  const [enabled, setEnabled] = useState<string[]>([]);
  const [opened, setOpened] = useState(false);
  const [revision, setRevision] = useState(0);
  useEffect(() => onWake(() => setRevision((value) => value + 1)), []);
  useEffect(() => {
    const controller = new AbortController();
    void Promise.all(
      gateways.map(async (gateway) => {
        try {
          const settings = await new GatewayClient(gateway).improveSettings(controller.signal);
          return {
            url: gateway.url,
            enabled: settings.mode === 'human' || settings.mode === 'automatic',
          };
        } catch {
          return { url: gateway.url, enabled: null };
        }
      }),
    ).then((answers) => {
      if (controller.signal.aborted) return;
      setEnabled((previous) =>
        answers
          .filter((answer) => answer.enabled ?? previous.includes(answer.url))
          .map((answer) => answer.url),
      );
    });
    return () => controller.abort();
  }, [gateways, refreshKey, revision]);
  const initialUrl = enabled.includes(primaryUrl ?? '') ? primaryUrl : enabled[0];
  return (
    <>
      {enabled.length > 0 && (
        <IconButton label="Open Improve" title="Improve" onClick={() => setOpened(true)}>
          <ImproveIcon className="size-4" />
        </IconButton>
      )}
      {opened && (
        <Suspense fallback={null}>
          <ImproveDialog
            gateways={gateways}
            initialUrl={initialUrl}
            onClose={() => {
              setOpened(false);
              setRevision((value) => value + 1);
            }}
          />
        </Suspense>
      )}
    </>
  );
}
