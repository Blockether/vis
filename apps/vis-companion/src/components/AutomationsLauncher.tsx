import { lazy, Suspense, useEffect, useState } from 'react';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';
import { onWake } from '../lib/wake';
import { IconButton } from './ui';
import { AutomationsIcon } from './icons';

const AutomationsDialog = lazy(async () => ({
  default: (await import('../screens/AutomationsScreen')).AutomationsDialog,
}));

/**
 * The global entry is present only when a paired machine allows automations or still keeps
 * some, so a machine with the feature off adds no control.
 */
export function AutomationsLauncher({
  gateways,
  primaryUrl,
  refreshKey,
}: {
  gateways: GatewayConn[];
  primaryUrl?: string;
  refreshKey?: boolean;
}) {
  const [shown, setShown] = useState<string[]>([]);
  const [opened, setOpened] = useState(false);
  const [revision, setRevision] = useState(0);
  useEffect(() => onWake(() => setRevision((value) => value + 1)), []);
  useEffect(() => {
    const controller = new AbortController();
    void Promise.all(
      gateways.map(async (gateway) => {
        try {
          const list = await new GatewayClient(gateway).automations(controller.signal);
          return { url: gateway.url, shown: list.is_enabled || list.automations.length > 0 };
        } catch {
          return { url: gateway.url, shown: null };
        }
      }),
    ).then((answers) => {
      if (controller.signal.aborted) return;
      setShown((previous) =>
        answers
          .filter((answer) => answer.shown ?? previous.includes(answer.url))
          .map((answer) => answer.url),
      );
    });
    return () => controller.abort();
  }, [gateways, refreshKey, revision]);
  const initialUrl = shown.includes(primaryUrl ?? '') ? primaryUrl : shown[0];
  return (
    <>
      {shown.length > 0 && (
        <IconButton label="Open automations" title="Automations" onClick={() => setOpened(true)}>
          <AutomationsIcon className="size-4" />
        </IconButton>
      )}
      {opened && shown.length > 0 && (
        <Suspense fallback={null}>
          <AutomationsDialog
            gateways={gateways.filter((gateway) => shown.includes(gateway.url))}
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
