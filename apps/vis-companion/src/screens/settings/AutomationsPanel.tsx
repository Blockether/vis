import { lazy, Suspense, useEffect, useState } from 'react';
import type { AutomationsClient } from '../AutomationsScreen';
import { SettingsPanel } from './SettingsLayout';

const AutomationsWorkspace = lazy(async () => ({
  default: (await import('../AutomationsScreen')).AutomationsWorkspace,
}));

/**
 * AUTOMATIONS ARE A MACHINE SETTING. They run on one machine, with its models and
 * folders, so they stand under that machine's row in Settings, not in the app bar.
 * A machine that does not answer the automations request (an older or unavailable
 * gateway) shows no band. The band opens closed, so the other settings stay near.
 */
export function AutomationsPanel({
  client,
  gatewayUrl,
}: {
  client: AutomationsClient;
  /** The address this app uses for the machine; it completes the webhook path. */
  gatewayUrl?: string;
}) {
  const [isAvailable, setIsAvailable] = useState(false);
  const [isOpen, setIsOpen] = useState(false);
  useEffect(() => {
    const controller = new AbortController();
    client
      .automations(controller.signal)
      .then((list) => {
        if (!controller.signal.aborted) setIsAvailable(Array.isArray(list?.automations));
      })
      .catch(() => {});
    return () => controller.abort();
  }, [client]);
  if (!isAvailable) return null;
  return (
    <SettingsPanel
      title="Automations"
      disclosure={{
        isOpen,
        onToggle: () => setIsOpen((open) => !open),
        label: `${isOpen ? 'Hide' : 'Show'} automations`,
      }}
    >
      <Suspense fallback={null}>
        <AutomationsWorkspace client={client} gatewayUrl={gatewayUrl} />
      </Suspense>
    </SettingsPanel>
  );
}
