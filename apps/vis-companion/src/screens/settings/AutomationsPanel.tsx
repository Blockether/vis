import { lazy, Suspense, useContext, useEffect, useState } from 'react';
import { IconButton } from '../../components/ui';
import { MinusIcon, PlusIcon } from '../../components/icons';
import type { AutomationsClient, AutomationsForm } from '../AutomationsScreen';
import { SettingsBandsOpen, SettingsPanel } from './SettingsLayout';

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
  const bandsOpen = useContext(SettingsBandsOpen);
  const [isAvailable, setIsAvailable] = useState(false);
  const [isOpen, setIsOpen] = useState(bandsOpen);
  // The band owns the form, so its header + opens it like the + of the other bands.
  const [form, setForm] = useState<AutomationsForm>(null);
  const isCreating = form?.id === null;
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
  const createLabel = isCreating ? 'Close the new automation' : 'New automation';
  return (
    <SettingsPanel
      title="Automations"
      disclosure={{
        isOpen,
        onToggle: () => setIsOpen((open) => !open),
        label: `${isOpen ? 'Hide' : 'Show'} automations`,
      }}
      action={
        <IconButton
          variant="quiet"
          align="trailing"
          label={createLabel}
          title={createLabel}
          aria-expanded={isCreating}
          onClick={() => setForm(isCreating ? null : { id: null })}
        >
          {isCreating ? <MinusIcon className="size-4" /> : <PlusIcon className="size-4" />}
        </IconButton>
      }
    >
      <Suspense fallback={null}>
        <AutomationsWorkspace
          client={client}
          gatewayUrl={gatewayUrl}
          form={form}
          onForm={setForm}
        />
      </Suspense>
    </SettingsPanel>
  );
}
