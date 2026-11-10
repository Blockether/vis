import { useContext, useState } from 'react';
import { Button, Text } from '../../components/ui';
import type { ModelPref, SettingValue, Toggle } from '../../lib/types';
import { ProviderRouterDialog } from '../RouterScreen';
import { SettingMachine } from './SettingField';

/** The model that a model row names, or null for `{}` or a broken value. */
export function modelValue(value: SettingValue | undefined): ModelPref | null {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return null;
  const { provider, model } = value;
  return typeof provider === 'string' && provider && typeof model === 'string' && model ? { provider, model } : null;
}

/**
 * A setting whose value is one chat model, or `{}` for none. The row shows
 * `provider/model`; Change opens the session model picker in choose mode, so the
 * choices are the same models, and its first entry writes `{}`.
 */
export function ModelField({
  toggle,
  busy,
  disabled = false,
  onSave,
}: {
  toggle: Toggle;
  busy: boolean;
  /** Locked by a more specific scope: the value shows, but cannot change. */
  disabled?: boolean;
  onSave: (value: SettingValue) => Promise<boolean>;
}) {
  const machine = useContext(SettingMachine);
  const [isOpen, setOpen] = useState(false);
  const current = modelValue(toggle.value);
  return (
    <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-x-4 gap-y-2 px-3 py-2 sm:px-4">
      <div className="min-w-0">
        <Text as="p" variant="label" className="break-words">{toggle.label}</Text>
        {toggle.description && (
          <Text as="p" variant="description" className="mt-0.5 break-words">{toggle.description}</Text>
        )}
        <Text as="p" variant="meta" className="mt-1 break-all" aria-label={`${toggle.label} value`}>
          {current ? `${current.provider}/${current.model}` : 'Not set'}
        </Text>
      </div>
      <Button
        variant="secondary"
        density="panel"
        className="self-center"
        aria-label={`Change ${toggle.label}`}
        aria-busy={busy}
        disabled={busy || disabled || !machine}
        onClick={() => setOpen(true)}
      >
        Change
      </Button>
      {isOpen && machine && (
        <ProviderRouterDialog
          client={machine.client}
          chosen={current ?? {}}
          onChoose={(pref) => onSave(pref.provider && pref.model ? { provider: pref.provider, model: pref.model } : {})}
          onClose={() => setOpen(false)}
        />
      )}
    </div>
  );
}
