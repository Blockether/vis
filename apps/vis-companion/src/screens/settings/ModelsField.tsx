import { useContext, useState } from 'react';
import { Button, Text } from '../../components/ui';
import type { ModelPref, SettingValue, Toggle } from '../../lib/types';
import { ProviderRouterDialog } from '../RouterScreen';
import { SettingMachine } from './SettingField';

type Model = { provider: string; model: string };

/** One entry of a models row, or null for an entry that names no model. */
function modelValue(value: SettingValue): Model | null {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return null;
  const { provider, model } = value;
  return typeof provider === 'string' && provider && typeof model === 'string' && model ? { provider, model } : null;
}

/** The models that a models row lists, in order; entries that name no model drop out. */
export function modelsValue(value: SettingValue | undefined): Model[] {
  if (!Array.isArray(value)) return [];
  return value.map(modelValue).filter((entry): entry is Model => entry !== null);
}

const same = (a: Model, b: ModelPref) => a.provider === b.provider && a.model === b.model;

/**
 * A setting whose value is an ordered list of chat models; `[]` names none. The
 * first model is the default. Add opens the session model picker in choose mode,
 * so the choices are the same models.
 */
export function ModelsField({
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
  const models = modelsValue(toggle.value);
  const isLocked = busy || disabled || !machine;
  const save = (next: Model[]) => onSave(next as unknown as SettingValue);
  return (
    <div className="min-w-0 px-3 py-2 sm:px-4">
      <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-x-4">
        <div className="min-w-0">
          <Text as="p" variant="label" className="break-words">{toggle.label}</Text>
          {toggle.description && (
            <Text as="p" variant="description" className="mt-0.5 break-words">{toggle.description}</Text>
          )}
        </div>
        <Button
          variant="secondary"
          density="panel"
          className="self-center"
          aria-label={`Add a model to ${toggle.label}`}
          aria-busy={busy}
          disabled={isLocked}
          onClick={() => setOpen(true)}
        >
          Add
        </Button>
      </div>
      <ul className="mt-2 grid gap-1" aria-label={`${toggle.label} value`}>
        {models.length === 0 && (
          <li><Text as="p" variant="meta">Not set</Text></li>
        )}
        {models.map((entry, index) => {
          const name = `${entry.provider}/${entry.model}`;
          return (
            <li key={name} className="flex min-w-0 items-center gap-2">
              <Text as="span" variant="meta" className="min-w-0 flex-1 break-all">
                {name}{index === 0 && ' (default)'}
              </Text>
              {index > 0 && (
                <Button
                  variant="quiet"
                  density="panel"
                  aria-label={`Make ${name} the default`}
                  disabled={isLocked}
                  onClick={() => void save([entry, ...models.filter((_, at) => at !== index)])}
                >
                  Make default
                </Button>
              )}
              <Button
                variant="remove"
                density="panel"
                aria-label={`Remove ${name}`}
                disabled={isLocked}
                onClick={() => void save(models.filter((_, at) => at !== index))}
              >
                Remove
              </Button>
            </li>
          );
        })}
      </ul>
      {isOpen && machine && (
        <ProviderRouterDialog
          client={machine.client}
          onChoose={async (pref) =>
            !pref.provider || !pref.model || models.some((entry) => same(entry, pref))
              ? true
              : save([...models, { provider: pref.provider, model: pref.model }])}
          onClose={() => setOpen(false)}
        />
      )}
    </div>
  );
}
