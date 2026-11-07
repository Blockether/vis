import { useContext, useId, useState } from 'react';
import { Button, ConfirmRow, IconButton, Input, Select, Text } from '../../components/ui';
import { PlayIcon } from '../../components/icons';
import type { CustomPreset, PresetsOptions, PresetsValue, SettingValue, Toggle } from '../../lib/types';
import { SettingMachine } from './SettingField';
import { FormLabel } from './SettingsLayout';

/** The value of a presets row, with a missing or broken part read as empty. */
export function presetsValue(value: SettingValue | undefined, options: PresetsOptions): PresetsValue {
  const object = value && typeof value === 'object' && !Array.isArray(value) ? value : {};
  const custom = (Array.isArray(object.custom) ? object.custom : []).flatMap((entry) => {
    if (!entry || typeof entry !== 'object' || Array.isArray(entry) || typeof entry.name !== 'string') return [];
    const raw = entry.slots && typeof entry.slots === 'object' && !Array.isArray(entry.slots) ? entry.slots : {};
    const slots = Object.fromEntries(
      Object.entries(raw).flatMap(([slot, text]) => (typeof text === 'string' ? [[slot, text]] : [])),
    );
    return [{ name: entry.name, slots }];
  });
  const active = typeof object.active === 'string' && object.active ? object.active : (options.builtin[0]?.id ?? '');
  return { active, custom };
}

/** The slot values that a new preset starts with: a built-in preset gives its own id to each slot. */
export function presetSlots(value: PresetsValue, options: PresetsOptions): Record<string, string> {
  const custom = value.custom.find((preset) => preset.name === value.active);
  return Object.fromEntries(
    options.slots.map((slot) => [slot.id, custom ? (custom.slots[slot.id] ?? '') : value.active]),
  );
}

/** A name that no other preset has, without regard to case. */
function freeName(value: PresetsValue, options: PresetsOptions): string {
  const taken = new Set([
    ...options.builtin.flatMap((preset) => [preset.id.toLowerCase(), preset.label.toLowerCase()]),
    ...value.custom.map((preset) => preset.name.toLowerCase()),
  ]);
  for (let index = 1; ; index += 1) {
    const name = index === 1 ? 'My preset' : `My preset ${index}`;
    if (!taken.has(name.toLowerCase())) return name;
  }
}

/** Why a preset name cannot be saved, or null. The gateway checks the same rules. */
function nameProblem(name: string, value: PresetsValue, options: PresetsOptions, editing?: string): string | null {
  const wanted = name.trim().toLowerCase();
  if (!wanted) return 'Enter a name.';
  if (options.builtin.some((preset) => preset.id.toLowerCase() === wanted || preset.label.toLowerCase() === wanted))
    return 'A built-in preset has this name.';
  if (value.custom.some((preset) => preset.name !== editing && preset.name.toLowerCase() === wanted))
    return 'Another preset has this name.';
  return null;
}

type Draft = { editing?: string; name: string; slots: Record<string, string> };

/**
 * A setting whose value is a set of named presets, each with one value for every slot.
 * Built-in presets come first and cannot change; the person adds, edits and deletes
 * their own. Each change writes the whole value at once. The slots and the built-in
 * presets come from `editor_options`, so the editor knows no setting of its own.
 */
export function PresetsField({
  toggle,
  busy,
  disabled = false,
  error,
  onSave,
}: {
  toggle: Toggle;
  busy: boolean;
  /** Locked: the presets show, but cannot change. */
  disabled?: boolean;
  /** The gateway's refusal of the last write. */
  error?: string | null;
  onSave: (value: SettingValue) => Promise<boolean>;
}) {
  const options: PresetsOptions = toggle.editor_options ?? { slots: [], builtin: [] };
  const value = presetsValue(toggle.value, options);
  const machine = useContext(SettingMachine);
  const helpId = useId();
  const [draft, setDraft] = useState<Draft | null>(null);
  const [isDeleteAsked, setDeleteAsked] = useState(false);
  const [preview, setPreview] = useState<{ key: string; note?: string } | null>(null);
  const locked = disabled || busy;
  const builtin = options.builtin.find((preset) => preset.id === value.active);
  const custom = value.custom.find((preset) => preset.name === value.active);
  const isFull = options.max_custom !== undefined && value.custom.length >= options.max_custom;
  const canPreview = options.is_previewable === true && machine !== null;
  const exampleId = options.builtin[0]?.id;

  const save = (next: PresetsValue) => onSave(next as unknown as SettingValue);

  /** What one slot value plays, in words. */
  const slotText = (text: string) => {
    if (!text) return 'No sound';
    const preset = options.builtin.find((one) => one.id === text);
    return preset ? `${preset.label} (built in)` : text;
  };

  async function play(key: string, slot: string, text: string) {
    if (!machine) return;
    setPreview({ key });
    try {
      const result = await machine.client.previewSetting(toggle.id, slot, text);
      setPreview(result.is_played ? null : { key, note: 'The gateway did not play this sound.' });
    } catch (e) {
      setPreview({ key, note: (e as Error).message });
    }
  }

  const previewButton = (key: string, slot: string, label: string, text: string) =>
    canPreview && text && (
      <IconButton
        type="button"
        label={`Preview ${label}`}
        disabled={disabled || preview?.key === key && preview.note === undefined}
        aria-busy={preview?.key === key && preview.note === undefined}
        onClick={() => void play(key, slot, text)}
      >
        <PlayIcon className="size-4" />
      </IconButton>
    );

  async function submit() {
    if (!draft) return;
    const name = draft.name.trim();
    const preset: CustomPreset = { name, slots: draft.slots };
    const customs = draft.editing === undefined
      ? [...value.custom, preset]
      : value.custom.map((one) => (one.name === draft.editing ? preset : one));
    const active = draft.editing === undefined || value.active === draft.editing ? name : value.active;
    if (await save({ active, custom: customs })) setDraft(null);
  }

  async function remove(name: string) {
    const active = value.active === name ? (options.builtin[0]?.id ?? '') : value.active;
    if (await save({ active, custom: value.custom.filter((one) => one.name !== name) })) setDeleteAsked(false);
  }

  const problem = draft ? nameProblem(draft.name, value, options, draft.editing) : null;
  const choices = [
    ...options.builtin.map((preset) => ({ value: preset.id, label: `${preset.label} (built in)` })),
    ...value.custom.map((preset) => ({ value: preset.name, label: preset.name })),
  ];
  // A value that names no preset still shows, so the person sees what is saved.
  if (!choices.some((choice) => choice.value === value.active))
    choices.push({ value: value.active, label: `${value.active} (missing)` });

  return (
    <div className="flex min-w-0 flex-col gap-3 px-3 py-2 sm:px-4">
      {/* The chosen preset sits on the right, like the picker of every closed-choice row. */}
      <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-center gap-x-4 gap-y-2">
        <div className="min-w-0">
          <Text as="p" variant="label" className="break-words">{toggle.label}</Text>
          {toggle.description && (
            <Text as="p" variant="description" className="mt-0.5 break-words">{toggle.description}</Text>
          )}
        </div>
        <Select
          className="self-center"
          aria-label={toggle.label}
          aria-busy={busy}
          disabled={locked || draft !== null}
          options={choices}
          value={value.active}
          onValueChange={(active) => {
            if (active !== value.active) void save({ ...value, active });
          }}
        />
      </div>
      {draft === null && (
        // One grid for all rows, so the slot names make one column and the values another.
        <ul
          aria-label={`Sounds of ${builtin?.label ?? value.active}`}
          className={`grid min-w-0 gap-x-4 ${canPreview ? 'grid-cols-[auto_minmax(0,1fr)_auto]' : 'grid-cols-[auto_minmax(0,1fr)]'}`}
        >
          {options.slots.map((slot) => {
            const text = builtin ? builtin.id : (custom?.slots[slot.id] ?? '');
            const key = `active:${slot.id}`;
            return (
              <li key={slot.id} className="col-span-full grid min-h-8 grid-cols-subgrid items-center mouse:min-h-7">
                <Text variant="description">{slot.label}</Text>
                <Text variant="option" className="min-w-0 break-words">{slotText(text)}</Text>
                {previewButton(key, slot.id, slot.label, text)}
                {preview?.key === key && preview.note && (
                  <Text as="p" variant="description" className="col-span-full text-err" role="alert">{preview.note}</Text>
                )}
              </li>
            );
          })}
        </ul>
      )}
      {isDeleteAsked && custom ? (
        <ConfirmRow
          question={`Delete ${custom.name}?`}
          cost={`${options.builtin[0]?.label ?? 'The first built-in preset'} plays instead.`}
          keepLabel="Keep"
          confirmLabel={busy ? 'Deleting…' : 'Delete'}
          isBusy={busy}
          onKeep={() => setDeleteAsked(false)}
          onConfirm={() => void remove(custom.name)}
        />
      ) : (
        draft === null && (
          <div className="flex flex-wrap items-center gap-2">
            <Button
              type="button"
              variant="secondary"
              density="panel"
              disabled={locked || isFull}
              onClick={() => setDraft({ name: freeName(value, options), slots: presetSlots(value, options) })}
            >
              New preset
            </Button>
            {custom && (
              <>
                <Button
                  type="button"
                  variant="secondary"
                  density="panel"
                  aria-label={`Edit ${custom.name}`}
                  disabled={locked}
                  onClick={() => setDraft({ editing: custom.name, name: custom.name, slots: presetSlots(value, options) })}
                >
                  Edit
                </Button>
                <Button
                  type="button"
                  variant="secondary"
                  density="panel"
                  aria-label={`Delete ${custom.name}`}
                  disabled={locked}
                  onClick={() => setDeleteAsked(true)}
                >
                  Delete
                </Button>
              </>
            )}
          </div>
        )
      )}
      {draft && (
        <form
          aria-label={draft.editing === undefined ? 'New preset' : `Edit ${draft.editing}`}
          className="flex min-w-0 flex-col gap-3 border-t border-dialog-edge pt-3"
          onSubmit={(event) => {
            event.preventDefault();
            if (!locked && !problem) void submit();
          }}
        >
          <FormLabel label="Name">
            <Input
              value={draft.name}
              maxLength={60}
              disabled={locked}
              onChange={(event) => setDraft({ ...draft, name: event.target.value })}
            />
          </FormLabel>
          {problem && (
            <Text as="p" variant="description" className="-mt-2 text-err" role="alert">{problem}</Text>
          )}
          <Text as="p" id={helpId} variant="description">
            Leave a sound empty for silence. Enter a built-in preset id
            {exampleId ? `, such as ${exampleId},` : ''} or the path of a sound file on the gateway machine.
          </Text>
          {options.slots.map((slot) => {
            const key = `draft:${slot.id}`;
            const text = draft.slots[slot.id] ?? '';
            return (
              <div key={slot.id} className="flex min-w-0 flex-col gap-1">
                <FormLabel label={slot.label} hint={slot.description}>
                  <Input
                    value={text}
                    disabled={locked}
                    aria-describedby={helpId}
                    action={previewButton(key, slot.id, slot.label, text) || undefined}
                    onChange={(event) => setDraft({ ...draft, slots: { ...draft.slots, [slot.id]: event.target.value } })}
                  />
                </FormLabel>
                {preview?.key === key && preview.note && (
                  <Text as="p" variant="description" className="text-err" role="alert">{preview.note}</Text>
                )}
              </div>
            );
          })}
          <div className="flex flex-wrap items-center gap-2">
            <Button type="submit" density="panel" disabled={locked || problem !== null} aria-busy={busy}>
              {busy ? 'Saving…' : 'Save preset'}
            </Button>
            <Button type="button" variant="secondary" density="panel" disabled={busy} onClick={() => setDraft(null)}>
              Cancel
            </Button>
          </div>
        </form>
      )}
      {error && (
        <p className="text-err" role="alert">
          <Text variant="description" tone="inherit">{error}</Text>
        </p>
      )}
    </div>
  );
}
