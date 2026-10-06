import { useCallback, useEffect, useState } from 'react';

import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import { FastIcon, ReasoningIcon, ThinkingIcon, VerbosityIcon } from './icons';
import { keepKeyboard } from '../lib/keyboard';
import { Menu, MENU_WIDTH, MenuItem } from './Menu';
import { MetaButton } from './ui';

type CycleControl = {
  label: string;
  value: string;
  busy: boolean;
  cycle: () => void | Promise<void>;
};

type ChoiceControl = {
  label: string;
  value: string;
  /** Every level the next turn can use, lightest first. */
  choices: readonly string[];
  busy: boolean;
  choose: (value: string) => void | Promise<void>;
};

type ToggleControl = {
  enabled: boolean;
  busy: boolean;
  toggle: () => void | Promise<void>;
};

export type ComposerResponseControlsModel = {
  model: {
    value: string;
    title: string;
    choose: () => void;
  };
  /** A cycle for the simplified modes, or a list of the exact levels of the model. */
  reasoning?: ChoiceControl | CycleControl;
  verbosity?: CycleControl;
  thinking?: ToggleControl;
  fast?: ToggleControl;
};

function Divider() {
  return <span aria-hidden="true" className="h-2.5 w-px shrink-0 bg-dialog-edge mouse:h-2" />;
}

/**
 * With simplified thinking modes off, the reasoning chip opens the list of levels,
 * and a tap on a row picks that level. A provider can offer six or more exact levels.
 */
function ReasoningChoice({ control }: { control: ChoiceControl }) {
  const [menu, setMenu] = useState<MenuPosition | null>(null);
  const close = useCallback(() => setMenu(null), []);
  // Escape belongs to the open list. The screen under it reads Escape as "cancel
  // the running turn", so the list catches the key first.
  useEffect(() => {
    if (!menu) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      close();
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [menu, close]);

  return (
    <>
      <MetaButton
        isPicker
        density="compact"
        className="shrink-0"
        onMouseDown={keepKeyboard}
        onClick={(event) =>
          setMenu(menuPosition(event.currentTarget.getBoundingClientRect(), MENU_WIDTH))
        }
        disabled={control.busy}
        aria-busy={control.busy}
        aria-haspopup="dialog"
        aria-expanded={menu !== null}
        aria-live="polite"
        aria-label={`${control.label} — ${control.value}, choose a level`}
        title={`${control.label}: ${control.value} — choose a level`}
      >
        <ReasoningIcon className="size-3" />
        <span
          key={control.value}
          className="inline-block animate-chip-swap motion-reduce:animate-none"
        >
          {control.value}
        </span>
      </MetaButton>
      {menu && (
        <Menu label={control.label} at={menu} onDismiss={close}>
          {control.choices.map((choice) => (
            <MenuItem
              key={choice}
              title={choice}
              badge={choice === control.value ? 'current' : undefined}
              onSelect={() => {
                close();
                if (choice !== control.value) void control.choose(choice);
              }}
            />
          ))}
        </Menu>
      )}
    </>
  );
}

/** The simplified modes have three levels, so a tap on the chip steps to the next one. */
function ReasoningCycle({ control }: { control: CycleControl }) {
  return (
    <MetaButton
      density="compact"
      className="shrink-0"
      onMouseDown={keepKeyboard}
      onClick={() => void control.cycle()}
      disabled={control.busy}
      aria-busy={control.busy}
      aria-live="polite"
      aria-label={`${control.label} — ${control.value}, tap for the next level`}
      title={`${control.label}: ${control.value} — tap to cycle`}
    >
      <ReasoningIcon className="size-3" />
      <span
        key={control.value}
        className="inline-block animate-chip-swap motion-reduce:animate-none"
      >
        {control.value}
      </span>
    </MetaButton>
  );
}

/** Provider and response knobs that apply to the next submitted turn. */
export function ComposerResponseControls({
  controls,
}: {
  controls: ComposerResponseControlsModel;
}) {
  return (
    <div className="flex w-full items-center gap-1 pt-1 mouse:gap-1.5 mouse:pt-0.5">
      <MetaButton
        isPicker
        density="compact"
        className="min-w-0 shrink"
        onClick={controls.model.choose}
        aria-label="Change provider and model"
        title={controls.model.title}
      >
        <span className="truncate">{controls.model.value}</span>
      </MetaButton>

      {controls.reasoning && (
        <>
          <Divider />
          {'cycle' in controls.reasoning ? (
            <ReasoningCycle control={controls.reasoning} />
          ) : (
            <ReasoningChoice control={controls.reasoning} />
          )}
        </>
      )}

      {controls.verbosity && (
        <>
          <Divider />
          <MetaButton
            density="compact"
            className="shrink-0"
            onMouseDown={keepKeyboard}
            onClick={() => void controls.verbosity?.cycle()}
            disabled={controls.verbosity.busy}
            aria-busy={controls.verbosity.busy}
            aria-live="polite"
            aria-label={`${controls.verbosity.label} — ${controls.verbosity.value}, tap for the next level`}
            title={`${controls.verbosity.label}: ${controls.verbosity.value} — tap to cycle`}
          >
            <VerbosityIcon className="size-3" />
            {controls.verbosity.value}
          </MetaButton>
        </>
      )}

      {controls.thinking && (
        <>
          <Divider />
          <MetaButton
            density="compact"
            className="shrink-0"
            onMouseDown={keepKeyboard}
            onClick={() => void controls.thinking?.toggle()}
            disabled={controls.thinking.busy}
            aria-busy={controls.thinking.busy}
            aria-pressed={controls.thinking.enabled}
            aria-label={`Thinking summary — ${controls.thinking.enabled ? 'on' : 'off'}`}
            title={`Thinking summary: ${controls.thinking.enabled ? 'on' : 'off'}`}
          >
            <ThinkingIcon className="size-3" />
            {controls.thinking.enabled ? 'summarized' : 'omitted'}
          </MetaButton>
        </>
      )}

      {controls.fast && (
        <>
          <Divider />
          <MetaButton
            density="compact"
            className="shrink-0"
            onMouseDown={keepKeyboard}
            onClick={() => void controls.fast?.toggle()}
            disabled={controls.fast.busy}
            aria-busy={controls.fast.busy}
            aria-pressed={controls.fast.enabled}
            aria-label={`Fast mode — ${controls.fast.enabled ? 'on' : 'off'}`}
            title={`Fast mode: ${controls.fast.enabled ? 'on' : 'off'}`}
          >
            <FastIcon className="size-3" />
            {controls.fast.enabled ? 'fast' : 'standard'}
          </MetaButton>
        </>
      )}
    </div>
  );
}
