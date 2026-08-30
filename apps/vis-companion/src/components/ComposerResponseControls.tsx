import { FastIcon, ReasoningIcon, VerbosityIcon } from "./icons";
import { keepKeyboard } from "../lib/keyboard";
import { MetaButton } from "./ui";

type CycleControl = {
  label: string;
  value: string;
  busy: boolean;
  cycle: () => void | Promise<void>;
};

export type ComposerResponseControlsModel = {
  model: {
    value: string;
    title: string;
    choose: () => void;
  };
  reasoning?: CycleControl;
  verbosity?: CycleControl;
  fast?: {
    enabled: boolean;
    busy: boolean;
    toggle: () => void | Promise<void>;
  };
};

function Divider() {
  return (
    <span aria-hidden="true" className="h-2.5 w-px shrink-0 bg-dialog-edge" />
  );
}

/** Provider and response knobs that apply to the next submitted turn. */
export function ComposerResponseControls({
  controls,
}: {
  controls: ComposerResponseControlsModel;
}) {
  return (
    <div className="flex w-full items-center gap-2.5 pt-1">
      <MetaButton
        isPicker
        className="min-w-0 shrink truncate"
        onClick={controls.model.choose}
        aria-label="Change provider and model"
        title={controls.model.title}
      >
        {controls.model.value}
      </MetaButton>

      {controls.reasoning && (
        <>
          <Divider />
          <MetaButton
            className="shrink-0"
            onMouseDown={keepKeyboard}
            onClick={() => void controls.reasoning?.cycle()}
            disabled={controls.reasoning.busy}
            aria-busy={controls.reasoning.busy}
            aria-live="polite"
            aria-label={`${controls.reasoning.label} — ${controls.reasoning.value}, tap for the next level`}
            title={`${controls.reasoning.label}: ${controls.reasoning.value} — tap to cycle`}
          >
            <ReasoningIcon className="size-3" />
            <span
              key={controls.reasoning.value}
              className="inline-block animate-chip-swap motion-reduce:animate-none"
            >
              {controls.reasoning.value}
            </span>
          </MetaButton>
        </>
      )}

      {controls.verbosity && (
        <>
          <Divider />
          <MetaButton
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

      {controls.fast && (
        <>
          <Divider />
          <MetaButton
            className="shrink-0"
            onMouseDown={keepKeyboard}
            onClick={() => void controls.fast?.toggle()}
            disabled={controls.fast.busy}
            aria-busy={controls.fast.busy}
            aria-pressed={controls.fast.enabled}
            aria-label={`Fast mode — ${controls.fast.enabled ? "on" : "off"}`}
            title={`Fast mode: ${controls.fast.enabled ? "on" : "off"}`}
          >
            <FastIcon className="size-3" />
            {controls.fast.enabled ? "fast" : "standard"}
          </MetaButton>
        </>
      )}
    </div>
  );
}
