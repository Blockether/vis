import { useCallback, useEffect, useId, useRef, useState, type InputHTMLAttributes } from 'react';
import { useSessionActive } from '../lib/session-active';
import {
  Banner,
  Button,
  Checkbox,
  ChoiceList,
  ChoiceRow,
  ChoiceSegments,
  DialogFrame,
  Input,
  Modal,
  Select,
  ViewHeading,
  ToggleGroup,
  ViewCollapsible,
  ViewFlex,
  viewFlexChildClass,
  ViewGrid,
  ViewSection,
  ViewParagraph,
} from './ui';
import { COMMAND_ICONS } from './icons';
import { Clipboard } from '@capacitor/clipboard';
import { Capacitor } from '@capacitor/core';
import { Markdown } from './ChatContent';
import { hasSystemPasteMenu, hideSystemPasteMenu, moveSystemPasteMenu, showSystemPasteMenu } from '../lib/paste-menu';
import { GatewayError, type GatewayClient } from '../lib/gateway';
import type { SseEvent } from '../lib/types';
import { isViewContainerType, type ViewAction } from '../lib/view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyInputViewEvent,
  clampHumanInputRange,
  humanInputDatetimeMode,
  humanInputFormChange,
  humanInputFormRefused,
  humanInputFormStart,
  humanInputInputFields,
  isHumanInputSecret,
  humanInputOtp,
  humanInputOtpDigits,
  humanInputRange,
  isInputViewEvent,
  originLabel,
  toggleHumanInputOption,
  HUMAN_INPUT_CHOICE_MARKS,
  HUMAN_INPUT_DATETIME_FORMS,
  HUMAN_INPUT_SELECT_FOLD_AT,
  type HumanInputDatetimeMode,
  type HumanInputField,
  type HumanInputForm,
  type HumanInputRequest,
  type HumanInputValues,
} from '../lib/human-input';

/**
 * The range field's square track and thumb, with a finger-sized target. The input fills
 * its row, so the whole row takes a press; only the track paints the 4px line.
 */
function RangeSlider({ className = '', ...props }: InputHTMLAttributes<HTMLInputElement>) {
  return (
    <span className={`flex min-h-11 min-w-0 items-center mouse:min-h-7 ${className}`}>
      <input
        type="range"
        className="w-full min-w-0 cursor-pointer appearance-none self-stretch rounded-none bg-transparent accent-accent disabled:cursor-not-allowed [&::-webkit-slider-runnable-track]:h-1 [&::-webkit-slider-runnable-track]:bg-edge [&::-moz-range-track]:h-1 [&::-moz-range-track]:bg-edge [&::-webkit-slider-thumb]:-mt-1.5 [&::-webkit-slider-thumb]:size-4 [&::-webkit-slider-thumb]:appearance-none [&::-webkit-slider-thumb]:rounded-none [&::-webkit-slider-thumb]:border-0 [&::-webkit-slider-thumb]:bg-accent [&::-moz-range-thumb]:size-4 [&::-moz-range-thumb]:appearance-none [&::-moz-range-thumb]:rounded-none [&::-moz-range-thumb]:border-0 [&::-moz-range-thumb]:bg-accent"
        {...props}
      />
    </span>
  );
}

/** Keep non-secret answers in memory when the operator leaves an open request. */
const humanInputDrafts = new Map<string, Map<string, HumanInputValues>>();

function saveHumanInputDraft(sessionKey: string, request: HumanInputRequest, values: HumanInputValues) {
  const visible = humanInputInputFields(request.nodes).filter(
    (field) => !isHumanInputSecret(field),
  );
  if (visible.length === 0) return;
  const drafts = humanInputDrafts.get(sessionKey) ?? new Map<string, HumanInputValues>();
  drafts.set(request.id, Object.fromEntries(visible.map(({ id }) => [id, values[id]])));
  humanInputDrafts.set(sessionKey, drafts);
}

function forgetClosedHumanInputDrafts(sessionKey: string, open: readonly HumanInputRequest[]) {
  const drafts = humanInputDrafts.get(sessionKey);
  if (!drafts) return;
  const kept = new Set(open.map((row) => row.id));
  for (const id of drafts.keys()) if (!kept.has(id)) drafts.delete(id);
  if (drafts.size === 0) humanInputDrafts.delete(sessionKey);
}

type InputPromptProps = {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
};

export function HumanInputPrompt(props: InputPromptProps) {
  const sessionKey = JSON.stringify([props.client.base, props.sid]);
  return <HumanInputQueue key={sessionKey} {...props} sessionKey={sessionKey} />;
}

/** List snapshots recover missed frames. Live changes also apply to an in-flight snapshot. */
function HumanInputQueue({ client, subscriptions, sid, sessionKey }: InputPromptProps & { sessionKey: string }) {
  const [pending, setPending] = useState<HumanInputRequest[]>([]);
  const [isLoaded, setLoaded] = useState(false);
  const read = useRef<{ controller: AbortController; events: SseEvent[] } | null>(null);

  const apply = useCallback((event: SseEvent) => {
    read.current?.events.push(event);
    setPending((current) => applyInputViewEvent(current, event));
  }, []);

  useEffect(() => {
    const reload = () => {
      read.current?.controller.abort();
      const current = { controller: new AbortController(), events: [] as SseEvent[] };
      read.current = current;
      client.inputViews(sid, current.controller.signal)
        .then((requests) => {
          if (read.current !== current) return;
          setPending(current.events.reduce(applyInputViewEvent, requests));
          setLoaded(true);
        })
        .catch(() => undefined)
        .finally(() => {
          if (read.current === current) read.current = null;
        });
    };
    const stop = subscriptions.subscribeSession(sid, (event) => {
      if (event.type === 'subscription.ready') reload();
      else if (isInputViewEvent(event)) apply(event);
    }, { replay: false });
    if (!read.current) reload();
    return () => {
      stop();
      read.current?.controller.abort();
      read.current = null;
    };
  }, [apply, client, sid, subscriptions]);

  useEffect(() => {
    if (isLoaded) forgetClosedHumanInputDrafts(sessionKey, pending);
  }, [isLoaded, pending, sessionKey]);

  const close = useCallback((id: string) => {
    humanInputDrafts.get(sessionKey)?.delete(id);
    // A reply can precede the close frame, including during a list read.
    apply({ type: 'view.close', kind: 'input', session_id: sid, view_id: id });
  }, [apply, sessionKey, sid]);

  const request = pending[0];
  if (!request) return null;
  return <ActiveHumanInput key={request.id} {...{ client, sid, sessionKey, request, close }} waiting={pending.length - 1} />;
}

function preventSecretCopy(event: React.ClipboardEvent) {
  event.preventDefault();
}

function ActiveHumanInput({
  client,
  sid,
  sessionKey,
  request,
  waiting,
  close,
}: {
  client: GatewayClient;
  sid: string;
  sessionKey: string;
  request: HumanInputRequest;
  waiting: number;
  close: (id: string) => void;
}) {
  const [form, setForm] = useState<HumanInputForm>(() => {
    const initial = humanInputFormStart(request);
    const saved = humanInputDrafts.get(sessionKey)?.get(request.id);
    return saved ? { ...initial, values: { ...initial.values, ...saved } } : initial;
  });
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const sending = useRef(false);
  const bodyRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    saveHumanInputDraft(sessionKey, request, form.values);
  }, [form.values, request, sessionKey]);

  const setValue = useCallback((id: string, value: HumanInputValues[string]) => {
    setForm((current) => humanInputFormChange(current, id, value));
  }, []);

  const send = useCallback((action: ViewAction) => {
    if (sending.current) return;
    sending.current = true;
    setBusy(true);
    setError(null);
    // Only the gateway can validate an answer. Use the session that owns the queue,
    // not the child session named in the request document.
    client.viewAction(sid, request.id, action)
      .then((outcome) => {
        if (outcome?.is_accepted) close(request.id);
        else {
          setForm((current) => humanInputFormRefused(current, outcome?.errors));
          setError(action.action === 'cancel' ? 'The daemon did not cancel this request.' : 'The daemon rejected this answer.');
        }
      })
      .catch((cause: unknown) => {
        if (cause instanceof GatewayError && cause.status === 404) close(request.id);
        else setError(action.action === 'cancel' ? 'Could not cancel this request.' : 'Could not send this answer.');
      })
      .finally(() => {
        sending.current = false;
        setBusy(false);
      });
  }, [client, close, request.id, sid]);

  const submit = useCallback(() => send({ action: 'submit', values: form.values }), [form.values, send]);
  const cancel = useCallback(() => send({ action: 'cancel' }), [send]);

  // Match the TUI shortcuts. Leave handled keys with the control that took them.
  const isActive = useSessionActive();
  useEffect(() => {
    if (!isActive) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.defaultPrevented) return;
      if (event.key === 'Escape' && request.is_cancellable) {
        event.preventDefault();
        cancel();
      } else if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
        event.preventDefault();
        submit();
      }
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, [cancel, request.is_cancellable, submit, isActive]);

  // Do not raise a phone keyboard over the question when it opens.
  useEffect(() => {
    if (window.matchMedia?.('(pointer: coarse)').matches) return;
    bodyRef.current?.querySelector<HTMLElement>('input, textarea, button')?.focus();
  }, []);

  return (
    <HumanInputSheet
      request={request}
      values={form.values}
      errors={form.errors}
      error={error}
      busy={busy}
      waiting={waiting}
      bodyRef={bodyRef}
      onChange={setValue}
      onSubmit={submit}
      onCancel={cancel}
    />
  );
}

/**
 * The pause itself, with no gateway behind it: a request, the answers typed so
 * far, whatever the engine last refused, and three callbacks. The container
 * above owns the socket, this owns the pixels — which is why the design gallery
 * can photograph the SHIPPED sheet instead of a look-alike that drifts from it.
 */
export function HumanInputSheet({
  request,
  values,
  errors = {},
  error = null,
  busy = false,
  waiting = 0,
  bodyRef,
  onChange,
  onSubmit,
  onCancel,
}: {
  request: HumanInputRequest;
  values: HumanInputValues;
  /** Field id -> message, straight from the engine's refusal. Never the app's. */
  errors?: Record<string, string>;
  error?: string | null;
  busy?: boolean;
  waiting?: number;
  bodyRef?: React.RefObject<HTMLDivElement | null>;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
  onCancel: () => void;
}) {
  // A pause is a QUESTION, and a question opens in the sheet a question opens in:
  // `Modal size="fit"` rides up from the bottom edge and stops at the height its
  // own form needs. Taking the whole glass for six digit boxes made an interruption
  // read like a screen the operator had navigated to, with the verbs that end the
  // pause stranded a phone's length below the question. Above `sm:` it is the same
  // box every other dialog is, minus the fixed height.
  // The leader session shows the form of a subagent: name who asks.
  const origin = originLabel(request.origin);
  return (
    <Modal
      size="fit"
      // AND IT STANDS IN THE SESSION THAT ASKED IT. Reported from a wide window: the
      // form covered and dimmed the session list and the header beside the transcript,
      // so the whole application read as blocked while a single session waited. The
      // sheet stands in that session's own pane instead, where an opened run stands.
      within="session"
      // A pause with no way out is not dismissed by tapping the glass either: the
      // scrim answers exactly what the header's X and Esc answer, and for an
      // uncancellable request all three do nothing.
      onDismiss={request.is_cancellable ? onCancel : () => undefined}
    >
      <DialogFrame
        title={request.title}
        {...(request.is_cancellable
          ? { onClose: onCancel, closeLabel: 'Cancel this request' }
          : {})}
        footer={
          // Pinned: the banner that explains a refusal and the buttons that
          // answer it stay put while the form above them scrolls.
          <div className="space-y-2">
            {error && <Banner kind="err">{error}</Banner>}
            <div className="flex items-center justify-between gap-3">
              <span className="min-w-0 truncate text-chip">
                {waiting > 0
                  ? `${waiting} more request${waiting > 1 ? 's' : ''} waiting`
                  : 'This run is waiting for you'}
              </span>
              <span className="hidden shrink-0 text-chip sm:inline" aria-hidden="true">
                ⌘↵ submit{request.is_cancellable && ' · Esc cancel'}
              </span>
            </div>
            <div className="flex gap-2 sm:justify-end">
              {request.is_cancellable && (
                <Button
                  variant="secondary"
                  className="flex-1 sm:flex-none"
                  disabled={busy}
                  onClick={onCancel}
                >
                  {request.cancel_label}
                </Button>
              )}
              <Button className="flex-1 sm:flex-none" disabled={busy} onClick={onSubmit}>
                {busy ? 'Sending...' : request.submit_label}
              </Button>
            </div>
          </div>
        }
      >
        <div
          ref={bodyRef}
          className="max-h-[55svh] space-y-3 overflow-y-auto overscroll-contain p-4 sm:max-h-[60vh]"
        >
          {(request.description || request.source || origin) && (
            <div className="space-y-1">
              {request.description && (
                <div className="font-mono text-meta italic text-dialog-hint">
                  <Markdown compact nested hardBreaks>
                    {request.description}
                  </Markdown>
                </div>
              )}
              {/* WHO stopped the run is half the question: an answer means
                  something different to a deploy hook than to a linter. */}
              {request.source && (
                <p className="font-mono text-chip text-dialog-hint">
                  Asked by <span className="text-white">{request.source}</span>
                </p>
              )}
              {origin && (
                // One row: each title shortens with an ellipsis on its own, so a long
                // subagent title never hides the parent. Hover shows both in full.
                <p
                  data-testid="human-input-origin"
                  title={`Subagent ${origin.agent}, parent ${origin.parent}`}
                  className="flex min-w-0 items-baseline gap-[1ch] whitespace-nowrap font-mono text-chip text-dialog-hint"
                >
                  <span className="shrink-0">Subagent</span>
                  <span className="min-w-0 truncate text-white">{origin.agent}</span>
                  <span className="shrink-0">Parent</span>
                  <span className="min-w-0 truncate text-white">{origin.parent}</span>
                </p>
              )}
            </div>
          )}
          <ViewFlex>
            {/* A decoration has no name; position is its stable identity. */}
            {request.nodes.map((field, at) => (
              <HumanInputFieldRow
                key={`${request.id}:${at}:${field.id}`}
                field={field}
                values={values}
                errors={errors}
                disabled={busy}
                onChange={onChange}
                onSubmit={onSubmit}
              />
            ))}
          </ViewFlex>
        </div>
      </DialogFrame>
    </Modal>
  );
}

function HumanInputFieldLabel({ field }: { field: HumanInputField }) {
  return (
    <>
      {field.label}
      {/* The web's own mark, and the TUI band now draws the same one: a red `*`,
          not the word REQUIRED shouted beside every label. Screen readers still
          get the word. */}
      {field.is_required && (
        <>
          <span aria-hidden="true" className="ml-1 text-err">
            *
          </span>
          <span className="sr-only">, required</span>
        </>
      )}
    </>
  );
}

/** The native control that asks for each mode of a `datetime` field. */
const DATETIME_CONTROLS: Record<HumanInputDatetimeMode, 'date' | 'time' | 'datetime-local'> = {
  date: 'date',
  time: 'time',
  datetime: 'datetime-local',
};

/**
 * A choice of two to four short words fits one strip of cells, also on a phone. A long
 * word needs a row of its own.
 */
function isShortChoice(options: readonly { label: string }[]): boolean {
  return (
    options.length >= 2 &&
    options.length <= 4 &&
    options.every((option) => option.label.length <= 14) &&
    options.reduce((sum, option) => sum + option.label.length, 0) <= 32
  );
}

function FieldShell({
  field,
  error,
  controlId,
  labelInControl = false,
  isInline = false,
  children,
}: {
  field: HumanInputField;
  error?: string;
  /** The native control this visible label names; choice groups name themselves. */
  controlId?: string;
  /** The control already shows the label beside its own mark. */
  labelInControl?: boolean;
  /** The field is in a compact slot: the label and the description are only for a screen reader. */
  isInline?: boolean;
  children: React.ReactNode;
}) {
  const labelClass = isInline ? 'sr-only' : 'block font-mono text-ui font-bold text-white [overflow-wrap:anywhere]';
  return (
    <div className={`min-w-0 ${isInline ? '' : 'space-y-1'}`}>
      {!labelInControl &&
        (controlId ? (
          <label className={labelClass} htmlFor={controlId}>
            <HumanInputFieldLabel field={field} />
          </label>
        ) : (
          !isInline && (
            <span className={labelClass}>
              <HumanInputFieldLabel field={field} />
            </span>
          )
        ))}
      {field.description && (
        <p
          id={controlId ? `${controlId}-description` : undefined}
          className={isInline ? 'sr-only' : 'font-mono text-meta text-dialog-hint'}
        >
          {field.description}
        </p>
      )}
      {children}
      {error && (
        <p id={controlId ? `${controlId}-error` : undefined} className="font-mono text-ui text-err">
          {error}
        </p>
      )}
    </div>
  );
}

/**
 * A field row uses a drawn checkbox for inclusive choices and circular marks for
 * exclusive choices, while keeping the values and validation supplied by the engine.
 */
export function HumanInputFieldRow({
  field,
  values,
  errors,
  disabled,
  isCompact = false,
  isInline = false,
  onChange,
  onSubmit,
}: {
  field: HumanInputField;
  values: HumanInputValues;
  errors: Record<string, string>;
  disabled: boolean;
  /**
   * A form in a view stands among other things, so a long choice folds into one line.
   * A question shows every choice: the choices are the question.
   */
  isCompact?: boolean;
  /**
   * The field is in the app header or a diff: every choice folds into one
   * line, and only a checkbox shows its label.
   */
  isInline?: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
}) {
  const value = values[field.id];
  const error = errors[field.id];
  const chosen = Array.isArray(value) ? value : [];
  const options = field.options ?? [];
  const controlId = useId();
  const [expanded, setExpanded] = useState(field.default_expanded === true);
  const describedBy =
    [field.description ? `${controlId}-description` : '', error ? `${controlId}-error` : '']
      .filter(Boolean)
      .join(' ') || undefined;

  // A heading and a paragraph explain the form but collect no answer. The heading
  // is plain text; the paragraph uses the same safe Markdown as the description.
  if (field.type === 'heading') {
    return <ViewHeading>{field.text}</ViewHeading>;
  }
  if (field.type === 'paragraph') {
    return (
      <ViewParagraph as="div">
        <Markdown compact hardBreaks>
          {field.text ?? ''}
        </Markdown>
      </ViewParagraph>
    );
  }

  if (isViewContainerType(field.type)) {
    const children = (field.children ?? []).map((child, index) => (
      <div key={`${index}:${child.id}`} className={field.type === 'flex' ? viewFlexChildClass(field.direction, child.type, child.is_growing) : 'min-w-0'}>
        <HumanInputFieldRow
          isCompact={isCompact}
          field={child}
          values={values}
          errors={errors}
          disabled={disabled}
          onChange={onChange}
          onSubmit={onSubmit}
        />
      </div>
    ));
    const Icon = field.icon && COMMAND_ICONS[field.icon];
    const icon = Icon && <Icon className="size-4 shrink-0" />;
    switch (field.type) {
      case 'flex':
        return <ViewFlex direction={field.direction} is_wrapping={field.is_wrapping} gap={field.gap} align={field.align} justify={field.justify}>{children}</ViewFlex>;
      case 'grid':
        return <ViewGrid columns={field.columns} min_width={field.min_width} gap={field.gap}>{children}</ViewGrid>;
      case 'section':
        return <ViewSection label={field.label} description={field.description} icon={icon}><ViewFlex>{children}</ViewFlex></ViewSection>;
      case 'collapsible':
        return <ViewCollapsible label={field.label} description={field.description} icon={icon} isOpen={expanded} onToggle={() => setExpanded(!expanded)}><ViewFlex>{children}</ViewFlex></ViewCollapsible>;
    }
  }

  if (field.type === 'toggle') {
    return (
      <FieldShell field={field} controlId={controlId} isInline={isInline} error={error}>
        <ToggleGroup
          id={controlId}
          aria-label={field.label}
          aria-describedby={describedBy}
          aria-invalid={error ? true : undefined}
          aria-required={!field.is_multiple && field.is_required ? true : undefined}
          options={options.map((option) => {
            const Icon = option.icon && COMMAND_ICONS[option.icon];
            return { ...option, icon: Icon ? <Icon className="size-4 shrink-0" /> : undefined };
          })}
          value={typeof value === 'string' || Array.isArray(value) ? value : undefined}
          isMultiple={field.is_multiple}
          disabled={disabled}
          onToggle={(option) => onChange(field.id, field.is_multiple ? toggleHumanInputOption(field, values, option) : option)}
        />
      </FieldShell>
    );
  }

  if (field.type === 'checkbox') {
    const on = value === true;
    return (
      <FieldShell
        field={field}
        controlId={controlId}
        labelInControl
        {...(error ? { error } : {})}
      >
        <Checkbox
          id={controlId}
          variant="bare"
          isOn={on}
          disabled={disabled}
          aria-required={field.is_required || undefined}
          aria-invalid={error ? true : undefined}
          aria-describedby={describedBy}
          onClick={() => onChange(field.id, !on)}
        >
          <HumanInputFieldLabel field={field} />
        </Checkbox>
      </FieldShell>
    );
  }

  if (field.type === 'select' || field.type === 'multiselect') {
    const isMulti = field.type === 'multiselect';
    const picked = typeof value === 'string' ? value : '';
    // In a compact slot every choice is one line that opens a list; the label names it.
    if (isInline) {
      const empty = field.placeholder ?? field.label;
      return (
        <FieldShell field={field} controlId={controlId} isInline {...(error ? { error } : {})}>
          {isMulti ? (
            <Select
              id={controlId}
              disabled={disabled}
              aria-describedby={describedBy}
              options={options}
              values={chosen}
              noneLabel={empty}
              onValuesChange={(next) => onChange(field.id, next)}
            />
          ) : (
            <Select
              id={controlId}
              disabled={disabled}
              aria-describedby={describedBy}
              options={options}
              value={picked}
              placeholder={empty}
              onValueChange={(next) => onChange(field.id, next)}
            />
          )}
        </FieldShell>
      );
    }
    // Three faces for one exclusive choice. A few short words are a strip of cells.
    // Many choices fold into one line that opens a list, in a view and in a question,
    // so the fields after them and the submit button stay in view. The rest is a list.
    if (!isMulti && options.length >= HUMAN_INPUT_SELECT_FOLD_AT) {
      return (
        <FieldShell field={field} controlId={controlId} {...(error ? { error } : {})}>
          <Select
            id={controlId}
            className="w-full sm:max-w-sm"
            disabled={disabled}
            aria-describedby={describedBy}
            options={options}
            value={picked}
            placeholder="Choose one"
            onValueChange={(next) => onChange(field.id, next)}
          />
        </FieldShell>
      );
    }
    if (!isMulti && isShortChoice(options)) {
      return (
        <FieldShell field={field} {...(error ? { error } : {})}>
          <ChoiceSegments
            aria-label={field.label}
            disabled={disabled}
            options={options}
            value={picked}
            onValueChange={(next) => onChange(field.id, next)}
          />
        </FieldShell>
      );
    }
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <ChoiceList role={isMulti ? 'group' : 'radiogroup'} aria-label={field.label}>
          {options.map((option) => {
            const on = isMulti ? chosen.includes(option.value) : value === option.value;
            return isMulti ? (
              <Checkbox
                key={option.value}
                isOn={on}
                disabled={disabled}
                onClick={() =>
                  onChange(
                    field.id,
                    toggleHumanInputOption(field, { [field.id]: chosen }, option.value),
                  )
                }
              >
                {option.label}
              </Checkbox>
            ) : (
              <ChoiceRow
                key={option.value}
                isOn={on}
                disabled={disabled}
                role="radio"
                aria-checked={on}
                mark={
                  on
                    ? HUMAN_INPUT_CHOICE_MARKS.exclusiveOn
                    : HUMAN_INPUT_CHOICE_MARKS.exclusiveOff
                }
                onClick={() => onChange(field.id, option.value)}
              >
                {option.label}
              </ChoiceRow>
            );
          })}
        </ChoiceList>
      </FieldShell>
    );
  }

  if (field.type === 'range') {
    const { min, max, step } = humanInputRange(field);
    const current = typeof value === 'number' ? value : min;
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3">
          <RangeSlider
            disabled={disabled}
            min={min}
            max={max}
            step={step}
            value={current}
            aria-label={field.label}
            onChange={(event) =>
              onChange(field.id, clampHumanInputRange(field, event.target.valueAsNumber))
            }
          />
          <output className="min-w-[2ch] text-right font-mono text-ui font-bold tabular-nums text-white">
            {current}
          </output>
          {/* The ends of the track carry its bounds; the slider itself already names them. */}
          <p
            aria-hidden="true"
            className="flex justify-between font-mono text-meta tabular-nums text-dialog-hint"
          >
            <span>{min}</span>
            <span>{max}</span>
          </p>
        </div>
      </FieldShell>
    );
  }

  const text = typeof value === 'string' ? value : '';

  if (field.type === 'datetime') {
    // The native control of the platform picks the date or the time. It writes
    // the form of the mode, which is the form that the engine reads.
    const mode = field.mode ?? humanInputDatetimeMode(undefined);
    return (
      <FieldShell field={field} controlId={controlId} {...(error ? { error } : {})}>
        <Input
          id={controlId}
          type={DATETIME_CONTROLS[mode]}
          className="sm:max-w-xs"
          disabled={disabled}
          value={text}
          aria-describedby={describedBy}
          aria-invalid={Boolean(error)}
          aria-required={field.is_required}
          {...(field.earliest ? { min: field.earliest } : {})}
          {...(field.latest ? { max: field.latest } : {})}
          onChange={(event) =>
            onChange(field.id, event.target.value.slice(0, HUMAN_INPUT_DATETIME_FORMS[mode].length))
          }
        />
      </FieldShell>
    );
  }

  if (field.type === 'otp') {
    return (
      <FieldShell field={field} controlId={controlId} {...(error ? { error } : {})}>
        <OtpBoxes
          field={field}
          value={text}
          controlId={controlId}
          describedBy={describedBy}
          isInvalid={Boolean(error)}
          disabled={disabled}
          onChange={onChange}
          onSubmit={onSubmit}
        />
      </FieldShell>
    );
  }

  if (field.type === 'multiline') {
    return (
      <FieldShell field={field} controlId={controlId} {...(error ? { error } : {})}>
        <textarea
          id={controlId}
          rows={4}
          disabled={disabled}
          value={text}
          aria-describedby={describedBy}
          aria-invalid={Boolean(error)}
          aria-required={field.is_required}
          {...(field.max_length ? { maxLength: field.max_length } : {})}
          {...(field.placeholder ? { placeholder: field.placeholder } : {})}
          className="block w-full resize-y border border-field-edge bg-input px-2.5 py-1.5 font-mono text-ui text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted sm:px-3"
          onChange={(event) => onChange(field.id, event.target.value)}
        />
      </FieldShell>
    );
  }

  const Icon = field.icon ? COMMAND_ICONS[field.icon] : undefined;
  return (
    <FieldShell field={field} controlId={controlId} isInline={isInline} {...(error ? { error } : {})}>
      <Input
        id={controlId}
        type={field.type === 'password' ? 'password' : 'text'}
        {...(Icon ? { icon: <Icon className="size-4" /> } : {})}
        disabled={disabled}
        value={text}
        aria-describedby={describedBy}
        aria-invalid={Boolean(error)}
        aria-required={field.is_required}
        {...(field.max_length ? { maxLength: field.max_length } : {})}
        {...(field.placeholder || isInline ? { placeholder: field.placeholder ?? field.label } : {})}
        onChange={(event) => onChange(field.id, event.target.value)}
        // A one-line field has nothing to do with a bare Enter, so it answers
        // the question — the reflex every login form in the world has taught.
        onKeyDown={(event) => {
          if (event.key !== 'Enter' || event.shiftKey) return;
          event.preventDefault();
          onSubmit();
        }}
      />
    </FieldShell>
  );
}

/**
 * How long the number pad takes to stand after the touch that focused the field, in
 * milliseconds. Measured on iOS 18: about 630.
 */
const KEYBOARD_RISE_MS = 600;

/**
 * A one-time code is shown as BOXES, one digit each — the shape every bank and
 * every 2FA screen has taught, and the same `[1] [2] [3] [ ] [ ] [ ]` the TUI
 * band paints. Only digits get in, wherever they come from: typing, an SMS
 * autofill, or a paste of the whole `123 456` line.
 *
 * The boxes are only drawn. ONE real input lies over them and holds the whole
 * code. One box per input broke a phone in two ways: a double tap could not
 * paste, because each box held one character and selected itself on focus, and
 * the keyboard jumped, because focus moved to a new input at every digit and
 * iOS rebuilt the keyboard for each one. With one input, focus stays put and
 * SMS autofill works.
 *
 * Paste is a verb of the app, shown at the finger. Reported on an iPhone: the paste
 * menu of the invisible input showed, hid and showed again under a double tap, and a
 * long press showed nothing. While the input has focus, a touch on the row goes to
 * the row and keeps the focus, so the platform menu never flashes. The first tap
 * gives the focus, and a tap on the focused boxes shows Paste above the tapped box.
 * Reported later: a Paste verb at the far end of the hint line was not where the
 * double tap looked for it.
 *
 * In the phone app, that tap opens the paste menu of the system: see `lib/paste-menu`.
 * The Paste verb of the app is for a browser and for a phone system without that menu.
 */
function OtpBoxes({
  field,
  value,
  controlId,
  describedBy,
  isInvalid,
  disabled,
  onChange,
  onSubmit,
}: {
  field: HumanInputField;
  value: string;
  controlId: string;
  describedBy: string | undefined;
  isInvalid: boolean;
  disabled: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
}) {
  const { min, max } = humanInputOtp(field);
  const digits = humanInputOtpDigits(field, value);
  const [isFocused, setFocused] = useState(false);
  const input = useRef<HTMLInputElement>(null);
  // When a touch focused the input. The keyboard rises for about half a second, and
  // the second tap of a double tap lands on a key. Reported on an iPhone: a double
  // tap on the boxes typed a digit nobody chose.
  const touchFocusAt = useRef(Number.NEGATIVE_INFINITY);
  // Where the Paste verb stands in the row: over the box that the last tap hit.
  const [pasteAt, setPasteAt] = useState<{ left: number; top: number } | null>(null);
  const pasteVerb = useRef<HTMLDivElement>(null);
  // A press that began on the boxes while the input had the focus. The tap that
  // gives the focus is not such a press, so it shows no Paste.
  const isPressInFocus = useRef(false);
  const row = useRef<HTMLDivElement>(null);
  // The box of the system paste menu, from a request for the menu until the request to hide it.
  const systemMenuBox = useRef<HTMLElement | null>(null);
  const hideSystemMenu = useCallback(() => {
    if (!systemMenuBox.current) return;
    systemMenuBox.current = null;
    hideSystemPasteMenu();
  }, []);
  const hidePaste = useCallback(() => {
    setPasteAt(null);
    hideSystemMenu();
  }, [hideSystemMenu]);
  useEffect(() => {
    // A touch outside the boxes closes Paste. The system menu of Android stays open under it.
    const onPointerDown = (event: PointerEvent) => {
      if (!row.current?.contains(event.target as Node)) hidePaste();
    };
    document.addEventListener('pointerdown', onPointerDown, true);
    return () => {
      document.removeEventListener('pointerdown', onPointerDown, true);
      hideSystemMenu();
    };
  }, [hidePaste, hideSystemMenu]);
  useEffect(() => {
    // The keyboard of Android resizes the page after a tap, and the box moves. The system
    // menu follows the box. The shell takes its new height in the next frame, so the
    // measurement waits for that frame.
    let frame = 0;
    const follow = () => {
      cancelAnimationFrame(frame);
      frame = requestAnimationFrame(() => {
        if (systemMenuBox.current) moveSystemPasteMenu(systemMenuBox.current);
      });
    };
    const viewport = window.visualViewport;
    window.addEventListener('resize', follow);
    viewport?.addEventListener('resize', follow);
    viewport?.addEventListener('scroll', follow);
    return () => {
      cancelAnimationFrame(frame);
      window.removeEventListener('resize', follow);
      viewport?.removeEventListener('resize', follow);
      viewport?.removeEventListener('scroll', follow);
    };
  }, []);
  useEffect(() => {
    const element = row.current;
    if (!element) return undefined;
    // iOS moves the caret to a touched point. A touch on a box took the caret out of the
    // focused input: the keyboard typed nothing, and the system offered no Paste or put
    // its own menu at the first box. React listens to `touchstart` passively, so this
    // listener is a direct one.
    const onTouchStart = (event: TouchEvent) => {
      const isOnPasteVerb = pasteVerb.current?.contains(event.target as Node) ?? false;
      if (document.activeElement === input.current && !isOnPasteVerb) event.preventDefault();
    };
    element.addEventListener('touchstart', onTouchStart, { passive: false });
    return () => element.removeEventListener('touchstart', onTouchStart);
  }, []);
  useEffect(() => {
    const element = input.current;
    if (!element) return undefined;
    const onBeforeInput = (event: InputEvent) => {
      const isKeyTap = event.inputType === 'insertText' && (event.data ?? '').length === 1;
      if (isKeyTap && performance.now() - touchFocusAt.current < KEYBOARD_RISE_MS) event.preventDefault();
    };
    element.addEventListener('beforeinput', onBeforeInput);
    return () => element.removeEventListener('beforeinput', onBeforeInput);
  }, []);
  // A pasted code REPLACES the code: an operator who pastes again after a wrong
  // paste wants the new code, not the old code with digits added.
  const paste = (text: string): boolean => {
    hidePaste();
    const pasted = humanInputOtpDigits(field, text);
    if (pasted) onChange(field.id, pasted);
    return pasted !== '';
  };
  const pasteClipboard = () => {
    setPasteAt(null);
    const read = Capacitor.isNativePlatform()
      ? Clipboard.read().then((result) => (result.type === 'text/plain' ? result.value : ''))
      : navigator.clipboard.readText();
    read
      .then((text) => paste(text))
      .catch(() => console.warn('One-time code: could not read the clipboard.'));
  };
  // The next digit lands in this box; a full code keeps the last box marked.
  const active = Math.min(digits.length, max - 1);

  return (
    <div className="space-y-1">
      {/* BEST-EFFORT JUSTIFY: the boxes stretch to fill the row, but only as far
          as a digit box goes — a square one, the touch target the thumb expects.
          Stretching six of them across the whole glass made a six-digit code
          read as six empty fields, so the slack goes BETWEEN the boxes.

          The row is the FIELD COLUMN on a phone, flush with the label above it
          and the input below it: `max-w-sm` (24rem) left a 384px row under a
          408px field on any phone wider than 400pt — a 24px ragged edge on a
          440pt iPhone, the one thing best-effort justify must not do. The cap is
          `max-w-md`, wider than the widest phone's column, so it never bites on
          a phone and still stops the boxes drifting apart on wider glass; from
          `sm:` up, where `Modal` becomes a 36rem box, it tightens again. */}
      <div
        ref={row}
        data-otp-row=""
        className="relative flex max-w-md cursor-text flex-wrap justify-between gap-1.5 sm:max-w-sm"
        // A double tap still sends a mouse down, and that took the focus.
        onPointerDown={(event) => {
          isPressInFocus.current = isFocused && !pasteVerb.current?.contains(event.target as Node);
          if (isFocused) event.preventDefault();
        }}
        // The release, not the click: iOS does not always finish a tap as a click.
        onPointerUp={(event) => {
          if (!isPressInFocus.current) return;
          isPressInFocus.current = false;
          const boxes = Array.from(event.currentTarget.querySelectorAll<HTMLElement>('[data-otp-box]'));
          const distance = (box: HTMLElement) => {
            const rect = box.getBoundingClientRect();
            return Math.hypot(event.clientX - (rect.left + rect.right) / 2, event.clientY - (rect.top + rect.bottom) / 2);
          };
          // A tap between two boxes takes the nearer one.
          const box = boxes.reduce((near, next) => (distance(next) < distance(near) ? next : near));
          const at = { left: box.offsetLeft + box.offsetWidth / 2, top: box.offsetTop };
          if (!hasSystemPasteMenu()) {
            setPasteAt(at);
            return;
          }
          systemMenuBox.current = box;
          showSystemPasteMenu(box).catch((error: unknown) => {
            // An iPhone before iOS 16 has no such menu: the Paste verb of the app replaces it.
            console.warn('one-time code: no system paste menu', error);
            systemMenuBox.current = null;
            setPasteAt(at);
          });
        }}
        onMouseDown={(event) => {
          if (isFocused) event.preventDefault();
        }}
      >
        {Array.from({ length: max }, (_unused, index) => (
          <span
            key={index}
            aria-hidden="true"
            data-otp-box={index}
            className={`flex aspect-square min-w-8 max-w-11 flex-1 items-center justify-center border bg-input font-mono text-body tabular-nums ${
              disabled ? 'text-muted' : 'text-white'
            } ${isFocused && index === active ? 'border-accent ring-1 ring-accent/30' : 'border-edge'}`}
          >
            {digits[index] ?? ''}
          </span>
        ))}
        {/* No `maxLength`: a browser cuts a paste at that length BEFORE the
            digits are picked out, so `123 456` would lose its last digit. */}
        <input
          id={controlId}
          type="text"
          inputMode="numeric"
          pattern="[0-9]*"
          autoComplete="one-time-code"
          onCopy={preventSecretCopy}
          onCut={preventSecretCopy}
          autoCorrect="off"
          autoCapitalize="off"
          spellCheck={false}
          disabled={disabled}
          value={digits}
          aria-describedby={describedBy}
          aria-invalid={isInvalid}
          aria-required={field.is_required}
          ref={input}
          className={`absolute inset-0 size-full cursor-text border-0 bg-transparent p-0 font-mono text-body text-transparent caret-transparent outline-none selection:bg-transparent disabled:cursor-not-allowed ${
            isFocused ? 'pointer-events-none' : ''
          }`}
          onPointerDown={(event) => {
            if (event.pointerType === 'touch' && document.activeElement !== event.currentTarget) {
              touchFocusAt.current = performance.now();
            }
          }}
          onChange={(event) => {
            hidePaste();
            onChange(field.id, humanInputOtpDigits(field, event.target.value));
          }}
          onPaste={(event) => {
            if (paste(event.clipboardData.getData('text'))) event.preventDefault();
          }}
          onKeyDown={(event) => {
            if (event.key !== 'Enter') return;
            event.preventDefault();
            onSubmit();
          }}
          onFocus={() => setFocused(true)}
          onBlur={() => {
            setFocused(false);
            hidePaste();
          }}
        />
        {/* The row keeps the focus under a press here too, so the keyboard stays up. */}
        {pasteAt && !disabled && (
          <div
            ref={pasteVerb}
            className="absolute z-10 w-max -translate-x-1/2 -translate-y-full pb-1.5"
            style={{ left: pasteAt.left, top: pasteAt.top }}
          >
            <Button variant="overlay" density="inline" pressEffect="none" onClick={pasteClipboard}>
              Paste
            </Button>
          </div>
        )}
      </div>
      <p className="font-mono text-meta text-dialog-hint">
        {min === max ? `${max} digits` : `${min}–${max} digits`}
      </p>
    </div>
  );
}
