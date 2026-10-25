import { useCallback, useEffect, useId, useRef, useState, type InputHTMLAttributes } from 'react';
import {
  Banner,
  Button,
  Checkbox,
  ChoiceRow,
  DialogFrame,
  Input,
  Modal,
  ViewHeading,
  ViewLayout,
  ViewParagraph,
} from './ui';
import { Markdown } from './ChatContent';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyInputViewEvent,
  clampHumanInputRange,
  humanInputFormChange,
  humanInputFormRefused,
  humanInputFormStart,
  humanInputInputFields,
  humanInputOtp,
  humanInputOtpDigits,
  humanInputRange,
  isInputViewEvent,
  originLabel,
  toggleHumanInputOption,
  HUMAN_INPUT_CHOICE_MARKS,
  type HumanInputField,
  type HumanInputForm,
  type HumanInputRequest,
  type HumanInputValues,
} from '../lib/human-input';

/** The range field's square track and thumb, with a finger-sized target. */
function RangeSlider({ className = '', ...props }: InputHTMLAttributes<HTMLInputElement>) {
  return (
    <span className={`flex min-h-11 min-w-0 items-center mouse:min-h-7 ${className}`}>
      <input
        type="range"
        className="h-1 w-full min-w-0 cursor-pointer appearance-none rounded-none bg-edge accent-accent disabled:cursor-not-allowed [&::-webkit-slider-thumb]:size-4 [&::-webkit-slider-thumb]:appearance-none [&::-webkit-slider-thumb]:rounded-none [&::-webkit-slider-thumb]:border-0 [&::-webkit-slider-thumb]:bg-accent [&::-moz-range-thumb]:size-4 [&::-moz-range-thumb]:appearance-none [&::-moz-range-thumb]:rounded-none [&::-moz-range-thumb]:border-0 [&::-moz-range-thumb]:bg-accent"
        {...props}
      />
    </span>
  );
}

/**
 * Answers typed into open requests, by session and request ID. The draft stays
 * in memory only, never in storage, because a field can hold a password or a
 * one-time code. It lets a remounted screen show the answer again.
 */
const humanInputDrafts = new Map<string, HumanInputForm>();

function humanInputDraftKey(sid: string, requestId: string): string {
  return `${sid}\u0000${requestId}`;
}

function restoreHumanInputDraft(sid: string, request: HumanInputRequest | null): HumanInputForm {
  if (!request) return humanInputFormStart(null);
  return humanInputDrafts.get(humanInputDraftKey(sid, request.id)) ?? humanInputFormStart(request);
}

/** Drop the drafts of this session's requests that are no longer open. */
function forgetClosedHumanInputDrafts(sid: string, open: readonly HumanInputRequest[]): void {
  const kept = new Set(open.map((row) => humanInputDraftKey(sid, row.id)));
  const prefix = humanInputDraftKey(sid, '');
  for (const key of humanInputDrafts.keys()) {
    if (key.startsWith(prefix) && !kept.has(key)) humanInputDrafts.delete(key);
  }
}

/**
 * The companion half of the typed human-input pause.
 *
 * An extension can BLOCK a run on the operator. The engine publishes that pause
 * on every channel the request names — the TUI band and this screen are two
 * views of the SAME request, answered through the same engine coercion — and a
 * phone that is not looking gets the push the gateway sends alongside.
 *
 * Live `view.open` frames open the form; `view.close` closes it
 * no matter who answered, so a request settled in the TUI cannot leave a dead
 * dialog here.
 *
 * The dialog is a SHEET on a phone and a centred card on a desktop, and in both
 * the question scrolls while the answer bar does not: a long form must never be
 * able to push its own Submit button off the screen, least of all under a
 * virtual keyboard.
 */
export function HumanInputPrompt({
  client,
  subscriptions,
  sid,
}: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
}) {
  const [pending, setPending] = useState<HumanInputRequest[]>([]);
  // The open forms were read back at least once, so a draft for a form that is
  // not in `pending` belongs to a closed request.
  const [isLoaded, setLoaded] = useState(false);
  const [draft, setDraft] = useState<{ id: string | null; form: HumanInputForm }>(() => ({
    id: null,
    form: humanInputFormStart(null),
  }));
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const bodyRef = useRef<HTMLDivElement | null>(null);

  const request = pending[0] ?? null;
  const requestId = request?.id ?? null;
  const form = draft.form;

  // A screen opened while a run is ALREADY parked never saw the live frame, so
  // the open forms are read back on mount and whenever the stream reconnects.
  useEffect(() => {
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      client
        .inputViews(sid, controller.signal)
        .then((requests) => {
          if (cancelled) return;
          setPending(requests);
          setLoaded(true);
        })
        .catch(() => undefined);
    };
    reload();
    const stopConnection = subscriptions.subscribeConnection((connected) => {
      if (connected) reload();
    });
    const stopEvents = subscriptions.subscribeSession(sid, (event) => {
      if (!isInputViewEvent(event)) return;
      setPending((current) => applyInputViewEvent(current, event));
    });
    return () => {
      cancelled = true;
      controller.abort();
      stopConnection();
      stopEvents();
    };
  }, [client, sid, subscriptions]);

  // Each request opens on ITS OWN defaults — the same starting form the TUI
  // paints — and a keyed reset here keeps a queued second request from
  // inheriting the answer typed into the first. The key is the request ID, not
  // the object: a reconnect reads the same open request back as a new object,
  // and a reset on that read erased what the operator had typed while the app
  // was in the background. A draft also survives a remount of this screen.
  useEffect(() => {
    setDraft((current) =>
      current.id === requestId
        ? current
        : { id: requestId, form: restoreHumanInputDraft(sid, request) },
    );
  }, [request, requestId, sid]);

  useEffect(() => {
    setError(null);
    setBusy(false);
  }, [requestId]);

  useEffect(() => {
    if (draft.id !== null) humanInputDrafts.set(humanInputDraftKey(sid, draft.id), draft.form);
  }, [draft, sid]);

  useEffect(() => {
    if (isLoaded) forgetClosedHumanInputDrafts(sid, pending);
  }, [isLoaded, pending, sid]);

  const setValue = useCallback((id: string, value: HumanInputValues[string]) => {
    // Any touch of a field also DROPS the engine's complaint about it: that
    // message judged the previous answer, and the next confirmation will judge
    // this one. The rule lives in `humanInputFormChange`, so the sheet cannot
    // forget it and the TUI band does the same thing on a keystroke.
    setDraft((current) => ({ ...current, form: humanInputFormChange(current.form, id, value) }));
  }, []);

  const drop = useCallback(
    (id: string) => {
      humanInputDrafts.delete(humanInputDraftKey(sid, id));
      setPending((current) => current.filter((row) => row.id !== id));
    },
    [sid],
  );

  const submit = useCallback(() => {
    if (!request || busy) return;
    // The answer ALWAYS leaves. Validators are FUNCTIONS living in the
    // extension that asked the question, so this form cannot know whether the
    // answer is good — only a confirmation can, and a button that silently
    // refuses to fire teaches the operator nothing.
    setBusy(true);
    setError(null);
    client
      .viewAction(sid, request.id, { action: 'submit', values: form.values })
      .then((outcome) => {
        if (outcome?.is_accepted) {
          // The close event drops it too; doing it here as well keeps the
          // dialog from lingering for a stream that is briefly offline.
          drop(request.id);
          return;
        }
        setDraft((current) => ({
          ...current,
          form: humanInputFormRefused(current.form, outcome?.errors ?? {}),
        }));
        setError('The daemon rejected this answer.');
        setBusy(false);
      })
      .catch((cause: unknown) => {
        setError(cause instanceof Error ? cause.message : 'Could not send this answer.');
        setBusy(false);
      });
  }, [busy, client, drop, form, request, sid]);

  const cancel = useCallback(() => {
    if (!request || busy) return;
    setBusy(true);
    setError(null);
    client
      .viewAction(sid, request.id, { action: 'cancel' })
      .then(() => drop(request.id))
      .catch((cause: unknown) => {
        setError(cause instanceof Error ? cause.message : 'Could not cancel this request.');
        setBusy(false);
      });
  }, [busy, client, drop, request, sid]);

  // The SAME two chords the TUI band offers, so an operator who answers in one
  // channel does not have to relearn the other. `Enter` alone belongs to the
  // multiline field, which is why submit is the modified chord everywhere.
  const isCancellable = request?.is_cancellable ?? false;
  useEffect(() => {
    if (!request) return undefined;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape' && isCancellable) {
        event.preventDefault();
        cancel();
        return;
      }
      if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
        event.preventDefault();
        submit();
      }
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, [cancel, isCancellable, request, submit]);

  // A pause nobody asked for still opens ready to answer — but only where a
  // keyboard is already on screen. Focusing a field on a phone raises the
  // virtual keyboard over the very question being asked.
  useEffect(() => {
    if (!requestId) return;
    if (window.matchMedia?.('(pointer: coarse)').matches) return;
    bodyRef.current?.querySelector<HTMLElement>('input, textarea, button')?.focus();
  }, [requestId]);

  if (!request) return null;

  return (
    <HumanInputSheet
      request={request}
      values={form.values}
      errors={form.errors}
      error={error}
      busy={busy}
      waiting={pending.length - 1}
      bodyRef={bodyRef}
      onChange={setValue}
      onSubmit={submit}
      onCancel={cancel}
    />
  );
}

/** The fields a phone answers with its software keyboard. */
const KEYBOARD_FIELD_TYPES = new Set<string>(['plaintext', 'password', 'multiline', 'otp']);

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
  // A subagent's form also reaches its ancestor sessions: name who asks.
  const origin = originLabel(request.origin);
  // A field that raises the keyboard keeps the sheet where the keyboard puts it.
  // Reported: a double tap on the one-time code made the keyboard and screen jump.
  const isKeyboardReserved = humanInputInputFields(request.nodes).some((field) =>
    KEYBOARD_FIELD_TYPES.has(field.type),
  );
  return (
    <Modal
      size="fit"
      // AND IT STANDS IN THE SESSION THAT ASKED IT. Reported from a wide window: the
      // form covered and dimmed the session list and the header beside the transcript,
      // so the whole application read as blocked while a single session waited. The
      // sheet stands in that session's own pane instead, where an opened run stands.
      within="session"
      isKeyboardReserved={isKeyboardReserved}
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
                <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                  asked by <span className="text-white">{request.source}</span>
                </p>
              )}
              {origin && (
                // One row: each title shortens with an ellipsis on its own, so a long
                // subagent title never hides the parent. Hover shows both in full.
                <p
                  data-testid="human-input-origin"
                  title={`Subagent ${origin.agent} · parent ${origin.parent}`}
                  className="flex min-w-0 items-baseline gap-[1ch] whitespace-nowrap font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint"
                >
                  <span className="shrink-0">subagent</span>
                  <span className="min-w-0 truncate text-white">{origin.agent}</span>
                  <span className="shrink-0">· parent</span>
                  <span className="min-w-0 truncate text-white">{origin.parent}</span>
                </p>
              )}
            </div>
          )}
          <ViewLayout>
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
          </ViewLayout>
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

function FieldShell({
  field,
  error,
  controlId,
  labelInControl = false,
  children,
}: {
  field: HumanInputField;
  error?: string;
  /** The native control this visible label names; choice groups name themselves. */
  controlId?: string;
  /** The control already shows the label beside its own mark. */
  labelInControl?: boolean;
  children: React.ReactNode;
}) {
  const labelClass = 'block font-mono text-ui uppercase tracking-[0.08em] text-dialog-hint';
  return (
    <div className="min-w-0 space-y-1">
      {!labelInControl &&
        (controlId ? (
          <label className={labelClass} htmlFor={controlId}>
            <HumanInputFieldLabel field={field} />
          </label>
        ) : (
          <span className={labelClass}>
            <HumanInputFieldLabel field={field} />
          </span>
        ))}
      {field.description && (
        <p
          id={controlId ? `${controlId}-description` : undefined}
          className="font-mono text-ui text-dialog-hint"
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
function HumanInputFieldRow({
  field,
  values,
  errors,
  disabled,
  onChange,
  onSubmit,
}: {
  field: HumanInputField;
  values: HumanInputValues;
  errors: Record<string, string>;
  disabled: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
}) {
  const value = values[field.id];
  const error = errors[field.id];
  const chosen = Array.isArray(value) ? value : [];
  const options = field.options ?? [];
  const controlId = useId();
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

  // Keep form semantics outside the shared layout: groups label their fields,
  // but neither the group nor a decoration contributes an answer value.
  if (field.type === 'group') {
    return (
      <fieldset className="m-0 min-w-0 space-y-1 border-0 p-0">
        {field.label && (
          <legend className="mb-1 block font-mono text-ui uppercase tracking-[0.08em] text-dialog-hint">
            {field.label}
          </legend>
        )}
        {field.description && (
          <p className="font-mono text-ui text-dialog-hint">{field.description}</p>
        )}
        <ViewLayout
          direction={field.direction}
          data-group-id={field.id}
          data-direction={field.direction ?? 'column'}
        >
          {(field.children ?? []).map((child, at) => (
            <HumanInputFieldRow
              key={`${at}:${child.id}`}
              field={child}
              values={values}
              errors={errors}
              disabled={disabled}
              onChange={onChange}
              onSubmit={onSubmit}
            />
          ))}
        </ViewLayout>
      </fieldset>
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
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <div className="space-y-1" role={isMulti ? 'group' : 'radiogroup'} aria-label={field.label}>
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
        </div>
      </FieldShell>
    );
  }

  if (field.type === 'range') {
    const { min, max, step } = humanInputRange(field);
    const current = typeof value === 'number' ? value : min;
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <div className="flex items-center gap-3">
          <RangeSlider
            disabled={disabled}
            min={min}
            max={max}
            step={step}
            value={current}
            aria-label={field.label}
            className="flex-1"
            onChange={(event) =>
              onChange(field.id, clampHumanInputRange(field, event.target.valueAsNumber))
            }
          />
          <span className="shrink-0 font-mono text-meta tabular-nums text-white sm:text-ui">
            {current}
          </span>
        </div>
        <p className="font-mono text-chip text-dialog-hint">
          {min} – {max}
        </p>
      </FieldShell>
    );
  }

  const text = typeof value === 'string' ? value : '';

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
          className="w-full resize-y border border-edge bg-input px-2.5 py-1 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted sm:text-ui"
          onChange={(event) => onChange(field.id, event.target.value)}
        />
      </FieldShell>
    );
  }

  return (
    <FieldShell field={field} controlId={controlId} {...(error ? { error } : {})}>
      <Input
        id={controlId}
        type={field.type === 'password' ? 'password' : 'text'}
        disabled={disabled}
        value={text}
        aria-describedby={describedBy}
        aria-invalid={Boolean(error)}
        aria-required={field.is_required}
        {...(field.max_length ? { maxLength: field.max_length } : {})}
        {...(field.placeholder ? { placeholder: field.placeholder } : {})}
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
 * the platform's own paste menu and SMS autofill work.
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
        data-otp-row=""
        className="relative flex max-w-md flex-wrap justify-between gap-1.5 sm:max-w-sm"
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
          autoCorrect="off"
          autoCapitalize="off"
          spellCheck={false}
          disabled={disabled}
          value={digits}
          aria-describedby={describedBy}
          aria-invalid={isInvalid}
          aria-required={field.is_required}
          className="absolute inset-0 size-full cursor-text border-0 bg-transparent p-0 font-mono text-body text-transparent caret-transparent outline-none selection:bg-transparent disabled:cursor-not-allowed"
          onChange={(event) => onChange(field.id, humanInputOtpDigits(field, event.target.value))}
          // A pasted code REPLACES the code: an operator who pastes again after a
          // wrong paste wants the new code, not the old code with digits added.
          onPaste={(event) => {
            const pasted = humanInputOtpDigits(field, event.clipboardData.getData('text'));
            if (!pasted) return;
            event.preventDefault();
            onChange(field.id, pasted);
          }}
          onKeyDown={(event) => {
            if (event.key !== 'Enter') return;
            event.preventDefault();
            onSubmit();
          }}
          onFocus={() => setFocused(true)}
          onBlur={() => setFocused(false)}
        />
      </div>
      <p className="font-mono text-chip text-dialog-hint">
        {min === max ? `${max} digits` : `${min}–${max} digits`}
      </p>
    </div>
  );
}
