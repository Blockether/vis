import { useCallback, useEffect, useRef, useState } from 'react';
import { Banner, Button, ChoiceRow, DialogFrame, Input, Modal, Slider } from './ui';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyInputViewEvent,
  clampHumanInputRange,
  humanInputFormChange,
  humanInputFormRefused,
  humanInputFormStart,
  humanInputOtp,
  humanInputOtpDigits,
  humanInputRange,
  isInputViewEvent,
  toggleHumanInputOption,
  HUMAN_INPUT_CHOICE_MARKS,
  type HumanInputField,
  type HumanInputForm,
  type HumanInputRequest,
  type HumanInputValues,
} from '../lib/human-input';

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
  const [form, setForm] = useState<HumanInputForm>(() => humanInputFormStart(null));
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const bodyRef = useRef<HTMLDivElement | null>(null);

  const request = pending[0] ?? null;

  // A screen opened while a run is ALREADY parked never saw the live frame, so
  // the open forms are read back on mount and whenever the stream reconnects.
  useEffect(() => {
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      client
        .inputViews(sid, controller.signal)
        .then((requests) => {
          if (!cancelled) setPending(requests);
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
  // inheriting the answer typed into the first.
  useEffect(() => {
    setForm(humanInputFormStart(request));
    setError(null);
    setBusy(false);
  }, [request]);

  const setValue = useCallback((id: string, value: HumanInputValues[string]) => {
    // Any touch of a field also DROPS the engine's complaint about it: that
    // message judged the previous answer, and the next confirmation will judge
    // this one. The rule lives in `humanInputFormChange`, so the sheet cannot
    // forget it and the TUI band does the same thing on a keystroke.
    setForm((current) => humanInputFormChange(current, id, value));
  }, []);

  const drop = useCallback((id: string) => {
    setPending((current) => current.filter((row) => row.id !== id));
  }, []);

  const submit = useCallback(() => {
    if (!request || busy) return;
    // The answer ALWAYS leaves. Validators are FUNCTIONS living in the
    // extension that asked the question, so this form cannot know whether the
    // answer is good — only a confirmation can, and a button that silently
    // refuses to fire teaches the operator nothing.
    setBusy(true);
    setError(null);
    client
      .submitHumanInput(sid, request.id, form.values)
      .then((outcome) => {
        if (outcome?.is_accepted) {
          // The close event drops it too; doing it here as well keeps the
          // dialog from lingering for a stream that is briefly offline.
          drop(request.id);
          return;
        }
        setForm((current) => humanInputFormRefused(current, outcome?.errors ?? {}));
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
      .cancelHumanInput(sid, request.id)
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
    if (!request) return;
    if (window.matchMedia?.('(pointer: coarse)').matches) return;
    bodyRef.current?.querySelector<HTMLElement>('input, textarea, button')?.focus();
  }, [request]);

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
  return (
    <Modal
      size="fit"
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
              <Button
                className="flex-1 sm:flex-none"
                disabled={busy}
                onClick={onSubmit}
              >
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
          {(request.description || request.source) && (
            <div className="space-y-1">
              {request.description && (
                <p className="font-mono text-meta italic text-dialog-hint">
                  {request.description}
                </p>
              )}
              {/* WHO stopped the run is half the question: an answer means
                  something different to a deploy hook than to a linter. */}
              {request.source && (
                <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                  asked by <span className="text-white">{request.source}</span>
                </p>
              )}
            </div>
          )}
          {/* A DECORATION has no name, so position is the only identity a row
              is guaranteed to have. */}
          {request.fields.map((field, at) => (
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
        </div>
      </DialogFrame>
    </Modal>
  );
}

function FieldShell({
  field,
  error,
  children,
}: {
  field: HumanInputField;
  error?: string;
  children: React.ReactNode;
}) {
  return (
    <div className="space-y-1">
      <span className="block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {field.label}
        {/* The web's own mark, and the TUI band now draws the same one: a red `*`,
            not the word REQUIRED shouted beside every label. Screen readers still
            get the word. */}
        {field.is_required && (
          <>
            <span aria-hidden="true" className="ml-1 text-err">
              *
            </span>
            <span className="sr-only"> required</span>
          </>
        )}
      </span>
      {field.description && (
        <p className="font-mono text-chip italic text-dialog-hint">{field.description}</p>
      )}
      {children}
      {error && <p className="font-mono text-chip text-err">{error}</p>}
    </div>
  );
}

/**
 * One row of the form. The field set is the engine's closed one, and every row
 * carries the marks the TUI band draws, from the one table both surfaces read:
 * `●`/`○` for an exclusive choice, `[✓]`/`[ ]` for an inclusive one.
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

  // PURE DECORATION: a heading opens a section of a long form and a paragraph
  // explains one. Neither is a control: nothing keys it, it holds no value and
  // it can carry no error, so it renders as the words it was given and stops.
  if (field.type === 'heading') {
    return <h3 className="mt-1 font-mono text-ui font-semibold text-white">{field.text}</h3>;
  }
  if (field.type === 'paragraph') {
    return <p className="font-mono text-meta italic text-dialog-hint">{field.text}</p>;
  }

  // A LAYOUT GROUP renders no control of its own: it is a flex container that
  // owns fields, and a child may be a group again, so `row` and `column` nest
  // without a third rule. `fieldset`/`legend` is the group a screen reader
  // already understands.
  if (field.type === 'group') {
    const isRow = field.direction === 'row';
    return (
      <fieldset className="m-0 space-y-1 border-0 p-0">
        {field.label && (
          <legend className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
            {field.label}
          </legend>
        )}
        {field.description && (
          <p className="font-mono text-chip italic text-dialog-hint">{field.description}</p>
        )}
        <div
          data-group-id={field.id}
          data-direction={isRow ? 'row' : 'column'}
          className={
            isRow
              ? 'flex flex-row flex-wrap items-start gap-3'
              : 'flex flex-col gap-3'
          }
        >
          {(field.fields ?? []).map((child, at) => (
            <div key={`${at}:${child.id}`} className={isRow ? 'min-w-[7.5rem] flex-1' : ''}>
              <HumanInputFieldRow
                field={child}
                values={values}
                errors={errors}
                disabled={disabled}
                onChange={onChange}
                onSubmit={onSubmit}
              />
            </div>
          ))}
        </div>
      </fieldset>
    );
  }

  if (field.type === 'checkbox') {
    const on = value === true;
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <ChoiceRow
          isOn={on}
          disabled={disabled}
          aria-pressed={on}
          mark={
            on ? HUMAN_INPUT_CHOICE_MARKS.inclusiveOn : HUMAN_INPUT_CHOICE_MARKS.inclusiveOff
          }
          onClick={() => onChange(field.id, !on)}
        >
          {field.label}
        </ChoiceRow>
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
            return (
              <ChoiceRow
                key={option.value}
                isOn={on}
                disabled={disabled}
                {...(isMulti ? { 'aria-pressed': on } : { role: 'radio', 'aria-checked': on })}
                mark={
                  isMulti
                    ? on
                      ? HUMAN_INPUT_CHOICE_MARKS.inclusiveOn
                      : HUMAN_INPUT_CHOICE_MARKS.inclusiveOff
                    : on
                      ? HUMAN_INPUT_CHOICE_MARKS.exclusiveOn
                      : HUMAN_INPUT_CHOICE_MARKS.exclusiveOff
                }
                onClick={() =>
                  onChange(
                    field.id,
                    isMulti
                      ? toggleHumanInputOption(field, { [field.id]: chosen }, option.value)
                      : option.value,
                  )
                }
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
          <Slider
            disabled={disabled}
            min={min}
            max={max}
            step={step}
            value={current}
            aria-label={field.label}
            className="flex-1"
            onChange={(event) => onChange(field.id, clampHumanInputRange(field, event.target.valueAsNumber))}
          />
          <span className="shrink-0 font-mono text-meta tabular-nums text-white sm:text-ui">{current}</span>
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
      <FieldShell field={field} {...(error ? { error } : {})}>
        <OtpBoxes
          field={field}
          value={text}
          disabled={disabled}
          onChange={onChange}
          onSubmit={onSubmit}
        />
      </FieldShell>
    );
  }

  if (field.type === 'multiline') {
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <textarea
          rows={4}
          disabled={disabled}
          value={text}
          {...(field.max_length ? { maxLength: field.max_length } : {})}
          {...(field.placeholder ? { placeholder: field.placeholder } : {})}
          className="w-full resize-y border border-edge bg-input px-2.5 py-1 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted sm:text-ui"
          onChange={(event) => onChange(field.id, event.target.value)}
        />
      </FieldShell>
    );
  }

  return (
    <FieldShell field={field} {...(error ? { error } : {})}>
      <Input
        type={field.type === 'password' ? 'password' : 'text'}
        disabled={disabled}
        value={text}
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
 * A one-time code is entered as BOXES, one digit each — the shape every bank and
 * every 2FA screen has taught, and the same `[1] [2] [3] [ ] [ ] [ ]` the TUI
 * band paints. Only digits get in, wherever they come from: typing, an SMS
 * autofill, or a paste of the whole `123 456` line, which fills the boxes from
 * the caret onward instead of dropping six characters into one box.
 */
function OtpBoxes({
  field,
  value,
  disabled,
  onChange,
  onSubmit,
}: {
  field: HumanInputField;
  value: string;
  disabled: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
}) {
  const { min, max } = humanInputOtp(field);
  const digits = humanInputOtpDigits(field, value);
  const boxes = useRef<Array<HTMLInputElement | null>>([]);

  const focusAt = (index: number) => {
    const box = boxes.current[Math.max(0, Math.min(max - 1, index))];
    box?.focus();
    box?.select();
  };

  const fill = (index: number, raw: string) => {
    const typed = humanInputOtpDigits(field, raw);
    if (!typed) return;
    const next = humanInputOtpDigits(
      field,
      digits.slice(0, index) + typed + digits.slice(index + typed.length),
    );
    onChange(field.id, next);
    focusAt(index + typed.length);
  };

  const keyDown = (index: number, event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') {
      event.preventDefault();
      onSubmit();
      return;
    }
    if (event.key === 'Backspace') {
      // An empty box deletes the one BEFORE it — otherwise a correction takes
      // two presses and the caret never walks back.
      event.preventDefault();
      const at = digits[index] === undefined ? index - 1 : index;
      if (at < 0) return;
      onChange(field.id, digits.slice(0, at) + digits.slice(at + 1));
      focusAt(at);
      return;
    }
    if (event.key === 'ArrowLeft') {
      event.preventDefault();
      focusAt(index - 1);
    }
    if (event.key === 'ArrowRight') {
      event.preventDefault();
      focusAt(index + 1);
    }
  };

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
        className="flex max-w-md flex-wrap justify-between gap-1.5 sm:max-w-sm"
        role="group"
        aria-label={field.label}
      >
        {Array.from({ length: max }, (_unused, index) => (
          <input
            key={index}
            ref={(box) => {
              boxes.current[index] = box;
            }}
            type="text"
            inputMode="numeric"
            pattern="[0-9]*"
            maxLength={1}
            disabled={disabled}
            // Only the FIRST box asks for the code, or a platform autofill offers
            // the same six digits six times over.
            autoComplete={index === 0 ? 'one-time-code' : 'off'}
            aria-label={`${field.label} digit ${index + 1}`}
            value={digits[index] ?? ''}
            className="h-11 min-w-8 max-w-11 flex-1 border border-edge bg-input text-center font-mono text-body tabular-nums text-white focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted"
            onChange={(event) => fill(index, event.target.value)}
            onPaste={(event) => {
              event.preventDefault();
              fill(index, event.clipboardData.getData('text'));
            }}
            onKeyDown={(event) => keyDown(index, event)}
            onFocus={(event) => event.target.select()}
          />
        ))}
      </div>
      <p className="font-mono text-chip text-dialog-hint">
        {min === max ? `${max} digits` : `${min}–${max} digits`}
      </p>
    </div>
  );
}
