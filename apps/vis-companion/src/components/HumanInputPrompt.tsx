import { useCallback, useEffect, useRef, useState } from 'react';
import { createPortal } from 'react-dom';
import { Banner, Button, DialogFrame, Input } from './ui';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyHumanInputEvent,
  initialHumanInputValues,
  isHumanInputAnswerable,
  isHumanInputEvent,
  clampHumanInputRange,
  humanInputRange,
  toggleHumanInputOption,
  type HumanInputField,
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
 * Live `human_input.request` frames open the form; `human_input.close` closes it
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
  const [values, setValues] = useState<HumanInputValues>({});
  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});
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
        .humanInputRequests(sid, controller.signal)
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
      if (!isHumanInputEvent(event)) return;
      setPending((current) => applyHumanInputEvent(current, event));
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
    setValues(request ? initialHumanInputValues(request) : {});
    setFieldErrors({});
    setError(null);
    setBusy(false);
  }, [request]);

  const setValue = useCallback((id: string, value: HumanInputValues[string]) => {
    setValues((current) => ({ ...current, [id]: value }));
    setFieldErrors((current) => {
      if (!(id in current)) return current;
      const next = { ...current };
      delete next[id];
      return next;
    });
  }, []);

  const drop = useCallback((id: string) => {
    setPending((current) => current.filter((row) => row.id !== id));
  }, []);

  const submit = useCallback(() => {
    // The engine REFUSES an answer that leaves a required field blank. Checking
    // it here too means no path — the button, a stray Enter, a future shortcut —
    // can send one and bounce the operator off a rejection banner.
    if (!request || busy || !isHumanInputAnswerable(request, values)) return;
    setBusy(true);
    setError(null);
    client
      .submitHumanInput(sid, request.id, values)
      .then((outcome) => {
        if (outcome?.is_accepted) {
          // The close event drops it too; doing it here as well keeps the
          // dialog from lingering for a stream that is briefly offline.
          drop(request.id);
          return;
        }
        setFieldErrors(outcome?.errors ?? {});
        setError('The daemon rejected this answer.');
        setBusy(false);
      })
      .catch((cause: unknown) => {
        setError(cause instanceof Error ? cause.message : 'Could not send this answer.');
        setBusy(false);
      });
  }, [busy, client, drop, request, sid, values]);

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

  return createPortal(
    <HumanInputSheet
      request={request}
      values={values}
      fieldErrors={fieldErrors}
      error={error}
      busy={busy}
      waiting={pending.length - 1}
      bodyRef={bodyRef}
      onChange={setValue}
      onSubmit={submit}
      onCancel={cancel}
    />,
    document.body,
  );
}

/**
 * The pause itself, with no gateway behind it: a request, the answers typed so
 * far, and three callbacks. The container above owns the socket, this owns the
 * pixels — which is why the design gallery can photograph the SHIPPED sheet
 * instead of a look-alike that drifts away from it.
 */
export function HumanInputSheet({
  request,
  values,
  fieldErrors = {},
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
  fieldErrors?: Record<string, string>;
  error?: string | null;
  busy?: boolean;
  waiting?: number;
  bodyRef?: React.RefObject<HTMLDivElement | null>;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
  onCancel: () => void;
}) {
  const answerable = isHumanInputAnswerable(request, values);

  // A phone answers with its thumb, so the sheet sits on the BOTTOM edge and
  // only becomes a centred card once there is a mouse-sized window.
  return (
    <div
      className="fixed inset-0 z-50 flex items-end justify-center bg-black/60 p-[max(1rem,env(safe-area-inset-top))] pb-[max(1rem,env(safe-area-inset-bottom))] pl-[max(1rem,env(safe-area-inset-left))] pr-[max(1rem,env(safe-area-inset-right))] sm:items-center"
      role="presentation"
    >
      <div className="w-full max-w-md" role="presentation">
        <DialogFrame
          title={request.title}
          {...(request.is_cancellable ? { onClose: onCancel } : {})}
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
                    variant="ghost"
                    className="flex-1 sm:flex-none"
                    disabled={busy}
                    onClick={onCancel}
                  >
                    {request.cancel_label}
                  </Button>
                )}
                <Button
                  className="flex-1 sm:flex-none"
                  disabled={busy || !answerable}
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
            {request.fields.map((field) => (
              <HumanInputFieldRow
                key={`${request.id}:${field.id}`}
                field={field}
                value={values[field.id]}
                error={fieldErrors[field.id]}
                disabled={busy}
                onChange={onChange}
                onSubmit={onSubmit}
              />
            ))}
          </div>
        </DialogFrame>
      </div>
    </div>
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
        {/* Said in full, not as a `*`: this is the field that will refuse the form. */}
        {field.is_required && <span className="ml-1.5 text-err">REQUIRED</span>}
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
 * carries the same marks the TUI band draws: `●`/`○` for a single choice,
 * `[x]`/`[ ]` for a toggle.
 */
function HumanInputFieldRow({
  field,
  value,
  error,
  disabled,
  onChange,
  onSubmit,
}: {
  field: HumanInputField;
  value: HumanInputValues[string] | undefined;
  error?: string;
  disabled: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
  onSubmit: () => void;
}) {
  const chosen = Array.isArray(value) ? value : [];
  const options = field.options ?? [];

  if (field.type === 'checkbox') {
    const on = value === true;
    return (
      <FieldShell field={field} {...(error ? { error } : {})}>
        <button
          type="button"
          disabled={disabled}
          aria-pressed={on}
          className="flex w-full items-center gap-2 border border-edge bg-input px-2.5 py-1 text-left font-mono text-meta text-white transition-colors hover:border-accent focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted sm:text-ui"
          onClick={() => onChange(field.id, !on)}
        >
          <span aria-hidden="true">{on ? '[x]' : '[ ]'}</span>
          <span className="truncate">{field.label}</span>
        </button>
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
              <button
                key={option.value}
                type="button"
                disabled={disabled}
                {...(isMulti ? { 'aria-pressed': on } : { role: 'radio', 'aria-checked': on })}
                className={`flex w-full items-center gap-2 border px-2.5 py-1 text-left font-mono text-meta transition-colors focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted sm:text-ui ${
                  on ? 'border-accent bg-hover text-accent-ink' : 'border-edge bg-input text-white'
                }`}
                onClick={() =>
                  onChange(
                    field.id,
                    isMulti
                      ? toggleHumanInputOption(field, { [field.id]: chosen }, option.value)
                      : option.value,
                  )
                }
              >
                <span aria-hidden="true">{isMulti ? (on ? '[x]' : '[ ]') : on ? '●' : '○'}</span>
                <span className="truncate">{option.label}</span>
              </button>
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
          <input
            type="range"
            disabled={disabled}
            min={min}
            max={max}
            step={step}
            value={current}
            aria-label={field.label}
            className="h-1 min-w-0 flex-1 cursor-pointer appearance-none rounded-full bg-edge accent-accent disabled:cursor-not-allowed"
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
