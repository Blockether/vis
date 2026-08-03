import { useCallback, useEffect, useState } from 'react';
import { createPortal } from 'react-dom';
import { Banner, Button, DialogFrame, Input } from './ui';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyHumanInputEvent,
  initialHumanInputValues,
  isHumanInputAnswerable,
  isHumanInputBlank,
  isHumanInputEvent,
  toggleHumanInputOption,
  type HumanInputField,
  type HumanInputRequest,
  type HumanInputValues,
} from '../lib/human-input';

/**
 * The companion half of the typed human-input pause.
 *
 * An extension can BLOCK a run on the operator. The engine publishes that pause
 * on every channel the request names — the TUI dialog and this screen are two
 * views of the SAME request, answered through the same engine coercion — and a
 * phone that is not looking gets the push the gateway sends alongside.
 *
 * Live `human_input.request` frames open the form; `human_input.close` closes it
 * no matter who answered, so a request settled in the TUI cannot leave a dead
 * dialog here.
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

  const request = pending[0] ?? null;
  const requestId = request?.id ?? '';

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
    if (!request || busy) return;
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

  if (!request) return null;

  const answerable = isHumanInputAnswerable(request, values);

  return createPortal(
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-[max(1rem,env(safe-area-inset-top))] pb-[max(1rem,env(safe-area-inset-bottom))] pl-[max(1rem,env(safe-area-inset-left))] pr-[max(1rem,env(safe-area-inset-right))]"
      role="presentation"
    >
      <div className="max-h-full w-full max-w-md overflow-y-auto" role="presentation">
        <DialogFrame
          title={request.title}
          {...(request.is_cancellable ? { onClose: cancel } : {})}
          footer={
            <span>
              {pending.length > 1
                ? `${pending.length - 1} more request${pending.length > 2 ? 's' : ''} waiting`
                : 'This run is waiting for you'}
            </span>
          }
        >
          <div className="space-y-3 p-4">
            {request.description && (
              <p className="font-mono text-meta text-dialog-hint">{request.description}</p>
            )}
            {request.fields.map((field) => (
              <HumanInputFieldRow
                key={`${requestId}:${field.id}`}
                field={field}
                value={values[field.id]}
                error={fieldErrors[field.id]}
                disabled={busy}
                onChange={setValue}
              />
            ))}
            {error && <Banner kind="err">{error}</Banner>}
            <div className="flex justify-end gap-2">
              {request.is_cancellable && (
                <Button variant="ghost" disabled={busy} onClick={cancel}>
                  {request.cancel_label}
                </Button>
              )}
              <Button disabled={busy || !answerable} onClick={submit}>
                {busy ? 'Sending...' : request.submit_label}
              </Button>
            </div>
          </div>
        </DialogFrame>
      </div>
    </div>,
    document.body,
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
        {field.is_required ? ' *' : ''}
      </span>
      {children}
      {field.help && <p className="font-mono text-chip text-dialog-hint">{field.help}</p>}
      {error && <p className="font-mono text-chip text-err">{error}</p>}
    </div>
  );
}

/**
 * One row of the form. The field set is the engine's closed one, and every row
 * carries the same marks the TUI dialog draws: `●`/`○` for a single choice,
 * `[x]`/`[ ]` for a toggle.
 */
function HumanInputFieldRow({
  field,
  value,
  error,
  disabled,
  onChange,
}: {
  field: HumanInputField;
  value: HumanInputValues[string] | undefined;
  error?: string;
  disabled: boolean;
  onChange: (id: string, value: HumanInputValues[string]) => void;
}) {
  const missing = error ?? (isHumanInputBlank(field, value) ? undefined : undefined);
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
          className="flex w-full items-center gap-2 border border-edge bg-input px-2.5 py-1 text-left font-mono text-meta text-white transition-colors hover:border-accent disabled:cursor-not-allowed disabled:text-muted sm:text-ui"
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
      <FieldShell field={field} {...(missing ? { error: missing } : {})}>
        <div className="space-y-1">
          {options.map((option) => {
            const on = isMulti ? chosen.includes(option.value) : value === option.value;
            return (
              <button
                key={option.value}
                type="button"
                disabled={disabled}
                aria-pressed={on}
                className={`flex w-full items-center gap-2 border px-2.5 py-1 text-left font-mono text-meta transition-colors disabled:cursor-not-allowed disabled:text-muted sm:text-ui ${
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
      />
    </FieldShell>
  );
}
