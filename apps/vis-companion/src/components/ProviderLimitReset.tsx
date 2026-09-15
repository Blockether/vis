import { useCallback, useEffect, useRef, useState } from 'react';
import type { ProviderResetCredits, ProviderResetOutcome } from '../lib/types';
import { Button, DialogFrame, Modal } from './ui';

export interface ProviderLimitResetProps {
  credits?: ProviderResetCredits;
  isChecking?: boolean;
  hasPending?: boolean;
  onConsume: (accountId: string) => Promise<ProviderResetOutcome>;
}

const outcomeText: Record<ProviderResetOutcome, string> = {
  reset: 'Limits reset. Your task has not been resent.',
  nothing_to_reset: 'Nothing to reset. No reset was used.',
  no_credit: 'No resets available. No reset was used.',
  already_redeemed: 'This request was already processed. No additional reset was used.',
};

/** Deliberate account-wide mutation, separate from the read-only quota refresh. */
export function ProviderLimitReset({
  credits,
  isChecking = false,
  hasPending = false,
  onConsume,
}: ProviderLimitResetProps) {
  const [confirmAccount, setConfirmAccount] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [notice, setNotice] = useState<{ text: string; error: boolean } | null>(null);
  const inFlight = useRef(false);
  const trigger = useRef<HTMLButtonElement>(null);
  const group = useRef<HTMLDivElement>(null);
  const dismissButton = useRef<HTMLButtonElement>(null);
  const accountId = credits?.account_id;
  const available = credits?.status === 'ok' ? credits.available_count : null;
  const canReset = !!accountId && (hasPending || (available !== null && available > 0));
  const accountChanged = confirmAccount !== null && accountId !== confirmAccount;
  const summary = isChecking
    ? 'Checking available resets…'
    : available !== null
      ? `${available} ${available === 1 ? 'reset' : 'resets'} available`
      : credits?.status === 'unsupported'
        ? 'Limit resets are not available for this account.'
        : !credits
          ? 'The gateway did not report reset availability. Check that it is up to date.'
          : 'Available resets could not be checked.';

  const dismiss = useCallback(() => {
    if (inFlight.current) return;
    setConfirmAccount(null);
    const target = trigger.current && !trigger.current.disabled ? trigger.current : group.current;
    target?.focus({ preventScroll: true });
  }, []);

  useEffect(() => {
    if (!confirmAccount) return;
    // Keep Escape in this dialog even when disabling the focused submit button blurs it.
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      dismiss();
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [confirmAccount, dismiss]);

  useEffect(() => {
    if (notice) dismissButton.current?.focus({ preventScroll: true });
  }, [notice]);

  async function consume() {
    if (!confirmAccount || accountChanged || !canReset || isChecking || inFlight.current) return;
    inFlight.current = true;
    setBusy(true);
    setNotice(null);
    try {
      const outcome = await onConsume(confirmAccount);
      setNotice({ text: outcomeText[outcome], error: false });
    } catch {
      setNotice({
        text: 'Reset could not be confirmed. Check the result by retrying the same request; do not start another reset.',
        error: true,
      });
    } finally {
      inFlight.current = false;
      setBusy(false);
    }
  }

  return (
    <div
      ref={group}
      tabIndex={-1}
      className="border-t border-dialog-edge pt-3 font-mono text-ui text-dialog-foreground"
      aria-label="Codex limit resets"
      role="group"
    >
      <div className="flex items-center justify-between gap-3">
        <p role="status" className="min-w-0">
          {summary}
        </p>
        {(available !== null || hasPending) && (
          <Button
            ref={trigger}
            aria-haspopup="dialog"
            aria-expanded={!!confirmAccount}
            className="shrink-0"
            density="compact"
            variant="secondary"
            disabled={!canReset || isChecking || busy}
            onClick={() => {
              setNotice(null);
              setConfirmAccount(accountId!);
            }}
          >
            {hasPending ? 'Check reset result…' : 'Reset limits…'}
          </Button>
        )}
      </div>
      {/* Keep confirmation and results out of the settings flow to avoid layout jumps. */}
      {confirmAccount && (
        <Modal size="fit" onDismiss={dismiss}>
          <DialogFrame title="Reset limits" onClose={busy ? undefined : dismiss}>
            <div className="space-y-3 p-3 font-mono text-ui text-dialog-foreground">
              <p className="break-all">Account: {confirmAccount}</p>
              {notice ? (
                <p
                  role={notice.error ? 'alert' : 'status'}
                  className={notice.error ? 'text-err-ink' : undefined}
                >
                  {notice.text}
                </p>
              ) : (
                <p>
                  {hasPending
                    ? 'Retry the same reset request. This will not spend a second reset.'
                    : 'Use 1 reset for this ChatGPT account? This affects all devices and sessions. It cannot be undone.'}
                </p>
              )}
              {!notice && accountChanged && (
                <p role="alert" className="text-err-ink">
                  The account changed. Cancel and check the selected account.
                </p>
              )}
              <div className="flex flex-wrap justify-end gap-2">
                <Button
                  ref={dismissButton}
                  density="compact"
                  variant="secondary"
                  autoFocus
                  disabled={busy}
                  onClick={dismiss}
                >
                  {notice ? 'Done' : 'Cancel'}
                </Button>
                {!notice && (
                  <Button
                    density="compact"
                    disabled={busy || isChecking || !canReset || accountChanged}
                    onClick={() => void consume()}
                  >
                    {busy ? 'Checking result…' : hasPending ? 'Retry same request' : 'Use 1 reset'}
                  </Button>
                )}
              </div>
            </div>
          </DialogFrame>
        </Modal>
      )}
    </div>
  );
}
