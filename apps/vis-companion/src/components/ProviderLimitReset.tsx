import { useRef, useState } from 'react';
import type { ProviderResetCredits, ProviderResetOutcome } from '../lib/types';
import { Button } from './ui';

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
export function ProviderLimitReset({ credits, isChecking = false, hasPending = false, onConsume }: ProviderLimitResetProps) {
  const [confirmAccount, setConfirmAccount] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [notice, setNotice] = useState<{ text: string; error: boolean } | null>(null);
  const inFlight = useRef(false);
  const accountId = credits?.account_id;
  const available = credits?.status === 'ok' ? credits.available_count : null;
  const canReset = !!accountId && (hasPending || (available !== null && available > 0));
  const accountChanged = confirmAccount !== null && accountId !== confirmAccount;
  const summary = isChecking ? 'Checking available resets…'
    : available !== null ? `${available} ${available === 1 ? 'reset' : 'resets'} available`
    : credits?.status === 'unsupported' ? 'Limit resets are not available for this account.'
    : !credits ? 'The gateway did not report reset availability. Check that it is up to date.'
    : 'Available resets could not be checked.';

  async function consume() {
    if (!confirmAccount || accountChanged || !canReset || isChecking || inFlight.current) return;
    inFlight.current = true;
    setBusy(true);
    setNotice(null);
    try {
      const outcome = await onConsume(confirmAccount);
      setNotice({ text: outcomeText[outcome], error: false });
    } catch {
      setNotice({ text: 'Reset could not be confirmed. Check the result by retrying the same request; do not start another reset.', error: true });
    } finally {
      inFlight.current = false;
      setBusy(false);
      setConfirmAccount(null);
    }
  }

  return (
    <div className="space-y-3 border-t border-dialog-edge pt-3 font-mono text-ui text-dialog-foreground" aria-label="Codex limit resets" role="group">
      <p role="status">{summary}</p>
      {notice && <p role={notice.error ? 'alert' : 'status'} className={notice.error ? 'text-err-ink' : 'text-dialog-foreground'}>{notice.text}</p>}
      {confirmAccount ? (
        <div className="space-y-3" onKeyDown={event => {
          if (event.key === 'Escape') {
            event.stopPropagation();
            if (!busy) setConfirmAccount(null);
          }
        }}>
          <p className="break-all">Account: {confirmAccount}</p>
          <p>{hasPending
            ? 'Retry the same reset request. This will not spend a second reset.'
            : 'Use 1 reset for this ChatGPT account? This affects all devices and sessions. It cannot be undone.'}</p>
          {accountChanged && <p role="alert" className="text-err-ink">The account changed. Cancel and check the selected account.</p>}
          <div className="flex flex-wrap gap-2">
            <Button density="compact" variant="secondary" autoFocus disabled={busy} onClick={() => setConfirmAccount(null)}>Cancel</Button>
            <Button density="compact" disabled={busy || isChecking || !canReset || accountChanged} onClick={() => void consume()}>
              {busy ? 'Checking result…' : hasPending ? 'Retry same request' : 'Use 1 reset'}
            </Button>
          </div>
        </div>
      ) : (available !== null || hasPending) && (
        <div className="flex flex-wrap gap-2">
          <Button density="compact" variant="secondary" disabled={!canReset || isChecking || busy} onClick={() => { setNotice(null); setConfirmAccount(accountId!); }}>
            {hasPending ? 'Check reset result…' : 'Reset limits…'}
          </Button>
        </div>
      )}
    </div>
  );
}
