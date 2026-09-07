/** Pending MCP sign-in. App callbacks are automatic; manual input is explicit on web/desktop. */
import type { McpAuthFlow } from '../lib/types';
import { Button, Input } from './ui';

export function McpAuth({ flow, input, busy, onInput, onFinish, onCancel, onOpen }: {
  flow: McpAuthFlow; input: string; busy: boolean; onInput: (value: string) => void;
  onFinish: () => void; onCancel: () => void; onOpen: () => void;
}) {
  const app = flow.callback_mode === 'app';
  return <div className="col-span-2 mt-2 space-y-2 border-t border-dialog-edge pt-2">
    <p role="status" className="font-mono text-ui text-dialog-hint">Waiting for authorization…</p>
    <p className="font-mono text-ui text-dialog-hint">
      {app ? 'Approve sign-in in the browser. It will return to Vis and finish automatically.'
        : 'Approve sign-in in the browser. If it cannot reach your gateway, paste the final callback URL below.'}
      {' '}Tokens stay on your gateway. No OAuth data goes through the notification relay.
    </p>
    <Button variant="secondary" onClick={onOpen}>
      Open sign-in page again
    </Button>
    {!app && <Input aria-label="Paste the final callback URL" value={input} onChange={event => onInput(event.target.value)}
      placeholder="http://127.0.0.1:…/callback?code=…" inputMode="url" autoCapitalize="none" autoCorrect="off" />}
    <div className="flex flex-wrap justify-end gap-2">
      <Button variant="secondary" onClick={onCancel}>Cancel</Button>
      {!app && <Button disabled={busy || !input.trim()} onClick={onFinish}>Finish sign-in</Button>}
    </div>
  </div>;
}
