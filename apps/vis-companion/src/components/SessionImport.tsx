import { useCallback, useRef, type ReactNode } from 'react';

import type { GatewayClient } from '../lib/gateway';
import { SESSION_IMPORT_ACCEPT, importFailure, type ImportTarget } from '../lib/session-import';
import type { Session } from '../lib/types';
import type { Notices } from '../lib/use-notices';

/**
 * The session import of one screen. `open` shows the file picker of the host. The selected
 * file becomes a new session at `target`. A toast of `notices` says what happened or why it
 * failed, and `onImported` gets the new session. Render `input` one time.
 */
export function useSessionImport(notices: Notices): {
  open: (client: GatewayClient, target: ImportTarget, onImported: (session: Session) => void) => void;
  input: ReactNode;
} {
  const field = useRef<HTMLInputElement>(null);
  const asked = useRef<{
    client: GatewayClient;
    target: ImportTarget;
    onImported: (session: Session) => void;
  } | null>(null);
  const { push } = notices;
  const open = useCallback(
    (client: GatewayClient, target: ImportTarget, onImported: (session: Session) => void) => {
      asked.current = { client, target, onImported };
      field.current?.click();
    },
    [],
  );
  const run = async (file: File) => {
    const request = asked.current;
    if (!request) return;
    const id = `import:${file.name}:${file.size}`;
    const title = 'Import';
    // The pending toast stays until the result replaces it.
    push({ id, kind: 'neutral', title, text: `Importing ${file.name}…`, durationMs: 120_000 });
    try {
      const session = await request.client.importSession(file, request.target);
      const name = session.title?.trim() || 'the session';
      push({ id, kind: 'ok', title, text: `Imported ${name} as a new session.`, durationMs: 8_000 });
      request.onImported(session);
    } catch (cause) {
      push({ id, kind: 'err', title, text: importFailure(cause) });
    }
  };
  const input = (
    <input
      ref={field}
      type="file"
      accept={SESSION_IMPORT_ACCEPT}
      aria-label="Session file to import"
      hidden
      onChange={(event) => {
        const file = event.target.files?.[0];
        // The same file can be picked again after a failure.
        event.target.value = '';
        if (file) void run(file);
      }}
    />
  );
  return { open, input };
}
