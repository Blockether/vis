import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';

import { menuPosition, type AnchorBox, type MenuPosition } from '../lib/anchored-menu';
import type { GatewayClient } from '../lib/gateway';
import {
  SESSION_EXPORT_FORMATS,
  exportFailure,
  exportSession,
  type ExportedSession,
  type SessionExportFormat,
} from '../lib/session-export';
import type { Notices } from '../lib/use-notices';
import { DownloadIcon } from './icons';
import { MENU_WIDTH, Menu, MenuHeading, MenuItem } from './Menu';

/** The three formats of a session export, as one menu under the control that opened it. */
export function SessionExportMenu({
  title,
  at,
  onPick,
  onDismiss,
}: {
  /** The title of the session that the menu exports. */
  title: string;
  at: MenuPosition;
  onPick: (format: SessionExportFormat) => void;
  onDismiss: () => void;
}) {
  const name = title.trim() || 'this session';
  const choices = useRef<HTMLDivElement>(null);
  useEffect(() => {
    choices.current?.querySelector('button')?.focus();
  }, []);
  // Escape belongs to the open menu. The session screen under it reads Escape as "stop the
  // turn", so the menu takes the key before that listener gets it.
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.preventDefault();
      event.stopPropagation();
      onDismiss();
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [onDismiss]);
  return (
    <Menu label={`Export ${name}`} at={at} onDismiss={onDismiss}>
      <MenuHeading>Export {name} as…</MenuHeading>
      <div ref={choices} className="[&>*:last-child]:border-b-0">
        {SESSION_EXPORT_FORMATS.map((format) => (
          <MenuItem
            key={format.id}
            title={format.title}
            hint={format.hint}
            icon={<DownloadIcon className="size-3.5" />}
            onSelect={() => onPick(format.id)}
          />
        ))}
      </div>
    </Menu>
  );
}

/**
 * The export choice of one screen. `open` shows the three formats under `anchor`. A pick makes
 * the file, and a toast of `notices` says what happened or why it failed.
 */
export function useSessionExport(notices: Notices): {
  open: (client: GatewayClient, session: ExportedSession, anchor: AnchorBox | null) => void;
  menu: ReactNode;
} {
  const [asked, setAsked] = useState<{ client: GatewayClient; session: ExportedSession; at: MenuPosition } | null>(
    null,
  );
  const running = useRef(new Set<string>());
  const { push, dismiss } = notices;
  const open = useCallback((client: GatewayClient, session: ExportedSession, anchor: AnchorBox | null) => {
    const at = menuPosition(anchor, MENU_WIDTH);
    if (at) setAsked({ client, session, at });
  }, []);
  const close = useCallback(() => setAsked(null), []);
  const run = async (client: GatewayClient, session: ExportedSession, format: SessionExportFormat) => {
    const id = `export:${session.id}:${format}`;
    if (running.current.has(id)) return;
    running.current.add(id);
    const title = 'Export';
    // The pending toast stays until the result replaces it.
    push({ id, kind: 'neutral', title, text: `Exporting ${session.title.trim() || 'the session'}…`, durationMs: 120_000 });
    try {
      const message = await exportSession(client, session, format);
      if (message) push({ id, kind: 'ok', title, text: message, durationMs: 8_000 });
      else dismiss(id);
    } catch (cause) {
      push({ id, kind: 'err', title, text: exportFailure(cause, format) });
    } finally {
      running.current.delete(id);
    }
  };
  const menu = asked ? (
    <SessionExportMenu
      title={asked.session.title}
      at={asked.at}
      onDismiss={close}
      onPick={(format) => {
        close();
        void run(asked.client, asked.session, format);
      }}
    />
  ) : null;
  return { open, menu };
}
