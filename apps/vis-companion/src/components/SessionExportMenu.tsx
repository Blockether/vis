import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';

import { menuPosition, type AnchorBox, type MenuPosition } from '../lib/anchored-menu';
import type { GatewayClient } from '../lib/gateway';
import {
  SESSION_EXPORT_FORMATS,
  exportFailure,
  exportWays,
  keepExport,
  readExport,
  type ExportWay,
  type ExportedSession,
  type SessionExportFormat,
} from '../lib/session-export';
import type { Notices } from '../lib/use-notices';
import { DownloadIcon, ShareIcon } from './icons';
import { MENU_WIDTH, Menu, MenuHeading, MenuItem } from './Menu';

/** What each way to keep an export does, as the hint of its row. */
const WAY_HINTS: Record<ExportWay, string> = {
  Save: 'In Downloads/Vis Artifacts',
  Download: 'To the downloads of this browser',
  Share: 'To another app or another device',
};

/** One choice of the export, as a menu under the control that opened it. */
function ChoiceMenu({
  label,
  heading,
  at,
  onDismiss,
  children,
}: {
  label: string;
  heading: string;
  at: MenuPosition;
  onDismiss: () => void;
  children: ReactNode;
}) {
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
    <Menu label={label} at={at} onDismiss={onDismiss}>
      <MenuHeading closeLabel={`Close ${label}`} onClose={onDismiss}>
        {heading}
      </MenuHeading>
      <div ref={choices} className="[&>*:last-child]:border-b-0">
        {children}
      </div>
    </Menu>
  );
}

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
  return (
    <ChoiceMenu label={`Export ${name}`} heading="Export as…" at={at} onDismiss={onDismiss}>
      {SESSION_EXPORT_FORMATS.map((format) => (
        <MenuItem
          key={format.id}
          title={format.title}
          hint={format.hint}
          icon={<DownloadIcon className="size-3.5" />}
          onSelect={() => onPick(format.id)}
        />
      ))}
    </ChoiceMenu>
  );
}

/** An export that the gateway made, and that waits for the way to keep it. */
type MadeExport = { session: ExportedSession; format: SessionExportFormat; blob: Blob; ways: ExportWay[] };

/**
 * The export choice of one screen. `open` shows the three formats under `anchor`. A pick makes
 * the file. When this host can keep it in more than one way, the same menu then asks which
 * one, so the share sheet opens from a press. A toast of `notices` says what happened or why
 * it failed.
 */
export function useSessionExport(notices: Notices): {
  open: (client: GatewayClient, session: ExportedSession, anchor: AnchorBox | null) => void;
  menu: ReactNode;
} {
  const [asked, setAsked] = useState<{ client: GatewayClient; session: ExportedSession; at: MenuPosition } | null>(
    null,
  );
  const [made, setMade] = useState<(MadeExport & { at: MenuPosition }) | null>(null);
  const running = useRef(new Set<string>());
  const { push, dismiss } = notices;
  const open = useCallback((client: GatewayClient, session: ExportedSession, anchor: AnchorBox | null) => {
    const at = menuPosition(anchor, MENU_WIDTH);
    if (at) setAsked({ client, session, at });
  }, []);
  const close = useCallback(() => setAsked(null), []);
  const drop = useCallback(() => setMade(null), []);
  const title = 'Export';
  const keep = async ({ session, format, blob }: MadeExport, way: ExportWay) => {
    const id = `export:${session.id}:${format}`;
    try {
      const kept = await keepExport(blob, session, format, way);
      if (kept.text) push({ id, kind: 'ok', title, text: kept.text, path: kept.path, durationMs: 8_000 });
      else dismiss(id);
    } catch (cause) {
      push({ id, kind: 'err', title, text: exportFailure(cause, format) });
    }
  };
  const run = async (client: GatewayClient, session: ExportedSession, format: SessionExportFormat, at: MenuPosition) => {
    const id = `export:${session.id}:${format}`;
    if (running.current.has(id)) return;
    running.current.add(id);
    // The pending toast stays until the result replaces it.
    push({ id, kind: 'neutral', title, text: `Exporting ${session.title.trim() || 'the session'}…`, durationMs: 120_000 });
    try {
      const blob = await readExport(client, session, format);
      const ways = exportWays(session, format);
      if (ways.length > 1) {
        dismiss(id);
        setMade({ session, format, blob, ways, at });
      } else await keep({ session, format, blob, ways }, ways[0]!);
    } catch (cause) {
      push({ id, kind: 'err', title, text: exportFailure(cause, format) });
    } finally {
      running.current.delete(id);
    }
  };
  const menu = made ? (
    <ChoiceMenu label="Keep the export" heading="The export is ready" at={made.at} onDismiss={drop}>
      {made.ways.map((way) => (
        <MenuItem
          key={way}
          title={way}
          hint={WAY_HINTS[way]}
          icon={way === 'Share' ? <ShareIcon className="size-3.5" /> : <DownloadIcon className="size-3.5" />}
          onSelect={() => {
            drop();
            void keep(made, way);
          }}
        />
      ))}
    </ChoiceMenu>
  ) : asked ? (
    <SessionExportMenu
      title={asked.session.title}
      at={asked.at}
      onDismiss={close}
      onPick={(format) => {
        close();
        void run(asked.client, asked.session, format, asked.at);
      }}
    />
  ) : null;
  return { open, menu };
}
