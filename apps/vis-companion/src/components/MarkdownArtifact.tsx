import { memo, type ReactElement, type ReactNode, useEffect, useState } from 'react';

import { diffHeaderAnchors, diffHeaderPath } from '../lib/diff';
import { useOpenPath } from '../lib/open-path';
import { Markdown } from './ChatContent';
import { readArtifactText } from './TextArtifact';

export type DocumentChrome = (parts: {
  actions: ReactNode;
  note: string;
  body: ReactNode;
}) => ReactElement;
/**
 * A NOTE, READ AS PROSE.
 *
 * Opening a note renders it — headings as headings, through the transcript's
 * own `Markdown`, so there is one renderer and one set of type decisions in the
 * app. A `.txt` or a `.log` is read verbatim instead. The reader only reads: a
 * conversation about a change belongs in an app attachment, whose threads the
 * host stores and the agent answers.
 */
export const MarkdownArtifact = memo(function MarkdownArtifact({
  source,
  plain,
  chrome,
  onOpenAttachment,
}: {
  source: Blob | string;
  /** A `.txt`/`.log` note: the same reader, showing the file verbatim. */
  plain?: boolean;
  /** The band and the frame this document is read inside. */
  chrome: DocumentChrome;
  onOpenAttachment?: (attachmentId: string) => void;
}) {
  const [loaded, setLoaded] = useState<{ source: Blob | string; text: string } | null>(null);
  const [failed, setFailed] = useState<Blob | string | null>(null);

  useEffect(() => {
    let alive = true;
    readArtifactText(source)
      .then((text) => {
        if (alive) setLoaded({ source, text });
      })
      .catch(() => {
        if (alive) setFailed(source);
      });
    return () => {
      alive = false;
    };
  }, [source]);

  const text = loaded?.source === source ? loaded.text : null;
  if (text === null) {
    return chrome({
      actions: null,
      note: '',
      body: (
        <p className="p-4 font-mono text-meta text-dialog-hint">
          {failed === source ? 'This artifact could not be read.' : 'Loading…'}
        </p>
      ),
    });
  }
  return chrome({
    actions: null,
    note: '',
    body: (
      <div className="min-h-0 min-w-0 flex-1 overflow-y-auto bg-panel px-3 py-3 text-body text-foreground sm:px-4">
        {plain ? <PlainText text={text} /> : <Markdown onOpenAttachment={onOpenAttachment}>{text}</Markdown>}
      </div>
    ),
  });
});

/**
 * PLAIN TEXT IS A DOCUMENT TOO.
 *
 * A `.txt` or a `.log` has no headings to render, so each LINE is a block of
 * its own, and a patch paints each line in the tone of its change.
 */
export const PlainText = memo(function PlainText({
  text,
  diff = false,
}: {
  text: string;
  diff?: boolean;
}) {
  // A header points into the file the hunk under it changes, so the whole patch is
  // measured once here rather than line by line as it is painted.
  const lines = text.split('\n');
  const anchors = diff ? diffHeaderAnchors(lines) : [];
  return (
    <div className="font-mono text-body text-foreground">
      {diff && !text ? <p>No changes in this snapshot.</p> : null}
      {lines.map((line, at) => (
        <p
          key={at}
          className={`min-h-[18px] break-words whitespace-pre-wrap ${diff ? lineTone(line) : ''}`}
        >
          {diff ? diffLine(line, anchors[at]) : line}
        </p>
      ))}
    </div>
  );
});

function lineTone(line: string): string {
  if (line.startsWith('+')) return 'text-ok';
  if (line.startsWith('-')) return 'text-error';
  if (line.startsWith('@@') || line.startsWith('diff --git')) return 'text-link-fg';
  return '';
}

/**
 * THE FILE A PATCH NAMES, PRESSED.
 *
 * A diff header is the only place a snapshot says which file its hunks belong to,
 * and reading the change is usually the moment you want the file itself. The press
 * stops at the path, so the rest of the line stays plain text; with no
 * opener published the path stays plain words, exactly as an activity row does.
 */
function DiffPath({ path, line, words }: { path: string; line?: number; words: string }) {
  const openPath = useOpenPath();
  if (!openPath) return <>{words}</>;
  const press = (event: { preventDefault: () => void; stopPropagation: () => void }) => {
    event.preventDefault();
    event.stopPropagation();
    openPath(path, line);
  };
  return (
    <span
      role="button"
      tabIndex={0}
      aria-label={`Open ${path}`}
      title={path}
      className="cursor-pointer underline-offset-2 hover:underline"
      onPointerDown={(event) => event.stopPropagation()}
      onPointerUp={(event) => event.stopPropagation()}
      onClick={press}
      onKeyDown={(event) => {
        if (event.key === 'Enter' || event.key === ' ') press(event);
      }}
    >
      {words}
    </span>
  );
}

function diffLine(line: string, anchor?: number): ReactNode {
  const named = diffHeaderPath(line);
  if (!named) return line;
  return (
    <>
      {line.slice(0, named.start)}
      <DiffPath path={named.path} line={anchor} words={line.slice(named.start, named.end)} />
      {line.slice(named.end)}
    </>
  );
}
