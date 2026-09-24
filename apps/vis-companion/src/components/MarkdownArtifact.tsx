import {
  memo,
  type ReactElement,
  type ReactNode,
  useCallback,
  useEffect,
  useRef,
  useState,
} from 'react';

import { diffHeaderAnchors, diffHeaderPath } from '../lib/diff';
import type { GatewayClient } from '../lib/gateway';
import { useOpenPath } from '../lib/open-path';
import {
  GENERAL_LABEL,
  parseAnnotated,
  quoteOf,
  renderAnnotated,
  type MarkdownComment,
} from '../lib/markdown-annotations';
import {
  annotationDraftKey,
  clearAnnotationDraft,
  peekAnnotationDraft,
  readAnnotationDraft,
  sameComments,
  writeAnnotationDraft,
} from '../lib/annotation-drafts';
import { Markdown } from './ChatContent';
import { readArtifactText } from './TextArtifact';
import { CheckIcon, CommentIcon, TrashIcon } from './icons';
import {
  actionRequest,
  availableActions,
  documentInfo,
  planName,
  type PlanAction,
} from '../lib/plans';
import { BandButton, Button, IconButton, PROSE, Spinner } from './ui';
import { useSafeBottomStyle } from '../lib/viewport';

/** The blocks a tap may quote: one paragraph, heading, item or cell. */
const QUOTABLE_BLOCKS = 'p,li,h1,h2,h3,h4,h5,h6,blockquote,pre,td,th';

/** A press that travelled further than this was a scroll, not a tap (CSS px). */
const TAP_SLOP = 10;

/**
 * ONE COLOUR PER COMMENT, AND THE SAME COLOUR IN BOTH PLACES.
 *
 * A remark has an ordinal in the comment list and a matching background hue on
 * its passage. Highlighting never changes the document's content or geometry.
 *
 * The palette is spelled as THEME VARIABLES, never as hard-coded hex: the app's
 * paper is whichever palette this device selected, so an ink chosen for a cream
 * light theme is unreadable the moment a dark one is picked. Every token below is
 * part of the shared Blockether palette (`index.css`) and is re-published by
 * every theme, so the marks move with it.
 */
export const ANNOTATION_COLORS = [
  'var(--warning)',
  'var(--link-fg)',
  'var(--ok)',
  'var(--code-syntax-special)',
  'var(--code-syntax-number)',
  'var(--code-syntax-string)',
  'var(--code-syntax-keyword)',
  'var(--warning-border)',
  'var(--code-syntax-comment)',
  'color-mix(in oklab, var(--code-syntax-special) 60%, var(--link-fg))',
];

export function annotationColor(index: number): string {
  return ANNOTATION_COLORS[index % ANNOTATION_COLORS.length];
}

/** The paper a marked passage sits on: the same hue, thinned to a wash. */
export function annotationWash(index: number): string {
  return `color-mix(in oklab, ${annotationColor(index)} 16%, transparent)`;
}

/**
 * The annotator supplies header actions, metadata and the document column to its
 * enclosing frame. Ordinary documents save from the header; specifications keep
 * their single review action below the document and comments.
 */
/**
 * What the band REPORTS while remarks are waiting to be saved.
 *
 * A comment is added by one press and saved by another, so between them the
 * document on screen is not the document on the gateway. The band says so under
 * the name, in place of the file's kind and weight, until the save lands — and it
 * says it again on the next open, because the remarks come back with it
 * (`lib/annotation-drafts`).
 */
export const UNSAVED_NOTE = 'Unsaved draft';

export type DocumentChrome = (parts: {
  actions: ReactNode;
  note: string;
  body: ReactNode;
}) => ReactElement;
/**
 * A MARKDOWN NOTE, READ AS PROSE AND MARKED UP BY HAND.
 *
 * Opening a note renders it — headings as headings, through the transcript's
 * own `Markdown`, so there is one renderer and one set of type decisions in the
 * app. On top of that it is a document you can TALK BACK TO: select a passage,
 * say what you think, and the remark is kept in the note itself under one
 * `## Comments` heading (`lib/markdown-annotations`).
 *
 * Saving is therefore not an edit in place — it POSTs the whole document back
 * under its own filename, which the gateway stores as the NEXT VERSION of that
 * artifact. The thread of cuts already visible in `ArtifactVersions` is exactly
 * where the annotated copy lands.
 */
export const MarkdownArtifact = memo(function MarkdownArtifact({
  client,
  sid,
  iterationId,
  name,
  mediaType,
  source,
  version,
  plain,
  commentable = false,
  chrome,
  onOpenAttachment,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  source: Blob | string;
  version?: number;
  /** Missing or false is a reader, never an annotation surface. */
  commentable?: boolean;
  /** A `.txt`/`.log` note: the same annotator, reading the file verbatim. */
  plain?: boolean;
  /** The band and the frame this document is read inside. */
  chrome: DocumentChrome;
  onOpenAttachment?: (attachmentId: string) => void;
}) {
  const [loaded, setLoaded] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  const [plansEnabled, setPlansEnabled] = useState(false);

  useEffect(() => {
    if (!commentable || !planName(name)) return;
    const controller = new AbortController();
    void client
      .setting('plans', controller.signal)
      .then((toggle) => setPlansEnabled(toggle.enabled === true))
      .catch(() => setPlansEnabled(false));
    return () => controller.abort();
  }, [client, name, commentable]);

  const sendPlan = useCallback(
    async (action: PlanAction, savedVersion: number) => {
      if ((await client.setting('plans')).enabled !== true) {
        setPlansEnabled(false);
        throw new Error(
          'Plan before coding is off. Enable it in Settings and reopen this document.',
        );
      }
      await client.submitTurn(sid, actionRequest(name, savedVersion, action));
    },
    [client, sid, name],
  );

  useEffect(() => {
    let alive = true;
    setLoaded(null);
    setFailed(false);
    readArtifactText(source)
      .then((next) => {
        if (alive) setLoaded(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
    };
  }, [source]);

  const save = useCallback(
    async (text: string) => {
      const saved = await client.saveArtifactText(
        sid,
        iterationId,
        name,
        mediaType || (plain ? 'text/plain' : 'text/markdown'),
        text,
      );
      return saved.version;
    },
    [client, sid, iterationId, name, mediaType, plain],
  );

  if (failed || loaded === null) {
    return chrome({
      actions: null,
      note: '',
      body: (
        <p className="p-4 font-mono text-meta text-dialog-hint">
          {failed ? 'This artifact could not be read.' : 'Loading…'}
        </p>
      ),
    });
  }
  if (!commentable)
    return chrome({
      actions: null,
      note: '',
      body: (
        <div className="min-h-0 min-w-0 flex-1 overflow-y-auto bg-panel px-3 py-3 text-body text-foreground sm:px-4">
          {plain ? (
            <PlainText text={loaded} />
          ) : (
            <Markdown onOpenAttachment={onOpenAttachment}>{loaded}</Markdown>
          )}
        </div>
      ),
    });
  return (
    <MarkdownAnnotator
      key={`${name}:${version ?? source}`}
      planning={plansEnabled && version ? { filename: name, version, onSend: sendPlan } : undefined}
      text={loaded}
      onSave={save}
      plain={plain}
      chrome={chrome}
      onOpenAttachment={onOpenAttachment}
      draftKey={annotationDraftKey(
        client.base,
        sid,
        iterationId,
        planName(name) ? `${name}:v${version}` : name,
      )}
    />
  );
});

/**
 * PLAIN TEXT IS A DOCUMENT TOO.
 *
 * A `.txt` or a `.log` has no headings to render, but it has the same thing to
 * say back: each LINE is a block, so one tap quotes it exactly as a paragraph is
 * quoted in a note. The lines are `<p>` for that reason — `QUOTABLE_BLOCKS` and
 * the mark painter then need no branch for plain text at all.
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
 * stops at the path, so the rest of the line still quotes as a block; with no
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

const PLAN_LABELS: Record<PlanAction, string> = {
  revise: 'Send for revision',
  approve: 'Approve and start',
};

const SPEC_STATUSES: Record<string, string> = {
  draft: 'Draft',
  'in-review': 'In review',
  ready: 'Ready to implement',
  accepted: 'Ready to implement',
  implementing: 'Implementing',
  done: 'Done',
};

/**
 * The rendered note plus its comments — pure apart from `onSave`, so the whole
 * select/comment/save loop is testable without a gateway.
 */
export const MarkdownAnnotator = memo(function MarkdownAnnotator({
  text,
  onSave,
  plain,
  chrome,
  onOpenAttachment,
  draftKey,
  planning,
  review,
  initialComments,
}: {
  text: string;
  /** Persists the document and answers with the version it became. */
  onSave: (text: string, comments?: MarkdownComment[]) => Promise<number | undefined>;
  /** Read the file verbatim, line by line, instead of rendering markdown. */
  plain?: boolean;
  /** The band and the frame this document is read inside. */
  chrome: DocumentChrome;
  onOpenAttachment?: (attachmentId: string) => void;
  /**
   * WHICH document this is, for the device's own draft store
   * (`lib/annotation-drafts`). Without it the annotator keeps its remarks in
   * memory alone, and leaving the screen throws them away.
   */
  draftKey?: string;
  planning?: {
    filename: string;
    version: number;
    onSend: (action: PlanAction, version: number) => Promise<void>;
  };
  /** A code diff keeps its patch separate from the comments being saved. */
  initialComments?: MarkdownComment[];
  review?: {
    version: number;
    sourceLabel: string;
    onSend: (version: number) => Promise<void>;
  };
}) {
  const parsed = initialComments ? { body: text, comments: initialComments } : parseAnnotated(text);
  const proseRef = useRef<HTMLDivElement | null>(null);
  const [body] = useState(parsed.body);
  // A DOCUMENT REOPENS ON THE WORK THAT WAS LEFT IN IT.
  //
  // Reported: add a comment, leave the artifact by accident, come back — and the
  // remark was simply gone, because "Add comment" and "Save" are two presses and
  // everything between them lived in this component's state. What the device kept
  // is read ONCE, on the first render, and counts as a draft only while it still
  // differs from what the file itself carries.
  const [opened] = useState(() => {
    const kept = draftKey ? peekAnnotationDraft(draftKey) : null;
    const isDraft = kept !== null && !sameComments(kept, parsed.comments);
    return { comments: isDraft ? kept : parsed.comments, isDraft };
  });
  const [comments, setComments] = useState<MarkdownComment[]>(opened.comments);
  const [quote, setQuote] = useState<string | null>(null);
  const [draft, setDraft] = useState('');
  /** The remark this composer is REWRITING, or `null` while it writes a new one. */
  const [editing, setEditing] = useState<number | null>(null);
  const [dirty, setDirty] = useState(opened.isDraft);
  const [saving, setSaving] = useState(false);
  const [status, setStatus] = useState('');
  const [savedVersion, setSavedVersion] = useState<number>();
  const [sent, setSent] = useState(false);
  const operation = useRef(false);
  const version = savedVersion ?? planning?.version ?? review?.version;
  const plan = planning ? documentInfo(planning.filename, body) : null;
  // A saved edit still needs review if sending fails, even when it removed the
  // final comment. Retrying must not turn that revision request into approval.
  const pendingReview =
    comments.length > 0 || dirty || savedVersion !== undefined || quote !== null;
  const actions: PlanAction[] = review
    ? pendingReview
      ? ['revise']
      : []
    : availableActions(plan, pendingReview);
  const action = actions[0];
  const hasReview = !!plan || !!review;
  // The column carries `--safe-bottom` itself rather than inheriting it from the
  // document root; see `useSafeBottomStyle`.
  const safeBottomStyle = useSafeBottomStyle();

  // The DURABLE half of that store answers later than the first frame — it is a
  // native bridge call — and only matters when the synchronous mirror was empty,
  // which is a webview data reset. It may land only on a screen nobody has
  // touched yet: the cleanup drops an answer that arrives after the reader
  // started working, and a leftover equal to the file is deleted rather than
  // shown, because a draft that says what the document already says is not work.
  const isUntouched = !dirty && comments === opened.comments;
  useEffect(() => {
    if (!draftKey || opened.isDraft || !isUntouched) return;
    let alive = true;
    void readAnnotationDraft(draftKey).then((kept) => {
      if (!alive || kept === null) return;
      if (sameComments(kept, opened.comments)) {
        clearAnnotationDraft(draftKey);
        return;
      }
      setComments(kept);
      setDirty(true);
    });
    return () => {
      alive = false;
    };
  }, [draftKey, opened, isUntouched]);

  // Unsaved work is written on the press that made it, never on a timer: the next
  // thing that happens to a backgrounded phone app may be that it is killed.
  useEffect(() => {
    if (draftKey && dirty) writeAnnotationDraft(draftKey, comments);
  }, [draftKey, dirty, comments]);

  // ON A PHONE A PASSAGE IS TAPPED, NOT DRAGGED — AND A SCROLL IS NOT A TAP.
  //
  // iOS answers a long press inside prose with its OWN callout (Copy / Look Up /
  // Share) and only settles the range after it: a `touchend` handler either sees
  // nothing or is buried under the native menu, which is why commenting was
  // unusable on an iPhone. So the block IS the unit of annotation on touch — one
  // tap on a paragraph, a heading or a list item quotes it — and text selection
  // is left switched on for a mouse only (`mouse:select-text`), where dragging a
  // range is natural and no callout exists.
  //
  // The gesture is read from the POINTER, not from `click`: a finger that moved
  // is a flick of the page and must leave the note alone, so a press that
  // travelled more than `TAP_SLOP` never quotes anything. (`onClick` plus
  // `onMouseUp` also ran the same pick twice for every mouse click.) Tapping the
  // quoted block again lets it go, so a mis-tap costs one tap, not a trip to
  // Cancel.
  const tapFrom = useRef<{ x: number; y: number } | null>(null);
  const beginTap = useCallback((event: React.PointerEvent) => {
    tapFrom.current = { x: event.clientX, y: event.clientY };
  }, []);
  const endTap = useCallback(() => {
    tapFrom.current = null;
  }, []);

  const pickSelection = useCallback((event: React.PointerEvent) => {
    const prose = proseRef.current;
    const from = tapFrom.current;
    tapFrom.current = null;
    if (!prose) return;
    // A real drag-selection inside the prose wins; otherwise the tapped block.
    const selection = window.getSelection?.();
    const node = selection?.anchorNode ?? null;
    const dragged = !!node && prose.contains(node) ? quoteOf(selection?.toString() ?? '') : '';
    if (dragged.length > 0) {
      setQuote(dragged);
      setStatus('');
      return;
    }
    if (!from) return;
    const travelled = Math.hypot(event.clientX - from.x, event.clientY - from.y);
    if (travelled > TAP_SLOP) return;
    const target = event.target as HTMLElement | null;
    const block = target?.closest?.(QUOTABLE_BLOCKS) as HTMLElement | null;
    const tapped = block && prose.contains(block) ? quoteOf(block.textContent ?? '') : '';
    if (tapped.length === 0) return;
    setQuote((current) => (current === tapped ? null : tapped));
    setStatus('');
  }, []);

  const commit = useCallback(() => {
    if (quote === null || draft.trim().length === 0) return;
    const remark = { quote, body: draft.trim() };
    setComments((old) =>
      editing === null
        ? [...old, remark]
        : old.map((entry, at) => (at === editing ? remark : entry)),
    );
    setQuote(null);
    setDraft('');
    setEditing(null);
    setDirty(true);
  }, [quote, draft, editing]);

  // A REMARK IS NOT WRITTEN ONCE AND FROZEN.
  //
  // Reported: the only thing a finished comment offered was the bin, so fixing a
  // word meant deleting the remark and writing it again from the passage it was
  // about. The card is therefore the way back INTO it — pressing it puts the same
  // passage and the same words in the composer, and the next commit REPLACES that
  // remark instead of appending a second one about the same line.
  const editComment = useCallback(
    (at: number) => {
      const target = comments[at];
      if (!target) return;
      setEditing(at);
      setQuote(target.quote);
      setDraft(target.body);
      setStatus('');
    },
    [comments],
  );

  const closeComposer = useCallback(() => {
    setQuote(null);
    setDraft('');
    setEditing(null);
  }, []);

  // Removing shifts every ordinal after it, so an open composer can no longer be
  // pointing at the remark it was opened on: it closes with the row.
  const removeComment = useCallback((at: number) => {
    setComments((old) => old.filter((_, index) => index !== at));
    setQuote(null);
    setDraft('');
    setEditing(null);
    setDirty(true);
  }, []);

  // Highlight passages with background only. Padding, inline ordinals and
  // decorations change wrapping or add marks the reader did not request.
  useEffect(() => {
    const prose = proseRef.current;
    if (!prose) return;
    const blocks = Array.from(prose.querySelectorAll<HTMLElement>(QUOTABLE_BLOCKS));
    const painted: HTMLElement[] = [];
    for (const block of blocks) {
      const text = quoteOf(block.textContent ?? '');
      const hits: number[] = [];
      comments.forEach((comment, at) => {
        if (text.length > 0 && comment.quote === text) hits.push(at);
      });
      if (hits.length === 0) continue;
      block.style.backgroundColor = annotationWash(hits[0]);
      if (review) block.style.color = 'var(--foreground)';
      painted.push(block);
    }
    // THE PICKED PASSAGE IS SHOWN AS PICKED — BY ITS PAPER, AND BY NOTHING ELSE.
    //
    // Between the tap and the "Add comment" the human had nothing to check
    // against: the composer quoted the text in a caption at the bottom of the
    // screen while the passage itself sat unmarked. The pending block wears the
    // accent — the same ink the app uses for focus — until the remark lands or is
    // dropped.
    //
    // Reported: the rail down its leading edge was noise, and so was that
    // caption. A WASH IS THE MARK — it covers the whole passage instead of
    // pointing at it from the margin — which is exactly why the composer below
    // says nothing about which passage this is: the reader is looking at it.
    if (quote) {
      for (const block of blocks) {
        if (painted.includes(block)) continue;
        if (quoteOf(block.textContent ?? '') !== quote) continue;
        block.style.backgroundColor = 'color-mix(in oklab, var(--accent) 24%, transparent)';
        if (review) block.style.color = 'var(--foreground)';
        block.dataset.quotePending = 'true';
        painted.push(block);
      }
    }
    return () => {
      for (const block of painted) {
        delete block.dataset.quotePending;
        block.style.backgroundColor = '';
        if (review) block.style.color = '';
      }
    };
  }, [comments, body, quote, review]);

  const perform = async (action?: PlanAction) => {
    if (operation.current || sent || quote !== null) return;
    if (action && ((!planning && !review) || !actions.includes(action))) return;
    operation.current = true;
    setSaving(true);
    let nextVersion = version;
    try {
      if (dirty) {
        setStatus('Saving changes…');
        const nextText = renderAnnotated(body, comments);
        nextVersion = await (initialComments ? onSave(nextText, comments) : onSave(nextText));
        if (hasReview && (!Number.isSafeInteger(nextVersion) || !nextVersion || nextVersion < 1)) {
          throw new Error('The saved version could not be confirmed. Nothing was sent.');
        }
        setSavedVersion(nextVersion);
        setDirty(false);
        if (draftKey) clearAnnotationDraft(draftKey);
      }
      if (action && (planning || review)) {
        if (!nextVersion) throw new Error('Open a saved version before sending a review action.');
        setStatus(`Sending ${PLAN_LABELS[action].toLowerCase()} for v${nextVersion}…`);
        if (review) await review.onSend(nextVersion);
        else await planning!.onSend(action, nextVersion);
        setSent(true);
        setStatus(
          action === 'approve'
            ? `Implementation requested for v${nextVersion}. Follow progress in the session.`
            : `Revision requested for v${nextVersion}. Reopen the next revision to continue.`,
        );
      } else {
        setStatus(nextVersion ? `Saved as v${nextVersion}` : 'Saved');
      }
    } catch (error) {
      setStatus(error instanceof Error ? error.message : 'Could not save or send this revision.');
    } finally {
      operation.current = false;
      setSaving(false);
    }
  };

  const column = (
    <div
      style={safeBottomStyle}
      className="flex h-full min-h-0 min-w-0 flex-1 flex-col overflow-hidden pb-[var(--safe-bottom,env(safe-area-inset-bottom))]"
    >
      {/* Prose scrolls independently; the composer, remarks and specification
          action stay below it, above the keyboard and safe area. */}
      <div
        ref={proseRef}
        inert={saving || sent}
        onPointerDown={beginTap}
        onPointerUp={pickSelection}
        onPointerCancel={endTap}
        onContextMenu={(event) => event.preventDefault()}
        className="min-h-0 min-w-0 flex-1 touch-manipulation overflow-y-auto overscroll-contain bg-panel px-3 py-3 font-sans text-body [-webkit-tap-highlight-color:transparent] [-webkit-touch-callout:none] text-foreground select-none sm:px-4 mouse:select-text"
      >
        {plain ? (
          <PlainText text={body} diff={!!review} />
        ) : (
          <Markdown onOpenAttachment={onOpenAttachment}>{body}</Markdown>
        )}
      </div>

      {quote !== null ? (
        <div className="flex shrink-0 flex-col gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4">
          {/* A PICKED PASSAGE NEEDS NO CAPTION. It is painted in the accent two
              lines above this field, so repeating it here spent a line of a
              keyboard-sized screen saying what the reader is already looking at.
              A remark about the WHOLE note has no passage to point at, and that
              one still says so. */}
          {quote.length === 0 ? (
            <p className="text-meta text-dialog-hint">
              Comment on the {GENERAL_LABEL.toLowerCase()}
            </p>
          ) : null}
          <textarea
            autoFocus
            value={draft}
            onChange={(event) => setDraft(event.target.value)}
            aria-label="Comment"
            placeholder="What about this passage?"
            className="min-h-24 w-full resize-y border border-dialog-edge bg-panel px-3 py-2 font-sans text-body text-foreground focus-visible:outline-2 focus-visible:outline-accent"
          />
          <div className="flex items-center gap-2 *:flex-1 sm:*:flex-none">
            <Button type="button" onClick={commit} disabled={!draft.trim()}>
              {editing === null ? 'Add comment' : 'Update comment'}
            </Button>
            <Button type="button" variant="quiet" onClick={closeComposer}>
              Cancel
            </Button>
          </div>
        </div>
      ) : null}

      {comments.length > 0 ? (
        <ul
          aria-label="Comments"
          inert={saving || sent}
          className="flex max-h-[35vh] shrink-0 flex-col gap-2 overflow-y-auto border-t border-dialog-edge px-3 py-3 sm:px-4"
        >
          {comments.map((comment, at) => (
            <li
              key={`${at}:${comment.quote}`}
              className="flex items-start gap-2 rounded-none border-l-2 border-dialog-edge py-1 pr-1 pl-2"
              style={{
                borderLeftColor: annotationColor(at),
                backgroundColor: annotationWash(at),
              }}
            >
              {/* THE CARD IS THE WAY BACK INTO THE REMARK. It presses like a list
                  row and wears no box of its own, so the card keeps exactly one
                  mark — the bin — and everything else on it opens the composer. */}
              <button
                type="button"
                onClick={() => editComment(at)}
                aria-label={`Edit comment ${at + 1}`}
                className="flex min-w-0 flex-1 items-start gap-2 text-left"
              >
                <sup
                  aria-hidden="true"
                  className="mt-1 shrink-0 font-mono text-chip font-bold"
                  style={{ color: annotationColor(at) }}
                >
                  {at + 1}
                </sup>
                <span className="min-w-0 flex-1">
                  <span
                    className={`block truncate text-meta ${review ? 'text-foreground' : 'text-dialog-hint'}`}
                  >
                    {comment.quote.length === 0 ? GENERAL_LABEL : `“${comment.quote}”`}
                  </span>
                  {/* A remark remains visually distinct from the document as italic
                      quoted prose, while preserving natural word spacing. */}
                  <span className={`block text-body italic text-foreground ${PROSE}`}>
                    {comment.body}
                  </span>
                </span>
              </button>
              <IconButton
                label={`Remove comment ${at + 1}`}
                variant="quiet"
                onClick={() => removeComment(at)}
              >
                <TrashIcon className="size-4" />
              </IconButton>
            </li>
          ))}
        </ul>
      ) : null}

      {hasReview ? (
        <section
          aria-label={review ? 'Diff review' : 'Specification workflow'}
          className="flex shrink-0 flex-wrap items-center justify-between gap-3 border-t border-dialog-edge bg-panel-2 px-3 py-3 font-mono text-ui sm:px-4"
        >
          <div className="min-w-0 flex-1 text-dialog-hint">
            <span>
              {sent
                ? action === 'approve'
                  ? 'Implementation requested'
                  : 'Revision requested'
                : comments.length > 0 || dirty || savedVersion !== undefined || quote !== null
                  ? 'In review'
                  : review
                    ? 'Review changes'
                    : SPEC_STATUSES[plan!.status]}{' '}
              · v{version}
            </span>
            <p role="status">
              {status ||
                (quote !== null
                  ? 'Finish or cancel your comment before continuing.'
                  : comments.length > 0
                    ? `${comments.length} unresolved comment${comments.length === 1 ? '' : 's'}`
                    : dirty || savedVersion !== undefined
                      ? 'Changes awaiting review'
                      : '')}
            </p>
          </div>
          {action && quote === null ? (
            <Button
              type="button"
              variant="primary"
              disabled={saving || sent}
              onClick={() => void perform(action)}
            >
              {PLAN_LABELS[action]}
            </Button>
          ) : null}
        </section>
      ) : (
        <span className="sr-only" role="status">
          {status}
        </span>
      )}
    </div>
  );

  return chrome({
    // Specifications submit the whole review round from their footer, not a
    // second save control in the header.
    actions: (
      <>
        <BandButton
          type="button"
          onClick={() => {
            setQuote('');
            setDraft('');
            setEditing(null);
            setStatus('');
          }}
          disabled={quote !== null || saving || sent}
          label={`Comment on the ${GENERAL_LABEL.toLowerCase()}`}
        >
          <CommentIcon />
        </BandButton>
        {!hasReview ? (
          <BandButton
            type="button"
            label={saving ? 'Saving changes' : 'Save changes'}
            isPrimary
            onClick={() => void perform()}
            disabled={!dirty || saving || sent || quote !== null}
          >
            {saving ? <Spinner /> : <CheckIcon />}
          </BandButton>
        ) : null}
      </>
    ),
    // What just happened to this document, said under its name — and until it
    // does, that something is waiting to.
    note: review
      ? `Diff · ${review.sourceLabel}`
      : plan
        ? plan.kind === 'plan'
          ? 'Specification'
          : 'Implementation record'
        : status || (dirty ? UNSAVED_NOTE : ''),
    body: column,
  });
});
