import {
  memo,
  type ReactElement,
  type ReactNode,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";

import type { GatewayClient } from "../lib/gateway";
import {
  GENERAL_LABEL,
  parseAnnotated,
  quoteOf,
  renderAnnotated,
  type MarkdownComment,
} from "../lib/markdown-annotations";
import { Markdown } from "./ChatContent";
import { readArtifactText } from "./TextArtifact";
import { TrashIcon } from "./icons";
import { BandButton, Button, IconButton } from "./ui";
import { useSafeBottomStyle } from "../lib/viewport";

/** The blocks a tap may quote: one paragraph, heading, item or cell. */
const QUOTABLE_BLOCKS = "p,li,h1,h2,h3,h4,h5,h6,blockquote,pre,td,th";

/** A press that travelled further than this was a scroll, not a tap (CSS px). */
const TAP_SLOP = 10;

/**
 * ONE COLOUR PER COMMENT, AND THE SAME COLOUR IN BOTH PLACES.
 *
 * A remark is identified by its ORDINAL and by its hue: the passage it is about
 * is marked in that hue and carries the number, and the card below wears the
 * very same ordinal. Ten remarks on one note are then ten threads a reader can
 * follow, instead of ten identical grey boxes under a page of untouched prose.
 *
 * The palette is spelled as THEME VARIABLES, never as hard-coded hex: the app's
 * paper is whichever palette this device selected, so an ink chosen for a cream
 * light theme is unreadable the moment a dark one is picked. Every token below is
 * part of the shared Blockether palette (`index.css`) and is re-published by
 * every theme, so the marks move with it.
 */
export const ANNOTATION_COLORS = [
  "var(--warning)",
  "var(--link-fg)",
  "var(--ok)",
  "var(--code-syntax-special)",
  "var(--code-syntax-number)",
  "var(--code-syntax-string)",
  "var(--code-syntax-keyword)",
  "var(--warning-border)",
  "var(--code-syntax-comment)",
  "color-mix(in oklab, var(--code-syntax-special) 60%, var(--link-fg))",
];

export function annotationColor(index: number): string {
  return ANNOTATION_COLORS[index % ANNOTATION_COLORS.length];
}

/** The paper a marked passage sits on: the same hue, thinned to a wash. */
export function annotationWash(index: number): string {
  return `color-mix(in oklab, ${annotationColor(index)} 16%, transparent)`;
}

/**
 * THE DOCUMENT'S ONE VERB BELONGS TO THE BAND THAT NAMES IT.
 *
 * A note is always read inside somebody else's chrome — the artifacts sheet's
 * stacked overlay, the transcript's full-screen one — and that chrome already
 * carries the band with the document's name and the one way out of it. Save
 * stood in a docked footer under the comments instead: a 28px face asking for
 * 53px of an 844px phone, at the far end of the column from the ✕, to say what
 * the band says in a cell. The app settled that once already, when the model
 * picker's `Refresh` and `Manage providers` left their footer for the band.
 *
 * So the annotator hands its cell UP and the chrome decides which band it is:
 * `actions` is the `BandButton`, `note` is what the band should REPORT (the
 * version this document just became, or why it did not), and `body` is the
 * column that scrolls under it.
 */
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
  url,
  plain,
  chrome,
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  url: string;
  /** A `.txt`/`.log` note: the same annotator, reading the file verbatim. */
  plain?: boolean;
  /** The band and the frame this document is read inside. */
  chrome: DocumentChrome;
}) {
  const [loaded, setLoaded] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);

  useEffect(() => {
    let alive = true;
    setLoaded(null);
    setFailed(false);
    readArtifactText(url)
      .then((next) => {
        if (alive) setLoaded(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
    };
  }, [url]);

  const save = useCallback(
    async (text: string) => {
      const saved = await client.saveArtifactText(
        sid,
        iterationId,
        name,
        mediaType || (plain ? "text/plain" : "text/markdown"),
        text,
      );
      return saved.version;
    },
    [client, sid, iterationId, name, mediaType, plain],
  );

  if (failed || loaded === null) {
    return chrome({
      actions: null,
      note: "",
      body: (
        <p className="p-4 font-mono text-meta text-dialog-hint">
          {failed ? "This artifact could not be read." : "Loading…"}
        </p>
      ),
    });
  }
  return (
    <MarkdownAnnotator
      text={loaded}
      onSave={save}
      plain={plain}
      chrome={chrome}
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
const PlainText = memo(function PlainText({ text }: { text: string }) {
  return (
    <div className="font-mono text-body text-foreground">
      {text.split("\n").map((line, at) => (
        <p key={at} className="min-h-[18px] break-words whitespace-pre-wrap">
          {line}
        </p>
      ))}
    </div>
  );
});

/**
 * The rendered note plus its comments — pure apart from `onSave`, so the whole
 * select/comment/save loop is testable without a gateway.
 */
export const MarkdownAnnotator = memo(function MarkdownAnnotator({
  text,
  onSave,
  plain,
  chrome,
}: {
  text: string;
  /** Persists the document and answers with the version it became. */
  onSave: (text: string) => Promise<number | undefined>;
  /** Read the file verbatim, line by line, instead of rendering markdown. */
  plain?: boolean;
  /** The band and the frame this document is read inside. */
  chrome: DocumentChrome;
}) {
  const parsed = parseAnnotated(text);
  const proseRef = useRef<HTMLDivElement | null>(null);
  const [body] = useState(parsed.body);
  const [comments, setComments] = useState<MarkdownComment[]>(parsed.comments);
  const [quote, setQuote] = useState<string | null>(null);
  const [draft, setDraft] = useState("");
  const [dirty, setDirty] = useState(false);
  const [saving, setSaving] = useState(false);
  const [status, setStatus] = useState("");
  // The column carries `--safe-bottom` itself rather than inheriting it from the
  // document root; see `useSafeBottomStyle`.
  const safeBottomStyle = useSafeBottomStyle();

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
    const dragged =
      !!node && prose.contains(node) ? quoteOf(selection?.toString() ?? "") : "";
    if (dragged.length > 0) {
      setQuote(dragged);
      setStatus("");
      return;
    }
    if (!from) return;
    const travelled = Math.hypot(event.clientX - from.x, event.clientY - from.y);
    if (travelled > TAP_SLOP) return;
    const target = event.target as HTMLElement | null;
    const block = target?.closest?.(QUOTABLE_BLOCKS) as HTMLElement | null;
    const tapped =
      block && prose.contains(block) ? quoteOf(block.textContent ?? "") : "";
    if (tapped.length === 0) return;
    setQuote((current) => (current === tapped ? null : tapped));
    setStatus("");
  }, []);

  const addComment = useCallback(() => {
    if (quote === null || draft.trim().length === 0) return;
    setComments((old) => [...old, { quote, body: draft.trim() }]);
    setQuote(null);
    setDraft("");
    setDirty(true);
  }, [quote, draft]);

  const removeComment = useCallback((at: number) => {
    setComments((old) => old.filter((_, index) => index !== at));
    setDirty(true);
  }, []);

  // THE PASSAGE WEARS ITS OWN COMMENT.
  //
  // A remark that only exists in a list at the bottom leaves the reader guessing
  // which line it is about — so the quoted block is MARKED in that comment's
  // colour: a thin wash of the same hue behind it, a rule under it, and the
  // ordinal at its end. The colours are theme variables, so the mark is as
  // legible on the gateway's dark paper as on its light one. It is painted
  // straight onto the rendered markdown (the prose comes from the shared
  // `Markdown`, so there is no React node here to decorate), and the cleanup
  // removes exactly what this pass added.
  useEffect(() => {
    const prose = proseRef.current;
    if (!prose) return;
    const blocks = Array.from(
      prose.querySelectorAll<HTMLElement>(QUOTABLE_BLOCKS),
    );
    const marks: HTMLElement[] = [];
    const painted: HTMLElement[] = [];
    for (const block of blocks) {
      const text = quoteOf(block.textContent ?? "");
      const hits: number[] = [];
      comments.forEach((comment, at) => {
        if (text.length > 0 && comment.quote === text) hits.push(at);
      });
      if (hits.length === 0) continue;
      block.style.textDecorationLine = "underline";
      block.style.textDecorationColor = annotationColor(hits[0]);
      block.style.textDecorationThickness = "1px";
      block.style.textUnderlineOffset = "4px";
      block.style.backgroundColor = annotationWash(hits[0]);
      block.style.boxShadow = `inset 2px 0 0 0 ${annotationColor(hits[0])}`;
      block.style.borderRadius = "2px";
      block.style.paddingInline = "0.375rem";
      painted.push(block);
      for (const at of hits) {
        const mark = document.createElement("sup");
        mark.dataset.commentOrdinal = String(at + 1);
        mark.textContent = String(at + 1);
        mark.style.color = annotationColor(at);
        mark.style.fontWeight = "700";
        mark.style.marginInlineStart = "0.25em";
        mark.style.textDecoration = "none";
        block.appendChild(mark);
        marks.push(mark);
      }
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
        if (quoteOf(block.textContent ?? "") !== quote) continue;
        block.style.backgroundColor =
          "color-mix(in oklab, var(--accent) 24%, transparent)";
        block.style.borderRadius = "2px";
        block.style.paddingInline = "0.375rem";
        block.dataset.quotePending = "true";
        painted.push(block);
      }
    }
    return () => {
      for (const mark of marks) mark.remove();
      for (const block of painted) {
        delete block.dataset.quotePending;
        block.style.textDecorationLine = "";
        block.style.textDecorationColor = "";
        block.style.textDecorationThickness = "";
        block.style.textUnderlineOffset = "";
        block.style.backgroundColor = "";
        block.style.boxShadow = "";
        block.style.borderRadius = "";
        block.style.paddingInline = "";
      }
    };
  }, [comments, body, quote]);

  const save = useCallback(() => {
    setSaving(true);
    setStatus("");
    onSave(renderAnnotated(body, comments))
      .then((version) => {
        setDirty(false);
        setStatus(version ? `Saved as v${version}` : "Saved");
      })
      .catch(() => setStatus("Could not save this revision."))
      .finally(() => setSaving(false));
  }, [onSave, body, comments]);

  const column = (
    <div
      style={safeBottomStyle}
      className="flex h-full min-h-0 min-w-0 flex-1 flex-col overflow-hidden pb-[var(--safe-bottom,env(safe-area-inset-bottom))]"
    >
      {/* The prose is the only part that grows: everything under it is pinned, so
          a long note scrolls inside its own box. The column ends at the home
          indicator on its own now that no verb is docked under it — and at the
          KEYBOARD when one is up: `--safe-bottom` is `0px` while the keyboard
          covers the home indicator, so the composer sits on the keys instead of
          reserving a dead band above them (`useSafeBottomStyle`). */}
      <div
        ref={proseRef}
        onPointerDown={beginTap}
        onPointerUp={pickSelection}
        onPointerCancel={endTap}
        onContextMenu={(event) => event.preventDefault()}
        className="min-h-0 min-w-0 flex-1 touch-manipulation overflow-y-auto overscroll-contain bg-panel px-3 py-3 font-sans text-body [-webkit-tap-highlight-color:transparent] [-webkit-touch-callout:none] text-foreground select-none sm:px-4 mouse:select-text"
      >
        {plain ? <PlainText text={body} /> : <Markdown>{body}</Markdown>}
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
            <Button type="button" onClick={addComment} disabled={!draft.trim()}>
              Add comment
            </Button>
            <Button
              type="button"
              variant="quiet"
              onClick={() => {
                setQuote(null);
                setDraft("");
              }}
            >
              Cancel
            </Button>
          </div>
        </div>
      ) : (
        // A REMARK NEED NOT BE ABOUT A SENTENCE.
        //
        // "This plan is stale" is about the note, not about a line in it, and a
        // reader with nothing to point at had no way to say it. The whole-note
        // comment sits beside the invitation and opens the very same composer
        // with an empty quote.
        <div className="flex shrink-0 items-center justify-between gap-2 border-t border-dialog-edge px-3 py-2 sm:px-4">
          <p className="min-w-0 flex-1 text-meta text-dialog-hint">
            Tap a passage to comment on it.
          </p>
          <Button
            type="button"
            variant="quiet"
            density="compact"
            onClick={() => {
              setQuote("");
              setDraft("");
              setStatus("");
            }}
          >
            Comment on the note
          </Button>
        </div>
      )}

      {comments.length > 0 ? (
        <ul
          aria-label="Comments"
          className="flex max-h-[35vh] shrink-0 flex-col gap-2 overflow-y-auto border-t border-dialog-edge px-3 py-3 sm:px-4"
        >
          {comments.map((comment, at) => (
            <li
              key={`${at}:${comment.quote}`}
              className="flex items-start gap-2 rounded-[2px] border-l-2 border-dialog-edge py-1 pr-1 pl-2"
              style={{
                borderLeftColor: annotationColor(at),
                backgroundColor: annotationWash(at),
              }}
            >
              <sup
                aria-hidden="true"
                className="mt-1 shrink-0 font-mono text-chip font-bold"
                style={{ color: annotationColor(at) }}
              >
                {at + 1}
              </sup>
              <div className="min-w-0 flex-1">
                <p className="truncate text-meta text-dialog-hint">
                  {comment.quote.length === 0
                    ? GENERAL_LABEL
                    : `“${comment.quote}”`}
                </p>
                <p className="text-body text-foreground">{comment.body}</p>
              </div>
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

      {/* A live region is only announced when it was already standing, so the
          outcome the band shows has a permanent, silent twin here. */}
      <span className="sr-only" role="status">
        {status}
      </span>
    </div>
  );

  return chrome({
    // The document's one verb, in the band that names it: the band's own height,
    // welded by its hairline, one cell from the way out.
    actions: (
      <BandButton type="button" onClick={save} disabled={!dirty || saving}>
        {saving ? "Saving…" : "Save"}
      </BandButton>
    ),
    // What just happened to this document, said under its name.
    note: status,
    body: column,
  });
});
