import { memo, useCallback, useEffect, useRef, useState } from "react";

import type { GatewayClient } from "../lib/gateway";
import {
  parseAnnotated,
  quoteOf,
  renderAnnotated,
  type MarkdownComment,
} from "../lib/markdown-annotations";
import { Markdown } from "./ChatContent";
import { readArtifactText } from "./TextArtifact";
import { TrashIcon } from "./icons";
import { Button, IconButton } from "./ui";

/** The blocks a tap may quote: one paragraph, heading, item or cell. */
const QUOTABLE_BLOCKS = "p,li,h1,h2,h3,h4,h5,h6,blockquote,pre,td,th";

/**
 * ONE COLOUR PER COMMENT, AND THE SAME COLOUR IN BOTH PLACES.
 *
 * A remark is identified by its ORDINAL and by its hue: the passage it is about
 * is underlined in that hue and carries the number, and the card below wears the
 * very same chip. Six remarks on one note are then six threads a reader can
 * follow, instead of six identical grey boxes under a page of untouched prose.
 * The palette is spelled here because it is DATA the DOM is painted with, not a
 * class list: an underline colour has to be handed to `text-decoration-color`.
 */
export const ANNOTATION_COLORS = [
  "#b45309",
  "#1d4ed8",
  "#047857",
  "#be185d",
  "#6d28d9",
  "#0e7490",
];

export function annotationColor(index: number): string {
  return ANNOTATION_COLORS[index % ANNOTATION_COLORS.length];
}

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
}: {
  client: GatewayClient;
  sid: string;
  iterationId: string;
  name: string;
  mediaType: string;
  url: string;
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
        mediaType || "text/markdown",
        text,
      );
      return saved.version;
    },
    [client, sid, iterationId, name, mediaType],
  );

  if (failed || loaded === null) {
    return (
      <p className="p-4 font-mono text-meta text-dialog-hint">
        {failed ? "This artifact could not be read." : "Loading…"}
      </p>
    );
  }
  return <MarkdownAnnotator text={loaded} onSave={save} />;
});

/**
 * The rendered note plus its comments — pure apart from `onSave`, so the whole
 * select/comment/save loop is testable without a gateway.
 */
export const MarkdownAnnotator = memo(function MarkdownAnnotator({
  text,
  onSave,
}: {
  text: string;
  /** Persists the document and answers with the version it became. */
  onSave: (text: string) => Promise<number | undefined>;
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

  // ON A PHONE A PASSAGE IS TAPPED, NOT DRAGGED.
  //
  // iOS answers a long press inside prose with its OWN callout (Copy / Look Up /
  // Share) and only settles the range after it: a `touchend` handler either sees
  // nothing or is buried under the native menu, which is why commenting was
  // unusable on an iPhone. So the block IS the unit of annotation on touch — one
  // tap on a paragraph, a heading or a list item quotes it — and text selection
  // is left switched on for a mouse only (`mouse:select-text`), where dragging a
  // range is natural and no callout exists.
  const pickSelection = useCallback(
    (event: React.MouseEvent | React.PointerEvent) => {
      const prose = proseRef.current;
      if (!prose) return;
      // A real drag-selection inside the prose wins; otherwise the tapped block.
      const selection = window.getSelection?.();
      const node = selection?.anchorNode ?? null;
      const dragged =
        !!node && prose.contains(node)
          ? quoteOf(selection?.toString() ?? "")
          : "";
      const target = event.target as HTMLElement | null;
      const block = target?.closest?.(QUOTABLE_BLOCKS) as HTMLElement | null;
      const tapped =
        block && prose.contains(block) ? quoteOf(block.textContent ?? "") : "";
      const picked = dragged || tapped;
      if (picked.length === 0) return;
      setQuote(picked);
      setStatus("");
    },
    [],
  );

  const addComment = useCallback(() => {
    if (!quote || draft.trim().length === 0) return;
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
  // which line it is about — so the quoted block is UNDERLINED in that comment's
  // colour and carries its ordinal, painted straight onto the rendered markdown
  // (the prose comes from the shared `Markdown`, so there is no React node here
  // to decorate). The cleanup removes exactly what this pass added.
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
        if (comment.quote === text) hits.push(at);
      });
      if (hits.length === 0) continue;
      block.style.textDecorationLine = "underline";
      block.style.textDecorationColor = annotationColor(hits[0]);
      block.style.textDecorationThickness = "2px";
      block.style.textUnderlineOffset = "3px";
      painted.push(block);
      for (const at of hits) {
        const mark = document.createElement("sup");
        mark.dataset.commentOrdinal = String(at + 1);
        mark.textContent = String(at + 1);
        mark.style.color = annotationColor(at);
        mark.style.fontWeight = "700";
        mark.style.marginInlineStart = "0.25em";
        block.appendChild(mark);
        marks.push(mark);
      }
    }
    return () => {
      for (const mark of marks) mark.remove();
      for (const block of painted) {
        block.style.textDecorationLine = "";
        block.style.textDecorationColor = "";
        block.style.textDecorationThickness = "";
        block.style.textUnderlineOffset = "";
      }
    };
  }, [comments, body]);

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

  return (
    <div className="flex h-full min-h-0 min-w-0 flex-1 flex-col overflow-hidden">
      {/* The prose is the only part that grows: everything under it is pinned, so
          a long note scrolls inside its own box instead of pushing Save off the
          bottom of an iPhone. */}
      <div
        ref={proseRef}
        onClick={pickSelection}
        onMouseUp={pickSelection}
        className="min-h-0 min-w-0 flex-1 touch-manipulation overflow-y-auto bg-panel px-3 py-3 font-sans text-body [-webkit-touch-callout:none] text-foreground select-none sm:px-4 mouse:select-text"
      >
        <Markdown>{body}</Markdown>
      </div>

      {quote ? (
        <div className="flex shrink-0 flex-col gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4">
          <p className="text-meta text-dialog-hint">Comment on “{quote}”</p>
          <textarea
            autoFocus
            value={draft}
            onChange={(event) => setDraft(event.target.value)}
            aria-label="Comment"
            placeholder="What about this passage?"
            className="min-h-11 w-full resize-y border border-dialog-edge bg-panel px-3 py-2 font-sans text-body text-foreground focus-visible:outline-2 focus-visible:outline-accent"
          />
          <div className="flex items-center gap-2">
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
        <p className="shrink-0 border-t border-dialog-edge px-3 py-2 text-meta text-dialog-hint sm:px-4">
          Tap a passage to comment on it.
        </p>
      )}

      {comments.length > 0 ? (
        <ul
          aria-label="Comments"
          className="flex max-h-[35vh] shrink-0 flex-col gap-2 overflow-y-auto border-t border-dialog-edge px-3 py-3 sm:px-4"
        >
          {comments.map((comment, at) => (
            <li
              key={`${at}:${comment.quote}`}
              className="flex items-start gap-2 border-l-2 border-dialog-edge bg-panel py-1 pl-2"
              style={{ borderLeftColor: annotationColor(at) }}
            >
              <span
                aria-hidden="true"
                className="mt-0.5 grid size-5 shrink-0 place-items-center rounded-full font-mono text-chip text-white"
                style={{ backgroundColor: annotationColor(at) }}
              >
                {at + 1}
              </span>
              <div className="min-w-0 flex-1">
                <p className="truncate text-meta text-dialog-hint">
                  “{comment.quote}”
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

      <div className="flex shrink-0 items-center gap-3 border-t border-dialog-edge px-3 py-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:px-4">
        <Button type="button" onClick={save} disabled={!dirty || saving}>
          {saving ? "Saving…" : "Save"}
        </Button>
        {status ? (
          <span role="status" className="text-meta text-dialog-hint">
            {status}
          </span>
        ) : null}
      </div>
    </div>
  );
});
