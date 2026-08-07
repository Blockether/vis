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
import { Button } from "./ui";

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

  // A selection is only ours when it lives inside the prose: the comments list
  // and the composer are on the same screen, and quoting a comment with itself
  // is noise rather than an annotation.
  const pickSelection = useCallback(() => {
    const selection = window.getSelection?.();
    const node = selection?.anchorNode ?? null;
    const inside = !!node && !!proseRef.current?.contains(node);
    const picked = quoteOf(selection?.toString() ?? "");
    if (!inside || picked.length === 0) return;
    setQuote(picked);
    setStatus("");
  }, []);

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
    <div className="flex min-h-0 min-w-0 flex-1 flex-col">
      <div
        ref={proseRef}
        onMouseUp={pickSelection}
        onTouchEnd={pickSelection}
        className="min-h-0 min-w-0 flex-1 overflow-y-auto bg-panel px-3 py-3 font-sans text-body text-foreground sm:px-4"
      >
        <Markdown>{body}</Markdown>
      </div>

      {quote ? (
        <div className="flex flex-col gap-2 border-t border-dialog-edge bg-panel-2 px-3 py-3 sm:px-4">
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
        <p className="border-t border-dialog-edge px-3 py-2 text-meta text-dialog-hint sm:px-4">
          Select a passage to comment on it.
        </p>
      )}

      {comments.length > 0 ? (
        <ul
          aria-label="Comments"
          className="flex flex-col gap-2 border-t border-dialog-edge px-3 py-3 sm:px-4"
        >
          {comments.map((comment, at) => (
            <li
              key={`${at}:${comment.quote}`}
              className="flex items-start gap-3 border border-dialog-edge bg-panel px-3 py-2"
            >
              <div className="min-w-0 flex-1">
                <p className="text-meta text-dialog-hint">“{comment.quote}”</p>
                <p className="text-body text-foreground">{comment.body}</p>
              </div>
              <Button
                type="button"
                variant="quiet"
                aria-label={`Remove comment on ${comment.quote}`}
                onClick={() => removeComment(at)}
              >
                Remove
              </Button>
            </li>
          ))}
        </ul>
      ) : null}

      <div className="flex items-center gap-3 border-t border-dialog-edge px-3 py-3 sm:px-4">
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
