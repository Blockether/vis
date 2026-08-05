import { memo, useEffect, useState } from "react";
import { isMarkdownMedia } from "../lib/artifacts";
import { Markdown } from "./ChatContent";

/**
 * A WRITTEN artifact — a note, a comment, a log — read by the app itself.
 *
 * A PDF or an HTML page is untrusted markup and goes into a sandboxed frame
 * (`DocArtifact`); markdown is neither. Handed to an iframe, `# Heading` paints
 * as `# Heading`: the source of the artifact instead of the artifact, which is
 * exactly what "tapping the markdown does nothing useful" looked like on a
 * phone. So text is FETCHED and rendered — markdown through the transcript's own
 * `Markdown` (one renderer in the app, one set of type and colour decisions),
 * anything else verbatim in a monospaced block.
 *
 * There is no security cost to that: the bytes never become markup of the app's
 * own. `Markdown` disables raw HTML the same way it does for a model's answer,
 * and plain text is painted as text.
 */

/** A phone should never be asked to lay out a 40 MB log; read the head of it. */
export const TEXT_ARTIFACT_LIMIT = 256_000;

export function clampArtifactText(text: string): string {
  if (text.length <= TEXT_ARTIFACT_LIMIT) return text;
  return `${text.slice(0, TEXT_ARTIFACT_LIMIT)}\n\n… truncated`;
}

/** The artifact's own bytes, as text. `url` is an object/blob URL. */
export async function readArtifactText(url: string): Promise<string> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`artifact text ${response.status}`);
  return clampArtifactText(await response.text());
}

/**
 * The rendered document, given text that is already in hand — pure, so the
 * decision "prose or verbatim" is testable without a network or a DOM.
 */
export const TextBody = memo(function TextBody({
  text,
  mime,
  name,
}: {
  text: string;
  mime?: string;
  name?: string;
}) {
  if (isMarkdownMedia(mime, name)) {
    return (
      <article className="min-w-0 bg-panel px-3 py-3 font-sans text-body text-foreground sm:px-4">
        <Markdown>{text}</Markdown>
      </article>
    );
  }
  return (
    <pre className="min-w-0 overflow-x-auto bg-code px-3 py-3 font-mono text-meta whitespace-pre-wrap text-foreground sm:px-4">
      {text}
    </pre>
  );
});

/** The artifact, loaded then rendered. Mirrors `DocFrame`'s contract. */
export const TextFrame = memo(function TextFrame({
  url,
  mime,
  name,
}: {
  url: string;
  mime: string;
  name: string;
}) {
  const [text, setText] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);

  useEffect(() => {
    let alive = true;
    setText(null);
    setFailed(false);
    readArtifactText(url)
      .then((next) => {
        if (alive) setText(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
    };
  }, [url]);

  if (failed || text === null) {
    return (
      <p className="p-4 font-mono text-meta text-dialog-hint">
        {failed ? "This artifact could not be read." : "Loading…"}
      </p>
    );
  }
  return <TextBody text={text} mime={mime} name={name} />;
});
