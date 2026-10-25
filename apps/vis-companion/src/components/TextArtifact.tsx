import { memo, useEffect, useState } from 'react';
import { isMarkdownMedia, isSubtitleMedia } from '../lib/artifacts';
import { parseSubtitles, type SubtitleCue } from '../lib/subtitles';
import { Markdown } from './ChatContent';
import { clockTime } from './Media';
import { PROSE } from './ui';

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

/** The artifact's own downloaded bytes, or an object URL for legacy readers. */
export async function readArtifactText(source: Blob | string): Promise<string> {
  if (source instanceof Blob) return clampArtifactText(await source.text());
  const response = await fetch(source);
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
  raw = false,
}: {
  text: string;
  mime?: string;
  name?: string;
  /**
   * Show the SOURCE, not the document. A note sitting inline in the session is a
   * plan someone is still writing: rendering it there turns the turn that made it
   * into a wall of headings. Inline is the source; opening it is what renders.
   */
  raw?: boolean;
}) {
  const cues = !raw && isSubtitleMedia(mime, name) ? parseSubtitles(text) : [];
  if (cues.length > 0) return <SubtitleBody cues={cues} name={name} />;
  if (!raw && isMarkdownMedia(mime, name)) {
    return (
      <article className="min-w-0 bg-panel px-3 py-3 font-sans text-body text-foreground sm:px-4">
        <Markdown>{text}</Markdown>
      </article>
    );
  }
  return (
    <pre
      role="region"
      tabIndex={0}
      aria-label={name ?? 'Artifact text'}
      className="min-w-0 overflow-x-auto bg-code px-3 py-3 font-mono text-meta whitespace-pre-wrap text-foreground sm:px-4"
    >
      {text}
    </pre>
  );
});

/**
 * Subtitles as a transcript: the start time of each cue, then its words. A speaker
 * name shows once, at the first cue of each turn, so a dialogue reads as a script.
 */
export const SubtitleBody = memo(function SubtitleBody({
  cues,
  name,
}: {
  cues: SubtitleCue[];
  name?: string;
}) {
  return (
    <ol
      aria-label={name ?? 'Subtitles'}
      className="min-w-0 list-none bg-panel px-3 py-2 font-sans text-body text-foreground sm:px-4"
    >
      {cues.map((cue, index) => {
        const isTurn = Boolean(cue.speaker) && cue.speaker !== cues[index - 1]?.speaker;
        return (
          <li
            key={`${cue.start}-${index}`}
            className={`flex min-w-0 gap-3 ${isTurn && index > 0 ? 'mt-3' : 'mt-1'} first:mt-0`}
          >
            <span className="w-14 shrink-0 pt-0.5 font-mono text-chip tabular-nums text-dialog-hint">
              {clockTime(cue.start)}
            </span>
            <div className="min-w-0 flex-1">
              {isTurn ? (
                <span className="block font-mono text-chip font-bold uppercase tracking-wider text-accent-ink">
                  {cue.speaker}
                </span>
              ) : null}
              <p className={`min-w-0 whitespace-pre-wrap break-words ${PROSE}`}>{cue.text}</p>
            </div>
          </li>
        );
      })}
    </ol>
  );
});

/** The artifact, loaded then rendered. Mirrors `DocFrame`'s contract. */
export const TextFrame = memo(function TextFrame({
  url,
  mime,
  name,
  fill = false,
  raw = false,
}: {
  url: string;
  mime: string;
  name: string;
  /** Opened rather than previewed: fill the box and scroll inside it. */
  fill?: boolean;
  /** Paint markdown as its own source rather than as a rendered document. */
  raw?: boolean;
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
        {failed ? 'This artifact could not be read.' : 'Loading…'}
      </p>
    );
  }
  const body = <TextBody text={text} mime={mime} name={name} raw={raw} />;
  if (!fill) return body;
  return <div className="min-h-0 min-w-0 flex-1 overflow-y-auto p-3 sm:p-4">{body}</div>;
});
