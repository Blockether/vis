/**
 * THE ARTIFACTS SHEET — the session's own output, indexed.
 *
 * A session's most valuable product is scattered down the transcript in whatever
 * tool call happened to make it, so "show me that chart again" is a scroll hunt
 * through forty turns. The header's right slot answers that instead: one chip
 * that COUNTS what this session produced, and one surface that opens each one in
 * the viewer that can already zoom it, draw on it and attach the drawing to the
 * next message.
 *
 * Three rules the design has to keep, and each is falsifiable on screen:
 *
 *   * A session that produced NOTHING pays nothing. `ArtifactsChip` renders
 *     `null` at zero — no dead control, no gallery that opens onto "No artifacts
 *     yet".
 *   * A photo grid is not the only shape. Documents and recorded files have no
 *     picture and do not pretend to: a doc wears a page with its kind, a file
 *     wears its extension and is NOT a button, because a control that cannot do
 *     anything when you press it is a lie told with a focus ring.
 *   * Provenance is data. `PNG · 214KB · turn 6` is what makes an artifact
 *     citable, and it is the same sentence a screen reader gets, because a
 *     thumbnail has no text in it.
 *
 * Layering: `sm:` decides how many columns there is ROOM for; only `mouse:`
 * shrinks a hit box, so an iPad keeps 44px targets at desktop width.
 */

import { useEffect, useState, type ReactNode } from "react";
import {
  ARTIFACT_FILTERS,
  docKindLabel,
  isMarkdownMedia,
  isPdfMedia,
  isTextMedia,
  pageBySize,
  SHEET_PAGE,
  type SessionArtifact,
} from "../lib/artifacts";
import { useAttachImage } from "../lib/attach-image";
import { editedFilename } from "../lib/image-file";
import type { GatewayClient } from "../lib/gateway";
import { DocFrame } from "./DocArtifact";
import { ImageViewer } from "./ImageViewer";
import { MarkdownArtifact } from "./MarkdownArtifact";
import { PdfAnnotator } from "./PdfArtifact";
import { readArtifactText } from "./TextArtifact";
import { AlertIcon, ClipIcon, PlayIcon } from "./icons";
import {
  Chip,
  DialogHeader,
  KebabButton,
  ListRow,
  LoadMore,
} from "./ui";

/**
 * Two documents produced by the same turn have to stay distinguishable at a
 * glance, and neither owns a colour of its own — so one is picked from the
 * machine palette by NAME. Stable across renders because it is a pure function
 * of the artifact, not a counter.
 */
const ARTIFACT_HUES = [
  "bg-machine-violet",
  "bg-machine-teal",
  "bg-machine-orange",
  "bg-machine-aqua",
  "bg-machine-indigo",
  "bg-machine-rose",
  "bg-machine-azure",
  "bg-machine-brass",
  "bg-machine-coral",
  "bg-machine-olive",
];

export function artifactHue(key: string): string {
  let hash = 0;
  for (let at = 0; at < key.length; at += 1)
    hash = (hash * 31 + key.charCodeAt(at)) >>> 0;
  return ARTIFACT_HUES[hash % ARTIFACT_HUES.length];
}

/**
 * `v3 · PNG · 214KB · turn 6` — the line that makes an artifact citable.
 *
 * The version LEADS, because on a collapsed tile the first question is which cut
 * of the artifact is on screen. A single-cut artifact says nothing: `v1` on
 * everything would be noise on a session that never re-attached anything.
 */
function Meta({ artifact }: { artifact: SessionArtifact }) {
  return (
    <span className="block truncate font-mono text-chip text-dialog-hint">
      {[
        artifact.version > 1 ? `v${artifact.version}` : "",
        artifact.media,
        artifact.sizeLabel,
        `turn ${artifact.turn}`,
      ]
        .filter(Boolean)
        .join(" · ")}
    </span>
  );
}

/**
 * What a screen reader is told about a tile. A thumbnail is a picture with no
 * text in it, so the name, the kind, the size and the turn that produced it are
 * the only things that make the control announceable — and they are exactly the
 * provenance the sighted meta line already carries.
 */
export function describeArtifact(artifact: SessionArtifact): string {
  return [
    artifact.name,
    artifact.version > 1 ? `version ${artifact.version}` : "",
    artifact.media,
    artifact.sizeLabel,
    `produced in turn ${artifact.turn}`,
  ]
    .filter(Boolean)
    .join(", ");
}

/**
 * The bytes, on demand. Identical contract to the transcript's own tiles: the
 * client's object-URL cache is bounded and REVOKES what it evicts, so a URL is
 * only safe while the thing that painted it holds a retain — released on
 * unmount, which is what keeps the bound meaningful for everything off screen.
 */
function useArtifactUrl(
  client: GatewayClient,
  sid: string,
  artifact: SessionArtifact,
  enabled: boolean,
) {
  const [url, setUrl] = useState<string | null>(null);
  const [failed, setFailed] = useState(false);
  const { iterationId, index } = artifact;
  useEffect(() => {
    if (!enabled || !iterationId || !sid) return;
    let alive = true;
    const release = client.retainAttachment(sid, iterationId, index);
    client
      .attachmentUrl(sid, iterationId, index)
      .then((next) => {
        if (alive) setUrl(next);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
      release();
    };
  }, [client, sid, iterationId, index, enabled]);
  return { url, failed };
}

/**
 * A DOCUMENT SHOWS ITSELF TOO — AS A DOCUMENT, NEVER AS ITS SOURCE.
 *
 * A note's tile used to be five grey bars — a drawing of a page, identical on every
 * document in the session, so the only thing telling two notes apart was the filename
 * under them. A written artifact is text the app already reads itself, so the tile
 * reads the head of it and paints the document's own first lines.
 *
 * MARKDOWN IS PAINTED, NOT QUOTED. `## Plan`, `- [ ] ship`, `**bold**` and
 * `[a note](https://example.com)` spend a third of a 96px box on punctuation nobody
 * reads at 9px, so the peek came out as a smudge of hashes, asterisks and URLs. The
 * head is PARSED into what each line IS — a heading, a point in a list, a quotation,
 * a line of code — and the tile inks that with the app's own weight and one bullet
 * column, exactly as the reader one tap away does. Plain text has no markup to spend,
 * so a `.log` stays verbatim in mono.
 *
 * Only a WRITTEN note and only a small one: a PDF has no cheap raster, and a 40 MB log
 * is not worth a download for a 96px box — both keep the plate. The bytes come through
 * the same retained object URL the picture tiles use, and the sheet's own paging is
 * what bounds how many are fetched at once.
 */
const PREVIEW_LIMIT = 512_000;
// The budget is ARITHMETIC, not taste: `text-chip` is a 14px rhythm and the box is
// 96px (`h-24`) less its own 12px of padding, so seven lines are 98px — six land whole
// and the seventh is the one the fade dissolves, which is what says "there is more of
// this". Eight lines overflowed by a line and a half, and the fade ate the fifth.
const PREVIEW_LINES = 7;

/** One line of a peek: WHAT it is, so the tile can paint it the way the reader does. */
export type PreviewLine = {
  kind: "heading" | "bullet" | "quote" | "code" | "text";
  text: string;
  /** A list's own column: `•`, `1.`, or a task's state. */
  mark?: string;
};

/** The head of a file, verbatim, blank lines dropped. */
export function previewLines(text: string, limit = PREVIEW_LINES): string[] {
  const lines: string[] = [];
  for (const line of text.split("\n")) {
    const trimmed = line.trimEnd();
    if (trimmed.trim() === "") continue;
    lines.push(trimmed);
    if (lines.length === limit) break;
  }
  return lines;
}

/** Everything markdown spells with punctuation, spent: the WORDS are the peek. */
function inlineText(line: string): string {
  return line
    .replace(/!\[([^\]]*)\]\([^)]*\)/g, "$1")
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")
    .replace(/<[^>]*>/g, "")
    .replace(/`+/g, "")
    .replace(/\*\*|__|~~/g, "")
    .replace(/(^|[\s([])[*_]([^*_]+)[*_]/g, "$1$2")
    .replace(/\s+/g, " ")
    .trim();
}

/**
 * The head of a markdown note, read as the lines it is made of.
 *
 * This is NOT a second renderer — `ChatContent`'s `Markdown` is the app's one, and it
 * gets the whole document one tap away. Nothing renders in 96px; what a peek needs is
 * the single question "what is this line", answered cheaply, so the paint can give a
 * title its weight and a list its column. Front matter, rules, fences and a table's
 * hairline are the file's plumbing and are dropped — a tile has three lines to make
 * two notes distinguishable, and none of them is `---`.
 */
export function previewBlocks(text: string, limit = PREVIEW_LINES): PreviewLine[] {
  const out: PreviewLine[] = [];
  const lines = text.split("\n");
  let front = lines[0]?.trim() === "---";
  let fenced = false;
  for (let at = front ? 1 : 0; at < lines.length && out.length < limit; at += 1) {
    const line = lines[at].trim();
    if (front) {
      if (line === "---" || line === "...") front = false;
      continue;
    }
    if (/^(```|~~~)/.test(line)) {
      fenced = !fenced;
      continue;
    }
    if (fenced) {
      if (line) out.push({ kind: "code", text: line });
      continue;
    }
    if (!line) continue;
    if (/^([-*_=])\1{2,}$/.test(line)) continue;
    if (/^\|?[\s:|-]*\|[\s:|-]*$/.test(line) && line.includes("-")) continue;
    const heading = /^(#{1,6})\s+(.*)$/.exec(line);
    if (heading) {
      const words = inlineText(heading[2].replace(/\s+#+$/, ""));
      if (words) out.push({ kind: "heading", text: words });
      continue;
    }
    const quote = /^>\s?(.*)$/.exec(line);
    if (quote) {
      const words = inlineText(quote[1]);
      if (words) out.push({ kind: "quote", text: words });
      continue;
    }
    const bullet = /^([-*+]|\d+[.)])\s+(.*)$/.exec(line);
    if (bullet) {
      const task = /^\[([ xX])\]\s*(.*)$/.exec(bullet[2]);
      const words = inlineText(task ? task[2] : bullet[2]);
      let mark = "•";
      if (task) mark = task[1] === " " ? "○" : "✓";
      else if (/^\d/.test(bullet[1])) mark = bullet[1];
      if (words) out.push({ kind: "bullet", text: words, mark });
      continue;
    }
    if (line.startsWith("|")) {
      const cells = line
        .split("|")
        .map((cell) => inlineText(cell))
        .filter(Boolean);
      if (cells.length) out.push({ kind: "text", text: cells.join(" · ") });
      continue;
    }
    const words = inlineText(line);
    if (words) out.push({ kind: "text", text: words });
  }
  return out;
}

/** The note's text, once its bytes are in hand. A failure simply peeks at nothing. */
function useArtifactPreview(url: string | null, enabled: boolean): string {
  const [text, setText] = useState("");
  useEffect(() => {
    if (!enabled || !url) return;
    let alive = true;
    readArtifactText(url)
      .then((next) => {
        if (alive) setText(next);
      })
      .catch(() => {
        if (alive) setText("");
      });
    return () => {
      alive = false;
    };
  }, [url, enabled]);
  return text;
}

/** How each kind of line is inked: the reader's own emphasis, at a ninth of the size. */
const PEEK_TONE: Record<PreviewLine["kind"], string> = {
  heading: "font-bold text-white",
  bullet: "",
  quote: "border-l border-dialog-edge pl-1",
  code: "font-mono",
  text: "",
};

/**
 * THE HEAD OF A DOCUMENT, PAINTED AS A DOCUMENT.
 *
 * ONE RHYTHM: every line is `text-chip`'s 14px, so the count of them and the height of
 * the box are the same arithmetic and nothing is sliced through its letters.
 *
 * A list gets a COLUMN — the mark is its own cell and the words truncate beside it, so
 * three bullets read as three bullets instead of as ragged text with dashes in it. The
 * whole peek starts on the caption's leading edge (`px-2`), so the first letter of the
 * document sits exactly above the first letter of its name.
 *
 * The fade belongs to the BOX, not to this text: a gradient measured against the lines
 * reaches zero somewhere past the cut, so the bottom row is guillotined at whatever
 * opacity it happened to have (71% at 390px), and a note SHORTER than the box fades a
 * line that nothing was going to clip.
 */
function Peek({
  lines,
  plain,
  reserve,
}: {
  lines: PreviewLine[];
  plain: boolean;
  /** A control floats over the tile's top corner; the title stops before it. */
  reserve: boolean;
}) {
  return (
    <span
      aria-hidden="true"
      className={`block text-chip text-dialog-hint ${plain ? "font-mono" : ""}`}
    >
      {lines.map((line, at) => {
        // The `⋯` is 32px in the same corner the first line starts filling, and a
        // truncated title running UNDER a button reads as a rendering bug. Only the
        // line the control actually covers gives up the width.
        const clear = at === 0 && reserve ? "pr-9" : "";
        return line.mark ? (
          <span key={at} className={`flex gap-1 ${clear}`}>
            <span className="shrink-0">{line.mark}</span>
            <span className="min-w-0 flex-1 truncate">{line.text}</span>
          </span>
        ) : (
          <span
            key={at}
            className={`block truncate ${PEEK_TONE[line.kind]} ${clear}`}
          >
            {line.text}
          </span>
        );
      })}
    </span>
  );
}

/**
 * The thumbnail. A picture shows ITSELF — this is the one place in the app where
 * a produced figure is browsable without its turn around it, so a generic icon
 * would defeat the whole surface. A clip, a document and a file have no cheap
 * raster, and each says what it is instead of faking one.
 */
function Thumb({
  client,
  sid,
  artifact,
  hasHistory,
}: {
  client: GatewayClient;
  sid: string;
  artifact: SessionArtifact;
  hasHistory: boolean;
}) {
  const box = "h-24 sm:h-28 shrink-0 border-b border-dialog-edge";
  const previewable =
    artifact.kind === "doc" &&
    isTextMedia(artifact.mediaType, artifact.name) &&
    typeof artifact.size === "number" &&
    artifact.size <= PREVIEW_LIMIT;
  const { url, failed } = useArtifactUrl(
    client,
    sid,
    artifact,
    artifact.kind === "image" || previewable,
  );
  // Markdown is parsed into what its lines ARE; anything else is text and stays
  // verbatim — the same split the reader makes on the same two files.
  const head = useArtifactPreview(url, previewable);
  const plain = !isMarkdownMedia(artifact.mediaType, artifact.name);
  const preview: PreviewLine[] = plain
    ? previewLines(head).map((text): PreviewLine => ({ kind: "code", text }))
    : previewBlocks(head);

  if (artifact.kind === "image") {
    return (
      <span className={`block overflow-hidden bg-code ${box}`}>
        {url && !failed ? (
          <img
            src={url}
            alt=""
            loading="lazy"
            decoding="async"
            className="h-full w-full object-cover"
          />
        ) : (
          <span
            aria-hidden="true"
            className={`grid h-full w-full place-items-center text-dialog-hint ${
              failed ? "" : "animate-pulse motion-reduce:animate-none"
            }`}
          >
            {failed ? <AlertIcon className="size-5" /> : null}
          </span>
        )}
      </span>
    );
  }

  if (artifact.kind === "doc") {
    // The note itself, clipped by the box rather than summarised: a page continues
    // past the bottom of a thumbnail exactly as it does in the reader. It DISSOLVES
    // there rather than being guillotined — a row of letters sliced through the middle
    // reads as a broken box, while a fade reads as "there is more of this". The gradient
    // is measured against the BOX, so it reaches zero exactly ON the cut whatever the
    // note is: a page that overflows dissolves into the edge, and a four-line note fades
    // empty paper, which is no fade at all.
    //
    // A document that shows itself wears NO kind chip: the meta line under the tile
    // already says `MD · 1.9KB · turn 1`, and the chip was printed on top of the last
    // line the fade had left legible — a label over the thing it labels.
    if (preview.length) {
      return (
        <span
          className={`block overflow-hidden bg-panel-2 px-2 py-1.5 [mask-image:linear-gradient(to_bottom,black_80%,transparent)] ${box}`}
        >
          <Peek lines={preview} plain={plain} reserve={hasHistory} />
        </span>
      );
    }
    // Nothing to read yet, or nothing this app can read: the plate, hued by name. Its
    // ink starts on the caption's own leading edge, so a grid of tiles has ONE left
    // margin instead of one for pictures, one for plates and one for peeks.
    return (
      <span
        className={`relative flex flex-col justify-center gap-1 overflow-hidden bg-panel-2 px-2 ${box}`}
      >
        <span className={`h-1 w-2/3 ${artifactHue(artifact.key)}`} />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-4/5 bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-1/2 bg-dialog-hint/50" />
        <span className="absolute right-1 bottom-1 bg-ink/80 px-1 font-mono text-chip text-white">
          {docKindLabel(artifact.mediaType, artifact.name)}
        </span>
      </span>
    );
  }

  if (artifact.kind === "video") {
    return (
      <span
        className={`grid place-items-center bg-code text-dialog-hint ${box}`}
        aria-hidden="true"
      >
        <PlayIcon className="size-7" />
      </span>
    );
  }

  // A recorded file has no reader and no cheap raster, but it is still a FILE:
  // it wears the same plate a document does, greyed, with its own format word in
  // the corner. A `≡` in a beige box stood for every kind at once and therefore
  // said nothing — the extension is the only part a human actually reads.
  return (
    <span
      className={`relative flex flex-col justify-center gap-1 overflow-hidden bg-code px-2 ${box}`}
    >
      <span className="h-0.5 w-full bg-dialog-hint/25" />
      <span className="h-0.5 w-4/5 bg-dialog-hint/25" />
      <span className="h-0.5 w-full bg-dialog-hint/25" />
      <span className="h-0.5 w-1/2 bg-dialog-hint/25" />
      <span className="absolute right-1 bottom-1 bg-dialog-hint/60 px-1 font-mono text-chip text-white">
        {artifact.media || "FILE"}
      </span>
    </span>
  );
}

/**
 * A tile: picture on top, name and provenance under it.
 *
 * `w-full`, because a <button> sizes to fit its CONTENT and a truncated name
 * otherwise sticks out over the next cell. A recorded file renders the same
 * geometry as a plain element — the app has no reader for a `.log`, and an
 * openable-looking tile that does nothing is worse than an honest row.
 */
function Tile({
  client,
  sid,
  artifact,
  onOpen,
  onVersions,
}: {
  client: GatewayClient;
  sid: string;
  artifact: SessionArtifact;
  onOpen: (artifact: SessionArtifact) => void;
  onVersions: (artifact: SessionArtifact) => void;
}) {
  const shell =
    "flex min-h-11 w-full min-w-0 flex-col border border-dialog-edge bg-panel text-left";
  const versions = artifact.versions ?? [];
  const body = (
    <>
      <Thumb
        client={client}
        sid={sid}
        artifact={artifact}
        hasHistory={versions.length > 1}
      />
      <span className="min-w-0 px-2 py-1.5">
        <span className="block truncate font-mono text-meta font-bold text-white">
          {artifact.name}
        </span>
        <Meta artifact={artifact} />
      </span>
    </>
  );
  // THE DOT. Only an artifact that HAS a history offers to open one, and it is a
  // sibling of the tile rather than a child: a button inside a button is invalid
  // HTML, and the browser would hand the version menu's clicks to the tile.
  const dots = versions.length > 1 && (
    <KebabButton
      label={`Show ${versions.length} versions of ${artifact.name}`}
      variant="overlay"
      density="default"
      className="absolute top-1 right-1 size-8 mouse:size-7"
      onClick={() => onVersions(artifact)}
    />
  );

  if (artifact.kind === "file") {
    return (
      // No `aria-label`: a plain <div> has no role to carry one, and the name
      // and meta line inside it are already the whole announcement.
      <div className="relative min-w-0">
        <div className={shell}>{body}</div>
        {dots}
      </div>
    );
  }

  return (
    <div className="relative min-w-0">
      <button
        type="button"
        onClick={() => onOpen(artifact)}
        aria-label={`Open ${describeArtifact(artifact)}`}
        className={`${shell} transition-colors hover:bg-hover focus-visible:outline-2 focus-visible:outline-accent`}
      >
        {body}
      </button>
      {dots}
    </div>
  );
}

/**
 * The kind filter: a count per chip (a chip with nothing behind it is drawn
 * disabled rather than hidden — a strip that changes shape per session is a strip
 * you have to re-read every time).
 *
 * IT NO LONGER CARRIES THE WAY OUT. This strip is paper, so the one ✕ standing on
 * it inherited paper — the only way out in the app that read as a white button,
 * while every other opened surface leaves by a light mark on the dialog band. The
 * sheet now wears that band like the artifact it opens does, and the ✕ goes back to
 * inheriting the ink of the band it stands on rather than bringing paper of its own.
 */
function FilterStrip({
  list,
  active,
  onPick,
}: {
  list: SessionArtifact[];
  active: string;
  onPick: (label: string) => void;
}) {
  return (
    <div
      role="group"
      aria-label="Filter artifacts by kind"
      className="flex min-h-9 shrink-0 items-stretch border-b border-dialog-edge bg-panel px-3 mouse:min-h-8 sm:px-4"
    >
      <div className="flex min-w-0 flex-1 items-center gap-1.5 overflow-x-auto py-1.5">
        {ARTIFACT_FILTERS.map((filter) => {
          const count = list.filter((entry) =>
            filter.kinds.includes(entry.kind),
          ).length;
          const on = filter.label === active;
          return (
            <Chip
              key={filter.label}
              isOn={on}
              onClick={() => onPick(filter.label)}
              disabled={!count}
              aria-label={`${filter.label}, ${count} artifacts`}
            >
              <span aria-hidden="true">{filter.label}</span>
              <span
                aria-hidden="true"
                className={on ? "text-accent-foreground/70" : ""}
              >
                {count}
              </span>
            </Chip>
          );
        })}
      </div>
    </div>
  );
}

/**
 * The chrome a clip or a document is read inside. A picture brings its own.
 *
 * AN OPENED ARTIFACT IS THE WHOLE SCREEN. The overlay is the sheet's own box, so
 * the body under the header takes every remaining pixel and hands them to the
 * artifact: `fill` gives the child the box (it owns its own scrolling and its
 * own padding), while a list of rows keeps the padded scroller. A document that
 * sizes itself to `60vh` inside a padded scroller is a page floating in a
 * letterbox with the app's paper above and below it.
 *
 * The way out is the header band's own, and it NAMES the artifact (`Close chart.png`)
 * exactly as a document opened beside it does. That same mark used to be called
 * "Back to artifacts", so one sheet had two names for one gesture.
 */
function DetailOverlay({
  name,
  subtitle,
  actions,
  onClose,
  fill = false,
  children,
}: {
  name: string;
  /** What the band should REPORT about the artifact under its name. */
  subtitle?: ReactNode;
  /** The artifact's own verbs, as cells of this band before the ✕. */
  actions?: ReactNode;
  onClose: () => void;
  /** The child fills the body and scrolls itself. */
  fill?: boolean;
  children: ReactNode;
}) {
  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-label={name}
      className="absolute inset-0 z-40 flex flex-col bg-ink"
    >
      <DialogHeader
        isStacked
        title={name}
        subtitle={subtitle}
        actions={actions}
        closeLabel={`Close ${name}`}
        onClose={onClose}
      />
      <div
        className={
          fill
            ? "flex min-h-0 min-w-0 flex-1 flex-col"
            : "min-h-0 flex-1 overflow-y-auto p-3 sm:p-4"
        }
      >
        {children}
      </div>
    </div>
  );
}

/**
 * One opened artifact. The bytes are fetched HERE rather than by the tile, so
 * closing the detail releases them again and a session full of PDFs never holds
 * more than the one being read.
 */
function ArtifactDetail({
  client,
  sid,
  artifact,
  onClose,
}: {
  client: GatewayClient;
  sid: string;
  artifact: SessionArtifact;
  onClose: () => void;
}) {
  const attach = useAttachImage();
  const { url, failed } = useArtifactUrl(client, sid, artifact, true);

  if (failed || !url) {
    return (
      <DetailOverlay name={artifact.name} onClose={onClose}>
        <p className="p-4 font-mono text-meta text-dialog-hint">
          {failed ? "This artifact could not be loaded." : "Loading…"}
        </p>
      </DetailOverlay>
    );
  }

  // A picture is handed straight to the viewer that already owns zoom, pan, the
  // pen and the share sheet — the whole point of indexing artifacts is that
  // reaching one costs nothing extra.
  if (artifact.kind === "image") {
    return (
      <ImageViewer
        src={url}
        name={artifact.name}
        onClose={onClose}
        // Drawing on a picture is an ANSWER to it, so the ink is saved back
        // under the same filename — the next version of that artifact, the way
        // a commented note is — and the drawing is attached to the message too.
        // Its verb is the band's plain Save, like the note's: what saving MEANS
        // here belongs to this comment, not to a longer word on a title bar.
        onApply={async (edited: Blob) => {
          await client.saveArtifactBytes(
            sid,
            artifact.iterationId,
            artifact.name,
            artifact.mediaType || "image/png",
            new Uint8Array(await edited.arrayBuffer()),
          );
          if (attach) attach(edited, editedFilename(artifact.name));
        }}
      />
    );
  }

  if (artifact.kind === "video") {
    return (
      <DetailOverlay name={artifact.name} onClose={onClose} fill>
        <video
          src={url}
          controls
          playsInline
          preload="metadata"
          className="min-h-0 flex-1 bg-code object-contain"
        />
      </DetailOverlay>
    );
  }

  // A note and a log are the same document: markdown is RENDERED, plain text is
  // read verbatim line by line, and either one can be commented on — the whole
  // document saves back as the next version of the same filename.
  if (isTextMedia(artifact.mediaType, artifact.name)) {
    return (
      <MarkdownArtifact
        client={client}
        sid={sid}
        iterationId={artifact.iterationId}
        name={artifact.name}
        mediaType={artifact.mediaType}
        url={url}
        plain={!isMarkdownMedia(artifact.mediaType, artifact.name)}
        // The note's own verb stands in this overlay's band, one cell from the
        // ✕, and the band reports the version it saved as.
        chrome={({ actions, note, body }) => (
          <DetailOverlay
            name={artifact.name}
            subtitle={note}
            actions={actions}
            onClose={onClose}
            fill
          >
            {body}
          </DetailOverlay>
        )}
      />
    );
  }

  // A PDF is READ in its frame and DRAWN ON through the pen: the page is
  // rasterised, the ink stamped back onto it, and the stamped PDF saved under
  // the same filename — the next version, like an annotated note.
  const frame = (
    <DocFrame url={url} mime={artifact.mediaType} name={artifact.name} />
  );
  return (
    <DetailOverlay name={artifact.name} onClose={onClose} fill>
      {isPdfMedia(artifact.mediaType) ? (
        <PdfAnnotator
          client={client}
          sid={sid}
          iterationId={artifact.iterationId}
          name={artifact.name}
          mediaType={artifact.mediaType}
          url={url}
          frame={frame}
        />
      ) : (
        frame
      )}
    </DetailOverlay>
  );
}

/**
 * THE HISTORY BEHIND ONE NAME.
 *
 * The tile is always the LATEST cut, because that is the answer to "show me the
 * chart" ninety-nine times out of a hundred. This is the hundredth: the whole
 * thread, newest first, each row opening that exact cut in the same detail
 * overlay the tile opens. Nothing here re-downloads a thumbnail — a row is its
 * meta line, and the bytes are fetched only once a cut is actually opened.
 */
function ArtifactVersions({
  artifact,
  onOpen,
  onClose,
}: {
  artifact: SessionArtifact;
  onOpen: (artifact: SessionArtifact) => void;
  onClose: () => void;
}) {
  const versions = artifact.versions ?? [artifact];
  return (
    <DetailOverlay name={artifact.name} onClose={onClose}>
      <ul
        aria-label={`Versions of ${artifact.name}`}
        className="flex flex-col gap-2 p-3 sm:p-4"
      >
        {versions.map((version, position) => (
          <li key={version.key}>
            <ListRow
              isFramed
              onClick={() => onOpen(version)}
              aria-label={`Open ${describeArtifact(version)}`}
              className="gap-3"
            >
              <span className="font-mono text-meta font-bold text-white">
                v{version.version}
              </span>
              <span className="min-w-0 flex-1">
                <Meta artifact={version} />
              </span>
              {position === 0 && (
                <span className="font-mono text-chip text-accent">latest</span>
              )}
            </ListRow>
          </li>
        ))}
      </ul>
    </DetailOverlay>
  );
}

/**
 * THE REPURPOSED HEADER SLOT. It counts, so it says something true about THIS
 * session, and with nothing produced it renders nothing at all.
 *
 * On a phone the strip is too narrow for the word, so the visible chip is a
 * paperclip and a count — which is why the WORD lives in `aria-label`/`title`
 * instead of only in the pixels, and `aria-expanded` says whether the surface it
 * owns is open. Its BOX is the one the Share button used to occupy and the one
 * the session id beside it still occupies: `h-6`, the app's chip height, so the
 * header reads as a row of chips instead of one tower with text next to it.
 */
export function ArtifactsChip({
  count,
  open,
  controls = "artifacts-surface",
  onToggle,
}: {
  count: number;
  open: boolean;
  controls?: string;
  onToggle: () => void;
}) {
  if (!count) return null;
  const tone = open
    ? "border-accent bg-accent text-accent-foreground"
    : "border-dialog-title bg-dialog-title text-dialog-title-foreground hover:bg-accent-2";
  const label = `${count} artifacts produced by the model`;
  return (
    <button
      type="button"
      onClick={onToggle}
      aria-expanded={open}
      aria-controls={controls}
      aria-label={label}
      title={label}
      className={`inline-flex h-6 shrink-0 items-center gap-1 border px-2 font-mono text-chip font-bold tracking-[0.08em] uppercase focus-visible:outline-2 focus-visible:outline-accent ${tone}`}
    >
      <ClipIcon className="size-3" />
      <span aria-hidden="true" className="hidden sm:inline">
        Artifacts
      </span>
      <span aria-hidden="true">{count}</span>
    </button>
  );
}

/**
 * The surface itself, layered over the transcript it was produced by (the
 * session stays exactly where it was; this is a sheet, not a route). Escape
 * unwinds one layer at a time — an opened artifact first, then the sheet.
 */
export function ArtifactsSheet({
  client,
  sid,
  artifacts,
  onClose,
}: {
  client: GatewayClient;
  sid: string;
  artifacts: SessionArtifact[];
  onClose: () => void;
}) {
  const [filter, setFilter] = useState("All");
  const [opened, setOpened] = useState<SessionArtifact | null>(null);
  // The version list is its own surface, opened from the tile's dot and layered
  // UNDER the detail: opening a cut from it must return here, not to the grid.
  const [versionsOf, setVersionsOf] = useState<SessionArtifact | null>(null);
  // A thumbnail is a DOWNLOAD, so a session with two hundred artifacts in it is
  // two hundred requests the moment this sheet opens. One page at a time, by
  // count AND by weight, and a new filter starts its own first page.
  const [pages, setPages] = useState(1);
  const kinds =
    ARTIFACT_FILTERS.find((entry) => entry.label === filter)?.kinds ?? [];
  const shown = artifacts.filter((entry) => kinds.includes(entry.kind));
  const page = pageBySize(shown, (entry) => entry.size, pages, SHEET_PAGE);

  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== "Escape") return;
      event.stopPropagation();
      // Innermost surface first: the opened cut, then the version list it was
      // opened from, then the sheet itself.
      if (opened) {
        setOpened(null);
        return;
      }
      if (versionsOf) {
        setVersionsOf(null);
        return;
      }
      onClose();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose, opened, versionsOf]);

  return (
    <div
      id="artifacts-surface"
      role="region"
      aria-label="Artifacts produced by the model"
      className="absolute inset-0 z-30 flex flex-col bg-ink"
    >
      {/* The sheet is an opened surface, so it opens the way every other one does:
          the app's dialog band, naming itself, with the one ✕ inheriting its ink. */}
      <DialogHeader
        title="Artifacts"
        closeLabel="Close artifacts"
        onClose={onClose}
      />
      <FilterStrip
        list={artifacts}
        active={filter}
        onPick={(label) => {
          setFilter(label);
          setPages(1);
        }}
      />
      {/* The sheet is the whole box, composer included, so the bottom safe area is
          this scroller's own: the last row of tiles must clear the home indicator. */}
      <div className="min-h-0 flex-1 overflow-y-auto px-3 pt-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] sm:px-4 sm:pt-4">
        {shown.length ? (
          <>
            <div className="grid grid-cols-2 gap-2 sm:grid-cols-3 sm:gap-3 mouse:grid-cols-5">
              {page.shown.map((artifact) => (
                <Tile
                  key={artifact.key}
                  client={client}
                  sid={sid}
                  artifact={artifact}
                  onOpen={setOpened}
                  onVersions={setVersionsOf}
                />
              ))}
            </div>
            {page.rest.length > 0 && (
              <LoadMore
                label={`Load ${page.restLabel} of artifacts`}
                onClick={() => setPages((current) => current + 1)}
              >
                Load {page.restLabel}
              </LoadMore>
            )}
          </>
        ) : (
          <p className="font-mono text-meta text-dialog-hint">
            Nothing of that kind in this session.
          </p>
        )}
      </div>
      {versionsOf && (
        <ArtifactVersions
          artifact={versionsOf}
          onOpen={setOpened}
          onClose={() => setVersionsOf(null)}
        />
      )}
      {opened && (
        <ArtifactDetail
          client={client}
          sid={sid}
          artifact={opened}
          onClose={() => setOpened(null)}
        />
      )}
    </div>
  );
}
