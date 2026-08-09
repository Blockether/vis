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
  DialogClose,
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
    artifact.tool
      ? `produced in turn ${artifact.turn} by ${artifact.tool}`
      : `produced in turn ${artifact.turn}`,
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
 * A DOCUMENT SHOWS ITSELF TOO.
 *
 * A note's tile used to be five grey bars — a drawing of a page, identical on every
 * document in the session, so the only thing telling two notes apart was the filename
 * under them. A written artifact is text the app already reads itself, so the tile
 * reads the head of it and paints the document's own first lines.
 *
 * Only a WRITTEN note and only a small one: a PDF has no cheap raster, and a 40 MB log
 * is not worth a download for a 96px box — both keep the plate. The bytes come through
 * the same retained object URL the picture tiles use, and the sheet's own paging is
 * what bounds how many are fetched at once.
 */
const PREVIEW_LIMIT = 512_000;
const PREVIEW_LINES = 8;

/** The head of a note, blank lines dropped: eight tiny lines is the whole budget. */
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
}: {
  client: GatewayClient;
  sid: string;
  artifact: SessionArtifact;
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
  const preview = previewLines(useArtifactPreview(url, previewable));

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
    const kind = (
      <span className="absolute right-1 bottom-1 bg-ink/80 px-1 font-mono text-chip text-white">
        {docKindLabel(artifact.mediaType, artifact.name)}
      </span>
    );
    // The note itself, clipped by the box rather than summarised: a page continues
    // past the bottom of a thumbnail exactly as it does in the reader. It DISSOLVES
    // there rather than being guillotined — a row of letters sliced through the middle
    // reads as a broken box, while a fade reads as "there is more of this".
    if (preview.length) {
      return (
        <span className={`relative block overflow-hidden bg-panel-2 px-2 py-1 ${box}`}>
          <span
            aria-hidden="true"
            className="block font-mono text-chip text-dialog-hint [mask-image:linear-gradient(to_bottom,black_60%,transparent)]"
          >
            {preview.map((line, at) => (
              <span key={at} className="block truncate">
                {line}
              </span>
            ))}
          </span>
          {kind}
        </span>
      );
    }
    // Nothing to read yet, or nothing this app can read: the plate, hued by name.
    return (
      <span
        className={`relative flex flex-col justify-center gap-1 overflow-hidden bg-panel-2 px-3 ${box}`}
      >
        <span className={`h-1 w-2/3 ${artifactHue(artifact.key)}`} />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-4/5 bg-dialog-hint/50" />
        <span className="h-0.5 w-full bg-dialog-hint/50" />
        <span className="h-0.5 w-1/2 bg-dialog-hint/50" />
        {kind}
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
      className={`relative flex flex-col justify-center gap-1 overflow-hidden bg-code px-3 ${box}`}
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
      <Thumb client={client} sid={sid} artifact={artifact} />
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
 * The kind filter, doubling as the sheet's whole top band: a count per chip
 * (a chip with nothing behind it is drawn disabled rather than hidden — a
 * strip that changes shape per session is a strip you have to re-read every
 * time), and the way out welded to its right edge instead of a separate title
 * band above it — one row, not two.
 */
function FilterStrip({
  list,
  active,
  onPick,
  onClose,
}: {
  list: SessionArtifact[];
  active: string;
  onPick: (label: string) => void;
  onClose: () => void;
}) {
  return (
    <div
      role="group"
      aria-label="Filter artifacts by kind"
      className="flex min-h-9 shrink-0 items-stretch border-b border-dialog-edge bg-panel pl-3 mouse:min-h-8 sm:pl-4"
    >
      <div className="flex min-w-0 flex-1 items-center gap-1.5 overflow-x-auto py-1.5 pr-1.5">
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
      {/* The way out is the app's way out: welded to the band's right edge behind
          its own hairline, exactly as a dialog and an opened artifact are left.
          It used to be one more bordered chip in a strip of bordered chips. */}
      <DialogClose label="Close artifacts" tone="block" onClose={onClose} />
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
  onClose,
  fill = false,
  children,
}: {
  name: string;
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
        applyLabel="Save as new version"
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
      <DetailOverlay name={artifact.name} onClose={onClose} fill>
        <MarkdownArtifact
          client={client}
          sid={sid}
          iterationId={artifact.iterationId}
          name={artifact.name}
          mediaType={artifact.mediaType}
          url={url}
          plain={!isMarkdownMedia(artifact.mediaType, artifact.name)}
        />
      </DetailOverlay>
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
      <FilterStrip
        list={artifacts}
        active={filter}
        onClose={onClose}
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
