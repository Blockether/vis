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
  artifactTotalLabel,
  docKindLabel,
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
import { TextFrame } from "./TextArtifact";

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
 * The ONE glyph left. A clip has no cheap raster and `▶` is the mark that reads
 * as "play" in every locale; every other kind now says what it actually is — a
 * picture shows itself, a document and a file wear their own format word.
 */
const VIDEO_GLYPH = "▶";

/**
 * The mark of an ATTACHMENT, drawn in the app's own icon grammar (24-grid,
 * `currentColor`, 1.8 stroke) like the camera, the picture and the microphone in
 * the composer. `▣` was a geometric box that stood for "some object" and at
 * `text-chip` read as a smudge; a paperclip is the mark every mail client, chat
 * app and issue tracker already taught this user.
 */
function ClipIcon() {
  return (
    <svg
      viewBox="0 0 24 24"
      className="size-3.5 shrink-0"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.8"
      aria-hidden="true"
    >
      {/* The DIAGONAL clip, because at 14px an upright one closes up into a
          blob: the slant keeps both openings wider than the stroke. */}
      <path
        d="M20 11.5l-8.2 8.2a5 5 0 0 1-7.1-7.1l8.6-8.6a3.3 3.3 0 0 1 4.7 4.7l-8.6 8.6a1.7 1.7 0 0 1-2.4-2.4l7.9-7.9"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </svg>
  );
}

/** `PNG · 214KB · turn 6` — the line that makes an artifact citable. */
function Meta({ artifact }: { artifact: SessionArtifact }) {
  return (
    <span className="block truncate font-mono text-chip text-dialog-hint">
      {[artifact.media, artifact.sizeLabel, `turn ${artifact.turn}`]
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
  const { url, failed } = useArtifactUrl(
    client,
    sid,
    artifact,
    artifact.kind === "image",
  );

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
            className={`grid h-full w-full place-items-center font-mono text-subhead text-dialog-hint ${
              failed ? "" : "animate-pulse motion-reduce:animate-none"
            }`}
          >
            {failed ? "✗" : ""}
          </span>
        )}
      </span>
    );
  }

  if (artifact.kind === "doc") {
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
        <span className="absolute right-1 bottom-1 bg-ink/80 px-1 font-mono text-chip text-white">
          {docKindLabel(artifact.mediaType, artifact.name)}
        </span>
      </span>
    );
  }

  if (artifact.kind === "video") {
    return (
      <span
        className={`grid place-items-center bg-code font-mono text-subhead text-dialog-hint ${box}`}
        aria-hidden="true"
      >
        {VIDEO_GLYPH}
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
}: {
  client: GatewayClient;
  sid: string;
  artifact: SessionArtifact;
  onOpen: (artifact: SessionArtifact) => void;
}) {
  const shell =
    "flex min-h-11 w-full min-w-0 flex-col border border-dialog-edge bg-panel text-left";
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

  if (artifact.kind === "file") {
    return (
      // No `aria-label`: a plain <div> has no role to carry one, and the name
      // and meta line inside it are already the whole announcement.
      <div className={shell}>{body}</div>
    );
  }

  return (
    <button
      type="button"
      onClick={() => onOpen(artifact)}
      aria-label={`Open ${describeArtifact(artifact)}`}
      className={`${shell} transition-colors hover:bg-hover focus-visible:outline-2 focus-visible:outline-accent`}
    >
      {body}
    </button>
  );
}

/** The sheet's own header: what this is, how much of it there is, and out. */
function SurfaceHeader({
  list,
  onClose,
}: {
  list: SessionArtifact[];
  onClose: () => void;
}) {
  const total = artifactTotalLabel(list);
  return (
    // The app has ONE dialog band (`ui.tsx` `DialogFrame`): dark `dialog-title`
    // paper, the name centred in it, and the way out welded to the right edge
    // behind a rule. This surface used to invent its own — pale panel, title on
    // the left, a boxed ✕ floating in the padding — so the one screen a session's
    // output lives on did not look like the app it lives in.
    <header className="relative flex min-h-9 shrink-0 items-center justify-center bg-dialog-title px-12 py-1.5 text-dialog-title-foreground mouse:min-h-8">
      <h2 className="truncate text-center font-mono text-body font-bold tracking-wide">
        Artifacts
        <span className="font-normal text-dialog-title-foreground/60">
          {` · ${list.length}${total ? ` · ${total}` : ""}`}
        </span>
      </h2>
      {/* The band and its close are the DialogFrame's own metrics, not a taller
          copy of them: a 44px block welded across a 36px band is what made this
          header read as a toolbar bolted onto the sheet. */}
      <button
        type="button"
        onClick={onClose}
        aria-label="Close artifacts"
        className="absolute inset-y-0 right-0 grid min-w-9 place-items-center border-l border-dialog-title-foreground/20 font-mono text-title text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none mouse:min-w-8"
      >
        <span aria-hidden="true">✕</span>
      </button>
    </header>
  );
}

/**
 * The kind filter. A count per chip, and a chip with nothing behind it is drawn
 * disabled rather than hidden: a strip that changes shape per session is a strip
 * you have to re-read every time. `aria-pressed` is what makes "which filter am
 * I looking at" answerable without the accent colour.
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
      className="flex shrink-0 items-center gap-1.5 overflow-x-auto border-b border-dialog-edge bg-panel px-3 py-1.5 sm:px-4"
    >
      {ARTIFACT_FILTERS.map((filter) => {
        const count = list.filter((entry) =>
          filter.kinds.includes(entry.kind),
        ).length;
        const on = filter.label === active;
        return (
          <button
            key={filter.label}
            type="button"
            onClick={() => onPick(filter.label)}
            disabled={!count}
            aria-pressed={on}
            aria-label={`${filter.label}, ${count} artifacts`}
            className={`inline-flex min-h-7 shrink-0 items-center gap-1.5 border px-2 font-mono text-meta focus-visible:outline-2 focus-visible:outline-accent mouse:min-h-6 ${
              on
                ? "border-accent bg-accent font-bold text-accent-foreground"
                : count
                  ? "border-edge text-dialog-hint hover:bg-hover"
                  : "border-edge text-dialog-hint opacity-40"
            }`}
          >
            <span aria-hidden="true">{filter.label}</span>
            <span
              aria-hidden="true"
              className={on ? "text-accent-foreground/70" : ""}
            >
              {count}
            </span>
          </button>
        );
      })}
    </div>
  );
}

/** The one line that says what a tap buys: zoom, draw, send it back. */
function SurfaceFooter() {
  return (
    <footer className="shrink-0 border-t border-dialog-edge bg-panel-2 px-3 py-1.5 font-mono text-chip text-dialog-hint sm:px-4">
      Tap to open · pinch to zoom · draw on it and{" "}
      <span className="font-bold text-white">Attach to message</span> sends the
      picture back to the model
    </footer>
  );
}

/** The chrome a clip or a document is read inside. A picture brings its own. */
function DetailOverlay({
  name,
  onClose,
  children,
}: {
  name: string;
  onClose: () => void;
  children: ReactNode;
}) {
  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-label={name}
      className="absolute inset-0 z-20 flex flex-col bg-ink"
    >
      <header className="relative flex min-h-9 shrink-0 items-center justify-center bg-dialog-title px-12 py-1.5 text-dialog-title-foreground mouse:min-h-8">
        <h2
          className="min-w-0 truncate text-center font-mono text-body font-bold tracking-wide"
          title={name}
        >
          {name}
        </h2>
        <button
          type="button"
          onClick={onClose}
          aria-label="Back to artifacts"
          className="absolute inset-y-0 right-0 grid min-w-9 place-items-center border-l border-dialog-title-foreground/20 font-mono text-title text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none mouse:min-w-8"
        >
          <span aria-hidden="true">✕</span>
        </button>
      </header>
      <div className="min-h-0 flex-1 overflow-y-auto p-3 sm:p-4">
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
        applyLabel="Attach to message"
        onApply={
          attach
            ? (edited) => attach(edited, editedFilename(artifact.name))
            : undefined
        }
      />
    );
  }

  if (artifact.kind === "video") {
    return (
      <DetailOverlay name={artifact.name} onClose={onClose}>
        <video
          src={url}
          controls
          playsInline
          preload="metadata"
          className="h-full w-full bg-code object-contain"
        />
      </DetailOverlay>
    );
  }

  // Markdown and plain text are read by the APP, not by a sandboxed frame: an
  // iframe would paint `# Heading` as `# Heading`, which is the artifact's
  // source rather than the artifact.
  if (isTextMedia(artifact.mediaType, artifact.name)) {
    return (
      <DetailOverlay name={artifact.name} onClose={onClose}>
        <TextFrame url={url} mime={artifact.mediaType} name={artifact.name} />
      </DetailOverlay>
    );
  }

  return (
    <DetailOverlay name={artifact.name} onClose={onClose}>
      <DocFrame url={url} mime={artifact.mediaType} name={artifact.name} />
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
      <ClipIcon />
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
      setOpened((current) => {
        if (current) return null;
        onClose();
        return null;
      });
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [onClose]);

  return (
    <div
      id="artifacts-surface"
      role="region"
      aria-label="Artifacts produced by the model"
      className="absolute inset-0 z-10 flex flex-col border-t border-dialog-edge bg-ink"
    >
      <SurfaceHeader list={artifacts} onClose={onClose} />
      <FilterStrip
        list={artifacts}
        active={filter}
        onPick={(label) => {
          setFilter(label);
          setPages(1);
        }}
      />
      <div className="min-h-0 flex-1 overflow-y-auto p-3 sm:p-4">
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
                />
              ))}
            </div>
            {page.rest.length > 0 && (
              <button
                type="button"
                onClick={() => setPages((current) => current + 1)}
                aria-label={`Load ${page.restLabel} of artifacts`}
                className="mt-3 flex min-h-8 w-full items-center justify-center gap-1.5 border border-dialog-edge bg-panel font-mono text-meta text-dialog-hint hover:bg-hover focus-visible:outline-2 focus-visible:outline-accent mouse:min-h-7"
              >
                <span aria-hidden="true">↓</span>
                <span>Load {page.restLabel}</span>
              </button>
            )}
          </>
        ) : (
          <p className="font-mono text-meta text-dialog-hint">
            Nothing of that kind in this session.
          </p>
        )}
      </div>
      <SurfaceFooter />
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
