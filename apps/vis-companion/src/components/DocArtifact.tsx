import { memo, type ReactNode, useCallback, useContext, useEffect, useRef, useState } from 'react';
import { createPortal } from 'react-dom';

import type { GatewayClient } from '../lib/gateway';
import { ArtifactLinkContext } from '../lib/artifact-links';
import {
  attachmentBytes,
  docKindLabel,
  isMarkdownMedia,
  isMarkupMedia,
  isPdfMedia,
  isTextMedia,
  isDiffMedia,
} from '../lib/artifacts';
import { openExternalUrl } from '../lib/desktop';
import { parseAnnotated } from '../lib/markdown-annotations';
import type { IterationAttachment } from '../lib/types';
import { type DocumentChrome, MarkdownArtifact } from './MarkdownArtifact';
import { DiffArtifact } from './DiffArtifact';
import { PdfAnnotator } from './PdfArtifact';
import { readArtifactText, TextFrame } from './TextArtifact';
import { ChevronIcon } from './icons';
import { useArtifactShare } from './Media';
import { BandButton, DialogHeader, ListRow, overlayLayer } from './ui';
import { useStickyOverlay } from '../lib/sticky-overlay';
import { useBackLayer } from '../lib/edge-back';

/**
 * What an OPENED artifact needs in order to be marked up: which session and
 * iteration own it, and a client to save the revision through. Given this, a
 * note opened from the transcript is annotatable and a PDF can be drawn on —
 * both saving under the same filename, which is the next version.
 */
export type AnnotateContext = {
  client: GatewayClient;
  sid: string;
  iterationId: string;
};

/**
 * AN OPENED DOCUMENT IS THE WHOLE SCREEN, and there is one of it.
 *
 * A note, a PDF, a diff and a data file are opened from the transcript by one
 * gesture, so they open on one screen: the band names the document, reports what
 * it IS under that name, and the way out is the band's own cell. The body under
 * it takes every remaining pixel and owns its own scrolling. A RUN is not one of
 * these — it opens in the app's dialog (`RunDialog`), standing over the chat
 * rather than replacing it.
 *
 * IT IS THE VIEWPORT-PINNED LAYER, NOT THE GLASS. It mounts where `Modal`
 * mounts (`overlayLayer`), `absolute` in the shell the keyboard driver pins, so
 * a raised keyboard cannot bury a field at its bottom edge. Escape is the way
 * out a keyboard has, and it is here rather than at a call site so every
 * document answers that key the same.
 */
export function OverlayScreen({
  title,
  subtitle,
  actions,
  onClose,
  children,
}: {
  title: string;
  /** What the band REPORTS about the artifact under its name. */
  subtitle?: ReactNode;
  /** The artifact's own verbs, as cells of this band before the ✕. */
  actions?: ReactNode;
  onClose: () => void;
  children: ReactNode;
}) {
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === 'Escape') onClose();
    }
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  // A phone has no Escape, and its back is the same door: while this document is up,
  // back closes it rather than leaving the session standing underneath.
  useBackLayer(onClose);

  const { position } = overlayLayer();
  return (
    <div
      className={`${position} inset-0 z-50 flex h-full min-h-0 min-w-0 flex-col overflow-hidden overscroll-contain bg-panel pt-[env(safe-area-inset-top)]`}
    >
      <DialogHeader
        title={title}
        isTitleTruncated
        subtitle={subtitle}
        actions={actions}
        closeLabel={`Close ${title}`}
        onClose={onClose}
      />
      <div className="relative flex min-h-0 min-w-0 flex-1 flex-col">{children}</div>
    </div>
  );
}

// A PDF or an HTML page is a DOCUMENT, not a picture and not data: nothing in it
// is worth spending a model's context on, so `attach` clamps it to
// `audience: "user"` and emits a ````vis-doc` fence with five header lines
// (summary / host path / mime / name / size) and NO payload. The TUI opens the
// host file in the system viewer; this component is the app's half.
//
// THE DOCUMENT APPEARS ONCE. The fence and the attachment rail describe the same
// artifact, so painting a card for the fence AND a tile for the attachment put
// two boxes for one file in the turn. The attachment tile is the one that can
// open and annotate the bytes, so it is the only one: `ChatContent` renders
// nothing for the fence itself.

/**
 * An attached page is UNTRUSTED markup, so it is never mounted in the app's own
 * document: it renders in an iframe, which is a separate document with its own
 * CSS scope — a `body { background: red }` in the artifact cannot reach a single
 * pixel of the app around it.
 *
 * The `sandbox` attribute is what makes that isolation a SECURITY boundary
 * rather than a styling one, and the danger is a COMBINATION, not a flag:
 * a blob: URL inherits the app's origin, so `allow-same-origin` would hand the
 * artifact the app's storage, the gateway's bearer token and the ability to
 * strip its own sandbox. It is never granted, to any media type. Two more
 * capabilities stay off because they act on the app AROUND the frame rather
 * than inside it: `allow-top-navigation`, which would yank the user off the
 * companion, and `allow-popups`, which would open windows behind it. Its
 * `allow-popups-to-escape-sandbox` partner is worse: an unsandboxed popup can
 * open the page's own app-origin blob: URL, and it can rewrite the companion
 * through `window.opener`. A link leaves through the app instead (`DOC_LINK_BRIDGE`).
 *
 * `allow-scripts` IS GRANTED, to every document and with nothing to press
 * first. The frame is the boundary; the capabilities withheld above are what
 * that boundary means. A script with no origin, no top-level navigation and no
 * popups can paint its own document and reach nothing else — and a page that
 * only half renders is not the artifact the reader was handed. The cost is
 * LIVENESS, not privilege: a sandboxed blob: frame is same-site, so a page that
 * loops takes the app's web process with it and the app has to be relaunched.
 *
 * A PDF is the exception that is not one: `allow-scripts` there runs Chromium's
 * built-in viewer, which refuses to paint without it, never the artifact.
 */
export function docSandbox(mime: string | undefined): string {
  if (isPdfMedia(mime)) return 'allow-scripts';
  return 'allow-scripts allow-forms allow-modals allow-pointer-lock allow-downloads';
}

/** The message a framed page sends when the reader follows one of its links. */
const DOC_LINK_MESSAGE = 'vis-doc-open';

/** The schemes a framed page can hand to the browser: web pages and mail. */
const OPENABLE_SCHEMES = new Set(['http:', 'https:', 'mailto:']);

/**
 * THE WAY OUT OF THE FRAME FOR A LINK (#382). A report links to Jira, Gerrit and
 * Jenkins; inside the sandbox such a link either loads the site in the frame, where
 * `X-Frame-Options` paints it blank, or asks for a popup that the sandbox refuses.
 *
 * This script runs first in the framed page. It takes each followed link and each
 * `window.open`, and it posts the absolute URL to the app. `DocFrame` opens it as
 * any external link: a new `noopener` tab, or the system browser on the desktop.
 * A link to `#section` stays in the page, and a page that prevents the click keeps it.
 *
 * The source has no `<` and no `&`, so it is also well-formed inside an XHTML page.
 */
export const DOC_LINK_BRIDGE = `(function (w) {
  var d = w.document;
  function send(raw) {
    var href;
    try { href = new URL(String(raw), d.baseURI).href; } catch (e) { return false; }
    if (!/^(https?|mailto):/i.test(href)) return false;
    w.parent.postMessage({ type: '${DOC_LINK_MESSAGE}', href: href }, '*');
    return true;
  }
  function claim(e) {
    if (e.defaultPrevented || e.button > 1) return;
    var a = e.target instanceof w.Element ? e.target.closest('a[href], area[href]') : null;
    if (!a) return;
    var raw = a.getAttribute('href');
    if (!raw || raw.charAt(0) === '#') return;
    if (send(raw)) e.preventDefault();
  }
  w.addEventListener('click', claim);
  w.addEventListener('auxclick', claim);
  w.open = function (raw) { if (raw) send(raw); return null; };
})(window);`;

/**
 * The page's bytes with `DOC_LINK_BRIDGE` as its first script: just inside
 * `<head>`, else inside `<html>`, else after the doctype, else at the start.
 * The scan reads bytes, so the page keeps its own encoding. A UTF-16 page has
 * no ASCII markup to find and stays as it is.
 */
export function withDocLinkBridge(bytes: Uint8Array): Uint8Array {
  if (bytes[0] === 0xfe && bytes[1] === 0xff) return bytes;
  if (bytes[0] === 0xff && bytes[1] === 0xfe) return bytes;
  const head = new TextDecoder('latin1').decode(bytes.subarray(0, 8192));
  const tag =
    /<head(\s[^>]*)?>/i.exec(head) ?? /<html(\s[^>]*)?>/i.exec(head) ?? /<!doctype[^>]*>/i.exec(head);
  const bom = bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf ? 3 : 0;
  const at = tag ? tag.index + tag[0].length : bom;
  const script = new TextEncoder().encode(`<script>${DOC_LINK_BRIDGE}</script>`);
  const out = new Uint8Array(bytes.length + script.length);
  out.set(bytes.subarray(0, at), 0);
  out.set(script, at);
  out.set(bytes.subarray(at), at + script.length);
  return out;
}

/** The URL that a framed page asked the app to open, or null for any other message. */
export function docLinkHref(data: unknown): string | null {
  if (typeof data !== 'object' || data === null) return null;
  const { type, href } = data as { type?: unknown; href?: unknown };
  if (type !== DOC_LINK_MESSAGE || typeof href !== 'string') return null;
  try {
    const url = new URL(href);
    return OPENABLE_SCHEMES.has(url.protocol) ? url.href : null;
  } catch {
    return null;
  }
}

/**
 * The artifact itself, quarantined: markup renders in its own opaque-origin
 * frame, with no gateway credential and no parent callback. Its one message
 * to the app is a followed link, which the app opens only from this frame,
 * only for a web or mail URL and only while the reader's click is active.
 */
export const DocFrame = memo(function DocFrame({
  url,
  mime,
  name,
}: {
  url: string;
  mime: string;
  name: string;
}) {
  const frame = useRef<HTMLIFrameElement>(null);
  const markup = isMarkupMedia(mime);
  // A markup page is framed from its own copy that carries the link bridge.
  // The shared attachment URL stays as it is for every other view.
  const [bridged, setBridged] = useState<{ from: string; url: string } | null>(null);
  useEffect(() => {
    if (!markup) return;
    let alive = true;
    let made = '';
    void (async () => {
      try {
        const page = await (await fetch(url)).blob();
        const bytes = withDocLinkBridge(new Uint8Array(await page.arrayBuffer()));
        if (!alive) return;
        made = URL.createObjectURL(new Blob([bytes as BlobPart], { type: page.type || mime }));
        setBridged({ from: url, url: made });
      } catch {
        // Without the bridge the page still paints; only its links stay inside the frame.
        if (alive) setBridged({ from: url, url });
      }
    })();
    return () => {
      alive = false;
      if (made) URL.revokeObjectURL(made);
    };
  }, [markup, mime, url]);
  useEffect(() => {
    const onMessage = (event: MessageEvent) => {
      const own = frame.current?.contentWindow;
      if (!own || event.source !== own) return;
      const href = docLinkHref(event.data);
      if (!href) return;
      // A page script without a click cannot open tabs or the system browser.
      if (navigator.userActivation && !navigator.userActivation.isActive) return;
      openExternalUrl(href);
    };
    window.addEventListener('message', onMessage);
    return () => window.removeEventListener('message', onMessage);
  }, []);
  const src = markup ? (bridged?.from === url ? bridged.url : undefined) : url;
  return (
    <iframe
      ref={frame}
      title={name}
      src={src}
      sandbox={docSandbox(mime)}
      referrerPolicy="no-referrer"
      loading="lazy"
      className="min-h-0 w-full flex-1 border-0 bg-input"
    />
  );
});

/** The artifact's own bytes: a note read by the app, anything else in a frame. */
function DocBody({
  name,
  mime,
  url,
  failed,
}: {
  name: string;
  mime: string;
  url: string | null;
  failed: boolean;
}) {
  if (failed)
    return (
      <p className="px-2 py-3 text-meta text-footer-muted">
        This document could not be loaded from the gateway.
      </p>
    );
  if (!url) return <p className="px-2 py-3 text-meta text-footer-muted">Loading…</p>;
  // Markdown and plain text are read by the APP: an iframe would paint
  // `# Heading` as `# Heading`, the source instead of the document.
  return isTextMedia(mime, name) ? (
    <TextFrame url={url} mime={mime} name={name} fill />
  ) : (
    <DocFrame url={url} mime={mime} name={name} />
  );
}

/**
 * THE DOCUMENTS OF ONE STEP, in one stack.
 *
 * A card per file made every artifact its own framed box, so four files were four
 * frames with four edges and four kind chips down the transcript — the turn read
 * as a settings screen rather than as "this step wrote four documents". The stack
 * is the frame; a document is a ROW inside it, and the rows share one rule.
 *
 * The header is the group's own report — `4 documents · 31.4KB` — and it only
 * exists when there IS a group: one document is a single row with no header at
 * all, so the common turn pays nothing for the rarer one.
 *
 * A settled RUN is a row of this same stack: an artifact the reader opens,
 * under the rule the documents already share.
 */
export const DocStack = memo(function DocStack({
  summary,
  children,
}: {
  /** What the whole group is. Omitted for a lone document. */
  summary?: string;
  children: ReactNode;
}) {
  return (
    <div className="mt-2 min-w-0 border border-code-edge bg-input">
      {summary ? (
        <p className="border-b border-code-edge bg-panel px-3 py-1 font-mono text-chip text-footer-muted">
          {summary}
        </p>
      ) : null}
      <div className="divide-y divide-code-edge">{children}</div>
    </div>
  );
});

/**
 * `4 documents · 31.4KB` — what a stack is, said once above it.
 *
 * It counts ARTIFACTS, never cuts: the rail hands it one head per name
 * (`collapseAttachmentVersions`), so a note saved a second time is one document
 * of its newest weight instead of "2 documents" of both weights added together.
 *
 * The weight is claimed only when every document reported one, exactly as
 * `mediaSummary` claims a gallery's: a partial total is a wrong number, not a
 * smaller one.
 */
export function docStackSummary(docs: { size?: number }[]): string {
  const things = `${docs.length} ${docs.length === 1 ? 'document' : 'documents'}`;
  const total = docs.every((doc) => typeof doc.size === 'number')
    ? attachmentBytes(docs.reduce((sum, doc) => sum + (doc.size ?? 0), 0))
    : '';
  return total ? `${things} · ${total}` : things;
}

/**
 * An opened document owns the WHOLE viewport — full height and full width —
 * with nothing above it but its own caption row and the app's one X.
 */
export const DocOverlay = memo(function DocOverlay({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  annotate,
  commentable = false,
  versions,
  shownAt = 0,
  onPick,
  onClose,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  url: string | null;
  failed: boolean;
  /** Present when the human may mark this artifact up. */
  annotate?: AnnotateContext;
  commentable?: boolean;
  /** Every cut of this name, newest first and the head included. */
  versions?: IterationAttachment[];
  /** Which cut is being read, an index into `versions`. */
  shownAt?: number;
  /** Read another cut; the owner fetches its bytes. */
  onPick?: (at: number) => void;
  onClose: () => void;
}) {
  const artifactLinks = useContext(ArtifactLinkContext);
  const openAttachment = artifactLinks
    ? (attachmentId: string) => {
        onClose();
        artifactLinks.open(attachmentId);
      }
    : undefined;
  // THE HISTORY OF A NAME BELONGS TO THE BAND, NOT TO THE TRANSCRIPT.
  //
  // A revised document is ONE row in the step (`collapseAttachmentVersions`), so
  // the thread behind it needs a door — and it is a cell of the band that already
  // names the document, one hairline from the way out and beside Save. `v2 of 2`
  // opens the cuts and closes them again, so the transcript keeps its single row
  // and the chevron that has always opened it. A file written once has no thread
  // and no cell.
  const [isHistory, setHistory] = useState(false);
  const cuts = versions ?? [];
  const shown = cuts[shownAt];
  const canComment = shown ? shown.commentable === true : commentable === true;
  const shownLabel = shown ? attachmentBytes(shown.size) : sizeLabel;
  const versionCell =
    cuts.length > 1 ? (
      <BandButton
        onClick={() => setHistory((open) => !open)}
        aria-expanded={isHistory}
        aria-label={`Versions of ${name}`}
        title={`Versions of ${name}`}
      >
        v{shown?.version ?? 1} of {cuts.length}
      </BandButton>
    ) : null;
  // The shown cut's original bytes, so a document read in the session leaves the
  // app exactly as one opened from the artifacts sheet does.
  const share = useArtifactShare(
    url && !failed
      ? { name, mediaType: mime, load: async () => (await fetch(url)).blob() }
      : null,
  );

  const chrome: DocumentChrome = ({ actions, note, body }) => (
    <OverlayScreen
      title={name}
      subtitle={note || [docKindLabel(mime), shownLabel].filter(Boolean).join(' · ')}
      actions={
        <>
          {versionCell}
          {actions}
          {share.cell}
        </>
      }
      onClose={onClose}
    >
      {share.notice}
      {body}
    </OverlayScreen>
  );

  // The thread itself, in the document's own screen: newest first, each row the
  // cut it opens. Nothing is fetched until one is picked — the same rule the
  // gallery's `ArtifactVersions` follows.
  if (isHistory)
    return chrome({
      actions: null,
      note: `${cuts.length} versions`,
      body: (
        <ul
          aria-label={`Versions of ${name}`}
          className="flex min-h-0 flex-1 flex-col gap-2 overflow-y-auto p-3 sm:p-4"
        >
          {cuts.map((cut, at) => (
            <li key={`${cut.iteration_id ?? 'iter'}-${cut.index}`}>
              <ListRow
                isFramed
                onClick={() => {
                  onPick?.(at);
                  setHistory(false);
                }}
                aria-label={`Read v${cut.version ?? 1} of ${name}`}
                className="gap-3"
              >
                <span className="font-mono text-meta font-bold text-white">
                  v{cut.version ?? 1}
                </span>
                <span className="min-w-0 flex-1 truncate font-mono text-chip text-footer-muted">
                  {attachmentBytes(cut.size)}
                </span>
                {at === shownAt || at === 0 ? (
                  <span className="font-mono text-chip text-accent-ink">
                    {at === shownAt ? 'reading' : 'latest'}
                  </span>
                ) : null}
              </ListRow>
            </li>
          ))}
        </ul>
      ),
    });

  if (annotate && url && !failed && isDiffMedia(mime)) {
    return (
      <DiffArtifact
        client={annotate.client}
        sid={annotate.sid}
        iterationId={annotate.iterationId}
        name={name}
        source={url}
        version={shown?.version}
        commentable={canComment}
        chrome={chrome}
      />
    );
  }
  if (annotate && url && !failed && isTextMedia(mime, name)) {
    return (
      <MarkdownArtifact
        client={annotate.client}
        sid={annotate.sid}
        iterationId={annotate.iterationId}
        name={name}
        mediaType={mime}
        source={url}
        version={shown?.version}
        commentable={canComment}
        plain={!isMarkdownMedia(mime, name)}
        chrome={chrome}
        onOpenAttachment={openAttachment}
      />
    );
  }

  // A PDF is read the same way: the app rasterises the page so it fits, and the
  // band it hands up carries the pager and the pen.
  if (annotate && url && !failed && isPdfMedia(mime)) {
    return (
      <PdfAnnotator
        client={annotate.client}
        sid={annotate.sid}
        iterationId={annotate.iterationId}
        name={name}
        mediaType={mime}
        url={url}
        commentable={canComment}
        chrome={chrome}
      />
    );
  }

  return chrome({
    actions: null,
    note: '',
    body: <DocBody name={name} mime={mime} url={url} failed={failed} />,
  });
});

/**
 * ONE document in the transcript: a row of a {@link DocStack}, and the row IS the
 * verb — pressing anywhere on it throws the artifact over the whole screen.
 *
 * The screen's only verb used to be a chip at the far trailing edge, so nine
 * tenths of the card was dead paper a finger could land on for nothing. A row
 * that opens needs no `Open` beside it; the `›` says where the press goes.
 *
 * The bytes are NOT painted here. A note embedded in place stood taller than the
 * turn that produced it — the reader scrolled a whole document to reach the next
 * line of the conversation — and a PDF beside it did the same in a 60vh frame.
 * The transcript states what was produced; reading it is one tap away.
 */
export const DocPreview = memo(function DocPreview({
  name,
  mime,
  sizeLabel,
  url,
  failed,
  annotate,
  commentable = false,
  versions,
  shownAt = 0,
  onPick,
  onNeeded,
}: {
  name: string;
  mime: string;
  sizeLabel?: string;
  /** The artifact's object URL, or null while it is still being fetched. */
  url: string | null;
  failed: boolean;
  /** Present when the opened artifact may be marked up. */
  annotate?: AnnotateContext;
  commentable?: boolean;
  /** Every cut of this name, newest first — one entry for a file written once. */
  versions?: IterationAttachment[];
  /** Which cut the opened document is reading, an index into `versions`. */
  shownAt?: number;
  /** Read another cut; the owner fetches its bytes. */
  onPick?: (at: number) => void;
  /** Asked for the bytes — the parent starts the fetch. */
  onNeeded: () => void;
}) {
  // Keyed by the document, not by this row: a turn settling re-mounts the row
  // under a different subtree, and an opened document must stay opened.
  const [opened, setOpened] = useStickyOverlay(`doc:${name}`);

  // WHAT THE NEW VERSION IS, IN THE ROW THE DOCUMENT ALREADY HAD.
  //
  // Reported: "I commented on it and saved, and now there are two". A revision
  // does not deserve a row — it deserves a LINE. `v2 · 3 comments` under the name
  // says the thing the second row was accidentally saying, and costs no height:
  // the row's box is its own minimum either way.
  //
  // The comments are not metadata beside the file, they are IN it, under one
  // `## Comments` heading (`lib/markdown-annotations`) — so the count is read
  // from the bytes this row already holds, and only for a note that was actually
  // revised and is showing its newest cut.
  const [comments, setComments] = useState(0);
  const latest = versions?.[0];
  const revision = latest && (latest.version ?? 1) > 1 ? (latest.version ?? 1) : 0;

  useEffect(() => {
    onNeeded();
  }, [onNeeded]);

  useEffect(() => {
    if (!revision || shownAt !== 0 || !url || !isTextMedia(mime, name)) return;
    let alive = true;
    readArtifactText(url)
      .then((text) => {
        if (alive) setComments(parseAnnotated(text).comments.length);
      })
      .catch(() => undefined);
    return () => {
      alive = false;
    };
  }, [revision, shownAt, url, mime, name]);

  const close = useCallback(() => setOpened(false), [setOpened]);
  const open = useCallback(() => setOpened(true), [setOpened]);

  const revisionLine = revision
    ? [`v${revision}`, comments > 0 ? `${comments} ${comments === 1 ? 'comment' : 'comments'}` : '']
        .filter(Boolean)
        .join(' · ')
    : '';

  return (
    <>
      {/* The row IS the control, so the whole line is the target a finger gets:
          `ListRow` is the app's one pressable row and owns that box. */}
      <ListRow onClick={open} title={name} aria-label={`Open ${name}`}>
        <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-warn">
          {docKindLabel(mime, name)}
        </span>
        <span className="flex min-w-0 flex-1 flex-col">
          <span className="truncate font-mono text-chip text-muted">{name}</span>
          {revisionLine ? (
            <span className="truncate font-mono text-chip text-footer-muted">{revisionLine}</span>
          ) : null}
        </span>
        {sizeLabel ? (
          <span className="shrink-0 font-mono text-chip text-footer-muted">{sizeLabel}</span>
        ) : null}
        <ChevronIcon className="size-3 shrink-0 text-footer-muted opacity-70" />
      </ListRow>
      {/* The opened document is a SCREEN, not a part of the transcript: it is
          portalled out of the turn, so the composer strip the session screen pins
          to the bottom cannot paint on top of it — into the viewport-pinned shell
          (`overlayLayer`), so a keyboard raised over it does not bury the
          annotator's own field. */}
      {opened &&
        createPortal(
          <DocOverlay
            name={name}
            mime={mime}
            sizeLabel={sizeLabel}
            url={url}
            failed={failed}
            annotate={annotate}
            commentable={commentable}
            versions={versions}
            shownAt={shownAt}
            onPick={onPick}
            onClose={close}
          />,
          overlayLayer().host,
        )}
    </>
  );
});
