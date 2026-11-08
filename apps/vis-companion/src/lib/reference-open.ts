import { proposedFilename, fileHost, type FileHost } from './artifact-save';
import { attachmentIsAudio, attachmentIsVideo, isArchiveMedia, isPdfMedia } from './artifacts';
import { desktopInvoke, openExternalUrl } from './desktop';
import type { GatewayClient } from './gateway';
import type { PdfRangeSource } from './pdf-annotate';
import type { AttachmentLink, IterationAttachment } from './types';

/**
 * OPENING A FILE THAT THE SESSION DATABASE DOES NOT KEEP (#451).
 *
 * The file of a `reference` attachment stays on the disk of the gateway machine and can
 * be hundreds of megabytes. So the app never makes one Blob of it: a PDF reader reads
 * parts of it, and a download or another app gets a signed address of it
 * (`GatewayClient.attachmentLink`), because none of them can send the gateway token.
 *
 * - `stream`: a recording or a clip plays from the stream of the file preview (#449).
 * - `read`: a PDF opens in the reader of the app, which reads only the parts that a page
 *   needs. pdf.js draws the page, so this works also where the web view has no PDF viewer.
 * - `open`: the desktop app writes the stream to Downloads/Vis Artifacts and opens the
 *   file in its system app, such as Excel or Word.
 * - `reveal`: the same for an archive, which the file manager shows.
 * - `download`: a browser saves the file under its name.
 * - `system`: a phone gives the address to the system browser, which shows or saves it.
 */
export type ReferencePlan = 'stream' | 'read' | 'open' | 'reveal' | 'download' | 'system';

export function referencePlan(
  attachment: IterationAttachment,
  host: FileHost = fileHost(),
): ReferencePlan {
  if (attachmentIsAudio(attachment) || attachmentIsVideo(attachment)) return 'stream';
  // The reader reads parts, so it needs the size of the file.
  const isPdf = isPdfMedia(attachment.media_type) || /\.pdf$/i.test(attachment.filename ?? '');
  if (isPdf && (attachment.size ?? 0) > 0) return 'read';
  return handOffPlan(attachment, host);
}

/** How this host gives the file to another app or to the disk, the PDF reader not counted. */
export function handOffPlan(
  attachment: IterationAttachment,
  host: FileHost = fileHost(),
): 'open' | 'reveal' | 'download' | 'system' {
  if (host === 'desktop') {
    return isArchiveMedia(attachment.media_type, attachment.filename) ? 'reveal' : 'open';
  }
  return host === 'phone' ? 'system' : 'download';
}

/** The name of the verb of a hand-off, as a band cell or a button says it. */
export function handOffVerb(plan: 'open' | 'reveal' | 'download' | 'system'): string {
  return { open: 'Open in app', reveal: 'Show in folder', download: 'Download', system: 'Open' }[plan];
}

/** A link is asked for again this long before its ticket expires. */
export const LINK_RENEW_MS = 60_000;

/**
 * The signed address of one reference, asked for once and again only when its ticket
 * is about to expire or the gateway refused it.
 */
export class ReferenceLink {
  private link: AttachmentLink | null = null;
  private pending: Promise<AttachmentLink> | null = null;
  private readonly client: Pick<GatewayClient, 'attachmentLink' | 'gatewayUrl'>;
  private readonly sid: string;
  private readonly iterationId: string;
  private readonly index: number;
  private readonly now: () => number;

  constructor(
    client: Pick<GatewayClient, 'attachmentLink' | 'gatewayUrl'>,
    sid: string,
    iterationId: string,
    index: number,
    now: () => number = Date.now,
  ) {
    this.client = client;
    this.sid = sid;
    this.iterationId = iterationId;
    this.index = index;
    this.now = now;
  }

  /** The absolute address when the held ticket is still good, else null. No request. */
  peek(): string | null {
    const link = this.link;
    return link && this.now() < link.expires_at - LINK_RENEW_MS
      ? this.client.gatewayUrl(link.content_url)
      : null;
  }

  /** The absolute address. `renew` asks for a new ticket, as after a 401. */
  async url(renew = false): Promise<string> {
    const held = renew ? null : this.peek();
    if (held) return held;
    this.pending ??= this.client
      .attachmentLink(this.sid, this.iterationId, this.index)
      .finally(() => {
        this.pending = null;
      });
    this.link = await this.pending;
    return this.client.gatewayUrl(this.link.content_url);
  }
}

/** The same address, which the gateway answers with `Content-Disposition: attachment`. */
export function downloadUrl(url: string): string {
  return `${url}${url.includes('?') ? '&' : '?'}download=1`;
}

/**
 * The parts of a referenced PDF, read with Range requests from its signed address.
 *
 * The ticket is the authorization, so no token header goes with a read. A 401 means
 * that the ticket expired while the PDF was open: the link is asked for again once. A
 * response that is not the asked part is cancelled before its body is read, so a
 * gateway that ignores Range never sends the whole file into memory.
 */
export function referenceRange(
  link: Pick<ReferenceLink, 'url'>,
  length: number,
  fetcher: typeof fetch = (input, init) => fetch(input, init),
): PdfRangeSource {
  return {
    length,
    async read(begin, end) {
      for (let attempt = 0; ; attempt += 1) {
        const response = await fetcher(await link.url(attempt > 0), {
          headers: { Range: `bytes=${begin}-${end - 1}` },
          cache: 'no-store',
        });
        const isWhole = response.status === 200 && begin === 0 && end >= length;
        if (response.status === 206 || isWhole) return await response.arrayBuffer();
        void response.body?.cancel().catch(() => undefined);
        if (response.status === 401 && attempt === 0) continue;
        throw new Error(`HTTP ${response.status}`);
      }
    },
  };
}

/**
 * Give the file to the platform for a plan that is not `stream` or `read`, and say
 * what happened. `url` is the absolute signed address.
 *
 * On a phone the address goes to the system browser at once: a web view opens a window
 * only inside the press, so the caller gives an address that it already holds
 * (`ReferenceLink.peek`) when it can.
 */
export async function handOffReference(
  plan: 'open' | 'reveal' | 'download' | 'system',
  url: string,
  name: string,
): Promise<string> {
  const filename = proposedFilename(name);
  if (plan === 'open' || plan === 'reveal') {
    const invoke = desktopInvoke();
    if (!invoke) throw new Error('Opening a file needs the desktop app.');
    const result = (await invoke('open_remote_artifact', {
      url: downloadUrl(url),
      filename,
      reveal: plan === 'reveal',
    })) as { opened: boolean; path: string };
    if (plan === 'reveal') {
      return result.opened
        ? `Saved to ${result.path}.`
        : `Saved to ${result.path}. Open that folder to find it.`;
    }
    return result.opened
      ? `${filename} opened.`
      : `Saved to ${result.path}. Install an app for this file type to open it.`;
  }
  if (plan === 'system') {
    openExternalUrl(downloadUrl(url));
    return `${filename} opened in the browser.`;
  }
  // A browser ignores `download` on an address of another origin, but the gateway
  // answers `download=1` with `Content-Disposition: attachment` and the file name.
  const link = document.createElement('a');
  link.href = downloadUrl(url);
  link.download = filename;
  link.rel = 'noopener';
  link.click();
  return `Downloading ${filename}.`;
}
