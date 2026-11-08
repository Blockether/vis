// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';
import type { FileHost } from './artifact-save';
import {
  handOffReference,
  LINK_RENEW_MS,
  ReferenceLink,
  referencePlan,
  referenceRange,
} from './reference-open';
import type { AttachmentLink, IterationAttachment } from './types';

const LINK_SCHEMA = gatewaySchema.$defs.attachment_link;
const NOW = 1_794_200_000_000;
const TICKET = '/v1/sessions/s1/iterations/it-1/attachments/2?expires=1794243200000&ticket=AbC_-9z';

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

/** A link answer that the contract accepts. */
function link(content_url = TICKET, expires_at = NOW + 12 * 3_600_000): AttachmentLink {
  const answer = { content_url, expires_at };
  for (const key of LINK_SCHEMA.required) expect(answer).toHaveProperty(key);
  expect(content_url).toMatch(new RegExp(LINK_SCHEMA.properties.content_url.pattern));
  return answer;
}

describe('GatewayClient.attachmentLink', () => {
  it('asks the link route of one attachment and reads the contract shape', async () => {
    const answer = link();
    const fetches = vi.fn(async (_input: string, _init?: RequestInit) => new Response(JSON.stringify(answer)));
    vi.stubGlobal('fetch', fetches);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.test:7890', token: 'secret' });
    expect(await client.attachmentLink('s1', 'it-1', 2)).toEqual(answer);
    const [url, init] = fetches.mock.calls[0]!;
    expect(url).toBe('http://gateway.test:7890/v1/sessions/s1/iterations/it-1/attachments/2/link');
    expect(new Headers(init?.headers).get('Authorization')).toBe('Bearer secret');
    expect(client.gatewayUrl(answer.content_url)).toBe(`http://gateway.test:7890${TICKET}`);
  });

  it('refuses an answer without an address', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ expires_at: NOW }))));
    const { GatewayClient } = await import('./gateway');
    await expect(new GatewayClient({ url: 'http://gateway.test' }).attachmentLink('s1', 'it-1', 2)).rejects.toThrow(
      /no address/,
    );
  });
});

describe('referencePlan', () => {
  const file = (filename: string, media_type: string, size = 200_000_000): IterationAttachment => ({
    index: 0,
    filename,
    media_type,
    size,
    storage: 'reference',
    path: `/data/${filename}`,
  });
  const cases: [IterationAttachment, Record<FileHost, string>][] = [
    [file('talk.mp3', 'audio/mpeg'), { desktop: 'stream', web: 'stream', phone: 'stream' }],
    [file('talk.mp4', 'video/mp4'), { desktop: 'stream', web: 'stream', phone: 'stream' }],
    [file('atlas.pdf', 'application/pdf'), { desktop: 'read', web: 'read', phone: 'read' }],
    [file('atlas.pdf', 'application/octet-stream'), { desktop: 'read', web: 'read', phone: 'read' }],
    // Without a size the reader cannot read parts, so the file leaves the app.
    [file('atlas.pdf', 'application/pdf', 0), { desktop: 'open', web: 'download', phone: 'system' }],
    [
      file('q3.xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
      { desktop: 'open', web: 'download', phone: 'system' },
    ],
    [
      file('plan.docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'),
      { desktop: 'open', web: 'download', phone: 'system' },
    ],
    [file('dump.zip', 'application/zip'), { desktop: 'reveal', web: 'download', phone: 'system' }],
    [file('disk.bin', 'application/octet-stream'), { desktop: 'open', web: 'download', phone: 'system' }],
  ];
  it.each(cases)('opens %s per host', (attachment, plans) => {
    for (const host of ['desktop', 'web', 'phone'] as const) {
      expect([host, referencePlan(attachment, host)]).toEqual([host, plans[host]]);
    }
  });
});

describe('ReferenceLink', () => {
  it('asks once, again before the ticket expires, and again after a refusal', async () => {
    let now = NOW;
    const attachmentLink = vi.fn(async () => link(TICKET, now + 3_600_000));
    const client = { attachmentLink, gatewayUrl: (path: string) => `http://g${path}` };
    const held = new ReferenceLink(client, 's1', 'it-1', 2, () => now);
    expect(held.peek()).toBeNull();
    await Promise.all([held.url(), held.url()]);
    expect(attachmentLink).toHaveBeenCalledTimes(1);
    expect(attachmentLink).toHaveBeenCalledWith('s1', 'it-1', 2);
    expect(held.peek()).toBe(`http://g${TICKET}`);

    now += 3_600_000 - LINK_RENEW_MS;
    expect(held.peek()).toBeNull();
    await held.url();
    expect(attachmentLink).toHaveBeenCalledTimes(2);

    await held.url(true);
    expect(attachmentLink).toHaveBeenCalledTimes(3);
  });
});

describe('referenceRange', () => {
  const url = vi.fn(async (renew?: boolean) => (renew ? 'http://g/new' : 'http://g/old'));

  it('reads one part with a Range request and no token', async () => {
    const fetcher = vi.fn(async (_input: RequestInfo | URL, _init?: RequestInit) =>
      new Response(new Uint8Array([1, 2, 3]), { status: 206 }),
    );
    const bytes = await referenceRange({ url }, 1_000, fetcher).read(10, 13);
    expect(new Uint8Array(bytes)).toEqual(new Uint8Array([1, 2, 3]));
    const [address, init] = fetcher.mock.calls[0]!;
    expect(address).toBe('http://g/old');
    const headers = new Headers(init?.headers);
    expect(headers.get('Range')).toBe('bytes=10-12');
    expect(headers.has('Authorization')).toBe(false);
  });

  it('asks for a new ticket once after a 401, then fails', async () => {
    const fetcher = vi
      .fn<typeof fetch>()
      .mockResolvedValueOnce(new Response('', { status: 401 }))
      .mockResolvedValueOnce(new Response(new Uint8Array([7]), { status: 206 }));
    expect(new Uint8Array(await referenceRange({ url }, 10, fetcher).read(0, 1))).toEqual(new Uint8Array([7]));
    expect(fetcher.mock.calls[1]![0]).toBe('http://g/new');

    const refused = vi.fn<typeof fetch>(async () => new Response('', { status: 401 }));
    await expect(referenceRange({ url }, 10, refused).read(0, 1)).rejects.toThrow('HTTP 401');
    expect(refused).toHaveBeenCalledTimes(2);
  });

  it('never reads a whole file that answers a part with 200', async () => {
    const cancel = vi.fn(async () => undefined);
    const whole = { status: 200, body: { cancel }, arrayBuffer: vi.fn() } as unknown as Response;
    await expect(referenceRange({ url }, 1_000, async () => whole).read(0, 100)).rejects.toThrow('HTTP 200');
    expect(cancel).toHaveBeenCalled();
    expect(whole.arrayBuffer).not.toHaveBeenCalled();
  });
});

describe('handOffReference', () => {
  const url = `http://g${TICKET}`;

  it('downloads in a browser from the attachment address', async () => {
    const clicks: HTMLAnchorElement[] = [];
    vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(function (this: HTMLAnchorElement) {
      clicks.push(this);
    });
    expect(await handOffReference('download', url, 'Q3 report.xlsx')).toBe('Downloading Q3 report.xlsx.');
    expect(clicks[0]!.href).toBe(`${url}&download=1`);
    expect(clicks[0]!.download).toBe('Q3 report.xlsx');
  });

  it('gives the address to the system browser on a phone, inside the call', () => {
    const open = vi.spyOn(window, 'open').mockReturnValue(null);
    void handOffReference('system', url, 'plan.docx');
    expect(open).toHaveBeenCalledWith(`${url}&download=1`, '_blank', 'noopener,noreferrer');
  });

  it('lets the desktop app download and open the file', async () => {
    const invoke = vi.fn(async () => ({ opened: true, path: '/Users/me/Downloads/Vis Artifacts/1/q3.xlsx' }));
    vi.stubGlobal('__TAURI__', { core: { invoke } });
    expect(await handOffReference('open', url, 'q3.xlsx')).toBe('q3.xlsx opened.');
    expect(invoke).toHaveBeenCalledWith('open_remote_artifact', {
      url: `${url}&download=1`,
      filename: 'q3.xlsx',
      reveal: false,
    });
    expect(await handOffReference('reveal', url, 'dump.zip')).toBe(
      'Saved to /Users/me/Downloads/Vis Artifacts/1/q3.xlsx.',
    );
    expect(invoke).toHaveBeenLastCalledWith('open_remote_artifact', expect.objectContaining({ reveal: true }));
  });
});
