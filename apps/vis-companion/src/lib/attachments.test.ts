/**
 * The composer's `+` and what it is allowed to hand the gateway.
 *
 * Two things it could not do before: reach a file the photo gallery cannot see
 * (a voice memo, a document, a clip that arrived in a chat — none of them are in
 * the camera roll), and admit a recording at all. The gate is shared by every
 * chooser, so these pin the gate and the door in one place.
 *
 * @vitest-environment jsdom
 */
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const filePicker = vi.hoisted(() => ({
  pickFiles: vi.fn(),
  pickMedia: vi.fn(),
  pickImages: vi.fn(),
}));

vi.mock('@capawesome/capacitor-file-picker', () => ({ FilePicker: filePicker }));
vi.mock('@capacitor/core', () => ({
  Capacitor: { isNativePlatform: () => true, convertFileSrc: (path: string) => `native:${path}` },
}));

import {
  attachmentsFromFiles,
  candidateMediaType,
  dragCarriesFiles,
  isAudioMediaType,
  pickDocumentAttachments,
  rejectionNotice,
} from './attachments';

/** One picked file, exactly as the plugin hands it over. */
function picked(name: string, mimeType: string, bytes = 64) {
  return {
    name,
    mimeType,
    blob: new Blob([new Uint8Array(bytes)], { type: mimeType }),
  };
}

beforeEach(() => {
  filePicker.pickFiles.mockReset();
  filePicker.pickMedia.mockReset();
  filePicker.pickImages.mockReset();
});

afterEach(() => vi.unstubAllGlobals());

describe('the FILES door', () => {
  it('opens the document browser, not the gallery sheet', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('memo.m4a', 'audio/mp4')],
    });

    const result = await pickDocumentAttachments({
      mediaTypes: ['image/png', 'audio/mp4'],
    });

    expect(filePicker.pickMedia).not.toHaveBeenCalled();
    expect(filePicker.pickImages).not.toHaveBeenCalled();
    expect(filePicker.pickFiles).toHaveBeenCalledWith({
      types: ['image/png', 'audio/mp4'],
      readData: false,
    });
    expect(result.rejected).toEqual([]);
    expect(result.attachments.map((a) => a.media_type)).toEqual(['audio/mp4']);
    expect(result.attachments[0].filename).toBe('memo.m4a');
  });

  it('reads several native files concurrently instead of serializing their copies', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [
        { name: 'one.png', mimeType: 'image/png', path: '/one.png' },
        { name: 'two.png', mimeType: 'image/png', path: '/two.png' },
      ],
    });
    const releases: Array<() => void> = [];
    const fetchMock = vi.fn(
      () =>
        new Promise<Response>((resolve) =>
          releases.push(() =>
            resolve({
              ok: true,
              blob: async () => new Blob([new Uint8Array(8)], { type: 'image/png' }),
            } as Response),
          ),
        ),
    );
    vi.stubGlobal('fetch', fetchMock);

    const pending = pickDocumentAttachments({ mediaTypes: ['image/png'] });
    await Promise.resolve();
    await Promise.resolve();

    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(fetchMock).toHaveBeenNthCalledWith(1, 'native:/one.png');
    expect(fetchMock).toHaveBeenNthCalledWith(2, 'native:/two.png');
    releases.forEach((release) => release());
    await expect(pending).resolves.toMatchObject({ rejected: [], attachments: [{}, {}] });
  });

  it('claims a file the platform could not name by its extension', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('interview.mp3', '')],
    });

    const result = await pickDocumentAttachments({
      mediaTypes: ['audio/mpeg'],
    });

    expect(result.rejected).toEqual([]);
    expect(result.attachments.map((a) => a.media_type)).toEqual(['audio/mpeg']);
  });

  it('takes back the gzip diagnostics bundle this app exports', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('vis-diagnostics-20260501.jsonl.gz', '')],
    });

    const result = await pickDocumentAttachments({
      mediaTypes: ['application/gzip'],
    });

    expect(result.rejected).toEqual([]);
    expect(result.attachments[0].media_type).toBe('application/gzip');
  });

  it('refuses what this gateway never said it takes', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('archive.zip', 'application/zip')],
    });

    const result = await pickDocumentAttachments({ mediaTypes: ['audio/mp4'] });

    expect(result.attachments).toEqual([]);
    expect(result.rejected).toEqual([
      'archive.zip: application/zip is not an accepted attachment format',
    ]);
  });

  // Regression, user report: the Files door advertised only camera and recording media,
  // so the native document browser would not admit an ordinary document.
  it('admits a document the platform names only by its extension', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('report.pdf', '')],
    });

    const result = await pickDocumentAttachments({ mediaTypes: ['application/pdf'] });

    expect(result.rejected).toEqual([]);
    expect(result.attachments[0].media_type).toBe('application/pdf');
  });
});

describe('what a recording is measured against', () => {
  it('is the recording ceiling, never the still one', async () => {
    const files = [new File([new Uint8Array(512)], 'memo.m4a', { type: 'audio/mp4' })];

    const accepted = await attachmentsFromFiles(files, {
      mediaTypes: ['audio/mp4'],
      maxFileBytes: 64,
      maxAudioBytes: 4096,
    });
    expect(accepted.rejected).toEqual([]);
    expect(accepted.attachments).toHaveLength(1);

    const refused = await attachmentsFromFiles(files, {
      mediaTypes: ['audio/mp4'],
      maxFileBytes: 4096,
      maxAudioBytes: 64,
    });
    expect(refused.attachments).toEqual([]);
    expect(refused.rejected[0]).toContain('memo.m4a');
  });
});

describe('naming a candidate', () => {
  it('believes the platform when it said something', () => {
    expect(candidateMediaType('memo.m4a', 'audio/mp4')).toBe('audio/mp4');
  });

  it('falls back to the extension, and to nothing when that is unknown', () => {
    expect(candidateMediaType('memo.M4A', '')).toBe('audio/mp4');
    expect(candidateMediaType('clip.mov', undefined)).toBe('video/quicktime');
    expect(candidateMediaType('notes', null)).toBe('');
  });

  it('knows a recording from a picture', () => {
    expect(isAudioMediaType('audio/mpeg')).toBe(true);
    expect(isAudioMediaType('image/png')).toBe(false);
    expect(isAudioMediaType(undefined)).toBe(false);
  });
});

// Regression: an Android voice memo arrives from the document provider with the
// MIME type `application/octet-stream` — `ContentResolver.getType` answers that
// for every URI its table does not know. Believed, that word cost the user the
// file at the gate; carried into the Blob, it also silenced the player, because
// a data URL typed `application/octet-stream` will not decode in an <audio>.
describe('what Android hands over', () => {
  it('names an octet-stream memo by its extension and types its bytes to match', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [{ name: 'Recording.m4a', mimeType: 'application/octet-stream', data: 'AAAAAAAA' }],
    });

    const result = await pickDocumentAttachments({ mediaTypes: ['audio/mp4'] });

    expect(result.rejected).toEqual([]);
    expect(result.attachments.map((a) => a.media_type)).toEqual(['audio/mp4']);
    expect(result.attachments[0].previewUrl.startsWith('data:audio/mp4;base64,')).toBe(true);
  });

  it('re-types a dropped file whose Blob the platform left generic', async () => {
    const files = [
      new File([new Uint8Array(64)], 'memo.aac', { type: 'application/octet-stream' }),
    ];

    const result = await attachmentsFromFiles(files, { mediaTypes: ['audio/aac'] });

    expect(result.rejected).toEqual([]);
    expect(result.attachments[0].media_type).toBe('audio/aac');
    expect(result.attachments[0].previewUrl.startsWith('data:audio/aac;base64,')).toBe(true);
  });

  it('keeps an unnameable file unnameable rather than guessing', () => {
    expect(candidateMediaType('memo.m4a', 'application/octet-stream')).toBe('audio/mp4');
    expect(candidateMediaType('blob', 'application/octet-stream')).toBe('application/octet-stream');
  });

  it('takes every recorder format the gateway now advertises', () => {
    expect(candidateMediaType('memo.caf', '')).toBe('audio/x-caf');
    expect(candidateMediaType('memo.amr', '')).toBe('audio/amr');
    expect(candidateMediaType('memo.aiff', '')).toBe('audio/aiff');
    expect(candidateMediaType('memo.aac', '')).toBe('audio/aac');
    expect(candidateMediaType('memo.opus', '')).toBe('audio/ogg');
    expect(candidateMediaType('book.m4b', '')).toBe('audio/mp4');
  });
});

describe('Markdown attachments', () => {
  it.each([
    ['notes.md', ''],
    ['NOTES.MD', 'application/octet-stream'],
    ['notes.markdown', 'text/plain'],
    ['notes.mdown', 'binary/octet-stream'],
    ['notes.mkd', ''],
    ['shared-note', 'text/markdown; charset=utf-8'],
    ['shared-note', 'TEXT/X-MARKDOWN'],
  ])('admits %s declared as %s', async (name, type) => {
    const result = await attachmentsFromFiles([new File(['# Notes\n'], name, { type })]);
    expect(result.rejected).toEqual([]);
    expect(result.attachments[0]).toMatchObject({ filename: name, media_type: 'text/markdown' });
  });

  it('offers Markdown in the document picker', async () => {
    filePicker.pickFiles.mockResolvedValue({ files: [picked('notes.md', 'text/markdown')] });
    const result = await pickDocumentAttachments();
    expect(filePicker.pickFiles).toHaveBeenCalledWith({
      types: expect.arrayContaining(['text/markdown', 'text/x-markdown']),
      readData: false,
    });
    expect(result.rejected).toEqual([]);
    expect(result.attachments).toHaveLength(1);
  });

  it('still obeys the gateway format, size and count limits', async () => {
    const file = new File(['# Notes\n'], 'notes.md');
    for (const limits of [{ mediaTypes: ['image/png'] }, { maxFileBytes: 1 }, { maxFiles: 0 }]) {
      const result = await attachmentsFromFiles([file], limits);
      expect(result.attachments).toEqual([]);
      expect(result.rejected).toHaveLength(1);
    }
    expect(candidateMediaType('notes.txt', 'text/plain')).toBe('text/plain');
    expect(candidateMediaType('notes.md', 'image/png')).toBe('image/png');
  });
});

// Regression #261: a colleague's spreadsheets came back as "media not supported".
describe('CSV and TSV attachments', () => {
  it.each([
    ['rows.csv', ''],
    ['ROWS.CSV', 'application/octet-stream'],
    ['rows.csv', 'text/plain'],
    ['rows.csv', 'application/vnd.ms-excel'],
    ['shared-rows', 'text/csv; charset=utf-8'],
    ['shared-rows', 'TEXT/X-CSV'],
  ])('admits %s declared as %s', async (name, type) => {
    const result = await attachmentsFromFiles([new File(['name,total\ncafé,2\n'], name, { type })]);
    expect(result.rejected).toEqual([]);
    expect(result.attachments[0]).toMatchObject({ filename: name, media_type: 'text/csv' });
  });

  it.each([
    ['rows.tsv', ''],
    ['rows.tsv', 'text/plain'],
    ['shared-rows', 'text/tab-separated-values'],
  ])('admits %s declared as %s as a tab-separated table', async (name, type) => {
    const result = await attachmentsFromFiles([new File(['name\ttotal\n'], name, { type })]);
    expect(result.rejected).toEqual([]);
    expect(result.attachments[0]).toMatchObject({
      filename: name,
      media_type: 'text/tab-separated-values',
    });
  });

  it('offers tables in the document picker', async () => {
    filePicker.pickFiles.mockResolvedValue({ files: [picked('rows.csv', 'text/csv')] });
    const result = await pickDocumentAttachments();
    expect(filePicker.pickFiles).toHaveBeenCalledWith({
      types: expect.arrayContaining(['text/csv', 'text/tab-separated-values']),
      readData: false,
    });
    expect(result.rejected).toEqual([]);
    expect(result.attachments).toHaveLength(1);
  });

  it('leaves a type the platform actually named alone', () => {
    expect(candidateMediaType('notes.txt', 'text/plain')).toBe('text/plain');
    expect(candidateMediaType('rows.csv', 'image/png')).toBe('image/png');
  });
});

// Regression #261: every refusal arrived glued into one unreadable sentence.
describe('what the composer says about a refusal', () => {
  it('gives every refused file its own line, and says nothing when nothing was refused', () => {
    expect(rejectionNotice([])).toBeNull();
    expect(rejectionNotice(['rows.csv: too big', 'notes.exe: not accepted'])).toBe(
      '- rows.csv: too big\n- notes.exe: not accepted',
    );
  });

  it('names the type it refused, or says the type could not be read', async () => {
    const result = await attachmentsFromFiles([
      new File(['MZ'], 'installer.exe', { type: 'application/x-msdownload' }),
      new File([new Uint8Array(4)], 'mystery'),
    ]);
    expect(result.attachments).toEqual([]);
    expect(rejectionNotice(result.rejected)).toBe(
      '- installer.exe: application/x-msdownload is not an accepted attachment format\n' +
        '- mystery: its file type could not be recognised',
    );
  });
});

describe('a drag over the composer', () => {
  it('is claimed only when it carries files', () => {
    const carrying = { types: ['Files', 'text/plain'] } as unknown as DataTransfer;
    const text = { types: ['text/plain'] } as unknown as DataTransfer;
    expect(dragCarriesFiles(carrying)).toBe(true);
    expect(dragCarriesFiles(text)).toBe(false);
    expect(dragCarriesFiles(null)).toBe(false);
  });
});
// Regression: diagnostics shared back to Vis were rejected by paste/file intake.
describe('diagnostics attachments', () => {
  it.each([
    ['vis-diagnostics.jsonl.gz', 'application/gzip'],
    ['vis-diagnostics.jsonl', 'application/x-ndjson'],
  ])('admits %s from the clipboard with an unspecified type', async (name, media) => {
    const result = await attachmentsFromFiles([new File(['log bytes'], name)]);
    expect(result.rejected).toEqual([]);
    expect(result.attachments[0]).toMatchObject({ filename: name, media_type: media });
  });

  it('still obeys the gateway media and size limits', async () => {
    const file = new File(['log bytes'], 'vis-diagnostics.jsonl.gz');
    expect((await attachmentsFromFiles([file], { mediaTypes: ['image/png'] })).attachments).toEqual(
      [],
    );
    expect(
      (await attachmentsFromFiles([file], { mediaTypes: ['application/gzip'], maxFileBytes: 1 }))
        .attachments,
    ).toEqual([]);
  });
});
