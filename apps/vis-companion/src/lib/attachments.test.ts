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
import { beforeEach, describe, expect, it, vi } from 'vitest';

const filePicker = vi.hoisted(() => ({
  pickFiles: vi.fn(),
  pickMedia: vi.fn(),
  pickImages: vi.fn(),
}));

vi.mock('@capawesome/capacitor-file-picker', () => ({ FilePicker: filePicker }));
vi.mock('@capacitor/core', () => ({
  Capacitor: { isNativePlatform: () => true },
}));

import {
  attachmentsFromFiles,
  candidateMediaType,
  isAudioMediaType,
  pickDocumentAttachments,
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
      readData: true,
    });
    expect(result.rejected).toEqual([]);
    expect(result.attachments.map((a) => a.media_type)).toEqual(['audio/mp4']);
    expect(result.attachments[0].filename).toBe('memo.m4a');
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

  it('refuses what this gateway never said it takes', async () => {
    filePicker.pickFiles.mockResolvedValue({
      files: [picked('archive.zip', 'application/zip')],
    });

    const result = await pickDocumentAttachments({ mediaTypes: ['audio/mp4'] });

    expect(result.attachments).toEqual([]);
    expect(result.rejected).toEqual(['archive.zip: unsupported media format']);
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
      files: [
        { name: 'Recording.m4a', mimeType: 'application/octet-stream', data: 'AAAAAAAA' },
      ],
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
    expect(candidateMediaType('blob', 'application/octet-stream')).toBe(
      'application/octet-stream',
    );
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
