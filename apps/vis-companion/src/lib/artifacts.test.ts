import { describe, expect, it } from 'vitest';
import {
  ARTIFACT_FILTERS,
  artifactKind,
  artifactMedia,
  artifactTotalLabel,
  attachmentBytes,
  attachmentIsDoc,
  attachmentIsImage,
  attachmentIsPlayable,
  attachmentIsVideo,
  collectArtifacts,
  docKindLabel,
  isDocMedia,
  pageBySize,
  RAIL_PAGE,
  SHEET_PAGE,
  isPdfMedia,
} from './artifacts';
import type { TranscriptTurn } from './types';

describe('document media types', () => {
  it('recognises the three types the engine keeps off the wire', () => {
    expect(isDocMedia('application/pdf')).toBe(true);
    expect(isDocMedia('text/html')).toBe(true);
    expect(isDocMedia('application/xhtml+xml')).toBe(true);
    expect(isDocMedia('TEXT/HTML; charset=utf-8')).toBe(true);
    expect(isDocMedia('image/png')).toBe(false);
    expect(isDocMedia('text/csv')).toBe(false);
    expect(isDocMedia(undefined)).toBe(false);
  });

  it('labels the kind the way the fence summary does', () => {
    expect(docKindLabel('application/pdf')).toBe('PDF');
    expect(docKindLabel('text/html')).toBe('HTML');
    expect(isPdfMedia('application/pdf')).toBe(true);
    expect(isPdfMedia('text/html')).toBe(false);
  });
});

describe('attachment classification', () => {
  it('reads the media type first and the kind only as a fallback', () => {
    expect(attachmentIsImage({ index: 0, media_type: 'image/png' })).toBe(true);
    expect(attachmentIsImage({ index: 0, kind: 'image' })).toBe(true);
    expect(attachmentIsImage({ index: 0, media_type: 'text/csv' })).toBe(false);
    expect(attachmentIsVideo({ index: 0, media_type: 'video/mp4' })).toBe(true);
    expect(attachmentIsDoc({ index: 0, media_type: 'application/pdf' })).toBe(
      true,
    );
    expect(attachmentIsDoc({ index: 0, kind: 'doc' })).toBe(true);
  });

  it('puts a still and a clip on the same rail and nothing else', () => {
    expect(attachmentIsPlayable({ index: 0, media_type: 'image/png' })).toBe(
      true,
    );
    expect(attachmentIsPlayable({ index: 0, media_type: 'video/mp4' })).toBe(
      true,
    );
    expect(
      attachmentIsPlayable({ index: 0, media_type: 'application/pdf' }),
    ).toBe(false);
  });

  it('sorts every attachment into exactly one kind', () => {
    expect(artifactKind({ index: 0, media_type: 'image/png' })).toBe('image');
    expect(artifactKind({ index: 0, media_type: 'video/mp4' })).toBe('video');
    expect(artifactKind({ index: 0, media_type: 'text/html' })).toBe('doc');
    expect(artifactKind({ index: 0, media_type: 'text/csv' })).toBe('file');
    expect(artifactKind({ index: 0 })).toBe('file');
  });

  it('names the format from the filename, then the media type', () => {
    expect(artifactMedia({ index: 0, filename: 'chart.png' })).toBe('PNG');
    expect(
      artifactMedia({
        index: 0,
        filename: 'coverage.json',
        media_type: 'text/plain',
      }),
    ).toBe('JSON');
    expect(
      artifactMedia({
        index: 0,
        filename: 'report',
        media_type: 'application/pdf',
      }),
    ).toBe('PDF');
    expect(
      artifactMedia({ index: 0, media_type: 'application/xhtml+xml' }),
    ).toBe('XML');
    expect(artifactMedia({ index: 0 })).toBe('FILE');
  });

  it('renders a size only when the gateway declared one', () => {
    expect(attachmentBytes(512)).toBe('512B');
    expect(attachmentBytes(2048)).toBe('2.0KB');
    expect(attachmentBytes(3 * 1024 * 1024)).toBe('3.0MB');
    expect(attachmentBytes(undefined)).toBe('');
    expect(attachmentBytes(-1)).toBe('');
  });
});

const turns: TranscriptTurn[] = [
  {
    id: 't1',
    iterations: [
      {
        id: 'i1',
        tool_name: 'python_execution',
        attachments: [
          {
            index: 0,
            iteration_id: 'i1',
            filename: 'revenue.png',
            media_type: 'image/png',
            size: 2048,
          },
          {
            index: 1,
            iteration_id: 'i1',
            filename: 'notes.csv',
            media_type: 'text/csv',
            size: 1024,
          },
        ],
      },
    ],
  },
  {
    id: 't2',
    iterations: [
      { id: 'i2', tool_name: 'shell' },
      {
        id: 'i3',
        tool_name: 'shell',
        attachments: [
          {
            index: 0,
            iteration_id: 'i3',
            filename: 'report.pdf',
            media_type: 'application/pdf',
          },
        ],
      },
    ],
  },
];

describe('collecting what a session produced', () => {
  it('flattens turn → iteration → attachment, newest first', () => {
    const list = collectArtifacts(turns);
    expect(list.map((entry) => entry.name)).toEqual([
      'report.pdf',
      'notes.csv',
      'revenue.png',
    ]);
    expect(list.map((entry) => entry.kind)).toEqual(['doc', 'file', 'image']);
  });

  it('counts the turn from the start of the session, not of the window', () => {
    expect(collectArtifacts(turns).map((entry) => entry.turn)).toEqual([
      2, 1, 1,
    ]);
    // 40 turns live before the page we hold: the newest one is turn 42.
    expect(collectArtifacts(turns, 40).map((entry) => entry.turn)).toEqual([
      42, 41, 41,
    ]);
  });

  it('carries the provenance a tile has to announce', () => {
    const [doc, , image] = collectArtifacts(turns);
    expect(doc.tool).toBe('shell');
    expect(doc.iterationId).toBe('i3');
    expect(doc.sizeLabel).toBe('');
    expect(image.key).toBe('i1:0');
    expect(image.tool).toBe('python_execution');
    expect(image.sizeLabel).toBe('2.0KB');
  });

  it('adds up only the sizes it was told, and says nothing otherwise', () => {
    expect(artifactTotalLabel(collectArtifacts(turns))).toBe('3.0KB');
    expect(artifactTotalLabel([])).toBe('');
    expect(artifactTotalLabel(collectArtifacts([turns[1]]))).toBe('');
  });

  it('has a filter for every kind an artifact can be', () => {
    const covered = new Set(
      ARTIFACT_FILTERS.filter((filter) => filter.label !== 'All').flatMap(
        (filter) => filter.kinds,
      ),
    );
    expect([...covered].sort()).toEqual(['doc', 'file', 'image', 'video']);
    expect(ARTIFACT_FILTERS[0].kinds).toEqual([
      'image',
      'video',
      'doc',
      'file',
    ]);
  });
});

describe('pageBySize', () => {
  const MB = 1024 * 1024;
  const page = (sizes: (number | undefined)[], pages = 1, limits = RAIL_PAGE) =>
    pageBySize(
      sizes.map((size, at) => ({ name: `a${at}.png`, size })),
      (entry) => entry.size,
      pages,
      limits,
    );

  it('hides nothing when a gallery already fits', () => {
    const shown = page([1024, 1024, 1024]);
    expect(shown.shown).toHaveLength(3);
    expect(shown.rest).toEqual([]);
    expect(shown.restLabel).toBe('');
  });

  it('stops at the COUNT bound and says what is left, with its weight', () => {
    const shown = page(Array.from({ length: 20 }, () => 1024));
    expect(shown.shown).toHaveLength(RAIL_PAGE.items);
    expect(shown.rest).toHaveLength(14);
    expect(shown.restBytes).toBe(14 * 1024);
    expect(shown.restLabel).toBe('14 more · 14.0KB');
  });

  it('stops at the BYTE bound long before the count one', () => {
    // Six 3 MB screenshots are not six thumbnails: two fit an 8 MB page.
    const shown = page([3 * MB, 3 * MB, 3 * MB, 3 * MB]);
    expect(shown.shown).toHaveLength(2);
    expect(shown.rest).toHaveLength(2);
    expect(shown.restBytes).toBe(6 * MB);
  });

  it('always shows the first artifact, however heavy it is', () => {
    // A budget that can hide the ONLY picture there is would be a broken
    // screen, not a thrifty one.
    const shown = page([64 * MB, 1024]);
    expect(shown.shown).toHaveLength(1);
    expect(shown.rest).toHaveLength(1);
  });

  it('falls back to the count bound when no size is known', () => {
    const shown = page(Array.from({ length: 9 }, () => undefined));
    expect(shown.shown).toHaveLength(RAIL_PAGE.items);
    expect(shown.restBytes).toBe(0);
    // Nothing weighed: claiming "3 more · 0B" would be a lie.
    expect(shown.restLabel).toBe('3 more');
  });

  it('buys exactly one more page of BOTH bounds per reveal', () => {
    const sizes = Array.from({ length: 20 }, () => 1024);
    expect(page(sizes, 2).shown).toHaveLength(2 * RAIL_PAGE.items);
    expect(page([3 * MB, 3 * MB, 3 * MB, 3 * MB], 2).shown).toHaveLength(4);
  });

  it('gives the thumbnail grid a bigger page than the transcript rail', () => {
    expect(SHEET_PAGE.items).toBeGreaterThan(RAIL_PAGE.items);
    expect(SHEET_PAGE.bytes).toBeGreaterThan(RAIL_PAGE.bytes);
  });
});
