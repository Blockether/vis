import { describe, expect, it } from 'vitest';
import { fileVerbs, proposedFilename } from './artifact-save';

describe('file verbs', () => {
  // The last verb is the primary. A phone has no save panel: its share sheet holds Save to Files.
  it.each([
    ['desktop', 'workbook', true, ['Open in app', 'Save…']],
    ['desktop', 'file', true, ['Share', 'Save…']],
    ['desktop', 'file', false, ['Save…']],
    ['desktop', 'archive', true, ['Save…']],
    ['phone', 'workbook', true, ['Open in app']],
    ['phone', 'archive', true, ['Save']],
    ['phone', 'file', true, ['Share']],
    ['web', 'file', true, ['Share', 'Download']],
    ['web', 'archive', true, ['Download']],
  ] as const)('offers a %s %s (share sheet: %s) the verbs %j', (host, kind, canShare, verbs) => {
    expect(fileVerbs(host, kind, canShare)).toEqual(verbs);
  });
});

describe('saved file names', () => {
  it('proposes the artifact name with its spaces and without path separators', () => {
    expect(proposedFilename('Q3 report.pdf')).toBe('Q3 report.pdf');
    expect(proposedFilename('../notes/a:b.txt')).toBe('-notes-a-b.txt');
    expect(proposedFilename('..')).toBe('artifact');
  });
});
