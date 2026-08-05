import { describe, expect, it } from 'vitest';
import {
  editedFilename,
  fileBaseName,
  pageCaptureFilename,
  sheetDismissed,
  viewCaptureFilename,
} from './image-file';

// The model never receives the PDF or the HTML page itself, so a capture's NAME
// is the only thing that says what it is a picture of. `report-p3.png` answers
// "which page did the human draw on"; `capture.png` would not.
describe('capture filenames', () => {
  it('carries the page number of a PDF page', () => {
    expect(pageCaptureFilename('report.pdf', 3)).toBe('report-p3.png');
    expect(pageCaptureFilename('Q3 report.pdf', 12)).toBe('Q3-report-p12.png');
  });

  it('never invents a page zero', () => {
    expect(pageCaptureFilename('report.pdf', 0)).toBe('report-p1.png');
    expect(pageCaptureFilename('report.pdf', -4)).toBe('report-p1.png');
    expect(pageCaptureFilename('report.pdf', 2.7)).toBe('report-p2.png');
  });

  it('says capture, not page, for an artifact that has no pages', () => {
    expect(viewCaptureFilename('page.html')).toBe('page-capture.png');
  });

  it('always ends up with a usable png name', () => {
    expect(fileBaseName('')).toBe('document');
    expect(fileBaseName('.hidden')).toBe('document');
    expect(fileBaseName('a/b\\c.html')).toBe('a-b-c');
    expect(viewCaptureFilename('../../etc/passwd')).toBe(
      'etc-passwd-capture.png',
    );
  });

  // One helper, two callers: a shared picture and a captured page cannot drift
  // into two different ideas of a safe name.
  it('names an edited picture after the one it came from, always as a png', () => {
    expect(editedFilename('holiday.jpeg')).toBe('holiday.png');
    expect(editedFilename('')).toBe('vis-image.png');
    expect(editedFilename('../weird name!.png')).toBe('weird-name.png');
  });
});

// Every system sheet reports "the human changed their mind" as a throw. Showing
// that as an error is how a cancelled photo turned into a red failure line.
describe('dismissed system sheets', () => {
  it('reads a cancellation as a decision, not a failure', () => {
    expect(sheetDismissed(new Error('User cancelled photos app'))).toBe(true);
    expect(sheetDismissed(new Error('Share dismissed'))).toBe(true);
    expect(sheetDismissed('AbortError')).toBe(true);
  });

  it('leaves a real failure alone', () => {
    expect(sheetDismissed(new Error('Network request failed'))).toBe(false);
  });
});
