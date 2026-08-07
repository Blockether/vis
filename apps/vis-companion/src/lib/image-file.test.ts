import { describe, expect, it } from 'vitest';
import { editedFilename, fileBaseName, sheetDismissed } from './image-file';

// A picture's NAME is the only thing that says what it is a picture of, so it is
// derived from the original rather than invented.
describe('capture filenames', () => {
  it('always ends up with a usable png name', () => {
    expect(fileBaseName('')).toBe('document');
    expect(fileBaseName('.hidden')).toBe('document');
    expect(fileBaseName('a/b\\c.html')).toBe('a-b-c');
    expect(editedFilename('../../etc/passwd')).toBe('etc-passwd.png');
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
