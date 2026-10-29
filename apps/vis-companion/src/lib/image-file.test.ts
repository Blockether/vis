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
});

// Every system sheet reports "the human changed their mind" as a throw. Showing
// that as an error is how a cancelled photo turned into a red failure line.
describe('dismissed system sheets', () => {
  it('reads a cancellation as a decision, not a failure', () => {
    expect(sheetDismissed(new Error('User cancelled photos app'))).toBe(true);
    expect(sheetDismissed(new Error('Share dismissed'))).toBe(true);
    expect(sheetDismissed('AbortError')).toBe(true);
  });
});
