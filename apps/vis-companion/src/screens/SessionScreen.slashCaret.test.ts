import { describe, expect, it } from 'vitest';
import source from './SessionScreen.tsx?raw';

// SessionScreen is too heavy to mount in a unit test (Capacitor, voice, the gateway
// client — see SessionScreen.header.test), so the composer's slash-command completion
// is asserted in source, the same pattern that screen's other tests use.
//
// Regression, iOS keyboard on slash-command tap: completing a command set the prompt but
// left the native selection and the `caret` state where the original "/" was — inside the
// freshly written word. The iOS virtual keyboard then sat mid-word, fired autocorrect, and
// inserted at the wrong spot. The fix is to mirror `completeFile`: focus, move the caret to
// the end of the completed text, and keep `caret` in sync.

const start = source.indexOf('function completeSlash(');
if (start < 0) throw new Error('completeSlash is gone');
const end = source.indexOf('function completeFile(', start);
if (end < 0) throw new Error('completeFile is gone');
const completeSlash = source.slice(start, end);

describe('slash-command completion caret', () => {
  it('parks the caret at the end of the completed command (matches completeFile)', () => {
    expect(completeSlash).toContain('setSelectionRange');
    expect(completeSlash).toContain('setCaret');
  });
});

// Regression: the screen fetched a gateway-global slash palette instead of the
// active session's project-relative palette.
describe('session-scoped slash discovery', () => {
  it('passes the active session id to the gateway client', () => {
    expect(source).toContain('.slashes(sid, signal)');
  });
});
