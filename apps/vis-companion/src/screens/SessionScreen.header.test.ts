import { describe, expect, it } from 'vitest';
import source from './SessionScreen.tsx?raw';

// The session header must tell the operator when a session is parked in a
// DRAFT workspace (an isolated agent clone) and NAME it, while showing nothing
// of the kind on the project itself. SessionScreen is too heavy to mount in a
// unit test (Capacitor, voice, the gateway client), so the header is asserted
// in source — the same pattern the status-bar-padding and artifact-chip tests
// already use for this screen.

const fnStart = source.indexOf('export function SessionScreen({');
if (fnStart < 0) throw new Error('SessionScreen is gone');
const headerEnd = source.indexOf('</header>', fnStart);
const header = source.slice(fnStart, headerEnd);

describe('session header draft indicator', () => {
  it('derives the name from the workspace label of a draft only', () => {
    expect(header).toContain('isDraftWorkspace(session)');
    expect(header).toContain('workspace?.label?.trim()');
  });

  it('shows the draft name beside the connection status, conditionally', () => {
    // Gated behind the name so a session on the project itself renders nothing
    // in that place — not even an empty "draft" label.
    expect(header).toContain('draftName !== ""');
    expect(header).toContain('draft {draftName');
  });
});

// Regression, user report ("big on Ipad we don't need this"): the artifacts sheet
// covered the transcript and left the composer standing under it — a band of chrome
// for a message nobody is writing, and on a tablet the biggest thing on a screen that
// is not about it.
describe('the composer under an open artifacts sheet', () => {
  const composer = source.slice(source.indexOf('<footer', fnStart));

  it('is hidden while the sheet is open, never unmounted', () => {
    expect(composer).toContain('${artifactsOpen ? "hidden" : ""}');
  });
});
