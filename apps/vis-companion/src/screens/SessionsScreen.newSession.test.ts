import { describe, expect, it } from 'vitest';
import source from './SessionsScreen.tsx?raw';

// Regression (reported: "the new session is button so frequently used that we should
// take it from the ⋯ and put on every machine header before the ⋯, as a yellow
// button"): starting a session — the one thing this screen exists for — was the FIRST
// ROW of the `⋯` menu, so the most frequent verb in the app cost a tap, a popover and a
// read of three other options before it could be pressed.
//
// SessionsScreen is far too heavy to mount in a unit test (Capacitor, the gateway
// client, the fleet poller), so placement is asserted in source — the same pattern
// `SessionScreen.header.test.ts` already uses. The button itself is rendered and
// checked in `components/ui.test.tsx`.

const headerAt = source.lastIndexOf('<NewSessionButton');
const guardAt = source.lastIndexOf('{!machine.error && (', headerAt);

describe('where "New session" lives', () => {
  it('is a button, on the fleet bar and on every machine header', () => {
    expect(source.match(/<NewSessionButton/g)?.length).toBe(2);
  });

  it('sits immediately BEFORE the ⋯ it was taken out of', () => {
    for (const after of source.split('<NewSessionButton').slice(1)) {
      const kebab = after.indexOf('<MachineKebab');
      expect(kebab).toBeGreaterThan(-1);
      // Nothing between them: the two verbs of one machine are one control group.
      expect(after.slice(0, kebab)).not.toContain('</div>');
    }
  });

  it('is no longer a row of the menu, which keeps the rarer verbs', () => {
    expect(source).not.toMatch(/title="New session"/);
    expect(source).toMatch(/title="New session in a draft/);
    expect(source).toMatch(/title="Switch project/);
  });

  it('starts on THAT machine, in the project it is already in, with no question', () => {
    expect(source).toContain('function startOnMachine(machine: FleetMachine, anchor: HTMLElement)');
    expect(source).toContain("void createSession({ kind: 'trunk' }, machine.conn, root.path)");
  });

  it('falls through to the folder browser when the machine has never run one', () => {
    const fn = source.slice(source.indexOf('function startOnMachine'));
    expect(fn.slice(0, fn.indexOf('\n  }'))).toContain("'browse'");
  });

  it('offers no verb at all on a machine that is not answering', () => {
    expect(guardAt).toBeGreaterThan(-1);
    expect(source.slice(guardAt, headerAt)).not.toContain(')}');
    expect(source).toContain('!!barMachine.error');
  });
});
