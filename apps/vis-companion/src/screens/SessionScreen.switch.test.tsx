// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { screen } from '@testing-library/react';

import { renderSessionScreen } from './session-screen-harness';

// Every switch between sessions mounts this screen anew, so its first frame is what
// the reader sees on each switch. Anything that fades in there, or corrects itself a
// frame later, reads as a flicker.

const connectedHub = {
  subscribeConnection: (on: (live: boolean) => void) => {
    on(true);
    return () => {};
  },
};

describe('a session screen opened by a switch', () => {
  it('never paints "Reconnecting" over a connected hub', () => {
    const changes: MutationRecord[] = [];
    const watch = new MutationObserver((records) => changes.push(...records));
    watch.observe(document.body, {
      subtree: true,
      childList: true,
      characterData: true,
      characterDataOldValue: true,
    });
    renderSessionScreen({ subscriptions: connectedHub });
    changes.push(...watch.takeRecords());
    watch.disconnect();
    const replaced = changes.flatMap((record) => [
      record.oldValue ?? '',
      ...Array.from(record.removedNodes, (node) => node.textContent ?? ''),
    ]);
    expect(replaced.join('\n')).not.toContain('Reconnecting');
    expect(screen.getByText('Connected')).toBeInTheDocument();
  });
});
