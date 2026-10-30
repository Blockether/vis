// @vitest-environment jsdom
// Regression, user report ("opening a session still feels very slow"): the first
// session that a launch opened stood 330 ms on an empty sheet. `lazy` suspended
// although the warm-up had loaded the chunk, and React held that fallback.
import { act, render, screen } from '@testing-library/react';
import { Component, type ReactNode } from 'react';
import { describe, expect, it } from 'vitest';

import { splitScreen } from './split-screen';

function Ready({ name }: { name: string }) {
  return <p>{name}</p>;
}

/** Let the pending chunk promises settle. */
const settle = () => act(async () => await new Promise((resolve) => setTimeout(resolve, 0)));

class Boundary extends Component<{ children: ReactNode }, { failed: boolean }> {
  state = { failed: false };
  static getDerivedStateFromError() {
    return { failed: true };
  }
  render() {
    return this.state.failed ? <p>chunk failed</p> : this.props.children;
  }
}

describe('splitScreen', () => {
  it('mounts a warmed screen in its first commit, without the fallback', async () => {
    const shown: string[] = [];
    function Fallback() {
      shown.push('fallback');
      return null;
    }
    const split = splitScreen(async () => Ready, <Fallback />);
    split.warm();
    await settle();

    render(<split.Screen name="session" />);

    expect(screen.getByText('session')).toBeTruthy();
    expect(shown).toEqual([]);
  });

  it('asks again for a chunk that failed at a mount', async () => {
    let attempts = 0;
    const split = splitScreen(async () => {
      attempts += 1;
      if (attempts === 1) throw new Error('chunk gone');
      return Ready;
    }, null);
    const first = render(
      <Boundary>
        <split.Screen name="session" />
      </Boundary>,
    );
    await settle();
    expect(screen.getByText('chunk failed')).toBeTruthy();
    first.unmount();

    render(
      <Boundary>
        <split.Screen name="session" />
      </Boundary>,
    );
    await settle();

    expect(screen.getByText('session')).toBeTruthy();
  });
});
