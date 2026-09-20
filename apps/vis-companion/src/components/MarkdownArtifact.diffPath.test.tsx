// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { OpenPathContext } from '../lib/open-path';
import { PlainText } from './MarkdownArtifact';

afterEach(cleanup);

const PATCH = [
  'diff --git a/src/app.ts b/src/app.ts',
  '--- a/src/app.ts',
  '+++ b/src/app.ts',
  '@@ -1 +1 @@',
  '-const value = 0;',
  '+const value = 1;',
].join('\n');

function showPatch(openPath: ((path: string) => void) | null) {
  render(
    <OpenPathContext.Provider value={openPath}>
      <PlainText text={PATCH} diff />
    </OpenPathContext.Provider>,
  );
}

it('opens the file a patch header names, and nothing else in the patch', () => {
  const openPath = vi.fn();
  showPatch(openPath);

  const targets = screen.getAllByRole('button', { name: 'Open src/app.ts' });
  // The three header lines name the file; hunk, addition and removal lines do not.
  expect(targets).toHaveLength(3);
  for (const target of targets) expect(target.textContent).toBe('src/app.ts');

  fireEvent.click(targets[2]);
  expect(openPath).toHaveBeenCalledWith('src/app.ts');

  fireEvent.keyDown(targets[2], { key: 'Enter' });
  fireEvent.keyDown(targets[2], { key: ' ' });
  expect(openPath).toHaveBeenCalledTimes(3);
});

it('keeps the marker in front of the path plain words', () => {
  showPatch(vi.fn());

  const line = screen.getAllByRole('button', { name: 'Open src/app.ts' })[2].closest('p')!;
  expect(line.textContent).toBe('+++ b/src/app.ts');
});

it('leaves the path plain when no opener is published', () => {
  showPatch(null);

  expect(screen.queryByRole('button')).toBeNull();
  expect(document.body.textContent).toContain('+++ b/src/app.ts');
});
