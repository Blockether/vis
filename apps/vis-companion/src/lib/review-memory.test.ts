// @vitest-environment jsdom
// A review view choice, such as hidden test files, must survive a reload of the page.
import { act, renderHook } from '@testing-library/react';
import { createElement, type ReactNode } from 'react';
import { beforeEach, describe, expect, it, vi } from 'vitest';

async function reloaded(session: string) {
  vi.resetModules();
  const memory = await import('./review-memory');
  const wrapper = ({ children }: { children: ReactNode }) =>
    createElement(
      memory.ReviewMemoryContext,
      { value: JSON.stringify(['gw', session, `view-${Math.random()}`]) },
      createElement(memory.ReviewPrefsContext, { value: JSON.stringify(['gw', session, 'review-1']) }, children),
    );
  return renderHook(() => memory.useReviewPref('diff:hideTests', false), { wrapper });
}

describe('review view choices', () => {
  beforeEach(() => localStorage.clear());

  it('keep hidden test files for the same session after a reload', async () => {
    const first = await reloaded('s1');
    act(() => first.result.current[1](true));
    first.unmount();

    expect((await reloaded('s1')).result.current[0]).toBe(true);
    expect((await reloaded('s2')).result.current[0]).toBe(false);
  });
});
