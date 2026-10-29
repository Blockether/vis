import { beforeEach, describe, expect, it } from 'vitest';

import { readSidebarShown } from './sidebar';

beforeEach(() => {
  localStorage.clear();
});

describe('the sidebar preference', () => {

  it('survives a store holding something else entirely', () => {
    localStorage.setItem('vis.sidebar', 'not a state');
    expect(readSidebarShown()).toBe(true);
  });
});
