import { beforeEach, describe, expect, it } from 'vitest';

import { readSidebarShown, writeSidebarShown } from './sidebar';

beforeEach(() => {
  localStorage.clear();
});

describe('the sidebar preference', () => {
  it('starts shown and remembers being put away', () => {
    expect(readSidebarShown()).toBe(true);
    writeSidebarShown(false);
    expect(readSidebarShown()).toBe(false);
    writeSidebarShown(true);
    expect(readSidebarShown()).toBe(true);
  });

  it('survives a store holding something else entirely', () => {
    localStorage.setItem('vis.sidebar', 'not a state');
    expect(readSidebarShown()).toBe(true);
  });
});
