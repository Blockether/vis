import { describe, expect, it } from 'vitest';

import { homeifyPath, workspaceRelativePath } from './path';

describe('path labels', () => {
  it('homeifies Unix and Windows user directories', () => {
    expect(homeifyPath('/Users/ana/code/vis')).toBe('~/code/vis');
    expect(homeifyPath('C:\\Users\\Ana\\code\\vis')).toBe('~/code/vis');
  });

  it('makes paths relative to the longest matching workspace root', () => {
    expect(workspaceRelativePath('/repo/vis/src/a.ts', ['/repo', '/repo/vis'])).toBe('src/a.ts');
  });

  // Regression, T120: the engine abbreviates a tool label's home directory before it
  // ships it, so the shortest path on screen was the only one a workspace root — which
  // arrives absolute — no longer matched, and it printed in full.
  it('matches a root in its home form as well as its absolute one', () => {
    expect(workspaceRelativePath('~/vis/src/a.ts', ['/Users/ana/vis'])).toBe('src/a.ts');
    expect(workspaceRelativePath('/Users/ana/vis/src/a.ts', ['/Users/ana/vis'])).toBe('src/a.ts');
  });
});
