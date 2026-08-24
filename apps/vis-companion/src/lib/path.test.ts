import { describe, expect, it } from 'vitest';

import { compactProjectPath, homeifyPath, workspaceRelativePath } from './path';

describe('path labels', () => {
  it('homeifies Unix and Windows user directories', () => {
    expect(homeifyPath('/Users/ana/code/vis')).toBe('~/code/vis');
    expect(homeifyPath('C:\\Users\\Ana\\code\\vis')).toBe('~/code/vis');
  });

  it('makes paths relative to the longest matching workspace root', () => {
    expect(workspaceRelativePath('/repo/vis/src/a.ts', ['/repo', '/repo/vis'])).toBe('src/a.ts');
  });
});

describe('compactProjectPath', () => {
  it('does not repeat the project name already printed above it', () => {
    expect(compactProjectPath('/Users/ana/rewrite/uberworkspace', 'uberworkspace')).toBe('~/rewrite');
  });

  it('keeps the useful suffix of a deep parent path', () => {
    expect(compactProjectPath('/srv/work/clients/acme/uberworkspace', 'uberworkspace')).toBe(
      '/…/clients/acme',
    );
  });

  it('keeps the full root when stripping its only segment would erase context', () => {
    expect(compactProjectPath('/uberworkspace', 'uberworkspace')).toBe('/uberworkspace');
  });
});
