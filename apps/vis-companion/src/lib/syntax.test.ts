import { describe, expect, it } from 'vitest';
import { languageOfPath } from './syntax';

describe('languageOfPath', () => {
  it('colours ES module and CommonJS files', () => {
    expect(['a.mjs', 'a.cjs', 'a.mts', 'a.cts'].map(languageOfPath)).toEqual([
      'javascript',
      'javascript',
      'typescript',
      'typescript',
    ]);
  });
});
