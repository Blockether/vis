import { describe, expect, it } from 'vitest';

import { searchRanges, searchSegments, searchTerms } from './search-highlight';

const marked = (text: string, query: string) =>
  searchSegments(text, searchTerms(query))
    .filter((segment) => segment.isMatch)
    .map((segment) => segment.text);

// The highlight follows the gateway's full-text rules: words without case or accents,
// each query word matched on its own at the start of a word in the text.
describe('search highlight', () => {
  it('splits a query into distinct folded words, longest first', () => {
    expect(searchTerms('  Sessions, SESS café-Cafe ')).toEqual(['sessions', 'sess', 'cafe']);
    expect(searchTerms('?! ')).toEqual([]);
  });

  it('marks each word where a word in the text starts with it, without case or accents', () => {
    expect(searchSegments('Café sessions, not obsessions', searchTerms('CAFE sess'))).toEqual([
      { text: 'Café', isMatch: true },
      { text: ' ', isMatch: false },
      { text: 'sess', isMatch: true },
      { text: 'ions, not obsessions', isMatch: false },
    ]);
    expect(marked('Zażółć gęślą jaźń', 'gesla JAZN')).toEqual(['gęślą', 'jaźń']);
  });

  it('reads query punctuation as a break between words, never as a pattern', () => {
    expect(searchTerms('win(dows)+')).toEqual(['dows', 'win']);
    expect(marked('**win(dows)+** and WIN(DOWS)+, not windows', 'win(dows)+')).toEqual([
      'win',
      'dows',
      'WIN',
      'DOWS',
      'win',
    ]);
    expect(marked('a.b', '.*')).toEqual([]);
  });

  it('merges words that overlap into one range on the original text', () => {
    expect(searchRanges('Sessions', searchTerms('sess session'))).toEqual([[0, 7]]);
    expect(searchSegments('🙂 Windows', searchTerms('windows'))).toEqual([
      { text: '🙂 ', isMatch: false },
      { text: 'Windows', isMatch: true },
    ]);
  });

  it('marks nothing for an empty query or an empty text', () => {
    expect(searchSegments('Windows', searchTerms('  '))).toEqual([
      { text: 'Windows', isMatch: false },
    ]);
    expect(searchSegments('', searchTerms('windows'))).toEqual([]);
  });
});
