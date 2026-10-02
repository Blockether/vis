import { expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import { manifestMetadata } from './github.js';
import { cardsHTML, detailHTML, filters, previewHTML, visibleItems } from './web/render.js';
import fixtures from './web/catalog.fixture.json';

const manifest = readFileSync('examples/vis-greeter/pyproject.toml', 'utf8');
const withKeywords = (keywords) =>
  manifest.replace('[project]', `[project]\nkeywords = ${JSON.stringify(keywords)}`);
const item = { ...fixtures[0], tags: ['browser', 'automation'] };

test.each([[], ['custom-tag'], ['browser', 'automation'], ['x'.repeat(24), '3d']])(
  'authors can define zero to two custom project keywords: %j',
  (...tags) => {
    expect(manifestMetadata(withKeywords(tags)).tags).toEqual(tags);
  },
);
test('keywords are optional for existing releases', () => {
  expect(manifestMetadata(manifest).tags).toEqual([]);
});
test.each([
  ['a string', 'browser'],
  ['too many tags', ['one', 'two', 'three']],
  ['duplicates', ['browser', 'browser']],
  ['an empty tag', ['']],
  ['a long tag', ['x'.repeat(25)]],
  ['uppercase', ['Browser']],
  ['whitespace', ['browser automation']],
  ['leading punctuation', ['-browser']],
  ['trailing punctuation', ['browser-']],
  ['repeated separators', ['browser--automation']],
  ['HTML', ['<img src=x>']],
  ['a non-string', [42]],
])('catalog review rejects %s', (_label, tags) => {
  expect(() => manifestMetadata(withKeywords(tags))).toThrow('project.keywords');
});
test('cards and detail headings replace category and redundant source links with tags', () => {
  const card = JSDOM.fragment(cardsHTML([item], filters())).querySelector('.extension-card');
  expect(card.querySelector('.tag')).toBeNull();
  expect(card.querySelector('.repository-link')).toBeNull();
  expect(card.querySelector('.card-main').nextElementSibling.className).toBe('extension-tags');
  expect(card.querySelector('.extension-tags').nextElementSibling.className).toBe('card-meta');
  const detail = JSDOM.fragment(detailHTML(item));
  const heading = detail.querySelector('.detail-heading');
  expect(heading.querySelector('.tag')).toBeNull();
  expect(heading.querySelector('a')).toBeNull();
  expect(heading.lastElementChild.className).toBe('extension-tags');
  expect(detail.querySelector('#source-link').getAttribute('href')).toBe(item.source_url);
  expect(detail.querySelector('.topics')).toBeNull();
  for (const html of [cardsHTML([item], filters()), detailHTML(item), previewHTML(item)]) {
    const tags = JSDOM.fragment(html).querySelector('[aria-label="Tags"]');
    expect([...tags.children].map((tag) => tag.textContent)).toEqual(item.tags);
  }
});
test('custom tags are searchable without changing category filters', () => {
  expect(visibleItems([item], filters('?q=AUTOMATION'))).toEqual([item]);
  expect(visibleItems([item], filters('?q=automation&category=providers'))).toEqual([]);
});
test('all render paths escape tags, show at most two and omit empty tag lists', () => {
  for (const render of [(item) => cardsHTML([item], filters()), detailHTML, previewHTML]) {
    const html = JSDOM.fragment(
      render({ ...item, tags: ['<script>alert(1)</script>', 'custom', 'third'] }),
    );
    expect([...html.querySelectorAll('.extension-tag')].map((tag) => tag.textContent)).toEqual([
      '<script>alert(1)</script>',
      'custom',
    ]);
    expect(html.querySelector('script')).toBeNull();
    for (const tags of [undefined, [], 'browser']) {
      expect(JSDOM.fragment(render({ ...item, tags })).querySelector('.extension-tags')).toBeNull();
    }
  }
});
test('a custom tag cannot create the official publisher badge', () => {
  const html = JSDOM.fragment(cardsHTML([{ ...item, tags: ['vis-official'] }], filters()));
  expect(html.querySelector('.extension-tag').textContent).toBe('vis-official');
  expect(html.querySelector('.official-badge')).toBeNull();
});
