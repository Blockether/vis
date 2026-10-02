import { expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import { manifestMetadata } from './github.js';
import {
  cardsHTML,
  detailHTML,
  filters,
  filterURL,
  previewHTML,
  shellHTML,
  visibleItems,
} from './web/render.js';
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
test('custom tags are searchable and can be combined with exact tag filters', () => {
  expect(visibleItems([item], filters('?q=AUTOMATION'))).toEqual([item]);
  expect(visibleItems([item], filters('?q=automation&tag=other'))).toEqual([]);
});
test('all render paths escape tags, show at most two and omit empty tag lists', () => {
  for (const render of [(item) => cardsHTML([item], filters()), detailHTML, previewHTML]) {
    const html = JSDOM.fragment(
      render({
        ...item,
        tags: ['<script>alert(1)</script>', 'custom', 'third'],
      }),
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

const official = {
  ...item,
  id: 'official',
  repository: 'Blockether/vis-lang-python',
  repository_url: 'https://github.com/Blockether/vis-lang-python',
  subdirectory: '',
  name: 'vis-lang-python',
  description: 'Python editor tools',
  tags: ['python', 'development'],
};
const community = {
  ...item,
  id: 'community',
  description: 'Community editor tools',
  tags: ['python', 'python'],
  official: true,
  is_official: true,
};
const catalog = [
  official,
  community,
  { ...item, tags: ['automation', 'vis-official'] },
  { ...item, tags: undefined },
];

test('sidebar puts official publishing and counted custom tags above legacy documentation navigation', () => {
  const sidebar = JSDOM.fragment(shellHTML({ items: catalog })).querySelector(
    '#catalog-navigation',
  );
  const count = (selector) => sidebar.querySelector(`${selector} .count`).textContent;
  expect(count('[data-catalog-filter="all"]')).toBe('4');
  expect(count('[data-catalog-filter="official"]')).toBe('1');
  expect(count('[data-tag="python"]')).toBe('2');
  expect(count('[data-tag="vis-official"]')).toBe('1');
  expect([...sidebar.querySelectorAll('[data-tag]')].map((link) => link.dataset.tag)).toEqual([
    'automation',
    'development',
    'python',
    'vis-official',
  ]);
  expect(sidebar.querySelectorAll('[data-catalog-filter]')[1].textContent).toBe('Official1');
  expect(sidebar.querySelector('[data-category],.tagline,[aria-label="Documentation"]')).toBeNull();
  expect(sidebar.textContent).not.toMatch(
    /Tools|Providers|Workflows|Writing extensions|Getting started/,
  );
});

test('tag and official filters combine and cannot be granted by metadata flags or tag text', () => {
  expect(visibleItems(catalog, filters('?tag=python'))).toEqual([official, community]);
  expect(visibleItems(catalog, filters('?official=1&tag=python'))).toEqual([official]);
  expect(visibleItems(catalog, filters('?official=1&tag=vis-official'))).toEqual([]);
  expect(visibleItems(catalog, filters('?tag=py'))).toEqual([]);
  expect(visibleItems(catalog, filters('?tag=python&q=community'))).toEqual([community]);
});

test('filter links preserve search and sorting, count matching extensions and toggle independently', () => {
  const sidebar = JSDOM.fragment(
    shellHTML({
      items: catalog,
      search: '?q=editor&tag=python&official=1&sort=name',
    }),
  );
  const link = (selector) => sidebar.querySelector(selector);
  const state = (selector) =>
    filters(new URL(link(selector).getAttribute('href'), 'https://center.example.com').search);
  expect(link('[data-catalog-filter="all"] .count').textContent).toBe('2');
  expect(link('[data-catalog-filter="official"] .count').textContent).toBe('1');
  expect(link('[data-tag="python"] .count').textContent).toBe('1');
  expect(link('[data-tag="automation"] .count').textContent).toBe('0');
  expect(link('[data-catalog-filter="official"]').getAttribute('aria-current')).toBe('page');
  expect(link('[data-tag="python"]').getAttribute('aria-current')).toBe('page');
  expect(state('[data-catalog-filter="all"]')).toEqual({
    q: 'editor',
    tag: '',
    official: false,
    sort: 'name',
  });
  expect(state('[data-catalog-filter="official"]')).toEqual({
    q: 'editor',
    tag: 'python',
    official: false,
    sort: 'name',
  });
  expect(state('[data-tag="python"]')).toEqual({
    q: 'editor',
    tag: '',
    official: true,
    sort: 'name',
  });
  expect(state('[data-tag="development"]')).toEqual({
    q: 'editor',
    tag: 'development',
    official: true,
    sort: 'name',
  });
});

test('filter URLs contain only supported fields and ignore obsolete category and view parameters', () => {
  const state = filters('?tag=python&official=1&q=editor&sort=name&category=tools&view=list');
  expect(state).toEqual({
    q: 'editor',
    tag: 'python',
    official: true,
    sort: 'name',
  });
  expect(filters(filterURL(state).split('?')[1])).toEqual(state);
  expect(filterURL({ ...state, category: 'tools', view: 'list' })).not.toMatch(/category|view/);
  expect(filters('?tag=%3Cscript%3E&official=true&sort=invalid')).toEqual(filters());
  expect(filters('?tag=' + 'a'.repeat(25)).tag).toBe('');
});

test('empty and untagged catalogs keep primary counters without an empty tag section', () => {
  for (const items of [[], [{ ...item, tags: undefined }]]) {
    const sidebar = JSDOM.fragment(shellHTML({ items })).querySelector('#catalog-navigation');
    expect(sidebar.querySelector('[aria-label="Extension tags"]')).toBeNull();
    expect(sidebar.querySelector('[data-catalog-filter="all"] .count').textContent).toBe(
      String(items.length),
    );
    expect(sidebar.querySelector('[data-catalog-filter="official"] .count').textContent).toBe('0');
  }
});
