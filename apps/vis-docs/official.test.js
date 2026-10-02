import { expect, test } from 'vitest';
import { JSDOM } from 'jsdom';
import { isOfficialExtension } from './web/discovery.js';
import { cardsHTML, detailHTML, filters, previewHTML } from './web/render.js';
import fixtures from './web/catalog.fixture.json';

const officialSources = [
  ['Blockether/spel', 'extensions/vis-spel', 'vis-spel'],
  ['Blockether/vis-decisions', '', 'vis-decisions'],
  ['Blockether/vis-lang-clojure', 'extension', 'vis-lang-clojure'],
  ['Blockether/vis-lang-interface', '', 'vis-lang-interface'],
  ['Blockether/vis-lang-python', '', 'vis-lang-python'],
];
const official = {
  ...fixtures[0],
  repository: 'Blockether/vis-lang-clojure',
  repository_url: 'https://github.com/Blockether/vis-lang-clojure',
  subdirectory: 'extension',
  name: 'vis-lang-clojure',
};

test.each(officialSources)(
  'the maintained package %s/%s (%s) is official',
  (repository, subdirectory, name) => {
    const item = {
      ...official,
      repository,
      repository_url: 'https://github.com/' + repository,
      subdirectory,
      name,
    };
    expect(isOfficialExtension(item)).toBe(true);
    expect(
      isOfficialExtension({
        ...item,
        repository: repository.toUpperCase(),
        repository_url: item.repository_url.toUpperCase(),
        version: '9.0.0rc1',
        official: false,
        is_official: false,
      }),
    ).toBe(true);
  },
);

test.each([
  [
    'a fork with the same package name',
    {
      repository: 'example/vis-lang-clojure',
      repository_url: 'https://github.com/example/vis-lang-clojure',
    },
  ],
  [
    'an unlisted first-party repository',
    {
      repository: 'Blockether/another-extension',
      repository_url: 'https://github.com/Blockether/another-extension',
    },
  ],
  ['a different package in the same repository', { name: 'another-extension' }],
  ['a different repository label', { repository: 'example/vis-lang-clojure' }],
  ['the repository root instead of its extension', { subdirectory: '' }],
  ['a differently cased directory', { subdirectory: 'Extension' }],
  ['a nested directory', { subdirectory: 'extension/other' }],
  ['a traversal path', { subdirectory: 'extension/../extension' }],
  ['an insecure URL', { repository_url: 'http://github.com/Blockether/vis-lang-clojure' }],
  [
    'a lookalike host',
    { repository_url: 'https://github.com.example.com/Blockether/vis-lang-clojure' },
  ],
  [
    'URL user information',
    { repository_url: 'https://github.com@example.com/Blockether/vis-lang-clojure' },
  ],
  [
    'a suffixed repository',
    { repository_url: 'https://github.com/Blockether/vis-lang-clojure-fork' },
  ],
  [
    'an unverified URL with a query',
    { repository_url: official.repository_url + '?official=true' },
  ],
  ['a missing repository URL', { repository_url: undefined }],
  ['a missing directory', { subdirectory: undefined }],
  ['a missing package name', { name: undefined }],
])('%s cannot claim official status', (_label, changes) => {
  const item = {
    ...official,
    ...changes,
    owner: 'Blockether',
    official: true,
    is_official: true,
    verified: true,
  };
  expect(isOfficialExtension(item)).toBe(false);
  for (const html of [cardsHTML([item], filters()), detailHTML(item), previewHTML(item)]) {
    expect(JSDOM.fragment(html).querySelector('.official-badge')).toBeNull();
  }
});

test('the same accessible badge appears on cards, detail pages and verified previews', () => {
  for (const html of [
    cardsHTML([official], filters()),
    detailHTML(official),
    previewHTML(official),
  ]) {
    const fragment = JSDOM.fragment(html);
    expect(fragment.querySelectorAll('.official-badge')).toHaveLength(1);
    const badge = fragment.querySelector('.official-badge');
    expect(badge.textContent).toBe('Official');
    expect(badge.title).toBe('Published and maintained by the Vis team');
    expect(badge.querySelector('svg').getAttribute('aria-hidden')).toBe('true');
  }
  expect(
    JSDOM.fragment(detailHTML(official)).querySelector('.security-note').textContent,
  ).toContain('--trust allows extension code and build backends to run with your permissions.');
});

test('official badges sit beside the extension title in every view', () => {
  for (const html of [
    cardsHTML([official], filters()),
    detailHTML(official),
    previewHTML(official),
  ]) {
    const fragment = JSDOM.fragment(html);
    const badge = fragment.querySelector('.extension-title > .official-badge');
    expect(badge).not.toBeNull();
    expect(badge.previousElementSibling.matches('h1, h3')).toBe(true);
  }
});

test('cards show the repository name and badge, then owner and version, then stars', () => {
  const card = JSDOM.fragment(cardsHTML([{ ...official, stars: 1234 }], filters())).querySelector(
    '.extension-card',
  );
  expect(card.dataset.name).toBe('blockether/vis-lang-clojure');
  expect(card.querySelector('.card-main .extension-title > h3').textContent).toBe(
    'vis-lang-clojure',
  );
  expect(card.querySelector('.card-main .card-publisher').textContent).toBe(
    `blockether · v${official.version}`,
  );
  expect(card.querySelector('.card-top, .version')).toBeNull();
  const stars = card.querySelector('.card-meta > .card-stars');
  expect(stars.textContent).toBe('1.2K stars');
  expect(stars.querySelector('svg').getAttribute('aria-hidden')).toBe('true');
  expect(stars.querySelector('.sr-only').textContent).toBe(' stars');
});
