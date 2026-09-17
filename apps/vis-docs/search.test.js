// @vitest-environment jsdom
import { afterEach, expect, test, vi } from 'vitest';
import { mountSearch } from '../../resources/vis-docs/assets/search.js';

const tick = async () => {
  for (let i = 0; i < 20; i++) await Promise.resolve();
};

const index = () => ({
  pages: [
    {
      title: 'Getting started',
      section: '',
      heading: '',
      href: 'index.html',
      text: 'Install Vis and connect a model provider.',
    },
    {
      title: 'Configuration',
      section: 'Reference',
      heading: 'Router',
      href: 'configuration.html#router',
      text: 'The router picks the provider for each turn.',
    },
    {
      title: 'Configuration',
      section: 'Reference',
      heading: 'Logging',
      href: 'configuration.html#logging',
      text: 'Router decisions are logged to a file.',
    },
  ],
});

const $ = (selector) => document.querySelector(selector);
const input = () => $('.search input');
const results = () => $('.search-results');
const type = (value) => {
  input().value = value;
  input().dispatchEvent(new window.Event('input', { bubbles: true }));
};
const key = (value, target = input()) => {
  const event = new window.KeyboardEvent('keydown', {
    key: value,
    bubbles: true,
    cancelable: true,
  });
  target.dispatchEvent(event);
  return event;
};

let dispose;
function setup(fetchImpl) {
  document.body.innerHTML = `
    <header class="top">
      <search class="search" data-index="assets/search.json">
        <input type="search" placeholder="Search docs" aria-label="Search the documentation"
               autocomplete="off" spellcheck="false">
        <div class="search-results" hidden></div>
      </search>
    </header>`;
  const fetch = fetchImpl || vi.fn(async () => ({ ok: true, json: async () => index() }));
  vi.stubGlobal('fetch', fetch);
  ({ dispose } = mountSearch(document));
  return fetch;
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

test('loads the index on focus and announces the box as a combobox', async () => {
  const fetch = setup();
  expect(input().getAttribute('role')).toBe('combobox');
  expect(results().getAttribute('role')).toBe('listbox');
  expect(input().getAttribute('aria-controls')).toBe(results().id);
  input().focus();
  await tick();
  expect(fetch).toHaveBeenCalledTimes(1);
  expect(fetch.mock.calls[0][0].href).toBe('http://localhost:3000/assets/search.json');
});

test('ranks heading matches above body matches and links the section anchor', async () => {
  setup();
  input().focus();
  await tick();
  type('router');
  const options = results().querySelectorAll('.search-result');
  expect(options.length).toBe(2);
  expect(options[0].getAttribute('href')).toBe('configuration.html#router');
  expect(options[0].querySelector('strong').textContent).toBe('Configuration');
  expect(options[0].querySelector('.search-result__heading').textContent).toBe('Router');
  expect(options[0].querySelector('.search-result__section').textContent).toBe('Reference');
  // The snippet marks the words as the page wrote them, not as the query typed them.
  expect(options[0].querySelector('.search-result__snippet mark').textContent).toBe('router');
  expect(results().hidden).toBe(false);
});

test('requires every term, reports empty results and clears with the query', async () => {
  setup();
  input().focus();
  await tick();
  type('router nothing');
  expect(results().querySelectorAll('.search-result').length).toBe(0);
  expect(results().querySelector('.search-empty').textContent).toBe('No matching sections.');
  expect(results().hidden).toBe(false);
  type('');
  expect(results().hidden).toBe(true);
  expect(input().getAttribute('aria-expanded')).toBe('false');
});

test('walks the results by keyboard and follows the active option on Enter', async () => {
  setup();
  const followed = vi.fn((event) => event.preventDefault());
  document.addEventListener('click', followed);
  try {
    input().focus();
    await tick();
    type('router');
    key('ArrowDown');
    const [first, second] = results().querySelectorAll('.search-result');
    expect(input().getAttribute('aria-activedescendant')).toBe(first.id);
    expect(first.getAttribute('aria-selected')).toBe('true');
    key('ArrowDown');
    expect(input().getAttribute('aria-activedescendant')).toBe(second.id);
    key('ArrowUp');
    expect(input().getAttribute('aria-activedescendant')).toBe(first.id);
    key('Enter');
    expect(followed).toHaveBeenCalledTimes(1);
    expect(followed.mock.calls[0][0].target).toBe(first);
  } finally {
    document.removeEventListener('click', followed);
  }
});

test('closes on Escape and an outside press, and `/` focuses the box outside fields', async () => {
  setup();
  input().focus();
  await tick();
  type('router');
  key('Escape');
  expect(results().hidden).toBe(true);
  type('router');
  document.body.dispatchEvent(new window.MouseEvent('mousedown', { bubbles: true }));
  expect(results().hidden).toBe(true);
  input().blur();

  const slash = key('/', document.body);
  expect(slash.defaultPrevented).toBe(true);
  expect(document.activeElement).toBe(input());

  const field = document.createElement('textarea');
  document.body.append(field);
  field.focus();
  expect(key('/', field).defaultPrevented).toBe(false);
  expect(document.activeElement).toBe(field);
});

test('disables the box when the index cannot load', async () => {
  setup(vi.fn(async () => ({ ok: false, json: async () => ({}) })));
  input().focus();
  await tick();
  expect(input().disabled).toBe(true);
  expect(input().placeholder).toBe('Search unavailable');
});

test('dispose removes the listeners and semantics it installed', async () => {
  setup();
  input().focus();
  await tick();
  dispose();
  dispose = undefined;
  expect(input().getAttribute('role')).toBeNull();
  input().blur();
  const slash = key('/', document.body);
  expect(slash.defaultPrevented).toBe(false);
  expect(document.activeElement).toBe(document.body);
});
