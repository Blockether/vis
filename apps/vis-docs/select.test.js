// @vitest-environment jsdom
import { afterEach, expect, test, vi } from 'vitest';
import { mountSelects } from '../../resources/vis-docs/assets/select.js';

let controls;
const field = () => document.querySelector('select');
const trigger = () => document.querySelector('[role=combobox]');
const menu = () => document.querySelector('[role=listbox]');
const key = (value) => {
  const event = new window.KeyboardEvent('keydown', {
    key: value,
    bubbles: true,
    cancelable: true,
  });
  trigger().dispatchEvent(event);
  return event;
};
function setup(content = '') {
  document.body.innerHTML =
    content ||
    `<form><label for="choice">Install for</label><select id="choice" name="scope" aria-describedby="help"><option value="project" selected>Project</option><option value="disabled" disabled>Preview</option><option value="global">Global</option><option value="">Off</option></select><p id="help">Choose an installation scope</p><button type="submit">Continue</button></form>`;
  controls = mountSelects(document);
}
afterEach(() => {
  controls?.dispose();
  controls = undefined;
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

test('enhances native fields without losing labels, descriptions, form values or no-script markup', () => {
  setup();
  expect(field().hidden).toBe(true);
  expect(field().id).toBe('choice');
  expect(trigger().type).toBe('button');
  expect(trigger().getAttribute('aria-label')).toBe('Install for');
  expect(trigger().getAttribute('aria-describedby')).toBe('help');
  expect(trigger().textContent).toBe('Project');
  document.querySelector('label').click();
  expect(document.activeElement).toBe(trigger());
  controls.dispose();
  expect(field().hidden).toBe(false);
  expect(document.querySelector('.vis-select')).toBeNull();
  expect(new window.FormData(document.querySelector('form')).get('scope')).toBe('project');
});

test('click commits once through native input/change events and does not submit', () => {
  setup();
  const change = vi.fn(),
    input = vi.fn(),
    submit = vi.fn();
  field().addEventListener('change', change);
  document.querySelector('form').addEventListener('input', input);
  document.querySelector('form').addEventListener('submit', submit);
  trigger().click();
  expect(menu().getAttribute('aria-label')).toBe('Install for');
  expect(document.querySelector('[aria-selected=true]').textContent).toBe('Project');
  document.querySelectorAll('[role=option]')[2].click();
  expect(field().value).toBe('global');
  expect(trigger().textContent).toBe('Global');
  expect(document.activeElement).toBe(trigger());
  expect(menu()).toBeNull();
  expect(change).toHaveBeenCalledTimes(1);
  expect(input).toHaveBeenCalledTimes(1);
  expect(submit).not.toHaveBeenCalled();
  trigger().click();
  document.querySelectorAll('[role=option]')[2].click();
  expect(change).toHaveBeenCalledTimes(1);
});

test('arrows explore without changing the saved value, skip unavailable options, and commit empty choices', () => {
  setup();
  key('ArrowDown');
  key('ArrowDown');
  expect(document.getElementById(trigger().getAttribute('aria-activedescendant')).textContent).toBe(
    'Global',
  );
  expect(field().value).toBe('project');
  expect(document.querySelector('[aria-selected=true]').textContent).toBe('Project');
  key('ArrowDown');
  expect(document.getElementById(trigger().getAttribute('aria-activedescendant')).textContent).toBe(
    'Off',
  );
  key('Enter');
  expect(field().value).toBe('');
  expect(trigger().textContent).toBe('Off');
  key('Home');
  key(' ');
  expect(field().value).toBe('project');
  key('End');
  key('Enter');
  expect(field().value).toBe('');
});

test('typeahead finds labels, ignores disabled matches and supports repeated-character cycling', () => {
  setup(
    `<label>Backend<select><option>Automatic</option><option disabled>Another</option><option>Archive</option><option>Global</option></select></label>`,
  );
  key('a');
  expect(document.getElementById(trigger().getAttribute('aria-activedescendant')).textContent).toBe(
    'Archive',
  );
  key('a');
  expect(document.getElementById(trigger().getAttribute('aria-activedescendant')).textContent).toBe(
    'Automatic',
  );
  key('Escape');
  key('g');
  key('l');
  key('Enter');
  expect(field().value).toBe('Global');
  expect(trigger().getAttribute('aria-label')).toBe('Backend');
});

test('Escape cancels inside a parent dialog; outside press cancels without selecting', () => {
  setup(
    `<dialog open><label for="choice">Backend</label><select id="choice"><option>Automatic</option><option>Global</option></select></dialog><button>Outside</button>`,
  );
  const parentKey = vi.fn();
  document.querySelector('dialog').addEventListener('keydown', parentKey);
  trigger().click();
  expect(menu().parentElement).toBe(document.querySelector('dialog'));
  key('End');
  expect(key('Escape').defaultPrevented).toBe(true);
  expect(parentKey).not.toHaveBeenCalled();
  expect(field().value).toBe('Automatic');
  expect(document.querySelector('dialog').open).toBe(true);
  trigger().click();
  key('End');
  document.body.dispatchEvent(new window.Event('pointerdown', { bubbles: true }));
  expect(menu()).toBeNull();
  expect(field().value).toBe('Automatic');
});

test('Tab commits and closes without preventing normal focus traversal', () => {
  setup();
  key('End');
  expect(key('Tab').defaultPrevented).toBe(false);
  expect(menu()).toBeNull();
  expect(field().value).toBe('');
});

test('Tab also closes when every option is unavailable', () => {
  setup('<label>Release<select><option disabled selected>Unavailable</option></select></label>');
  trigger().click();
  expect(menu()).not.toBeNull();
  expect(key('Tab').defaultPrevented).toBe(false);
  expect(menu()).toBeNull();
});

test('refresh follows external values/options and closes disabled or removed controls', () => {
  setup();
  field().value = 'global';
  controls.refresh();
  expect(trigger().textContent).toBe('Global');
  trigger().click();
  field().disabled = true;
  controls.refresh();
  expect(trigger().disabled).toBe(true);
  expect(menu()).toBeNull();
  field().disabled = false;
  field().replaceChildren();
  controls.refresh();
  expect(trigger().textContent).toBe('No options available');
  expect(trigger().disabled).toBe(true);
  field().append(new window.Option('Again', 'again'));
  controls.refresh();
  expect(trigger().disabled).toBe(false);
  trigger().click();
  document.querySelector('form').remove();
  controls.refresh();
  expect(menu()).toBeNull();
});

test('reset restores the native default and refresh is idempotent without duplicate listeners', async () => {
  setup();
  controls.refresh();
  controls.refresh();
  expect(document.querySelectorAll('.vis-select')).toHaveLength(1);
  trigger().click();
  document.querySelectorAll('[role=option]')[2].click();
  document.querySelector('form').reset();
  await Promise.resolve();
  expect(trigger().textContent).toBe('Project');
  trigger().click();
  controls.dispose();
  expect(menu()).toBeNull();
  expect(field().hidden).toBe(false);
  expect(document.querySelector('.vis-select')).toBeNull();
});

test('leaves multiple selections, listboxes and intentionally hidden fields native', () => {
  setup(
    '<select multiple><option>A</option></select><select size="3"><option>B</option></select><select hidden><option>C</option></select>',
  );
  expect(trigger()).toBeNull();
  expect(document.querySelectorAll('select')).toHaveLength(3);
});

test('disabled optgroups cannot be selected or reached through keyboard navigation', () => {
  setup(
    '<label>Backend<select><option>A</option><optgroup label="Unavailable" disabled><option>B</option></optgroup><option>C</option></select></label>',
  );
  trigger().click();
  document.querySelectorAll('[role=option]')[1].click();
  expect(field().value).toBe('A');
  expect(menu()).not.toBeNull();
  key('ArrowDown');
  key('ArrowDown');
  key('Enter');
  expect(field().value).toBe('C');
});

test('hidden routes close their menu without stealing focus from the new heading', () => {
  setup();
  trigger().click();
  const heading = document.createElement('h1');
  heading.tabIndex = -1;
  document.body.append(heading);
  document.querySelector('form').hidden = true;
  heading.focus();
  controls.refresh();
  expect(menu()).toBeNull();
  expect(document.activeElement).toBe(heading);
});

test('separate mounts keep unique listbox and active-option relationships', () => {
  document.body.innerHTML =
    '<section><label>A<select><option>One</option></select></label></section><section><label>B<select><option>Two</option></select></label></section>';
  const [first, second] = document.querySelectorAll('section');
  const a = mountSelects(first),
    b = mountSelects(second);
  controls = {
    dispose() {
      a.dispose();
      b.dispose();
    },
  };
  first.querySelector('button').click();
  second.querySelector('button').click();
  expect(
    new Set([...document.querySelectorAll('[role=listbox]')].map((node) => node.id)).size,
  ).toBe(2);
});

test('wide release fields keep a matching menu width within the viewport', () => {
  setup();
  vi.spyOn(trigger(), 'getBoundingClientRect').mockReturnValue({
    width: 480,
    left: 32,
    top: 100,
    bottom: 144,
  });
  trigger().click();
  expect(menu().style.width).toBe('480px');
  expect(menu().style.maxWidth).toBe(`min(max(24rem, 480px), ${window.innerWidth - 24}px)`);
});
