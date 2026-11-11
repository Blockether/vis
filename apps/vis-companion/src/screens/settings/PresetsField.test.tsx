// @vitest-environment jsdom
import { cleanup, screen, waitFor, within } from '@testing-library/react';
import userEvent, { PointerEventsCheckLevel } from '@testing-library/user-event';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { renderOpenBands } from '../../test-settings';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { Toggle } from '../../lib/types';
import { MachineSettings } from './MachineSettings';
import fixture from '../../../../../packages/vis-contract/resources/vis-contract/fixtures/setting-presets.json';

const row = fixture.row as unknown as Toggle;
const gateway = { id: 'presets-test', url: 'http://127.0.0.1:7890', token: 'test' };
type Call = { method: string; path: string; body: Record<string, unknown> };
let calls: Call[] = [];
let refusal: string | null = null;

function settingsWith(enabled: boolean, toggle: Toggle = row) {
  return {
    revision: 'presets-1',
    groups: [{
      id: 'sounds',
      title: 'Sounds',
      toggles: [{ id: 'sounds', label: 'Sounds', type: 'boolean' as const, enabled, children: [toggle] }],
    }],
  };
}

beforeEach(() => {
  calls = [];
  refusal = null;
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(settingsWith(true));
  vi.stubGlobal('fetch', vi.fn(async (url: string, init?: RequestInit) => {
    const path = new URL(url).pathname;
    const body = init?.body ? JSON.parse(String(init.body)) : {};
    calls.push({ method: init?.method ?? 'GET', path, body });
    const json = (data: unknown, status = 200) =>
      new Response(JSON.stringify(data), { status, headers: { 'Content-Type': 'application/json' } });
    if (path === fixture.preview_path) return json(fixture.preview_result);
    if (refusal) return json({ error: { message: refusal } }, 400);
    return json({ ...row, value: body.value });
  }));
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

async function open() {
  renderOpenBands(
    <MachineSettings gateway={gateway} speechPrefs={DEFAULT_SPEECH_PREFS} onSpeechChange={async () => DEFAULT_SPEECH_PREFS} />,
  );
  return await screen.findByRole('combobox', { name: row.label });
}
const writes = () => calls.filter((call) => call.path === '/v1/settings' && call.method === 'POST');
const user = () => userEvent.setup({ pointerEventsCheck: PointerEventsCheckLevel.Never });

it('shows a sound file under home with ~', async () => {
  const value = { active: 'My mix', custom: [{ name: 'My mix', slots: { finish: '/Users/ana/Sounds/done.wav' } }] };
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(settingsWith(true, { ...row, value }));
  await open();
  const slots = screen.getByRole('list', { name: 'Sounds of My mix' });
  expect(within(slots).getByText('~/Sounds/done.wav')).toBeVisible();
});

it('shows the active preset, each slot with what it plays, and previews a slot as the contract says', async () => {
  const picker = await open();
  expect(picker).toHaveTextContent('My mix');
  const slots = screen.getByRole('list', { name: 'Sounds of My mix' });
  expect(within(slots).getByText('Finish')).toBeVisible();
  expect(within(slots).getByText('~/Sounds/done.wav')).toBeVisible();
  expect(within(slots).getByText('Glass (built in)')).toBeVisible();
  expect(within(slots).getByText('No sound')).toBeVisible();
  // An empty slot has nothing to play.
  expect(within(slots).queryByRole('button', { name: 'Preview Error' })).toBeNull();

  await user().click(within(slots).getByRole('button', { name: 'Preview Question' }));
  await waitFor(() => expect(calls.some((call) => call.path === fixture.preview_path)).toBe(true));
  const preview = calls.find((call) => call.path === fixture.preview_path)!;
  expect(preview.method).toBe('POST');
  expect(preview.body).toEqual(fixture.preview_request);
  expect(writes()).toHaveLength(0);
});

it('ignores the late answer of a preview after the person previews another slot', async () => {
  let answerFirst: (result: { is_played: boolean }) => void = () => {};
  vi.spyOn(GatewayClient.prototype, 'previewSetting')
    .mockImplementationOnce(() => new Promise((resolve) => { answerFirst = resolve as typeof answerFirst; }))
    .mockResolvedValueOnce({ is_played: true } as never);
  const actor = user();
  await open();
  const slots = screen.getByRole('list', { name: 'Sounds of My mix' });
  await actor.click(within(slots).getByRole('button', { name: 'Preview Finish' }));
  await actor.click(within(slots).getByRole('button', { name: 'Preview Question' }));
  await waitFor(() => expect(within(slots).getByRole('button', { name: 'Preview Question' })).toBeEnabled());
  answerFirst({ is_played: false });
  await new Promise((resolve) => setTimeout(resolve, 0));
  expect(screen.queryByRole('alert')).toBeNull();
});

it('lists built-in presets first and writes the whole value when one is chosen', async () => {
  const actor = user();
  await actor.click(await open());
  const options = within(screen.getByRole('listbox', { name: row.label })).getAllByRole('option');
  expect(options.map((option) => option.textContent)).toEqual([
    ...fixture.row.editor_options.builtin.map((preset) => `${preset.label} (built in)`),
    'My mix',
  ]);
  await actor.click(screen.getByRole('option', { name: 'Soft (built in)' }));
  await waitFor(() => expect(writes()).toHaveLength(1));
  expect(writes()[0].body).toMatchObject({ ...fixture.write, value: { ...fixture.write.value, active: 'soft' } });
});

it('adds a preset from the chosen one, edits it and deletes it back to the first built-in preset', async () => {
  const actor = user();
  await open();
  await actor.click(screen.getByRole('button', { name: /New preset/ }));
  const form = screen.getByRole('form', { name: 'New preset' });
  // A custom preset is copied slot by slot.
  expect(within(form).getByRole('textbox', { name: /Finish/ })).toHaveValue('~/Sounds/done.wav');
  expect(within(form).getByRole('textbox', { name: /Question/ })).toHaveValue('glass');
  expect(within(form).getByText(/Leave a sound empty for silence/)).toBeVisible();
  const name = within(form).getByRole('textbox', { name: 'Name' });
  await actor.clear(name);
  await actor.type(name, 'classic');
  expect(within(form).getByRole('alert')).toHaveTextContent('A built-in preset has this name.');
  await actor.clear(name);
  await actor.type(name, 'Night');
  await actor.click(within(form).getByRole('button', { name: 'Save preset' }));
  await waitFor(() => expect(writes()).toHaveLength(1));
  expect(writes()[0].body.value).toEqual({
    active: 'Night',
    custom: [...fixture.row.value.custom, { name: 'Night', slots: fixture.row.value.custom[0].slots }],
  });

  await waitFor(() => expect(screen.getByRole('combobox', { name: row.label })).toHaveTextContent('Night'));
  await actor.click(screen.getByRole('button', { name: 'Edit Night' }));
  const edit = screen.getByRole('form', { name: 'Edit Night' });
  const error = within(edit).getByRole('textbox', { name: /Error/ });
  await actor.type(error, 'glass');
  await actor.click(within(edit).getByRole('button', { name: 'Save preset' }));
  await waitFor(() => expect(writes()).toHaveLength(2));
  expect(writes()[1].body.value).toEqual({
    active: 'Night',
    custom: [...fixture.row.value.custom, { name: 'Night', slots: { ...fixture.row.value.custom[0].slots, error: 'glass' } }],
  });

  await waitFor(() => expect(screen.getByRole('button', { name: 'Delete Night' })).toBeEnabled());
  await actor.click(screen.getByRole('button', { name: 'Delete Night' }));
  await actor.click(screen.getByRole('button', { name: 'Delete' }));
  await waitFor(() => expect(writes()).toHaveLength(3));
  expect(writes()[2].body.value).toEqual({ active: fixture.row.editor_options.builtin[0].id, custom: fixture.row.value.custom });
});

it('shows the gateway refusal at the presets editor and keeps the form open', async () => {
  const actor = user();
  refusal = `Not saved: ${fixture.invalid_values[4].reason}.`;
  await open();
  await actor.click(screen.getByRole('button', { name: /New preset/ }));
  const form = screen.getByRole('form', { name: 'New preset' });
  const finish = within(form).getByRole('textbox', { name: /Finish/ });
  await actor.clear(finish);
  await actor.type(finish, '~/no-such-file.wav');
  await actor.click(within(form).getByRole('button', { name: 'Save preset' }));
  expect(await screen.findByRole('alert')).toHaveTextContent(refusal);
  expect(screen.getByRole('form', { name: 'New preset' })).toBeVisible();
});

it('hides the presets while the parent switch is off', async () => {
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(settingsWith(false));
  renderOpenBands(
    <MachineSettings gateway={gateway} speechPrefs={DEFAULT_SPEECH_PREFS} onSpeechChange={async () => DEFAULT_SPEECH_PREFS} />,
  );
  await screen.findByRole('switch', { name: 'Sounds: off' });
  expect(screen.queryByRole('combobox', { name: row.label })).toBeNull();
  expect(screen.queryByRole('button', { name: /New preset/ })).toBeNull();
  expect(screen.queryByText(/to change this/)).toBeNull();
});
