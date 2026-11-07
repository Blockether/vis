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

function settingsWith(enabled: boolean) {
  return {
    revision: 'presets-1',
    groups: [{
      id: 'sounds',
      title: 'Sounds',
      toggles: [{ id: 'sounds', label: 'Sounds', type: 'boolean' as const, enabled, children: [row] }],
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

it('shows the active preset, each slot with what it plays, and previews a slot as the contract says', async () => {
  const picker = await open();
  expect(picker).toHaveTextContent('My mix');
  const slots = screen.getByRole('list', { name: 'Sounds of My mix' });
  expect(within(slots).getByText('Finish')).toBeVisible();
  expect(within(slots).getByText('~/Sounds/done.wav')).toBeVisible();
  expect(within(slots).getByText('Glass (built in)')).toBeVisible();
  expect(within(slots).getByText('No sound')).toBeVisible();
  // An empty slot has nothing to play.
  expect(within(slots).getByRole('button', { name: 'Preview Error' })).toBeDisabled();

  await user().click(within(slots).getByRole('button', { name: 'Preview Question' }));
  await waitFor(() => expect(calls.some((call) => call.path === fixture.preview_path)).toBe(true));
  const preview = calls.find((call) => call.path === fixture.preview_path)!;
  expect(preview.method).toBe('POST');
  expect(preview.body).toEqual(fixture.preview_request);
  expect(writes()).toHaveLength(0);
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
  expect(within(form).getByText(/Empty plays no sound/)).toBeVisible();
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

it('locks the presets while the parent switch is off', async () => {
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(settingsWith(false));
  const picker = await open();
  expect(picker).toBeDisabled();
  expect(screen.getByRole('button', { name: /New preset/ })).toBeDisabled();
  expect(screen.getByText('Turn on Sounds to change this.')).toBeVisible();
});
