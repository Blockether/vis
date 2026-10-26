// @vitest-environment jsdom
import { cleanup, fireEvent, screen, waitFor, within } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { Toggle } from '../../lib/types';
import { MachineSettings } from './MachineSettings';

const gateway = { id: 'secret-settings', url: 'http://127.0.0.1:7890', token: 'test' };
// The gateway row of a secret: it says only whether a value is saved.
const unset: Toggle = {
  id: 'graphify_kb_token',
  label: 'Knowledge base token',
  type: 'string',
  editor: 'secret',
  is_set: false,
  max_length: 4096,
  source: 'default',
  scope: 'global',
  scopes: ['global'],
  is_override: false,
};
const saved: Toggle = { ...unset, is_set: true, source: 'global', is_override: true };
let row: Toggle = unset;
beforeEach(() => {
  row = unset;
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockImplementation(async () => ({
    revision: 'secret-1',
    groups: [{ id: 'extension:graphify', title: 'graphify', toggles: [row] }],
  }));
  vi.stubGlobal('fetch', vi.fn(async () => new Response('{}', { headers: { 'Content-Type': 'application/json' } })));
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});
function open() {
  renderOpenBands(
    <MachineSettings gateway={gateway} speechPrefs={DEFAULT_SPEECH_PREFS} onSpeechChange={async () => DEFAULT_SPEECH_PREFS} />,
  );
  return screen.findByText('Knowledge base token');
}

it('sets a secret through a masked field and keeps no typed text after Save', async () => {
  const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockResolvedValue(saved);
  await open();
  expect(screen.getByText('Not set')).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Remove' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Set' }));
  const field = screen.getByLabelText<HTMLInputElement>('Knowledge base token');
  expect(field).toHaveAttribute('type', 'password');
  expect(field).toHaveAttribute('autocomplete', 'off');
  expect(field).toHaveAttribute('spellcheck', 'false');
  expect(field).toHaveAttribute('maxlength', '4096');
  expect(field).toHaveValue('');
  expect(screen.getByRole('button', { name: 'Save' })).toBeDisabled();
  fireEvent.change(field, { target: { value: '   ' } });
  expect(screen.getByRole('button', { name: 'Save' })).toBeDisabled();
  fireEvent.change(field, { target: { value: 'tok-123' } });
  fireEvent.submit(field.closest('form')!);
  expect(save).toHaveBeenCalledExactlyOnceWith('graphify_kb_token', 'value', 'tok-123');
  await screen.findByText('Saved');
  expect(screen.queryByLabelText('Knowledge base token')).toBeNull();
  // Focus stays in the row, not on the page body.
  await waitFor(() => expect(screen.getByRole('button', { name: 'Replace' })).toHaveFocus());
  expect(document.body.innerHTML).not.toContain('tok-123');
  fireEvent.click(screen.getByRole('button', { name: 'Replace' }));
  expect(screen.getByLabelText('Knowledge base token')).toHaveValue('');
});

it('cancels a typed secret and shows a save error without the value', async () => {
  vi.spyOn(GatewayClient.prototype, 'setSetting').mockRejectedValue(
    new GatewayError(400, 'value tok-456 is too long'),
  );
  await open();
  fireEvent.click(screen.getByRole('button', { name: 'Set' }));
  fireEvent.change(screen.getByLabelText('Knowledge base token'), { target: { value: 'draft' } });
  fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  expect(screen.queryByLabelText('Knowledge base token')).toBeNull();
  expect(screen.getByRole('button', { name: 'Set' })).toHaveFocus();
  fireEvent.click(screen.getByRole('button', { name: 'Set' }));
  const field = screen.getByLabelText('Knowledge base token');
  expect(field).toHaveValue('');
  fireEvent.change(field, { target: { value: 'tok-456' } });
  fireEvent.submit(field.closest('form')!);
  await screen.findByText('value ••• is too long');
  // The panel banner can be scrolled away, so the row reports the failure too.
  expect(screen.getByRole('alert')).toHaveTextContent('Not saved. Enter the value again.');
  expect(document.body.innerHTML).not.toContain('tok-456');
  expect(screen.getByLabelText('Knowledge base token')).toHaveValue('');
  await waitFor(() => expect(screen.getByLabelText('Knowledge base token')).toHaveFocus());
});

it('removes a saved machine secret only after confirmation', async () => {
  row = saved;
  const write = vi.spyOn(GatewayClient.prototype, 'setSetting').mockResolvedValue(unset);
  await open();
  expect(screen.getByText('Saved')).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Remove' }));
  const question = screen.getByRole('group', { name: 'Remove Knowledge base token?' });
  fireEvent.click(within(question).getByRole('button', { name: 'Keep' }));
  expect(write).not.toHaveBeenCalled();
  expect(screen.getByRole('button', { name: 'Remove' })).toHaveFocus();
  fireEvent.click(screen.getByRole('button', { name: 'Remove' }));
  fireEvent.click(
    within(screen.getByRole('group', { name: 'Remove Knowledge base token?' })).getByRole('button', { name: 'Remove' }),
  );
  await waitFor(() => expect(write).toHaveBeenCalledExactlyOnceWith('graphify_kb_token', 'inherit'));
  await screen.findByText('Not set');
  expect(screen.queryByRole('button', { name: 'Remove' })).toBeNull();
  await waitFor(() => expect(screen.getByRole('button', { name: 'Set' })).toHaveFocus());
});
