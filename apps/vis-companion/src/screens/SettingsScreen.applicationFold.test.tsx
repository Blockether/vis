// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { SettingsDialog } from './SettingsScreen';

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
it('makes device settings reachable from navigation without a hidden application column', async () => {
  render(<SettingsDialog gateways={[]} onAddMachine={async () => {}} onClose={() => {}} />);
  fireEvent.click(screen.getByRole('button', { name: 'This device' }));
  await screen.findByRole('heading', { name: 'Theme' });
  expect(
    screen.getByRole('switch', { name: 'Show Python code and results: on' }),
  ).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: /Show application settings/ })).toBeNull();
});
it('keeps device preferences distinct from machine and session configuration', async () => {
  render(<SettingsDialog gateways={[]} onAddMachine={async () => {}} onClose={() => {}} />);
  fireEvent.click(screen.getByRole('button', { name: 'This device' }));
  expect(screen.getByText(/Appearance and response display apply immediately/)).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Apply changes' })).toBeNull();
});
