// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient, GatewayError } from '../lib/gateway';
import type { Session } from '../lib/types';
import { useNotices } from '../lib/use-notices';
import { NoticeToast } from './NoticeToast';
import { useSessionImport } from './SessionImport';

const target = { root: '/Users/dev/shop', groupId: 'group-1' };
const snapshot = new File([new Uint8Array([86, 73, 83, 83, 78, 65, 80])], 'shop.vissnap');

/** One screen that offers the import: a control, the file field and the toast. */
function Screen({ client, onImported }: { client: GatewayClient; onImported: (session: Session) => void }) {
  const notices = useNotices();
  const sessionImport = useSessionImport(notices);
  return (
    <>
      <button type="button" onClick={() => sessionImport.open(client, target, onImported)}>
        Import
      </button>
      {sessionImport.input}
      <NoticeToast notices={notices} />
    </>
  );
}

const pick = (file: File) => {
  fireEvent.click(screen.getByRole('button', { name: 'Import' }));
  fireEvent.change(screen.getByLabelText('Session file to import'), { target: { files: [file] } });
};

afterEach(() => vi.unstubAllGlobals());

describe('the session import of a screen', () => {
  it('sends the picked file to its target, says what was imported and gives the new session', async () => {
    const made = { id: 'new-1', title: 'Shop work' } as Session;
    const importSession = vi.fn(async () => made);
    const onImported = vi.fn();
    render(<Screen client={{ importSession } as unknown as GatewayClient} onImported={onImported} />);
    pick(snapshot);

    await waitFor(() => expect(screen.getByRole('status')).toHaveTextContent('Imported Shop work as a new session.'));
    expect(importSession).toHaveBeenCalledExactlyOnceWith(snapshot, target);
    expect(onImported).toHaveBeenCalledExactlyOnceWith(made);
  });

  it('can pick and import an ATIF file', async () => {
    const atif = new File(['{"schema_version":"ATIF-v1.8","steps":[]}'], 'shop.json', { type: 'application/json' });
    const made = { id: 'new-1', title: 'Shop work' } as Session;
    const fetch = vi.fn(async (_url: string, _init: RequestInit) => Response.json(made, { status: 201 }));
    vi.stubGlobal('fetch', fetch);
    const client = new GatewayClient({ url: 'http://localhost:7331' });
    const onImported = vi.fn();
    render(<Screen client={client} onImported={onImported} />);
    fireEvent.click(screen.getByRole('button', { name: 'Import' }));
    await userEvent.upload(screen.getByLabelText(/file to import/i), atif);

    await waitFor(() => expect(onImported).toHaveBeenCalledExactlyOnceWith(made));
    expect(fetch).toHaveBeenCalledOnce();
    const [url, init] = fetch.mock.calls[0]!;
    expect(new URL(url).pathname).toBe('/v1/sessions/actions/import');
    expect(Object.fromEntries(new URL(url).searchParams)).toEqual({ root: target.root, group_id: target.groupId });
    expect(init.method).toBe('POST');
    expect(init.body).toBe(atif);
  });

  it('says why the gateway refused a file, and opens no session', async () => {
    const importSession = vi.fn(async () => {
      throw new GatewayError(400, 'This file is not a Vis session snapshot');
    });
    const onImported = vi.fn();
    render(<Screen client={{ importSession } as unknown as GatewayClient} onImported={onImported} />);
    pick(new File(['notes'], 'notes.txt'));

    await waitFor(() =>
      expect(screen.getByRole('status')).toHaveTextContent('The import failed: This file is not a Vis session snapshot'),
    );
    expect(onImported).not.toHaveBeenCalled();
  });
});
