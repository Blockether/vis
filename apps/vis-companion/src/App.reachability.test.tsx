// @vitest-environment jsdom
// Regression, user report ("the app keeps dropping me, I get Reconnecting over
// and over"): every resume re-entered the full address sweep, and the sweep
// asked EVERY address the app had ever saved. A phone that keeps a machine's
// old tailnet address therefore spent each wake waiting on an address that had
// not answered in fourteen hours, while the live stream reopened behind it.
import { waitFor } from '@testing-library/react';
import { beforeEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { forgetUnreachableAddresses } from './lib/reachability';

/** This network, answering. */
const HERE = 'http://192.168.0.116:7890';
/** The same machine's tailnet address, which nothing answers on today. */
const GONE = 'http://100.123.200.103:7890';

beforeEach(() => {
  forgetUnreachableAddresses();
});

describe('the addresses a resumed app asks about', () => {
  it('stops asking an address that answers nothing', async () => {
    const view = renderApp({
      machines: [{ label: 'tower', url: HERE, alts: [GONE] }],
      unreachable: [GONE],
    });
    const probes = () => view.requests.filter((href) => href.startsWith(GONE)).length;
    try {
      // The app does try it once — an address is only silent after it has been
      // asked.
      await waitFor(() => {
        expect(probes()).toBe(1);
      });

      for (let resume = 0; resume < 3; resume += 1) {
        window.dispatchEvent(new Event('pagehide'));
        window.dispatchEvent(new Event('pageshow'));
        await new Promise((resolve) => setTimeout(resolve, 300));
      }

      expect(probes()).toBe(1);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
