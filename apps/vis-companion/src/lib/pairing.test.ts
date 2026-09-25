import { describe, expect, it } from 'vitest';

import { parsePairing } from './pairing';

describe('parsePairing', () => {
  it('reads the vis:// link that the gateway QR encodes', () => {
    expect(
      parsePairing(
        'vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&alt=http%3A%2F%2F10.0.0.5%3A7890&token=abc',
      ),
    ).toEqual({
      url: 'http://100.64.0.10:7890',
      token: 'abc',
      label: '100.64.0.10:7890',
      alts: ['http://10.0.0.5:7890'],
    });
  });

  it('accepts a bare gateway URL', () => {
    expect(parsePairing('  http://10.0.0.5:7890  ')).toEqual({
      url: 'http://10.0.0.5:7890',
      label: '10.0.0.5:7890',
    });
  });

  it('rejects anything else, including the JSON payload no gateway produces', () => {
    expect(parsePairing('{"type":"vis-gateway-pairing","url":"http://10.0.0.5:7890"}')).toBeNull();
    expect(parsePairing('not a gateway')).toBeNull();
  });
});
