import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const here = dirname(fileURLToPath(import.meta.url));
const source = readFileSync(join(here, 'android-prepare.mjs'), 'utf8');

describe('android device voice catalogue', () => {
  it('carries Android quality and network verdicts into the generated plugin', () => {
    expect(source).toContain('item.put(\"quality\", voice.getQuality())');
    expect(source).toContain(
      'item.put(\"is_network_required\", voice.isNetworkConnectionRequired())',
    );
  });
});
