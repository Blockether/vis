import { describe, expect, it } from 'vitest';
import { canExtractZipEntry, extractZipEntry, readZipEntries } from './zip';
import { buildZip } from './zip-harness';

const text = (value: string) => new TextEncoder().encode(value);

describe('ZIP reader', () => {
  it('lists entries and opens stored and deflated files', async () => {
    const report = 'quarterly numbers\n'.repeat(40);
    const buffer = await buildZip([
      { name: 'docs/', data: new Uint8Array() },
      { name: 'docs/readme.txt', data: text('hello') },
      { name: 'docs/report.md', data: text(report), deflate: true },
      { name: 'zażółć.txt', data: text('ok') },
    ]);
    const entries = readZipEntries(buffer);
    expect(entries.map((entry) => [entry.name, entry.size, entry.directory])).toEqual([
      ['docs/', 0, true],
      ['docs/readme.txt', 5, false],
      ['docs/report.md', report.length, false],
      ['zażółć.txt', 2, false],
    ]);
    expect(canExtractZipEntry(entries[0])).toBe(false);
    expect(new TextDecoder().decode(await extractZipEntry(buffer, entries[1]))).toBe('hello');
    expect(entries[2].compressedSize).toBeLessThan(entries[2].size);
    expect(new TextDecoder().decode(await extractZipEntry(buffer, entries[2]))).toBe(report);
  });

  it('reads ZIP64 sizes and offsets', async () => {
    const buffer = await buildZip(
      [
        { name: 'a.txt', data: text('first') },
        { name: 'b.txt', data: text('second'), deflate: true },
      ],
      true,
    );
    const entries = readZipEntries(buffer);
    expect(entries.map((entry) => [entry.name, entry.size])).toEqual([
      ['a.txt', 5],
      ['b.txt', 6],
    ]);
    expect(new TextDecoder().decode(await extractZipEntry(buffer, entries[1]))).toBe('second');
  });

  it('lists encrypted entries but does not open them', async () => {
    const buffer = await buildZip([{ name: 'secret.txt', data: text('x'), flags: 0x801 }]);
    const [entry] = readZipEntries(buffer);
    expect(entry.encrypted).toBe(true);
    expect(canExtractZipEntry(entry)).toBe(false);
    await expect(extractZipEntry(buffer, entry)).rejects.toThrow('encrypted');
  });

  it('refuses bytes that are not a ZIP archive', () => {
    expect(() => readZipEntries(new TextEncoder().encode('plain text, long enough to scan').buffer)).toThrow(
      'Not a ZIP archive.',
    );
  });
});
