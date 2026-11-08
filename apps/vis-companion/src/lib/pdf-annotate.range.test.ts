import { expect, it } from 'vitest';
import { PDF_RANGE_CHUNK, rangeTask, type PdfRangeSource } from './pdf-annotate';

/** The legacy worker of pdf.js, which runs in this process when the test sets it. */
async function legacyWorker(): Promise<unknown> {
  // @ts-expect-error pdfjs-dist ships no types for its legacy worker module.
  return import('pdfjs-dist/legacy/build/pdf.worker.mjs');
}

/**
 * A valid PDF of `pages` pages with `padding` bytes of an unused stream between them, so
 * that the file is large and the parts that a page needs are small.
 */
function largePdf(pages: number, padding: number): Uint8Array {
  const encoder = new TextEncoder();
  const parts: Uint8Array[] = [];
  const offsets: number[] = [];
  let length = 0;
  const add = (part: string | Uint8Array) => {
    const bytes = typeof part === 'string' ? encoder.encode(part) : part;
    parts.push(bytes);
    length += bytes.length;
  };
  const object = (id: number, body: string | Uint8Array[], dictionary?: string) => {
    offsets[id] = length;
    if (typeof body === 'string') add(`${id} 0 obj\n${body}\nendobj\n`);
    else {
      add(`${id} 0 obj\n${dictionary}\nstream\n`);
      body.forEach(add);
      add('\nendstream\nendobj\n');
    }
  };
  add('%PDF-1.4\n');
  const kids = Array.from({ length: pages }, (_, at) => `${4 + at * 2} 0 R`).join(' ');
  object(1, '<< /Type /Catalog /Pages 2 0 R >>');
  object(2, `<< /Type /Pages /Kids [${kids}] /Count ${pages} >>`);
  object(3, [new Uint8Array(padding).fill(32)], `<< /Length ${padding} >>`);
  for (let at = 0; at < pages; at += 1) {
    const content = `BT /F1 12 Tf 72 720 Td (Page ${at + 1}) Tj ET`;
    object(4 + at * 2, `<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents ${5 + at * 2} 0 R >>`);
    object(5 + at * 2, [encoder.encode(content)], `<< /Length ${content.length} >>`);
  }
  const count = 4 + pages * 2;
  const xref = length;
  add(`xref\n0 ${count}\n0000000000 65535 f \n`);
  for (let id = 1; id < count; id += 1) add(`${String(offsets[id]).padStart(10, '0')} 00000 n \n`);
  add(`trailer\n<< /Size ${count} /Root 1 0 R >>\nstartxref\n${xref}\n%%EOF\n`);
  const file = new Uint8Array(length);
  let at = 0;
  for (const part of parts) {
    file.set(part, at);
    at += part.length;
  }
  return file;
}

// #451: a referenced PDF can be hundreds of megabytes. The reader opens it from the parts
// that pdf.js asks for, not from one download of the whole file.
it('opens a large PDF from a few parts of it', async () => {
  const lib = await import('pdfjs-dist/legacy/build/pdf.mjs');
  (globalThis as { pdfjsWorker?: unknown }).pdfjsWorker = await legacyWorker();
  const file = largePdf(3, 24 * 1024 * 1024);
  const reads: [number, number][] = [];
  const source: PdfRangeSource = {
    length: file.length,
    async read(begin, end) {
      reads.push([begin, end]);
      return file.slice(begin, end).buffer;
    },
  };
  const { task } = await rangeTask(lib as unknown as Parameters<typeof rangeTask>[0], source);
  try {
    const doc = await task.promise;
    expect(doc.numPages).toBe(3);
    const page = await doc.getPage(3);
    expect(page.getViewport({ scale: 1 }).width).toBe(612);
    const read = reads.reduce((sum, [begin, end]) => sum + end - begin, 0);
    expect(reads[0]).toEqual([0, PDF_RANGE_CHUNK]);
    expect(read).toBeLessThan(file.length / 20);
  } finally {
    await task.destroy();
  }
}, 30_000);

it('ends the document when a part cannot be read', async () => {
  const lib = await import('pdfjs-dist/legacy/build/pdf.mjs');
  (globalThis as { pdfjsWorker?: unknown }).pdfjsWorker = await legacyWorker();
  const file = largePdf(1, 4 * 1024 * 1024);
  const source: PdfRangeSource = {
    length: file.length,
    async read(begin, end) {
      if (begin > 0) throw new Error('HTTP 401');
      return file.slice(begin, end).buffer;
    },
  };
  const { task, failed } = await rangeTask(lib as unknown as Parameters<typeof rangeTask>[0], source);
  try {
    await expect(Promise.race([task.promise, failed])).rejects.toThrow('HTTP 401');
  } finally {
    await task.destroy();
  }
}, 30_000);
