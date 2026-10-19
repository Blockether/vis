/**
 * Read a ZIP archive in the page, without a dependency or a server round trip.
 *
 * The central directory names every entry. An entry is inflated only when the person opens it.
 * Stored and deflated entries are supported; encrypted entries and other methods are listed,
 * but not opened.
 */

export interface ZipEntry {
  /** The path inside the archive, with `/` separators. */
  name: string;
  /** Uncompressed size in bytes. */
  size: number;
  compressedSize: number;
  /** 0 = stored, 8 = deflated. */
  method: number;
  directory: boolean;
  encrypted: boolean;
  /** Offset of the local file header. */
  offset: number;
}

const EOCD_SIGNATURE = 0x06054b50;
const ZIP64_LOCATOR_SIGNATURE = 0x07064b50;
const ZIP64_EOCD_SIGNATURE = 0x06064b50;
const CENTRAL_SIGNATURE = 0x02014b50;
const LOCAL_SIGNATURE = 0x04034b50;
const MAX_COMMENT = 0xffff;
const ZIP64_MARK = 0xffffffff;

function bigToNumber(value: bigint): number {
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) throw new Error('ZIP archive is too large to read.');
  return Number(value);
}

function findEndOfDirectory(view: DataView): number {
  const last = view.byteLength - 22;
  const first = Math.max(0, last - MAX_COMMENT);
  for (let at = last; at >= first; at -= 1) {
    if (view.getUint32(at, true) === EOCD_SIGNATURE) return at;
  }
  throw new Error('Not a ZIP archive.');
}

function decodeName(bytes: Uint8Array, utf8: boolean): string {
  // Most tools write UTF-8 without the flag; fall back to Latin-1 only for invalid UTF-8.
  try {
    return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
  } catch {
    return utf8 ? new TextDecoder('utf-8').decode(bytes) : new TextDecoder('latin1').decode(bytes);
  }
}

/** Read the entry list of a ZIP archive. */
export function readZipEntries(buffer: ArrayBuffer): ZipEntry[] {
  const view = new DataView(buffer);
  if (view.byteLength < 22) throw new Error('Not a ZIP archive.');
  const end = findEndOfDirectory(view);
  let count = view.getUint16(end + 10, true);
  let directoryOffset = view.getUint32(end + 16, true);
  if ((directoryOffset === ZIP64_MARK || count === 0xffff) && end >= 20) {
    const locator = end - 20;
    if (view.getUint32(locator, true) === ZIP64_LOCATOR_SIGNATURE) {
      const zip64End = bigToNumber(view.getBigUint64(locator + 8, true));
      if (view.getUint32(zip64End, true) !== ZIP64_EOCD_SIGNATURE) throw new Error('Broken ZIP64 archive.');
      count = bigToNumber(view.getBigUint64(zip64End + 32, true));
      directoryOffset = bigToNumber(view.getBigUint64(zip64End + 48, true));
    }
  }
  const entries: ZipEntry[] = [];
  let at = directoryOffset;
  for (let index = 0; index < count; index += 1) {
    if (at + 46 > view.byteLength || view.getUint32(at, true) !== CENTRAL_SIGNATURE) {
      throw new Error('Broken ZIP directory.');
    }
    const flags = view.getUint16(at + 8, true);
    const method = view.getUint16(at + 10, true);
    let compressedSize = view.getUint32(at + 20, true);
    let size = view.getUint32(at + 24, true);
    const nameLength = view.getUint16(at + 28, true);
    const extraLength = view.getUint16(at + 30, true);
    const commentLength = view.getUint16(at + 32, true);
    let offset = view.getUint32(at + 42, true);
    const name = decodeName(new Uint8Array(buffer, at + 46, nameLength), (flags & 0x800) !== 0);
    // ZIP64 extra field: only the values marked 0xffffffff are present, in this order.
    let extra = at + 46 + nameLength;
    const extraEnd = extra + extraLength;
    while (extra + 4 <= extraEnd) {
      const id = view.getUint16(extra, true);
      const length = view.getUint16(extra + 2, true);
      if (id === 0x0001) {
        let field = extra + 4;
        if (size === ZIP64_MARK) {
          size = bigToNumber(view.getBigUint64(field, true));
          field += 8;
        }
        if (compressedSize === ZIP64_MARK) {
          compressedSize = bigToNumber(view.getBigUint64(field, true));
          field += 8;
        }
        if (offset === ZIP64_MARK) offset = bigToNumber(view.getBigUint64(field, true));
      }
      extra += 4 + length;
    }
    entries.push({
      name,
      size,
      compressedSize,
      method,
      directory: name.endsWith('/'),
      encrypted: (flags & 0x1) !== 0,
      offset,
    });
    at = extraEnd + commentLength;
  }
  return entries;
}

/** Whether the page can open this entry. */
export function canExtractZipEntry(entry: ZipEntry): boolean {
  return !entry.directory && !entry.encrypted && (entry.method === 0 || entry.method === 8);
}

/** The original bytes of one entry. */
export async function extractZipEntry(buffer: ArrayBuffer, entry: ZipEntry): Promise<Uint8Array<ArrayBuffer>> {
  if (entry.encrypted) throw new Error('This entry is encrypted.');
  if (entry.directory) throw new Error('This entry is a folder.');
  const view = new DataView(buffer);
  if (view.getUint32(entry.offset, true) !== LOCAL_SIGNATURE) throw new Error('Broken ZIP entry.');
  const nameLength = view.getUint16(entry.offset + 26, true);
  const extraLength = view.getUint16(entry.offset + 28, true);
  const start = entry.offset + 30 + nameLength + extraLength;
  if (start + entry.compressedSize > buffer.byteLength) throw new Error('Broken ZIP entry.');
  const data = new Uint8Array(buffer, start, entry.compressedSize);
  if (entry.method === 0) return data.slice();
  if (entry.method !== 8) throw new Error('This compression method is not supported.');
  const stream = (new Response(data).body as ReadableStream<BufferSource>).pipeThrough(new DecompressionStream('deflate-raw'));
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer());
  if (bytes.byteLength !== entry.size) throw new Error('Broken ZIP entry.');
  return bytes;
}
