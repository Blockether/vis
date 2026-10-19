/** Test harness: a minimal ZIP writer for the reader and the archive view. */

export type Bytes = Uint8Array<ArrayBuffer>;
export type ZipSource = { name: string; data: Bytes; deflate?: boolean; flags?: number };

async function deflateRaw(data: Bytes): Promise<Bytes> {
  const stream = (new Response(data).body as ReadableStream<BufferSource>).pipeThrough(new CompressionStream('deflate-raw'));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

/** A minimal ZIP writer for the reader's tests; CRC values stay 0 because the reader ignores them. */
/** Stored entries only, built synchronously for story fixtures. */
export function storedZip(sources: ZipSource[]): Blob {
  return assembleZip(
    sources.map((source) => ({ ...source, deflate: false })),
    sources.map((source) => source.data),
    false,
  );
}

/** Stored or deflated entries, optionally with ZIP64 records. */
export async function buildZip(sources: ZipSource[], zip64 = false): Promise<ArrayBuffer> {
  const bodies = await Promise.all(
    sources.map((source) => (source.deflate ? deflateRaw(source.data) : source.data)),
  );
  return assembleZip(sources, bodies, zip64).arrayBuffer();
}

function assembleZip(sources: ZipSource[], bodies: Bytes[], zip64: boolean): Blob {
  const encoder = new TextEncoder();
  const locals: Bytes[] = [];
  const centrals: Bytes[] = [];
  let offset = 0;
  sources.forEach((source, index) => {
    const name = encoder.encode(source.name);
    const body = bodies[index];
    const local = new Uint8Array(30 + name.length + body.length);
    const lv = new DataView(local.buffer);
    lv.setUint32(0, 0x04034b50, true);
    lv.setUint16(6, source.flags ?? 0x800, true);
    lv.setUint16(8, source.deflate ? 8 : 0, true);
    lv.setUint32(18, body.length, true);
    lv.setUint32(22, source.data.length, true);
    lv.setUint16(26, name.length, true);
    local.set(name, 30);
    local.set(body, 30 + name.length);
    const extraLength = zip64 ? 4 + 24 : 0;
    const central = new Uint8Array(46 + name.length + extraLength);
    const cv = new DataView(central.buffer);
    cv.setUint32(0, 0x02014b50, true);
    cv.setUint16(8, source.flags ?? 0x800, true);
    cv.setUint16(10, source.deflate ? 8 : 0, true);
    cv.setUint32(20, zip64 ? 0xffffffff : body.length, true);
    cv.setUint32(24, zip64 ? 0xffffffff : source.data.length, true);
    cv.setUint16(28, name.length, true);
    cv.setUint16(30, extraLength, true);
    cv.setUint32(42, zip64 ? 0xffffffff : offset, true);
    central.set(name, 46);
    if (zip64) {
      const at = 46 + name.length;
      cv.setUint16(at, 0x0001, true);
      cv.setUint16(at + 2, 24, true);
      cv.setBigUint64(at + 4, BigInt(source.data.length), true);
      cv.setBigUint64(at + 12, BigInt(body.length), true);
      cv.setBigUint64(at + 20, BigInt(offset), true);
    }
    locals.push(local);
    centrals.push(central);
    offset += local.length;
  });
  const directorySize = centrals.reduce((sum, part) => sum + part.length, 0);
  const tail: Bytes[] = [];
  if (zip64) {
    const record = new Uint8Array(56);
    const rv = new DataView(record.buffer);
    rv.setUint32(0, 0x06064b50, true);
    rv.setBigUint64(32, BigInt(sources.length), true);
    rv.setBigUint64(40, BigInt(directorySize), true);
    rv.setBigUint64(48, BigInt(offset), true);
    const locator = new Uint8Array(20);
    const xv = new DataView(locator.buffer);
    xv.setUint32(0, 0x07064b50, true);
    xv.setBigUint64(8, BigInt(offset + directorySize), true);
    tail.push(record, locator);
  }
  const end = new Uint8Array(22 + 7);
  const ev = new DataView(end.buffer);
  ev.setUint32(0, 0x06054b50, true);
  ev.setUint16(10, zip64 ? 0xffff : sources.length, true);
  ev.setUint32(12, directorySize, true);
  ev.setUint32(16, zip64 ? 0xffffffff : offset, true);
  ev.setUint16(20, 7, true);
  end.set(encoder.encode('comment'), 22);
  return new Blob([...locals, ...centrals, ...tail, end]);
}
