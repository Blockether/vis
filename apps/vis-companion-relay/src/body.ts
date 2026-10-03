/** A request body read with a byte cap, shared by every route of this relay. */

/** A body big enough to cost CPU is refused before a single byte is parsed. */
export const TOO_LARGE = Symbol("too_large");

/**
 * `content-length` is the cheap refusal, but a chunked body declares none, so
 * the bytes are counted as they arrive and the stream is cancelled the instant
 * the cap is passed. Buffering first and measuring after would let a single
 * unauthenticated POST put Cloudflare's whole 100 MB body allowance into a
 * 128 MB isolate, and take every other request sharing it down too.
 *
 * The cancel is deliberate: it stops the upload mid-flight, which is the whole
 * point. `wrangler dev` wraps the worker in a body-draining middleware that
 * then logs "Network connection lost" and can take the local server with it —
 * a dev-only facade, absent from the deployed bundle (`deploy --dry-run
 * --outdir` contains no drainer). Do not remove the cancel to quiet it.
 */
export async function readBytes(
  request: Request,
  limit: number,
): Promise<Uint8Array | typeof TOO_LARGE | null> {
  const stream = request.body;
  if (!stream) return new Uint8Array(0);
  const reader = stream.getReader() as ReadableStreamDefaultReader<Uint8Array>;
  const chunks: Uint8Array[] = [];
  let total = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      total += value.byteLength;
      if (total > limit) {
        await reader.cancel();
        return TOO_LARGE;
      }
      chunks.push(value);
    }
  } catch {
    return null;
  }

  const bytes = new Uint8Array(total);
  let at = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, at);
    at += chunk.byteLength;
  }
  return bytes;
}
