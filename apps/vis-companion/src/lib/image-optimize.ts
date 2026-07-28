// Shrink an image BEFORE it leaves the phone.
//
// The device-side twin of `src/com/blockether/vis/internal/image_optimize.clj`,
// with the same policy numbers on purpose: long edge capped at the bound the
// providers resize to anyway, JPEG for opaque pixels, PNG kept when the picture
// really carries alpha, and the re-encode kept only when it wins big enough to
// be worth a lossy round-trip.
//
// Why here and not only in the engine: everything upstream of the gateway pays
// for the original bytes. A 12 MP phone photo is ~4 MB, +33% as the base64 the
// submit endpoint speaks, held TWICE in the WKWebView (the data URL rides both
// the request and the composer preview) and pushed over a Tailscale/LAN hop
// before anything can shrink it. That is the jetsam curve, the slow send and
// the "image too large" rejection all at once. Shrinking at capture makes the
// upload ~10x smaller and the engine-side optimizer a no-op it can skip.

/** Long-edge pixel bound — pixels above it are paid for and then thrown away. */
export const MAX_DIMENSION = 1568;
/** Below this a decode/encode round-trip cannot pay for itself (64 KiB). */
export const FLOOR_BYTES = 64 * 1024;
/** High enough that UI text in a screenshot stays legible to a vision model. */
export const JPEG_QUALITY = 0.82;
/** Keep the re-encode only at or under this fraction of the original. */
export const MIN_GAIN = 0.85;
/**
 * Refuse to even DECODE beyond this. A decoded frame costs width*height*4 bytes
 * of RAM regardless of how well it was compressed, so a 100 MB panorama is a
 * crash, not a slow path.
 */
export const MAX_DECODE_BYTES = 32 * 1024 * 1024;

/** GIF is absent on purpose: it may be animated and a re-encode drops that. */
const OPTIMIZABLE = new Set(['image/png', 'image/jpeg', 'image/jpg', 'image/bmp', 'image/webp']);
/** Formats whose pixels can carry alpha; the rest are opaque by construction. */
const MAY_HAVE_ALPHA = new Set(['image/png', 'image/webp']);

export interface OptimizedImage {
  blob: Blob;
  mediaType: string;
  width: number;
  height: number;
}

interface Decoded {
  source: CanvasImageSource;
  width: number;
  height: number;
  release: () => void;
}

async function decodeImage(blob: Blob): Promise<Decoded> {
  // `createImageBitmap` decodes off the main thread and is releasable, which is
  // what keeps a burst of picked photos from stacking decoded frames in RAM.
  if (typeof createImageBitmap === 'function') {
    const bitmap = await createImageBitmap(blob);
    return {
      source: bitmap,
      width: bitmap.width,
      height: bitmap.height,
      release: () => bitmap.close(),
    };
  }
  const url = URL.createObjectURL(blob);
  try {
    const image = await new Promise<HTMLImageElement>((resolve, reject) => {
      const element = new Image();
      element.onload = () => resolve(element);
      element.onerror = () => reject(new Error('Could not decode image'));
      element.src = url;
    });
    return {
      source: image,
      width: image.naturalWidth,
      height: image.naturalHeight,
      release: () => URL.revokeObjectURL(url),
    };
  } catch (cause) {
    URL.revokeObjectURL(url);
    throw cause;
  }
}

// Scanned in horizontal strips: one full-frame ImageData of a 1568² canvas is
// ~10 MB, and we only ever need to find the FIRST non-opaque pixel.
function hasAlpha(context: CanvasRenderingContext2D, width: number, height: number): boolean {
  const strip = 64;
  for (let top = 0; top < height; top += strip) {
    const rows = Math.min(strip, height - top);
    const { data } = context.getImageData(0, top, width, rows);
    for (let i = 3; i < data.length; i += 4) {
      if (data[i] !== 255) return true;
    }
  }
  return false;
}

function encode(canvas: HTMLCanvasElement, type: string, quality?: number): Promise<Blob | null> {
  return new Promise((resolve) => {
    canvas.toBlob((out) => resolve(out), type, quality);
  });
}

/**
 * Shrink one image, or `null` when the original bytes are already the best
 * answer (too small, wrong format, undecodable, no worthwhile gain). Never
 * throws: every caller can fall back to what the user picked.
 */
export async function optimizeImage(
  blob: Blob,
  mediaType: string,
  { maxDimension = MAX_DIMENSION, floorBytes = FLOOR_BYTES, jpegQuality = JPEG_QUALITY, minGain = MIN_GAIN } = {},
): Promise<OptimizedImage | null> {
  const type = (mediaType || blob.type || '').trim().toLowerCase();
  if (!OPTIMIZABLE.has(type)) return null;
  if (blob.size < floorBytes) return null;
  if (blob.size > MAX_DECODE_BYTES) return null;

  let decoded: Decoded | null = null;
  try {
    decoded = await decodeImage(blob);
    const { width: sourceWidth, height: sourceHeight } = decoded;
    if (!sourceWidth || !sourceHeight) return null;
    const scale = Math.min(1, maxDimension / Math.max(sourceWidth, sourceHeight));
    const width = Math.max(1, Math.round(sourceWidth * scale));
    const height = Math.max(1, Math.round(sourceHeight * scale));

    const canvas = document.createElement('canvas');
    canvas.width = width;
    canvas.height = height;
    const context = canvas.getContext('2d', { willReadFrequently: true });
    if (!context) return null;
    context.imageSmoothingEnabled = true;
    context.imageSmoothingQuality = 'high';
    context.drawImage(decoded.source, 0, 0, width, height);
    decoded.release();
    decoded = null;

    // Alpha decides the container: JPEG on a picture with real transparency
    // would flatten it, and PNG on a photo is usually LARGER than the original.
    const opaque = !MAY_HAVE_ALPHA.has(type) || !hasAlpha(context, width, height);
    const targetType = opaque ? 'image/jpeg' : 'image/png';
    const out = await encode(canvas, targetType, opaque ? jpegQuality : undefined);
    // Free the backing store now rather than at the next GC — iOS counts it.
    canvas.width = 0;
    canvas.height = 0;
    if (!out || !out.size) return null;
    if (out.size > blob.size * minGain) return null;
    return { blob: out, mediaType: targetType, width, height };
  } catch {
    return null;
  } finally {
    decoded?.release();
  }
}

/** Keep a filename honest when the container changed. */
export function retargetFilename(filename: string, mediaType: string): string {
  const extension = mediaType === 'image/jpeg' ? '.jpg' : mediaType === 'image/png' ? '.png' : '';
  if (!extension) return filename;
  if (filename.toLowerCase().endsWith(extension)) return filename;
  const dot = filename.lastIndexOf('.');
  const base = dot > 0 ? filename.slice(0, dot) : filename;
  return `${base}${extension}`;
}
