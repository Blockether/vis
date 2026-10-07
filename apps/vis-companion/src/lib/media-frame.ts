/**
 * Reserve attachment geometry from column and viewport dimensions before bytes arrive.
 * Placeholder, media and failure states share it, preventing asynchronous scroll shifts.
 * Inline bytes also give the picture's own size before any decode (`inlinePictureSize`).
 */

/**
 * Fixed aspect ratio capped independently of the media's decoded dimensions.
 *
 * Its child is laid OVER the frame (`*:absolute *:inset-0`), never sized by its
 * own `h-full` alone. WebKit resolved the zoom trigger's `h-full` against the 4:3
 * height BEFORE the 60svh cap, so in a wide column a picture sat under a band of
 * empty mat, and the frame cut its lower edge away.
 */
export const mediaFrameClass =
  'relative block w-full aspect-[4/3] max-h-[60svh] overflow-hidden border border-code-edge bg-code *:absolute *:inset-0';

/** The pulse a slot paints while its bytes are in flight, filling the frame. */
export const mediaPendingClass = 'block h-full w-full animate-pulse bg-thinking-surface';

/**
 * A picture or clip inside the reserved frame. `object-contain` keeps every
 * pixel of a tall phone screenshot visible; the leftover paper is the frame's
 * own MAT, so the picture is CENTRED in it. Shoved to `object-left` it looked
 * like a small image that had failed to fill a broken box, and the caption
 * beneath the empty half read as a label sitting ON the picture.
 */
export const mediaContentClass = 'block h-full w-full object-contain object-center';

/** A picture's size in pixels, the way the browser paints it. */
export interface PictureSize {
  width: number;
  height: number;
}

/** The plate's frame and figure styles once the picture's size is known. */
export interface PlateFit {
  aspectRatio: string;
  width: string;
}

/**
 * The plate fitted to a picture whose size is known before it decodes: the frame
 * takes the picture's own ratio, and the plate is no wider than the column, the
 * picture's own pixels or the 60svh height cap at that ratio. User report: the 4:3
 * reservation framed a wide or tall screenshot in bands of empty mat on two sides.
 * `undefined` keeps the reservation of the class for a picture of unknown size.
 */
export function plateFit(size: PictureSize | null | undefined): PlateFit | undefined {
  if (!size) return undefined;
  const ratio = `${size.width} / ${size.height}`;
  return {
    aspectRatio: ratio,
    width: `min(100%, ${size.width}px, calc(60svh * ${ratio}))`,
  };
}

/**
 * The plate's label, DOCKED under the mat and sharing its frame: one strip of
 * paper carrying the file's name, so nothing about the name can be mistaken for
 * part of the picture.
 */
export const mediaCaptionClass =
  'flex min-w-0 items-center gap-2 border border-t-0 border-code-edge bg-thinking-surface px-2 py-1 font-mono text-chip text-footer-muted';

/**
 * ONE picture is a PLATE; several are a GALLERY.
 *
 * A rail that gave every picture the full 4/3 plate turned four dropped
 * screenshots into four 60svh boxes stacked down the column — a wall to scroll
 * past rather than something to look at. A lone picture still gets its plate
 * and its caption, because that is the whole content of the message; from the
 * second one the rail becomes a grid of square tiles and the names move into
 * the viewer, where there is room for them.
 */
export type MediaLayout = 'plate' | 'grid';

/** The layout a rail of `count` pictures takes. The rule is the same on BOTH
 *  rails: what the human sent and what the model produced read alike. */
export function mediaGroupLayout(count: number): MediaLayout {
  return count > 1 ? 'grid' : 'plate';
}

/**
 * The gallery itself: two columns on a phone, more when there is room.
 *
 * Columns are the only thing width decides. A 390px phone gives ~183px tiles
 * and the widest desktop still gives ~160px, so no tile ever approaches a hit
 * box worth policing — and no `sm:` utility here shrinks one, because adding a
 * column is not the same as pinning a smaller box.
 */
export const mediaGridClass = 'grid grid-cols-2 gap-2 sm:grid-cols-3 mouse:grid-cols-4';

/** One gallery cell: square, reserved before its bytes land, same paper and
 *  same edge as the plate — a tile is the plate at gallery size, not a
 *  different control. */
export const mediaTileFrameClass =
  'block w-full aspect-square overflow-hidden border border-code-edge bg-code';

/**
 * A picture inside a tile FILLS it. The plate mats a tall screenshot because it
 * is the message; a contact sheet is read by what each frame is OF, and a grid
 * of letterboxed slivers separated by their own empty paper answers that worse
 * than a crop does. Full pixels stay one tap away in the viewer.
 */
export const mediaTileContentClass = 'block h-full w-full object-cover object-center';

/**
 * The size of a picture whose bytes are INLINE (bare base64 or a `data:` URL), read
 * from its header before anything decodes it. PNG, GIF and JPEG answer; a JPEG that
 * its EXIF turns a quarter answers with its axes swapped, as the browser paints it.
 * Any other format, or a malformed header, is `null`, and its plate keeps the 4:3.
 */
export function inlinePictureSize(base64: string): PictureSize | null {
  const payload = base64.startsWith('data:') ? base64.slice(base64.indexOf(',') + 1) : base64;
  const read = base64Reader(payload);
  // Ten bytes tell the formats apart and hold a GIF's size; a PNG's IHDR ends at 24.
  const head = read(0, 10);
  if (!head) return null;
  if (head[0] === 0xff && head[1] === 0xd8) return jpegSize(read);
  const signature = ascii(head, 0, 6);
  if (signature === 'GIF87a' || signature === 'GIF89a') {
    return pictureSize(uint16(head, 6, true), uint16(head, 8, true));
  }
  const png = ascii(head, 1, 3) === 'PNG' ? read(0, 24) : null;
  if (!png || ascii(png, 12, 4) !== 'IHDR') return null;
  return pictureSize(uint32(png, 16, false), uint32(png, 20, false));
}

/** `length` bytes from byte `offset`, or `null` past the end of the data. */
type ByteReader = (offset: number, length: number) => Uint8Array | null;

/**
 * Random access into base64 text: byte `k` lives in the 4-character group `k / 3`,
 * so a header walk decodes only the bytes it reads, never a whole photograph.
 */
function base64Reader(payload: string): ByteReader {
  return (offset, length) => {
    const skip = offset % 3;
    let text: string;
    try {
      text = atob(payload.slice(((offset - skip) / 3) * 4, Math.ceil((offset + length) / 3) * 4));
    } catch {
      return null;
    }
    if (text.length < skip + length) return null;
    const bytes = new Uint8Array(length);
    for (let index = 0; index < length; index += 1) bytes[index] = text.charCodeAt(skip + index);
    return bytes;
  };
}

function pictureSize(width: number, height: number): PictureSize | null {
  return width > 0 && height > 0 ? { width, height } : null;
}

function ascii(bytes: Uint8Array, at: number, length: number): string {
  return String.fromCharCode(...bytes.subarray(at, at + length));
}

function uint16(bytes: Uint8Array, at: number, little: boolean): number {
  return little ? bytes[at] | (bytes[at + 1] << 8) : (bytes[at] << 8) | bytes[at + 1];
}

function uint32(bytes: Uint8Array, at: number, little: boolean): number {
  const [low, high] = little ? [at, at + 2] : [at + 2, at];
  return uint16(bytes, high, little) * 0x10000 + uint16(bytes, low, little);
}

/** SOFn markers carry the frame size. C4 (DHT), C8 and CC (DAC) share the range, not the layout. */
function isStartOfFrame(marker: number): boolean {
  return marker >= 0xc0 && marker <= 0xcf && marker !== 0xc4 && marker !== 0xc8 && marker !== 0xcc;
}

/** Walk the segments to the frame header. An EXIF segment on the way says if the axes swap. */
function jpegSize(read: ByteReader): PictureSize | null {
  let at = 2;
  let turned = false;
  // Real headers reach their frame in a few hops; the cap stops a malformed file.
  for (let hop = 0; hop < 64; hop += 1) {
    const head = read(at, 4);
    if (!head || head[0] !== 0xff) return null;
    // A fill byte can come before a marker.
    if (head[1] === 0xff) {
      at += 1;
      continue;
    }
    if (isStartOfFrame(head[1])) {
      const frame = read(at + 5, 4);
      if (!frame) return null;
      const height = uint16(frame, 0, false);
      const width = uint16(frame, 2, false);
      return turned ? pictureSize(height, width) : pictureSize(width, height);
    }
    const length = uint16(head, 2, false);
    // Scan data or the end of the image before a frame header: there is no size.
    if (head[1] === 0xda || head[1] === 0xd9 || length < 2) return null;
    if (head[1] === 0xe1 && exifTurnsQuarter(read, at + 4, length - 2)) turned = true;
    at += 2 + length;
  }
  return null;
}

/** EXIF orientations 5–8 turn the picture a quarter, and the browser paints it turned. */
function exifTurnsQuarter(read: ByteReader, start: number, length: number): boolean {
  const head = read(start, 14);
  if (!head || ascii(head, 0, 6) !== 'Exif\0\0') return false;
  const order = ascii(head, 6, 2);
  if (order !== 'II' && order !== 'MM') return false;
  const little = order === 'II';
  const directory = start + 6 + uint32(head, 10, little);
  const count = read(directory, 2);
  if (!count) return false;
  const entries = Math.min(uint16(count, 0, little), Math.floor((start + length - directory - 2) / 12));
  const table = entries > 0 ? read(directory + 2, entries * 12) : null;
  if (!table) return false;
  for (let entry = 0; entry < entries; entry += 1) {
    if (uint16(table, entry * 12, little) !== 0x0112) continue;
    const orientation = uint16(table, entry * 12 + 8, little);
    return orientation >= 5 && orientation <= 8;
  }
  return false;
}
