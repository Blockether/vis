import { Clipboard } from '@capacitor/clipboard';
import { Capacitor } from '@capacitor/core';
import { blobAsDataUrl } from './image-file';

/**
 * Putting a finished picture on the clipboard. Sharing and saving it are the
 * viewer's file verbs (`useFileActions`), the same as for every other artifact.
 * The platform branch lives here rather than in the viewer. The call resolves
 * with the sentence to show the human, and throws only on a real failure.
 */

/** Put the picture on the clipboard. */
export async function copyImage(blob: Blob, name: string): Promise<string> {
  if (Capacitor.isNativePlatform()) {
    await Clipboard.write({ image: await blobAsDataUrl(blob), label: name });
  } else if (navigator.clipboard?.write && typeof ClipboardItem !== 'undefined') {
    await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]);
  } else {
    throw new Error('Image copying is not supported by this browser');
  }
  return 'Image copied. Paste it into your next message.';
}
