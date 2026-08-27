import { Clipboard } from '@capacitor/clipboard';
import { Capacitor } from '@capacitor/core';
import { blobAsDataUrl, editedFilename } from './image-file';
import { shareArtifact } from './artifact-share';

/**
 * Handing a finished picture to the operating system: the clipboard, the share
 * sheet, or a download where neither exists.
 *
 * Every platform branch lives here rather than in the viewer, so the component
 * that shows a picture does not also have to know about Capacitor, temporary
 * files and `navigator.canShare`. Each call resolves with the sentence to show
 * the human, and throws only on a real failure.
 */

/**
 * Does a real share sheet exist for image FILES?
 *
 * Without one the share button downloads, and calling that "Share" is a lie the
 * user only discovers after tapping — so the label follows the capability.
 */
export function canShareImageFiles(): boolean {
  if (Capacitor.isNativePlatform()) return true;
  if (typeof navigator.share !== 'function') return false;
  if (typeof navigator.canShare !== 'function') return true;
  return navigator.canShare({
    files: [new File([], 'image.png', { type: 'image/png' })],
  });
}

/** What the button is honestly called on this platform. */
export function shareVerb(): 'Share' | 'Save' {
  return canShareImageFiles() ? 'Share' : 'Save';
}

/** Put the picture on the clipboard. */
export async function copyImage(blob: Blob, name: string): Promise<string> {
  if (Capacitor.isNativePlatform()) {
    await Clipboard.write({ image: await blobAsDataUrl(blob), label: name });
  } else if (
    navigator.clipboard?.write &&
    typeof ClipboardItem !== 'undefined'
  ) {
    await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]);
  } else {
    throw new Error('Image copying is not supported by this browser');
  }
  return 'Image copied. Paste it into your next message.';
}

/**
 * Share the finished PNG, or download it where no sheet exists. Resolves EMPTY
 * when the human dismissed the sheet: that is a decision, not a failure.
 */
export async function shareImage(blob: Blob, name: string): Promise<string> {
  return shareArtifact(blob, editedFilename(name), 'image/png', {
    title: name,
    dialogTitle: 'Share image',
    noun: 'Image',
  });
}
