import { Clipboard } from '@capacitor/clipboard';
import { Capacitor } from '@capacitor/core';
import { Directory, Filesystem } from '@capacitor/filesystem';
import { Share } from '@capacitor/share';
import { blobAsDataUrl, editedFilename, sheetDismissed } from './image-file';

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
 * Share the picture, or download it where no sheet exists. Resolves EMPTY when
 * the human dismissed the sheet: that is a decision, not a failure to report.
 */
export async function shareImage(blob: Blob, name: string): Promise<string> {
  const filename = editedFilename(name);
  let nativePath: string | null = null;
  try {
    if (Capacitor.isNativePlatform()) {
      nativePath = `shared/${Date.now()}-${filename}`;
      const dataUrl = await blobAsDataUrl(blob);
      await Filesystem.writeFile({
        path: nativePath,
        directory: Directory.Cache,
        data: dataUrl.slice(dataUrl.indexOf(',') + 1),
        recursive: true,
      });
      const { uri } = await Filesystem.getUri({
        path: nativePath,
        directory: Directory.Cache,
      });
      await Share.share({
        title: name,
        files: [uri],
        dialogTitle: 'Share image',
      });
      return 'Image shared.';
    }
    const file = new File([blob], filename, { type: 'image/png' });
    if (
      navigator.share &&
      (!navigator.canShare || navigator.canShare({ files: [file] }))
    ) {
      await navigator.share({ title: name, files: [file] });
      return 'Image shared.';
    }
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    window.setTimeout(() => URL.revokeObjectURL(url), 1_000);
    return 'Image downloaded.';
  } catch (cause) {
    if (sheetDismissed(cause)) return '';
    throw cause;
  } finally {
    // The temporary copy the share sheet read from is ours to clean up, whether
    // the sheet was used or waved away.
    if (nativePath) {
      void Filesystem.deleteFile({
        path: nativePath,
        directory: Directory.Cache,
      }).catch(() => undefined);
    }
  }
}
