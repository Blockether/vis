import { Capacitor } from '@capacitor/core';
import { Directory, Filesystem } from '@capacitor/filesystem';
import { Share } from '@capacitor/share';
import { blobAsDataUrl, sheetDismissed } from './image-file';

/** Options for the same file hand-off wearing a more specific noun in its caller. */
export type ArtifactShareOptions = {
  title?: string;
  dialogTitle?: string;
  noun?: string;
};

/** A filename safe to place below the app's cache directory, extension preserved. */
export function sharedFilename(name: string): string {
  const safe = (name || 'artifact')
    .replace(/[\\/:*?"<>|\u0000-\u001f]+/gu, '-')
    .replace(/\s+/gu, '-')
    .replace(/^[.\s-]+|[.\s-]+$/gu, '');
  return safe || 'artifact';
}

/** Whether this platform can hand a file to another app rather than only save it. */
export function canShareArtifactFiles(name = 'artifact', mediaType = ''): boolean {
  if (Capacitor.isNativePlatform()) return true;
  if (typeof navigator.share !== 'function') return false;
  if (typeof navigator.canShare !== 'function') return true;
  return navigator.canShare({
    files: [new File([], sharedFilename(name), { type: mediaType })],
  });
}

/** Honest action text: native/web share sheet when available, download otherwise. */
export function artifactShareVerb(name?: string, mediaType?: string): 'Share' | 'Save' {
  return canShareArtifactFiles(name, mediaType) ? 'Share' : 'Save';
}

/**
 * Hand one artifact's original bytes to the OS. Native platforms stage a private,
 * short-lived cache file because a share sheet accepts a URI rather than a Blob.
 */
export async function shareArtifact(
  blob: Blob,
  name: string,
  mediaType = '',
  options: ArtifactShareOptions = {},
): Promise<string> {
  const filename = sharedFilename(name);
  const title = options.title ?? name;
  const dialogTitle = options.dialogTitle ?? 'Share artifact';
  const noun = options.noun ?? 'Artifact';
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
      await Share.share({ title, files: [uri], dialogTitle });
      return `${noun} shared.`;
    }

    const file = new File([blob], filename, {
      type: mediaType || blob.type || 'application/octet-stream',
    });
    if (
      navigator.share &&
      (!navigator.canShare || navigator.canShare({ files: [file] }))
    ) {
      await navigator.share({ title, files: [file] });
      return `${noun} shared.`;
    }

    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    window.setTimeout(() => URL.revokeObjectURL(url), 1_000);
    return `${noun} downloaded.`;
  } catch (cause) {
    if (sheetDismissed(cause)) return '';
    throw cause;
  } finally {
    if (nativePath) {
      void Filesystem.deleteFile({
        path: nativePath,
        directory: Directory.Cache,
      }).catch(() => undefined);
    }
  }
}

/** Fetch the retained attachment URL only when the human asks to share it. */
export async function shareArtifactUrl(
  url: string,
  name: string,
  mediaType = '',
): Promise<string> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`artifact ${response.status}`);
  return shareArtifact(await response.blob(), name, mediaType);
}
