import { Capacitor } from '@capacitor/core';
import { desktopInvoke } from './desktop';

/**
 * Keeping an opened file: which verbs its band offers on this host, and the desktop
 * save panel behind `Save as…`.
 *
 * The desktop app saves where the person chooses, as a PDF reader does. A phone
 * offers one Share, because its share sheet already holds Save to Files. A browser
 * downloads, and offers Share as well when it has a share sheet for files.
 */

/** One verb of an opened file's band. */
export type FileVerb = 'Share' | 'Save as…' | 'Download' | 'Open in app' | 'Save';

/** The three hosts of this one bundle. */
export type FileHost = 'desktop' | 'phone' | 'web';

/** What the file is to its band: a workbook opens in an app, an archive is kept. */
export type FileKind = 'workbook' | 'archive' | 'file';

export function fileHost(): FileHost {
  if (desktopInvoke()) return 'desktop';
  return Capacitor.isNativePlatform() ? 'phone' : 'web';
}

/** The verbs of the band in order. The last one is the primary. */
export function fileVerbs(host: FileHost, kind: FileKind, canShare: boolean): FileVerb[] {
  if (host === 'desktop') {
    if (kind === 'workbook') return ['Open in app', 'Save as…'];
    return kind === 'file' && canShare ? ['Share', 'Save as…'] : ['Save as…'];
  }
  if (host === 'phone') {
    if (kind === 'workbook') return ['Open in app'];
    return kind === 'archive' ? ['Save'] : ['Share'];
  }
  return kind === 'file' && canShare ? ['Share', 'Download'] : ['Download'];
}

/**
 * The name the save panel proposes: the artifact's own, spaces kept. Only the
 * characters that no file system accepts in a name are replaced.
 */
export function proposedFilename(name: string): string {
  const safe = (name || '')
    .replace(/[\\/:*?"<>|\u0000-\u001f]+/gu, '-')
    .replace(/^[.\s]+|\s+$/gu, '');
  return safe || 'artifact';
}

/** The folder and the name of a saved path, enough to recognise it in one line. */
export function savedPlace(path: string): string {
  const parts = path.split(/[\\/]+/u).filter(Boolean);
  return parts.slice(-2).join('/') || path;
}

/** A file the desktop host wrote. */
export type SavedFile = { path: string };

/**
 * Ask the desktop host where to save the original bytes, and write them there.
 * Resolves null when the person closed the panel: that is a choice, not a failure.
 */
export async function saveArtifactAs(blob: Blob, name: string): Promise<SavedFile | null> {
  const invoke = desktopInvoke();
  if (!invoke) throw new Error('Save as needs the desktop app.');
  const result = (await invoke('save_artifact_as', {
    filename: proposedFilename(name),
    data: Array.from(new Uint8Array(await blob.arrayBuffer())),
  })) as { saved: boolean; path: string | null };
  return result.saved && result.path ? { path: result.path } : null;
}

/** Show a file that `saveArtifactAs` wrote in Finder, Explorer or the file manager. */
export async function revealSavedArtifact(path: string): Promise<void> {
  const invoke = desktopInvoke();
  if (!invoke) throw new Error('Showing a file needs the desktop app.');
  await invoke('reveal_saved_artifact', { path });
}
