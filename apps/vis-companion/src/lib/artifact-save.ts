import { Capacitor } from '@capacitor/core';
import { desktopInvoke } from './desktop';

/**
 * Keeping an opened file: which verbs its band offers on this host, and the desktop
 * save panel behind `Save…`.
 *
 * The desktop app saves where the person chooses, as a PDF reader does. A phone
 * offers one Share, because its share sheet already holds Save to Files. A browser
 * downloads, and offers Share as well when it has a share sheet for files.
 */

/** One verb of an opened file's band. */
export type FileVerb = 'Share' | 'Save…' | 'Download' | 'Open in app' | 'Save';

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
    if (kind === 'workbook') return ['Open in app', 'Save…'];
    return kind === 'file' && canShare ? ['Share', 'Save…'] : ['Save…'];
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

/** A file the desktop host wrote. */
export type SavedFile = { path: string };

/**
 * Ask the desktop host where to save the original bytes, and write them there.
 * Resolves null when the person closed the panel: that is a choice, not a failure.
 */
export async function saveArtifactAs(blob: Blob, name: string): Promise<SavedFile | null> {
  const invoke = desktopInvoke();
  if (!invoke) throw new Error('Saving needs the desktop app.');
  const result = (await invoke('save_artifact_as', {
    filename: proposedFilename(name),
    data: Array.from(new Uint8Array(await blob.arrayBuffer())),
  })) as { saved: boolean; path: string | null };
  return result.saved && result.path ? { path: result.path } : null;
}
