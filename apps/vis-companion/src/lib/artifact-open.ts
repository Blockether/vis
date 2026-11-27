import { Capacitor } from '@capacitor/core';
import { downloadArtifact, shareArtifact, sharedFilename } from './artifact-share';
import { isArchiveMedia, isXlsxMedia, XLSX_MEDIA } from './artifacts';
import { desktopInvoke } from './desktop';
import { homeifyPath } from './path';

/** Browsers can save a workbook, but cannot launch an installed desktop app. */
export function spreadsheetOpenVerb(): 'Open in app' | 'Download' {
  return desktopInvoke() || Capacitor.isNativePlatform() ? 'Open in app' : 'Download';
}

/** Give the original workbook to the platform, without a remote document viewer. */
export async function openSpreadsheet(blob: Blob, name: string, mediaType = ''): Promise<string> {
  if (!isXlsxMedia(mediaType || blob.type, name)) throw new Error('Not an XLSX workbook.');
  const safe = sharedFilename(name);
  // A MIME-only workbook still needs the extension used by the OS association.
  const filename = /\.xlsx$/i.test(safe) ? safe : `${safe}.xlsx`;
  const invoke = desktopInvoke();
  if (invoke) {
    const result = (await invoke('open_xlsx', {
      filename,
      data: Array.from(new Uint8Array(await blob.arrayBuffer())),
    })) as { opened: boolean };
    return result.opened
      ? 'Workbook opened in your spreadsheet app.'
      : 'Workbook saved in Downloads/Vis Artifacts. Install a spreadsheet app to open it.';
  }
  if (Capacitor.isNativePlatform()) {
    return shareArtifact(blob, filename, XLSX_MEDIA, {
      title: name,
      dialogTitle: 'Open workbook in an app',
      noun: 'Workbook',
      retainCacheFile: true,
    });
  }
  return downloadArtifact(blob, filename, 'Workbook');
}

/** What keeping an archive does on this host: show it in the file manager, or save it. */
export function archiveOpenVerb(): 'Show in Finder' | 'Show in folder' | 'Save' | 'Download' {
  if (desktopInvoke()) return /Mac/i.test(navigator.userAgent) ? 'Show in Finder' : 'Show in folder';
  return Capacitor.isNativePlatform() ? 'Save' : 'Download';
}

/** The words of one kept file: its noun in a message and the title of the phone sheet. */
export type KeptFileWords = { noun: string; dialogTitle: string };

/**
 * What happened to one kept file. `text` says it, and it is '' when the person closed the
 * sheet. `text` shows the home folder as `~`. `path` is the full path where the desktop host
 * saved the file; no other host knows a path.
 */
export type KeptFile = { text: string; path?: string };

/**
 * Keep one file on this host. The desktop host saves it in Downloads/Vis Artifacts and shows it
 * in Finder, Explorer or the Linux file manager. A phone offers the share sheet, which holds
 * Save to Files. A browser downloads it.
 */
export async function keepFile(
  blob: Blob,
  name: string,
  mediaType: string,
  { noun, dialogTitle }: KeptFileWords,
): Promise<KeptFile> {
  const filename = sharedFilename(name);
  const invoke = desktopInvoke();
  if (invoke) {
    const result = (await invoke('reveal_artifact', {
      filename,
      data: Array.from(new Uint8Array(await blob.arrayBuffer())),
    })) as { revealed: boolean; path: string };
    const shown = homeifyPath(result.path);
    return {
      text: result.revealed ? `${noun} saved to ${shown}.` : `${noun} saved to ${shown}. Open that folder to find it.`,
      path: result.path,
    };
  }
  if (Capacitor.isNativePlatform()) {
    return {
      text: await shareArtifact(blob, filename, mediaType || blob.type, {
        title: name,
        dialogTitle,
        noun,
        retainCacheFile: true,
      }),
    };
  }
  return { text: downloadArtifact(blob, filename, noun) };
}

/**
 * Show a file that the desktop host kept, in Finder, Explorer or the Linux file manager.
 * False when the host is not the desktop app, the file is gone or no file manager answers.
 */
export async function revealKeptFile(path: string): Promise<boolean> {
  const invoke = desktopInvoke();
  if (!invoke) return false;
  try {
    return (await invoke('reveal_kept_artifact', { path })) === true;
  } catch {
    return false;
  }
}

/** Keep the original archive as a file; see `keepFile` for what each host does. */
export async function keepArchive(blob: Blob, name: string, mediaType = ''): Promise<string> {
  if (!isArchiveMedia(mediaType || blob.type, name)) throw new Error('Not an archive.');
  return (await keepFile(blob, name, mediaType, { noun: 'Archive', dialogTitle: 'Save archive' })).text;
}
