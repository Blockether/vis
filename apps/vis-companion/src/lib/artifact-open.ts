import { Capacitor } from '@capacitor/core';
import { downloadArtifact, shareArtifact, sharedFilename } from './artifact-share';
import { isArchiveMedia, isXlsxMedia, XLSX_MEDIA } from './artifacts';
import { desktopInvoke } from './desktop';

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

/**
 * Keep the original archive as a file. The desktop host saves it in Downloads/Vis Artifacts and
 * shows it in Finder, Explorer or the Linux file manager. A phone offers the save sheet.
 */
export async function keepArchive(blob: Blob, name: string, mediaType = ''): Promise<string> {
  if (!isArchiveMedia(mediaType || blob.type, name)) throw new Error('Not an archive.');
  const filename = sharedFilename(name);
  const invoke = desktopInvoke();
  if (invoke) {
    const result = (await invoke('reveal_artifact', {
      filename,
      data: Array.from(new Uint8Array(await blob.arrayBuffer())),
    })) as { revealed: boolean; path: string };
    return result.revealed
      ? `Archive saved to ${result.path}.`
      : `Archive saved to ${result.path}. Open that folder to find it.`;
  }
  if (Capacitor.isNativePlatform()) {
    return shareArtifact(blob, filename, mediaType || blob.type, {
      title: name,
      dialogTitle: 'Save archive',
      noun: 'Archive',
      retainCacheFile: true,
    });
  }
  return downloadArtifact(blob, filename, 'Archive');
}
