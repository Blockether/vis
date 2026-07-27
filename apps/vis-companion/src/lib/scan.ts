// Pairing-QR decoding for the companion.
//
// Live scanning happens IN THE WEBVIEW (getUserMedia + jsQR), not in a native
// plugin. The ML Kit barcode plugin needs GoogleMLKit, which is CocoaPods-only
// and does NOT link into the SPM iOS project — so on iPhone `isSupported()` is
// always false and the whole scan collapsed to "shoot a still, decode once",
// which is why the camera never reacted to a code held in front of it.
// WKWebView is fine with a live preview: Capacitor's WebViewDelegationHandler
// answers `requestMediaCapturePermissionFor` with `.grant` and its
// configuration sets `allowsInlineMediaPlayback = true`. ML Kit is still
// preferred on Android, where it actually links.
import jsQR from 'jsqr';
import { Capacitor, registerPlugin } from '@capacitor/core';

/** Longest edge fed to jsQR from a live frame. Bigger = slower, not better. */
const LIVE_EDGE = 1024;
/** Longest edge fed to jsQR from a still. Stills are far higher resolution. */
const PHOTO_EDGE = 1800;

function decode(image: ImageData): string | null {
  const found = jsQR(image.data, image.width, image.height, {
    // A terminal QR is white-on-dark: inverted. Never drop this.
    inversionAttempts: 'attemptBoth',
  });
  const text = found?.data?.trim();
  return text ? text : null;
}

function sample(
  source: CanvasImageSource,
  sx: number,
  sy: number,
  sw: number,
  sh: number,
  edge: number,
  canvas: HTMLCanvasElement,
): ImageData | null {
  const scale = Math.min(1, edge / Math.max(sw, sh));
  const w = Math.max(1, Math.round(sw * scale));
  const h = Math.max(1, Math.round(sh * scale));
  canvas.width = w;
  canvas.height = h;
  const ctx = canvas.getContext('2d', { willReadFrequently: true });
  if (!ctx) return null;
  ctx.drawImage(source, sx, sy, sw, sh, 0, 0, w, h);
  return ctx.getImageData(0, 0, w, h);
}

/**
 * Two passes over one image: the whole frame, then the centre square.
 *
 * The centre crop is not redundant. A dense pairing QR (version 10+, ~60
 * modules) photographed with the whole terminal in frame lands at barely one
 * pixel per module after the downscale, and jsQR cannot resolve that. Cropping
 * to the middle — where a person aims — roughly doubles the module size.
 */
function decodeSource(
  source: CanvasImageSource,
  width: number,
  height: number,
  edge: number,
  canvas: HTMLCanvasElement,
): string | null {
  if (!width || !height) return null;
  const whole = sample(source, 0, 0, width, height, edge, canvas);
  if (whole) {
    const hit = decode(whole);
    if (hit) return hit;
  }
  const side = Math.round(Math.min(width, height) * 0.7);
  const cropped = sample(
    source,
    Math.round((width - side) / 2),
    Math.round((height - side) / 2),
    side,
    side,
    edge,
    canvas,
  );
  return cropped ? decode(cropped) : null;
}

/** One live camera frame → pairing string, or null when nothing decodes. */
export function decodeFrame(
  video: HTMLVideoElement,
  canvas: HTMLCanvasElement,
): string | null {
  return decodeSource(video, video.videoWidth, video.videoHeight, LIVE_EDGE, canvas);
}

function decodeQrFromBase64(base64: string, format: string): Promise<string | null> {
  return new Promise((resolve, reject) => {
    const img = new Image();
    const canvas = document.createElement('canvas');
    img.onload = () => {
      resolve(decodeSource(img, img.naturalWidth, img.naturalHeight, PHOTO_EDGE, canvas));
    };
    img.onerror = () => reject(new Error('Could not read the captured photo'));
    img.src = `data:image/${format || 'jpeg'};base64,${base64}`;
  });
}

/** Still-photo fallback, for when the live preview cannot start at all. */
export async function scanQrFromPhoto(): Promise<string | null> {
  const { Camera, CameraResultType, CameraSource } = await import('@capacitor/camera');
  const permission = await Camera.requestPermissions({ permissions: ['camera'] });
  if (permission.camera === 'denied') {
    throw new Error('Camera access was denied — enable it in Settings');
  }
  const photo = await Camera.getPhoto({
    quality: 100,
    allowEditing: false,
    resultType: CameraResultType.Base64,
    source: CameraSource.Camera,
    saveToGallery: false,
  });
  if (!photo.base64String) return null;
  return decodeQrFromBase64(photo.base64String, photo.format);
}

/**
 * Native live scanner on iOS and Android, via the official `@capacitor/barcode-scanner`.
 * iOS gets Apple's Vision-backed scanner through the plugin's SwiftPM package —
 * unlike the ML Kit plugin, which is CocoaPods-only and never links here.
 *
 * The bridge is registered directly rather than importing the plugin's JS entry:
 * that entry pulls in `html5-qrcode` (~370 kB) purely for its web fallback and
 * its format enum, which a native shell never needs and would wait on.
 */
const NATIVE_HINT_QR_CODE = 0;
const NATIVE_CAMERA_BACK = 1;
const NATIVE_ORIENTATION_ADAPTIVE = 3;

interface NativeScannerPlugin {
  scanBarcode(options: Record<string, unknown>): Promise<{ ScanResult?: string }>;
}

let nativeScanner: NativeScannerPlugin | null = null;

export async function scanWithNativeScanner(): Promise<string | null> {
  if (!Capacitor.isNativePlatform()) return null;
  if (!Capacitor.isPluginAvailable('CapacitorBarcodeScanner')) return null;
  nativeScanner ??= registerPlugin<NativeScannerPlugin>('CapacitorBarcodeScanner');
  const result = await nativeScanner.scanBarcode({
    hint: NATIVE_HINT_QR_CODE,
    cameraDirection: NATIVE_CAMERA_BACK,
    scanOrientation: NATIVE_ORIENTATION_ADAPTIVE,
    scanInstructions: 'Point at the pairing QR in your terminal',
    scanButton: false,
    scanText: ' ',
    cancelButtonAccessibilityLabel: 'Cancel QR scan',
    torchButtonOnAccessibilityLabel: 'Turn flashlight off',
    torchButtonOffAccessibilityLabel: 'Turn flashlight on',
  });
  return result.ScanResult || null;
}

/** True when a live in-webview preview is possible at all. */
export function liveScanSupported(): boolean {
  return typeof navigator !== 'undefined' && !!navigator.mediaDevices?.getUserMedia;
}

/** True when the still-photo fallback exists (native shells only). */
export function photoScanSupported(): boolean {
  return Capacitor.isNativePlatform();
}
