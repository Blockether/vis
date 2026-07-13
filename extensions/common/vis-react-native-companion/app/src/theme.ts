import { Platform } from "react-native";

/* ── iOS companion palette — neutral system surfaces with a single blue
   accent. Keep the legacy amber keys as aliases so older call-sites inherit
   the non-yellow primary without churn. ── */
export const c = {
  paper: "#F7F7FA",
  field: "#FFFFFF",
  line: "#D1D1D6",
  lineSoft: "#E5E5EA",
  hair: "#D8D8DE",
  tsepBg: "#F2F2F7",
  tsep: "#9A9AA2",
  border: "#8E8E93",
  ink: "#111114",
  ink2: "#2C2C2E",
  dim: "#6E6E73",
  accent: "#0A84FF",
  accentSoft: "#EAF3FF",
  accentInk: "#003A75",
  amber: "#0A84FF",
  amberBright: "#EAF3FF",
  amberInk: "#FFFFFF",
  amberDeep: "#0066CC",
  roleUser: "#0A84FF",
  roleVis: "#16A34A",
  err: "#FF3B30",
  errBg: "#FFE8E6",
  ok: "#34C759",
  codeBg: "#F2F2F7",
  codeInk: "#111114",
  chipBg: "#EAF3FF",
  chipInk: "#003A75",
} as const;

/* ── project accents ──────────────────────────────────────────────
   Each project (session group) gets a stable, vivid color: the gateway
   color when set, else one picked from this palette by id hash. The drawer
   paints it on the header bar, icon, count pill and child-row indent. */
export const projectPalette = [
  "#0A84FF" /* blue    */,
  "#34C759" /* green   */,
  "#5856D6" /* indigo  */,
  "#AF52DE" /* purple  */,
  "#FF2D55" /* rose    */,
  "#00A7B5" /* teal    */,
  "#8E8E93" /* graphite*/,
] as const;

export const projectColor = (
  id?: string | null,
  override?: string | null,
): string => {
  if (override) return override;
  const key = id ?? "";
  let h = 0;
  for (let i = 0; i < key.length; i += 1)
    h = (h * 31 + key.charCodeAt(i)) >>> 0;
  return projectPalette[h % projectPalette.length] ?? projectPalette[0];
};

/* translucent tint of a project color for soft fills (#RRGGBB + alpha byte) */
export const tint = (hex: string, alpha = "1F"): string =>
  hex.length === 7 ? `${hex}${alpha}` : hex;

export const mono = Platform.select({
  ios: "Menlo",
  android: "monospace",
  default: "monospace",
}) as string;

const asMs = (epoch: number): number => (epoch > 1e12 ? epoch : epoch * 1000);

export const timeHHMM = (epoch?: number): string | undefined => {
  if (!epoch) return undefined;
  const d = new Date(asMs(epoch));
  if (Number.isNaN(d.getTime())) return undefined;
  return `${String(d.getHours()).padStart(2, "0")}:${String(d.getMinutes()).padStart(2, "0")}`;
};

/* "just now" / "4m" / "2h" / "3d" — the sessions-drawer age column */
export const relTime = (epoch?: number): string => {
  if (!epoch) return "";
  const delta = Date.now() - asMs(epoch);
  if (!Number.isFinite(delta) || delta < 0) return "";
  const s = Math.floor(delta / 1000);
  if (s < 60) return "just now";
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m ago`;
  const h = Math.floor(m / 60);
  if (h < 24) return `${h}h ago`;
  return `${Math.floor(h / 24)}d ago`;
};

export const shortId = (id?: string): string => (id ? id.slice(0, 8) : "");
