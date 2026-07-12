import { Platform } from "react-native";

/* ── the TUI's blockether-light palette (internal/theme.clj) — one flat
   warm ground, amber accent, muted chrome. No cards, no elevation. ── */
export const c = {
  paper: "#FAF3EB" /* :terminal-bg */,
  field: "#FFFDF8" /* :input-field-bg */,
  line: "#C9C1B3" /* :dialog-border */,
  lineSoft: "#E4DCCE" /* between tsep-bg and border */,
  hair: "#C8BFB0" /* :answer-sep-fg */,
  tsepBg: "#F5EEE4" /* :turn-separator-bg */,
  tsep: "#B4AC9E" /* :turn-separator-fg */,
  border: "#8C857A" /* :border-fg */,
  ink: "#262626" /* :box-fg */,
  ink2: "#3F3F3F" /* :md-h2-fg */,
  dim: "#6F6A63" /* :footer-fg-muted */,
  amber: "#F0AD00" /* :dialog-title-bg */,
  amberBright: "#FFC420",
  amberInk: "#261E00" /* :dialog-title-fg */,
  amberDeep: "#B47800" /* :header-hover-fg-ish */,
  roleUser: "#F0AD00" /* user gutter — the accent */,
  roleVis: "#16A34A" /* :ai-role-fg */,
  err: "#DC2626" /* :status-bad */,
  errBg: "#FAE2E2" /* :code-err-bg */,
  ok: "#16A34A" /* :status-ok */,
  codeBg: "#F2EBDF" /* :code-block-bg */,
  codeInk: "#262626" /* :code-block-fg */,
  chipBg: "#FAECC5" /* :md-summary-bg */,
  chipInk: "#7A4A00" /* :md-summary-fg */,
  accent: "#7C5CFF" /* modern violet — active / project pop */,
  accentSoft: "#EFEAFF",
  accentInk: "#2E1F6B"
} as const;

/* ── project accents ──────────────────────────────────────────────
   Each project (session group) gets a stable, vivid color: the gateway
   color when set, else one picked from this palette by id hash. The drawer
   paints it on the header bar, icon, count pill and child-row indent. */
export const projectPalette = [
  "#F0AD00" /* amber   */,
  "#16A34A" /* green   */,
  "#2563EB" /* blue    */,
  "#7C5CFF" /* violet  */,
  "#DB2777" /* magenta */,
  "#0891B2" /* teal    */,
  "#EA580C" /* orange  */
] as const;

export const projectColor = (id?: string | null, override?: string | null): string => {
  if (override) return override;
  const key = id ?? "";
  let h = 0;
  for (let i = 0; i < key.length; i += 1) h = (h * 31 + key.charCodeAt(i)) >>> 0;
  return projectPalette[h % projectPalette.length];
};

/* translucent tint of a project color for soft fills (#RRGGBB + alpha byte) */
export const tint = (hex: string, alpha = "1F"): string =>
  hex.length === 7 ? `${hex}${alpha}` : hex;

export const mono = Platform.select({
  ios: "Menlo",
  android: "monospace",
  default: "monospace"
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
