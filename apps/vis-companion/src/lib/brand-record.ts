/**
 * The white label of the web app, as one checked record.
 *
 * A brand comes from the build (`VIS_WEB_BRAND`) or from the gateway that serves
 * the page (`GET /v1/web/brand`). Its rules are those of `web_brand` in the
 * gateway contract. This module uses no DOM: the Vite config checks a build
 * brand with it.
 */
import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json' with { type: 'json' };
import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json' with { type: 'json' };
import { THEMES } from './themes.generated.ts';

const BRAND = gatewaySchema.$defs.web_brand.properties;
const HREF = gatewaySchema.$defs.web_brand_href;
const LINK = gatewaySchema.$defs.web_brand_link.properties;
const THEME = gatewaySchema.$defs.web_brand_theme.properties;
const ICONS: readonly string[] = viewSchema.$defs.app_icon.enum;

/** One icon link of the header. `icon` is an app icon name. */
export interface BrandLink {
  readonly icon: string;
  readonly label: string;
  readonly href: string;
}

/** The only theme of a white label: a shipped theme, with some of its CSS variables replaced. */
export interface BrandTheme {
  readonly label?: string;
  readonly base: string;
  readonly colors?: Readonly<Record<string, string>>;
}

/** An absent key keeps the Vis default. */
export interface Brand {
  readonly name?: string;
  readonly logo?: string;
  readonly href?: string;
  readonly links?: readonly BrandLink[];
  readonly theme?: BrandTheme;
}

interface TextRule {
  minLength?: number;
  maxLength?: number;
  pattern?: string;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function textFault(name: string, value: unknown, rule: TextRule): string | null {
  if (typeof value !== 'string') return `${name} must be a string`;
  if (value.length < (rule.minLength ?? 0)) return `${name} is empty`;
  const max = rule.maxLength;
  // The contract counts characters. Count them only when the UTF-16 length is over the limit.
  if (max !== undefined && value.length > max && [...value].length > max) {
    return `${name} is longer than ${max} characters`;
  }
  if (rule.pattern !== undefined && !new RegExp(rule.pattern).test(value)) {
    return `${name} does not match ${rule.pattern}`;
  }
  return null;
}

function keyFault(name: string, value: Record<string, unknown>, known: object): string | null {
  const extra = Object.keys(value).find((key) => !(key in known));
  return extra === undefined ? null : `${name} has the unknown key ${extra}`;
}

function linksFault(links: unknown): string | null {
  if (!Array.isArray(links)) return 'links must be a list';
  if (links.length > BRAND.links.maxItems) return `links has more than ${BRAND.links.maxItems} items`;
  for (const [index, link] of links.entries()) {
    const at = `links[${index}]`;
    if (!isRecord(link)) return `${at} must be an object`;
    const fault =
      keyFault(at, link, LINK) ??
      (ICONS.includes(link.icon as string) ? null : `${at}.icon is not an app icon`) ??
      textFault(`${at}.label`, link.label, LINK.label) ??
      textFault(`${at}.href`, link.href, HREF);
    if (fault) return fault;
  }
  return null;
}

function themeFault(theme: unknown): string | null {
  if (!isRecord(theme)) return 'theme must be an object';
  const { label, base, colors } = theme;
  const fault =
    keyFault('theme', theme, THEME) ??
    textFault('theme.base', base, THEME.base) ??
    (THEMES.some((choice) => choice.id === base) ? null : `theme.base is not a shipped theme: ${String(base)}`) ??
    (label === undefined ? null : textFault('theme.label', label, THEME.label));
  if (fault || colors === undefined) return fault;
  if (!isRecord(colors)) return 'theme.colors must be an object';
  if (Object.keys(colors).length > THEME.colors.maxProperties) {
    return `theme.colors has more than ${THEME.colors.maxProperties} keys`;
  }
  for (const [key, color] of Object.entries(colors)) {
    const colorFault =
      textFault(`theme.colors key ${key}`, key, THEME.colors.propertyNames) ??
      textFault(`theme.colors.${key}`, color, THEME.colors.additionalProperties);
    if (colorFault) return colorFault;
  }
  return null;
}

/** Why `value` is not a brand, or null when it is one. */
export function brandFault(value: unknown): string | null {
  if (!isRecord(value)) return 'the brand must be an object';
  const { name, logo, href, links, theme } = value;
  return (
    keyFault('the brand', value, BRAND) ??
    (name === undefined ? null : textFault('name', name, BRAND.name)) ??
    (logo === undefined ? null : textFault('logo', logo, BRAND.logo)) ??
    (href === undefined ? null : textFault('href', href, HREF)) ??
    (links === undefined ? null : linksFault(links)) ??
    (theme === undefined ? null : themeFault(theme))
  );
}

/** `value` as a brand. A value that breaks a rule gives no brand at all, never a part of one. */
export function brandOf(value: unknown): Brand {
  return brandFault(value) === null ? (value as Brand) : {};
}
