# White label

Give the Vis web app your own name, logo, header links, and theme. Use a white label when you serve
Vis to your team or your customers under your own brand.

## When to use

- **You want your logo and your name in the web app.** Write [a brand](#what-a-brand-has), then
  choose [where to set it](#where-to-set-the-brand).
- **You want links to your own pages in the header.** Add [icon links](#icon-links).
- **You want one fixed look.** Give the brand [one theme](#one-theme). The web app then shows no
  theme choice.
- **You build the web app yourself.** Read [Put the brand in the
  build](#put-the-brand-in-the-build).

To set the brand of a running gateway from a program, read [White label API](white-label-api.md).
To install or serve the web app, read [Runtime distributions](distributions.md).

## What a brand has

A brand is one JSON object. Each key is optional, and a key that you omit keeps the Vis default.

```json
{
  "name": "Acme",
  "logo": "/acme-logo.svg",
  "href": "https://acme.example",
  "links": [
    {"icon": "book", "label": "Handbook", "href": "https://acme.example/handbook"},
    {"icon": "chat", "label": "Support", "href": "https://acme.example/support"}
  ],
  "theme": {
    "base": "tokyonight-night",
    "label": "Acme Night",
    "colors": {"--bg": "#101010", "--primary": "#e11d48"}
  }
}
```

| Key | Result |
|---|---|
| `name` | The name beside the logo and in the page title. It has at most 24 characters. |
| `logo` | The picture in the header, on the start screens, and on the browser tab. |
| `href` | The address that the logo opens in a new tab. It starts with `http://` or `https://`. |
| `links` | Up to six [icon links](#icon-links) in the header. |
| `theme` | The [only theme](#one-theme) of the web app. |

A `logo` is one of these values:

- An `https://` address.
- A path of the web app, such as `/acme-logo.svg`.
- A base64 `data:image/` URL of a PNG, JPEG, WebP, or SVG picture. The value has at most 262,144
  characters.

A brand with a logo and no name shows no name in the header. Omit the name when the logo already
holds it.

## Where the brand shows

| Place | What changes |
|---|---|
| Header | The logo, the name, the address behind them, and the icon links. |
| Browser tab | The page title and the tab icon. |
| Start, empty, and error screens | The logo. |
| Settings | The **Theme** section is absent while the brand has a theme. |

The brand shows in the web app that a gateway serves at its own address. The Companion app on a
phone, the terminal, and an HTML transcript export keep the Vis look.

The brand replaces the name only in the header and in the page title. Other messages of the web app
still use the name Vis.

The brand does not rename the coding agent. To give the agent one name that nobody can change, read
[One fixed name](configuration.md#one-fixed-name).

## Icon links

Each link needs all three of these keys:

| Key | Value |
|---|---|
| `icon` | One of the icon names below. |
| `label` | The name of the link, at most 40 characters. It is the tooltip and the name for a screen reader. |
| `href` | The address, which starts with `http://` or `https://`. It opens in a new tab. |

The links stand before the search control, in the order of the list. A window narrower than 640
pixels, such as a phone, hides them.

| Topic | Icon names |
|---|---|
| Work | `app`, `review`, `checklist`, `dashboard`, `activity`, `chart` |
| Code | `code`, `terminal`, `files`, `search`, `git`, `branch` |
| People and time | `calendar`, `clock`, `mail`, `chat`, `users`, `globe` |
| Systems | `database`, `cloud`, `server`, `shield`, `lock`, `key` |
| Other | `settings`, `tools`, `rocket`, `flask`, `music`, `image`, `book`, `lightning` |

## One theme

A brand theme starts from a theme that Vis ships and replaces some of its colors.

| Key | Value |
|---|---|
| `base` | The theme to start from. This key is necessary. |
| `colors` | CSS variables of that theme, each with a `#rrggbb` color. |
| `label` | Your name for the theme. |

The values of `base` are `blockether-light`, `blockether-dark`, `high-contrast-dark`, `paper`,
`solarized-dark`, `solarized-light`, `tokyonight-day`, `tokyonight-moon`, `tokyonight-night`,
`tokyonight-storm`, `vis-dark`, and `vis-light`.

Each key of `colors` is a variable of the base theme, for example `--bg`, `--fg`, `--primary`,
`--accent`, or `--link-fg`. The file `apps/vis-companion/src/lib/themes.generated.css` in the Vis
repository lists all variables of each theme. The `--bg` color is also the color of the browser bar
on a phone.

While the brand has a theme, the web app has no other theme. The settings show no **Theme**
section, and the theme that a person chose before does not apply. When you remove the theme from
the brand, that choice applies again.

## Where to set the brand

Do not put private data in a brand. The web app reads the brand before it connects, so each person
who can reach the gateway address can read it.

| Place | Use it when | How |
|---|---|---|
| The gateway | You run a released Vis, or you want to change the brand without a build. | [White label API](white-label-api.md) |
| The build | You build the web app yourself and want the brand inside the files. | [Put the brand in the build](#put-the-brand-in-the-build) |

If you set both, each key of the gateway brand replaces the same key of the build brand.

The gateway saves its brand in `~/.vis/gateway/web-brand.json`. An open page shows a changed brand
after a reload.

## Put the brand in the build

You need a checkout of the Vis repository, Node.js, and npm.

1. Save the brand as a JSON file, for example `brand.json`.
2. If the logo is a path of the web app, put the picture in `apps/vis-companion/public/`.
3. Build the web app with the `VIS_WEB_BRAND` variable:

   ```bash
   cd apps/vis-companion
   npm ci
   VIS_WEB_BRAND=/path/to/brand.json npm run build:web
   ```

4. Serve the build. Set `VIS_WEB_DIR` to `apps/vis-companion/dist-web` before the gateway starts.
   [Runtime distributions](distributions.md) explains this variable.

A relative `VIS_WEB_BRAND` path starts at `apps/vis-companion`. A brand that breaks a rule stops
the build, and the message names the key.

## Troubleshooting

- **The page still shows the old brand.** Reload the page. An open page reads the brand only when
  it loads.
- **The build stops with a `VIS_WEB_BRAND` message.** Correct the key that the message names. The
  tables on this page give the rule of each key.
- **The gateway refuses the theme.** The `base` is not a theme that Vis ships, or a key of `colors`
  is not a variable of that theme.
- **The header shows no icon links.** Make the window wider than 640 pixels.

## See also

- To read, set, and remove the brand of a gateway from Python or with HTTP requests, read [White
  label API](white-label-api.md).
- To install the web app and serve your own build, read [Runtime distributions](distributions.md).
- To give the coding agent one name that nobody can change, read
  [Configuration](configuration.md#one-fixed-name).
- To run a shared gateway for your team, read [Running a gateway](gateway-service.md).
