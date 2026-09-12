# Exporting sessions

Export a saved session to share a conversation, review its tool calls or keep a
readable copy. Vis stores sessions locally and can export them as Markdown or HTML.

```bash
vis-agent sessions list
vis-agent sessions export <SESSION-ID> [--md | --html PATH]
```

`<SESSION-ID>` is the full id or any unambiguous prefix from `sessions list`.

## Markdown

Markdown is the default format. The export prints the transcript, including tool calls, to stdout:

```bash
vis-agent sessions export 3a7b2c1d > session.md
```

## HTML

Use `--html` for a self-contained page you can open in a browser. Vis creates
missing directories and adds `.html` if the output path has no extension:

```bash
vis-agent sessions export 3a7b2c1d --html report.html
```

Exports are not redacted. Read one before sharing it; see
[Reporting a bug](reporting-bugs.md).

## See also

- [Reporting a bug](reporting-bugs.md) — remove private information before sharing an export.
- [Desktop and mobile setup](index.md#connecting-the-companion-app) — access sessions on another machine.
