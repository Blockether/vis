# Exporting sessions

Every session is stored locally and can be exported as Markdown or HTML.

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

Writes a self-contained, styled page to the given path. Missing directories are
created, and `.html` is added if the path has no extension:

```bash
vis-agent sessions export 3a7b2c1d --html report.html
```

Exports are not redacted. Read one before sharing it; see
[Reporting a bug](reporting-bugs.md).

## See also

- [Reporting a bug](reporting-bugs.md) — remove private information before sharing an export.
- [Remote access and the Companion app](gateway.md) — access sessions on another machine.
