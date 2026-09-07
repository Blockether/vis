# Reporting a bug

Report bugs at <https://github.com/Blockether/vis/issues>.

Session transcripts can contain private code and credentials. Include only
information needed to reproduce the Vis problem.

## Security issues

Do not open a public issue for a vulnerability such as a sandbox escape,
credential leak or unauthenticated gateway access. Email
<security@blockether.com> with the affected version.

## What to include

1. `vis-agent --version`, your OS and whether you run the native binary or the
   JVM build.
2. The output of `vis-agent doctor`. Credentials are redacted; check the paths it
   prints.
3. What you did, what happened and what you expected.
4. A minimal reproduction, ideally in an empty directory.
5. The relevant part of your config with secrets replaced by `${ENV_VAR}`.
6. The error text and the lines around it, not the whole log.

## What to leave out

- API keys, tokens and anything under `~/.vis/` such as `state.yml`,
  `gateway.token`, `devices.edn`, the session database `vis.mdb` and the event
  logs in `gateway/events/`. They contain credentials or full transcripts.
- Private source code, diffs and API details. Use a minimal public example instead.
- Employer, client and product names, internal hostnames and private URLs.
- Personal data. Public issues are permanent and indexed.
- Home paths such as `/Users/jane/work/acme/…`. Write `<project>/…`.

## Sharing a transcript

If a transcript is needed to demonstrate the bug, export and redact it:

```bash
vis-agent sessions export <SESSION-ID> --md > /tmp/report.md
```

Exports are not redacted. Read the file end to end, keep only the turns that
show the bug, replace project names, hosts and paths, and search for `key`,
`token`, `secret`, `password`, `https://` and your company name.

Screenshots and recordings show your real files; use a scratch project or crop
to the affected part.

## Template

```markdown
**Version:** <vis-agent version, native or JVM, OS version, architecture>

**What I did:** ran `/reload` after adding a Python extension.
**What happened:** the tool disappeared from the session.
**Expected:** the tool is registered again.

**Repro** (empty directory, no project config):
1. mkdir /tmp/repro && cd /tmp/repro
2. mkdir -p .vis/extensions && cp greeter.py .vis/extensions/
3. vis-agent, then /reload

**vis-agent doctor:** <paste>

**Error:** <the relevant lines>
```

## See also

- [Exporting sessions](exporting-sessions.md) — create a transcript export.
- [Configuration](configuration.md) — identify relevant settings.
