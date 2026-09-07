# Reporting a bug

Report bugs at <https://github.com/Blockether/vis/issues>.

Vis works inside your repository with your credentials, so a session transcript
is rarely safe to share as is. A good report describes Vis, not your project.

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
- Source code, diffs and internal API shapes. Retype a neutral example instead.
- Employer, client and product names, internal hostnames and private URLs.
- Personal data. Public issues are permanent and indexed.
- Home paths such as `/Users/jane/work/acme/…`. Write `<project>/…`.

## Sharing a transcript

If the transcript is the bug, export it and edit it before sharing:

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
**Version:** vis-agent 0.42.0 (native, macOS 15.3 arm64)

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

- [Exporting sessions](exporting-sessions.md) — producing the transcript a report attaches.
- [Configuration](configuration.md) — the settings a report should name.
