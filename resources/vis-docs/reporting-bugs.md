# Reporting a bug

Report bugs at <https://github.com/Blockether/vis/issues>.

Session transcripts can contain private code and credentials. Include only
information needed to reproduce the Vis problem.

## When to use

- **Vis crashed, stopped responding or gave a wrong result, and you can make it
  happen again.** Collect [what to include](#what-to-include) and fill in the
  [template](#template).
- **You found a security problem**, such as a sandbox escape or a credential leak.
  Do not open a public issue. Follow [Security issues](#security-issues).
- **The report needs a transcript or logs.** Remove private code and credentials
  first, as described in [What to leave out](#what-to-leave-out) and [Sharing a
  transcript](#sharing-a-transcript).

If your own extension does not load, start with [Extension
troubleshooting](extension-troubleshooting.md).

## Security issues

Do not open a public issue for a vulnerability such as a sandbox escape,
credential leak or unauthenticated gateway access. Email
<security@blockether.com> with the affected version.

## What to include

1. `vis-agent --version`, your OS and whether you run the native binary or the
   JVM build.
2. The output of `vis-agent doctor`. Credentials are redacted. Check the paths it
   prints.
3. What you did, what happened and what you expected.
4. A minimal reproduction, ideally in an empty directory.
5. The relevant part of your config with secrets replaced by `${ENV_VAR}`.
6. The error text and the lines around it, not the whole log. See
   [Logs and diagnostics](logging.md) for file locations and retention.

## What to leave out

- API keys and tokens, credential-bearing configuration such as `state.yml`,
  `gateway.token`, `devices.edn`, session databases such as `vis.mdb`, and raw
  event journals in `~/.vis/gateway/events/`.
- Private source code, diffs and API details. Use a minimal public example instead.
- Employer, client and product names, internal hostnames and private URLs.
- Personal data. Public issues are permanent and indexed.
- Home paths such as `/Users/jane/work/acme/…`. Write `<project>/…`.

## Sharing a transcript

If the report needs a transcript to show the bug, export it and redact it:

```bash
vis-agent sessions export <SESSION-ID> --md > /tmp/report.md
```

Exports are not redacted. Before you share an export, do these steps:

1. Read the file from start to end.
2. Keep only the turns that show the bug.
3. Replace project names, hosts and paths.
4. Search for `key`, `token`, `secret`, `password`, `https://` and your company name.

Screenshots and recordings show your real files. Use a scratch project or crop
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
