# Reporting a bug

Report a bug at <https://github.com/Blockether/vis/issues>.

A session transcript can contain private code and credentials. Include only the information that
reproduces the Vis problem.

## When to use

- **Vis crashed, stopped responding, or gave a wrong result, and you can make it happen again.**
  Collect [what to include](#what-to-include) and fill in the [template](#template).
- **You found a security problem, such as a sandbox escape or a credential leak.** Do not open a
  public issue. Follow [Security issues](#security-issues).
- **The report needs a transcript or logs.** Remove private code and credentials first. See
  [What to leave out](#what-to-leave-out) and [Sharing a transcript](#sharing-a-transcript).

If your own extension does not load, start with
[Extension troubleshooting](extension-troubleshooting.md).

[Sessions](sessions.md#export-a-session) explains the transcript export. [Configuration](configuration.md)
holds the settings that you report.

## Security issues

Do not open a public issue for a vulnerability, such as a sandbox escape, a credential leak, or
unauthenticated gateway access. Email <security@blockether.com> with the affected version.

## What to include

1. The output of `vis-agent --version`, your OS, and the build that you run: the native binary or
   the JVM build.
2. The output of `vis-agent doctor`. It redacts credentials. Check the paths that it prints.
3. What you did, what happened, and what you expected.
4. A minimal reproduction, in an empty directory if possible.
5. The relevant part of your configuration, with each secret replaced by `${ENV_VAR}`.
6. The error text and the lines around it, not the whole log. [Logs and diagnostics](logging.md)
   gives the file locations and the retention.

## What to leave out

- API keys and tokens.
- Configuration that holds credentials, such as `state.yml`, `gateway.token`, and `devices.edn`.
- Session databases such as `vis.mdb`, and the raw event journals in `~/.vis/gateway/events/`.
- Private source code, diffs, and API details. Use a minimal public example.
- Employer names, client names, product names, internal hostnames, and private URLs.
- Personal data. A public issue is permanent, and search engines index it.
- Home paths such as `/Users/jane/work/acme/…`. Write `<project>/…`.

## Sharing a transcript

If only a transcript shows the bug, export it:

```bash
vis-agent sessions export <SESSION-ID> --md > /tmp/report.md
```

To export from your own code, read [Export a session](sessions-api.md#export-a-session) in the
Sessions API.

Vis does not redact an export. Before you share one, do these steps:

1. Read the file from start to end.
2. Keep only the turns that show the bug.
3. Replace project names, hosts, and paths.
4. Search for `key`, `token`, `secret`, `password`, `https://`, and your company name.

Screenshots and recordings show your real files. Use a scratch project, or crop the image to the
affected part.

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

- To create a transcript export, read [Sessions](sessions.md#export-a-session).
- To export a transcript from a script, read [Sessions API](sessions-api.md#export-a-session).
- To find the settings that you report, read [Configuration](configuration.md).
