# Reporting bugs safely

Bugs go to the public issue tracker at
<https://github.com/Blockether/vis/issues>.

Vis runs *inside* your work: your repository, your prompts, your provider
credentials, your infrastructure. A useful bug report is therefore also a
disclosure risk — the fastest way to file one is to paste a whole session, and
that is exactly the thing you must not do. This page is the recipe for a report
that is reproducible for us and safe for you and your employer.

**Rule of thumb:** a good Vis issue describes *Vis*, not your project. If a
sentence only makes sense to someone who knows your codebase, it probably does
not belong in the issue.

## Security issues are not GitHub issues

If the bug is a **vulnerability** — sandbox escape, credential leakage, a jail
bypass, an unauthenticated gateway path — do **not** open a public issue.
Report it privately to <security@blockether.com> with the affected version or
commit, and let us coordinate disclosure. Everything else (crashes, wrong
output, UI glitches, performance, docs) belongs in the public tracker.

## What to include

Almost every Vis bug is diagnosable from this, and none of it is proprietary:

1. **Version and platform** — `vis-agent --version`, your OS and architecture, and
   whether you run the native binary or the JVM build.
2. **`vis-agent doctor` output.** It is the single highest-signal attachment:
   provider/config/extension/sandbox health with credentials already redacted.
   Still skim it before pasting — it prints paths, and a path can name a
   client.
3. **What you did, what happened, what you expected** — three short
   paragraphs, in that order.
4. **A minimal reproduction**, ideally in a scratch repository (`/tmp/repro`)
   rather than in your real one. If a fresh empty project reproduces it, say
   so; that alone removes most of the privacy problem.
5. **The relevant config**, trimmed to the keys involved, with secrets replaced
   by `${ENV_VAR}` placeholders — which is
   [how Vis wants secrets written anyway](configuration.md).
6. **The error text and, if any, the stack trace** — the lines around the
   failure, not the whole log.

## What to leave out

Never paste, attach, or screenshot:

- **Credentials of any kind** — API keys, OAuth tokens, `Authorization` /
  `Bearer` headers, session cookies, gateway pairing tokens, signing keys.
  Concretely, treat these files as secret and never share them:
  `~/.vis/state.yml` (provider setup and OAuth tokens),
  `~/.vis/gateway.token`, `~/.vis/devices.edn`, `~/.vis/apns/*.p8`,
  `~/.vis/fcm/*.json`.
- **Whole session state.** The session database `~/.vis/vis.mdb` and the
  gateway event logs `~/.vis/gateway/events/*.ndjson` contain complete verbatim
  transcripts — every prompt, file read, diff, and tool result of every session
  on that machine. Never attach them.
- **Raw log dumps.** `~/.vis/vis.log` is a debugging log, not a sanitized one:
  it records prompts, file paths, and command lines. Attach the handful of
  lines around the failure, after reading them.
- **Proprietary source.** Diffs, file contents, schema, internal API shapes.
  If a snippet is genuinely needed, retype it as a neutral minimal example.
- **Commercial context.** Employer, client, product codename, repository name,
  ticket ids, internal hostnames, private registry or Git URLs, VPN or
  bastion addresses, staging domains, seat/licence identifiers.
- **Personal data.** Customer records, emails, names, tokens embedded in
  fixtures — anything that would be a data-protection incident if it were
  indexed by a search engine. A public GitHub issue is permanent and crawled.
- **Absolute home paths**, when avoidable: `/Users/jane.doe/work/acme-core/…`
  leaks your name *and* your client in one line. Write `<project>/src/…`.

## Sanitizing a session before you share it

Sometimes the transcript really is the bug (bad tool call, mangled render, lost
turn). Then export it and edit it — do not hand over the database:

```bash
vis-agent sessions list
vis-agent sessions export <SESSION-ID> --md > /tmp/report.md
```

`vis-agent sessions export` reproduces the conversation **faithfully and without
redaction**. So, before attaching `/tmp/report.md`:

1. Read it end to end. Every line of it.
2. Delete unrelated turns. Keep the shortest span that still shows the bug.
3. Replace project identifiers: repo and product names → `acme`, hostnames →
   `example.internal`, absolute paths → `<project>/…`, people → `Alice`.
4. Search it for `key`, `token`, `secret`, `password`, `Bearer`, `https://`,
   `@`, and your company name. Fix every hit.
5. Prefer pasting the trimmed excerpt into the issue over uploading the file —
   a file is easy to attach unread, and GitHub keeps attachments reachable even
   after an issue is edited or deleted.

If sanitizing takes longer than reproducing, reproduce instead: a five-line
repro in an empty directory is a better bug report than a redacted transcript.

## Screenshots and recordings

The TUI and the companion app show your real work: file names, branch names,
diffs, tool output. Before you screenshot, switch to a scratch project, or crop
to the broken widget. `vis-agent sessions export --mp4` has the same exposure as a
screen recording — it replays the real session.

## A template

```markdown
**Version:** vis-agent 0.42.0 (native, macOS 15.3 arm64)

**What I did:** ran `/reload` after adding a Python extension.
**What happened:** the tool disappeared from the session context.
**Expected:** the tool is re-registered.

**Repro** (empty dir, no project config):
1. mkdir /tmp/repro && cd /tmp/repro
2. mkdir -p .vis/extensions && cp greeter.py .vis/extensions/
3. vis-agent, then /reload

**Extension:** <the 15-line greeter.py from the docs>

**vis-agent doctor:**
<paste>

**Error:**
<the 5 relevant lines>
```

That report contains no credential, no client, and no proprietary code — and it
is enough to fix the bug.
