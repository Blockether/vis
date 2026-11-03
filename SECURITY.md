# Security policy

## Supported versions

We fix security problems in these versions:

| Version | Supported |
| --- | --- |
| The latest stable release (`vX.Y.Z`) | Yes |
| The current native beta (`beta-<commit>`) | Yes |
| Older stable releases and betas | No |
| The `dev` track (JVM source from `main`) | Fixes go to `main` |

Run `vis-agent update` to get the latest release before you report a problem.

## Report a vulnerability

Do not open a public issue, discussion or pull request for a security problem.

Use one of these private channels:

1. Use [Report a vulnerability](https://github.com/Blockether/vis/security/advisories/new) on GitHub. This is the preferred channel.
2. Send an e-mail to `contact@blockether.com`. Put `SECURITY` in the subject.

Include this data when you can:

- The Vis version (`vis-agent --version`) and your operating system.
- The affected component, for example the gateway, the jail or the installer.
- The steps that reproduce the problem, and its effect.
- A proof of concept, logs or a patch.

Remove secrets, tokens and private data from logs before you send them.

## Response times

| Step | Target |
| --- | --- |
| Acknowledgement of your report | 3 business days |
| First assessment and severity | 7 days |
| Fix or published advisory | 90 days |

We tell you about the progress until the problem is fixed. If a fix needs more time, we agree on a new date with you.

## Scope

These parts of Vis are in scope:

- The engine and the gateway, including authentication and the HTTP and event API.
- The process jail and the Python sandbox.
- The installer, `vis-agent update` and the integrity of release assets.
- The Companion apps (desktop, iOS and Android), the web app and the Companion relay.
- Extension loading and the extension SDK.

These items are out of scope:

- A problem in a third-party extension, MCP server or model provider. Report it to its owner.
- An action that a model does when you gave it the permission, for example a shell command in an unjailed session.
- A problem that needs a compromised operating system account or physical access to the device.

## Disclosure

1. We confirm the problem and prepare a fix in private.
2. We publish the fix in a release.
3. We publish a [GitHub Security Advisory](https://github.com/Blockether/vis/security/advisories) and request a CVE when it applies.
4. The release notes name the advisory.

We give credit to the reporter in the advisory, unless you ask us not to.

## Verify a release

Each release, native beta and installer publishes a `SHA256SUMS` file. The installer and `vis-agent update` refuse a download that does not match it.

Each asset also has a [GitHub artifact attestation](https://docs.github.com/en/actions/how-tos/secure-your-work/use-artifact-attestations). Sigstore signs it with the identity of the release workflow in this repository. Releases published before this policy have no attestation.

To verify a downloaded asset, run this command with the [GitHub CLI](https://cli.github.com/):

```bash
gh attestation verify vis-agent-macos-arm64.tar.gz --repo Blockether/vis
```

The command fails if the file changed after the release workflow published it.

The attestation proves which workflow, commit and tag published the asset. The macOS assets are built on a self-hosted runner, so the attestation does not prove an isolated build.

The macOS runtime and the macOS desktop app also have an Apple Developer ID signature and an Apple notarization. The Windows desktop installer has an Authenticode signature.
