// Release and push credentials, read the same way by every script: an environment variable
// first, so CI (which has no keychain) injects the same values as GitHub secrets, then the
// macOS login keychain that scripts/secrets.mjs writes. Never a dotfile or a file in this repo.

import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

// `security -w` prints hex whenever the stored secret is not plain printable ASCII, which a
// multi-line PEM never is.
export const unhex = (s) =>
  /^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s;

/** One login-keychain item, or undefined when it is missing or this is not macOS. */
export const keychain = (service, account) => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], {
    encoding: 'utf8',
  });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};

/** The environment variable when it is set, otherwise the keychain item. */
export const secret = (envName, service, account) =>
  process.env[envName]?.trim() || keychain(service, account);

/**
 * The App Store Connect API key (`npm run secrets asc …`). The .p8 contents come from
 * VIS_ASC_KEY, then the file at VIS_ASC_KEY_PATH, then the keychain.
 */
export const ascCredentials = () => ({
  keyId: secret('VIS_ASC_KEY_ID', 'vis-ios', 'asc_key_id'),
  issuerId: secret('VIS_ASC_ISSUER_ID', 'vis-ios', 'asc_issuer_id'),
  keyPem:
    process.env.VIS_ASC_KEY?.trim() ||
    (process.env.VIS_ASC_KEY_PATH
      ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8')
      : keychain('vis-ios', 'asc_key')),
});
