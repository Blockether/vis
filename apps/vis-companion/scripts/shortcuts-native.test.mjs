import { readFileSync } from 'node:fs';
import { expect, it } from 'vitest';

const read = (path) => readFileSync(new URL(path, import.meta.url), 'utf8');

it('builds the App Intents from the tested Swift core and the gateway handshake', () => {
  const prepare = read('./ios-prepare.mjs');
  for (const name of ['VisJSON', 'VisStructuredOutput', 'VisClient', 'VisShortcutsActions'])
    expect(prepare).toContain(`'${name}'`);
  expect(prepare).toContain('native/shortcuts/Sources/VisShortcutsCore/${name}.swift');
  expect(prepare).toContain('native/ios/VisShortcutsIntents.swift');
  expect(prepare).toContain('packages/vis-contract/resources/vis-contract/schema/gateway.json');
  expect(prepare).toContain('handshake.protocol.const');
  expect(prepare).toContain('handshake.min_gateway.const');
  expect(prepare).toContain('fileOk(shortcutsSwift, shortcutsSource)');
});

it('offers the SDK operations as Shortcut actions', () => {
  const intents = read('../native/ios/VisShortcutsIntents.swift');
  for (const intent of [
    'TalkToVisIntent',
    'AskVisIntent',
    'SendVisRequestIntent',
    'WaitForVisAnswerIntent',
    'GetVisModelsIntent',
    'CreateVisSessionIntent',
    'FindVisSessionsIntent',
    'GetVisSessionModelIntent',
    'SetVisSessionModelIntent',
    'SendToVisIntent',
  ])
    expect(intents).toContain(`struct ${intent}: AppIntent`);
  expect(intents).toContain('responseSchema: responseSchema, maxCorrections: maxCorrections');
  expect(intents).toContain('intent: TalkToVisIntent()');
});
