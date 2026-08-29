// Mounting the SESSION screen, so its tests can look at the screen.
//
// SessionScreen talks to a gateway, a subscription hub, Capacitor and a voice
// recorder, which is why its rules used to be asserted by matching strings in
// its own source. A source match passes for a control that never renders and
// fails for a rename that changed nothing, so the screen is mounted instead:
// the client is a plain object with the members the screen actually calls, and
// every answer is the empty/no-op one unless a test hands over a fact.
import { render } from "@testing-library/react";

import { SessionScreen } from "./SessionScreen";
import type { Session } from "../lib/types";

/** The parts of the gateway client and hub this screen touches. */
type Fake = Record<string, unknown>;

export function sessionFixture(overrides: Partial<Session> = {}): Session {
  return {
    id: "s1",
    title: "A session",
    status: "idle",
    ...overrides,
  } as Session;
}

export function renderSessionScreen({
  session = sessionFixture(),
  client: clientOverrides = {},
  subscriptions: hubOverrides = {},
}: {
  session?: Session;
  client?: Fake;
  subscriptions?: Fake;
} = {}) {
  const nothing = () => null;
  const known: Fake = {
    base: "http://gateway.example.com",
    // Everything the screen reads from its cache on the first render.
    cachedSession: () => session,
    cachedQueuedTurns: () => [],
    cachedSentAttachments: () => [],
    // Everything it may ask the gateway for while the test looks at it.
    session: () => Promise.resolve(session),
    transcript: () => Promise.resolve([]),
    transcriptWindow: () => Promise.resolve([]),
    transcriptIfMoved: () => Promise.resolve(null),
    capabilities: () => Promise.resolve(null),
    voiceModel: () => Promise.resolve(null),
    turnTrace: () => Promise.resolve(null),
    submitTurn: () => Promise.resolve(null),
    cancelTurn: () => Promise.resolve(null),
    cancelCurrentTurn: () => Promise.resolve(null),
    setSetting: () => Promise.resolve(null),
    createSession: () => Promise.resolve(session),
    noteSessionModel: () => {},
    rememberRunningTurn: () => {},
    rememberSentAttachments: () => {},
    forgetQueuedTurn: () => {},
    onArtifactRevision: () => () => {},
    // Retaining an attachment hands back the RELEASE, synchronously.
    retainAttachment: () => () => {},
    attachmentUrl: () => Promise.resolve(null),
    ...clientOverrides,
  };
  // Anything else the screen or one of its children reaches for answers the way
  // an empty gateway does: a cache knows nothing, a request resolves to nothing.
  // Naming every member instead would make this harness a second, staler copy
  // of the client's surface.
  const client = new Proxy(known, {
    get(target, key: string) {
      if (key in target) return target[key];
      return key.startsWith("cached")
        ? nothing
        : () => Promise.resolve([]);
    },
    has: () => true,
  });
  const subscriptions: Fake = {
    hasEndedTurn: () => false,
    resync: () => {},
    subscribeSession: () => () => {},
    subscribeConnection: () => () => {},
    ...hubOverrides,
  };

  const screenFor = (sid: string) => (
    <SessionScreen
      client={client as never}
      subscriptions={subscriptions as never}
      sid={sid}
      onBack={() => {}}
      onOpenSession={() => {}}
    />
  );
  const view = render(screenFor(session.id));
  return Object.assign(view, {
    rerenderSession: (sid: string) => view.rerender(screenFor(sid)),
  });
}
