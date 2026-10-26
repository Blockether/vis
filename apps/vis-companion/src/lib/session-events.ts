import { createContext } from 'react';

import type { SessionSubscriptionHub } from './subscriptions';

/**
 * The session event stream for surfaces that open far from the session screen,
 * such as an app attachment. Without it a surface reads its state again after
 * each action instead.
 */
export const SessionEventsContext = createContext<SessionSubscriptionHub | null>(null);
