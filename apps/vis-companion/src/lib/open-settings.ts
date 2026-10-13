import { createContext } from 'react';
import type { GatewayClient } from './gateway';
import type { SettingsTarget } from './types';

/** Opens Machine settings of the gateway at `base`. Locked inherited rows use it (#373). */
export const OpenMachineSettings = createContext<((base: string) => void) | undefined>(undefined);

/** Opens Project, Group or Session settings over the current settings (#373). */
export const OpenScopedSettings = createContext<
  ((client: GatewayClient, target: SettingsTarget) => void) | undefined
>(undefined);
