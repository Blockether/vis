import type { ReactNode } from 'react';
import { SettingsBandsOpen } from '../screens/settings/SettingsLayout';

/**
 * Opens every settings band below it. Settings opens with every band folded; a story
 * or test about what a band holds uses this to show that content at once.
 */
export function OpenBands({ children }: { children: ReactNode }) {
  return <SettingsBandsOpen.Provider value>{children}</SettingsBandsOpen.Provider>;
}
