import type { CapacitorConfig } from '@capacitor/cli';

const config: CapacitorConfig = {
  appId: 'com.blockether.viscompanion',
  appName: 'vis-companion',
  webDir: 'dist',
  server: {
    // The companion talks to the gateway over the network the user configures
    // (LAN, Tailscale, or a cloudflared tunnel). The gateway URL is stored at
    // runtime, so no server URL is baked in here.
    androidScheme: 'https',
  },
  // Register the `vis://` custom scheme so a scanned/opened pairing link
  // (vis://gateway?url=…&token=…) deep-links straight into the app.
  ios: {
    scheme: 'vis',
  },
};

export default config;
