# vis React Native companion

Expo/React Native shell for the vis gateway.

Start a vis gateway first (for example with another gateway host), then run the native iOS/Expo dev server:

```sh
cd extensions/common/vis-react-native-companion/app
npm install
EXPO_PUBLIC_VIS_GATEWAY_URL=http://<host>:<port> npm run ios
```

For an iOS Simulator, Xcode + Simulator must be installed and selected with `xcode-select`.
If the gateway requires auth, also pass `EXPO_PUBLIC_VIS_TOKEN=<token>`.
The app talks to the canonical gateway `/v1` JSON API; `/rn/config.json` is only a helper probe.

TypeScript 7 is intentionally kept in `devDependencies`; the typecheck config is named `tsconfig.typecheck.json` because Expo SDK 54 still expects the pre-7 TypeScript module API when loading a root `tsconfig.json`.
