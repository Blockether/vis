# vis React Native companion

Expo/React Native shell for the vis gateway.

Start a vis gateway first (for example with the web companion or another gateway host), then:

```sh
cd extensions/common/vis-react-native-companion/app
npm install
EXPO_PUBLIC_VIS_GATEWAY_URL=http://<host>:<port> npm run start
```

If the gateway requires auth, also pass `EXPO_PUBLIC_VIS_TOKEN=<token>`.
The app talks to the canonical gateway `/v1` JSON API; `/rn/config.json` is only a helper probe.
