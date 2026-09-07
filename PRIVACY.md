# Privacy Policy — Vis Gateway Companion

**Last updated:** 2026-09-08
**Applies to:** the Vis Gateway Companion app (`com.blockether.viscompanion`)
for Android, iOS and web, and the `vis-agent` gateway it connects to.
**Provider:** Blockether (contact: contact@blockether.com).

## Short version

The app connects to a gateway you configure and does not require a Blockether
account. Conversation data is sent to that gateway. If you enable push
notifications, the publisher's relay and platform push provider also process
notification data, including an answer preview.

## Data stored on your device

The app stores connection details and preferences in app-private storage
(Capacitor Preferences or browser local storage):

| Data | Purpose | Storage |
| --- | --- | --- |
| Gateway URL(s) and bearer token(s) from pairing | to reach and authenticate against your own gateway | on-device only |
| Selected gateway, theme, UI preferences | to restore your setup | on-device only |
| Cached session titles / messages for display | to render the conversation you opened | on-device only, transient |
| Push token, delivery grant or Web Push subscription *(only if notifications are enabled)* | to deliver alerts to this device | saved on-device; grant or subscription registered with the gateway; native token processed by the relay |

Removing a gateway deletes its saved URL and token. You can clear app or
browser storage to remove locally stored settings and cached data.

## What leaves the device

The app sends prompts, responses and session data to the configured gateway
address over your selected connection, such as a local network, Tailscale or
an HTTPS tunnel.

When notifications are enabled, the app obtains a device token or Web Push
subscription. Native apps request a delivery grant from the publisher's relay
and register that grant with the paired gateway. The relay receives the device
token. The gateway sends notification content to the relay, which forwards it
to Apple Push Notification service or Firebase Cloud Messaging. Browser push
uses the browser's push service.

Notification payloads include the session title, identifiers and an answer
preview of up to 180 characters. They do not include the full transcript.
The relay processes this content; it is not end-to-end encrypted between the
gateway and app. Disabling notifications or removing the gateway unregisters
the device through the app.

If your gateway uses an AI provider, it sends model requests under your
provider account and that provider's privacy policy. The app does not select
or contact model providers independently of the gateway.

## Blockether and third-party services

The app does not include analytics, advertising or crash-reporting SDKs.
Blockether does operate the push relay described above. The relay
implementation uses encrypted delivery grants rather than a device-token
database; it processes device tokens and notification content to deliver
notifications.

Google Play, Apple TestFlight and the App Store may collect installation and
crash statistics under their own policies. Push providers also process delivery
data under their policies.

## Permissions

- **Internet / network access** — to reach the gateway you paired with.
- **Camera** *(optional, only if you use QR pairing)* — the camera frame is
  decoded on-device to read the pairing QR code. No image is stored or uploaded.
- **Notifications** *(optional)* — receive alerts when a turn finishes. The
  gateway generates the content; relay and push services deliver it.

Each is requested only when you use the corresponding feature, and the app works
without the optional ones.

## Children

The app is a developer tool and is not directed at children under 13.

## Security

Gateway credentials are stored in app-private storage. Use HTTPS or a private
network for remote access and require gateway authentication. If a device is
lost, replace the gateway token and update authorized clients. Pairing displays
credentials; it is not a token-revocation command.

## Your rights

You control the data stored in your app and gateway. Remove saved gateways,
clear app storage or delete gateway data when no longer needed. Disable
notifications before removing the app to unregister it from the gateway.
For questions about data processed by the notification relay, contact
contact@blockether.com.

## Open source

The app and relay source are public at <https://github.com/Blockether/vis>,
under `apps/vis-companion/` and `apps/vis-companion-relay/`.

## Changes

Changes to this policy are published in this file and recorded in git history.
Contact: contact@blockether.com.
