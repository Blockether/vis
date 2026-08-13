export const PUSH_PLUGIN = {
  pkg: '@capacitor/push-notifications',
  classpath: 'com.capacitorjs.plugins.pushnotifications.PushNotificationsPlugin',
};

export function configureAndroidPushPlugin(plugins, enabled) {
  const withoutPush = plugins.filter(({ pkg }) => pkg !== PUSH_PLUGIN.pkg);
  return enabled ? [...withoutPush, PUSH_PLUGIN] : withoutPush;
}
