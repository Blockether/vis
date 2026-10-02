import { useCallback, useEffect, useRef, useState } from 'react';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import { sameValue, settingValue } from '../../lib/settings-model';
import type { SettingChange, SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';

export function useSettingsDraft(
  client: GatewayClient,
  target: SettingsTarget,
  contextSessionId?: string,
  hasRawDrafts = false,
  onApplied?: () => void,
) {
  const scope = target.scope;
  const targetId = target.target_id;
  const [data, setData] = useState<SettingsResponse | null>(() => client.cachedSettings(target));
  const [changes, setChanges] = useState<Record<string, SettingChange>>({});
  const [latest, setLatest] = useState<SettingsResponse | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});
  const [busy, setBusy] = useState(false);
  const [loaded, setLoaded] = useState(false);
  const [saved, setSaved] = useState(false);
  const state = useRef({ dirty: false, busy: false, revision: data?.revision, epoch: 0 });
  const dirty = Object.keys(changes).length > 0 || hasRawDrafts;
  useEffect(() => {
    state.current = { ...state.current, dirty, busy, revision: data?.revision };
  }, [dirty, busy, data?.revision]);
  useEffect(() => {
    const controller = new AbortController();
    let reading = false;
    const refresh = async () => {
      if (reading || state.current.busy) return;
      reading = true;
      const epoch = state.current.epoch;
      try {
        const next = await client.settings(
          controller.signal,
          { scope, target_id: targetId },
          contextSessionId,
        );
        if (controller.signal.aborted || epoch !== state.current.epoch) return;
        if (!state.current.dirty) {
          setData(next);
          setLatest(null);
        } else if (next.revision !== state.current.revision) setLatest(next);
        setLoaded(true);
        if (!state.current.dirty) setError(null);
      } catch (err) {
        if (!controller.signal.aborted) {
          setLoaded(false);
          setError((err as Error).message);
        }
      } finally {
        reading = false;
      }
    };
    void refresh();
    const timer = window.setInterval(() => void refresh(), 3000);
    return () => {
      controller.abort();
      window.clearInterval(timer);
    };
  }, [client, scope, targetId, contextSessionId]);
  const stage = useCallback(
    (setting: Toggle, change: SettingChange) => {
      setSaved(false);
      setFieldErrors((current) => {
        const next = { ...current };
        delete next[setting.id];
        return next;
      });
      setChanges((current) => {
        const next = { ...current };
        if (
          (change.action === 'inherit' && !setting.is_override) ||
          (change.action === 'value' &&
            setting.is_override &&
            sameValue(change.value, settingValue(setting)))
        )
          delete next[setting.id];
        else next[setting.id] = change;
        state.current.dirty = Object.keys(next).length > 0 || hasRawDrafts;
        return next;
      });
    },
    [hasRawDrafts],
  );
  const discard = useCallback(() => {
    setChanges({});
    setFieldErrors({});
    setSaved(false);
    state.current.dirty = false;
    if (latest) {
      setData(latest);
      setLatest(null);
    }
  }, [latest]);
  const reviewLatest = () => {
    if (latest) {
      setData(latest);
      setLatest(null);
      setError(null);
    }
  };
  const apply = async () => {
    if (!data?.revision || busy || !dirty || latest || !loaded) return;
    state.current.busy = true;
    state.current.epoch += 1;
    setBusy(true);
    setError(null);
    setFieldErrors({});
    try {
      const next = await client.applySettings(data.revision, Object.values(changes), {
        scope, target_id: targetId,
      }, contextSessionId);
      setData(next);
      setChanges({});
      setLatest(null);
      setSaved(true);
      state.current.dirty = false;
      state.current.revision = next.revision;
      onApplied?.();
    } catch (err) {
      setError((err as Error).message);
      if (err instanceof GatewayError) {
        const body = err.body as
          { id?: string; field_errors?: Record<string, string>; error?: unknown } | undefined;
        const detail = (body?.error && typeof body.error === 'object' ? body.error : body) as
          { id?: string; field_errors?: Record<string, string> } | undefined;
        if (detail?.field_errors) setFieldErrors(detail.field_errors);
        else if (detail?.id) setFieldErrors({ [detail.id]: err.message });
        if (err.status === 409) {
          try {
            setLatest(
              await client.settings(undefined, { scope, target_id: targetId }, contextSessionId),
            );
          } catch {
            setLoaded(false);
          }
        } else if (err.status === 0) setLoaded(false);
      }
    } finally {
      state.current.busy = false;
      setBusy(false);
    }
  };
  return {
    data,
    changes,
    latest,
    error,
    fieldErrors,
    busy,
    loaded,
    dirty,
    saved,
    stage,
    discard,
    reviewLatest,
    apply,
  };
}
