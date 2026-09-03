import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import { GatewayClient, GatewayError } from "../../lib/gateway";
import type {
  GatewayCapabilities,
  SpeechPrefs,
  SpeechVoice,
  SpeechVoices,
  VoiceEngineAbsence,
  VoiceModelState,
} from "../../lib/types";
import {
  SPEECH_RATES,
  setSpeechAsrEngine,
  setSpeechDeviceVoice,
  setSpeechGatewayVoice,
  setSpeechRate,
  setSpeechTtsEngine,
} from "../../lib/storage";
import { speechOutput } from "../../lib/speech";
import { DownloadIcon, PlayIcon, StopIcon } from "../../components/icons";
import {
  bestDeviceVoices,
  deviceVoices,
  iosVoiceDownloadGuidance,
  type DeviceVoice,
} from "../../lib/speech-voices";
import { onWake } from "../../lib/wake";
import {
  Banner,
  Button,
  ChoiceCell,
  ConfirmRow,
  Input,
  SettingsChoiceDisclosure,
  SettingsChoiceGroup,
  SettingsDisclosure,
} from "../../components/ui";
import { FormLabel, SettingsPanel } from "./SettingsLayout";

/**
 * ONE ENGINE'S VOICES, immediately under the engine that owns them.
 *
 * A cloning engine speaks by imitating a reference recording, so a voice IS a clip and
 * "create a voice" is an upload and nothing else. The clip is stored on the machine that
 * imported it and every session there speaks with the same catalogue.
 *
 * A machine whose built-in speaking runtime is unavailable renders NOTHING. Speech is
 * optional in use; a group explaining a capability that cannot run is noise.
 */
export function VoicesPanel({
  client,
  prefs,
  engine = prefs.ttsEngine,
  onChange,
}: {
  client: GatewayClient;
  prefs: SpeechPrefs;
  engine?: string | null;
  onChange: SaveSpeechPrefs;
}) {
  const [catalogue, setCatalogue] = useState<SpeechVoices | null>(null);
  const [isAbsent, setIsAbsent] = useState(false);
  const [err, setErr] = useState<string | null>(null);
  const [note, setNote] = useState<string | null>(null);
  const [clip, setClip] = useState<File | null>(null);
  const [voiceName, setVoiceName] = useState("");
  const [language, setLanguage] = useState("");
  const [says, setSays] = useState("");
  const [pending, setPending] = useState<string | null>(null);
  const [confirming, setConfirming] = useState<string | null>(null);
  const [confirmingInstall, setConfirmingInstall] = useState<string | null>(
    null,
  );
  // Which of THIS machine's voices this device asks for. Stored by id: a machine
  // that no longer has it speaks in its selected engine's default instead.
  const fileRef = useRef<HTMLInputElement>(null);
  // Which voice this device is auditioning right now. One at a time on purpose: the
  // player has one output, so a second press replaces the sound instead of layering it.
  const [playing, setPlaying] = useState<string | null>(null);
  const auditionRef = useRef<AbortController | null>(null);
  const cancelAudition = useCallback((resetControl = true) => {
    const controller = auditionRef.current;
    if (!controller) return;
    auditionRef.current = null;
    controller.abort();
    speechOutput.stop();
    if (resetControl) setPlaying(null);
  }, []);
  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const answer = await client.speechVoices({
          signal,
          engine,
        });
        if (signal?.aborted) return;
        setCatalogue(answer);
        setIsAbsent(false);
        setErr(null);
      } catch (e) {
        if (signal?.aborted) return;
        // A 501 means this gateway's built-in speech runtime is unavailable. That
        // is not worth a red banner because speech is not required to use Vis, so
        // the whole band goes away.
        if (e instanceof GatewayError && e.status === 501) {
          setIsAbsent(true);
          setCatalogue(null);
          setErr(null);
          return;
        }
        setErr((e as Error).message);
      }
    },
    [client, engine],
  );

  useEffect(() => {
    const controller = new AbortController();
    void load(controller.signal);
    return () => controller.abort();
  }, [load]);

  useEffect(() => () => cancelAudition(false), [cancelAudition]);

  useEffect(() => {
    if (
      !catalogue?.voices?.some((voice) => voice.model?.status === "downloading")
    )
      return;
    const timer = window.setTimeout(() => void load(), ENGINE_POLL_MS);
    return () => window.clearTimeout(timer);
  }, [catalogue, load]);

  async function chooseVoice(id: string | null) {
    await onChange(() => setSpeechGatewayVoice(id));
  }

  /**
   * The audition: bytes for ONE voice, played on this device.
   *
   * Nothing is stored by listening and no preference is spent, so a catalogue can be
   * heard before it is chosen — which is the whole point, since choosing a voice that
   * has to be downloaded first costs 60-100 MB.
   */
  async function playSample(voice: SpeechVoice) {
    cancelAudition();
    const controller = new AbortController();
    auditionRef.current = controller;
    setPlaying(voice.id);
    setErr(null);
    try {
      const audio = await client.speechVoiceSample(voice.id, {
        signal: controller.signal,
        engine,
      });
      if (controller.signal.aborted || auditionRef.current !== controller)
        return;
      await speechOutput.playSample(audio);
    } catch (e) {
      if (controller.signal.aborted || auditionRef.current !== controller)
        return;
      setErr((e as Error).message);
    } finally {
      if (auditionRef.current === controller) {
        auditionRef.current = null;
        setPlaying(null);
      }
    }
  }
  function chooseClip(file: File | null) {
    setClip(file);
    setNote(null);
    setErr(null);
    if (fileRef.current && !file) fileRef.current.value = "";
    if (file && !voiceName.trim()) {
      setVoiceName(
        file.name
          .replace(/\.[^.]+$/, "")
          .replace(/[_-]+/g, " ")
          .trim(),
      );
    }
  }

  async function importClip() {
    if (!clip || !voiceName.trim()) return;
    setPending("import");
    try {
      const voice = await client.importSpeechVoice(
        clip,
        {
          name: voiceName.trim(),
          lang: language.trim() || undefined,
          text: says.trim() || undefined,
        },
        { engine },
      );
      setNote(`${voice.label ?? voice.id} can speak on this machine now.`);
      chooseClip(null);
      setVoiceName("");
      setLanguage("");
      setSays("");
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function forget(voice: SpeechVoice) {
    setPending(voice.id);
    try {
      await client.forgetSpeechVoice(voice.id, { engine });
      setConfirming(null);
      setNote(null);
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function prepareVoice(voice: SpeechVoice, isLicenseAccepted: boolean) {
    setPending(`install:${voice.id}`);
    setErr(null);
    try {
      const state = await client.speechModel({
        start: true,
        engine,
        voice: voice.id,
        isLicenseAccepted,
      });
      setConfirmingInstall(null);
      if (state.status === "failed") {
        setErr(state.error ?? `Could not download ${voice.label ?? voice.id}.`);
        setNote(null);
      } else {
        setNote(
          state.status === "ready"
            ? `${voice.label ?? voice.id} is ready.`
            : `${voice.label ?? voice.id} is downloading.`,
        );
      }
      await load();
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  if (isAbsent) return null;

  const voices = catalogue?.voices ?? [];
  const canImport = catalogue?.engine?.is_voice_import === true;

  return (
    <SettingsChoiceGroup label="Voices" isNested>
      {/* A VOICE IS A ROW, NOT A CARD. Reported over this screen: every voice sat in
          its own hairline box inside a padded box inside the panel, three frames
          deep, and the band spent two lines saying where a DIFFERENT band lives.
          The list divides on one rule like every other list here, and the panel's
          one verb is its last row. */}
      <div className="divide-y divide-dialog-edge">
        {(err || note) && (
          <div className="space-y-2 p-3">
            {err && <Banner kind="err">{err}</Banner>}
            {note && <Banner kind="ok">{note}</Banner>}
          </div>
        )}

        {catalogue === null && !err && (
          <p className="px-3 py-5 font-mono text-meta text-dialog-hint sm:px-4">
            Reading this machine's voices…
          </p>
        )}

        {catalogue && voices.length === 0 && (
          <p className="px-3 py-5 font-mono text-meta text-dialog-hint sm:px-4">
            {canImport
              ? "No voice yet — import a recording and it becomes one."
              : "This engine speaks in no named voice."}
          </p>
        )}

        {voices.length > 0 && (
          <ChoiceCell
            className="w-full"
            isLeaf
            title="Engine default"
            sub="whatever this machine picks"
            isSelected={prefs?.gatewayVoice == null}
            onClick={() => void chooseVoice(null)}
          />
        )}

        {voices.map((voice) => {
          const name = voice.label ?? voice.id;
          const isPlaying = playing === voice.id;
          const model = voice.model;
          const canPrepare =
            model?.status === "absent" || model?.status === "failed";
          const isDownloading = model?.status === "downloading";
          const hasDownloadAction = canPrepare || isDownloading;
          const modelWord =
            model?.status === "downloading"
              ? `${model.phase === "extracting" ? "unpacking" : "downloading"}${
                  typeof model.progress === "number"
                    ? ` ${Math.round(model.progress)}%`
                    : ""
                }`
              : model?.status === "absent"
                ? "not downloaded yet"
                : model?.status === "failed"
                  ? "failed"
                  : model?.status === "ready"
                    ? "ready"
                    : null;
          // Once the model is local, a ready or cheap-to-prepare sample can be heard without
          // changing the choice. Until then, the model download is the row's leading action.
          const canHear = !!(
            voice.is_sample_ready || voice.is_sample_preparable
          );
          const hasTrailing = !!voice.is_imported;
          const leadingAction = hasDownloadAction
            ? {
                label: isDownloading
                  ? `Downloading ${name}`
                  : model?.status === "failed"
                    ? `Retry download ${name}`
                    : `Download ${name}`,
                icon: <DownloadIcon className="size-3" />,
                disabled: isDownloading || pending === `install:${voice.id}`,
                onClick: () => {
                  cancelAudition();
                  if (!canPrepare) return;
                  if (voice.is_opt_in) setConfirmingInstall(voice.id);
                  else void prepareVoice(voice, false);
                },
              }
            : canHear
              ? {
                  label: isPlaying
                    ? `Stop the sample of ${name}`
                    : `Play a sample of ${name}`,
                  icon: isPlaying ? (
                    <StopIcon className="size-3" />
                  ) : (
                    <PlayIcon className="size-3" />
                  ),
                  onClick: () => {
                    if (isPlaying) cancelAudition();
                    else void playSample(voice);
                  },
                }
              : undefined;
          return (
            <div key={voice.id}>
              <div
                className={
                  hasTrailing
                    ? "grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 pr-3"
                    : "min-w-0"
                }
              >
                <ChoiceCell
                  className="w-full min-w-0"
                  isLeaf
                  title={name}
                  sub={[
                    voice.language,
                    voice.is_imported
                      ? "imported here"
                      : (modelWord ?? "ships with the engine"),
                  ]
                    .filter(Boolean)
                    .join(" · ")}
                  isSelected={prefs?.gatewayVoice === voice.id}
                  showSelectionMark={!model || model.status === "ready"}
                  leadingAction={leadingAction}
                  disabled={!!model && model.status !== "ready"}
                  onClick={() => {
                    void chooseVoice(voice.id);
                    if (canHear) void playSample(voice);
                  }}
                />
                {hasTrailing && (
                  <div className="flex shrink-0 items-center gap-2">
                    {voice.is_imported && confirming !== voice.id && (
                      <Button
                        variant="secondary"
                        onClick={() => setConfirming(voice.id)}
                      >
                        Forget
                      </Button>
                    )}
                  </div>
                )}
              </div>
              {model?.status === "failed" && model.error && (
                <div className="border-t border-dialog-edge p-3">
                  <Banner kind="err">{model.error}</Banner>
                </div>
              )}
              {confirmingInstall === voice.id && (
                <div className="border-t border-dialog-edge">
                  <div className="space-y-2 px-3 pt-3 font-mono text-meta text-dialog-hint sm:px-4">
                    <p>
                      {voice.notice ??
                        `This voice requires acceptance of ${voice.license}.`}
                    </p>
                    {voice.license && (
                      <p className="font-bold text-white">{voice.license}</p>
                    )}
                    {voice.source_url && (
                      <a
                        className="block truncate text-accent underline"
                        href={voice.source_url}
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        Read the source and licence
                      </a>
                    )}
                  </div>
                  <ConfirmRow
                    question={`Accept ${voice.license ?? "these terms"} and download ${voice.label ?? voice.id}?`}
                    confirmLabel="Accept and download"
                    isBusy={pending === `install:${voice.id}`}
                    onKeep={() => setConfirmingInstall(null)}
                    onConfirm={() => void prepareVoice(voice, true)}
                  />
                </div>
              )}
              {confirming === voice.id && (
                <ConfirmRow
                  question={`Forget ${voice.label ?? voice.id}?`}
                  confirmLabel="Forget"
                  isBusy={pending === voice.id}
                  onKeep={() => setConfirming(null)}
                  onConfirm={() => void forget(voice)}
                />
              )}
            </div>
          );
        })}
      </div>

      {canImport && (
        <>
          <input
            ref={fileRef}
            type="file"
            accept="audio/*"
            className="hidden"
            aria-label="Recording to import as a voice"
            onChange={(event) => chooseClip(event.target.files?.[0] ?? null)}
          />
          {clip === null ? (
            <Button
              variant="primary"
              density="panel"
              className="w-full justify-center"
              onClick={() => fileRef.current?.click()}
            >
              Import a voice…
            </Button>
          ) : (
            <div className="space-y-2 p-3">
              <FormLabel
                label="Recording"
                hint="Ten to thirty seconds of clear speech is plenty."
              >
                <p className="truncate font-mono text-meta text-white">
                  {clip.name}
                </p>
              </FormLabel>
              <FormLabel label="Name">
                <Input
                  value={voiceName}
                  placeholder="What to call this voice"
                  onChange={(event) => setVoiceName(event.target.value)}
                />
              </FormLabel>
              <FormLabel
                label="Language"
                hint="Optional — the tag this clip speaks in, like en or en-GB."
              >
                <Input
                  value={language}
                  placeholder="en"
                  onChange={(event) => setLanguage(event.target.value)}
                />
              </FormLabel>
              <FormLabel
                label="What the clip says"
                hint="Optional, and worth typing: the model is TOLD these words, so the clone tracks the voice instead of guessing them."
              >
                <Input
                  value={says}
                  placeholder="Transcript of the recording"
                  onChange={(event) => setSays(event.target.value)}
                />
              </FormLabel>
              <div className="flex flex-wrap gap-2">
                <Button
                  variant="primary"
                  disabled={!voiceName.trim() || pending === "import"}
                  onClick={() => void importClip()}
                >
                  {pending === "import" ? "Importing…" : "Import"}
                </Button>
                <Button variant="quiet" onClick={() => chooseClip(null)}>
                  Cancel
                </Button>
              </div>
            </div>
          )}
        </>
      )}
    </SettingsChoiceGroup>
  );
}

/** One persisted speech preference write, shared by every open machine panel. */
export type SaveSpeechPrefs = (
  write: () => Promise<void>,
) => Promise<SpeechPrefs>;

/** How often a moving model refreshes its own progress. */
const ENGINE_POLL_MS = 1200;

/** What each device speed sounds like, so the number is not the only thing on the cell. */
const SPEECH_RATE_WORDS: Record<string, string> = {
  "0.85": "unhurried",
  "1": "natural",
  "1.2": "brisk",
};

/** One stable line lets the ear compare device voices rather than compare wording. */
const DEVICE_VOICE_SAMPLE = "This is what this voice sounds like.";

type EngineReading = {
  state: VoiceModelState | null;
  /** Set when the direction has NO engine at all — 501, with whatever failed to load. */
  absence: VoiceEngineAbsence | null;
  error: string | null;
};

type EngineReadings = Record<string, EngineReading>;

type EngineChoice = { id: string; label?: string };

function selectedEngine(
  requested: string | null,
  fallback: string | null | undefined,
  engines: EngineChoice[],
): string | null {
  if (requested && engines.some((engine) => engine.id === requested))
    return requested;
  if (fallback && engines.some((engine) => engine.id === fallback))
    return fallback;
  return engines[0]?.id ?? null;
}

/** A machine engine is local TO THE GATEWAY, which the Companion names explicitly. */
function gatewayEngineName(engine: EngineChoice): string {
  return (engine.label?.trim() || engine.id)
    .replace(/\s*\((?:local|gateway)\)\s*$/i, "")
    .replace(/[-_\s]+local$/i, "")
    .trim();
}

function gatewayEngineLabel(engine: EngineChoice): string {
  return `${gatewayEngineName(engine)} (gateway)`;
}
function engineWord(reading: EngineReading | null): string {
  if (reading === null) return "checking…";
  if (reading.absence) return "not installed";
  if (reading.error) return "cannot be read";
  switch (reading.state?.status) {
    case "ready":
      return "ready";
    case "downloading": {
      const action =
        reading.state.phase === "extracting" ? "unpacking" : "downloading";
      return typeof reading.state.progress === "number"
        ? `${action} ${Math.round(reading.state.progress)}%`
        : action;
    }
    case "failed":
      return "failed";
    case "absent":
      return "not downloaded yet";
    default:
      return "unavailable";
  }
}

/** One engine's preparation action and exceptional detail, directly under its own row. */
function EngineProblem({
  engineName,
  reading,
  isBusy,
  onPrepare,
}: {
  engineName: string;
  reading: EngineReading | null;
  isBusy: boolean;
  onPrepare: () => void;
}) {
  const state = reading?.state ?? null;
  const canPrepare =
    reading !== null &&
    !reading.absence &&
    (state?.status === "absent" ||
      state?.status === "failed" ||
      !!reading.error);
  if (
    !reading?.absence &&
    !reading?.error &&
    state?.status !== "failed" &&
    !canPrepare
  ) {
    return null;
  }
  const isDownload = state?.status === "absent";
  return (
    <div className="space-y-2 border-t border-dialog-edge px-3 py-3 sm:px-4">
      {reading?.absence && (
        <p className="font-mono text-chip text-dialog-hint">
          {reading.absence.reasons?.length
            ? reading.absence.reasons.join(" · ")
            : "This machine has no engine for this direction installed."}
        </p>
      )}
      {state?.status === "failed" && state.error && (
        <Banner kind="err">{state.error}</Banner>
      )}
      {reading?.error && <Banner kind="err">{reading.error}</Banner>}
      {canPrepare && (
        <Button
          variant="primary"
          disabled={isBusy}
          aria-label={`${isDownload ? "Download" : "Retry"} ${engineName} model`}
          onClick={onPrepare}
        >
          {isBusy ? "Asking…" : isDownload ? "Download model" : "Try again"}
        </Button>
      )}
    </div>
  );
}

/**
 * THE TWO SPEECH DIRECTIONS, each opening onto the concrete engines that can do it.
 *
 * Selection belongs to this device and travels on each request. ASR names one machine
 * engine. TTS puts this device's system engine in the same list as every engine the
 * machine advertised, so there is one choice instead of a separate reply-routing panel.
 */
export function SpeechEnginesPanel({
  client,
  prefs,
  onChange,
}: {
  client: GatewayClient;
  prefs: SpeechPrefs;
  onChange: SaveSpeechPrefs;
}) {
  const [capabilities, setCapabilities] = useState<GatewayCapabilities | null>(
    () => client.cachedCapabilities(),
  );
  const [open, setOpen] = useState<"asr" | "tts" | null>(null);
  const [listening, setListening] = useState<EngineReadings>({});
  const [speaking, setSpeaking] = useState<EngineReadings>({});
  const [voices, setVoices] = useState<DeviceVoice[] | null>(null);
  const [busy, setBusy] = useState<string | null>(null);
  const [openTtsSettings, setOpenTtsSettings] = useState<ReadonlySet<string>>(
    () => new Set(),
  );
  const [err, setErr] = useState<string | null>(null);
  // `undefined` is silence; `null` is the system default actively speaking.
  const [playingDeviceVoice, setPlayingDeviceVoice] = useState<
    string | null | undefined
  >(undefined);
  const deviceAuditionRef = useRef<object | null>(null);
  const cancelDeviceAudition = useCallback((resetControl = true) => {
    if (!deviceAuditionRef.current) return;
    deviceAuditionRef.current = null;
    speechOutput.stop();
    if (resetControl) setPlayingDeviceVoice(undefined);
  }, []);

  const voiceFeature = capabilities?.features?.voice;
  const speechFeature = capabilities?.features?.speech;
  const asrEngines = useMemo(
    () => voiceFeature?.engines ?? [],
    [voiceFeature?.engines],
  );
  const ttsEngines = useMemo(
    () => speechFeature?.engines ?? [],
    [speechFeature?.engines],
  );
  const asrEngine = selectedEngine(
    prefs.asrEngine,
    voiceFeature?.selected,
    asrEngines,
  );
  const chosenTtsEngine =
    prefs.ttsEngine &&
    ttsEngines.some((engine) => engine.id === prefs.ttsEngine)
      ? prefs.ttsEngine
      : null;

  useEffect(() => {
    const controller = new AbortController();
    void client
      .capabilities(controller.signal)
      .then((answer) => {
        if (!controller.signal.aborted) {
          setCapabilities(answer);
          setErr(null);
        }
      })
      .catch((cause: unknown) => {
        if (!controller.signal.aborted) setErr((cause as Error).message);
      });
    return () => controller.abort();
  }, [client]);

  useEffect(() => {
    let isLive = true;
    const refresh = () => {
      void deviceVoices()
        .then((list) => {
          if (isLive) setVoices(list);
        })
        .catch(() => {
          if (isLive) setVoices([]);
        });
    };
    refresh();
    const stopRefreshingOnWake = onWake(refresh);
    return () => {
      isLive = false;
      stopRefreshingOnWake();
    };
  }, []);

  useEffect(() => () => cancelDeviceAudition(false), [cancelDeviceAudition]);

  const readOne = useCallback(
    async (ask: () => Promise<VoiceModelState>): Promise<EngineReading> => {
      try {
        return { state: await ask(), absence: null, error: null };
      } catch (cause) {
        if (cause instanceof GatewayError && cause.status === 501) {
          return {
            state: null,
            absence: {
              error: cause.message,
              reasons: (cause.body as VoiceEngineAbsence | undefined)?.reasons,
            },
            error: null,
          };
        }
        return { state: null, absence: null, error: (cause as Error).message };
      }
    },
    [],
  );

  const readDirection = useCallback(
    async (
      engines: EngineChoice[],
      ask: (engine: string | null) => Promise<VoiceModelState>,
    ): Promise<EngineReadings> => {
      const ids: Array<string | null> =
        engines.length > 0 ? engines.map(({ id }) => id) : [null];
      const answers = await Promise.all(
        ids.map(
          async (engine) =>
            [engine ?? "", await readOne(() => ask(engine))] as const,
        ),
      );
      return Object.fromEntries(answers);
    },
    [readOne],
  );

  const loadModels = useCallback(
    async (signal?: AbortSignal) => {
      const [heard, spoken] = await Promise.all([
        readDirection(asrEngines, (engine) =>
          client.voiceModel({ signal, engine }),
        ),
        readDirection(ttsEngines, (engine) =>
          client.speechModel({ signal, engine }),
        ),
      ]);
      if (signal?.aborted) return;
      setListening(heard);
      setSpeaking(spoken);
    },
    [asrEngines, client, readDirection, ttsEngines],
  );

  useEffect(() => {
    const controller = new AbortController();
    void loadModels(controller.signal);
    return () => controller.abort();
  }, [loadModels]);

  const isMoving = [
    ...Object.values(listening),
    ...Object.values(speaking),
  ].some((reading) => reading.state?.status === "downloading");
  useEffect(() => {
    if (!isMoving) return;
    const timer = window.setInterval(() => void loadModels(), ENGINE_POLL_MS);
    return () => window.clearInterval(timer);
  }, [isMoving, loadModels]);

  async function choose(direction: "asr" | "tts", engine: string | null) {
    try {
      await onChange(() =>
        direction === "asr"
          ? setSpeechAsrEngine(engine)
          : setSpeechTtsEngine(engine),
      );
      setErr(null);
    } catch (cause) {
      setErr((cause as Error).message);
    }
  }

  async function chooseDeviceSetting(write: () => Promise<void>) {
    try {
      await onChange(write);
      setErr(null);
    } catch (cause) {
      setErr((cause as Error).message);
    }
  }

  async function playDeviceVoice(voiceId: string | null) {
    cancelDeviceAudition();
    const audition = {};
    deviceAuditionRef.current = audition;
    setPlayingDeviceVoice(voiceId);
    setErr(null);
    try {
      await speechOutput.playDeviceSample(
        DEVICE_VOICE_SAMPLE,
        voiceId,
        prefs.rate,
      );
    } catch (cause) {
      if (deviceAuditionRef.current !== audition) return;
      setErr((cause as Error).message);
    } finally {
      if (deviceAuditionRef.current === audition) {
        deviceAuditionRef.current = null;
        setPlayingDeviceVoice(undefined);
      }
    }
  }

  function toggleTtsSettings(engine: string) {
    setOpenTtsSettings((current) => {
      const next = new Set(current);
      if (next.has(engine)) next.delete(engine);
      else next.add(engine);
      return next;
    });
  }

  async function prepare(direction: "asr" | "tts", engine: string) {
    const busyKey = `${direction}:${engine}`;
    setBusy(busyKey);
    try {
      const state =
        direction === "asr"
          ? await client.voiceModel({ start: true, engine })
          : await client.speechModel({ start: true, engine });
      const reading: EngineReading = { state, absence: null, error: null };
      if (direction === "asr") {
        setListening((current) => ({ ...current, [engine]: reading }));
      } else {
        setSpeaking((current) => ({ ...current, [engine]: reading }));
      }
    } catch (cause) {
      const failed: EngineReading = {
        state: null,
        absence: null,
        error: (cause as Error).message,
      };
      if (direction === "asr") {
        setListening((current) => ({ ...current, [engine]: failed }));
      } else {
        setSpeaking((current) => ({ ...current, [engine]: failed }));
      }
    } finally {
      setBusy(null);
    }
  }

  const asrChoice = asrEngines.find((engine) => engine.id === asrEngine);
  const asrLabel = asrChoice
    ? gatewayEngineLabel(asrChoice)
    : (asrEngine ?? (capabilities ? "Not installed" : "Checking…"));
  const ttsChoice = ttsEngines.find((engine) => engine.id === chosenTtsEngine);
  const deviceList = bestDeviceVoices(voices ?? [], prefs.deviceVoice);
  const voiceDownloadGuidance = iosVoiceDownloadGuidance();
  const chosenDeviceVoice = deviceList.find(
    (voice) => voice.id === prefs.deviceVoice,
  );
  const ttsLabel = ttsChoice
    ? gatewayEngineLabel(ttsChoice)
    : chosenDeviceVoice
      ? `This device · ${chosenDeviceVoice.label}`
      : "This device";

  return (
    <SettingsPanel title="Speech engines">
      <div className="divide-y divide-dialog-edge">
        {err && (
          <div className="p-3 sm:p-4">
            <Banner kind="err">{err}</Banner>
          </div>
        )}

        <div>
          <SettingsDisclosure
            label="ASR"
            value={asrLabel}
            isOpen={open === "asr"}
            aria-controls="speech-asr-engines"
            onClick={() =>
              setOpen((current) => (current === "asr" ? null : "asr"))
            }
          />
          {open === "asr" && (
            <div
              id="speech-asr-engines"
              className="border-t border-dialog-edge"
            >
              {asrEngines.length > 0 ? (
                <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                  {asrEngines.map((engine) => {
                    const reading = listening[engine.id] ?? null;
                    return (
                      <div key={engine.id} className="grid bg-input">
                        <ChoiceCell
                          title={gatewayEngineLabel(engine)}
                          sub={engineWord(reading)}
                          isSelected={engine.id === asrEngine}
                          isLeaf
                          onClick={() => void choose("asr", engine.id)}
                        />
                        <EngineProblem
                          engineName={gatewayEngineName(engine)}
                          reading={reading}
                          isBusy={busy === `asr:${engine.id}`}
                          onPrepare={() => void prepare("asr", engine.id)}
                        />
                      </div>
                    );
                  })}
                </div>
              ) : (
                <>
                  <p className="px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                    No ASR engine is registered on this machine.
                  </p>
                  <EngineProblem
                    engineName="ASR"
                    reading={listening[""] ?? null}
                    isBusy={false}
                    onPrepare={() => undefined}
                  />
                </>
              )}
            </div>
          )}
        </div>

        <div>
          <SettingsDisclosure
            label="TTS"
            value={ttsLabel}
            isOpen={open === "tts"}
            aria-controls="speech-tts-engines"
            onClick={() =>
              setOpen((current) => (current === "tts" ? null : "tts"))
            }
          />
          {open === "tts" && (
            <div
              id="speech-tts-engines"
              className="border-t border-dialog-edge"
            >
              <SettingsChoiceGroup label="TTS engines">
                <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                  <div data-speech-engine="device" className="grid bg-input">
                    <SettingsChoiceDisclosure
                      title="This device"
                      sub={chosenDeviceVoice?.label ?? "system TTS"}
                      isSelected={chosenTtsEngine === null}
                      isOpen={openTtsSettings.has("device")}
                      controls="speech-tts-settings-device"
                      onSelect={() => void choose("tts", null)}
                      onToggle={() => toggleTtsSettings("device")}
                    />
                    {openTtsSettings.has("device") && (
                      <div id="speech-tts-settings-device" className="grid">
                        {voices === null && (
                          <p className="border-t border-dialog-edge px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                            Asking this device what it can speak in…
                          </p>
                        )}
                        {voices !== null && voices.length === 0 && (
                          <p className="border-t border-dialog-edge px-3 py-4 font-mono text-chip text-dialog-hint sm:px-4">
                            This device has no system TTS engine installed.
                          </p>
                        )}
                        {voices !== null && voices.length > 0 && (
                          <SettingsChoiceGroup label="Voices" isNested>
                            <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                              <ChoiceCell
                                title="System default"
                                sub="the voice this device prefers"
                                isSelected={prefs.deviceVoice === null}
                                isLeaf
                                leadingAction={{
                                  label:
                                    playingDeviceVoice === null
                                      ? "Stop the sample of System default"
                                      : "Play a sample of System default",
                                  icon:
                                    playingDeviceVoice === null ? (
                                      <StopIcon className="size-3" />
                                    ) : (
                                      <PlayIcon className="size-3" />
                                    ),
                                  onClick: () => {
                                    if (playingDeviceVoice === null)
                                      cancelDeviceAudition();
                                    else void playDeviceVoice(null);
                                  },
                                }}
                                onClick={() =>
                                  void chooseDeviceSetting(() =>
                                    setSpeechDeviceVoice(null),
                                  )
                                }
                              />
                              {deviceList.map((voice) => {
                                const isPlaying =
                                  playingDeviceVoice === voice.id;
                                return (
                                  <ChoiceCell
                                    key={voice.id}
                                    title={voice.label}
                                    sub={[
                                      voice.language,
                                      voice.isDefault ? "device default" : null,
                                    ]
                                      .filter(Boolean)
                                      .join(" · ")}
                                    isSelected={prefs.deviceVoice === voice.id}
                                    isLeaf
                                    leadingAction={{
                                      label: isPlaying
                                        ? `Stop the sample of ${voice.label}`
                                        : `Play a sample of ${voice.label}`,
                                      icon: isPlaying ? (
                                        <StopIcon className="size-3" />
                                      ) : (
                                        <PlayIcon className="size-3" />
                                      ),
                                      onClick: () => {
                                        if (isPlaying) cancelDeviceAudition();
                                        else void playDeviceVoice(voice.id);
                                      },
                                    }}
                                    onClick={() =>
                                      void chooseDeviceSetting(() =>
                                        setSpeechDeviceVoice(voice.id),
                                      )
                                    }
                                  />
                                );
                              })}
                            </div>
                            {voiceDownloadGuidance && (
                              <p className="border-t border-dialog-edge p-3 font-mono text-meta text-dialog-hint sm:p-4">
                                {voiceDownloadGuidance}
                              </p>
                            )}
                          </SettingsChoiceGroup>
                        )}
                        <SettingsChoiceGroup label="Speech rate" isNested>
                          <div className="grid grid-cols-3 gap-px bg-dialog-edge">
                            {SPEECH_RATES.map((rate) => (
                              <ChoiceCell
                                key={rate}
                                title={`${rate}×`}
                                sub={SPEECH_RATE_WORDS[String(rate)] ?? "speed"}
                                isSelected={prefs.rate === rate}
                                onClick={() =>
                                  void chooseDeviceSetting(() =>
                                    setSpeechRate(rate),
                                  )
                                }
                              />
                            ))}
                          </div>
                        </SettingsChoiceGroup>
                        {ttsEngines.length === 0 && (
                          <EngineProblem
                            engineName="TTS"
                            reading={speaking[""] ?? null}
                            isBusy={false}
                            onPrepare={() => undefined}
                          />
                        )}
                      </div>
                    )}
                  </div>
                  {ttsEngines.map((engine) => {
                    const reading = speaking[engine.id] ?? null;
                    const isSelected = engine.id === chosenTtsEngine;
                    const isSettingsOpen = openTtsSettings.has(engine.id);
                    const settingsId = `speech-tts-settings-${engine.id}`;
                    return (
                      <div
                        key={engine.id}
                        data-speech-engine={engine.id}
                        className="grid bg-input"
                      >
                        <SettingsChoiceDisclosure
                          title={gatewayEngineLabel(engine)}
                          sub={engineWord(reading)}
                          isSelected={isSelected}
                          isOpen={isSettingsOpen}
                          controls={settingsId}
                          onSelect={() => void choose("tts", engine.id)}
                          onToggle={() => toggleTtsSettings(engine.id)}
                        />
                        {isSettingsOpen && (
                          <div id={settingsId} className="grid">
                            <EngineProblem
                              engineName={gatewayEngineName(engine)}
                              reading={reading}
                              isBusy={busy === `tts:${engine.id}`}
                              onPrepare={() => void prepare("tts", engine.id)}
                            />
                            <VoicesPanel
                              client={client}
                              prefs={prefs}
                              engine={engine.id}
                              onChange={onChange}
                            />
                          </div>
                        )}
                      </div>
                    );
                  })}
                </div>
              </SettingsChoiceGroup>
            </div>
          )}
        </div>
      </div>
    </SettingsPanel>
  );
}
