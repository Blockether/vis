import { useCallback, useEffect, useRef, useState } from 'react';

import type { GatewayConn, SpeechPrefs, ThemePref } from '../lib/types';
import { applyTheme } from '../lib/theme';
import {
  setPythonCodeShown,
  setStepsSummarized,
  usePythonCodeShown,
  useStepsSummarized,
} from '../lib/transcript-display';
import { DEFAULT_SPEECH_PREFS, getSpeechPrefs, getThemePref, setThemePref } from '../lib/storage';
import { speechOutput } from '../lib/speech';
import { MinusIcon, PlusIcon } from '../components/icons';
import { DEFAULT_THEME, THEMES, type ThemeChoice } from '../lib/themes.generated';
import {
  Banner,
  Button,
  ChoiceCell,
  DialogFrame,
  IconButton,
  Modal,
  Select,
  Switch,
  Text,
} from '../components/ui';
import { AddMachine, MachineRows, useFleetHealth } from '../components/Machines';
import { DiagnosticsPanel } from './settings/DiagnosticsPanel';
import { MachineSettings } from './settings/MachineSettings';
import { SettingsPanel } from './settings/SettingsLayout';
import { SETTINGS_CATEGORIES, type SettingsCategory } from '../lib/settings-model';
import type { SettingsLeaveGuard } from './settings/SettingsEditor';

/** A machine's identity across an address change: a URL is a property of it, not it. */
function machineId(conn: GatewayConn): string {
  return conn.id ?? conn.url;
}

/** Task navigation keeps one machine or device editor visible at a time. */
export function SettingsDialog({
  gateways,
  primaryUrl,
  providerMachineUrl,
  onAddMachine,
  onMakePrimary,
  onRename,
  onRemove,
  onSelectAddress,
  onClose,
  contextSession,
}: {
  gateways: GatewayConn[];
  primaryUrl?: string | null;
  /** Open this machine immediately; Providers is the first panel under its row. */
  providerMachineUrl?: string;
  /** Pairing is setup, and setup happens HERE — never by leaving this dialog. */
  onAddMachine: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  /**
   * A machine's own verbs act on the ROW they came out of, and every one of them
   * names its machine. They used to act on whichever machine the column happened
   * to be READING, because they were controls in that machine's own panel — so a
   * fleet's verbs all pointed at one row, and the row under the thumb was not it.
   */
  onMakePrimary?: (conn: GatewayConn) => void | Promise<void>;
  onRename?: (conn: GatewayConn, label: string | undefined) => void | Promise<void>;
  onRemove?: (conn: GatewayConn) => void | Promise<void>;
  /**
   * Bind one machine to a different address. It acts on the ROW it came out of —
   * the machine's own address line — and never on another machine's.
   */
  onSelectAddress?: (conn: GatewayConn, url: string, pinned: boolean) => void | Promise<void>;
  onClose: () => void;
  /** The session open behind this dialog: its machine locks the rows that session's own scopes decide. */
  contextSession?: { url: string; sid: string };
}) {
  const showPythonCode = usePythonCodeShown();
  const summarizeSteps = useStepsSummarized();
  const [pref, setPref] = useState<ThemePref>(DEFAULT_THEME.id);
  const [speechPrefs, setSpeechPrefs] = useState<SpeechPrefs>(DEFAULT_SPEECH_PREFS);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  // Pairing opens INSIDE this dialog, as the first band of the machines column:
  // see the panel under that column's heading, and the band's ＋ that is its only door.
  const [selectedId, setSelectedId] = useState(() =>
    machineId(
      gateways.find(
        (conn) => conn.url === (providerMachineUrl ?? contextSession?.url ?? primaryUrl),
      ) ??
        gateways[0] ?? { url: '' },
    ),
  );
  const [pane, setPane] = useState<'config' | 'device' | 'machines'>(
    gateways.length ? 'config' : 'machines',
  );
  const [category, setCategory] = useState<SettingsCategory>(
    providerMachineUrl ? 'response' : 'basic',
  );
  const selectedMachine = gateways.find((conn) => machineId(conn) === selectedId) ?? gateways[0];
  const guard = useRef<SettingsLeaveGuard | null>(null);
  const setGuard = useCallback((next: SettingsLeaveGuard | null) => {
    guard.current = next;
  }, []);
  const navigate = useCallback((action: () => void) => {
    if (guard.current) guard.current(action);
    else action();
  }, []);
  const close = useCallback(() => navigate(onClose), [navigate, onClose]);
  const addRef = useRef<HTMLDivElement | null>(null);
  const [isAdding, setIsAdding] = useState(false);

  // The form opens at the TOP of a column that scrolls itself, so a fleet already
  // scrolled past its first rows would otherwise answer the ＋ off-screen.
  useEffect(() => {
    if (isAdding) addRef.current?.scrollIntoView({ block: 'nearest', behavior: 'auto' });
  }, [isAdding]);

  useEffect(() => {
    let cancelled = false;
    void (async () => {
      const [theme, speech] = await Promise.all([getThemePref(), getSpeechPrefs()]);
      if (cancelled) return;
      setPref(theme);
      setSpeechPrefs(speech);
    })();
    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key !== 'Escape' || event.defaultPrevented) return;
      // One Escape, one surface: the pairing form closes first, or adding a machine
      // and reading its settings ended on the same keystroke.
      if (isAdding) {
        setIsAdding(false);
        return;
      }
      close();
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [isAdding, close]);

  async function chooseTheme(next: ThemeChoice) {
    setPending(`theme:${next.id}`);
    try {
      await setThemePref(next.id);
      setPref(next.id);
      applyTheme(next);
    } catch (e) {
      setErr((e as Error).message);
    } finally {
      setPending(null);
    }
  }

  async function changeSpeech(write: () => Promise<void>): Promise<SpeechPrefs> {
    await write();
    const next = await getSpeechPrefs();
    speechOutput.apply(next);
    setSpeechPrefs(next);
    return next;
  }

  // The diagnostics band keeps its own fold at EVERY width: its facts are support
  // material this device reads a few times a year, not a setting it changes.
  const [diagOpen, setDiagOpen] = useState(false);

  const { health, retry } = useFleetHealth(gateways);

  return (
    <Modal size="full" onDismiss={close}>
      <DialogFrame title="Settings" onClose={close}>
        <div className="flex min-h-0 flex-1 flex-col sm:flex-row">
          <nav
            aria-label="Settings navigation"
            className="shrink-0 border-b border-dialog-edge bg-panel-2 p-3 sm:w-56 sm:overflow-y-auto sm:border-b-0 sm:border-r"
          >
            <div className="space-y-3">
              <Select
                aria-label="Settings machine"
                value={machineId(selectedMachine ?? { url: '' })}
                options={gateways.map((conn) => ({
                  value: machineId(conn),
                  label: conn.label || conn.url,
                }))}
                onValueChange={(value) =>
                  navigate(() => {
                    setSelectedId(value);
                    setPane('config');
                  })
                }
              />
              <div className="sm:hidden">
                <Select
                  aria-label="Settings page"
                  value={pane === 'config' ? category : pane}
                  options={[
                    ...SETTINGS_CATEGORIES.filter(([id]) => id !== 'appearance').map(
                      ([value, label]) => ({ value, label }),
                    ),
                    { value: 'device', label: 'This device' },
                    { value: 'machines', label: 'Manage machines' },
                  ]}
                  onValueChange={(value) => {
                    if (value === 'device' || value === 'machines') navigate(() => setPane(value));
                    else if (pane === 'config') setCategory(value as SettingsCategory);
                    else
                      navigate(() => {
                        setCategory(value as SettingsCategory);
                        setPane('config');
                      });
                  }}
                />
              </div>
              <div className="hidden gap-1 sm:flex sm:flex-col">
                {SETTINGS_CATEGORIES.filter(([id]) => id !== 'appearance').map(([id, label]) => (
                  <Button
                    key={id}
                    variant="secondary"
                    density="panel"
                    className="justify-start"
                    aria-pressed={pane === 'config' && category === id}
                    onClick={() => {
                      if (pane === 'config') setCategory(id);
                      else
                        navigate(() => {
                          setCategory(id);
                          setPane('config');
                        });
                    }}
                  >
                    {label}
                  </Button>
                ))}
                <Button
                  variant="secondary"
                  density="panel"
                  className="justify-start"
                  aria-pressed={pane === 'device'}
                  onClick={() => navigate(() => setPane('device'))}
                >
                  This device
                </Button>
                <Button
                  variant="secondary"
                  density="panel"
                  className="justify-start"
                  aria-pressed={pane === 'machines'}
                  onClick={() => navigate(() => setPane('machines'))}
                >
                  Manage machines
                </Button>
              </div>
            </div>
          </nav>
          <div className="flex min-h-0 min-w-0 flex-1 flex-col">
            {pane === 'config' &&
              (selectedMachine ? (
                <MachineSettings
                  key={`${machineId(selectedMachine)}:${selectedMachine.url}`}
                  gateway={selectedMachine}
                  speechPrefs={speechPrefs}
                  onSpeechChange={changeSpeech}
                  category={category}
                  onCategoryChange={setCategory}
                  onLeaveGuard={setGuard}
                  contextSessionId={
                    contextSession?.url === selectedMachine.url ? contextSession.sid : undefined
                  }
                />
              ) : (
                <div className="space-y-3 p-6">
                  <Text as="p" variant="description">
                    Pair a machine to edit its settings.
                  </Text>
                  <Button
                    variant="secondary"
                    onClick={() => {
                      setPane('machines');
                      setIsAdding(true);
                    }}
                  >
                    Add a machine
                  </Button>
                </div>
              ))}
            {pane === 'machines' && (
              <div className="min-h-0 flex-1 overflow-y-auto">
                <SettingsPanel
                  title="Machines"
                  headingLevel={3}
                  action={
                    <IconButton
                      variant="quiet"
                      align="trailing"
                      label={isAdding ? 'Cancel adding a machine' : 'Add a machine'}
                      aria-expanded={isAdding}
                      onClick={() => setIsAdding((open) => !open)}
                    >
                      {isAdding ? (
                        <MinusIcon className="size-4" />
                      ) : (
                        <PlusIcon className="size-4" />
                      )}
                    </IconButton>
                  }
                >
                  {isAdding && (
                    <div ref={addRef} className="p-3 sm:p-4">
                      <AddMachine
                        onAdd={async (conn, makeActive) => {
                          await onAddMachine(conn, makeActive);
                          setIsAdding(false);
                          setSelectedId(machineId(conn));
                          setPane('config');
                        }}
                      />
                    </div>
                  )}
                  {gateways.length > 0 ? (
                    <MachineRows
                      conns={gateways}
                      alignMenuWithHeader
                      openUrls={new Set()}
                      primaryUrl={primaryUrl}
                      health={health}
                      onPick={(conn) => {
                        setSelectedId(machineId(conn));
                        setPane('config');
                      }}
                      onRetry={retry}
                      onMakePrimary={onMakePrimary}
                      onRename={onRename}
                      onForget={onRemove}
                      onSelectAddress={onSelectAddress}
                    />
                  ) : (
                    <div className="p-6">
                      <Text as="p" variant="description">No machines paired. Add one to begin.</Text>
                    </div>
                  )}
                </SettingsPanel>
              </div>
            )}
            {pane === 'device' && (
              <div className="min-h-0 flex-1 overflow-y-auto">
                <div className="border-b border-dialog-edge px-3 py-3 sm:px-4">
                  <Text as="h3" variant="section">
                    This device
                  </Text>
                  <Text as="p" variant="description">
                    Appearance and response display apply immediately. They do not change any
                    machine or session.
                  </Text>
                </div>
                {err && (
                  <div className="p-3 sm:p-4">
                    <Banner kind="err">{err}</Banner>
                  </div>
                )}

                <SettingsPanel title="Responses" headingLevel={3}>
                  <div className="divide-y divide-dialog-edge">
                    <div className="flex items-center justify-between gap-4 px-3 py-3 sm:px-4">
                      <div className="min-w-0 space-y-1">
                        <Text as="p" variant="label">
                          Show Python code and results
                        </Text>
                        <Text as="p" variant="description">
                          Show source code and raw results before Activity. Turn off to show only
                          Activity.
                        </Text>
                      </div>
                      <Switch
                        label="Show Python code and results"
                        isOn={showPythonCode}
                        onClick={() => setPythonCodeShown(!showPythonCode)}
                      />
                    </div>
                    <div className="flex items-center justify-between gap-4 px-3 py-3 sm:px-4">
                      <div className="min-w-0 space-y-1">
                        <Text as="p" variant="label">
                          Summarize steps between notes
                        </Text>
                        <Text as="p" variant="description">
                          Combine the steps between progress notes into one Activity, during and
                          after a turn. Turn off to show Activity for each step.
                        </Text>
                      </div>
                      <Switch
                        label="Summarize steps between notes"
                        isOn={summarizeSteps}
                        onClick={() => setStepsSummarized(!summarizeSteps)}
                      />
                    </div>
                  </div>
                </SettingsPanel>
                <SettingsPanel title="Theme" headingLevel={3}>
                  <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                    {/* NO MODE COLUMN. Every theme is named `Blockether Light`, `Solarized
                    Dark`, `Vis Light`, so a trailing `light`/`dark` restated the last word
                    of its own row six times down the list. The name is the whole answer. */}
                    {THEMES.map((choice) => (
                      <ChoiceCell
                        key={choice.id}
                        title={choice.label}
                        variant="list"
                        isSelected={pref === choice.id}
                        isLeaf
                        disabled={pending?.startsWith('theme:') ?? false}
                        onClick={() => void chooseTheme(choice)}
                      />
                    ))}
                  </div>
                </SettingsPanel>
                <DiagnosticsPanel isOpen={diagOpen} onToggle={() => setDiagOpen((open) => !open)} />
              </div>
            )}
          </div>
        </div>
      </DialogFrame>
    </Modal>
  );
}
