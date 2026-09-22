import { useCallback, useEffect, useRef, useState } from 'react';

import type { GatewayConn, SpeechPrefs, ThemePref } from '../lib/types';
import { applyTheme } from '../lib/theme';
import { usePythonCodeShown, setPythonCodeShown } from '../lib/transcript-display';
import { DEFAULT_SPEECH_PREFS, getSpeechPrefs, getThemePref, setThemePref } from '../lib/storage';
import { speechOutput } from '../lib/speech';
import { CloseIcon, PlusIcon } from '../components/icons';
import { DEFAULT_THEME, THEMES, type ThemeChoice } from '../lib/themes.generated';
import { Banner, ChoiceCell, DialogFrame, IconButton, Modal, Switch, Text } from '../components/ui';
import { AddMachine, MachineRows, useFleetHealth } from '../components/Machines';
import { DiagnosticsPanel } from './settings/DiagnosticsPanel';
import { MachineSettings } from './settings/MachineSettings';
import { SettingsColumn, SettingsPanel } from './settings/SettingsLayout';

/** A machine's identity across an address change: a URL is a property of it, not it. */
function machineId(conn: GatewayConn): string {
  return conn.id ?? conn.url;
}

/**
 * SETTINGS IS ONE PLACE: this device on the left, the machines on the right.
 *
 * There used to be two settings dialogs that could never be open at once —
 * `Application settings` behind the cog in the bar, `Machine settings` behind a
 * machine's `⋯` three screens away — so "where do I change this?" was answered by
 * remembering which of two doors a choice lived behind, and pairing a machine was
 * filed under the device while the machine it produced was filed somewhere else.
 *
 * One dialog, two columns, one rule between them. MACHINES owns the fleet — which
 * machines this device is paired with, how to add another, and what each of them
 * decides — and APPLICATION owns what this copy of Vis decides (its theme).
 * Machines leads, because the cog is opened to reach a machine far more often than to
 * repaint the app, and below `sm:` the columns stack in that same order.
 *
 * A sole online machine stays fully open, with no disclosure control. With several
 * machines, each row opens its own settings independently. Unavailable rows retry
 * the connection instead of disclosing settings.
 */
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
}) {
  const showPythonCode = usePythonCodeShown();
  const [pref, setPref] = useState<ThemePref>(DEFAULT_THEME.id);
  const [speechPrefs, setSpeechPrefs] = useState<SpeechPrefs>(DEFAULT_SPEECH_PREFS);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  // Pairing opens INSIDE this dialog, as the first band of the machines column:
  // see the panel under that column's heading, and the band's ＋ that is its only door.
  const [isAdding, setIsAdding] = useState(false);
  const addRef = useRef<HTMLDivElement | null>(null);

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
      if (event.key !== 'Escape') return;
      // One Escape, one surface: the pairing form closes first, or adding a machine
      // and reading its settings ended on the same keystroke.
      if (isAdding) {
        setIsAdding(false);
        return;
      }
      onClose();
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [isAdding, onClose]);

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

  // Fleets start closed; MachineRows keeps a sole online machine open automatically.
  // A route from the model picker opens the requested machine directly on Providers.
  const [openIds, setOpenIds] = useState<ReadonlySet<string>>(
    () =>
      new Set(
        gateways.filter((conn) => conn.url === providerMachineUrl).map((conn) => machineId(conn)),
      ),
  );
  const toggleMachine = useCallback((conn: GatewayConn) => {
    setOpenIds((open) => {
      const next = new Set(open);
      if (!next.delete(machineId(conn))) next.add(machineId(conn));
      return next;
    });
  }, []);
  const openUrls = new Set(
    gateways.filter((conn) => openIds.has(machineId(conn))).map((conn) => conn.url),
  );

  // On a phone the columns stack and the machines lead, so the application's own
  // settings fold until asked for; side by side there is room and no fold exists.
  const [appOpen, setAppOpen] = useState(false);

  // The diagnostics band keeps its own fold at EVERY width: its facts are support
  // material this device reads a few times a year, not a setting it changes.
  const [diagOpen, setDiagOpen] = useState(false);

  const { health, retry } = useFleetHealth(gateways);

  return (
    // The app's ONE dialog: `Modal` + `DialogFrame`, the same outer component
    // "Manage projects" and every ask already open in. `wide` is the one size that
    // holds two columns of settings side by side; the height is every dialog's.
    <Modal size="wide" onDismiss={onClose}>
      <DialogFrame title="Settings" onClose={onClose}>
        {/* Each column scrolls ITSELF on desktop. One shared scroller made the short
            column a 1500px empty gutter: scrolling to a machine's Sandbox panel dragged
            Theme off the top of the screen for no reason. Below `sm:` the halves stack
            and the dialog body is the one scroller again. */}
        <div className="grid min-w-0 grid-cols-1 divide-y divide-dialog-edge sm:min-h-0 sm:flex-1 sm:grid-cols-2 sm:divide-x sm:divide-y-0 sm:overflow-hidden">
          <SettingsColumn
            title="Machines"
            action={
              /* ONE DOOR THAT OPENS AND CLOSES: the ＋ becomes an × while the form is
                 open, so the band that put the form there also takes it away. The
                 standard icon box, so the mark centers on the rail the machine rows'
                 menu marks below it center on. */
              <IconButton
                variant="quiet"
                label={isAdding ? 'Cancel adding a machine' : 'Add a machine'}
                title={isAdding ? 'Cancel adding a machine' : 'Add a machine'}
                onClick={() => setIsAdding((adding) => !adding)}
              >
                {isAdding ? <CloseIcon className="size-4" /> : <PlusIcon className="size-4" />}
              </IconButton>
            }
          >
            {/* THE COG'S FIRST ANSWER IS THE FLEET. Reported over the machines screen:
                this should open when I click the cog. It did not — this column held a
                strip of bare machine NAMES and a `Pair machine` button whose only job
                was to CLOSE the dialog and navigate to a screen the app bar has no door
                to, so "which machines does this app know, and how do I add one?" was
                answered nowhere the cog could reach. The list and both ways to pair are
                now the very components that screen is made of: one object, and nothing
                leaves this dialog to reach it. It leads the dialog because it is what
                the cog was opened FOR — below `sm:` the columns stack in that order. */}
            {/* PAIRING IS A BAND IN THIS COLUMN, never a second dialog over the first.
                Reported from the settings dialog: the ＋ opened `Add a machine` as its
                own modal ON TOP of Settings — a dialog inside a dialog, with two close
                marks, two Escape targets, and the fleet the machine was about to join
                greyed out behind it. It opens where it belongs instead: the first band
                under the heading whose ＋ asked for it, carrying the same edge and
                rhythm as every other group of settings. It stays closed until it is
                asked for, so the cog still opens on the fleet it was pressed for. */}
            {isAdding && (
              <div ref={addRef}>
                <SettingsPanel title="Add a machine">
                  <div className="p-3 sm:p-4">
                    <AddMachine
                      onAdd={async (conn, makeActive) => {
                        await onAddMachine(conn, makeActive);
                        setIsAdding(false);
                      }}
                    />
                  </div>
                </SettingsPanel>
              </div>
            )}
            {gateways.length > 0 ? (
              <MachineRows
                conns={gateways}
                openUrls={openUrls}
                primaryUrl={primaryUrl}
                health={health}
                onPick={toggleMachine}
                onRetry={retry}
                onMakePrimary={onMakePrimary}
                onRename={onRename}
                onForget={onRemove}
                onSelectAddress={onSelectAddress}
                renderPanel={(conn) => (
                  <MachineSettings
                    key={machineId(conn)}
                    gateway={conn}
                    speechPrefs={speechPrefs}
                    onSpeechChange={changeSpeech}
                  />
                )}
              />
            ) : (
              <SettingsPanel title="No machine yet">
                <div className="px-4 py-6 text-center">
                  <Text as="p" variant="description">
                    Add a machine above, and its settings live under its own row.
                  </Text>
                </div>
              </SettingsPanel>
            )}
          </SettingsColumn>

          <SettingsColumn
            title="Application"
            disclosure={{
              isOpen: appOpen,
              onToggle: () => setAppOpen((open) => !open),
              label: `${appOpen ? 'Hide' : 'Show'} application settings`,
            }}
          >
            {err && (
              <div className="p-3 sm:p-4">
                <Banner kind="err">{err}</Banner>
              </div>
            )}

            <SettingsPanel title="Transcript">
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
            </SettingsPanel>
            <SettingsPanel title="Theme">
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
          </SettingsColumn>
        </div>
      </DialogFrame>
    </Modal>
  );
}
