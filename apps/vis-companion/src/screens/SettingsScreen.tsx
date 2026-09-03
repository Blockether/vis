import { useCallback, useEffect, useState } from "react";

import type { GatewayConn, SpeechPrefs, ThemePref } from "../lib/types";
import { applyTheme } from "../lib/theme";
import {
  DEFAULT_SPEECH_PREFS,
  getSpeechPrefs,
  getThemePref,
  setThemePref,
} from "../lib/storage";
import { speechOutput } from "../lib/speech";
import { PlusIcon } from "../components/icons";
import {
  DEFAULT_THEME,
  THEMES,
  type ThemeChoice,
} from "../lib/themes.generated";
import {
  Banner,
  ChoiceCell,
  DialogFrame,
  IconButton,
  Modal,
} from "../components/ui";
import {
  AddMachine,
  MachineRows,
  useFleetHealth,
} from "../components/Machines";
import { DiagnosticsPanel } from "./settings/DiagnosticsPanel";
import { MachineSettings } from "./settings/MachineSettings";
import { SettingsColumn, SettingsPanel } from "./settings/SettingsLayout";

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
 * A MACHINE'S SETTINGS ARE HIDDEN UNDER THAT MACHINE. Every row is a disclosure and
 * its panels stand under its own row, opened by the chevron the rest of this app
 * opens things with. They used to be ONE column body under the whole list, showing
 * whichever machine was pressed last: pressing a machine opened nothing, it swapped
 * the settings already on screen for another machine's — reported as a press that
 * changes the view instead of opening the row — and the machine that column happened
 * to be reading wore the word `CURRENT`, which named no choice the reader had made.
 * Opening one machine leaves every other machine exactly as it was.
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
  onRename?: (
    conn: GatewayConn,
    label: string | undefined,
  ) => void | Promise<void>;
  onRemove?: (conn: GatewayConn) => void | Promise<void>;
  /**
   * Bind one machine to a different address. It acts on the ROW it came out of —
   * the machine's own address line — and never on another machine's.
   */
  onSelectAddress?: (
    conn: GatewayConn,
    url: string,
    pinned: boolean,
  ) => void | Promise<void>;
  onClose: () => void;
}) {
  const [pref, setPref] = useState<ThemePref>(DEFAULT_THEME.id);
  const [speechPrefs, setSpeechPrefs] =
    useState<SpeechPrefs>(DEFAULT_SPEECH_PREFS);
  const [pending, setPending] = useState<string | null>(null);
  const [err, setErr] = useState<string | null>(null);
  // Pairing opens over this dialog rather than inside it: see the sheet at the
  // foot of the return, and the band’s + that is its only door.
  const [isAdding, setIsAdding] = useState(false);

  useEffect(() => {
    let cancelled = false;
    void (async () => {
      const [theme, speech] = await Promise.all([
        getThemePref(),
        getSpeechPrefs(),
      ]);
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
      if (event.key !== "Escape") return;
      // One Escape, one surface: the pairing sheet standing over this dialog
      // leaves first, or adding a machine and reading its settings ended on the
      // same keystroke.
      if (isAdding) {
        setIsAdding(false);
        return;
      }
      onClose();
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
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

  async function changeSpeech(
    write: () => Promise<void>,
  ): Promise<SpeechPrefs> {
    await write();
    const next = await getSpeechPrefs();
    speechOutput.apply(next);
    setSpeechPrefs(next);
    return next;
  }

  // The main cog opens the fleet closed. A route from the model picker already names
  // both the machine and the job, so that one machine opens directly on Providers.
  const [openIds, setOpenIds] = useState<ReadonlySet<string>>(
    () =>
      new Set(
        gateways
          .filter((conn) => conn.url === providerMachineUrl)
          .map((conn) => machineId(conn)),
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
    gateways
      .filter((conn) => openIds.has(machineId(conn)))
      .map((conn) => conn.url),
  );

  // On a phone the columns stack and the machines lead, so the application's own
  // settings fold until asked for; side by side there is room and no fold exists.
  const [appOpen, setAppOpen] = useState(false);

  const health = useFleetHealth(gateways);

  return (
    // The app's ONE dialog: `Modal` + `DialogFrame`, the same outer component
    // "Manage projects" and every ask already open in. `wide` is the one size that
    // holds two columns of settings side by side; the height is every dialog's.
    <Modal size="wide" onDismiss={onClose}>
      <DialogFrame
        title="Settings"
        onClose={onClose}
      >
        {/* Each column scrolls ITSELF on desktop. One shared scroller made the short
            column a 1500px empty gutter: scrolling to a machine's Sandbox panel dragged
            Theme off the top of the screen for no reason. Below `sm:` the halves stack
            and the dialog body is the one scroller again. */}
        <div className="grid min-w-0 grid-cols-1 divide-dialog-edge sm:min-h-0 sm:flex-1 sm:grid-cols-2 sm:divide-x sm:overflow-hidden">
          <SettingsColumn
            title="Machines"
            action={
              /* THE COLUMN'S ONE VERB ENDS ITS BAND AS A BARE MARK. The title already
                 names what the plus adds; a filled disc repeated the same emphasis as
                 the dialog close above it. `edge` spends the header's trailing gutter
                 as hit area and lands the stroke nearer the physical right edge. */
              <IconButton
                variant="quiet"
                edge
                label="Add a machine"
                title="Add a machine"
                onClick={() => setIsAdding(true)}
              >
                <PlusIcon className="size-4" />
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
            {gateways.length > 0 ? (
              <MachineRows
                conns={gateways}
                openUrls={openUrls}
                primaryUrl={primaryUrl}
                health={health}
                onPick={toggleMachine}
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
                <p className="px-4 py-6 text-center font-mono text-body text-dialog-hint">
                  Add a machine above, and its settings live under its own row.
                </p>
              </SettingsPanel>
            )}
          </SettingsColumn>

          <SettingsColumn
            title="Application"
            disclosure={{
              isOpen: appOpen,
              onToggle: () => setAppOpen((open) => !open),
              label: `${appOpen ? "Hide" : "Show"} application settings`,
            }}
          >
            {err && (
              <div className="p-3 sm:p-4">
                <Banner kind="err">{err}</Banner>
              </div>
            )}

            <SettingsPanel title="Theme">
              <div className="grid grid-cols-1 gap-px bg-dialog-edge">
                {/* NO MODE COLUMN. Every theme is named `Blockether Light`, `Solarized
                    Dark`, `Vis Light`, so a trailing `light`/`dark` restated the last word
                    of its own row six times down the list. The name is the whole answer. */}
                {THEMES.map((choice) => (
                  <ChoiceCell
                    key={choice.id}
                    title={choice.label}
                    isSelected={pref === choice.id}
                    isLeaf
                    disabled={pending?.startsWith("theme:") ?? false}
                    onClick={() => void chooseTheme(choice)}
                  />
                ))}
              </div>
            </SettingsPanel>
            <DiagnosticsPanel />
          </SettingsColumn>
        </div>
      </DialogFrame>

      {/* PAIRING IS A SHEET OVER SETTINGS, not a panel standing open inside it.
          Both ways in — the link (or its QR) and a typed address — used to sit
          permanently expanded under the machine list, so the column opened on
          two forms for a machine that does not exist yet and the fleet the cog
          was pressed FOR started below them. The band's ＋ is the door now, and
          `fit` means the sheet is as tall as the two cards and no taller. */}
      {isAdding && (
        <Modal size="fit" onDismiss={() => setIsAdding(false)}>
          <DialogFrame
            title="Add a machine"
            subtitle="Paste the pairing link printed by ‘vis-agent gateway pair’, scan its QR, or type the address."
            onClose={() => setIsAdding(false)}
          >
            <div className="p-3 sm:p-4">
              <AddMachine
                onAdd={async (conn, makeActive) => {
                  await onAddMachine(conn, makeActive);
                  setIsAdding(false);
                }}
                isStacked
              />
            </div>
          </DialogFrame>
        </Modal>
      )}
    </Modal>
  );
}
