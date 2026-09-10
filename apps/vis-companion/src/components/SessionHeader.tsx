import { useState } from "react";
import gatewayContract from "../../../../packages/vis-contract/resources/vis-contract/gateway.json";
import type { SessionGoal } from "../lib/types";
import { useDeskRail } from "../lib/fit-rows";
import { markSessionId } from "../lib/session-id";
import { ArtifactsChip } from "./ArtifactsSheet";
import { BackButton, Button, CopyChip, DialogFrame, Modal, SidebarToggle } from "./ui";

const GOAL_STATUS = gatewayContract.session_goal.status_labels;

export type SessionHeaderModel = Readonly<{
  title: string;
  sessionId: string;
  connected: boolean;
  goal?: SessionGoal | null;
  artifacts: Readonly<{ count: number; isOpen: boolean }>;
}>;

export type SessionHeaderCommands = Readonly<{
  back: () => void;
  toggleArtifacts: () => void;
}>;

function SessionIdChip({ sessionId }: { sessionId: string }) {
  const short = sessionId.length > 8 ? sessionId.slice(0, 8) : sessionId;
  return (
    <CopyChip
      value={markSessionId(sessionId)}
      label="Copy session id"
      title={`Copy session id
${sessionId}`}
      density="compact"
      className="max-w-[9rem]"
    >
      {short}
    </CopyChip>
  );
}

/** The session's navigation, identity, connection state, and artifact door. */
export function SessionHeader({
  model,
  commands,
  sidebar,
}: {
  model: SessionHeaderModel;
  commands: SessionHeaderCommands;
  /**
   * The desk's list toggle, when the shell has a list beside this transcript to
   * put away: it stands on the band's leading edge, where the phone's arrow does.
   */
  sidebar?: { isShown: boolean; onToggle: () => void };
}) {
  // ON A DESK THE LIST IS BESIDE THIS HEADER, not behind it: there is nothing to go
  // back to, so the arrow goes. Its edge is not left bare, though — the toggle that
  // puts the list away, or brings it back, takes that leading column; only a desk
  // with no list to toggle lets the title claim the edge.
  const isDesk = useDeskRail();
  const [goalDetails, setGoalDetails] = useState(false);
  const goal = model.goal;
  return (
    /* The notch strip stands above the 52px band via box-content. Edge controls
       own horizontal safe-area padding so the header's paper still reaches the glass. */
    <header className="z-10 flex min-h-13 shrink-0 items-stretch gap-0 border-b border-dialog-edge bg-panel-2 box-content pt-[env(safe-area-inset-top)] mouse:min-h-9 mouse:pt-0">
      {!isDesk && (
        <BackButton label="Back to sessions" onClick={commands.back} />
      )}
      {isDesk && sidebar && (
        <SidebarToggle isShown={sidebar.isShown} onClick={sidebar.onToggle} />
      )}
      <div
        className={`min-w-0 flex-1 self-center py-1.5 mouse:py-1 ${isDesk && !sidebar ? "pl-4 pr-3" : "px-3"}`}
      >
        {/* The title is the sentence the screen is about. It stays one step above
            its body by size and weight, while the connection line and id chip
            step down to the facts scale — all in the app's one mono face. */}
        <h1 className="truncate text-subhead font-semibold text-white mouse:text-title">
          {model.title}
        </h1>
        <div className="flex min-w-0 items-center gap-1.5 font-mono text-ui text-dialog-hint mouse:text-meta">
          <span
            className={`size-1.5 shrink-0 ${
              model.connected
                ? "bg-ok"
                : "animate-pulse bg-turn-edge motion-reduce:animate-none"
            }`}
          />
          <span className="shrink-0">
            {model.connected ? "Connected" : "Reconnecting"}
          </span>
          {goal && (
            <Button variant="quiet" density="panel" pressEffect="none" className="min-w-0 max-w-full"
              aria-label={`Goal: ${GOAL_STATUS[goal.status]} — ${goal.objective}`}
              aria-haspopup="dialog" onClick={() => setGoalDetails(true)}>
              <span className="block max-w-full truncate">Goal: {goal.status !== "active" && `${GOAL_STATUS[goal.status]} — `}{goal.objective}</span>
            </Button>
          )}
        </div>
      </div>
      <div className="flex shrink-0 items-center gap-2 self-center pl-1 pr-[max(0.5rem,env(safe-area-inset-right))] sm:pr-[max(0.75rem,env(safe-area-inset-right))] mouse:gap-1">
        <SessionIdChip sessionId={model.sessionId} />
        <ArtifactsChip
          count={model.artifacts.count}
          open={model.artifacts.isOpen}
          onToggle={commands.toggleArtifacts}
        />
      </div>
      {goal && goalDetails && (
        <Modal onDismiss={() => setGoalDetails(false)} size="fit">
          <DialogFrame title="Session goal" subtitle={GOAL_STATUS[goal.status]}
            closeLabel="Close session goal" onClose={() => setGoalDetails(false)}>
            <div className="space-y-3 p-4 text-body text-white">
              <p className="whitespace-pre-wrap break-words">{goal.objective}</p>
              <p className="text-ui text-dialog-hint">
                Iterations: {goal.iterations_used.toLocaleString("en-US")} / {goal.iteration_budget?.toLocaleString("en-US") ?? "unlimited"}
              </p>
              <p className="text-ui text-dialog-hint">
                {goal.tokens_used.toLocaleString("en-US")} tokens used (statistic)
              </p>
              {goal.reason && <p className="whitespace-pre-wrap break-words">{goal.reason}</p>}
              <p className="text-ui text-dialog-hint">Use /goal --pause, --resume or --cancel in the composer.</p>
            </div>
          </DialogFrame>
        </Modal>
      )}
    </header>
  );
}
