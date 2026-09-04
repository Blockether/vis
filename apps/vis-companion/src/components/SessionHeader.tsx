import { markSessionId } from "../lib/session-id";
import { ArtifactsChip } from "./ArtifactsSheet";
import { BackButton, CopyChip } from "./ui";

export type SessionHeaderModel = Readonly<{
  title: string;
  sessionId: string;
  connected: boolean;
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
}: {
  model: SessionHeaderModel;
  commands: SessionHeaderCommands;
}) {
  return (
    /* The notch strip stands above the 52px band via box-content. Edge controls
       own horizontal safe-area padding so the header's paper still reaches the glass. */
    <header className="z-10 flex min-h-13 shrink-0 items-stretch gap-0 border-b border-dialog-edge bg-panel-2 box-content pt-[env(safe-area-inset-top)] mouse:min-h-9 mouse:pt-0">
      <BackButton label="Back to sessions" onClick={commands.back} />
      <div className="min-w-0 flex-1 self-center px-3 py-1.5 mouse:py-1">
        {/* The title is the sentence the screen is about, and a human's sentence is
            PROSE: Inter, the transcript's own face, one step above its body — while the
            connection line under it and the id chip beside it stay mono facts. */}
        <h1 className="truncate text-subhead font-semibold text-white mouse:text-title">
          {model.title}
        </h1>
        <div className="flex min-w-0 items-center gap-1.5 font-mono text-meta text-dialog-hint">
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
    </header>
  );
}
