import { SidebarToggle } from "../components/ui";

/**
 * What the desk's transcript column holds while no session is open.
 *
 * The list beside it is the whole answer to "what now": open a row, or press the
 * `+` a project header carries. This pane only says so, in the transcript's own
 * empty-state voice (`SessionScreen`), and offers no second door — a create here
 * would have to ask which project it meant, which is the chooser the list is.
 *
 * Its one control is the list's own toggle, on the leading edge of a band the
 * height of a session's header, so the control stands still when a session opens.
 */
export function EmptyPane({
  sidebar,
}: {
  sidebar?: { isShown: boolean; onToggle: () => void };
}) {
  return (
    <section
      aria-label="No session open"
      className="flex h-full min-h-0 min-w-0 flex-1 flex-col transition-[opacity,transform,translate,scale,rotate] duration-300 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none"
    >
      {sidebar && (
        <div className="flex min-h-13 shrink-0 items-stretch box-content pt-[env(safe-area-inset-top)] mouse:min-h-9 mouse:pt-0">
          <SidebarToggle isShown={sidebar.isShown} onClick={sidebar.onToggle} />
        </div>
      )}
      <div className="flex min-h-0 flex-1 flex-col items-center justify-center px-6 text-center">
        <img
          src="/vis-logo.png"
          alt=""
          className="w-16 max-w-full object-contain"
          aria-hidden="true"
        />
        <div className="mt-4 max-w-md">
          <h2 className="text-head font-semibold text-dialog-foreground">
            Pick a session
          </h2>
          <p className="mt-1 text-body text-dialog-hint">
            Open one from the list, or start a new one with the + beside a
            project.
          </p>
        </div>
      </div>
    </section>
  );
}
