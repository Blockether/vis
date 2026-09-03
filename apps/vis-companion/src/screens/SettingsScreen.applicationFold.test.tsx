// @vitest-environment jsdom
import { fireEvent, render, screen } from "@testing-library/react";
import { useState } from "react";
import { describe, expect, it } from "vitest";
import { SettingsColumn } from "./settings/SettingsLayout";
import settingsScreenSource from "./SettingsScreen.tsx?raw";

/** The dialog's two columns stack below `sm:`; there the application's own settings
 *  fold behind their band, and the machines lead. Beside each other — the same
 *  breakpoint the grid turns two columns at — no fold exists at all. */

function Harness({ initialOpen }: { initialOpen: boolean }) {
  const [open, setOpen] = useState(initialOpen);
  return (
    <SettingsColumn
      title="Application"
      disclosure={{
        isOpen: open,
        onToggle: () => setOpen((current) => !current),
        label: `${open ? "Hide" : "Show"} application settings`,
      }}
    >
      <p>Theme</p>
    </SettingsColumn>
  );
}

/** The setup file's stand-in answers false to every query; a narrow window is the
 *  honest default, and a wide one is spelled out here explicitly. */
const setViewport = (wide: boolean) => {
  window.matchMedia = ((query: string) => ({
    matches: query === "(min-width: 640px)" ? wide : false,
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as typeof window.matchMedia;
};

describe("the settings dialog's stacked application fold", () => {
  it("hides the application column's body until its band is pressed", () => {
    setViewport(false);
    render(<Harness initialOpen={false} />);
    expect(screen.queryByText("Theme")).not.toBeInTheDocument();
    const band = screen.getByRole("button", { name: "Show application settings" });
    expect(band).toHaveAttribute("aria-expanded", "false");
    fireEvent.click(band);
    expect(screen.getByText("Theme")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "Hide application settings" })).toHaveAttribute(
      "aria-expanded",
      "true",
    );
  });

  it("keeps a standing column open with no fold where both columns fit", () => {
    setViewport(true);
    render(<Harness initialOpen={false} />);
    expect(screen.getByText("Theme")).toBeInTheDocument();
    expect(screen.queryByRole("button")).not.toBeInTheDocument();
  });

  it("dropped the dialog's 'this device' copy along with the fold", () => {
    expect(settingsScreenSource).not.toContain("This device");
    expect(settingsScreenSource).not.toContain('meta="this device"');
  });
});
