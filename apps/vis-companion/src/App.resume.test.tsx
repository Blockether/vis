// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";
import { rememberMachineOutage } from "./lib/fleet-outage";
import { loadOpenSession } from "./lib/storage";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

const LAPTOP = "http://resume-laptop.example.com";

const fleet = () => [
  {
    label: "laptop",
    url: LAPTOP,
    sessions: [listSession({ id: "s1", title: "Session one" })],
  },
];

/** Both mirrors of a Preferences key, the way `app-harness` seeds the pairing. */
const seed = (key: string, value: string) => {
  for (const prefix of ["", "CapacitorStorage."])
    localStorage.setItem(`${prefix}${key}`, value);
};

/** What the run before the kill wrote down about the transcript on screen. */
const openSession = (at: number, url = LAPTOP, sid = "s1") =>
  seed("vis.openSession", JSON.stringify({ url, sid, at }));

/** A cold start: iOS reboots the webview from a blank hash, never the previous one. */
const coldStart = () => {
  window.location.hash = "";
  const view = renderApp({ machines: fleet() });
  restore = view.restore;
  return view;
};

const MINUTE = 60 * 1000;

// Regression, user report (paraphrased: "it keeps going back to a session I do not
// even have open — even after I close the app — and that session's gateway is off"):
// a route-less launch reopened the FIRST entry of the per-gateway subscription list,
// which every visited session joins and none ever leaves, so going back to the list
// and killing the app still relaunched into the transcript; nothing asked whether the
// machine was answering, and nothing asked how long ago that was.
describe("a cold start reopens only the transcript the app died on", () => {
  it("lands back in a transcript that was on screen minutes ago", async () => {
    openSession(Date.now() - 2 * MINUTE);
    const view = coldStart();

    expect(await screen.findByLabelText("Message Vis", {}, { timeout: 5_000 })).toBeTruthy();
    // On top of the list, the way a tap on its row would have put it.
    expect(screen.getByRole("button", { name: "Back to sessions" })).toBeTruthy();
    view.unmount();
  });

  it("starts on the list when the transcript was last seen hours ago", async () => {
    openSession(Date.now() - 3 * 60 * MINUTE);
    const view = coldStart();

    expect(await screen.findByText("Session one")).toBeTruthy();
    expect(screen.queryByLabelText("Message Vis")).toBeNull();
    view.unmount();
  });

  it("starts on the list when that transcript's machine is known to be dark", async () => {
    openSession(Date.now() - MINUTE);
    rememberMachineOutage(LAPTOP, "Failed to fetch");
    const view = coldStart();

    // Not the transcript: the list, with the machine's own tile to reconnect it.
    expect(await screen.findByRole("button", { name: "Projects on laptop" })).toBeTruthy();
    expect(screen.queryByLabelText("Message Vis")).toBeNull();
    view.unmount();
  });

  it("starts on the list when the transcript belongs to a machine no longer paired", async () => {
    openSession(Date.now() - MINUTE, "http://forgotten.example.com");
    const view = coldStart();

    expect(await screen.findByText("Session one")).toBeTruthy();
    expect(screen.queryByLabelText("Message Vis")).toBeNull();
    view.unmount();
  });

  it("ignores the subscription list, which remembers every session ever visited", async () => {
    seed("vis.sessionSubscriptions", JSON.stringify({ [LAPTOP]: ["s1"] }));
    const view = coldStart();

    expect(await screen.findByText("Session one")).toBeTruthy();
    expect(screen.queryByLabelText("Message Vis")).toBeNull();
    view.unmount();
  });

  it("forgets the transcript the moment the user goes back to the list", async () => {
    const view = coldStart();
    fireEvent.click(await screen.findByText("Session one"));
    await screen.findByLabelText("Message Vis", {}, { timeout: 5_000 });
    await waitFor(async () =>
      expect(await loadOpenSession()).toMatchObject({ url: LAPTOP, sid: "s1" }),
    );

    fireEvent.click(screen.getByRole("button", { name: "Back to sessions" }));
    await screen.findByText("Session one");
    await waitFor(async () => expect(await loadOpenSession()).toBeNull());
    view.unmount();
  });
});
