// Regression, no issue: a `node`-environment test that touched
// `globalThis.localStorage` reached NODE's own web storage — on by default
// since Node 25 — instead of a stand-in. Every run printed `Warning:
// --localstorage-file was provided without a valid path`, and what it handed
// back was a stub whose `getItem`/`setItem` are not functions, shared by every
// file that worker went on to run.

import { describe, expect, it } from "vitest";

describe("the storage every test file is handed", () => {
  it("is a working store, not the stub Node hands back without a file", () => {
    for (const store of [localStorage, sessionStorage]) {
      for (const op of ["getItem", "setItem", "removeItem", "key", "clear"] as const) {
        expect(typeof store[op]).toBe("function");
      }
    }
  });

  it("holds what a test puts in it, and starts this file empty", () => {
    expect(localStorage.length).toBe(0);
    expect(sessionStorage.length).toBe(0);
    localStorage.setItem("vis.probe", "kept");
    expect(localStorage.getItem("vis.probe")).toBe("kept");
    expect(localStorage.key(0)).toBe("vis.probe");
    // Two stores, not one alias: parked scroll state must not read as a draft.
    expect(sessionStorage.getItem("vis.probe")).toBeNull();
    localStorage.removeItem("vis.probe");
    expect(localStorage.getItem("vis.probe")).toBeNull();
  });

  it("still lets a test hand over its own store", () => {
    for (const name of ["localStorage", "sessionStorage"] as const) {
      const descriptor = Object.getOwnPropertyDescriptor(globalThis, name);
      expect(descriptor?.configurable).toBe(true);
      expect(descriptor?.writable).toBe(true);
    }
  });
});
