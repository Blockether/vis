// @vitest-environment jsdom
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { ExpandableImage, ImageViewer } from "./ImageViewer";
import { MediaGrid, MediaTile } from "./Media";

// The viewer is now a COMPOSITION: the geometry comes from `lib/zoom-pan`, the
// strokes from `AnnotationLayer`, the sheets from `lib/image-share`. These
// tests hold the seams together — that the readout really reflects the shared
// zoom maths, and that the pen the whole app shares is the pen this dialog
// mounts.

globalThis.IS_REACT_ACT_ENVIRONMENT = true;

let root: Root;
let host: HTMLDivElement;

beforeEach(() => {
  host = document.createElement("div");
  document.body.append(host);
  root = createRoot(host);
  HTMLCanvasElement.prototype.getContext = vi.fn(() => null) as never;
  act(() =>
    root.render(
      <ImageViewer
        src="blob:picture"
        name="chart.png"
        onClose={() => undefined}
      />,
    ),
  );
});

afterEach(() => {
  act(() => root.unmount());
  host.remove();
});

function control(label: string): HTMLButtonElement {
  const button = document.querySelector<HTMLButtonElement>(
    `button[aria-label="${label}"]`,
  );
  if (!button) throw new Error(`no control labelled ${label}`);
  return button;
}

function named(text: string): HTMLButtonElement {
  const button = [...document.querySelectorAll("button")].find(
    (b) => b.textContent === text,
  );
  if (!button) throw new Error(`no button named ${text}`);
  return button;
}

describe("ImageViewer", () => {
  it("opens as a modal named after the picture, fitted at 100%", () => {
    const dialog = document.querySelector('[role="dialog"]');
    expect(dialog?.getAttribute("aria-label")).toBe("chart.png image viewer");
    expect(control("Reset zoom").textContent).toBe("100%");
  });

  it("draws the picture actions as named icons, not toolbar words", () => {
    const actions = [
      control("Draw on image"),
      control("Trim to view"),
      control("Copy image"),
    ];
    const handoff = document.querySelector<HTMLButtonElement>(
      'button[aria-label="Share image"], button[aria-label="Save image"]',
    );
    if (!handoff) throw new Error("no image handoff control");
    actions.push(handoff);

    for (const action of actions) {
      expect(action.textContent).toBe("");
      expect(action.querySelector("svg")).not.toBeNull();
      expect(action.title).toBe(action.getAttribute("aria-label"));
    }
  });

  // Regression, user report ("the close buttons, instead of being just the X mark,
  // are shown as `Close`"): the image viewer's header carried a secondary text button
  // where every other surface wears the app's one `CloseButton`.
  it("leaves through the app's one X, named after the picture", () => {
    const close = control("Close chart.png");
    expect(close.textContent).toBe("");
    expect(close.querySelector("svg")).not.toBeNull();
    expect(close.className).not.toContain("border-l");
    expect(close.querySelector("span")?.className).toContain("rounded-full");
  });

  // Regression, user report: the viewer's close looked nothing like the close in a
  // dialog — smaller, and the wrong colour. The viewer hand-built its own `bg-panel`
  // band with a quiet `text-ui` filename and asked for a panel-toned way out,
  // so the app's one way out wore two faces on two surfaces of the same screen.
  it("titles itself with the app's one dialog band", () => {
    const header = document.querySelector('[role="dialog"] header');
    expect(header?.className).toContain("bg-dialog-title");
    expect(header?.className).toContain("min-h-12");
    expect(header?.className).toContain("items-stretch");
    expect(header?.querySelector("h2")?.textContent).toBe("chart.png");

    // The target still inherits the band's full ink. Its compact face is circular, and both
    // Blockether palettes fill it with the same amber pair mirrored — Light amber over dark
    // ink, Dark ink marked in amber — so neither band shows an unfilled way out.
    const close = control("Close chart.png");
    const face = close.querySelector("span");
    expect(header?.className).toContain("text-dialog-title-foreground");
    expect(header?.className).not.toContain("text-dialog-title-foreground/70");
    expect(close.className).toContain("text-current");
    expect(face?.className).toContain("border-current");
    expect(face?.className).toContain("blockether-light:bg-accent");
    expect(face?.className).toContain("blockether-light:text-accent-foreground");
    expect(face?.className).toContain("blockether-dark:bg-accent-foreground");
    expect(face?.className).toContain("blockether-dark:text-accent");
  });

  // Regression, same phone report ("the headline has wrong height"): the title band is
  // the safe-area inset TALLER under a notch (48px + inset), while the picture's top pad
  // and its own cap were a fixed 5rem/10rem — so the top of a tall image sat under the
  // title bar it was supposed to clear.
  it("keeps the picture clear of the band that clears the notch", () => {
    const header = document.querySelector('[role="dialog"] header');
    expect(header?.className).toContain("box-content");

    const picture = document.querySelector('[role="dialog"] img');
    expect(picture?.closest(".place-items-center")?.className).toContain(
      "pt-[calc(5rem+env(safe-area-inset-top))]",
    );
    expect(picture?.className).toContain("max-h-full");
    expect(picture?.className).toContain("max-w-[calc(100vw-2rem)]");
  });

  // Regression, reported attachment filename click: an image edit could only be opened
  // by striking its thumbnail; the adjacent filename looked like part of the same chip
  // but was not a trigger.
  it("opens when the attachment filename is clicked", () => {
    act(() =>
      root.render(
        <ExpandableImage src="blob:picture" alt="chart.png" className="size-8">
          <span>chart.png</span>
        </ExpandableImage>,
      ),
    );

    const trigger = control("Open chart.png full screen");
    const filename = [...trigger.querySelectorAll("span")].find(
      (element) => element.textContent === "chart.png",
    );
    expect(filename).toBeTruthy();
    act(() =>
      filename?.dispatchEvent(new MouseEvent("click", { bubbles: true })),
    );
    expect(document.querySelector('[role="dialog"]')).not.toBeNull();
  });

  // The readout is written straight to the DOM rather than through state — a
  // pinch that re-rendered React on every frame stutters on exactly the devices
  // that pinch — so this is the only proof the shared maths reached the screen.
  it("zooms through the shared geometry and resets back to fitted", () => {
    act(() => control("Zoom in").click());
    expect(control("Reset zoom").textContent).toBe("135%");
    act(() => control("Zoom out").click());
    expect(control("Reset zoom").textContent).toBe("100%");
    act(() => control("Zoom in").click());
    act(() => control("Reset zoom").click());
    expect(control("Reset zoom").textContent).toBe("100%");
  });

  // Zooming into a detail and wanting only that detail is one gesture short of
  // a crop: Trim makes what the frame shows the picture, so the pen, copy,
  // share and apply all act on that region instead of the page around it.
  it("offers Trim beside Draw, and says so when there is nothing to trim", () => {
    const trim = control("Trim to view");
    expect(trim.textContent).toBe("");
    expect(document.querySelector('[aria-label="Undo trim"]')).toBeNull();
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).toContain("Trim");

    // jsdom gives every box a zero rect, which is exactly "the frame already
    // shows the whole picture" — the tap must explain itself, never crop.
    act(() => trim.click());
    expect(document.querySelector('[aria-live="polite"]')?.textContent).toBe(
      "Zoom in first — the whole picture is already in view.",
    );
    expect(document.querySelector('[aria-label="Undo trim"]')).toBeNull();
  });

  // A stroke in flight owns the picture: cropping under the pen would flatten
  // half a mark and leave the other half pointing at pixels that are gone.
  it("holds Trim while the pen is out", () => {
    act(() => control("Draw on image").click());
    expect(control("Trim to view").disabled).toBe(true);
    act(() => control("Draw on image").click());
    expect(control("Trim to view").disabled).toBe(false);
  });

  it("mounts the shared annotation layer, inert until Draw is pressed", () => {
    const canvas = document.querySelector("canvas");
    expect(canvas?.getAttribute("aria-label")).toBe("Image annotation layer");
    expect(canvas?.getAttribute("data-annotation")).toBe("idle");
    expect(document.querySelector('[aria-label="Drawing tools"]')).toBeNull();

    act(() => control("Draw on image").click());
    expect(control("Draw on image").getAttribute("aria-pressed")).toBe("true");
    expect(
      document.querySelector("canvas")?.getAttribute("data-annotation"),
    ).toBe("active");
    // Five inks, Undo and Clear sit directly under the title band, not in the footer.
    const tools = document.querySelector('[aria-label="Drawing tools"]');
    expect(tools?.querySelectorAll("button[aria-pressed]")).toHaveLength(5);
    expect(tools?.parentElement?.className).toContain("top-[calc(3rem");
    expect(document.querySelector("header")?.nextElementSibling).toBe(
      tools?.parentElement,
    );

    act(() => control("Draw on image").click());
    expect(document.querySelector('[aria-label="Drawing tools"]')).toBeNull();
  });

  // The promise printed under the buttons has to match the button that is
  // actually there: without `onApply` the picture can only leave by copy/share.
  it("says what drawing is FOR, and only offers Apply when there is somewhere to apply it", () => {
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).toContain("to zoom");
    act(() => control("Draw on image").click());
    expect(document.querySelector('[aria-live="polite"]')?.textContent).toBe(
      "Draw on the image, then copy or share it.",
    );

    act(() =>
      root.render(
        <ImageViewer
          src="blob:picture"
          name="chart.png"
          onClose={() => undefined}
          onApply={() => undefined}
          applyLabel="Attach to message"
        />,
      ),
    );
    expect(named("Attach to message")).toBeTruthy();
  });
});

// Regression, user report: drawing on an artifact put TWO finish buttons on the same
// strip — "Done" beside "Use edit" — and neither was named Save nor stood where a
// screen is left from. The verb that keeps the ink is the band's cell now, one
// hairline from the ✕, and the pen's toggle stays a toggle.
it("keeps the ink through one Save in the band, one cell from the way out", () => {
  const applied = vi.fn();
  act(() =>
    root.render(
      <ImageViewer
        src="blob:picture"
        name="chart.png"
        onClose={() => undefined}
        onApply={applied}
      />,
    ),
  );

  const header = document.querySelector('[role="dialog"] header');
  const save = named("Save");
  expect(header?.contains(save)).toBe(true);
  expect(save.nextElementSibling).toBe(control("Close chart.png"));

  // The strip never grows a second way to finish: pressed IS drawing.
  act(() => control("Draw on image").click());
  expect(control("Draw on image").getAttribute("aria-pressed")).toBe("true");
  expect([...document.querySelectorAll("button")].map((b) => b.textContent)).not.toContain("Done");

  // And Save puts the pen down: it is the end of drawing, not a step beside it.
  act(() => save.click());
  expect(control("Draw on image").getAttribute("aria-pressed")).toBe("false");
});

// Regression, user report: pressing Save on an untouched shared picture started a
// full-resolution PNG encode. On a phone photo WebKit could stay in "Preparing image"
// with the whole app unresponsive even though there were no edited pixels to keep.
it("closes an untouched pending image without preparing a replacement", async () => {
  const applied = vi.fn();
  const closed = vi.fn();
  act(() =>
    root.render(
      <ImageViewer
        src="blob:shared-photo"
        name="shared-photo.jpg"
        onClose={closed}
        onApply={applied}
      />,
    ),
  );

  await act(async () => named("Save").click());

  expect(applied).not.toHaveBeenCalled();
  expect(closed).toHaveBeenCalledOnce();
  expect(document.querySelector('[aria-live="polite"]')?.textContent).not.toContain(
    "Preparing image",
  );
});
// Regression: the CSS transition meant for button/reset snaps also applied
// while a finger was dragging the picture every frame, fighting the direct
// pointer-driven transform and reading as pinch/pan stutter and lag.
it("suspends the snap transition while a pinch or pan is live, and restores it on lift", () => {
  const surface = document.querySelector<HTMLDivElement>(
    '[role="dialog"] .cursor-grab',
  );
  const transformed = document.querySelector<HTMLDivElement>(".origin-center");
  if (!surface || !transformed) throw new Error("viewer surface not found");
  Element.prototype.setPointerCapture = vi.fn();

  expect(transformed.style.transitionDuration).toBe("");

  act(() => {
    surface.dispatchEvent(
      new PointerEvent("pointerdown", {
        pointerId: 1,
        clientX: 0,
        clientY: 0,
        bubbles: true,
      }),
    );
  });
  expect(transformed.style.transitionDuration).toBe("0ms");

  act(() => {
    surface.dispatchEvent(
      new PointerEvent("pointermove", {
        pointerId: 1,
        clientX: 10,
        clientY: 0,
        bubbles: true,
      }),
    );
  });
  expect(transformed.style.transitionDuration).toBe("0ms");

  act(() => {
    surface.dispatchEvent(
      new PointerEvent("pointerup", {
        pointerId: 1,
        clientX: 10,
        clientY: 0,
        bubbles: true,
      }),
    );
  });
  expect(transformed.style.transitionDuration).toBe("");
});

// Regression, iOS draw pinch: the canvas drew the primary finger but stopped it
// before the viewport's bubble handler registered the first point. The second
// finger then looked like the only pointer, so pinch-to-zoom could not start.
// The viewport must record its pointers in capture phase while the annotation
// layer lets the second finger reach its move handler.
it("still pinch-zooms with a second finger while a stroke is in progress", () => {
  Element.prototype.setPointerCapture = vi.fn();
  // Draw resets the transform to fitted, so zoom AFTER entering drawing mode
  // or the reset — not the pinch — would be the only thing this proves.
  act(() => control("Draw on image").click());
  act(() => control("Zoom in").click());
  expect(control("Reset zoom").textContent).toBe("135%");

  const canvas = document.querySelector("canvas");
  if (!canvas) throw new Error("annotation canvas not found");

  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointerdown", {
        pointerId: 1,
        isPrimary: true,
        clientX: 0,
        clientY: 0,
        bubbles: true,
      }),
    );
  });
  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointerdown", {
        pointerId: 2,
        isPrimary: false,
        clientX: 300,
        clientY: 0,
        bubbles: true,
      }),
    );
  });
  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointermove", {
        pointerId: 2,
        isPrimary: false,
        clientX: 250,
        clientY: 0,
        bubbles: true,
      }),
    );
  });

  expect(control("Reset zoom").textContent).not.toBe("135%");
});

// Regression, reported on mobile: pinching to zoom while the pen was out kept
// the first finger's stroke alive, so every two-finger zoom scribbled a line
// across the picture. A second finger means pinch — the mark is abandoned.
it("abandons the stroke in progress when a second finger starts a pinch", () => {
  Element.prototype.setPointerCapture = vi.fn();
  act(() => control("Draw on image").click());
  const canvas = document.querySelector("canvas");
  if (!canvas) throw new Error("annotation canvas not found");
  canvas.width = 400;
  canvas.height = 300;
  canvas.getBoundingClientRect = () =>
    ({ left: 0, top: 0, width: 400, height: 300 }) as DOMRect;

  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointerdown", {
        pointerId: 1,
        isPrimary: true,
        clientX: 10,
        clientY: 10,
        bubbles: true,
      }),
    );
  });
  expect(named("Undo").disabled).toBe(false);

  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointerdown", {
        pointerId: 2,
        isPrimary: false,
        clientX: 300,
        clientY: 10,
        bubbles: true,
      }),
    );
  });
  expect(named("Undo").disabled).toBe(true);

  // The finger that is now half of the pinch must not keep painting either.
  act(() => {
    canvas.dispatchEvent(
      new PointerEvent("pointermove", {
        pointerId: 1,
        isPrimary: true,
        clientX: 80,
        clientY: 60,
        bubbles: true,
      }),
    );
  });
  expect(named("Undo").disabled).toBe(true);
});

// Regression, reported from the app: a stroke only appeared when the finger
// touched down exactly on the picture's edge. Starting on the dark margin a
// little outside it and dragging in drew nothing at all, even well past the
// point where ink should have appeared.
it("draws a stroke that starts beside the picture and crosses onto it", () => {
  Element.prototype.setPointerCapture = vi.fn();
  act(() => control("Draw on image").click());
  const viewport = document.querySelector<HTMLDivElement>(
    '[role="dialog"] .cursor-grab',
  );
  const canvas = document.querySelector("canvas");
  if (!viewport || !canvas) throw new Error("viewer surface not found");
  canvas.width = 400;
  canvas.height = 300;
  canvas.getBoundingClientRect = () =>
    ({ left: 100, top: 100, width: 400, height: 300 }) as DOMRect;

  const at = (type: string, x: number, y: number) =>
    act(() => {
      viewport.dispatchEvent(
        new PointerEvent(type, {
          pointerId: 1,
          isPrimary: true,
          clientX: x,
          clientY: y,
          bubbles: true,
        }),
      );
    });

  // Down on the margin, left of the picture, then in.
  at("pointerdown", 40, 200);
  at("pointermove", 160, 200);
  at("pointerup", 160, 200);

  expect(named("Undo").disabled).toBe(false);
});

// Regression, user report ("on desktop and safari the zooming is too fast and not
// reliable when it comes to artefacts and the images"): the wheel was handled through
// React's `onWheel`, which React attaches PASSIVE at the root, so the
// `preventDefault()` in it was ignored and the browser kept its own page zoom under
// the open picture. Every event also moved the picture a fixed 15%, so one flick of a
// trackpad — sixty events a second — hit the 6x ceiling instantly.
it("takes the wheel itself, and moves by the distance scrolled", () => {
  const surface = document.querySelector<HTMLDivElement>(
    '[role="dialog"] .cursor-grab',
  );
  if (!surface) throw new Error("viewer surface not found");

  const notch = new WheelEvent("wheel", {
    deltaY: -100,
    bubbles: true,
    cancelable: true,
  });
  act(() => {
    surface.dispatchEvent(notch);
  });
  expect(notch.defaultPrevented).toBe(true);
  expect(control("Reset zoom").textContent).toBe("125%");

  act(() => control("Reset zoom").click());
  // Ten frames of a trackpad glide are still less than one notch.
  act(() => {
    for (let i = 0; i < 10; i += 1) {
      surface.dispatchEvent(
        new WheelEvent("wheel", {
          deltaY: -2,
          bubbles: true,
          cancelable: true,
        }),
      );
    }
  });
  const glided = Number.parseInt(control("Reset zoom").textContent ?? "", 10);
  expect(glided).toBeGreaterThan(100);
  expect(glided).toBeLessThan(125);
});

// Safari sends `gesturestart`/`gesturechange`/`gestureend` for a trackpad pinch
// INSTEAD of the ctrl+wheel every other browser sends, so on Safari the viewer's own
// pinch never ran and the page zoomed instead.
it("zooms with Safari's own trackpad pinch", () => {
  const surface = document.querySelector<HTMLDivElement>(
    '[role="dialog"] .cursor-grab',
  );
  if (!surface) throw new Error("viewer surface not found");

  const gesture = (type: string, scale: number) =>
    Object.assign(new Event(type, { bubbles: true, cancelable: true }), {
      scale,
      clientX: 0,
      clientY: 0,
    });

  const start = gesture("gesturestart", 1);
  act(() => {
    surface.dispatchEvent(start);
  });
  expect(start.defaultPrevented).toBe(true);

  act(() => {
    surface.dispatchEvent(gesture("gesturechange", 2));
  });
  expect(control("Reset zoom").textContent).toBe("200%");

  act(() => {
    surface.dispatchEvent(gesture("gestureend", 2));
  });
  expect(control("Reset zoom").textContent).toBe("200%");
});

// Regression, user report ("when I click the image I cannot jump to the next
// one with the arrows left/right"): a gallery opened one picture at a time —
// the viewer showed only what was tapped and the arrow keys did nothing, so
// reading a contact sheet cost one close-and-tap per image.
describe("a gallery in the viewer", () => {
  const pictures = [
    { src: "blob:one", name: "one.png" },
    { src: "blob:two", name: "two.png" },
    { src: "blob:three", name: "three.png" },
  ];

  const open = (at: number, shown = pictures) =>
    act(() =>
      root.render(
        <ImageViewer
          src={shown[at].src}
          name={shown[at].name}
          pictures={shown}
          at={at}
          onClose={() => undefined}
        />,
      ),
    );

  const title = () => document.querySelector('[role="dialog"] h2')?.textContent;
  const shownSrc = () =>
    document.querySelector('[role="dialog"] img')?.getAttribute("src");
  const press = (key: string) =>
    act(() => {
      window.dispatchEvent(new KeyboardEvent("keydown", { key }));
    });
  const stage = () =>
    document.querySelector<HTMLElement>('[role="dialog"] .transform-gpu');
  const finger = (type: string, x: number, y = 300) => {
    const glass = document.querySelector<HTMLDivElement>(
      '[role="dialog"] .cursor-grab',
    );
    if (!glass) throw new Error("viewer surface not found");
    act(() => {
      glass.dispatchEvent(
        new PointerEvent(type, {
          pointerId: 7,
          isPrimary: true,
          clientX: x,
          clientY: y,
          bubbles: true,
        }),
      );
    });
  };
  // A finger down at 200 and lifted at `to`: left of it is the next picture.
  const swipe = (to: number) => {
    Element.prototype.setPointerCapture = vi.fn();
    finger("pointerdown", 200);
    finger("pointermove", to);
    finger("pointerup", to);
  };

  it("walks to the next and previous picture with the arrow keys", () => {
    open(1);
    expect(title()).toBe("two.png");

    press("ArrowRight");
    expect(title()).toBe("three.png");
    expect(shownSrc()).toBe("blob:three");

    press("ArrowLeft");
    press("ArrowLeft");
    expect(title()).toBe("one.png");
    expect(shownSrc()).toBe("blob:one");
  });

  // Regression, user report ("in the application we should not have the left right
  // … on iOS and android we should have swipes working"): the gallery was walked
  // only from a pair of chevrons on the toolbar, and dragging the picture — the one
  // gesture a phone offers — did nothing at all.
  it("walks the gallery with a swipe, and keeps no arrows to press instead", () => {
    open(0);
    expect(document.querySelector('[aria-label="Next image"]')).toBeNull();
    expect(document.querySelector('[aria-label="Previous image"]')).toBeNull();

    swipe(60);
    expect(title()).toBe("two.png");
    expect(shownSrc()).toBe("blob:two");
    swipe(340);
    expect(title()).toBe("one.png");
  });

  // A gesture that moves nothing until it fires reads as a dead screen, so the
  // picture travels with the finger — and comes back when the drag stops short.
  it("carries the picture with the finger, and puts it back if it stopped short", () => {
    open(1);
    Element.prototype.setPointerCapture = vi.fn();
    finger("pointerdown", 200);
    finger("pointermove", 170);
    expect(stage()?.style.transform).toContain("translate3d(-30px, 0, 0)");

    finger("pointerup", 170);
    expect(stage()?.style.transform).not.toContain("-30px");
    expect(title()).toBe("two.png");
  });

  // A pointer the SYSTEM takes away — a call, a notification, the edge gesture —
  // is not a reader asking for the next picture.
  it("keeps the picture it was on when the touch is cancelled", () => {
    open(0);
    Element.prototype.setPointerCapture = vi.fn();
    finger("pointerdown", 200);
    finger("pointermove", 40);
    finger("pointercancel", 40);
    expect(title()).toBe("one.png");
    expect(stage()?.style.transform).not.toContain("-160px");
  });

  it("stops at both ends instead of wrapping", () => {
    open(0);
    press("ArrowLeft");
    swipe(340);
    expect(title()).toBe("one.png");

    press("ArrowRight");
    press("ArrowRight");
    press("ArrowRight");
    expect(title()).toBe("three.png");
    swipe(60);
    expect(title()).toBe("three.png");
  });

  it("says where the reader stands and how to move", () => {
    open(0);
    const hint = document.querySelector('[aria-live="polite"]')?.textContent;
    expect(hint).toContain("1 of 3");
    expect(hint).toContain("swipe");
  });

  // A viewer with nowhere to step must not step: a lone picture sliding under the
  // thumb would be reporting a gallery that is not there.
  it("does not swipe a picture that is alone", () => {
    open(0, [pictures[0]]);
    swipe(60);
    expect(title()).toBe("one.png");
    expect(stage()?.style.transform).not.toContain("translate3d(-140px");
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).not.toContain("of 1");
  });
});

describe("the grid is the gallery", () => {
  it("hands a tapped tile the pictures laid out beside it", () => {
    act(() =>
      root.render(
        <MediaGrid summary="2 images · 16B">
          {[0, 1].map((at) => (
            <MediaTile key={at}>
              <ExpandableImage
                src={`blob:pic-${at}`}
                alt={`pic-${at}.png`}
                className="size-8"
                galleryAt={at}
              />
            </MediaTile>
          ))}
        </MediaGrid>,
      ),
    );

    act(() => control("Open pic-0.png full screen").click());
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).toContain("1 of 2");

    act(() => {
      window.dispatchEvent(new KeyboardEvent("keydown", { key: "ArrowRight" }));
    });
    expect(document.querySelector('[role="dialog"] h2')?.textContent).toBe(
      "pic-1.png",
    );
  });

  // The viewer hands the flattened result back to the slot the trigger owns, so
  // an editable picture that had walked to a neighbour would replace the wrong
  // attachment: it stays a single-picture viewer.
  it("does not step an editable picture", () => {
    act(() =>
      root.render(
        <MediaGrid summary="2 images · 16B">
          {[0, 1].map((at) => (
            <MediaTile key={at}>
              <ExpandableImage
                src={`blob:edit-${at}`}
                alt={`edit-${at}.png`}
                className="size-8"
                galleryAt={at}
                onApply={() => undefined}
              />
            </MediaTile>
          ))}
        </MediaGrid>,
      ),
    );

    act(() => control("Open edit-0.png full screen").click());
    expect(
      document.querySelector('[aria-live="polite"]')?.textContent,
    ).not.toContain("of 2");
    act(() => {
      window.dispatchEvent(new KeyboardEvent("keydown", { key: "ArrowRight" }));
    });
    expect(document.querySelector('[role="dialog"] h2')?.textContent).toBe(
      "edit-0.png",
    );
  });
});

// Regression, reported from an iPad: writing by hand on an open picture selected
// the picture instead of drawing on it. A stroke may begin on the dark margin,
// so the viewport that owns that stroke has to refuse selection as well as the
// sheet over the picture does.
it("refuses selection under the pen, margin included", () => {
  const viewport = document.querySelector<HTMLDivElement>(
    '[role="dialog"] .cursor-grab',
  );
  const canvas = document.querySelector("canvas");
  if (!viewport || !canvas) throw new Error("viewer surface not found");
  expect(viewport.className).toContain("select-none");
  expect(canvas.className).toContain("select-none");
});
