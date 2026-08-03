"""Rasterize captured TUI frames to PNG (and attach them to the conversation).

Input is what `com.blockether.vis.ext.channel-tui.capture/write-json!` produces:
`{"cols": n, "rows": n, "frames": [[[[char, fg, bg, bold], ...], ...], ...]}` —
one frame per terminal flush, every cell carrying the exact character, colors and
bold flag the virtual terminal held.

Usage from `python_execution` (see AGENTS.md -> TUI rendering):

    exec(open("extensions/channels/vis-channel-tui/tools/tui_png.py").read())
    show_frames("/tmp/vis-frames.json", "/tmp/vis-magit", label="magit")

`show_frames` writes the PNGs and attaches them with the sandbox `vis_attach`
shim when it is available, so the human sees the REAL render, not a text dump.

SPEED — the one rule: never draw on the frame image. In this sandbox every
`ImageDraw` call on a large image costs ~0.3s (the whole buffer crosses the
imaging FFI boundary), while the same call on a cell-sized image costs ~0.15ms
and `Image.paste` is free. So a frame is assembled purely out of pastes:
glyphs are rendered once into a cached (char, fg, bg, bold) tile — the cache
survives across frames and across calls — and background runs are solid tiles.
A 120x40 frame lands in under a second instead of ~35s.

Box-drawing characters are painted as lines through the cell centre rather than
as glyphs, so borders connect seamlessly at any cell size.
"""

from PIL import Image, ImageDraw, ImageFont

# The grid: a terminal cell is about twice as tall as it is wide. The font size
# is NOT hardcoded — glyph rasterisation scales differently per font (and per
# imaging backend), so `_fit` measures real ink and picks the largest size whose
# widest glyph still fits the cell. That is what keeps text from bleeding into
# its neighbour, which is what made earlier renders look mangled.
CELL_W, CELL_H = 18, 34

# Hard ceiling on EITHER PNG dimension. These frames are attached and viewed
# inline, and a 120-column capture at the natural cell size is 2160px wide —
# over the limit. `_capped_cell` shrinks the cell (and `font` refits the glyphs
# to that smaller cell) until both dimensions fit, so the image stays legible
# instead of being scaled after the fact. Below MIN_CELL_* text stops being
# readable at all, so a capture bigger than 256x128 cells is rendered oversized
# rather than illegibly small.
MAX_PNG_PX = 1024
MIN_CELL_W, MIN_CELL_H = 4, 8

# Descending preference: JetBrains Mono first — round, wide-countered, easy to
# read at small sizes — then the usual system monospaces.
FONT_FAMILIES = (
    (
        "~/Library/Fonts/JetBrainsMono-Regular.ttf",
        "~/Library/Fonts/JetBrainsMono-Bold.ttf",
    ),
    ("~/Library/Fonts/FiraCode-Regular.ttf", "~/Library/Fonts/FiraCode-Bold.ttf"),
    ("/System/Library/Fonts/SFNSMono.ttf", None),
    ("/System/Library/Fonts/Menlo.ttc", None),
    ("/Library/Fonts/DejaVuSansMono.ttf", "/Library/Fonts/DejaVuSansMono-Bold.ttf"),
    (
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf",
    ),
    (
        "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
        "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf",
    ),
)

# Sizes tried by `_fit`, largest first.
FONT_SIZES = tuple(range(24, 6, -1))

# Glyphs that define the extremes of the box a cell must hold.
PROBE = "Wg|"

# left, right, up, down arms of each box-drawing glyph
BOX_ARMS = {
    "\u2500": (1, 1, 0, 0),
    "\u2501": (1, 1, 0, 0),
    "\u2502": (0, 0, 1, 1),
    "\u2503": (0, 0, 1, 1),
    "\u250c": (0, 1, 0, 1),
    "\u250f": (0, 1, 0, 1),
    "\u2510": (1, 0, 0, 1),
    "\u2513": (1, 0, 0, 1),
    "\u2514": (0, 1, 1, 0),
    "\u2517": (0, 1, 1, 0),
    "\u2518": (1, 0, 1, 0),
    "\u251b": (1, 0, 1, 0),
    "\u251c": (0, 1, 1, 1),
    "\u2524": (1, 0, 1, 1),
    "\u252c": (1, 1, 0, 1),
    "\u2534": (1, 1, 1, 0),
    "\u253c": (1, 1, 1, 1),
    "\u2550": (1, 1, 0, 0),
    "\u2551": (0, 0, 1, 1),
    "\u2574": (1, 0, 0, 0),
    "\u2576": (0, 1, 0, 0),
}


def _expand(path):
    import os as _os

    return _os.path.expanduser(path)


def _load(path, size, index=None):
    # The host resolves a font by FILE, so a family we do not actually have on
    # disk must LOSE the probe in `font` instead of silently answering with the
    # fallback face.
    import os as _os

    resolved = _expand(path)
    if not _os.path.exists(resolved):
        return None
    try:
        if index is None:
            return ImageFont.truetype(resolved, size)
        return ImageFont.truetype(resolved, size, index=index)
    except Exception:
        return None


def _ink(font_obj):
    """(width, top, bottom) of the probe glyphs' ink, relative to the draw origin.

    Measured by actually rasterising, because font metrics are not a reliable
    predictor of what this backend paints.
    """
    pad = 8
    width = top = bottom = None
    for ch in PROBE:
        img = Image.new("RGB", (8 * CELL_W, 6 * CELL_H), "#000000")
        ImageDraw.Draw(img).text((pad, pad), ch, font=font_obj, fill="#ffffff")
        box = img.getbbox()
        if not box:
            continue
        x0, y0, x1, y1 = box
        width = (x1 - pad) if width is None else max(width, x1 - pad)
        top = (y0 - pad) if top is None else min(top, y0 - pad)
        bottom = (y1 - pad) if bottom is None else max(bottom, y1 - pad)
    if width is None:
        return (CELL_W, 0, CELL_H)
    return (width, top, bottom)


def _fit(path, index=None, heavy=None, cell_w=CELL_W, cell_h=CELL_H):
    """Largest size of this face whose widest/tallest glyph fits one cell.

    Returns `(font, dx, dy)` — the offsets centre the glyph in its cell.
    """
    for size in FONT_SIZES:
        font_obj = (_load(heavy, size) if heavy else None) or _load(path, size, index)
        if font_obj is None:
            return None
        width, top, bottom = _ink(font_obj)
        height = bottom - top
        if width <= cell_w and height <= cell_h - 2:
            return (
                font_obj,
                max(0, (cell_w - width) // 2),
                max(0, (cell_h - height) // 2) - top,
            )
    return None


_FONTS = {}


def font(bold=False, cell_w=CELL_W, cell_h=CELL_H):
    """`(font, dx, dy)` for this weight, fitted to THIS cell size and cached."""
    key = (bold, cell_w, cell_h)
    if key not in _FONTS:
        fitted = None
        for regular, heavy in FONT_FAMILIES:
            fitted = _fit(
                regular,
                index=1 if bold else None,
                heavy=heavy if bold else None,
                cell_w=cell_w,
                cell_h=cell_h,
            )
            if fitted:
                break
        _FONTS[key] = fitted or (ImageFont.load_default(), 0, 0)
    return _FONTS[key]


# (char, fg, bg, bold, cell_w, cell_h) -> painted cell. Global on purpose: the
# second frame of a capture is almost entirely cache hits.
_TILES = {}


def _tile(ch, fg, bg, bold, cell_w, cell_h):
    """One fully painted cell — cheap only because the image is cell-sized."""
    key = (ch, fg, bg, bold, cell_w, cell_h)
    hit = _TILES.get(key)
    if hit is not None:
        return hit
    img = Image.new("RGB", (cell_w, cell_h), bg)
    draw = ImageDraw.Draw(img)
    arms = BOX_ARMS.get(ch)
    if arms:
        left, right, up, down = arms
        cx, cy = cell_w // 2, cell_h // 2
        if left:
            draw.rectangle([0, cy, cx, cy + 1], fill=fg)
        if right:
            draw.rectangle([cx, cy, cell_w, cy + 1], fill=fg)
        if up:
            draw.rectangle([cx, 0, cx + 1, cy], fill=fg)
        if down:
            draw.rectangle([cx, cy, cx + 1, cell_h], fill=fg)
    else:
        face, dx, dy = font(bold, cell_w, cell_h)
        draw.text((dx, dy), ch, font=face, fill=fg)
    _TILES[key] = img
    return img


def _solid(width, height, color):
    return Image.new("RGB", (width, height), color)


def _capped_cell(rows, cols, cell_w=CELL_W, cell_h=CELL_H):
    """The largest cell of this shape whose frame fits inside `MAX_PNG_PX`."""
    scale = min(
        1.0, MAX_PNG_PX / float(cols * cell_w), MAX_PNG_PX / float(rows * cell_h)
    )
    if scale >= 1.0:
        return (cell_w, cell_h)
    return (
        max(MIN_CELL_W, int(cell_w * scale)),
        max(MIN_CELL_H, int(cell_h * scale)),
    )


def render_frame(frame, cell_w=CELL_W, cell_h=CELL_H):
    """One captured frame -> PIL image, colors and bold exactly as captured.

    The cell shrinks when it has to: the PNG never exceeds `MAX_PNG_PX` on either
    side, and the font is refitted to whatever cell that leaves.
    """
    rows, cols = len(frame), max(len(r) for r in frame)
    cell_w, cell_h = _capped_cell(rows, cols, cell_w, cell_h)
    img = Image.new("RGB", (cols * cell_w, rows * cell_h), "#000000")
    for y, row in enumerate(frame):
        # Backgrounds as runs of solid tiles: one paste per colour change.
        x = 0
        while x < len(row):
            bg = row[x][2]
            end = x
            while end < len(row) and row[end][2] == bg:
                end += 1
            img.paste(_solid((end - x) * cell_w, cell_h, bg), (x * cell_w, y * cell_h))
            x = end
        for x, cell in enumerate(row):
            ch, fg, bg, bold = cell[0], cell[1], cell[2], bool(cell[3])
            if ch == " " or ch == "":
                continue
            img.paste(_tile(ch, fg, bg, bold, cell_w, cell_h), (x * cell_w, y * cell_h))
    return img


def render_frames(json_path, out_prefix, indexes=None):
    """Rasterize a capture file; returns the written PNG paths."""
    import json as _json

    with open(json_path) as handle:
        capture = _json.load(handle)
    frames = capture["frames"]
    picks = range(len(frames)) if indexes is None else indexes
    paths = []
    for i in picks:
        path = f"{out_prefix}-{i}.png"
        render_frame(frames[i]).save(path)
        paths.append(path)
    return paths


def show_frames(json_path, out_prefix, indexes=None, label=None):
    """Rasterize AND attach, so the human sees the real render. Returns paths."""
    paths = render_frames(json_path, out_prefix, indexes)
    attach = globals().get("vis_attach") or getattr(
        __import__("builtins"), "vis_attach", None
    )
    if attach:
        for i, path in enumerate(paths):
            # sandbox shim signature: vis_attach(path, kind, media_type, filename, ...)
            attach(path, filename=f"{label or 'tui-frame'}-{i}.png")
    return paths
