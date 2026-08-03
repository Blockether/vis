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

Speed matters: `ImageDraw.text` costs milliseconds per call, so glyphs are
rendered once into a cached tile per (char, fg, bg, bold) and pasted, background
runs are filled as rectangles, and box-drawing characters are painted as lines
through the cell center so borders connect seamlessly.
"""

from PIL import Image, ImageDraw, ImageFont

CELL_W, CELL_H, FONT_SIZE = 13, 24, 16

FONT_CANDIDATES = (
    "/System/Library/Fonts/Menlo.ttc",
    "/System/Library/Fonts/Monaco.ttf",
    "/Library/Fonts/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
)

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


def _font(bold=False):
    for path in FONT_CANDIDATES:
        try:
            return ImageFont.truetype(path, FONT_SIZE, index=1 if bold else 0)
        except Exception:
            try:
                return ImageFont.truetype(path, FONT_SIZE)
            except Exception:
                continue
    return ImageFont.load_default()


_FONTS = {}


def font(bold=False):
    if bold not in _FONTS:
        _FONTS[bold] = _font(bold)
    return _FONTS[bold]


def _tile(ch, fg, bg, bold):
    """One fully painted cell: cached, because text drawing dominates runtime."""
    img = Image.new("RGB", (CELL_W, CELL_H), bg)
    draw = ImageDraw.Draw(img)
    arms = BOX_ARMS.get(ch)
    if arms:
        left, right, up, down = arms
        cx, cy = CELL_W // 2, CELL_H // 2
        if left:
            draw.rectangle([0, cy, cx, cy + 1], fill=fg)
        if right:
            draw.rectangle([cx, cy, CELL_W, cy + 1], fill=fg)
        if up:
            draw.rectangle([cx, 0, cx + 1, cy], fill=fg)
        if down:
            draw.rectangle([cx, cy, cx + 1, CELL_H], fill=fg)
    else:
        draw.text((1, 2), ch, font=font(bold), fill=fg)
    return img


def render_frame(frame, cell_w=CELL_W, cell_h=CELL_H):
    """One captured frame -> PIL image, colors and bold exactly as captured."""
    rows, cols = len(frame), max(len(r) for r in frame)
    img = Image.new("RGB", (cols * cell_w, rows * cell_h), "#000000")
    draw = ImageDraw.Draw(img)
    tiles = {}
    for y, row in enumerate(frame):
        # backgrounds first, as runs, so we pay for a handful of rectangles
        x = 0
        while x < len(row):
            bg = row[x][2]
            end = x
            while end < len(row) and row[end][2] == bg:
                end += 1
            draw.rectangle(
                [x * cell_w, y * cell_h, end * cell_w - 1, (y + 1) * cell_h - 1],
                fill=bg,
            )
            x = end
        for x, cell in enumerate(row):
            ch, fg, bg, bold = cell[0], cell[1], cell[2], bool(cell[3])
            if ch == " " or ch == "":
                continue
            key = (ch, fg, bg, bold)
            tile = tiles.get(key)
            if tile is None:
                tile = tiles[key] = _tile(ch, fg, bg, bold)
            img.paste(tile, (x * cell_w, y * cell_h))
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
