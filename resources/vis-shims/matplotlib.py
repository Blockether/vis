# vis sandbox matplotlib-compat shim (imaging-backed pyplot subset).


def __vis_install_matplotlib__():
    import sys
    import types
    import base64

    _COLORS = set("bgrcmykw")
    _MARKERS = set("o.,x+*sdv^<>ph")

    _state = {}

    def _reset():
        _state.clear()
        _state.update(
            {
                "series": [],
                "title": None,
                "xlabel": None,
                "ylabel": None,
                "grid": False,
                "legend": False,
                "xlim": None,
                "ylim": None,
                "xscale": "linear",
                "yscale": "linear",
                "annotations": [],
                "xticks": None,
                "xticklabels": None,
                "yticks": None,
                "yticklabels": None,
                "width": 640,
                "dpi": 100.0,
                "height": 480,
                "axis_off": False,
                "projection": None,
                "zlabel": None,
                "zlim": None,
                "zticks": None,
                "elev": None,
                "azim": None,
                "ax3d": None,
            }
        )

    _reset()

    def _nums(v):
        out = []
        if v is None:
            return out
        try:
            for e in v:
                try:
                    out.append(float(e))
                except Exception:
                    out.append(0.0)
        except TypeError:
            out.append(float(v))
        return out

    def _add_series(
        kind, x, y, label, color, marker=None, linestyle=None, y2=None, labels=None
    ):
        s = {
            "kind": kind,
            "x": _nums(x),
            "y": _nums(y),
            "label": label,
            "color": color,
            "marker": marker,
            "linestyle": linestyle,
            "y2": (_nums(y2) if y2 is not None else None),
            "labels": labels,
        }
        _state["series"].append(s)
        return s

    def _parse_fmt(fmt):
        color = None
        marker = None
        line = None
        s = fmt or ""
        for ls in ("--", "-.", ":", "-"):
            if ls in s:
                line = ls
                s = s.replace(ls, "", 1)
                break
        for ch in s:
            if ch in _COLORS and color is None:
                color = ch
            elif ch in _MARKERS and marker is None:
                marker = ch
        return color, marker, line

    def figure(num=None, figsize=None, dpi=None, **kwargs):
        # a new figure starts empty, like matplotlib -- otherwise the previous
        # figure's series leak into it.
        _reset()
        d = float(dpi or 100)
        _state["dpi"] = d
        if figsize:
            _state["width"] = int(float(figsize[0]) * d)
            _state["height"] = int(float(figsize[1]) * d)
        return _Figure()

    def plot(*args, **kwargs):
        a = list(args)
        handles = []
        i = 0
        n = len(a)
        while i < n:
            if i + 1 < n and not isinstance(a[i + 1], str):
                x = list(a[i])
                y = list(a[i + 1])
                i += 2
            else:
                y = list(a[i])
                x = list(range(len(y)))
                i += 1
            fmt = ""
            if i < n and isinstance(a[i], str):
                fmt = a[i]
                i += 1
            color, marker, line = _parse_fmt(fmt)
            kind = "scatter" if (marker and not line) else "line"
            s = _add_series(
                kind,
                x,
                y,
                kwargs.get("label"),
                kwargs.get("color", color),
                marker=kwargs.get("marker", marker),
                linestyle=kwargs.get("linestyle", kwargs.get("ls", line)),
            )
            handles.append(_Line(s))
        return handles

    def scatter(x, y, s=None, c=None, label=None, color=None, **kwargs):
        _add_series("scatter", x, y, label, color or c)
        return None

    def _is_number(v):
        if isinstance(v, bool):
            return False
        if isinstance(v, (int, float)):
            return True
        try:
            float(v)
            return True
        except (TypeError, ValueError):
            return False

    def _categorical(vals):
        # matplotlib maps string/categorical x onto an integer axis, deduping
        # to distinct categories in first-seen order; each value takes its
        # category's index. Numeric input keeps the float path (None, None).
        seq = list(vals)
        if not seq or all(_is_number(v) for v in seq):
            return None, None
        order = []
        index = {}
        for v in seq:
            key = str(v)
            if key not in index:
                index[key] = len(order)
                order.append(key)
        return [index[str(v)] for v in seq], order

    def bar(x, height, width=0.8, label=None, color=None, **kwargs):
        pos, labels = _categorical(x)
        if pos is not None:
            _add_series("bar", pos, height, label, color, labels=labels)
        else:
            _add_series("bar", x, height, label, color)
        return None

    def barh(y, width, height=0.8, label=None, color=None, **kwargs):
        pos, labels = _categorical(y)
        if pos is not None:
            _add_series("bar", pos, width, label, color, labels=labels)
        else:
            _add_series("bar", y, width, label, color)
        return None

    def hist(x, bins=10, label=None, color=None, **kwargs):
        xs = _nums(x)
        if not xs:
            return [], []
        bins = int(bins) if bins else 10
        lo = min(xs)
        hi = max(xs)
        if hi == lo:
            hi = lo + 1.0
        w = (hi - lo) / bins
        counts = [0] * bins
        for v in xs:
            idx = int((v - lo) / w)
            if idx >= bins:
                idx = bins - 1
            if idx < 0:
                idx = 0
            counts[idx] += 1
        centers = [lo + (i + 0.5) * w for i in range(bins)]
        _add_series("bar", centers, counts, label, color)
        return counts, centers

    def fill_between(x, y1, y2=0, label=None, color=None, **kwargs):
        xs = list(x)
        n = len(xs)
        y1l = _nums(y1)
        if isinstance(y2, (int, float)):
            y2l = [float(y2)] * n
        else:
            y2l = _nums(y2)
        _add_series("fill", xs, y1l, label, color, y2=y2l)
        return None

    def step(x, y, *args, label=None, color=None, **kwargs):
        _add_series("step", x, y, label, color)
        return None

    def axhline(y=0, color=None, linestyle=None, ls=None, label=None, **kwargs):
        _add_series("hline", [], [y], label, color, linestyle=(linestyle or ls))
        return None

    def axvline(x=0, color=None, linestyle=None, ls=None, label=None, **kwargs):
        _add_series("vline", [x], [], label, color, linestyle=(linestyle or ls))
        return None

    def pie(sizes, labels=None, colors=None, autopct=None, **kwargs):
        _add_series(
            "pie",
            list(sizes),
            [],
            None,
            None,
            labels=(list(labels) if labels else None),
        )
        return None

    def errorbar(x, y, yerr=None, xerr=None, fmt="", label=None, color=None, **kwargs):
        _add_series("line", x, y, label, color)
        return None

    def text(x, y, s, **kwargs):
        _state["annotations"].append({"x": float(x), "y": float(y), "text": str(s)})
        return None

    def annotate(s, xy=None, xytext=None, **kwargs):
        pt = xytext or xy or (0, 0)
        _state["annotations"].append(
            {"x": float(pt[0]), "y": float(pt[1]), "text": str(s)}
        )
        return None

    def title(s, **kwargs):
        _state["title"] = str(s)

    def suptitle(s, **kwargs):
        _state["title"] = str(s)

    def xlabel(s, **kwargs):
        _state["xlabel"] = str(s)

    def ylabel(s, **kwargs):
        _state["ylabel"] = str(s)

    def grid(b=True, **kwargs):
        _state["grid"] = bool(b)

    def legend(*args, **kwargs):
        _state["legend"] = True

    def xlim(*args, **kwargs):
        if len(args) == 2:
            _state["xlim"] = [float(args[0]), float(args[1])]
        elif len(args) == 1 and args[0] is not None:
            _state["xlim"] = [float(args[0][0]), float(args[0][1])]
        return _state["xlim"]

    def ylim(*args, **kwargs):
        if len(args) == 2:
            _state["ylim"] = [float(args[0]), float(args[1])]
        elif len(args) == 1 and args[0] is not None:
            _state["ylim"] = [float(args[0][0]), float(args[0][1])]
        return _state["ylim"]

    def xscale(v, **kwargs):
        _state["xscale"] = str(v)

    def yscale(v, **kwargs):
        _state["yscale"] = str(v)

    def semilogx(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state["xscale"] = "log"
        return r

    def semilogy(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state["yscale"] = "log"
        return r

    def loglog(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state["xscale"] = "log"
        _state["yscale"] = "log"
        return r

    def _ticks(axis, args, kwargs):
        # matplotlib's xticks()/yticks(): no args reads the current locator,
        # (ticks[, labels]) sets it. Positions are floats in data space; labels
        # ride along by index and the renderer falls back to formatted numbers.
        ticks = kwargs.get("ticks")
        labels = kwargs.get("labels")
        if len(args) > 0:
            ticks = args[0]
        if len(args) > 1:
            labels = args[1]
        if ticks is not None:
            vals = [float(t) for t in list(ticks)]
            _state[axis + "ticks"] = vals if vals else None
        if labels is not None:
            _state[axis + "ticklabels"] = [str(v) for v in list(labels)]
        return (
            list(_state.get(axis + "ticks") or []),
            list(_state.get(axis + "ticklabels") or []),
        )

    def xticks(*args, **kwargs):
        return _ticks("x", args, kwargs)

    def yticks(*args, **kwargs):
        return _ticks("y", args, kwargs)

    def tight_layout(*args, **kwargs):
        return None

    def subplots_adjust(*args, **kwargs):
        return None

    def clf(*args, **kwargs):
        _reset()

    def cla(*args, **kwargs):
        _reset()

    def close(*args, **kwargs):
        _reset()

    def _human_size(n):
        # Compact byte-size label for the vis-image fence (B / KB / MB).
        n = float(n)
        for unit in ("B", "KB", "MB"):
            if n < 1024.0 or unit == "MB":
                return (str(int(n)) + " B") if unit == "B" else ("%.1f %s" % (n, unit))
            n = n / 1024.0

    _img_seq = [0]

    def _emit_image():
        # Render the current figure to a PNG on the HOST (imaging) and print a
        # `vis-image` fence: 5 header lines (summary / path / mime / WxH / size) the
        # channel reads to paint the picture inline, plus the ASCII plot appended as
        # the fallback body for non-graphical terminals. The PNG is written HOST-side
        # (`__vis_mpl_render_file__`), so this works even when the sandbox's own
        # Python filesystem is denied. Returns True on success, False (no bridge /
        # render error) so show() can fall back to the plain ASCII print.
        render = globals().get("__vis_mpl_render_file__")
        if render is None:
            return False
        try:
            spec = _spec()
            env = render(spec)
            if not env[0]:
                return False
            path, w, h, nbytes = env[1]
            path = str(path)
            w = int(w)
            h = int(h)
            size = _human_size(int(nbytes))
            _img_seq[0] += 1
            title = spec.get("title") or "matplotlib figure"
            summary = (
                "[Image #"
                + str(_img_seq[0])
                + ": "
                + str(title)
                + " "
                + str(w)
                + "×"
                + str(h)
                + ", "
                + size
                + "]"
            )
            fence = "`" * 4
            lines = [
                fence + "vis-image",
                summary,
                path,
                "image/png",
                str(w) + "x" + str(h),
                size,
            ]
            if spec.get("series"):
                lines.append(_render_ascii(spec, 74, 22, False))
            lines.append(fence)
            print("\n".join(lines))
            return True
        except Exception:
            return False

    def show(*args, **kwargs):
        # Prefer the host imaging PNG backend: write the figure to a temp file and emit a
        # `vis-image` fence so a graphical TUI/web paints it inline, the ASCII plot
        # riding along as the fence's text fallback. No bridge (or a render failure)
        # falls back to printing the ASCII plot straight to stdout, so `plt.show()`
        # still shows the plot in a purely textual environment.
        if _state.get("series"):
            if _emit_image():
                return None
            print(
                _render_ascii(
                    _spec(),
                    kwargs.get("width", 74),
                    kwargs.get("height", 22),
                    kwargs.get("color", True),
                )
            )
        return None

    class _Line(object):
        # Handle returned by plot(); supports `line, = plt.plot(...)` unpacking
        # and the common set_* mutators (they edit the accumulated series).
        def __init__(self, s):
            self._s = s

        def set_label(self, v):
            self._s["label"] = v
            return None

        def set_color(self, v):
            self._s["color"] = v
            return None

        def set_linestyle(self, v):
            self._s["linestyle"] = v
            return None

        def set_linewidth(self, *a, **k):
            return None

        def get_label(self):
            return self._s.get("label")

    def axis(*args, **kwargs):
        if not args:
            return (0.0, 1.0, 0.0, 1.0)
        a = args[0]
        if a is False or a == "off":
            _state["axis_off"] = True
        elif a is True or a == "on":
            _state["axis_off"] = False
        elif isinstance(a, (list, tuple)) and len(a) == 4:
            _state["xlim"] = [float(a[0]), float(a[1])]
            _state["ylim"] = [float(a[2]), float(a[3])]
        return (0.0, 1.0, 0.0, 1.0)

    def _quartiles(vals):
        xs = sorted(_nums(vals))
        if not xs:
            return None

        def q(p):
            if len(xs) == 1:
                return xs[0]
            idx = p * (len(xs) - 1)
            lo = int(idx)
            hi = min(lo + 1, len(xs) - 1)
            return xs[lo] + (xs[hi] - xs[lo]) * (idx - lo)

        return {"lo": xs[0], "q1": q(0.25), "q2": q(0.5), "q3": q(0.75), "hi": xs[-1]}

    def boxplot(data, positions=None, labels=None, **kwargs):
        try:
            first = data[0]
            seqs = data if hasattr(first, "__iter__") else [data]
        except Exception:
            seqs = [data]
        stats = []
        for d in seqs:
            st = _quartiles(d)
            if st:
                stats.append(st)
        pos = (
            [float(p) for p in positions]
            if positions
            else list(range(1, len(stats) + 1))
        )
        ally = []
        for st in stats:
            ally.append(st["lo"])
            ally.append(st["hi"])
        s = _add_series("box", pos, ally, None, None)
        s["stats"] = stats
        s["positions"] = pos
        return {"boxes": stats}

    def imshow(
        data, cmap=None, aspect=None, extent=None, vmin=None, vmax=None, **kwargs
    ):
        rows = [_nums(r) for r in data]
        nr = len(rows)
        nc = max((len(r) for r in rows), default=0)
        flat = [v for r in rows for v in r]
        lo = float(vmin) if vmin is not None else (min(flat) if flat else 0.0)
        hi = float(vmax) if vmax is not None else (max(flat) if flat else 1.0)
        s = _add_series("image", [0.0, float(nc)], [0.0, float(nr)], None, None)
        s["rows"] = rows
        s["nrows"] = nr
        s["ncols"] = nc
        s["vmin"] = lo
        s["vmax"] = hi
        return s

    def colorbar(*args, **kwargs):
        return None

    def hlines(
        y,
        xmin=None,
        xmax=None,
        colors=None,
        color=None,
        linestyles=None,
        linestyle=None,
        label=None,
        **kwargs,
    ):
        for yy in _nums(y):
            _add_series(
                "hline",
                [],
                [yy],
                label,
                color or colors,
                linestyle=(linestyle or linestyles),
            )
        return None

    def vlines(
        x,
        ymin=None,
        ymax=None,
        colors=None,
        color=None,
        linestyles=None,
        linestyle=None,
        label=None,
        **kwargs,
    ):
        for xx in _nums(x):
            _add_series(
                "vline",
                [xx],
                [],
                label,
                color or colors,
                linestyle=(linestyle or linestyles),
            )
        return None

    class _Axes(object):
        # Minimal OO Axes: every method delegates to the module-level artist so
        # `fig, ax = plt.subplots(); ax.plot(...)` works like the pyplot API.
        @property
        def patch(self):
            # Background artist stub: `ax.patch.set_facecolor(...)` is common
            # styling boilerplate and must not raise.
            p = getattr(self, "_patch", None)
            if p is None:
                p = _Patch()
                self._patch = p
            return p

        def set_facecolor(self, c, **k):
            return self.patch.set_facecolor(c)

        def set_alpha(self, a, **k):
            return self.patch.set_alpha(a)

        def plot(self, *a, **k):
            return plot(*a, **k)

        def scatter(self, *a, **k):
            return scatter(*a, **k)

        def bar(self, *a, **k):
            return bar(*a, **k)

        def barh(self, *a, **k):
            return barh(*a, **k)

        def hist(self, *a, **k):
            return hist(*a, **k)

        def fill_between(self, *a, **k):
            return fill_between(*a, **k)

        def step(self, *a, **k):
            return step(*a, **k)

        def pie(self, *a, **k):
            return pie(*a, **k)

        def errorbar(self, *a, **k):
            return errorbar(*a, **k)

        def axhline(self, *a, **k):
            return axhline(*a, **k)

        def axvline(self, *a, **k):
            return axvline(*a, **k)

        def text(self, *a, **k):
            return text(*a, **k)

        def annotate(self, *a, **k):
            return annotate(*a, **k)

        def legend(self, *a, **k):
            return legend(*a, **k)

        def grid(self, *a, **k):
            return grid(*a, **k)

        def set(self, **kwargs):
            if "title" in kwargs:
                self.set_title(kwargs["title"])
            if "xlabel" in kwargs:
                self.set_xlabel(kwargs["xlabel"])
            if "ylabel" in kwargs:
                self.set_ylabel(kwargs["ylabel"])
            if "xlim" in kwargs:
                self.set_xlim(kwargs["xlim"])
            if "ylim" in kwargs:
                self.set_ylim(kwargs["ylim"])
            if "xscale" in kwargs:
                self.set_xscale(kwargs["xscale"])
            if "yscale" in kwargs:
                self.set_yscale(kwargs["yscale"])
            return self

        def set_title(self, s, **k):
            title(s)

        def set_xlabel(self, s, **k):
            xlabel(s)

        def set_ylabel(self, s, **k):
            ylabel(s)

        def set_xlim(self, *a, **k):
            return xlim(*a, **k)

        def set_ylim(self, *a, **k):
            return ylim(*a, **k)

        def set_xscale(self, v, **k):
            xscale(v)

        def set_yscale(self, v, **k):
            yscale(v)

        def set_xticks(self, *a, **k):
            return xticks(*a, **k)

        def set_yticks(self, *a, **k):
            return yticks(*a, **k)

        def tick_params(self, *a, **k):
            return None

        def twinx(self, *a, **k):
            return _Axes()

        def twiny(self, *a, **k):
            return _Axes()

        def axis(self, *a, **k):
            return axis(*a, **k)

        def imshow(self, *a, **k):
            return imshow(*a, **k)

        def boxplot(self, *a, **k):
            return boxplot(*a, **k)

        def hlines(self, *a, **k):
            return hlines(*a, **k)

        def vlines(self, *a, **k):
            return vlines(*a, **k)

        def set_xticklabels(self, labels=None, *a, **k):
            return xticks(None, labels)

        def set_yticklabels(self, labels=None, *a, **k):
            return yticks(None, labels)

        def semilogx(self, *a, **k):
            return semilogx(*a, **k)

        def semilogy(self, *a, **k):
            return semilogy(*a, **k)

        def loglog(self, *a, **k):
            return loglog(*a, **k)

        def stackplot(self, *a, **k):
            return stackplot(*a, **k)

    class _Patch(object):
        # Background artist stand-in for `figure.patch` / `axes.patch`. The
        # renderer paints its own chrome, so the properties are only recorded
        # and read back; nothing here changes the picture.
        def __init__(self):
            self._props = {}

        def _set(self, key, value):
            self._props[key] = value
            return None

        def set(self, **k):
            for key in k:
                self._set(key, k[key])
            return None

        def set_facecolor(self, c, **k):
            return self._set("facecolor", c)

        def set_edgecolor(self, c, **k):
            return self._set("edgecolor", c)

        def set_color(self, c, **k):
            return self._set("color", c)

        def set_alpha(self, a, **k):
            return self._set("alpha", a)

        def set_linewidth(self, w, **k):
            return self._set("linewidth", w)

        def set_visible(self, v=True, **k):
            return self._set("visible", v)

        def get_facecolor(self):
            return self._props.get("facecolor")

        def get_edgecolor(self):
            return self._props.get("edgecolor")

        def get_alpha(self):
            return self._props.get("alpha")

    def _figure_patch():
        # One patch per figure: `plt.gcf()` hands out a fresh _Figure wrapper,
        # so the artist has to live in figure state to keep its identity.
        p = _state.get("patch")
        if not isinstance(p, _Patch):
            p = _Patch()
            _state["patch"] = p
        return p

    class _Canvas(object):
        # `fig.canvas.draw()` and friends: no live event loop, but the size
        # query answers from figure state so blitting code gets sane numbers.
        def draw(self, *a, **k):
            return None

        def draw_idle(self, *a, **k):
            return None

        def flush_events(self, *a, **k):
            return None

        def get_width_height(self):
            return (int(_state["width"]), int(_state["height"]))

        def print_png(self, fname, *a, **k):
            return savefig(fname, format="png")

        def mpl_connect(self, *a, **k):
            return 0

        def mpl_disconnect(self, *a, **k):
            return None

    class _SubplotSpec(object):
        def __init__(self, gridspec, key):
            self.gridspec = gridspec
            self.key = key

    class _GridSpec(object):
        # add_subplot()/add_axes() ignore their layout arguments, so a gridspec
        # only has to be indexable: `fig.add_subplot(gs[0, 1])` then works.
        def __init__(self, nrows=1, ncols=1, **k):
            self.nrows = int(nrows)
            self.ncols = int(ncols)

        def __getitem__(self, key):
            return _SubplotSpec(self, key)

        def new_subplotspec(self, loc, rowspan=1, colspan=1):
            return _SubplotSpec(self, loc)

        def update(self, **k):
            return None

        def tight_layout(self, *a, **k):
            return None

    class _Figure(object):
        # Wraps the single global figure state so the OO idiom
        # `fig, ax = plt.subplots(); ...; fig.savefig(...)` works. Every method
        # delegates to the module-level artist / renderer.
        def savefig(self, *a, **k):
            return savefig(*a, **k)

        def suptitle(self, s, **k):
            _state["title"] = str(s)
            return None

        def tight_layout(self, *a, **k):
            return None

        def subplots_adjust(self, *a, **k):
            return None

        def set_size_inches(self, w, h=None, **k):
            if h is None and hasattr(w, "__len__"):
                w, h = w[0], w[1]
            d = float(_state.get("dpi") or 100.0)
            _state["width"] = int(float(w) * d)
            _state["height"] = int(float(h) * d)
            return None

        def add_subplot(self, *a, **k):
            return _new_axes(*a, **k)

        def add_axes(self, *a, **k):
            return _new_axes(*a, **k)

        def gca(self, *a, **k):
            return gca(*a, **k)

        def colorbar(self, mappable=None, *a, **k):
            return _Colorbar(mappable, k.get("label"))

        def legend(self, *a, **k):
            return legend(*a, **k)

        def clf(self, *a, **k):
            _reset()

        def align_labels(self, *a, **k):
            return None

        # --- artist / canvas / geometry accessors ---------------------------
        # Styling and layout boilerplate written against real matplotlib
        # (`fig.patch.set_facecolor`, `fig.canvas.draw`, `fig.get_size_inches`)
        # has to run unchanged even though the renderer ignores most of it.
        @property
        def patch(self):
            return _figure_patch()

        @property
        def canvas(self):
            return _Canvas()

        @property
        def axes(self):
            return [gca()]

        def get_axes(self):
            return [gca()]

        @property
        def dpi(self):
            return float(_state.get("dpi") or 100.0)

        def get_dpi(self):
            return float(_state.get("dpi") or 100.0)

        def set_dpi(self, d, **k):
            _state["dpi"] = float(d)
            return None

        def set_facecolor(self, c, **k):
            return _figure_patch().set_facecolor(c)

        def set_edgecolor(self, c, **k):
            return _figure_patch().set_edgecolor(c)

        def set_alpha(self, a, **k):
            return _figure_patch().set_alpha(a)

        def get_facecolor(self):
            return _figure_patch().get_facecolor()

        def get_size_inches(self):
            d = float(_state.get("dpi") or 100.0)
            return (float(_state["width"]) / d, float(_state["height"]) / d)

        def get_figwidth(self):
            return self.get_size_inches()[0]

        def get_figheight(self):
            return self.get_size_inches()[1]

        def set_figwidth(self, w, **k):
            return self.set_size_inches(w, self.get_figheight())

        def set_figheight(self, h, **k):
            return self.set_size_inches(self.get_figwidth(), h)

        def add_gridspec(self, nrows=1, ncols=1, **k):
            return _GridSpec(nrows, ncols)

        def supxlabel(self, s, **k):
            _state["xlabel"] = str(s)
            return None

        def supylabel(self, s, **k):
            _state["ylabel"] = str(s)
            return None

        def text(self, *a, **k):
            return text(*a, **k)

        def sca(self, ax=None, **k):
            return ax

        def delaxes(self, ax=None, **k):
            return None

        def show(self, *a, **k):
            return show(*a, **k)

        def set_tight_layout(self, *a, **k):
            return None

        def set_constrained_layout(self, *a, **k):
            return None

        def autofmt_xdate(self, *a, **k):
            return None

    class _Colorbar(object):
        # colorbar() handle. The renderer paints no colour ramp, but the usual
        # `cb = fig.colorbar(surf); cb.set_label(...)` follow-up must not explode.
        def __init__(self, mappable=None, label=None):
            self.mappable = mappable
            self.ax = None
            self.label = label

        def set_label(self, s, **k):
            self.label = str(s)
            return None

        def set_ticks(self, *a, **k):
            return None

        def set_ticklabels(self, *a, **k):
            return None

        def update_normal(self, *a, **k):
            return None

        def remove(self):
            return None

    def _projection_of(a, k):
        # `projection=` arrives as a kwarg, inside `subplot_kw=`, or positionally
        # in the `add_subplot(111, "3d")` form.
        p = k.get("projection")
        if p is None and isinstance(k.get("subplot_kw"), dict):
            p = k["subplot_kw"].get("projection")
        if p is None:
            for v in a:
                if isinstance(v, str) and v.lower() in ("3d", "polar", "rectilinear"):
                    p = v
        return str(p).lower() if p is not None else None

    def _new_axes(*a, **k):
        return _Axes3D() if _projection_of(a, k) == "3d" else _Axes()

    def subplots(nrows=1, ncols=1, figsize=None, dpi=None, **kwargs):
        figure(figsize=figsize, dpi=dpi)
        n = int(nrows) * int(ncols)

        def mk():
            return _new_axes(**kwargs)

        if n <= 1:
            return _Figure(), mk()
        return _Figure(), [mk() for _ in range(n)]

    def subplot(*args, **kwargs):
        return _new_axes(*args, **kwargs)

    def axes(*args, **kwargs):
        return _new_axes(*args, **kwargs)

    def gca(*args, **kwargs):
        ax = _state.get("ax3d")
        if ax is not None and _state.get("projection") == "3d":
            return ax
        return _new_axes(*args, **kwargs)

    def gcf(*args, **kwargs):
        return _Figure()

    # ---- mplot3d ----------------------------------------------------------
    # A 3-D axes is the SAME global figure state with `projection` flipped to
    # "3d": the host renderer then normalises the data into a unit cube, rotates
    # it by (elev, azim), and paints depth-sorted, shaded geometry. Grids travel
    # as plain lists of lists, so np.meshgrid output works unchanged.
    def _rows2(v):
        """A 2-D array-like (meshgrid output, list of lists) as list[list[float]]."""
        rows = []
        if v is None:
            return rows
        try:
            items = list(v)
        except TypeError:
            return [[float(v)]]
        for r in items:
            if hasattr(r, "__iter__") and not isinstance(r, (str, bytes)):
                rows.append(_nums(r))
            else:
                try:
                    rows.append([float(r)])
                except Exception:
                    rows.append([0.0])
        return rows

    def _mesh(X, Y, zg):
        """X/Y coerced to Z's shape; a 1-D coordinate array broadcasts like meshgrid."""
        nr = len(zg)
        nc = max((len(r) for r in zg), default=0)

        def spread(g, across):
            flat = [r[0] for r in g] if g and all(len(r) == 1 for r in g) else None
            if flat is None:
                return g
            if across:
                return [list(flat) for _ in range(nr)]
            return [[v] * nc for v in flat]

        def fit(g, default):
            out = []
            for i in range(nr):
                r = list(g[i]) if i < len(g) else []
                r = r + [default(i, j) for j in range(len(r), nc)]
                out.append([float(v) for v in r[:nc]])
            return out

        xg = fit(spread(_rows2(X), True), lambda i, j: float(j))
        yg = fit(spread(_rows2(Y), False), lambda i, j: float(i))
        return xg, yg

    def _cmap_name(c):
        return None if c is None else str(getattr(c, "name", c))

    def _grid_series(kind, X, Y, Z, label=None, color=None, cmap=None, edges=True):
        zg = _rows2(Z)
        xg, yg = _mesh(X, Y, zg)
        xs = [v for r in xg for v in r] or [0.0, 1.0]
        ys = [v for r in yg for v in r] or [0.0, 1.0]
        s = _add_series(kind, [min(xs), max(xs)], [min(ys), max(ys)], label, color)
        s["X"] = xg
        s["Y"] = yg
        s["Z"] = zg
        s["cmap"] = _cmap_name(cmap)
        s["edges"] = bool(edges)
        _state["projection"] = "3d"
        return s

    def _points_series(
        kind,
        xs,
        ys,
        zs,
        label=None,
        color=None,
        marker=None,
        linestyle=None,
        cmap=None,
        c=None,
        sizes=None,
        size=None,
    ):
        if isinstance(c, str) and color is None:
            color, c = c, None
        s = _add_series(kind, xs, ys, label, color, marker=marker, linestyle=linestyle)
        s["z"] = _nums(zs)
        s["c"] = _nums(c) if c is not None else None
        s["cmap"] = _cmap_name(cmap) or ("viridis" if c is not None else None)
        s["sizes"] = _nums(sizes) if sizes is not None else None
        s["size"] = float(size) if size is not None else None
        _state["projection"] = "3d"
        return s

    def _contour_segments(xg, yg, zg, level):
        """Marching squares: one level's line segments, at most two per grid cell."""
        segs = []
        nr = len(zg)
        nc = max((len(r) for r in zg), default=0)
        for i in range(nr - 1):
            for j in range(nc - 1):
                cell = ((i, j), (i, j + 1), (i + 1, j + 1), (i + 1, j))
                hits = []
                for k in range(4):
                    ai, aj = cell[k]
                    bi, bj = cell[(k + 1) % 4]
                    za = zg[ai][aj]
                    zb = zg[bi][bj]
                    if (za < level) == (zb < level):
                        continue
                    t = 0.0 if zb == za else (level - za) / (zb - za)
                    hits.append(
                        (
                            xg[ai][aj] + t * (xg[bi][bj] - xg[ai][aj]),
                            yg[ai][aj] + t * (yg[bi][bj] - yg[ai][aj]),
                        )
                    )
                for k in range(0, len(hits) - 1, 2):
                    p, q = hits[k], hits[k + 1]
                    segs.append([p[0], p[1], level, q[0], q[1], level])
        return segs

    class _Poly3DCollection(object):
        # Artist handle for `mpl_toolkits.mplot3d.art3d` imports. The 3-D artists
        # themselves return their accumulated series dict, as imshow() does.
        def __init__(self, *a, **k):
            self.series = a[0] if a else None

        def set_label(self, v, **k):
            if isinstance(self.series, dict):
                self.series["label"] = v
            return None

    class _Axes3D(object):
        """mplot3d-compatible axes: surfaces, wireframes, 3-D lines/scatter, bars,
        contours, `view_init`, z labels/limits. Every artist appends to the same
        global figure state with `projection` set to "3d"."""

        name = "3d"

        def __init__(self, fig=None, *a, **k):
            _state["projection"] = "3d"
            _state["ax3d"] = self
            # matplotlib draws the 3-D panes WITH gridlines by default
            _state["grid"] = True

        def plot_surface(
            self,
            X,
            Y,
            Z,
            cmap=None,
            color=None,
            label=None,
            edgecolor=None,
            linewidth=None,
            shade=True,
            **k,
        ):
            edges = True
            if linewidth is not None and float(linewidth) == 0.0:
                edges = False
            if isinstance(edgecolor, str) and edgecolor.lower() == "none":
                edges = False
            if cmap is None and color is None:
                cmap = "viridis"
            return _grid_series(
                "surface3d", X, Y, Z, label=label, color=color, cmap=cmap, edges=edges
            )

        def plot_wireframe(self, X, Y, Z, color=None, label=None, **k):
            return _grid_series("wire3d", X, Y, Z, label=label, color=color)

        def contour(
            self,
            X,
            Y,
            Z,
            levels=None,
            cmap=None,
            colors=None,
            offset=None,
            zdir="z",
            **k,
        ):
            zg = _rows2(Z)
            xg, yg = _mesh(X, Y, zg)
            flat = [v for r in zg for v in r] or [0.0, 1.0]
            lo, hi = min(flat), max(flat)
            if levels is None or isinstance(levels, int):
                n = int(levels) if isinstance(levels, int) else 8
                lv = [lo + (hi - lo) * (i + 1) / (n + 1.0) for i in range(n)]
            else:
                lv = [float(v) for v in levels]
            span = (hi - lo) or 1.0
            name = _cmap_name(cmap) or "viridis"
            out = []
            for v in lv:
                segs = _contour_segments(xg, yg, zg, v)
                if not segs:
                    continue
                if offset is not None:
                    z0 = float(offset)
                    segs = [[a, b, z0, d, e, z0] for a, b, _c, d, e, _f in segs]
                col = (
                    colors
                    if isinstance(colors, str)
                    else get_cmap(name)._hex((v - lo) / span)
                )
                s = _add_series("seg3d", [], [], None, col)
                s["segs"] = segs
                s["z"] = []
                out.append(s)
            _state["projection"] = "3d"
            return out

        contour3D = contour

        def scatter(
            self,
            xs,
            ys,
            zs=None,
            zdir="z",
            s=None,
            c=None,
            cmap=None,
            marker=None,
            color=None,
            label=None,
            depthshade=True,
            **k,
        ):
            pts = list(xs)
            if zs is None:
                zl = [0.0] * len(pts)
            elif hasattr(zs, "__len__") and not isinstance(zs, (str, bytes)):
                zl = zs
            else:
                zl = [float(zs)] * len(pts)
            sizes = s if (s is not None and hasattr(s, "__len__")) else None
            size = None if sizes is not None else s
            return _points_series(
                "scatter3d",
                pts,
                ys,
                zl,
                label=label,
                color=color,
                marker=marker,
                cmap=cmap,
                c=c,
                sizes=sizes,
                size=size,
            )

        scatter3D = scatter

        def plot(self, xs, ys, zs=None, *args, **k):
            fmt = None
            if isinstance(zs, str):
                fmt, zs = zs, None
            for a in args:
                if isinstance(a, str):
                    fmt = a
            color, marker, line = _parse_fmt(fmt)
            pts = list(xs)
            if zs is None:
                zs = [0.0] * len(pts)
            s = _points_series(
                "line3d",
                pts,
                ys,
                zs,
                label=k.get("label"),
                color=k.get("color", color),
                marker=k.get("marker", marker),
                linestyle=k.get("linestyle", line),
            )
            return [_Line(s)]

        plot3D = plot

        def bar3d(self, x, y, z, dx, dy, dz, color=None, shade=True, label=None, **k):
            def col(v, n):
                if hasattr(v, "__len__") and not isinstance(v, (str, bytes)):
                    return _nums(v)
                return [float(v)] * n

            xs = (
                _nums(x)
                if (hasattr(x, "__len__") and not isinstance(x, (str, bytes)))
                else [float(x)]
            )
            n = len(xs)
            s = _add_series(
                "bar3d", xs, col(y, n), label, color if isinstance(color, str) else None
            )
            s["z"] = col(z, n)
            s["dx"] = col(dx, n)
            s["dy"] = col(dy, n)
            s["dz"] = col(dz, n)
            s["colors"] = (
                list(color)
                if (color is not None and not isinstance(color, str))
                else None
            )
            _state["projection"] = "3d"
            return s

        def text(self, x, y, z, s, **k):
            _state["annotations"].append(
                {"x": float(x), "y": float(y), "z": float(z), "text": str(s)}
            )
            return None

        text3D = text

        def text2D(self, x, y, s, **k):
            return None

        def view_init(self, elev=None, azim=None, roll=None, vertical_axis="z", **k):
            if elev is not None:
                _state["elev"] = float(elev)
            if azim is not None:
                _state["azim"] = float(azim)
            return None

        def set_title(self, s, **k):
            title(s)

        def set_xlabel(self, s, **k):
            xlabel(s)

        def set_ylabel(self, s, **k):
            ylabel(s)

        def set_zlabel(self, s, **k):
            _state["zlabel"] = str(s)

        def set_xlim(self, *a, **k):
            return xlim(*a, **k)

        def set_ylim(self, *a, **k):
            return ylim(*a, **k)

        def set_zlim(self, lo=None, hi=None, **k):
            if hi is None and hasattr(lo, "__len__"):
                lo, hi = lo[0], lo[1]
            _state["zlim"] = None if lo is None else [float(lo), float(hi)]
            return _state["zlim"]

        set_xlim3d = set_xlim
        set_ylim3d = set_ylim
        set_zlim3d = set_zlim

        def set_xticks(self, *a, **k):
            return xticks(*a, **k)

        def set_yticks(self, *a, **k):
            return yticks(*a, **k)

        def set_zticks(self, ticks=None, labels=None, **k):
            _state["zticks"] = _nums(ticks) if ticks is not None else None
            return None

        def set(self, **kwargs):
            for key in ("title", "xlabel", "ylabel", "zlabel", "xlim", "ylim", "zlim"):
                if key in kwargs:
                    getattr(self, "set_" + key)(kwargs[key])
            return self

        def grid(self, b=True, **k):
            return grid(b)

        def legend(self, *a, **k):
            return legend(*a, **k)

        def axis(self, *a, **k):
            return axis(*a, **k)

        def set_axis_off(self):
            _state["axis_off"] = True

        def set_axis_on(self):
            _state["axis_off"] = False

        def tick_params(self, *a, **k):
            return None

        def set_box_aspect(self, *a, **k):
            return None

        def set_proj_type(self, *a, **k):
            return None

        def set_facecolor(self, *a, **k):
            return None

        def set_zscale(self, *a, **k):
            return None

        def add_collection3d(self, *a, **k):
            return None

        def mouse_init(self, *a, **k):
            return None

        def get_xlim(self):
            return _state.get("xlim") or (0.0, 1.0)

        def get_ylim(self):
            return _state.get("ylim") or (0.0, 1.0)

        def get_zlim(self):
            return _state.get("zlim") or (0.0, 1.0)

    _TAB10 = [
        (31, 119, 180),
        (255, 127, 14),
        (44, 160, 44),
        (214, 39, 40),
        (148, 103, 189),
        (140, 86, 75),
        (227, 119, 194),
        (127, 127, 127),
        (188, 189, 34),
        (23, 190, 207),
    ]
    _CNAMED = {
        "b": (31, 119, 180),
        "g": (44, 160, 44),
        "r": (214, 39, 40),
        "c": (23, 190, 207),
        "m": (191, 0, 191),
        "y": (188, 189, 34),
        "k": (70, 70, 70),
        "w": (230, 230, 230),
        "blue": (31, 119, 180),
        "orange": (255, 127, 14),
        "green": (44, 160, 44),
        "red": (214, 39, 40),
        "purple": (148, 103, 189),
        "brown": (140, 86, 75),
        "pink": (227, 119, 194),
        "gray": (127, 127, 127),
        "grey": (127, 127, 127),
        "olive": (188, 189, 34),
        "cyan": (23, 190, 207),
        "magenta": (191, 0, 191),
        "yellow": (188, 189, 34),
        "black": (70, 70, 70),
        "white": (230, 230, 230),
    }

    def _rgb(color, idx):
        # Resolve a matplotlib color spec to an (r,g,b) triple; fall back to the
        # tab10 cycle by series index when unknown/None.
        if color is None:
            return _TAB10[idx % len(_TAB10)]
        if isinstance(color, (tuple, list)):
            # An (r,g,b[,a]) NUMERIC tuple scales 0..1 floats to 0..255; a
            # per-element color LIST (e.g. bar(..., color=['#4C9F70', ...]))
            # resolves its first entry, so hex/named specs never reach float().
            nums = color[:3]
            if len(color) >= 3 and all(
                isinstance(v, (int, float)) and not isinstance(v, bool) for v in nums
            ):
                return tuple(max(0, min(255, int(round(float(v) * 255)))) for v in nums)
            if len(color):
                return _rgb(color[0], idx)
            return _TAB10[idx % len(_TAB10)]
        s = str(color).strip()
        if s.startswith("#"):
            h = s[1:]
            if len(h) == 3:
                h = "".join(ch * 2 for ch in h)
            if len(h) >= 6:
                try:
                    return (int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16))
                except ValueError:
                    pass
        if len(s) >= 2 and s[0] == "C" and s[1:].isdigit():
            return _TAB10[int(s[1:]) % len(_TAB10)]
        return _CNAMED.get(s.lower(), _TAB10[idx % len(_TAB10)])

    # Braille dot bit for (col in 0..1, row in 0..3) inside one 2x4 cell.
    _BRAILLE = ((0x01, 0x08), (0x02, 0x10), (0x04, 0x20), (0x40, 0x80))

    def _pts3d(s):
        """Every (x, y, z) a 3-D series occupies - used only for autoscaling."""
        k = str(s.get("kind"))
        if k in ("surface3d", "wire3d"):
            X = s.get("X") or []
            Y = s.get("Y") or []
            Z = s.get("Z") or []
            out = []
            for i in range(len(Z)):
                for j in range(len(Z[i])):
                    if i < len(X) and j < len(X[i]) and i < len(Y) and j < len(Y[i]):
                        out.append((X[i][j], Y[i][j], Z[i][j]))
            return out
        if k == "bar3d":
            xs = s.get("x") or []
            ys = s.get("y") or []
            zs = s.get("z") or []
            dx = s.get("dx") or []
            dy = s.get("dy") or []
            dz = s.get("dz") or []
            out = []
            for i in range(len(xs)):
                out.append((xs[i], ys[i], zs[i]))
                out.append(
                    (
                        xs[i] + (dx[i] if i < len(dx) else 1.0),
                        ys[i] + (dy[i] if i < len(dy) else 1.0),
                        zs[i] + (dz[i] if i < len(dz) else 1.0),
                    )
                )
            return out
        if k == "seg3d":
            out = []
            for g in s.get("segs") or []:
                out.append((g[0], g[1], g[2]))
                out.append((g[3], g[4], g[5]))
            return out
        xs = s.get("x") or []
        ys = s.get("y") or []
        zs = s.get("z") or []
        return [(xs[i], ys[i], zs[i]) for i in range(min(len(xs), len(ys), len(zs)))]

    def _flatten3d(spec):
        """A 3-D spec projected onto the camera plane as an ordinary 2-D spec.

        Same orthographic camera as the host PNG renderer - data normalised into
        the unit cube, rotated by `elev`/`azim`, projected onto the camera's
        right/up basis - so `plt.show()` on a text-only terminal and
        `savefig('f.txt')` draw the SAME figure the graphics terminal paints,
        in braille. Surfaces and wireframes become mesh strands (colour-mapped by
        mean height), bars their visible box edges, contours their segments."""
        import math

        series = spec.get("series") or []
        ev = spec.get("elev")
        av = spec.get("azim")
        el = math.radians(30.0 if ev is None else float(ev))
        az = math.radians(-60.0 if av is None else float(av))
        ca = math.cos(az)
        sa = math.sin(az)
        ce = math.cos(el)
        se = math.sin(el)
        pts = []
        for s in series:
            pts += _pts3d(s)
        if not pts:
            pts = [(0.0, 0.0, 0.0), (1.0, 1.0, 1.0)]

        def rng(idx, lim):
            if lim:
                lo, hi = float(lim[0]), float(lim[1])
            else:
                vals = [float(p[idx]) for p in pts]
                lo, hi = min(vals), max(vals)
            if hi - lo < 1e-12:
                lo, hi = lo - 0.5, hi + 0.5
            return lo, hi

        x0, x1 = rng(0, spec.get("xlim"))
        y0, y1 = rng(1, spec.get("ylim"))
        z0, z1 = rng(2, spec.get("zlim"))

        def prjn(nx, ny, nz):
            return (
                -sa * nx + ca * ny,
                -ca * se * nx - sa * se * ny + ce * nz,
            )

        def prj(x, y, z):
            return prjn(
                (float(x) - x0) / (x1 - x0) - 0.5,
                (float(y) - y0) / (y1 - y0) - 0.5,
                (float(z) - z0) / (z1 - z0) - 0.5,
            )

        out = []

        def add(kind, ps, color=None, label=None, marker=None):
            if not ps:
                return
            out.append(
                {
                    "kind": kind,
                    "x": [p[0] for p in ps],
                    "y": [p[1] for p in ps],
                    "color": color,
                    "label": label,
                    "marker": marker,
                }
            )

        def strand(X, Y, Z, pairs):
            ps = []
            zs = []
            for i, j in pairs:
                if i < len(Z) and j < len(Z[i]) and i < len(X) and j < len(X[i]):
                    ps.append(prj(X[i][j], Y[i][j], Z[i][j]))
                    zs.append(float(Z[i][j]))
            return ps, zs

        # the cube floor, so the braille projection still reads as a box
        floor = [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]
        add("line", [prjn(a, b, -0.5) for a, b in floor + floor[:1]], "#b8b8b8")

        zspan = (z1 - z0) or 1.0
        for s in series:
            k = str(s.get("kind"))
            col = s.get("color")
            lbl = s.get("label")
            if k in ("surface3d", "wire3d"):
                X = s.get("X") or []
                Y = s.get("Y") or []
                Z = s.get("Z") or []
                nr = len(Z)
                nc = max([len(r) for r in Z] or [0])
                cm = get_cmap(s.get("cmap")) if s.get("cmap") else None
                first = True
                for i in list(range(0, nr, max(1, nr // 14))) + [nr - 1]:
                    ps, zs = strand(X, Y, Z, [(i, j) for j in range(nc)])
                    c = col
                    if cm is not None and zs:
                        c = cm._hex((sum(zs) / len(zs) - z0) / zspan)
                    add("line", ps, c, lbl if first else None)
                    first = False
                for j in list(range(0, nc, max(1, nc // 14))) + [nc - 1]:
                    ps, zs = strand(X, Y, Z, [(i, j) for i in range(nr)])
                    c = col
                    if cm is not None and zs:
                        c = cm._hex((sum(zs) / len(zs) - z0) / zspan)
                    add("line", ps, c, None)
            elif k == "scatter3d":
                xs = s.get("x") or []
                ys = s.get("y") or []
                zs = s.get("z") or []
                n = min(len(xs), len(ys), len(zs))
                add(
                    "scatter",
                    [prj(xs[i], ys[i], zs[i]) for i in range(n)],
                    col,
                    lbl,
                    s.get("marker") or "o",
                )
            elif k == "bar3d":
                xs = s.get("x") or []
                ys = s.get("y") or []
                zs = s.get("z") or []
                dx = s.get("dx") or []
                dy = s.get("dy") or []
                dz = s.get("dz") or []
                cols = s.get("colors")
                for i in range(len(xs)):
                    xa = float(xs[i])
                    ya = float(ys[i])
                    za = float(zs[i])
                    xb = xa + float(dx[i] if i < len(dx) else 1.0)
                    yb = ya + float(dy[i] if i < len(dy) else 1.0)
                    zb = za + float(dz[i] if i < len(dz) else 1.0)
                    c = cols[i] if (cols and i < len(cols)) else col
                    top = [(xa, ya), (xb, ya), (xb, yb), (xa, yb)]
                    add(
                        "line",
                        [prj(a, b, zb) for a, b in top + top[:1]],
                        c,
                        lbl if i == 0 else None,
                    )
                    for a, b in top:
                        add("line", [prj(a, b, za), prj(a, b, zb)], c)
            elif k == "seg3d":
                for g in s.get("segs") or []:
                    add("line", [prj(g[0], g[1], g[2]), prj(g[3], g[4], g[5])], col)
            else:
                xs = s.get("x") or []
                ys = s.get("y") or []
                zs = s.get("z") or []
                n = min(len(xs), len(ys), len(zs))
                add(
                    "line",
                    [prj(xs[i], ys[i], zs[i]) for i in range(n)],
                    col,
                    lbl,
                    s.get("marker"),
                )

        box = [
            prjn(a, b, c) for a in (-0.5, 0.5) for b in (-0.5, 0.5) for c in (-0.5, 0.5)
        ]

        def deg(v, d):
            v = d if v is None else float(v)
            return str(int(v)) if float(v) == int(v) else "%.4g" % v

        return {
            "width": spec.get("width"),
            "height": spec.get("height"),
            "title": spec.get("title"),
            "xlabel": "3-D view · elev %s° · azim %s°"
            % (deg(ev, 30.0), deg(av, -60.0)),
            "ylabel": spec.get("zlabel"),
            "series": out,
            "xlim": [min(p[0] for p in box), max(p[0] for p in box)],
            "ylim": [min(p[1] for p in box), max(p[1] for p in box)],
            "legend": spec.get("legend"),
            "grid": False,
            "annotations": [],
            "hide_ticks": True,
        }

    def _render_ascii(spec, width=74, height=22, color=False):
        # Pure-Python renderer of the current figure spec - no JVM, no image.
        # Rasterises line/scatter/step/fill/bar/hline/vline into a Unicode
        # BRAILLE canvas (2x4 sub-cell dots => smooth high-res curves) inside a
        # box-drawing frame with y/x tick labels, title, axis labels and a
        # per-series legend. `color=True` adds ANSI truecolor per series.
        if str(spec.get("projection") or "") == "3d":
            spec = _flatten3d(spec)
        series = spec.get("series") or []
        title = spec.get("title")
        xlabel = spec.get("xlabel")
        ylabel = spec.get("ylabel")
        all_x = []
        all_y = []
        has_bar = False
        cat_labels = None
        for s in series:
            k = str(s.get("kind"))
            if k in ("pie", "image", "box"):
                continue
            if k == "bar":
                has_bar = True
                if cat_labels is None and s.get("labels"):
                    cat_labels = s.get("labels")
            all_x += [float(v) for v in (s.get("x") or [])]
            all_y += [float(v) for v in (s.get("y") or [])]
            if s.get("y2") is not None:
                all_y += [float(v) for v in s.get("y2")]
        xlim = spec.get("xlim")
        ylim = spec.get("ylim")
        if xlim:
            xmin, xmax = float(xlim[0]), float(xlim[1])
        elif all_x:
            xmin, xmax = min(all_x), max(all_x)
        else:
            xmin, xmax = 0.0, 1.0
        ys = list(all_y)
        if has_bar:
            ys.append(0.0)
        if ylim:
            ymin, ymax = float(ylim[0]), float(ylim[1])
        elif ys:
            ymin, ymax = min(ys), max(ys)
        else:
            ymin, ymax = 0.0, 1.0
        if xmax == xmin:
            xmin, xmax = xmin - 1.0, xmax + 1.0
        if ymax == ymin:
            ymin, ymax = ymin - 1.0, ymax + 1.0
        pad = 0.05 * (ymax - ymin)
        ymin -= pad
        ymax += pad
        Wc = max(20, int(width))
        Hc = max(8, int(height))
        DW = Wc * 2
        DH = Hc * 4
        cell = [[[0, None] for _ in range(Wc)] for _ in range(Hc)]

        def dput(dx, dy, rgb):
            if 0 <= dx < DW and 0 <= dy < DH:
                cc = cell[dy // 4][dx // 2]
                cc[0] |= _BRAILLE[dy % 4][dx % 2]
                cc[1] = rgb

        def dxof(x):
            return int(round((float(x) - xmin) / (xmax - xmin) * (DW - 1)))

        def dyof(y):
            return int(round((ymax - float(y)) / (ymax - ymin) * (DH - 1)))

        legend = []
        ci = 0
        for s in series:
            k = str(s.get("kind"))
            if k in ("pie", "image", "box"):
                continue
            rgb = _rgb(s.get("color"), ci)
            xs = [float(v) for v in (s.get("x") or [])]
            ysv = [float(v) for v in (s.get("y") or [])]
            if k == "bar":
                base = dyof(max(ymin, min(ymax, 0.0)))
                for x, y in zip(xs, ysv):
                    dc = dxof(x)
                    dr = dyof(y)
                    lo, hi = (dr, base) if dr <= base else (base, dr)
                    for rr in range(lo, hi + 1):
                        for off in (-1, 0, 1):
                            dput(dc + off, rr, rgb)
            elif k == "hline":
                for y in ysv:
                    dr = dyof(y)
                    for dc in range(DW):
                        dput(dc, dr, rgb)
            elif k == "vline":
                for x in xs:
                    dc = dxof(x)
                    for dr in range(DH):
                        dput(dc, dr, rgb)
            elif k == "fill":
                y2 = [float(v) for v in (s.get("y2") or [])]
                for i2, x in enumerate(xs):
                    if i2 >= len(ysv):
                        break
                    dc = dxof(x)
                    r1 = dyof(ysv[i2])
                    r2 = dyof(y2[i2] if i2 < len(y2) else 0.0)
                    lo, hi = (r1, r2) if r1 <= r2 else (r2, r1)
                    for rr in range(lo, hi + 1):
                        dput(dc, rr, rgb)
            elif k == "scatter":
                for x, y in zip(xs, ysv):
                    dput(dxof(x), dyof(y), rgb)
            elif k == "step":
                pts = list(zip(xs, ysv))
                for (x1, y1), (x2, y2) in zip(pts, pts[1:]):
                    c1, c2 = dxof(x1), dxof(x2)
                    r1, r2 = dyof(y1), dyof(y2)
                    for dc in range(min(c1, c2), max(c1, c2) + 1):
                        dput(dc, r1, rgb)
                    for dr in range(min(r1, r2), max(r1, r2) + 1):
                        dput(c2, dr, rgb)
            else:
                pts = list(zip(xs, ysv))
                for (x1, y1), (x2, y2) in zip(pts, pts[1:]):
                    c1, r1 = dxof(x1), dyof(y1)
                    c2, r2 = dxof(x2), dyof(y2)
                    n = max(abs(c2 - c1), abs(r2 - r1), 1)
                    for t in range(n + 1):
                        f = t / n
                        dput(
                            int(round(c1 + (c2 - c1) * f)),
                            int(round(r1 + (r2 - r1) * f)),
                            rgb,
                        )
                for x, y in pts:
                    dput(dxof(x), dyof(y), rgb)
            lbl = s.get("label")
            legend.append((rgb, str(lbl) if lbl else None))
            ci += 1

        def fmt(v):
            if abs(v) < 1e15 and float(v) == int(v):
                return str(int(v))
            return "%.3g" % v

        def paint(txt, rgb):
            if not color or rgb is None:
                return txt
            return "\x1b[38;2;%d;%d;%dm%s\x1b[0m" % (rgb[0], rgb[1], rgb[2], txt)

        def dim(txt):
            return ("\x1b[90m" + txt + "\x1b[0m") if color else txt

        nyt = 0 if spec.get("hide_ticks") else min(5, Hc)
        yticks = {}
        for i in range(nyt):
            t = (i / (nyt - 1)) if nyt > 1 else 0.0
            yticks[int(round(t * (Hc - 1)))] = fmt(ymax - t * (ymax - ymin))
        lw = max((len(v) for v in yticks.values()), default=1)
        out = []
        if title:
            out.append(" " * (lw + 1) + str(title).center(Wc))
        if ylabel:
            out.append(" " * (lw + 1) + str(ylabel)[:Wc])
        for r in range(Hc):
            buf = []
            for cc in cell[r]:
                bits, rgb = cc
                buf.append(paint(chr(0x2800 + bits), rgb) if bits else " ")
            axis = "┤" if r in yticks else "│"
            out.append(yticks.get(r, "").rjust(lw) + dim(axis) + "".join(buf))
        axisrow = ["─"] * Wc
        nxt = min(5, Wc)
        xt = {}
        if cat_labels:
            for i, lab in enumerate(cat_labels):
                col = int(round((float(i) - xmin) / (xmax - xmin) * (Wc - 1)))
                if 0 <= col < Wc:
                    xt[col] = str(lab)
        else:
            for i in range(nxt):
                t = (i / (nxt - 1)) if nxt > 1 else 0.0
                xt[int(round(t * (Wc - 1)))] = fmt(xmin + t * (xmax - xmin))
        if spec.get("hide_ticks"):
            xt = {}
        for col in xt:
            if 0 <= col < Wc:
                axisrow[col] = "┬"
        out.append(" " * lw + dim("└" + "".join(axisrow)))
        xrow = [" "] * Wc
        for col, label in xt.items():
            start = min(max(0, col - len(label) // 2), Wc - len(label))
            for j, chc in enumerate(label):
                if 0 <= start + j < Wc:
                    xrow[start + j] = chc
        out.append(" " * (lw + 1) + "".join(xrow))
        if xlabel:
            out.append(" " * (lw + 1) + str(xlabel).center(Wc))
        labs = [(rgb, l) for (rgb, l) in legend if l]
        if labs:
            out.append("")
            out.append(
                " " * (lw + 1)
                + "   ".join(paint("●", rgb) + " " + l for rgb, l in labs)
            )
        return "\n".join(out)

    def _spec():
        return {
            "width": _state["width"],
            "height": _state["height"],
            "title": _state["title"],
            "xlabel": _state["xlabel"],
            "ylabel": _state["ylabel"],
            "grid": _state["grid"],
            "legend": _state["legend"],
            "xlim": _state["xlim"],
            "ylim": _state["ylim"],
            "xscale": _state["xscale"],
            "yscale": _state["yscale"],
            "axis_off": _state.get("axis_off", False),
            "annotations": list(_state["annotations"]),
            "xticks": _state.get("xticks"),
            "yticks": _state.get("yticks"),
            "xticklabels": _state.get("xticklabels"),
            "yticklabels": _state.get("yticklabels"),
            "projection": _state.get("projection"),
            "zlabel": _state.get("zlabel"),
            "zlim": _state.get("zlim"),
            "zticks": _state.get("zticks"),
            "elev": _state.get("elev"),
            "azim": _state.get("azim"),
            "series": list(_state["series"]),
        }

    def savefig(fname, format=None, dpi=None, **kwargs):
        # Text targets (.txt/.asc filename, or format 'ascii'/'txt') get the
        # pure-Python ASCII render; everything else goes through the host imaging PNG
        # backend and writes the returned bytes.
        is_text = str(format).lower() in ("ascii", "txt") or (
            isinstance(fname, str) and fname.lower().endswith((".txt", ".asc"))
        )
        if is_text:
            txt = _render_ascii(
                _spec(),
                int(kwargs.get("width", 74)),
                int(kwargs.get("height", 22)),
                bool(kwargs.get("color", False)),
            )
            if hasattr(fname, "write"):
                fname.write(txt)
            else:
                with open(fname, "w") as _f:
                    _f.write(txt)
            return fname
        render = globals().get("__vis_mpl_render__")
        if render is None:
            raise RuntimeError(
                "vis: matplotlib imaging backend is not bound in this sandbox."
            )
        env = render(_spec())
        if not env[0]:
            raise RuntimeError("matplotlib render failed: " + str(env[1]))
        data = base64.b64decode(env[1])
        if hasattr(fname, "write"):
            fname.write(data)
        else:
            with open(fname, "wb") as _f:
                _f.write(data)
        return fname

    class _RcParams(dict):
        # matplotlib.rcParams: a dict that never KeyErrors on unknown keys and
        # tolerates `.update(...)` / item assignment, so user rc tweaks are
        # harmless no-ops that still read back instead of blowing up the sandbox.
        def __missing__(self, key):
            return None

    _rcparams = _RcParams()
    _rcparams.update(
        {
            "figure.figsize": [6.4, 4.8],
            "figure.dpi": 100.0,
            "savefig.dpi": 100.0,
            "lines.linewidth": 1.5,
            "font.size": 10.0,
            "axes.grid": False,
            "interactive": False,
            "backend": "Agg",
        }
    )

    _backend = ["Agg"]

    def use(backend=None, *a, **k):
        # matplotlib.use(...) — record the requested backend name; the vis shim
        # always renders through its imaging backend regardless of the choice.
        if backend is not None:
            _backend[0] = str(backend)
        return None

    def switch_backend(backend=None, *a, **k):
        return use(backend)

    def get_backend():
        return _backend[0]

    def rc(*a, **k):
        return None

    def rcdefaults(*a, **k):
        return None

    def ion(*a, **k):
        return None

    def ioff(*a, **k):
        return None

    def isinteractive(*a, **k):
        return False

    def draw(*a, **k):
        return None

    def draw_if_interactive(*a, **k):
        return None

    def pause(*a, **k):
        return None

    def set_cmap(*a, **k):
        return None

    def margins(*a, **k):
        return None

    def minorticks_on(*a, **k):
        return None

    def minorticks_off(*a, **k):
        return None

    def clim(*a, **k):
        return None

    def figtext(x, y, s, *a, **k):
        _state["annotations"].append({"x": float(x), "y": float(y), "text": str(s)})
        return None

    class _NullCtx(object):
        def __enter__(self):
            return None

        def __exit__(self, *a):
            return False

    def stackplot(x, *ys, labels=None, colors=None, baseline="zero", **kwargs):
        xs = list(x)
        layers = []
        if len(ys) == 1:
            first = ys[0]
            probe = None
            try:
                probe = first[0]
            except Exception:
                probe = None
            if (
                probe is not None
                and hasattr(probe, "__iter__")
                and not isinstance(probe, (str, bytes))
            ):
                layers = [_nums(row) for row in first]
            else:
                layers = [_nums(first)]
        else:
            layers = [_nums(y) for y in ys]
        labs = list(labels) if labels else []
        cols = list(colors) if colors else []
        base = [0.0] * len(xs)
        for k, layer in enumerate(layers):
            top = [
                base[i] + (layer[i] if i < len(layer) else 0.0) for i in range(len(xs))
            ]
            _add_series(
                "fill",
                xs,
                top,
                labs[k] if k < len(labs) else None,
                cols[k] if k < len(cols) else None,
                y2=list(base),
            )
            base = top
        return []

    _VIRIDIS = (
        (0.267, 0.005, 0.329),
        (0.283, 0.141, 0.458),
        (0.254, 0.265, 0.530),
        (0.207, 0.372, 0.553),
        (0.164, 0.471, 0.558),
        (0.128, 0.567, 0.551),
        (0.135, 0.659, 0.518),
        (0.267, 0.749, 0.441),
        (0.478, 0.821, 0.318),
        (0.741, 0.873, 0.150),
        (0.993, 0.906, 0.144),
    )
    _PLASMA = (
        (0.050, 0.030, 0.528),
        (0.294, 0.011, 0.631),
        (0.472, 0.006, 0.660),
        (0.627, 0.126, 0.588),
        (0.752, 0.273, 0.478),
        (0.851, 0.412, 0.372),
        (0.929, 0.559, 0.267),
        (0.976, 0.717, 0.163),
        (0.949, 0.885, 0.146),
    )
    _MAGMA = (
        (0.001, 0.000, 0.014),
        (0.163, 0.072, 0.310),
        (0.427, 0.121, 0.507),
        (0.694, 0.166, 0.472),
        (0.906, 0.320, 0.383),
        (0.988, 0.583, 0.408),
        (0.996, 0.827, 0.601),
        (0.987, 0.991, 0.750),
    )
    _COOLWARM = (
        (0.230, 0.299, 0.754),
        (0.552, 0.690, 0.996),
        (0.866, 0.866, 0.866),
        (0.968, 0.657, 0.537),
        (0.706, 0.016, 0.150),
    )

    class _Colormap(object):
        def __init__(self, name, anchors):
            self.name = name
            self.N = 256
            self._a = anchors

        def __call__(self, v, alpha=1.0):
            if hasattr(v, "__iter__") and not isinstance(v, (str, bytes)):
                return [self(x, alpha) for x in v]
            try:
                f = float(v)
            except Exception:
                f = 0.0
            if isinstance(v, int) and not isinstance(v, bool) and f > 1.0:
                f = f / float(self.N - 1)
            if f < 0.0:
                f = 0.0
            if f > 1.0:
                f = 1.0
            a = self._a
            pos = f * (len(a) - 1)
            lo = int(pos)
            hi = lo + 1 if lo + 1 < len(a) else lo
            t = pos - lo
            rgb = tuple(a[lo][k] + (a[hi][k] - a[lo][k]) * t for k in range(3))
            return (rgb[0], rgb[1], rgb[2], float(alpha))

        def _hex(self, v):
            r, g, b, _a = self(v)
            return "#" + "".join("%02x" % int(_bi_round(c * 255.0)) for c in (r, g, b))

        def reversed(self):
            return _Colormap(self.name + "_r", tuple(reversed(self._a)))

    def _bi_round(v):
        return int(v + 0.5) if v >= 0 else -int(-v + 0.5)

    _CMAPS = {
        "viridis": _VIRIDIS,
        "plasma": _PLASMA,
        "magma": _MAGMA,
        "inferno": _MAGMA,
        "coolwarm": _COOLWARM,
        "gray": ((0.0, 0.0, 0.0), (1.0, 1.0, 1.0)),
        "grey": ((0.0, 0.0, 0.0), (1.0, 1.0, 1.0)),
        "binary": ((1.0, 1.0, 1.0), (0.0, 0.0, 0.0)),
        "hot": ((0.0, 0.0, 0.0), (0.9, 0.0, 0.0), (1.0, 0.8, 0.0), (1.0, 1.0, 1.0)),
        "cool": ((0.0, 1.0, 1.0), (1.0, 0.0, 1.0)),
        "jet": (
            (0.0, 0.0, 0.5),
            (0.0, 0.5, 1.0),
            (0.5, 1.0, 0.5),
            (1.0, 0.5, 0.0),
            (0.5, 0.0, 0.0),
        ),
    }

    def get_cmap(name=None, lut=None):
        if name is None:
            name = "viridis"
        if isinstance(name, _Colormap):
            return name
        key = str(name)
        rev = key.endswith("_r")
        base = key[:-2] if rev else key
        anchors = _CMAPS.get(base)
        if anchors is None:
            anchors = _VIRIDIS
            base = "viridis"
        cm = _Colormap(base, anchors)
        return cm.reversed() if rev else cm

    pyplot = types.ModuleType("matplotlib.pyplot")
    pyplot.__doc__ = "vis imaging-backed matplotlib.pyplot subset."
    for _fn in (
        figure,
        plot,
        scatter,
        bar,
        barh,
        hist,
        fill_between,
        step,
        axhline,
        axvline,
        hlines,
        vlines,
        pie,
        errorbar,
        text,
        annotate,
        title,
        suptitle,
        xlabel,
        ylabel,
        grid,
        legend,
        axis,
        xlim,
        ylim,
        xscale,
        yscale,
        semilogx,
        semilogy,
        loglog,
        xticks,
        yticks,
        tight_layout,
        subplots_adjust,
        boxplot,
        imshow,
        colorbar,
        clf,
        cla,
        close,
        show,
        savefig,
        stackplot,
        get_cmap,
        subplots,
        subplot,
        axes,
        gca,
        gcf,
        use,
        switch_backend,
        get_backend,
        rc,
        rcdefaults,
        ion,
        ioff,
        isinteractive,
        draw,
        draw_if_interactive,
        pause,
        set_cmap,
        margins,
        minorticks_on,
        minorticks_off,
        clim,
        figtext,
    ):
        setattr(pyplot, _fn.__name__, _fn)
    pyplot.Axes = _Axes
    pyplot.Axes3D = _Axes3D
    pyplot.Colormap = _Colormap
    pyplot.rcParams = _rcparams

    style = types.ModuleType("matplotlib.style")
    style.use = lambda *a, **k: None
    style.context = lambda *a, **k: _NullCtx()
    style.available = [
        "default",
        "classic",
        "ggplot",
        "bmh",
        "fivethirtyeight",
        "seaborn-v0_8",
        "seaborn-v0_8-whitegrid",
        "dark_background",
    ]
    pyplot.style = style

    cm = types.ModuleType("matplotlib.cm")
    cm.get_cmap = get_cmap
    for _cname in _CMAPS:
        setattr(cm, _cname, get_cmap(_cname))
    pyplot.cm = cm
    pyplot.colormaps = get_cmap

    mpl = types.ModuleType("matplotlib")
    mpl.__doc__ = "vis matplotlib-compat shim (no CPython matplotlib wheel)."
    mpl.__version__ = "3.0-vis-imaging"
    mpl.pyplot = pyplot
    mpl.style = style
    mpl.cm = cm
    mpl.colormaps = cm
    mpl.use = use
    mpl.get_backend = get_backend
    mpl.rc = rc
    mpl.rcdefaults = rcdefaults
    mpl.rcParams = _rcparams

    # mpl_toolkits.mplot3d -- the import path every 3-D example uses. Registered
    # by this shim (and triggered by importing `mpl_toolkits` itself), so both
    # `from mpl_toolkits.mplot3d import Axes3D` and a bare
    # `fig.add_subplot(projection="3d")` work.
    art3d = types.ModuleType("mpl_toolkits.mplot3d.art3d")
    art3d.Poly3DCollection = _Poly3DCollection
    art3d.Line3DCollection = _Poly3DCollection
    art3d.Path3DCollection = _Poly3DCollection

    axes3d = types.ModuleType("mpl_toolkits.mplot3d.axes3d")
    axes3d.Axes3D = _Axes3D

    proj3d = types.ModuleType("mpl_toolkits.mplot3d.proj3d")

    mplot3d = types.ModuleType("mpl_toolkits.mplot3d")
    mplot3d.Axes3D = _Axes3D
    mplot3d.axes3d = axes3d
    mplot3d.art3d = art3d
    mplot3d.proj3d = proj3d

    mpl_toolkits = types.ModuleType("mpl_toolkits")
    mpl_toolkits.mplot3d = mplot3d

    sys.modules["matplotlib"] = mpl
    sys.modules["matplotlib.pyplot"] = pyplot
    sys.modules["matplotlib.style"] = style
    sys.modules["matplotlib.cm"] = cm
    sys.modules["mpl_toolkits"] = mpl_toolkits
    sys.modules["mpl_toolkits.mplot3d"] = mplot3d
    sys.modules["mpl_toolkits.mplot3d.axes3d"] = axes3d
    sys.modules["mpl_toolkits.mplot3d.art3d"] = art3d
    sys.modules["mpl_toolkits.mplot3d.proj3d"] = proj3d

    # Autoload: staple the module names onto builtins so `matplotlib.pyplot`,
    # a bare `pyplot`, and the conventional `plt` alias all work WITHOUT any
    # explicit import.
    try:
        import builtins as _b

        _b.matplotlib = mpl
        _b.pyplot = pyplot
        _b.plt = pyplot
        _b.mpl_toolkits = mpl_toolkits
    except Exception:
        pass


__vis_install_matplotlib__()
del __vis_install_matplotlib__
