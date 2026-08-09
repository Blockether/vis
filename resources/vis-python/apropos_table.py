def __vis_apropos_table__(query=""):
    hidden = set(globals().get("__vis_advertised_native_tools__") or ())
    d = {k: v for k, v in apropos(query).items() if k not in hidden}
    if not d:
        return "apropos(" + repr(query) + "): no unadvertised capabilities match."

    groups = globals().get("__vis_groups__") or {}

    def __cell(s):
        return str(s).replace("\n", " ").replace("|", "\\|")

    by_group = {}
    for k in d:
        by_group.setdefault(str(groups.get(k, "engine")), []).append(k)

    out = []
    for g in sorted(by_group):
        out.append("### " + __cell(g))
        out.append("| capability | gist |")
        out.append("| --- | --- |")
        for k in by_group[g]:
            out.append("| `" + __cell(k) + "` | " + __cell(d[k]) + " |")
        out.append("")
    out.append(
        "Groups: " + ", ".join("`" + __cell(g) + "`" for g in sorted(by_group)) + "."
    )
    out.append(
        'One group at a time: `apropos("providers")`. One contract: `doc(name)`.'
    )
    return "\n".join(out)
