# Issue #256: runs unchanged in a JVM worker and the native binary's python_execution.
# Sandbox blocks allow top-level await and inject the declared tools plus doc().
# ruff: noqa: F704, F821
import dataclasses

for as_list in (False, True):
    r = await sequence_probe_search(as_list=as_list)
    assert [page.title for page in r] == ["First", "Second"]
    assert len(r) == 2 and bool(r) and r.total == 20
    assert r[0] is r.results[0] and r[-1] is r.results[-1]
    assert r[:] == r.results and r[::-1] == r.results[::-1]
    assert r[0]["title"] == "First" and dataclasses.is_dataclass(r[0])
    assert r["results"] is r.results and r["total"] == 20
    assert not hasattr(r, "remote_only") and not hasattr(r, "_private")
    assert not hasattr(r, "__dict__")
    # Issue #259: an unknown attribute names the record's fields instead of a bare AttributeError.
    try:
        _ = r.methods
    except AttributeError as error:
        assert "PageList has no field 'methods'" in str(error), str(error)
        assert "results, total" in str(error), str(error)
    else:
        raise AssertionError("Unknown attribute allowed")
    try:
        r[0] = None
    except TypeError:
        pass
    else:
        raise AssertionError("Item assignment allowed")
    try:
        r.results = []
    except dataclasses.FrozenInstanceError:
        pass
    else:
        raise AssertionError("Field assignment allowed")

empty = await sequence_probe_search(True)
assert not empty and len(empty) == 0 and list(empty) == []
report = await sequence_probe_nested()
assert report.pages[0].title == "First" and len(report.pages) == 2
for plain in (report.plain, report.undeclared):
    assert plain.results == [] and plain["results"] == [] and bool(plain)
    try:
        iter(plain)
    except TypeError as error:
        assert "not iterable" in str(error)
    else:
        raise AssertionError("Implicit collection behavior allowed")

for kind in ("none", "mapping", "text", "set", "lazy", "subclass"):
    try:
        await sequence_probe_invalid(kind)
    except (TypeError, RuntimeError) as error:
        assert "must contain a built-in list or tuple" in str(error), str(error)
    else:
        raise AssertionError("Invalid sequence field accepted")

assert sequence_probe_search.contract["returns"]["sequence_field"] == "results"
assert "Sequence over results" in doc("sequence_probe_search")
print("Field-backed sequences verified")
