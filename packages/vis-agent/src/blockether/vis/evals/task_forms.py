"""Inspect the straight-line forms and observations of Task trials."""

import ast
import json


class _LiveBranches(ast.NodeTransformer):
    def visit_If(self, node):
        try:
            condition = ast.literal_eval(node.test)
        except (ValueError, TypeError):
            return self.generic_visit(node)
        return self.visit(
            ast.Module(body=node.body if condition else node.orelse, type_ignores=[])
        ).body

    def visit_While(self, node):
        if isinstance(node.test, ast.Constant) and not node.test.value:
            return self.visit(ast.Module(body=node.orelse, type_ignores=[])).body
        return self.generic_visit(node)


def live_form(code):
    """Remove literal dead branches before looking for Task idioms."""
    return _LiveBranches().visit(ast.parse(code))


def observations(stdout):
    """Read structured observations in their output order."""
    for line in stdout.splitlines():
        try:
            value = json.loads(line)
        except ValueError:
            continue
        if isinstance(value, dict):
            yield value


def resolve(tree, value, before):
    """Follow earlier simple assignments to the expression they name."""
    while isinstance(value, ast.Name):
        bindings = [
            node
            for node in ast.walk(tree)
            if isinstance(node, ast.Assign)
            and node.lineno < before
            and any(isinstance(t, ast.Name) and t.id == value.id for t in node.targets)
        ]
        if not bindings:
            break
        binding = max(bindings, key=lambda node: node.lineno)
        value = binding.value
        before = binding.lineno
    return value


def argument(call, index, name):
    """Read a positional argument or the equivalent keyword argument."""
    return (
        call.args[index]
        if len(call.args) > index
        else next((arg.value for arg in call.keywords if arg.arg == name), None)
    )


def prints_marker(node, marker):
    """Check for an explicit print of the trial marker."""
    return (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id == "print"
        and len(node.args) == 1
        and isinstance(node.args[0], ast.Constant)
        and node.args[0].value == marker
    )


class TaskFlow:
    """Follow Task values and their bound methods through successive Python blocks."""

    def __init__(self):
        self.names = {}
        self.folded = False
        self.released = self.recovered_await = False
        self.record_read = self.cancelled = self.caught_cancel = False

    def feed(self, code):
        """Read one successful block, in source order."""
        self._body(live_form(code).body)

    def _bind(self, target, value):
        if isinstance(target, ast.Name):
            self.names[target.id] = value
        elif isinstance(target, (ast.List, ast.Tuple)):
            for index, item in enumerate(target.elts):
                self._bind(
                    item,
                    value[index]
                    if isinstance(value, tuple) and index < len(value)
                    else value,
                )
        elif isinstance(target, ast.Starred):
            self._bind(target.value, value)

    def _symbol(self, node):
        if isinstance(node, ast.Name):
            if node.id in self.names:
                return self.names[node.id]
            if node.id in {
                "asyncio",
                "council",
                "fold_session",
                "BaseException",
                "next",
                "iter",
                "list",
                "tuple",
                "set",
                "sorted",
                "getattr",
            }:
                return node.id
            return None
        if isinstance(node, ast.Attribute):
            base = self._symbol(node.value)
            return f"{base}.{node.attr}" if isinstance(base, str) else None
        return None

    def _handler(self, node):
        kind = node.type
        options = kind.elts if isinstance(kind, ast.Tuple) else [kind]
        return kind is None or any(
            self._symbol(option)
            in {
                "asyncio.CancelledError",
                "asyncio.exceptions.CancelledError",
                "BaseException",
            }
            for option in options
        )

    def _body(self, statements, catches=False):
        for node in statements:
            if isinstance(node, ast.Import):
                for alias in node.names:
                    self.names[alias.asname or alias.name.split(".")[0]] = alias.name
            elif isinstance(node, ast.ImportFrom):
                for alias in node.names:
                    self.names[alias.asname or alias.name] = (
                        f"{node.module}.{alias.name}"
                    )
            elif isinstance(node, (ast.Assign, ast.AnnAssign)):
                value = self._value(node.value, catches)
                targets = (
                    node.targets if isinstance(node, ast.Assign) else [node.target]
                )
                for target in targets:
                    self._bind(target, value)
            elif isinstance(node, (ast.Try, ast.TryStar)):
                self._body(
                    node.body, catches or any(self._handler(h) for h in node.handlers)
                )
                self._body(node.orelse, catches)
                self._body(node.finalbody, catches)
            elif isinstance(node, ast.If):
                self._value(node.test, catches)
                self._body(node.body, catches)
                self._body(node.orelse, catches)
            elif isinstance(node, (ast.With, ast.AsyncWith)):
                self._body(node.body, catches)
            elif isinstance(node, ast.Expr):
                self._value(node.value, catches)
            elif isinstance(node, (ast.For, ast.AsyncFor)):
                self._bind(node.target, self._value(node.iter, catches))
                self._body(node.body, catches)

    @staticmethod
    def _recovered(value):
        return value == "recovered" or (
            isinstance(value, tuple)
            and any(TaskFlow._recovered(item) for item in value)
        )

    def _value(self, node, catches):
        if isinstance(node, ast.Await):
            value = self._value(node.value, catches)
            if value == "recovered":
                self.recovered_await |= self.released
                return None
            if value == "child" and self.cancelled:
                self.caught_cancel |= catches
                return None
            return value
        if isinstance(node, (ast.List, ast.Tuple, ast.Set)):
            return tuple(self._value(item, catches) for item in node.elts)
        if isinstance(node, ast.Subscript):
            value = self._value(node.value, catches)
            key = self._value(node.slice, catches)
            if isinstance(value, tuple):
                index = (
                    node.slice.value if isinstance(node.slice, ast.Constant) else None
                )
                if type(index) is int and -len(value) <= index < len(value):
                    return value[index]
                return "recovered" if self._recovered((value, key)) else None
            return "recovered" if self._recovered(key) else value
        if isinstance(
            node, (ast.ListComp, ast.SetComp, ast.GeneratorExp, ast.DictComp)
        ):
            saved = self.names.copy()
            sources = []
            for part in node.generators:
                source = self._value(part.iter, catches)
                sources.append(source)
                self._bind(part.target, source)
                sources.extend(self._value(cond, catches) for cond in part.ifs)
            fields = (
                (node.key, node.value)
                if isinstance(node, ast.DictComp)
                else (node.elt,)
            )
            items = [self._value(field, catches) for field in fields]
            self.names = saved
            if self._recovered(tuple(sources + items)):
                return "recovered"
            return items[0] if len(items) == 1 else None
        if isinstance(node, ast.Lambda):
            saved = self.names.copy()
            for arg in ast.walk(node.args):
                if isinstance(arg, ast.arg):
                    self.names[arg.arg] = None
            value = self._value(node.body, catches)
            self.names = saved
            return "recovered" if self._recovered(value) else None
        if isinstance(node, ast.Call):
            kind = self._value(node.func, catches)
            args = [self._value(arg, catches) for arg in node.args]
            kwargs = {arg.arg: self._value(arg.value, catches) for arg in node.keywords}
            if kind == "fold_session":
                self.folded = True
            elif kind == "asyncio.all_tasks" and self.folded:
                return "recovered"
            elif kind == "council.run":
                return "child"
            elif kind == "child.record":
                self.record_read |= not self.cancelled
            elif kind == "child.cancel":
                self.cancelled = True
            elif kind == "recovered.type":
                self.released |= any(
                    arg in (("literal", "release"), ("literal", "release\n"))
                    for arg in (*args, *kwargs.values())
                )
            elif (
                kind == "getattr"
                and len(args) >= 2
                and isinstance(args[0], str)
                and isinstance(args[1], tuple)
                and args[1][:1] == ("literal",)
            ):
                return f"{args[0]}.{args[1][1]}"
            if self._recovered((kind, *args, *kwargs.values())):
                return "recovered"
            return None
        if isinstance(node, ast.Attribute):
            base = self._value(node.value, catches)
            if self._recovered(base):
                return "recovered.type" if node.attr == "type" else "recovered"
            return f"{base}.{node.attr}" if isinstance(base, str) else None
        if isinstance(node, ast.Constant):
            return ("literal", node.value) if isinstance(node.value, str) else None
        if isinstance(node, ast.Name):
            return self._symbol(node)
        if isinstance(node, ast.expr) and self._recovered(
            tuple(self._value(child, catches) for child in ast.iter_child_nodes(node))
        ):
            return "recovered"
        return None
