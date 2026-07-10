// vis TypeScript/Bun REPL eval server.
//
// Line-framed JSON protocol (mirror of the Python pack): one request object per
// stdin line — {"code": "..."} or {"op": "ping"} — one response object per
// stdout line: {ok, out, err, value, data, type, exc}. Globals persist across
// evals (real REPL state): top-level const/let/var/function/class declarations
// are rewritten to globalThis assignments, `import x from "m"` to dynamic
// `await import`, and the whole snippet runs inside an AsyncFunction (so
// top-level await just works) wrapped in `with (globalThis) { ... }`.
//
// stdout/stderr are captured PERMANENTLY (not just during an eval): protocol
// frames go through the saved raw write, everything else — including async
// logging from background tasks (servers!) between evals — lands in a buffer
// flushed with the NEXT response's "out"/"err". So a running system's logs are
// readable, and they can never corrupt the protocol.

const transpiler = new Bun.Transpiler({
  loader: "tsx",
  target: "bun",
  // a REPL must keep side-effect-free expressions — `1+1` IS the point
  deadCodeElimination: false,
});
const AsyncFunction = (async () => {}).constructor;

// ── output capture ───────────────────────────────────────────────────────────
const rawWrite = process.stdout.write.bind(process.stdout);
let outBuf = [];
let errBuf = [];
const CAP = 200_000;
function push(buf, chunk) {
  if (buf.length < 4096) buf.push(String(chunk).slice(0, CAP));
  return true;
}
process.stdout.write = (chunk, ...rest) => {
  const cb = rest[rest.length - 1];
  if (typeof cb === "function") cb();
  return push(outBuf, chunk);
};
process.stderr.write = (chunk, ...rest) => {
  const cb = rest[rest.length - 1];
  if (typeof cb === "function") cb();
  return push(errBuf, chunk);
};
// Bun's console bypasses process.stdout.write — patch the METHODS too (the
// console object itself stays intact: its stdin async-iterator drives the
// protocol loop below).
const fmtArgs = (args) =>
  args.map((x) => (typeof x === "string" ? x : repr(x))).join(" ") + "\n";
for (const m of ["log", "info", "debug"]) console[m] = (...a) => void push(outBuf, fmtArgs(a));
for (const m of ["warn", "error", "trace"]) console[m] = (...a) => void push(errBuf, fmtArgs(a));
function drain(buf) {
  const s = buf.join("").slice(0, CAP);
  buf.length = 0;
  return s;
}

// ── value → JSON-safe structured data (mirror of the Python pack's _safe) ────
function repr(v) {
  try {
    return Bun.inspect(v, { depth: 4 }).slice(0, 8000);
  } catch (e) {
    return "<uninspectable: " + String(e).slice(0, 200) + ">";
  }
}
function typeName(v) {
  if (v === null) return "null";
  const t = typeof v;
  if (t !== "object") return t;
  return (v.constructor && v.constructor.name) || "Object";
}
function opaque(v, depth) {
  // OPAQUE value — can't be turned into data (socket, stream, class with only
  // methods). It is NOT lost: it stays LIVE in the REPL's globals — bind it to
  // a name and keep calling it in later evals. Here we just describe it.
  const info = { __type__: typeName(v), __repr__: repr(v), __opaque__: true };
  if (depth === 0) {
    const attrs = new Set();
    let p = v;
    while (p && p !== Object.prototype && attrs.size < 50) {
      for (const k of Object.getOwnPropertyNames(p)) {
        if (!k.startsWith("_") && k !== "constructor") attrs.add(k);
      }
      p = Object.getPrototypeOf(p);
    }
    if (attrs.size) info.__attrs__ = [...attrs].slice(0, 50);
  }
  return info;
}
function safe(v, depth = 0) {
  if (depth > 6) return repr(v);
  if (v === null || v === undefined) return null;
  const t = typeof v;
  if (t === "boolean" || t === "string") return v;
  if (t === "number") return Number.isFinite(v) ? v : String(v);
  if (t === "bigint") return String(v) + "n";
  if (t === "function" || t === "symbol") return opaque(v, depth);
  if (Array.isArray(v)) return v.slice(0, 1000).map((x) => safe(x, depth + 1));
  if (v instanceof Date) return { __type__: "Date", iso: v.toISOString() };
  if (v instanceof RegExp) return { __type__: "RegExp", source: String(v) };
  if (v instanceof Error) {
    return {
      __type__: typeName(v),
      message: String(v.message),
      stack: String(v.stack || "").slice(0, 4000),
    };
  }
  if (v instanceof Map) {
    return {
      __type__: "Map",
      entries: [...v.entries()]
        .slice(0, 1000)
        .map(([k, x]) => [safe(k, depth + 1), safe(x, depth + 1)]),
    };
  }
  if (v instanceof Set) {
    return { __type__: "Set", values: [...v].slice(0, 1000).map((x) => safe(x, depth + 1)) };
  }
  if (ArrayBuffer.isView(v) || v instanceof ArrayBuffer) return opaque(v, depth);
  if (v instanceof Promise) return opaque(v, depth);
  // plain object or class instance with data fields
  const keys = Object.keys(v);
  const proto = Object.getPrototypeOf(v);
  const plain = proto === Object.prototype || proto === null;
  if (plain || keys.length) {
    const out = plain ? {} : { __type__: typeName(v) };
    for (const k of keys.slice(0, 1000)) {
      try {
        out[k] = safe(v[k], depth + 1);
      } catch (e) {
        out[k] = "<threw: " + String(e).slice(0, 120) + ">";
      }
    }
    return out;
  }
  return opaque(v, depth);
}

// ── top-level statement scanner ──────────────────────────────────────────────
// Split transpiled JS into DEPTH-0 statements. Tracks strings, template
// literals (incl. ${} nesting), comments, and bracket depth. Boundaries: a
// depth-0 `;`, or a depth-0 `}` not followed by else/catch/finally/while.
function splitTop(js) {
  const stmts = [];
  const n = js.length;
  let start = 0;
  let depth = 0;
  let i = 0;
  let mode = null; // '"' | "'" | '`' | 'line' | 'block'
  const tmpl = []; // depths at which ${ opened
  const boundary = (end) => {
    const s = js.slice(start, end);
    if (s.trim()) stmts.push(s);
    start = end;
  };
  while (i < n) {
    const c = js[i];
    const d = js[i + 1];
    if (mode === "line") {
      if (c === "\n") mode = null;
      i++;
      continue;
    }
    if (mode === "block") {
      if (c === "*" && d === "/") {
        mode = null;
        i += 2;
      } else i++;
      continue;
    }
    if (mode === '"' || mode === "'") {
      if (c === "\\") i += 2;
      else {
        if (c === mode || c === "\n") mode = null;
        i++;
      }
      continue;
    }
    if (mode === "`") {
      if (c === "\\") i += 2;
      else if (c === "`") {
        mode = null;
        i++;
      } else if (c === "$" && d === "{") {
        tmpl.push(depth);
        depth++;
        mode = null;
        i += 2;
      } else i++;
      continue;
    }
    // normal mode
    if (c === "/" && d === "/") {
      mode = "line";
      i += 2;
      continue;
    }
    if (c === "/" && d === "*") {
      mode = "block";
      i += 2;
      continue;
    }
    if (c === '"' || c === "'" || c === "`") {
      mode = c;
      i++;
      continue;
    }
    if (c === "(" || c === "[" || c === "{") {
      depth++;
      i++;
      continue;
    }
    if (c === ")" || c === "]") {
      depth--;
      i++;
      continue;
    }
    if (c === "}") {
      depth--;
      if (tmpl.length && depth === tmpl[tmpl.length - 1]) {
        tmpl.pop();
        mode = "`";
        i++;
        continue;
      }
      i++;
      if (depth === 0) {
        // A depth-0 `}` ends a STATEMENT (block/function/class) only when what
        // follows starts a new statement — transpiled output terminates
        // expression statements with `;`, so a `}` continued by an operator
        // (`= ) ] , . ` etc.) is mid-expression (destructuring pattern, object
        // literal) and must NOT split.
        const rest = js.slice(i).match(/^\s*(\S?)/);
        const nxt = rest ? rest[1] : "";
        const kw = js.slice(i).match(/^\s*([A-Za-z]+)/);
        const stmtStart = nxt === "" || /[A-Za-z_$0-9{]/.test(nxt);
        if (stmtStart && !(kw && ["else", "catch", "finally", "while", "from", "instanceof"].includes(kw[1])))
          boundary(i);
      }
      continue;
    }
    if (c === ";" && depth === 0) {
      i++;
      boundary(i);
      continue;
    }
    i++;
  }
  boundary(n);
  return stmts;
}

function hasTopEq(s) {
  // a bare `=` (not == != <= >= =>) at depth 0 — i.e. the declarator has an
  // initializer. Cheap scan; strings in a declarator name position don't occur.
  let depth = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if ("([{".includes(c)) depth++;
    else if (")]}".includes(c)) depth--;
    else if (c === "=" && depth === 0) {
      if (s[i + 1] === "=" || s[i + 1] === ">" || "=!<>".includes(s[i - 1])) continue;
      return true;
    }
  }
  return false;
}

// ── statement rewriting (REPL persistence semantics) ─────────────────────────
function rewriteImport(body) {
  let m = body.match(/^import\s*(?:["'](.+?)["'])\s*;?\s*$/s);
  if (m) return "await import(" + JSON.stringify(m[1]) + ");";
  m = body.match(/^import\s+(.+?)\s+from\s*["'](.+?)["']\s*;?\s*$/s);
  if (!m) return body; // unknown shape — leave as-is (will error visibly)
  const mod = JSON.stringify(m[2]);
  let clause = m[1].trim();
  const out = [];
  const dm = clause.match(/^([A-Za-z_$][\w$]*)\s*(,\s*)?/);
  if (dm && !clause.startsWith("{") && !clause.startsWith("*")) {
    out.push(
      dm[1] +
        " = ((__m) => (__m && __m.default !== undefined) ? __m.default : __m)(await import(" +
        mod +
        "))"
    );
    clause = clause.slice(dm[0].length).trim();
  }
  if (clause.startsWith("*")) {
    const nm = clause.match(/^\*\s*as\s+([A-Za-z_$][\w$]*)/);
    if (nm) out.push(nm[1] + " = await import(" + mod + ")");
  } else if (clause.startsWith("{")) {
    const inner = clause.slice(1, clause.lastIndexOf("}"));
    const binds = inner
      .split(",")
      .map((x) => x.trim())
      .filter(Boolean)
      .map((x) => {
        const am = x.match(/^([\w$]+)\s+as\s+([\w$]+)$/);
        return am ? am[1] + ": " + am[2] : x;
      });
    if (binds.length) out.push("({ " + binds.join(", ") + " } = await import(" + mod + "))");
  }
  return out.length ? out.join(";\n") + ";" : body;
}

function rewriteStmt(s) {
  const ws = s.match(/^\s*/)[0];
  let body = s.slice(ws.length);
  let m;
  if (body.startsWith("export default ")) body = "__vis_export_default = " + body.slice(15);
  else if (body.startsWith("export ")) body = body.slice(7);
  if (/^import[\s"']/.test(body)) return ws + rewriteImport(body);
  if ((m = body.match(/^(const|let|var)\s+/))) {
    let rest = body.slice(m[0].length);
    if (!hasTopEq(rest)) {
      // `let x;` / `let a, b` — assign undefined so the name exists on global
      return (
        ws +
        rest
          .replace(/;\s*$/, "")
          .split(",")
          .map((x) => x.trim() + " = undefined")
          .filter((x) => x !== " = undefined")
          .join(", ") +
        ";"
      );
    }
    const c = rest.trimStart()[0];
    if (c === "{" || c === "[") return ws + "(" + rest.replace(/;\s*$/, "") + ");";
    return ws + rest;
  }
  if ((m = body.match(/^(async\s+)?function\s*\*?\s*([A-Za-z_$][\w$]*)/))) {
    return ws + m[2] + " = " + body;
  }
  if ((m = body.match(/^class\s+([A-Za-z_$][\w$]*)/))) {
    return ws + m[1] + " = " + body;
  }
  return s;
}

// ── eval pipeline ────────────────────────────────────────────────────────────
async function run(code) {
  let ok = true;
  let exc = null;
  let value;
  let hasValue = false;
  try {
    const js = transpiler.transformSync(String(code));
    const stmts = splitTop(js).map(rewriteStmt);
    if (stmts.length) {
      // last-expression capture (REPL semantics): if the tail statement parses
      // as an expression, its value is the eval's value.
      const last = stmts[stmts.length - 1].trim().replace(/;\s*$/, "");
      let isExpr = false;
      try {
        new AsyncFunction("return (" + last + "\n)");
        isExpr = true;
      } catch (_e) {
        isExpr = false;
      }
      if (isExpr) {
        stmts[stmts.length - 1] = "globalThis.__VIS_VALUE__ = (" + last + "\n)";
        hasValue = true;
      }
    }
    const bodySrc = "with (globalThis) {\n" + stmts.join("\n") + "\n}";
    await new AsyncFunction(bodySrc)();
    if (hasValue) {
      value = globalThis.__VIS_VALUE__;
      delete globalThis.__VIS_VALUE__;
      if (value !== undefined) globalThis._ = value;
    }
  } catch (e) {
    ok = false;
    exc = e && e.stack ? String(e.stack) : String(e);
  }
  const hasV = hasValue && value !== undefined && ok;
  let data = null;
  if (hasV) {
    try {
      data = safe(value);
    } catch (_e) {
      data = null;
    }
  }
  return {
    ok,
    out: drain(outBuf),
    err: drain(errBuf),
    value: hasV ? repr(value) : null,
    data,
    type: hasV ? typeName(value) : null,
    exc,
  };
}

// ── module reload helpers ──────────────────────────────────────────────────────────────
// Bun honors `delete require.cache[abs]` for ESM too — dropping a TRANSITIVE
// dep from the cache makes the next (cache-busted) import re-evaluate it. So:
//   invalidate("src")      — drop every cached project module matching substr
//   await reload("./x.ts") — invalidate + cache-busted re-import of ONE module
let __vis_reload_n = 0;
globalThis.invalidate = (substr) => {
  const keys = Object.keys(require.cache).filter((k) =>
    substr ? k.includes(String(substr)) : !k.includes("node_modules")
  );
  for (const k of keys) delete require.cache[k];
  return keys.length;
};
globalThis.reload = async (p) => {
  const abs = Bun.resolveSync(String(p), process.cwd());
  delete require.cache[abs];
  return await import(abs + "?vis_reload=" + ++__vis_reload_n);
};

// ── protocol loop ────────────────────────────────────────────────────────────
function reply(obj) {
  let line;
  try {
    line = JSON.stringify(obj);
  } catch (e) {
    line = JSON.stringify({ ok: false, exc: "unserializable response: " + String(e) });
  }
  rawWrite(line + "\n");
}
for await (const line of console) {
  const t = String(line).trim();
  if (!t) continue;
  let req;
  try {
    req = JSON.parse(t);
  } catch (_e) {
    reply({ ok: false, exc: "bad request" });
    continue;
  }
  if (req && req.op === "ping") {
    reply({ ok: true, pong: true });
    continue;
  }
  reply(await run((req && req.code) || ""));
}
