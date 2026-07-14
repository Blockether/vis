import React from "react";
import { StyleSheet, Text, TextStyle } from "react-native";

import { c, mono } from "./theme";

/* ── syntax highlighting ──────────────────────────────────────────────
   Dependency-free highlighter for the phone transcript. The web channel
   ships a real grammar engine; here a SINGLE ordered tokenizer paints
   strings / comments / numbers / keywords across the handful of languages
   the agent's tool code touches (python, clojure, bash, ts/js). Pure and
   fast — one regex sweep, no native module — so it runs under Expo Go. */

/* keyword pool, deliberately cross-language: a word only lights up as a
   keyword, everything else is plain identifier ink. */
const KEYWORDS = new Set([
  // python
  "def",
  "class",
  "return",
  "if",
  "elif",
  "else",
  "for",
  "while",
  "in",
  "not",
  "and",
  "or",
  "import",
  "from",
  "as",
  "with",
  "try",
  "except",
  "finally",
  "raise",
  "lambda",
  "yield",
  "await",
  "async",
  "pass",
  "break",
  "continue",
  "global",
  "nonlocal",
  "del",
  "assert",
  "is",
  "None",
  "True",
  "False",
  "self",
  // js / ts
  "const",
  "let",
  "var",
  "function",
  "new",
  "typeof",
  "instanceof",
  "export",
  "default",
  "extends",
  "implements",
  "interface",
  "type",
  "enum",
  "public",
  "private",
  "protected",
  "static",
  "void",
  "null",
  "undefined",
  "true",
  "false",
  "this",
  "super",
  "case",
  "switch",
  "throw",
  "catch",
  "do",
  "of",
  // clojure
  "defn",
  "fn",
  "loop",
  "recur",
  "cond",
  "when",
  "ns",
  "require",
  "defmacro",
  "defmulti",
  "defmethod",
  "when-let",
  "if-let",
  // bash
  "echo",
  "then",
  "fi",
  "done",
  "esac",
  "local",
]);

type Tok = { t: string; k: string };

/* comment | string | number | word | anything-else, in that priority. */
const TOKEN = new RegExp(
  [
    "(//[^\\n]*|#[^\\n]*|;;[^\\n]*|/\\*[\\s\\S]*?\\*/)", // 1 comment
    "(\"(?:\\\\.|[^\"\\\\])*\"|'(?:\\\\.|[^'\\\\])*'|`(?:\\\\.|[^`\\\\])*`)", // 2 string
    "(\\b\\d[\\w.]*\\b)", // 3 number
    "([A-Za-z_$][\\w$?!*/>+-]*)", // 4 word
    "([\\s\\S])", // 5 other
  ].join("|"),
  "g",
);

export const tokenize = (code: string): Tok[] => {
  const out: Tok[] = [];
  let m: RegExpExecArray | null;
  TOKEN.lastIndex = 0;
  while ((m = TOKEN.exec(code)) !== null) {
    if (m[1] != null) out.push({ t: m[1], k: "comment" });
    else if (m[2] != null) out.push({ t: m[2], k: "string" });
    else if (m[3] != null) out.push({ t: m[3], k: "number" });
    else if (m[4] != null)
      out.push({ t: m[4], k: KEYWORDS.has(m[4]) ? "keyword" : "word" });
    else out.push({ t: m[5] ?? "", k: "punct" });
    if (m.index === TOKEN.lastIndex) TOKEN.lastIndex += 1; // guard empty match
  }
  return out;
};

/* Coalesce runs of same-kind tokens so the tree stays shallow (a long line
   of default-ink words becomes ONE <Text>, not dozens). */
export const Highlight = ({
  code,
  style,
}: {
  code: string;
  style?: TextStyle | TextStyle[];
}) => {
  const toks = tokenize(code);
  const runs: Tok[] = [];
  for (const tok of toks) {
    const prev = runs[runs.length - 1];
    if (prev && prev.k === tok.k) prev.t += tok.t;
    else runs.push({ t: tok.t, k: tok.k });
  }
  return (
    <Text style={[styles.base, style]}>
      {runs.map((tok, i) => {
        const hue = HUES[tok.k];
        return hue ? (
          <Text key={i} style={hue}>
            {tok.t}
          </Text>
        ) : (
          tok.t
        );
      })}
    </Text>
  );
};

const styles = StyleSheet.create({
  base: { fontFamily: mono, color: c.codeInk },
  comment: { color: c.dim, fontStyle: "italic" },
  string: { color: "#0B7A3B" },
  number: { color: "#B4530A" },
  keyword: { color: "#7C3AED", fontWeight: "600" },
});

const HUES: Record<string, TextStyle | undefined> = {
  comment: styles.comment,
  string: styles.string,
  number: styles.number,
  keyword: styles.keyword,
};
