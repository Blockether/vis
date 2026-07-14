import React from "react";
import { ScrollView, StyleSheet, Text, View } from "react-native";

import { c, mono } from "./theme";
import { Highlight } from "./Highlight";

/* Markdown-lite for transcript answers: fenced code blocks in an ink
   terminal box, headings, bullets, inline `code` / **bold**. Deliberately
   tiny — the web channel has the full renderer; this keeps the phone
   transcript readable without shipping a markdown engine.

   `dense` shrinks every font one notch: the assistant's own answer prose
   renders at the readable default, but tool-card bodies (a shell command's
   stdout/stderr, a REPL result, a python_execution RESULT) pass `dense` so
   the machine output stays compact and doesn't crowd the phone. */

type Block =
  | { kind: "code"; lang: string; body: string }
  | { kind: "text"; body: string };

export const splitBlocks = (src: string): Block[] => {
  const blocks: Block[] = [];
  const fence = /```([^\n`]*)\n([\s\S]*?)```/g;
  let last = 0;
  let match: RegExpExecArray | null;
  while ((match = fence.exec(src)) !== null) {
    if (match.index > last)
      blocks.push({ kind: "text", body: src.slice(last, match.index) });
    blocks.push({
      kind: "code",
      lang: (match[1] ?? "").trim(),
      body: (match[2] ?? "").replace(/\n$/, ""),
    });
    last = match.index + match[0].length;
  }
  if (last < src.length) {
    /* A still-streaming answer often has an OPEN fence whose closing ``` has
       not arrived yet — render it as a code block now instead of leaking raw
       backticks into the prose until the turn settles. */
    const tail = src.slice(last);
    const open = /```([^\n`]*)\n?([\s\S]*)$/.exec(tail);
    if (open) {
      if (open.index > 0)
        blocks.push({ kind: "text", body: tail.slice(0, open.index) });
      blocks.push({
        kind: "code",
        lang: (open[1] ?? "").trim(),
        body: (open[2] ?? "").replace(/\n$/, ""),
      });
    } else {
      blocks.push({ kind: "text", body: tail });
    }
  }
  return blocks;
};

/* inline `code` and **bold** inside one text run */
export const renderInline = (
  line: string,
  keyBase: string,
  st: MdStyles = styles,
): React.ReactNode[] => {
  const out: React.ReactNode[] = [];
  const re = /(`[^`]+`|\*\*[^*]+\*\*)/g;
  let last = 0;
  let i = 0;
  let match: RegExpExecArray | null;
  while ((match = re.exec(line)) !== null) {
    if (match.index > last) out.push(line.slice(last, match.index));
    const tok = match[0];
    if (tok.startsWith("`")) {
      out.push(
        <Text key={`${keyBase}-c${i}`} style={st.inlineCode}>
          {tok.slice(1, -1)}
        </Text>,
      );
    } else {
      out.push(
        <Text key={`${keyBase}-b${i}`} style={st.bold}>
          {tok.slice(2, -2)}
        </Text>,
      );
    }
    last = match.index + tok.length;
    i += 1;
  }
  if (last < line.length) {
    /* A trailing, not-yet-closed inline `code span (streaming) — style the tail
       as code rather than surfacing a lone literal backtick. */
    const rest = line.slice(last);
    const tick = rest.indexOf("`");
    if (tick >= 0 && rest.indexOf("`", tick + 1) === -1) {
      if (tick > 0) out.push(rest.slice(0, tick));
      out.push(
        <Text key={`${keyBase}-c${i}`} style={st.inlineCode}>
          {rest.slice(tick + 1)}
        </Text>,
      );
    } else {
      out.push(rest);
    }
  }
  return out;
};

const TextBlock = ({
  body,
  keyBase,
  st,
}: {
  body: string;
  keyBase: string;
  st: MdStyles;
}) => {
  const lines = body.replace(/^\n+|\n+$/g, "").split("\n");
  if (lines.length === 1 && lines[0] === "") return null;
  return (
    <View>
      {lines.map((line, i) => {
        const key = `${keyBase}-l${i}`;
        const heading = /^(#{1,4})\s+(.*)$/.exec(line);
        if (heading) {
          return (
            <Text key={key} style={st.heading}>
              {renderInline(heading[2] ?? "", key, st)}
            </Text>
          );
        }
        const bullet = /^\s*[-*]\s+(.*)$/.exec(line);
        if (bullet) {
          return (
            <View key={key} style={st.bulletRow}>
              <Text style={st.bulletDot}>{"\u2022"}</Text>
              <Text style={st.line}>
                {renderInline(bullet[1] ?? "", key, st)}
              </Text>
            </View>
          );
        }
        return (
          <Text key={key} style={st.line}>
            {renderInline(line, key, st)}
          </Text>
        );
      })}
    </View>
  );
};

export const Markdown = ({
  text,
  dense = false,
}: {
  text: string;
  dense?: boolean;
}) => {
  const st = dense ? denseStyles : styles;
  return (
    <View>
      {splitBlocks(text).map((block, i) =>
        block.kind === "code" ? (
          <View key={`b${i}`} style={st.codeBox}>
            {block.lang ? (
              <Text style={st.codeLang}>{block.lang}</Text>
            ) : null}
            <ScrollView horizontal showsHorizontalScrollIndicator={false}>
              <Highlight code={block.body} style={st.code} />
            </ScrollView>
          </View>
        ) : (
          <TextBlock key={`b${i}`} body={block.body} keyBase={`b${i}`} st={st} />
        ),
      )}
    </View>
  );
};

/* One notch smaller across the board when `dense`. Colours/weights/layout are
   shared; only the type sizes shrink so machine output stays compact. */
const mkStyles = (dense: boolean) =>
  StyleSheet.create({
    line: {
      color: c.ink,
      fontSize: dense ? 11 : 13,
      lineHeight: dense ? 16 : 19,
      flexShrink: 1,
    },
    heading: {
      color: c.ink,
      fontSize: dense ? 12 : 14,
      lineHeight: dense ? 17 : 20,
      fontWeight: "800",
      marginTop: 4,
    },
    bold: { fontWeight: "700", color: c.ink },
    inlineCode: {
      fontFamily: mono,
      fontSize: dense ? 10.5 : 12,
      color: c.chipInk,
      backgroundColor: c.codeBg,
    },
    bulletRow: { flexDirection: "row", gap: 6, paddingLeft: 2 },
    bulletDot: {
      color: c.amber,
      fontSize: dense ? 11 : 13,
      lineHeight: dense ? 16 : 19,
    },
    codeBox: {
      backgroundColor: c.codeBg,
      borderLeftWidth: 2,
      borderLeftColor: c.border,
      marginVertical: dense ? 4 : 6,
      paddingHorizontal: 8,
      paddingVertical: dense ? 4 : 6,
    },
    codeLang: {
      fontFamily: mono,
      fontSize: dense ? 8 : 9,
      color: c.dim,
      textTransform: "uppercase",
      letterSpacing: 1,
      marginBottom: 4,
    },
    code: {
      fontFamily: mono,
      fontSize: dense ? 10 : 11.5,
      lineHeight: dense ? 14 : 16,
      color: c.codeInk,
    },
  });

type MdStyles = ReturnType<typeof mkStyles>;
const styles = mkStyles(false);
const denseStyles = mkStyles(true);
