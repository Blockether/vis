import React from "react";
import { ScrollView, StyleSheet, Text, View } from "react-native";

import { c, mono } from "./theme";

/* Markdown-lite for transcript answers: fenced code blocks in an ink
   terminal box, headings, bullets, inline `code` / **bold**. Deliberately
   tiny — the web channel has the full renderer; this keeps the phone
   transcript readable without shipping a markdown engine. */

type Block =
  | { kind: "code"; lang: string; body: string }
  | { kind: "text"; body: string };

const splitBlocks = (src: string): Block[] => {
  const blocks: Block[] = [];
  const fence = /```([^\n`]*)\n([\s\S]*?)```/g;
  let last = 0;
  let match: RegExpExecArray | null;
  while ((match = fence.exec(src)) !== null) {
    if (match.index > last) blocks.push({ kind: "text", body: src.slice(last, match.index) });
    blocks.push({ kind: "code", lang: (match[1] ?? "").trim(), body: (match[2] ?? "").replace(/\n$/, "") });
    last = match.index + match[0].length;
  }
  if (last < src.length) blocks.push({ kind: "text", body: src.slice(last) });
  return blocks;
};

/* inline `code` and **bold** inside one text run */
const renderInline = (line: string, keyBase: string): React.ReactNode[] => {
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
        <Text key={`${keyBase}-c${i}`} style={styles.inlineCode}>
          {tok.slice(1, -1)}
        </Text>
      );
    } else {
      out.push(
        <Text key={`${keyBase}-b${i}`} style={styles.bold}>
          {tok.slice(2, -2)}
        </Text>
      );
    }
    last = match.index + tok.length;
    i += 1;
  }
  if (last < line.length) out.push(line.slice(last));
  return out;
};

const TextBlock = ({ body, keyBase }: { body: string; keyBase: string }) => {
  const lines = body.replace(/^\n+|\n+$/g, "").split("\n");
  if (lines.length === 1 && lines[0] === "") return null;
  return (
    <View>
      {lines.map((line, i) => {
        const key = `${keyBase}-l${i}`;
        const heading = /^(#{1,4})\s+(.*)$/.exec(line);
        if (heading) {
          return (
            <Text key={key} style={styles.heading}>
              {renderInline(heading[2] ?? "", key)}
            </Text>
          );
        }
        const bullet = /^\s*[-*]\s+(.*)$/.exec(line);
        if (bullet) {
          return (
            <View key={key} style={styles.bulletRow}>
              <Text style={styles.bulletDot}>{"\u2022"}</Text>
              <Text style={styles.line}>{renderInline(bullet[1] ?? "", key)}</Text>
            </View>
          );
        }
        return (
          <Text key={key} style={styles.line}>
            {renderInline(line, key)}
          </Text>
        );
      })}
    </View>
  );
};

export const Markdown = ({ text }: { text: string }) => (
  <View>
    {splitBlocks(text).map((block, i) =>
      block.kind === "code" ? (
        <View key={`b${i}`} style={styles.codeBox}>
          {block.lang ? <Text style={styles.codeLang}>{block.lang}</Text> : null}
          <ScrollView horizontal showsHorizontalScrollIndicator={false}>
            <Text style={styles.code}>{block.body}</Text>
          </ScrollView>
        </View>
      ) : (
        <TextBlock key={`b${i}`} body={block.body} keyBase={`b${i}`} />
      )
    )}
  </View>
);

const styles = StyleSheet.create({
  line: { color: c.ink, fontSize: 13, lineHeight: 19, flexShrink: 1 },
  heading: { color: c.ink, fontSize: 14, lineHeight: 20, fontWeight: "800", marginTop: 4 },
  bold: { fontWeight: "700", color: c.ink },
  inlineCode: {
    fontFamily: mono,
    fontSize: 12,
    color: c.chipInk,
    backgroundColor: c.codeBg
  },
  bulletRow: { flexDirection: "row", gap: 6, paddingLeft: 2 },
  bulletDot: { color: c.amber, fontSize: 13, lineHeight: 19 },
  codeBox: {
    backgroundColor: c.codeBg,
    borderLeftWidth: 2,
    borderLeftColor: c.border,
    marginVertical: 6,
    paddingHorizontal: 8,
    paddingVertical: 6
  },
  codeLang: {
    fontFamily: mono,
    fontSize: 9,
    color: c.dim,
    textTransform: "uppercase",
    letterSpacing: 1,
    marginBottom: 4
  },
  code: { fontFamily: mono, fontSize: 11.5, lineHeight: 16, color: c.codeInk }
});