package com.blockether.vis.tui.render;

/**
 * Text passes of the Vis terminal renderer that run for each character of a painted line.
 * Each method returns its argument itself when it has nothing to change, so a clean line
 * costs one scan and no allocation.
 */
public final class TerminalText {
    private TerminalText() {}

    /**
     * Answers true for a character that raw Lanterna painting must never receive as is.
     * C0 controls other than tab, line feed and carriage return become spaces. The inline
     * style sentinels U+E110 to U+E119 are removed.
     */
    private static boolean needsSanitize(char c) {
        return c < 0x09 || c == 0x0B || c == 0x0C || (c >= 0x0E && c <= 0x1F)
                || (c >= 0xE110 && c <= 0xE119);
    }

    /**
     * Prepares a string for raw Lanterna painting: each C0 control other than tab, line
     * feed and carriage return becomes a space, and each inline style sentinel
     * (U+E110 to U+E119) is removed.
     */
    public static String sanitizeForLanterna(String s) {
        final int n = s.length();
        int i = 0;
        while (i < n && !needsSanitize(s.charAt(i))) {
            i++;
        }
        if (i == n) {
            return s;
        }
        final StringBuilder out = new StringBuilder(n);
        out.append(s, 0, i);
        for (; i < n; i++) {
            final char c = s.charAt(i);
            if (c >= 0xE110 && c <= 0xE119) {
                continue;
            }
            out.append(needsSanitize(c) ? ' ' : c);
        }
        return out.toString();
    }
}
