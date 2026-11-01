package com.blockether.vis.tui.render;

import java.util.Set;

/**
 * Colors Clojure source for a terminal with ANSI SGR foreground codes, in the palette of the
 * Python highlighter.
 *
 * <p>One pass lexes the source and classifies each symbol from the token before it: the head of a
 * list, the name after a defining form, a type hint or a class name. Nothing is parsed, so code
 * that is still streaming in colors the same way as finished code, and an unclosed string colors
 * to the end of the source.
 *
 * <p>The result is the source with SGR sequences inserted and nothing else changed: removing them
 * gives the source back. Every colored run is closed before a line break and reopened after it, so
 * each line can be painted on its own.
 */
public final class ClojureHighlighter {
    /** SGR foreground code for special forms and core macros at the head of a list. */
    public static final int KEYWORD = 36;
    /** SGR foreground code for strings, regular expressions and character literals. */
    public static final int STRING = 31;
    /** SGR foreground code for escape sequences inside strings. */
    public static final int ESCAPE = 35;
    /** SGR foreground code for numbers. */
    public static final int NUMBER = 34;
    /** SGR foreground code for keywords, {@code nil}, {@code true}, {@code false} and {@code ##Inf}. */
    public static final int CONSTANT = 35;
    /** SGR foreground code for comments and the {@code #_} discard marker. */
    public static final int COMMENT = 90;
    /** SGR foreground code for called and defined functions. */
    public static final int FUNCTION = 33;
    /** SGR foreground code for class names, type hints, tagged literals and namespace names. */
    public static final int TYPE = 32;

    private static final String RESET = "\u001b[0m";

    private static final System.Logger LOG = System.getLogger(ClojureHighlighter.class.getName());

    /** Heads that read as language syntax rather than as a call. Any {@code def...} head is one too. */
    private static final Set<String> SPECIAL = Set.of(
            "def", "if", "do", "let", "quote", "var", "fn", "loop", "recur", "throw", "try", "catch",
            "finally", "monitor-enter", "monitor-exit", "new", "set!", ".", "letfn", "let*", "fn*",
            "loop*", "case", "cond", "condp", "cond->", "cond->>", "some->", "some->>", "->", "->>",
            "as->", "when", "when-not", "when-let", "when-some", "when-first", "if-let", "if-not",
            "if-some", "and", "or", "not", "doto", "dotimes", "doseq", "for", "while", "binding",
            "with-open", "with-redefs", "locking", "delay", "future", "ns", "in-ns", "require",
            "import", "use", "refer", "reify", "proxy", "extend-type", "extend-protocol", "comment",
            "assert");

    /** Defining heads whose name is a type or a namespace, not a function. */
    private static final Set<String> TYPE_DEFINERS = Set.of(
            "ns", "in-ns", "defprotocol", "defrecord", "deftype", "definterface", "defstruct");

    private final String s;
    private final int n;
    private final StringBuilder out;
    private boolean colored;
    /** The previous token opened a list, so a symbol here is its head. */
    private boolean head;
    /** Color for the next symbol, the name after a defining head; 0 when none is pending. */
    private int pendingName;

    private ClojureHighlighter(String source) {
        this.s = source;
        this.n = source.length();
        this.out = new StringBuilder(n + n / 2 + 16);
    }

    /**
     * Returns {@code source} with ANSI color sequences for a terminal. Never throws: source that is
     * {@code null}, empty or already contains an escape character is returned as it is, and so is
     * source the highlighter fails on. The failure is logged.
     */
    public static String highlight(String source) {
        if (source == null || source.isEmpty() || source.indexOf('\u001b') >= 0) {
            return source;
        }
        try {
            return new ClojureHighlighter(source).run();
        } catch (RuntimeException e) {
            LOG.log(System.Logger.Level.WARNING, "Clojure highlighting failed; the source stays plain", e);
            return source;
        }
    }

    private char at(int i) {
        return i < n ? s.charAt(i) : '\0';
    }

    private static boolean isLineBreak(char c) {
        return c == '\n' || c == '\r';
    }

    private static boolean isBlank(char c) {
        return c == ',' || Character.isWhitespace(c);
    }

    /** Whether {@code c} ends a symbol, keyword or number. */
    private static boolean isTerminator(char c) {
        switch (c) {
            case '(': case ')': case '[': case ']': case '{': case '}':
            case '"': case ';': case '@': case '^': case '`': case '~': case '\\':
                return true;
            default:
                return isBlank(c);
        }
    }

    /** End of the symbol-like run that starts at {@code i}. */
    private int symbolEnd(int i) {
        while (i < n && !isTerminator(s.charAt(i))) {
            i++;
        }
        return i;
    }

    private String run() {
        int i = 0;
        while (i < n) {
            i = token(i);
        }
        return colored ? out.toString() : s;
    }

    /** Lexes and emits the token at {@code i}; returns the index after it. */
    private int token(int i) {
        final char c = s.charAt(i);
        if (isBlank(c)) {
            int j = i + 1;
            while (j < n && isBlank(s.charAt(j))) {
                j++;
            }
            out.append(s, i, j);
            return j;
        }
        switch (c) {
            case ';': {
                int j = i;
                while (j < n && !isLineBreak(s.charAt(j))) {
                    j++;
                }
                return plainState(emit(COMMENT, i, j));
            }
            case '"':
                return plainState(string(i));
            case '\\':
                return plainState(character(i));
            case ':':
                return plainState(emit(CONSTANT, i, symbolEnd(i + 1)));
            case '#':
                return dispatch(i);
            case '^':
                return metadata(i);
            case '(':
                out.append(c);
                head = true;
                return i + 1;
            case '\'': case '`': case '~': case '@':
                out.append(c);
                return i + 1;
            default:
                if (isTerminator(c)) {
                    out.append(c);
                    head = false;
                    pendingName = 0;
                    return i + 1;
                }
                return symbol(i);
        }
    }

    /** Clears the list-head and pending-name state after a token that is not a symbol. */
    private int plainState(int j) {
        head = false;
        pendingName = 0;
        return j;
    }

    private int string(int i) {
        int run = i;
        int j = i + 1;
        while (j < n) {
            final char c = s.charAt(j);
            if (c == '"') {
                return emit(STRING, run, j + 1);
            }
            if (c == '\\' && j + 1 < n) {
                emit(STRING, run, j);
                int k = j + 2;
                if (s.charAt(j + 1) == 'u') {
                    while (k < n && k < j + 6 && Character.digit(s.charAt(k), 16) >= 0) {
                        k++;
                    }
                }
                emit(ESCAPE, j, k);
                run = k;
                j = k;
            } else {
                j++;
            }
        }
        return emit(STRING, run, n);
    }

    /** A character literal: {@code \a}, {@code \newline}, {@code \u00e9} or one code point. */
    private int character(int i) {
        if (i + 1 >= n) {
            return emit(STRING, i, n);
        }
        int j = i + 1 + Character.charCount(s.codePointAt(i + 1));
        if (Character.isLetter(s.charAt(i + 1))) {
            while (j < n && Character.isLetterOrDigit(s.charAt(j))) {
                j++;
            }
        }
        return emit(STRING, i, j);
    }

    /** The reader dispatch forms that start with {@code #}. */
    private int dispatch(int i) {
        final char c = at(i + 1);
        switch (c) {
            case '"': {
                int j = i + 2;
                while (j < n && s.charAt(j) != '"') {
                    j += s.charAt(j) == '\\' ? 2 : 1;
                }
                return plainState(emit(STRING, i, Math.min(n, j + 1)));
            }
            case '_':
                return plainState(emit(COMMENT, i, i + 2));
            case '#':
                return plainState(emit(CONSTANT, i, symbolEnd(i + 2)));
            case ':':
                return plainState(emit(CONSTANT, i, symbolEnd(i + 2)));
            case '(':
                out.append(s, i, i + 2);
                head = true;
                return i + 2;
            default:
                if (Character.isLetter(c)) {
                    return plainState(emit(TYPE, i, symbolEnd(i + 1)));
                }
                out.append('#');
                return i + 1;
        }
    }

    /** A {@code ^Type} hint colors as a type; {@code ^:kw} and {@code ^{...}} color their parts. */
    private int metadata(int i) {
        final char c = at(i + 1);
        if (i + 1 < n && !isTerminator(c) && c != ':') {
            return emit(TYPE, i, symbolEnd(i + 1));
        }
        if (c == ':') {
            return emit(CONSTANT, i, symbolEnd(i + 2));
        }
        out.append('^');
        return i + 1;
    }

    private int symbol(int i) {
        final int j = symbolEnd(i);
        final char c = s.charAt(i);
        final boolean signed = (c == '+' || c == '-') && j > i + 1 && Character.isDigit(s.charAt(i + 1));
        if (Character.isDigit(c) || signed) {
            return plainState(emit(NUMBER, i, j));
        }
        final String w = s.substring(i, j);
        final int color;
        if (w.equals("nil") || w.equals("true") || w.equals("false")) {
            color = CONSTANT;
        } else if (pendingName != 0) {
            color = pendingName;
        } else if (head) {
            color = headColor(w);
        } else {
            color = isClassName(w) ? TYPE : 0;
        }
        final boolean definer = head && (w.startsWith("def") || w.equals("ns") || w.equals("in-ns"));
        emit(color, i, j);
        head = false;
        pendingName = definer ? (TYPE_DEFINERS.contains(w) ? TYPE : FUNCTION) : 0;
        return j;
    }

    private static int headColor(String w) {
        if (SPECIAL.contains(w) || w.startsWith("def")) {
            return KEYWORD;
        }
        if (w.indexOf('/') < 0 && isClassName(w)) {
            return TYPE;
        }
        return FUNCTION;
    }

    /** {@code String}, {@code Math/abs}, {@code java.util.Map} and {@code StringBuilder.}. */
    private static boolean isClassName(String w) {
        final int slash = w.indexOf('/');
        final String path = slash > 0 ? w.substring(0, slash) : w;
        final String trimmed = path.endsWith(".") ? path.substring(0, path.length() - 1) : path;
        final int dot = trimmed.lastIndexOf('.');
        final String last = trimmed.substring(dot + 1);
        return !last.isEmpty() && Character.isUpperCase(last.charAt(0));
    }

    /** Appends {@code s[a, b)} in {@code color}, closed before each line break; returns {@code b}. */
    private int emit(int color, int a, int b) {
        if (color == 0) {
            out.append(s, a, b);
            return b;
        }
        int i = a;
        while (i < b) {
            int j = i;
            while (j < b && !isLineBreak(s.charAt(j))) {
                j++;
            }
            if (j > i) {
                out.append("\u001b[").append(color).append('m').append(s, i, j).append(RESET);
                colored = true;
            }
            while (j < b && isLineBreak(s.charAt(j))) {
                out.append(s.charAt(j));
                j++;
            }
            i = j;
        }
        return b;
    }
}
