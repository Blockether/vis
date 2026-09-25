package com.blockether.vis.python;

import java.util.Arrays;

/**
 * Colors Python source for a terminal with ANSI SGR foreground codes.
 *
 * <p>One pass lexes the source, including the replacement fields of f-strings and t-strings, and
 * a second pass classifies each name from its neighbours: keywords, definitions, calls,
 * attributes, decorators, annotations and naming conventions. Nothing is parsed, so code that is
 * still streaming in colors the same way as finished code, and broken code colors as far as it
 * can be read.
 *
 * <p>The result is the source with SGR sequences inserted and nothing else changed: removing
 * them gives the source back. Every colored run is closed before a line break and reopened after
 * it, so each line can be painted on its own.
 */
public final class PythonHighlighter {
    /** SGR foreground code for keywords. */
    public static final int KEYWORD = 36;
    /** SGR foreground code for string literals, including format specifications. */
    public static final int STRING = 31;
    /** SGR foreground code for escape sequences and doubled braces inside strings. */
    public static final int ESCAPE = 35;
    /** SGR foreground code for numbers. */
    public static final int NUMBER = 34;
    /** SGR foreground code for {@code True}, {@code False}, {@code None} and ALL_CAPS names. */
    public static final int CONSTANT = 35;
    /** SGR foreground code for comments. */
    public static final int COMMENT = 90;
    /** SGR foreground code for defined, called and decorating functions. */
    public static final int FUNCTION = 33;
    /** SGR foreground code for classes, CapWords names and annotations. */
    public static final int TYPE = 32;
    /** SGR foreground code for attributes that are not called. */
    public static final int PROPERTY = 35;

    private static final String RESET = "\u001b[0m";

    private static final int K_NAME = 1;
    private static final int K_NUMBER = 2;
    private static final int K_STRING = 3;
    private static final int K_ESCAPE = 4;
    private static final int K_COMMENT = 5;
    private static final int K_OP = 6;
    private static final int K_NEWLINE = 7;
    private static final int K_NL = 8;
    private static final int K_FOPEN = 9;
    private static final int K_FCLOSE = 10;
    private static final int K_OTHER = 11;

    private final String s;
    private final int n;
    private int[] start = new int[64];
    private int[] end = new int[64];
    private int[] kind = new int[64];
    private int count;

    private PythonHighlighter(String source) {
        this.s = source;
        this.n = source.length();
    }

    /**
     * Returns {@code source} with ANSI color sequences for a terminal. Never throws: source that
     * is {@code null}, empty or already contains an escape character is returned as it is, and so
     * is source the highlighter fails on.
     */
    public static String highlight(String source) {
        if (source == null || source.isEmpty() || source.indexOf('\u001b') >= 0) {
            return source;
        }
        try {
            return new PythonHighlighter(source).run();
        } catch (RuntimeException | StackOverflowError e) {
            return source;
        }
    }

    private String run() {
        int stop = code(0, false, false);
        if (stop != n) {
            return s;
        }
        return render(classify());
    }

    // ------------------------------------------------------------------------------ lexing

    private void add(int k, int a, int b) {
        if (count == kind.length) {
            int size = count * 2;
            start = Arrays.copyOf(start, size);
            end = Arrays.copyOf(end, size);
            kind = Arrays.copyOf(kind, size);
        }
        start[count] = a;
        end[count] = b;
        kind[count] = k;
        count++;
    }

    private char at(int i) {
        return i < n ? s.charAt(i) : '\0';
    }

    private static boolean isLineBreak(char c) {
        return c == '\n' || c == '\r';
    }

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private static boolean isIdStart(int cp) {
        return cp == '_' || (cp < 128 ? (cp | 0x20) >= 'a' && (cp | 0x20) <= 'z' : Character.isUnicodeIdentifierStart(cp));
    }

    private static boolean isIdPart(int cp) {
        return cp < 128
                ? cp == '_' || (cp >= '0' && cp <= '9') || ((cp | 0x20) >= 'a' && (cp | 0x20) <= 'z')
                : Character.isUnicodeIdentifierPart(cp);
    }

    /**
     * Lexes code from {@code i}. At top level it runs to the end of the source; inside a
     * replacement field it stops at the field's closing brace, conversion or format specification
     * at bracket depth zero, or at a line break when the enclosing string is single-quoted.
     * Returns where it stopped.
     */
    private int code(int i, boolean field, boolean singleLine) {
        int depth = 0;
        while (i < n) {
            char c = s.charAt(i);
            if (c == ' ' || c == '\t' || c == '\f') {
                i++;
            } else if (isLineBreak(c)) {
                if (field && singleLine) {
                    return i;
                }
                int j = c == '\r' && at(i + 1) == '\n' ? i + 2 : i + 1;
                add(!field && depth == 0 ? K_NEWLINE : K_NL, i, j);
                i = j;
            } else if (c == '#') {
                int j = i + 1;
                while (j < n && !isLineBreak(s.charAt(j))) {
                    j++;
                }
                add(K_COMMENT, i, j);
                i = j;
            } else if (c == '\\') {
                int j = i + 1;
                if (at(j) == '\r') {
                    j++;
                }
                if (at(j) == '\n') {
                    j++;
                }
                i = field && singleLine ? i + 1 : Math.max(j, i + 1);
            } else if (isDigit(c) || (c == '.' && isDigit(at(i + 1)))) {
                int j = number(i);
                add(K_NUMBER, i, j);
                i = j;
            } else if (c == '"' || c == '\'') {
                i = string(i, i);
            } else {
                int cp = s.codePointAt(i);
                if (isIdStart(cp)) {
                    int j = i + Character.charCount(cp);
                    while (j < n) {
                        int d = s.codePointAt(j);
                        if (!isIdPart(d)) {
                            break;
                        }
                        j += Character.charCount(d);
                    }
                    char q = at(j);
                    if ((q == '"' || q == '\'') && isStringPrefix(i, j)) {
                        i = string(i, j);
                    } else {
                        add(K_NAME, i, j);
                        i = j;
                    }
                    continue;
                }
                if (field && depth == 0 && (c == '}' || c == ':' || (c == '!' && at(i + 1) != '='))) {
                    return i;
                }
                if (c == '(' || c == '[' || c == '{') {
                    depth++;
                    add(K_OP, i, i + 1);
                    i++;
                } else if (c == ')' || c == ']' || c == '}') {
                    if (depth > 0) {
                        depth--;
                    }
                    add(K_OP, i, i + 1);
                    i++;
                } else {
                    int len = operatorLength(i);
                    if (len > 0) {
                        add(K_OP, i, i + len);
                        i += len;
                    } else {
                        int w = Character.charCount(cp);
                        add(K_OTHER, i, i + w);
                        i += w;
                    }
                }
            }
        }
        return i;
    }

    private int operatorLength(int i) {
        char c = s.charAt(i);
        char d = at(i + 1);
        char e = at(i + 2);
        switch (c) {
            case '*':
            case '/':
            case '<':
            case '>':
                if (d == c) {
                    return e == '=' ? 3 : 2;
                }
                return d == '=' ? 2 : 1;
            case '.':
                return d == '.' && e == '.' ? 3 : 1;
            case '-':
                return d == '>' || d == '=' ? 2 : 1;
            case '=':
            case '!':
            case '+':
            case '%':
            case '&':
            case '|':
            case '^':
            case '@':
            case ':':
                return d == '=' ? 2 : 1;
            case ',':
            case ';':
            case '~':
                return 1;
            default:
                return 0;
        }
    }

    private int number(int i) {
        int j = i;
        if (s.charAt(j) == '0' && "xXoObB".indexOf(at(j + 1)) >= 0) {
            j += 2;
            while (j < n && (Character.digit(s.charAt(j), 16) >= 0 || s.charAt(j) == '_')) {
                j++;
            }
            return j;
        }
        j = digits(j);
        if (at(j) == '.') {
            j = digits(j + 1);
        }
        char c = at(j);
        if (c == 'e' || c == 'E') {
            int k = j + 1;
            if (at(k) == '+' || at(k) == '-') {
                k++;
            }
            if (isDigit(at(k))) {
                j = digits(k);
            }
        }
        if (at(j) == 'j' || at(j) == 'J') {
            j++;
        }
        return j;
    }

    private int digits(int j) {
        while (j < n && (isDigit(s.charAt(j)) || s.charAt(j) == '_')) {
            j++;
        }
        return j;
    }

    /** Whether {@code [a, b)} is a string prefix Python accepts: r, u, b, f, t and their pairs. */
    private boolean isStringPrefix(int a, int b) {
        if (b - a == 1) {
            return "rRuUbBfFtT".indexOf(s.charAt(a)) >= 0;
        }
        if (b - a != 2) {
            return false;
        }
        char x = Character.toLowerCase(s.charAt(a));
        char y = Character.toLowerCase(s.charAt(a + 1));
        return (x == 'r' && (y == 'b' || y == 'f' || y == 't')) || (y == 'r' && (x == 'b' || x == 'f' || x == 't'));
    }

    /**
     * Lexes the string literal whose prefix is {@code [p, q)} and whose opening quote is at
     * {@code q}. An unterminated single-quoted string ends at its line break; an unterminated
     * triple-quoted one at the end of the source.
     */
    private int string(int p, int q) {
        boolean raw = false;
        boolean fmt = false;
        boolean bytes = false;
        for (int k = p; k < q; k++) {
            char c = Character.toLowerCase(s.charAt(k));
            raw |= c == 'r';
            fmt |= c == 'f' || c == 't';
            bytes |= c == 'b';
        }
        char quote = s.charAt(q);
        boolean triple = at(q + 1) == quote && at(q + 2) == quote;
        int i = q + (triple ? 3 : 1);
        int run = p;
        while (i < n) {
            char c = s.charAt(i);
            if (c == quote && (!triple || (at(i + 1) == quote && at(i + 2) == quote))) {
                int e = i + (triple ? 3 : 1);
                add(K_STRING, run, e);
                return e;
            }
            if (c == '\\') {
                int e = raw ? -1 : escapeEnd(i, bytes);
                if (e > 0) {
                    if (i > run) {
                        add(K_STRING, run, i);
                    }
                    add(K_ESCAPE, i, e);
                    run = e;
                    i = e;
                } else if (fmt && (at(i + 1) == '{' || at(i + 1) == '}')) {
                    i++;
                } else if (at(i + 1) == '\r' && at(i + 2) == '\n') {
                    i += 3;
                } else {
                    i = Math.min(n, i + 2);
                }
            } else if (isLineBreak(c) && !triple) {
                break;
            } else if (fmt && (c == '{' || c == '}')) {
                if (at(i + 1) == c) {
                    if (i > run) {
                        add(K_STRING, run, i);
                    }
                    add(K_ESCAPE, i, i + 2);
                    run = i + 2;
                    i += 2;
                } else if (c == '{') {
                    if (i > run) {
                        add(K_STRING, run, i);
                    }
                    i = field(i, triple, quote);
                    run = i;
                } else {
                    i++;
                }
            } else {
                i++;
            }
        }
        if (i > run) {
            add(K_STRING, run, i);
        }
        return i;
    }

    /** End of the escape sequence at backslash {@code i}, or -1 when Python reads none there. */
    private int escapeEnd(int i, boolean bytes) {
        char d = at(i + 1);
        switch (d) {
            case '\n':
            case '\\':
            case '\'':
            case '"':
            case 'a':
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
            case 'v':
                return i + 2;
            case '\r':
                return at(i + 2) == '\n' ? i + 3 : i + 2;
            case 'x':
                return hex(i + 2, 2);
            case 'u':
                return bytes ? -1 : hex(i + 2, 4);
            case 'U':
                return bytes ? -1 : hex(i + 2, 8);
            case 'N':
                if (bytes || at(i + 2) != '{') {
                    return -1;
                }
                for (int j = i + 3; j < n && !isLineBreak(s.charAt(j)); j++) {
                    if (s.charAt(j) == '}') {
                        return j > i + 3 ? j + 1 : -1;
                    }
                }
                return -1;
            default:
                if (d >= '0' && d <= '7') {
                    int j = i + 2;
                    while (j < i + 4 && at(j) >= '0' && at(j) <= '7') {
                        j++;
                    }
                    return j;
                }
                return -1;
        }
    }

    private int hex(int j, int digits) {
        for (int k = 0; k < digits; k++) {
            if (Character.digit(at(j + k), 16) < 0) {
                return -1;
            }
        }
        return j + digits;
    }

    /** Lexes the replacement field opening at {@code i}; returns the index after it. */
    private int field(int i, boolean triple, char quote) {
        add(K_FOPEN, i, i + 1);
        int j = code(i + 1, true, !triple);
        if (at(j) == '!') {
            int k = j + 1;
            while (k < n && isIdPart(s.charAt(k))) {
                k++;
            }
            add(K_OP, j, k);
            j = k;
        }
        if (at(j) == ':') {
            j = spec(j, triple, quote);
        }
        if (at(j) == '}') {
            add(K_FCLOSE, j, j + 1);
            return j + 1;
        }
        return j;
    }

    /** Lexes a format specification from its colon up to the field's closing brace. */
    private int spec(int j, boolean triple, char quote) {
        int run = j;
        int i = j + 1;
        while (i < n) {
            char c = s.charAt(i);
            if (c == '}' || (!triple && (isLineBreak(c) || c == quote))
                    || (triple && c == quote && at(i + 1) == quote && at(i + 2) == quote)) {
                break;
            }
            if (c == '{') {
                if (i > run) {
                    add(K_STRING, run, i);
                }
                i = field(i, triple, quote);
                run = i;
            } else {
                i++;
            }
        }
        if (i > run) {
            add(K_STRING, run, i);
        }
        return i;
    }

    // ------------------------------------------------------------------------ classifying

    private boolean isOp(int t, char c) {
        return t >= 0 && kind[t] == K_OP && end[t] - start[t] == 1 && s.charAt(start[t]) == c;
    }

    private boolean isOp(int t, String op) {
        return t >= 0 && kind[t] == K_OP && end[t] - start[t] == op.length() && s.startsWith(op, start[t]);
    }

    private boolean isName(int t, String word) {
        return t >= 0 && kind[t] == K_NAME && end[t] - start[t] == word.length() && s.startsWith(word, start[t]);
    }

    private static boolean isKeyword(String w) {
        switch (w) {
            case "and":
            case "as":
            case "assert":
            case "async":
            case "await":
            case "break":
            case "class":
            case "continue":
            case "def":
            case "del":
            case "elif":
            case "else":
            case "except":
            case "finally":
            case "for":
            case "from":
            case "global":
            case "if":
            case "import":
            case "in":
            case "is":
            case "lambda":
            case "nonlocal":
            case "not":
            case "or":
            case "pass":
            case "raise":
            case "return":
            case "try":
            case "while":
            case "with":
            case "yield":
                return true;
            default:
                return false;
        }
    }

    private static boolean isConstant(String w) {
        return w.equals("True") || w.equals("False") || w.equals("None");
    }

    /** CONSTANT for ALL_CAPS names, TYPE for CapWords names, {@code otherwise} for the rest. */
    private static int convention(String w, int otherwise) {
        int i = 0;
        while (i < w.length() && w.charAt(i) == '_') {
            i++;
        }
        if (i == w.length() || !Character.isUpperCase(w.charAt(i))) {
            return otherwise;
        }
        for (int j = i + 1; j < w.length(); j++) {
            if (Character.isLowerCase(w.charAt(j))) {
                return TYPE;
            }
        }
        return CONSTANT;
    }

    /** Colors per token; 0 leaves a token in the terminal's default color. */
    private int[] classify() {
        int[] color = new int[count];
        int[] sig = new int[count];
        int m = 0;
        for (int t = 0; t < count; t++) {
            switch (kind[t]) {
                case K_STRING:
                    color[t] = STRING;
                    break;
                case K_ESCAPE:
                    color[t] = ESCAPE;
                    break;
                case K_NUMBER:
                    color[t] = NUMBER;
                    break;
                case K_COMMENT:
                    color[t] = COMMENT;
                    break;
                default:
                    break;
            }
            if (kind[t] != K_COMMENT && kind[t] != K_NL) {
                sig[m++] = t;
            }
        }
        // Statement context: imports, decorators and annotations.
        boolean[] plain = new boolean[count];
        boolean[] annotation = new boolean[count];
        int a = 0;
        while (a < m) {
            int b = statementEnd(sig, m, a);
            statement(sig, a, b, color, plain, annotation);
            a = b + 1;
        }
        for (int j = 0; j < m; j++) {
            int t = sig[j];
            if (kind[t] != K_NAME || color[t] != 0) {
                continue;
            }
            color[t] = nameColor(sig, m, j, plain[t], annotation[t]);
        }
        return color;
    }

    /** Index in {@code sig} of the NEWLINE or ';' that ends the statement starting at {@code a}, or m. */
    private int statementEnd(int[] sig, int m, int a) {
        int depth = 0;
        for (int j = a; j < m; j++) {
            int t = sig[j];
            int k = kind[t];
            if (k == K_NEWLINE || (depth == 0 && isOp(t, ';'))) {
                return j;
            }
            if (k == K_FOPEN || isOp(t, '(') || isOp(t, '[') || isOp(t, '{')) {
                depth++;
            } else if ((k == K_FCLOSE || isOp(t, ')') || isOp(t, ']') || isOp(t, '}')) && depth > 0) {
                depth--;
            }
        }
        return m;
    }

    private void statement(int[] sig, int a, int b, int[] color, boolean[] plain, boolean[] annotation) {
        if (a >= b) {
            return;
        }
        int first = sig[a];
        if (isName(first, "import") || isName(first, "from")) {
            for (int j = a; j < b; j++) {
                plain[sig[j]] = true;
            }
            return;
        }
        if (isOp(first, '@')) {
            color[first] = FUNCTION;
            int j = a + 1;
            while (j < b && kind[sig[j]] == K_NAME) {
                color[sig[j]] = FUNCTION;
                if (j + 2 < b && isOp(sig[j + 1], '.') && kind[sig[j + 2]] == K_NAME) {
                    color[sig[j + 1]] = FUNCTION;
                    j += 2;
                } else {
                    break;
                }
            }
            return;
        }
        int d = a;
        if (isName(first, "async")) {
            d++;
        }
        if (d < b && isName(sig[d], "def")) {
            definition(sig, d + 1, b, annotation);
            return;
        }
        // name(.name)*: annotation [= value]
        if (kind[first] == K_NAME && !isKeyword(text(first))) {
            int j = a + 1;
            while (j + 1 < b && isOp(sig[j], '.') && kind[sig[j + 1]] == K_NAME) {
                j += 2;
            }
            if (j < b && isOp(sig[j], ':')) {
                int depth = 0;
                for (int k = j + 1; k < b; k++) {
                    int t = sig[k];
                    if (depth == 0 && isOp(t, '=')) {
                        break;
                    }
                    depth = nest(t, depth);
                    annotation[t] = depth == 0 || !keywordArgument(sig, k, b);
                }
            }
        }
    }

    /** Marks parameter and return annotations of the def whose name is at {@code j}. */
    private void definition(int[] sig, int j, int b, boolean[] annotation) {
        j++;
        if (j < b && isOp(sig[j], '[')) {
            j = closing(sig, j, b) + 1;
        }
        if (j >= b || !isOp(sig[j], '(')) {
            return;
        }
        int close = closing(sig, j, b);
        int depth = 0;
        boolean paramStart = true;
        boolean inAnnotation = false;
        for (int k = j + 1; k < close; k++) {
            int t = sig[k];
            if (depth == 0) {
                if (isOp(t, ',')) {
                    paramStart = true;
                    inAnnotation = false;
                    continue;
                }
                if (isOp(t, '=')) {
                    inAnnotation = false;
                    paramStart = false;
                    continue;
                }
                if (paramStart && (isOp(t, '*') || isOp(t, "**"))) {
                    continue;
                }
                if (paramStart && kind[t] == K_NAME) {
                    paramStart = false;
                    if (k + 1 < close && isOp(sig[k + 1], ':')) {
                        inAnnotation = true;
                        k++;
                    }
                    continue;
                }
                paramStart = false;
            }
            if (inAnnotation) {
                annotation[t] = depth == 0 || !keywordArgument(sig, k, close);
            }
            depth = nest(t, depth);
        }
        int k = close + 1;
        if (k < b && isOp(sig[k], "->")) {
            int depth2 = 0;
            for (k++; k < b; k++) {
                int t = sig[k];
                if (depth2 == 0 && isOp(t, ':')) {
                    break;
                }
                depth2 = nest(t, depth2);
                annotation[t] = depth2 == 0 || !keywordArgument(sig, k, b);
            }
        }
    }

    /** Whether the name at {@code k} names a keyword argument, as {@code gt} in {@code Field(gt=0)}. */
    private boolean keywordArgument(int[] sig, int k, int b) {
        return kind[sig[k]] == K_NAME && k + 1 < b && isOp(sig[k + 1], '=');
    }

    private int nest(int t, int depth) {
        int k = kind[t];
        if (k == K_FOPEN || isOp(t, '(') || isOp(t, '[') || isOp(t, '{')) {
            return depth + 1;
        }
        if (k == K_FCLOSE || isOp(t, ')') || isOp(t, ']') || isOp(t, '}')) {
            return Math.max(0, depth - 1);
        }
        return depth;
    }

    /** Index in {@code sig} of the bracket closing the one at {@code j}, or {@code b} when unclosed. */
    private int closing(int[] sig, int j, int b) {
        int depth = 0;
        for (int k = j; k < b; k++) {
            int before = depth;
            depth = nest(sig[k], depth);
            if (depth == 0 && before > 0) {
                return k;
            }
        }
        return b;
    }

    private String text(int t) {
        return s.substring(start[t], end[t]);
    }

    private int nameColor(int[] sig, int m, int j, boolean plain, boolean annotation) {
        int t = sig[j];
        String w = text(t);
        int prev = j > 0 ? sig[j - 1] : -1;
        int next = j + 1 < m ? sig[j + 1] : -1;
        if (isConstant(w)) {
            return CONSTANT;
        }
        if (isKeyword(w)) {
            return KEYWORD;
        }
        boolean afterDot = isOp(prev, '.');
        if (!afterDot) {
            boolean statementStart = prev < 0 || kind[prev] == K_NEWLINE || isOp(prev, ';');
            if (statementStart && softKeyword(sig, m, j, w)) {
                return KEYWORD;
            }
            if (isName(prev, "def")) {
                return FUNCTION;
            }
            if (isName(prev, "class")) {
                return TYPE;
            }
        }
        if (annotation) {
            return TYPE;
        }
        boolean call = isOp(next, '(');
        if (plain) {
            return convention(w, 0);
        }
        if (afterDot) {
            return convention(w, call ? FUNCTION : PROPERTY);
        }
        return convention(w, call ? FUNCTION : 0);
    }

    /** Whether statement-initial {@code match}, {@code case} or {@code type} is used as a keyword. */
    private boolean softKeyword(int[] sig, int m, int j, String w) {
        int next = j + 1 < m ? sig[j + 1] : -1;
        if (next < 0 || kind[next] == K_NEWLINE) {
            return false;
        }
        switch (w) {
            case "match":
            case "case": {
                if (kind[next] == K_OP) {
                    char c = s.charAt(start[next]);
                    boolean opener = end[next] - start[next] == 1 && (c == '(' || c == '[' || c == '{');
                    boolean unary = end[next] - start[next] == 1 && (c == '-' || c == '*' || c == '~');
                    if (!opener && !unary) {
                        return false;
                    }
                }
                int depth = 0;
                int last = -1;
                for (int k = j + 1; k < m; k++) {
                    int t = sig[k];
                    if (kind[t] == K_NEWLINE || (depth == 0 && isOp(t, ';'))) {
                        break;
                    }
                    depth = nest(t, depth);
                    last = t;
                }
                return isOp(last, ':');
            }
            case "type": {
                int after = j + 2 < m ? sig[j + 2] : -1;
                return kind[next] == K_NAME && (isOp(after, '=') || isOp(after, '['));
            }
            default:
                return false;
        }
    }

    // --------------------------------------------------------------------------- rendering

    private String render(int[] color) {
        StringBuilder out = new StringBuilder(n + n / 2 + 16);
        int pos = 0;
        int open = 0;
        for (int t = 0; t < count; t++) {
            int c = color[t];
            if (c == 0) {
                continue;
            }
            int a = start[t];
            if (a > pos) {
                if (open != 0) {
                    out.append(RESET);
                    open = 0;
                }
                out.append(s, pos, a);
            }
            int b = end[t];
            int i = a;
            while (i < b) {
                int j = i;
                while (j < b && !isLineBreak(s.charAt(j))) {
                    j++;
                }
                if (j > i) {
                    if (open != c) {
                        out.append("\u001b[").append(c).append('m');
                        open = c;
                    }
                    out.append(s, i, j);
                }
                if (j < b) {
                    if (open != 0) {
                        out.append(RESET);
                        open = 0;
                    }
                    int k = j;
                    while (k < b && isLineBreak(s.charAt(k))) {
                        k++;
                    }
                    out.append(s, j, k);
                    j = k;
                }
                i = j;
            }
            pos = b;
        }
        if (open != 0) {
            out.append(RESET);
        }
        out.append(s, pos, n);
        return out.toString();
    }
}
