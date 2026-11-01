package com.blockether.vis.python;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Formats Python source the way {@code ruff format} does with its default settings: 88 columns,
 * four-space indentation, double quotes and magic trailing commas. One self-contained file: a
 * lexer, a parser, ruff's comment placement, its document IR and printer, and its formatting
 * rules.
 *
 * <p>This is a Java port of the Ruff formatter (https://github.com/astral-sh/ruff, MIT License,
 * Copyright (c) 2022 Charles Marsh), whose document printer derives from the Rome formatter
 * (MIT License, Copyright (c) Rome Tools, Inc. and its affiliates). The package NOTICE carries
 * both license texts.
 */
public final class PythonFormatter {
    private PythonFormatter() {
    }

    /** Parses {@code source}; throws {@link ParseException} for code Python itself would reject. */
    static Module parse(String source) {
        ArrayList<Tok> toks = new Lexer(source).run();
        return new Parser(source, toks).module();
    }

    // =====================================================================================
    // Lexer
    // =====================================================================================

    static final int T_NAME = 1, T_NUMBER = 2, T_STRING = 3, T_FSTART = 4, T_FMIDDLE = 5, T_FEND = 6,
            T_OP = 7, T_NEWLINE = 8, T_NL = 9, T_COMMENT = 10, T_INDENT = 11, T_DEDENT = 12, T_END = 13;

    /** A token: kind, source range and, for names and operators, the interned text. */
    static final class Tok {
        final int kind;
        final int start;
        final int end;
        final String text;

        Tok(int kind, int start, int end, String text) {
            this.kind = kind;
            this.start = start;
            this.end = end;
            this.text = text;
        }

        boolean is(String op) {
            return (kind == T_OP || kind == T_NAME) && text.equals(op);
        }

        boolean trivia() {
            return kind == T_NL || kind == T_COMMENT || kind == T_NEWLINE || kind == T_INDENT || kind == T_DEDENT || kind == T_END;
        }

        @Override
        public String toString() {
            return kind + ":" + start + "-" + end + (text == null ? "" : ":" + text);
        }
    }

    /** Signals Python source that cannot be parsed; the formatter never guesses at broken code. */
    public static final class ParseException extends RuntimeException {
        private static final long serialVersionUID = 1L;
        public final int offset;

        ParseException(String message, int offset) {
            super(message + " at offset " + offset);
            this.offset = offset;
        }
    }

    private static final String[] OPS3 = {"**=", "//=", ">>=", "<<=", "..."};
    private static final String[] OPS2 = {"->", ":=", "**", "//", ">>", "<<", "<=", ">=", "==", "!=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "@="};

    /** One-character operators by character, so the lexer never allocates them. */
    private static final String[] OPS1 = new String[128];

    static {
        for (char c : "()[]{}+-*/%@&|^~<>,:;.=".toCharArray()) {
            OPS1[c] = String.valueOf(c);
        }
    }

    static boolean isStringPrefix(String p) {
        switch (p.toLowerCase(Locale.ROOT)) {
            case "r": case "u": case "b": case "br": case "rb": case "f": case "fr": case "rf": case "t": case "tr": case "rt":
                return true;
            default:
                return false;
        }
    }

    static final class Lexer {
        final String s;
        final int n;
        final ArrayList<Tok> toks = new ArrayList<>();
        int i;
        int depth;
        int[] indents = new int[64];
        int nIndents = 1;
        boolean lineStart = true;
        boolean lineHasTokens;
        // f-string modes
        FMode[] modes = new FMode[8];
        int nModes;

        static final class FMode {
            final char quote;
            final boolean triple;
            final boolean raw;
            final int baseDepth;
            int[] fieldDepth = new int[4];
            boolean[] inSpec = new boolean[4];
            int nFields;

            FMode(char quote, boolean triple, boolean raw, int baseDepth) {
                this.quote = quote;
                this.triple = triple;
                this.raw = raw;
                this.baseDepth = baseDepth;
            }

            boolean literal() {
                return nFields == 0 || inSpec[nFields - 1];
            }

            void push(int d) {
                if (nFields == fieldDepth.length) {
                    fieldDepth = Arrays.copyOf(fieldDepth, nFields * 2);
                    inSpec = Arrays.copyOf(inSpec, nFields * 2);
                }
                fieldDepth[nFields] = d;
                inSpec[nFields] = false;
                nFields++;
            }
        }

        Lexer(String s) {
            this.s = s;
            this.n = s.length();
        }

        ParseException err(String msg, int at) {
            return new ParseException(msg, at);
        }

        void add(int kind, int start, int end, String text) {
            toks.add(new Tok(kind, start, end, text));
            if (kind != T_NL && kind != T_COMMENT && kind != T_NEWLINE && kind != T_INDENT && kind != T_DEDENT) {
                lineHasTokens = true;
            }
        }

        FMode mode() {
            return nModes == 0 ? null : modes[nModes - 1];
        }

        ArrayList<Tok> run() {
            int nul = s.indexOf('\0');
            if (nul >= 0) {
                throw err("source code cannot contain null bytes", nul);
            }
            while (true) {
                FMode m = mode();
                if (m != null && m.literal()) {
                    lexFMiddle(m);
                    continue;
                }
                if (lineStart && depth == 0 && m == null) {
                    if (!indentation()) {
                        break;
                    }
                }
                // skip whitespace
                while (i < n) {
                    char c = s.charAt(i);
                    if (c == ' ' || c == '\t' || c == '\f') {
                        i++;
                    } else if (c == '\\' && (i + 1 < n && (s.charAt(i + 1) == '\n' || s.charAt(i + 1) == '\r'))) {
                        i += 2;
                        if (s.charAt(i - 1) == '\r' && i < n && s.charAt(i) == '\n') {
                            i++;
                        }
                    } else if (c == '\\' && i + 1 >= n) {
                        throw err("unexpected end of file after line continuation", i);
                    } else {
                        break;
                    }
                }
                if (i >= n) {
                    if (nModes > 0) {
                        throw err("unterminated f-string", i);
                    }
                    break;
                }
                char c = s.charAt(i);
                if (c == '#') {
                    int st = i;
                    while (i < n && s.charAt(i) != '\n' && s.charAt(i) != '\r') {
                        i++;
                    }
                    toks.add(new Tok(T_COMMENT, st, i, null));
                    continue;
                }
                if (c == '\n' || c == '\r') {
                    int st = i;
                    i += (c == '\r' && i + 1 < n && s.charAt(i + 1) == '\n') ? 2 : 1;
                    if (depth > 0 || !lineHasTokens) {
                        toks.add(new Tok(T_NL, st, i, null));
                    } else {
                        toks.add(new Tok(T_NEWLINE, st, i, null));
                    }
                    if (depth == 0) {
                        lineStart = true;
                        lineHasTokens = false;
                    }
                    continue;
                }
                if (m != null && depth == m.fieldDepth[m.nFields - 1]) {
                    // Inside a replacement field at its own bracket depth.
                    if (c == '}') {
                        add(T_OP, i, i + 1, "}");
                        i++;
                        depth--;
                        m.nFields--;
                        continue;
                    }
                    if (c == ':') {
                        add(T_OP, i, i + 1, ":");
                        i++;
                        m.inSpec[m.nFields - 1] = true;
                        continue;
                    }
                    if (c == '!' && !(i + 1 < n && s.charAt(i + 1) == '=')) {
                        add(T_OP, i, i + 1, "!");
                        i++;
                        continue;
                    }
                }
                if (isIdStart(c)) {
                    int st = i;
                    i = identEnd(i);
                    String word = s.substring(st, i);
                    if (i < n && (s.charAt(i) == '\'' || s.charAt(i) == '"') && word.length() <= 2 && isStringPrefix(word)) {
                        string(st, word);
                        continue;
                    }
                    add(T_NAME, st, i, word);
                    continue;
                }
                if (c >= '0' && c <= '9' || (c == '.' && i + 1 < n && Character.isDigit(s.charAt(i + 1)))) {
                    number();
                    continue;
                }
                if (c == '\'' || c == '"') {
                    string(i, "");
                    continue;
                }
                operator(c);
            }
            // end of input
            if (lineHasTokens) {
                toks.add(new Tok(T_NEWLINE, n, n, null));
            }
            while (nIndents > 1) {
                nIndents--;
                toks.add(new Tok(T_DEDENT, n, n, null));
            }
            toks.add(new Tok(T_END, n, n, null));
            return toks;
        }

        /** Handles the indentation at the start of a logical line; returns false at end of input. */
        boolean indentation() {
            int col = 0;
            int st = i;
            while (i < n) {
                char c = s.charAt(i);
                if (c == ' ') {
                    col++;
                } else if (c == '\t') {
                    col = (col / 8 + 1) * 8;
                } else if (c == '\f') {
                    col = 0;
                } else {
                    break;
                }
                i++;
            }
            if (i >= n) {
                return false;
            }
            char c = s.charAt(i);
            if (c == '#' || c == '\n' || c == '\r') {
                return true; // blank or comment-only line: no indentation change
            }
            if (c == '\\' && i + 1 < n && (s.charAt(i + 1) == '\n' || s.charAt(i + 1) == '\r')) {
                // A continuation at the start of a line: treat the next line as the same line.
                return true;
            }
            lineStart = false;
            int cur = indents[nIndents - 1];
            if (col > cur) {
                if (nIndents == indents.length) {
                    indents = Arrays.copyOf(indents, nIndents * 2);
                }
                indents[nIndents++] = col;
                toks.add(new Tok(T_INDENT, st, i, null));
            } else if (col < cur) {
                while (nIndents > 1 && indents[nIndents - 1] > col) {
                    nIndents--;
                    toks.add(new Tok(T_DEDENT, i, i, null));
                }
                if (indents[nIndents - 1] != col) {
                    throw err("unindent does not match any outer indentation level", i);
                }
            }
            return true;
        }

        static boolean isIdStart(char c) {
            return c == '_' || (c < 128 ? Character.isLetter(c) : Character.isUnicodeIdentifierStart(c) || Character.isHighSurrogate(c));
        }

        int identEnd(int j) {
            while (j < n) {
                char c = s.charAt(j);
                if (c < 128) {
                    if (Character.isLetterOrDigit(c) || c == '_') {
                        j++;
                        continue;
                    }
                    break;
                }
                int cp = s.codePointAt(j);
                if (Character.isUnicodeIdentifierPart(cp) && !Character.isIdentifierIgnorable(cp)) {
                    j += Character.charCount(cp);
                } else {
                    break;
                }
            }
            return j;
        }

        void number() {
            int st = i;
            char c = s.charAt(i);
            if (c == '0' && i + 1 < n && "xXoObB".indexOf(s.charAt(i + 1)) >= 0) {
                char k = Character.toLowerCase(s.charAt(i + 1));
                i += 2;
                int ds = i;
                while (i < n) {
                    char d = s.charAt(i);
                    boolean ok = d == '_' || (k == 'x' ? Character.digit(d, 16) >= 0 : k == 'o' ? d >= '0' && d <= '7' : d == '0' || d == '1');
                    if (!ok) {
                        break;
                    }
                    i++;
                }
                if (i == ds) {
                    throw err("invalid number literal", st);
                }
            } else {
                digits();
                if (i < n && s.charAt(i) == '.') {
                    i++;
                    digits();
                }
                if (i < n && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
                    int save = i;
                    i++;
                    if (i < n && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
                        i++;
                    }
                    if (i < n && Character.isDigit(s.charAt(i))) {
                        digits();
                    } else {
                        i = save;
                    }
                }
                if (i < n && (s.charAt(i) == 'j' || s.charAt(i) == 'J')) {
                    i++;
                }
            }
            if (i < n && isIdStart(s.charAt(i))) {
                throw err("invalid number literal", st);
            }
            add(T_NUMBER, st, i, null);
        }

        void digits() {
            while (i < n && (Character.isDigit(s.charAt(i)) || s.charAt(i) == '_')) {
                i++;
            }
        }

        void string(int st, String prefix) {
            String lp = prefix.toLowerCase(Locale.ROOT);
            boolean fmt = lp.indexOf('f') >= 0 || lp.indexOf('t') >= 0;
            boolean raw = lp.indexOf('r') >= 0;
            char q = s.charAt(i);
            boolean triple = i + 2 < n && s.charAt(i + 1) == q && s.charAt(i + 2) == q;
            if (fmt) {
                i += triple ? 3 : 1;
                add(T_FSTART, st, i, null);
                if (nModes == modes.length) {
                    modes = Arrays.copyOf(modes, nModes * 2);
                }
                modes[nModes++] = new FMode(q, triple, raw, depth);
                return;
            }
            i += triple ? 3 : 1;
            while (true) {
                if (i >= n) {
                    throw err("missing closing quote in string literal", st);
                }
                char c = s.charAt(i);
                if (c == '\\') {
                    i += 2;
                    if (i - 1 < n && s.charAt(i - 1) == '\r' && i < n && s.charAt(i) == '\n') {
                        i++;
                    }
                    continue;
                }
                if (c == q) {
                    if (!triple) {
                        i++;
                        break;
                    }
                    if (i + 2 < n && s.charAt(i + 1) == q && s.charAt(i + 2) == q) {
                        i += 3;
                        break;
                    }
                    i++;
                    continue;
                }
                if ((c == '\n' || c == '\r') && !triple) {
                    throw err("missing closing quote in string literal", st);
                }
                i++;
            }
            if (i > n) {
                throw err("missing closing quote in string literal", st);
            }
            add(T_STRING, st, i, null);
        }

        boolean atQuote(FMode m) {
            if (s.charAt(i) != m.quote) {
                return false;
            }
            return !m.triple || (i + 2 < n && s.charAt(i + 1) == m.quote && s.charAt(i + 2) == m.quote);
        }

        void lexFMiddle(FMode m) {
            int st = i;
            boolean spec = m.nFields > 0;
            while (true) {
                if (i >= n) {
                    throw err("unterminated f-string", st);
                }
                char c = s.charAt(i);
                if (atQuote(m)) {
                    if (spec) {
                        throw err("f-string: expecting '}'", i);
                    }
                    if (i > st) {
                        add(T_FMIDDLE, st, i, null);
                    }
                    int e = i + (m.triple ? 3 : 1);
                    add(T_FEND, i, e, null);
                    i = e;
                    nModes--;
                    return;
                }
                if (c == '{') {
                    if (!spec && i + 1 < n && s.charAt(i + 1) == '{') {
                        i += 2;
                        continue;
                    }
                    if (i > st) {
                        add(T_FMIDDLE, st, i, null);
                    }
                    add(T_OP, i, i + 1, "{");
                    i++;
                    depth++;
                    m.push(depth);
                    return;
                }
                if (c == '}') {
                    if (spec) {
                        if (i > st) {
                            add(T_FMIDDLE, st, i, null);
                        }
                        add(T_OP, i, i + 1, "}");
                        i++;
                        depth--;
                        m.nFields--;
                        return;
                    }
                    if (i + 1 < n && s.charAt(i + 1) == '}') {
                        i += 2;
                        continue;
                    }
                    throw err("f-string: single '}' is not allowed", i);
                }
                if (c == '\\') {
                    if (i + 1 < n) {
                        char d = s.charAt(i + 1);
                        if (!m.raw && d == 'N' && i + 2 < n && s.charAt(i + 2) == '{') {
                            int close = s.indexOf('}', i + 3);
                            if (close < 0) {
                                throw err("unterminated \\N escape", i);
                            }
                            i = close + 1;
                            continue;
                        }
                        if (d == '{' || d == '}') {
                            i++;
                            continue;
                        }
                        i += 2;
                        continue;
                    }
                    i++;
                    continue;
                }
                if ((c == '\n' || c == '\r') && !m.triple) {
                    throw err("unterminated f-string", st);
                }
                i++;
            }
        }

        void operator(char c) {
            int st = i;
            for (String op : OPS3) {
                if (s.startsWith(op, i)) {
                    i += 3;
                    add(T_OP, st, i, op);
                    return;
                }
            }
            for (String op : OPS2) {
                if (s.startsWith(op, i)) {
                    i += 2;
                    add(T_OP, st, i, op);
                    return;
                }
            }
            String op;
            switch (c) {
                case '(': case '[': case '{':
                    depth++;
                    op = OPS1[c];
                    break;
                case ')': case ']': case '}':
                    if (depth == 0) {
                        throw err("unmatched '" + c + "'", i);
                    }
                    depth--;
                    op = OPS1[c];
                    break;
                case '+': case '-': case '*': case '/': case '%': case '@': case '&': case '|': case '^': case '~':
                case '<': case '>': case ',': case ':': case ';': case '.': case '=':
                    op = OPS1[c];
                    break;
                case '!':
                    if (nModes > 0) {
                        op = "!";
                        break;
                    }
                    throw err("unexpected character '!'", i);
                default:
                    throw err("unexpected character '" + c + "'", i);
            }
            i++;
            add(T_OP, st, i, op);
        }
    }

    // =====================================================================================
    // Syntax tree. Ranges follow ruff: an expression range excludes its own enclosing
    // parentheses (parenthesized tuples and generators keep theirs); a compound node starts
    // where its first child's source starts, parentheses included.
    // =====================================================================================

    interface Visit {
        void visit(Node n);
    }

    abstract static class Node {
        int start;
        int end;
        /** The comments attached to this node, or null. */
        NodeComments comments;

        Node at(int s, int e) {
            start = s;
            end = e;
            return this;
        }

        /** Visits direct child nodes in source order. */
        void each(Visit v) {
        }

        static void all(Visit v, List<? extends Node> nodes) {
            if (nodes != null) {
                for (Node x : nodes) {
                    v.visit(x);
                }
            }
        }

        static void one(Visit v, Node x) {
            if (x != null) {
                v.visit(x);
            }
        }
    }

    static final class Ident extends Node {
        final String id;

        Ident(String id) {
            this.id = id;
        }
    }

    static final class Module extends Node {
        List<Stmt> body;

        @Override
        void each(Visit v) {
            all(v, body);
        }
    }

    abstract static class Stmt extends Node {
    }

    abstract static class Expr extends Node {
    }

    static final class FunctionDef extends Stmt {
        boolean isAsync;
        List<Decorator> decorators;
        Ident name;
        TypeParams typeParams;
        Parameters parameters;
        Expr returns;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            all(v, decorators);
            one(v, typeParams);
            one(v, parameters);
            one(v, returns);
            all(v, body);
        }
    }

    static final class ClassDef extends Stmt {
        List<Decorator> decorators;
        Ident name;
        TypeParams typeParams;
        Arguments arguments;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            all(v, decorators);
            one(v, typeParams);
            one(v, arguments);
            all(v, body);
        }
    }

    static final class Return extends Stmt {
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Delete extends Stmt {
        List<Expr> targets;

        @Override
        void each(Visit v) {
            all(v, targets);
        }
    }

    static final class Assign extends Stmt {
        List<Expr> targets;
        Expr value;

        @Override
        void each(Visit v) {
            all(v, targets);
            one(v, value);
        }
    }

    static final class AugAssign extends Stmt {
        Expr target;
        String op;
        Expr value;

        @Override
        void each(Visit v) {
            one(v, target);
            one(v, value);
        }
    }

    static final class AnnAssign extends Stmt {
        Expr target;
        Expr annotation;
        Expr value;
        boolean simple;

        @Override
        void each(Visit v) {
            one(v, target);
            one(v, annotation);
            one(v, value);
        }
    }

    static final class TypeAlias extends Stmt {
        Expr name;
        TypeParams typeParams;
        Expr value;

        @Override
        void each(Visit v) {
            one(v, name);
            one(v, typeParams);
            one(v, value);
        }
    }

    static final class For extends Stmt {
        boolean isAsync;
        Expr target;
        Expr iter;
        List<Stmt> body;
        List<Stmt> orelse;

        @Override
        void each(Visit v) {
            one(v, target);
            one(v, iter);
            all(v, body);
            all(v, orelse);
        }
    }

    static final class While extends Stmt {
        Expr test;
        List<Stmt> body;
        List<Stmt> orelse;

        @Override
        void each(Visit v) {
            one(v, test);
            all(v, body);
            all(v, orelse);
        }
    }

    static final class If extends Stmt {
        Expr test;
        List<Stmt> body;
        List<ElifElse> clauses;

        @Override
        void each(Visit v) {
            one(v, test);
            all(v, body);
            all(v, clauses);
        }
    }

    static final class ElifElse extends Node {
        Expr test;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            one(v, test);
            all(v, body);
        }
    }

    static final class With extends Stmt {
        boolean isAsync;
        List<WithItem> items;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            all(v, items);
            all(v, body);
        }
    }

    static final class WithItem extends Node {
        Expr context;
        Expr vars;

        @Override
        void each(Visit v) {
            one(v, context);
            one(v, vars);
        }
    }

    static final class Match extends Stmt {
        Expr subject;
        List<MatchCase> cases;

        @Override
        void each(Visit v) {
            one(v, subject);
            all(v, cases);
        }
    }

    static final class MatchCase extends Node {
        Pattern pattern;
        Expr guard;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            one(v, pattern);
            one(v, guard);
            all(v, body);
        }
    }

    static final class Raise extends Stmt {
        Expr exc;
        Expr cause;

        @Override
        void each(Visit v) {
            one(v, exc);
            one(v, cause);
        }
    }

    static final class Try extends Stmt {
        List<Stmt> body;
        List<ExceptHandler> handlers;
        List<Stmt> orelse;
        List<Stmt> finalbody;
        boolean isStar;

        @Override
        void each(Visit v) {
            all(v, body);
            all(v, handlers);
            all(v, orelse);
            all(v, finalbody);
        }
    }

    static final class ExceptHandler extends Node {
        Expr type;
        Ident name;
        List<Stmt> body;

        @Override
        void each(Visit v) {
            one(v, type);
            all(v, body);
        }
    }

    static final class Assert extends Stmt {
        Expr test;
        Expr msg;

        @Override
        void each(Visit v) {
            one(v, test);
            one(v, msg);
        }
    }

    static final class Import extends Stmt {
        List<Alias> names;

        @Override
        void each(Visit v) {
            all(v, names);
        }
    }

    static final class ImportFrom extends Stmt {
        Ident module;
        List<Alias> names;
        int level;

        @Override
        void each(Visit v) {
            all(v, names);
        }
    }

    static final class Alias extends Node {
        Ident name;
        Ident asname;
    }

    static final class Global extends Stmt {
        List<Ident> names;
        boolean nonlocal;
    }

    static final class ExprStmt extends Stmt {
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    /** `pass`, `break` or `continue`. */
    static final class KeywordStmt extends Stmt {
        final String keyword;

        KeywordStmt(String keyword) {
            this.keyword = keyword;
        }
    }

    static final class Decorator extends Node {
        Expr expression;

        @Override
        void each(Visit v) {
            one(v, expression);
        }
    }

    static final class TypeParams extends Node {
        List<TypeParam> params;

        @Override
        void each(Visit v) {
            all(v, params);
        }
    }

    static final class TypeParam extends Node {
        /** 0 = TypeVar, 1 = TypeVarTuple (`*`), 2 = ParamSpec (`**`). */
        int kind;
        Ident name;
        Expr bound;
        Expr dflt;

        @Override
        void each(Visit v) {
            one(v, bound);
            one(v, dflt);
        }
    }

    static final class Parameters extends Node {
        List<ParamWD> posonly = new ArrayList<>();
        List<ParamWD> args = new ArrayList<>();
        Param vararg;
        List<ParamWD> kwonly = new ArrayList<>();
        Param kwarg;
        boolean lambda;

        boolean isEmpty() {
            return posonly.isEmpty() && args.isEmpty() && vararg == null && kwonly.isEmpty() && kwarg == null;
        }

        @Override
        void each(Visit v) {
            all(v, posonly);
            all(v, args);
            one(v, vararg);
            all(v, kwonly);
            one(v, kwarg);
        }
    }

    static final class Param extends Node {
        Ident name;
        Expr annotation;

        @Override
        void each(Visit v) {
            one(v, annotation);
        }
    }

    static final class ParamWD extends Node {
        Param param;
        Expr dflt;

        @Override
        void each(Visit v) {
            one(v, param);
            one(v, dflt);
        }
    }

    static final class Arguments extends Node {
        List<Expr> args = new ArrayList<>();
        List<Keyword> keywords = new ArrayList<>();
        /** Arguments and keywords in source order. */
        List<Node> ordered = new ArrayList<>();

        boolean isEmpty() {
            return ordered.isEmpty();
        }

        @Override
        void each(Visit v) {
            all(v, ordered);
        }
    }

    static final class Keyword extends Node {
        Ident arg;
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Comprehension extends Node {
        Expr target;
        Expr iter;
        List<Expr> ifs = new ArrayList<>();
        boolean isAsync;

        @Override
        void each(Visit v) {
            one(v, target);
            one(v, iter);
            all(v, ifs);
        }
    }

    // ----------------------------------------------------------------------- expressions

    static final class BoolOp extends Expr {
        String op;
        List<Expr> values;

        @Override
        void each(Visit v) {
            all(v, values);
        }
    }

    static final class Named extends Expr {
        Expr target;
        Expr value;

        @Override
        void each(Visit v) {
            one(v, target);
            one(v, value);
        }
    }

    static final class BinOp extends Expr {
        Expr left;
        String op;
        Expr right;

        @Override
        void each(Visit v) {
            one(v, left);
            one(v, right);
        }
    }

    static final class UnaryOp extends Expr {
        String op;
        Expr operand;

        @Override
        void each(Visit v) {
            one(v, operand);
        }
    }

    static final class Lambda extends Expr {
        Parameters parameters;
        Expr body;

        @Override
        void each(Visit v) {
            one(v, parameters);
            one(v, body);
        }
    }

    static final class IfExp extends Expr {
        Expr test;
        Expr body;
        Expr orelse;

        @Override
        void each(Visit v) {
            one(v, body);
            one(v, test);
            one(v, orelse);
        }
    }

    static final class DictE extends Expr {
        List<Expr> keys = new ArrayList<>();
        List<Expr> values = new ArrayList<>();

        @Override
        void each(Visit v) {
            for (int k = 0; k < values.size(); k++) {
                one(v, keys.get(k));
                v.visit(values.get(k));
            }
        }
    }

    /** List, set or tuple display. */
    static final class Seq extends Expr {
        /** '[' list, '{' set, '(' tuple. */
        final char kind;
        List<Expr> elts;
        boolean parenthesized;

        Seq(char kind) {
            this.kind = kind;
        }

        @Override
        void each(Visit v) {
            all(v, elts);
        }
    }

    /** List/set/dict comprehension or generator expression. */
    static final class Comp extends Expr {
        /** '[' list, '{' set, 'd' dict, '(' generator. */
        final char kind;
        Expr elt;
        Expr value;
        List<Comprehension> generators;
        boolean parenthesized;

        Comp(char kind) {
            this.kind = kind;
        }

        @Override
        void each(Visit v) {
            one(v, elt);
            one(v, value);
            all(v, generators);
        }
    }

    static final class Await extends Expr {
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Yield extends Expr {
        Expr value;
        boolean from;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Compare extends Expr {
        Expr left;
        List<String> ops = new ArrayList<>();
        List<Expr> comparators = new ArrayList<>();

        @Override
        void each(Visit v) {
            one(v, left);
            all(v, comparators);
        }
    }

    static final class Call extends Expr {
        Expr func;
        Arguments arguments;

        @Override
        void each(Visit v) {
            one(v, func);
            one(v, arguments);
        }
    }

    static final int S_STR = 0, S_BYTES = 1, S_FSTR = 2, S_TSTR = 3;

    /** A string, bytes, f-string or t-string expression made of one or more implicitly concatenated parts. */
    static final class Str extends Expr {
        int kind;
        /** {@link StrPart} or {@link FPart}. */
        List<Node> parts = new ArrayList<>();

        boolean implicit() {
            return parts.size() > 1;
        }

        @Override
        void each(Visit v) {
            all(v, parts);
        }
    }

    /** One plain string or bytes literal. */
    static final class StrPart extends Node {
    }

    /** One f-string or t-string. */
    static final class FPart extends Node {
        boolean tstring;
        /** {@link FLit} or {@link FInterp}. */
        List<Node> elements = new ArrayList<>();

        @Override
        void each(Visit v) {
            all(v, elements);
        }
    }

    static final class FLit extends Node {
    }

    static final class FInterp extends Node {
        Expr expr;
        /** Source range of the expression text, parentheses included. */
        int exprStart;
        int exprEnd;
        /** Debug text around the expression (`{ x = }`), or null when the field is not a debug field. */
        String debugLeading;
        String debugTrailing;
        char conversion;
        /** Format spec elements, or null. */
        List<Node> spec;
        int specStart;
        int specEnd;

        @Override
        void each(Visit v) {
            one(v, expr);
            all(v, spec);
        }
    }

    static final class Num extends Expr {
    }

    /** `True`, `False`, `None` or `...`. */
    static final class Const extends Expr {
        final String value;

        Const(String value) {
            this.value = value;
        }
    }

    static final class Attribute extends Expr {
        Expr value;
        Ident attr;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Subscript extends Expr {
        Expr value;
        Expr slice;

        @Override
        void each(Visit v) {
            one(v, value);
            one(v, slice);
        }
    }

    static final class Starred extends Expr {
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class Name extends Expr {
        final String id;

        Name(String id) {
            this.id = id;
        }
    }

    static final class Slice extends Expr {
        Expr lower;
        Expr upper;
        Expr step;

        @Override
        void each(Visit v) {
            one(v, lower);
            one(v, upper);
            one(v, step);
        }
    }

    // ----------------------------------------------------------------------- patterns

    abstract static class Pattern extends Node {
    }

    static final class PValue extends Pattern {
        Expr value;

        @Override
        void each(Visit v) {
            one(v, value);
        }
    }

    static final class PSingleton extends Pattern {
        String value;
    }

    static final class PSequence extends Pattern {
        List<Pattern> patterns = new ArrayList<>();
    
        @Override
        void each(Visit v) {
            all(v, patterns);
        }
    }

    static final class PMapping extends Pattern {
        List<Expr> keys = new ArrayList<>();
        List<Pattern> patterns = new ArrayList<>();
        Ident rest;

        @Override
        void each(Visit v) {
            for (int k = 0; k < keys.size(); k++) {
                v.visit(keys.get(k));
                v.visit(patterns.get(k));
            }
        }
    }

    static final class PClass extends Pattern {
        Expr cls;
        PArguments arguments;

        @Override
        void each(Visit v) {
            one(v, cls);
            one(v, arguments);
        }
    }

    static final class PArguments extends Node {
        List<Pattern> patterns = new ArrayList<>();
        List<PKeyword> keywords = new ArrayList<>();

        @Override
        void each(Visit v) {
            all(v, patterns);
            all(v, keywords);
        }
    }

    static final class PKeyword extends Node {
        Ident attr;
        Pattern pattern;

        @Override
        void each(Visit v) {
            one(v, pattern);
        }
    }

    static final class PStar extends Pattern {
        Ident name;
    }

    static final class PAs extends Pattern {
        Pattern pattern;
        Ident name;

        @Override
        void each(Visit v) {
            one(v, pattern);
        }
    }

    static final class POr extends Pattern {
        List<Pattern> patterns = new ArrayList<>();

        @Override
        void each(Visit v) {
            all(v, patterns);
        }
    }

    // =====================================================================================
    // Parser: recursive descent over the significant tokens.
    // =====================================================================================

    static final Set<String> KEYWORDS = new HashSet<>(Arrays.asList(
            "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue", "def",
            "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is",
            "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield"));

    static final Set<String> AUG_OPS = new HashSet<>(Arrays.asList(
            "+=", "-=", "*=", "/=", "//=", "%=", "@=", "&=", "|=", "^=", ">>=", "<<=", "**="));

    static final class Parser {
        final String s;
        final Tok[] t;
        int p;
        int prevEnd;

        Parser(String s, List<Tok> toks) {
            this.s = s;
            ArrayList<Tok> sig = new ArrayList<>(toks.size());
            for (Tok k : toks) {
                if (k.kind != T_NL && k.kind != T_COMMENT) {
                    sig.add(k);
                }
            }
            t = sig.toArray(new Tok[0]);
        }

        Tok cur() {
            return t[p];
        }

        Tok peek(int k) {
            return t[Math.min(p + k, t.length - 1)];
        }

        Tok next() {
            Tok k = t[p];
            if (k.kind != T_END) {
                p++;
            }
            prevEnd = k.end;
            return k;
        }

        boolean at(String op) {
            Tok k = t[p];
            return (k.kind == T_OP || k.kind == T_NAME) && k.text.equals(op);
        }

        boolean eat(String op) {
            if (at(op)) {
                next();
                return true;
            }
            return false;
        }

        Tok expect(String op) {
            if (!at(op)) {
                throw err("expected '" + op + "'");
            }
            return next();
        }

        ParseException err(String m) {
            return new ParseException(m, t[p].start);
        }

        boolean atName() {
            Tok k = t[p];
            return k.kind == T_NAME && !KEYWORDS.contains(k.text);
        }

        Ident ident() {
            if (!atName()) {
                throw err("expected a name");
            }
            Tok k = next();
            return (Ident) new Ident(k.text).at(k.start, k.end);
        }

        boolean atStmtEnd() {
            Tok k = t[p];
            return k.kind == T_NEWLINE || k.kind == T_END || (k.kind == T_OP && k.text.equals(";"));
        }

        boolean startsExpression() {
            Tok k = t[p];
            switch (k.kind) {
                case T_NAME:
                    if (!KEYWORDS.contains(k.text)) {
                        return true;
                    }
                    switch (k.text) {
                        case "True": case "False": case "None": case "not": case "lambda": case "await": case "yield":
                            return true;
                        default:
                            return false;
                    }
                case T_NUMBER: case T_STRING: case T_FSTART:
                    return true;
                case T_OP:
                    switch (k.text) {
                        case "(": case "[": case "{": case "-": case "+": case "~": case "*": case "...":
                            return true;
                        default:
                            return false;
                    }
                default:
                    return false;
            }
        }

        static int endOf(List<? extends Node> nodes) {
            return nodes.get(nodes.size() - 1).end;
        }

        // ------------------------------------------------------------------- statements

        Module module() {
            Module m = new Module();
            m.body = new ArrayList<>();
            while (cur().kind != T_END) {
                if (cur().kind == T_NEWLINE) {
                    next();
                    continue;
                }
                if (cur().kind == T_INDENT) {
                    throw err("unexpected indentation");
                }
                statement(m.body);
            }
            m.at(0, s.length());
            return m;
        }

        void statement(List<Stmt> out) {
            Tok k = cur();
            if (k.kind == T_NAME) {
                switch (k.text) {
                    case "if":
                        out.add(ifStmt());
                        return;
                    case "while":
                        out.add(whileStmt());
                        return;
                    case "for":
                        out.add(forStmt(k.start, false));
                        return;
                    case "try":
                        out.add(tryStmt());
                        return;
                    case "with":
                        out.add(withStmt(k.start, false));
                        return;
                    case "def":
                        out.add(funcDef(k.start, new ArrayList<>(), false));
                        return;
                    case "class":
                        out.add(classDef(k.start, new ArrayList<>()));
                        return;
                    case "async": {
                        Tok n1 = peek(1);
                        if (n1.is("def")) {
                            next();
                            out.add(funcDef(k.start, new ArrayList<>(), true));
                            return;
                        }
                        if (n1.is("for")) {
                            next();
                            out.add(forStmt(k.start, true));
                            return;
                        }
                        if (n1.is("with")) {
                            next();
                            out.add(withStmt(k.start, true));
                            return;
                        }
                        throw err("expected def, for or with after async");
                    }
                    case "match":
                        if (tryMatch(out)) {
                            return;
                        }
                        break;
                    default:
                        break;
                }
            } else if (k.kind == T_OP && k.text.equals("@")) {
                out.add(decorated());
                return;
            }
            simpleLine(out);
        }

        void simpleLine(List<Stmt> out) {
            while (true) {
                out.add(simpleStmt());
                if (eat(";")) {
                    if (cur().kind == T_NEWLINE || cur().kind == T_END) {
                        break;
                    }
                    continue;
                }
                break;
            }
            if (cur().kind == T_NEWLINE) {
                next();
            } else if (cur().kind != T_END) {
                throw err("simple statements must be separated by newlines or semicolons");
            }
        }

        List<Stmt> block() {
            expect(":");
            List<Stmt> body = new ArrayList<>();
            if (cur().kind == T_NEWLINE) {
                next();
                if (cur().kind != T_INDENT) {
                    throw err("expected an indented block");
                }
                next();
                while (cur().kind != T_DEDENT && cur().kind != T_END) {
                    statement(body);
                }
                if (cur().kind == T_DEDENT) {
                    next();
                }
            } else {
                simpleLine(body);
            }
            if (body.isEmpty()) {
                throw err("expected a statement");
            }
            return body;
        }

        Stmt simpleStmt() {
            Tok k = cur();
            int st = k.start;
            if (k.kind == T_NAME) {
                switch (k.text) {
                    case "pass": case "break": case "continue":
                        next();
                        return (Stmt) new KeywordStmt(k.text).at(st, k.end);
                    case "return": {
                        next();
                        Return r = new Return();
                        if (!atStmtEnd()) {
                            r.value = starExpressions();
                        }
                        return (Stmt) r.at(st, prevEnd);
                    }
                    case "raise": {
                        next();
                        Raise r = new Raise();
                        if (!atStmtEnd()) {
                            r.exc = expression();
                            if (eat("from")) {
                                r.cause = expression();
                            }
                        }
                        return (Stmt) r.at(st, prevEnd);
                    }
                    case "global": case "nonlocal": {
                        next();
                        Global g = new Global();
                        g.nonlocal = k.text.equals("nonlocal");
                        g.names = new ArrayList<>();
                        do {
                            g.names.add(ident());
                        } while (eat(","));
                        return (Stmt) g.at(st, prevEnd);
                    }
                    case "del": {
                        next();
                        Delete d = new Delete();
                        d.targets = new ArrayList<>();
                        do {
                            if (atStmtEnd()) {
                                break;
                            }
                            d.targets.add(bitOr());
                        } while (eat(","));
                        if (d.targets.isEmpty()) {
                            throw err("expected a target");
                        }
                        return (Stmt) d.at(st, prevEnd);
                    }
                    case "assert": {
                        next();
                        Assert a = new Assert();
                        a.test = expression();
                        if (eat(",")) {
                            a.msg = expression();
                        }
                        return (Stmt) a.at(st, prevEnd);
                    }
                    case "import": {
                        next();
                        Import im = new Import();
                        im.names = new ArrayList<>();
                        do {
                            im.names.add(alias(true));
                        } while (eat(","));
                        return (Stmt) im.at(st, prevEnd);
                    }
                    case "from":
                        return importFrom();
                    case "type": {
                        Tok n1 = peek(1);
                        Tok n2 = peek(2);
                        if (n1.kind == T_NAME && !KEYWORDS.contains(n1.text) && (n2.is("=") || n2.is("["))) {
                            next();
                            TypeAlias ta = new TypeAlias();
                            Tok nm = next();
                            ta.name = (Expr) new Name(nm.text).at(nm.start, nm.end);
                            if (at("[")) {
                                ta.typeParams = typeParams();
                            }
                            expect("=");
                            ta.value = expression();
                            return (Stmt) ta.at(st, prevEnd);
                        }
                        break;
                    }
                    default:
                        break;
                }
            }
            Expr first = starExpressionsOrYield();
            if (at("=")) {
                List<Expr> chain = new ArrayList<>();
                chain.add(first);
                while (eat("=")) {
                    chain.add(starExpressionsOrYield());
                }
                Assign a = new Assign();
                a.targets = new ArrayList<>(chain.subList(0, chain.size() - 1));
                a.value = chain.get(chain.size() - 1);
                return (Stmt) a.at(st, prevEnd);
            }
            if (at(":")) {
                next();
                AnnAssign a = new AnnAssign();
                a.target = first;
                a.annotation = expression();
                a.simple = first instanceof Name && first.start == st;
                if (eat("=")) {
                    a.value = starExpressionsOrYield();
                }
                return (Stmt) a.at(st, prevEnd);
            }
            if (cur().kind == T_OP && AUG_OPS.contains(cur().text)) {
                AugAssign a = new AugAssign();
                a.target = first;
                a.op = next().text;
                a.value = starExpressionsOrYield();
                return (Stmt) a.at(st, prevEnd);
            }
            ExprStmt e = new ExprStmt();
            e.value = first;
            return (Stmt) e.at(st, prevEnd);
        }

        Alias alias(boolean dotted) {
            Alias a = new Alias();
            int st = cur().start;
            if (dotted) {
                a.name = dottedName();
            } else {
                a.name = ident();
            }
            if (eat("as")) {
                a.asname = ident();
            }
            return (Alias) a.at(st, prevEnd);
        }

        Ident dottedName() {
            int st = cur().start;
            StringBuilder b = new StringBuilder(ident().id);
            while (at(".") && peek(1).kind == T_NAME) {
                next();
                b.append('.').append(ident().id);
            }
            return (Ident) new Ident(b.toString()).at(st, prevEnd);
        }

        Stmt importFrom() {
            int st = next().start;
            ImportFrom f = new ImportFrom();
            while (at(".") || at("...")) {
                f.level += next().text.length();
            }
            if (!at("import")) {
                f.module = dottedName();
            }
            expect("import");
            f.names = new ArrayList<>();
            if (at("*")) {
                Tok k = next();
                Alias a = new Alias();
                a.name = (Ident) new Ident("*").at(k.start, k.end);
                f.names.add((Alias) a.at(k.start, k.end));
            } else if (eat("(")) {
                do {
                    if (at(")")) {
                        break;
                    }
                    f.names.add(alias(false));
                } while (eat(","));
                expect(")");
            } else {
                do {
                    f.names.add(alias(false));
                } while (eat(","));
            }
            if (f.names.isEmpty()) {
                throw err("expected an import name");
            }
            return (Stmt) f.at(st, prevEnd);
        }

        Stmt ifStmt() {
            int st = next().start;
            If s = new If();
            s.test = named();
            s.body = block();
            s.clauses = new ArrayList<>();
            int end = endOf(s.body);
            while (at("elif")) {
                int cs = next().start;
                ElifElse c = new ElifElse();
                c.test = named();
                c.body = block();
                end = endOf(c.body);
                s.clauses.add((ElifElse) c.at(cs, end));
            }
            if (at("else")) {
                int cs = next().start;
                ElifElse c = new ElifElse();
                c.body = block();
                end = endOf(c.body);
                s.clauses.add((ElifElse) c.at(cs, end));
            }
            return (Stmt) s.at(st, end);
        }

        Stmt whileStmt() {
            int st = next().start;
            While w = new While();
            w.test = named();
            w.body = block();
            w.orelse = new ArrayList<>();
            if (eat("else")) {
                w.orelse = block();
            }
            return (Stmt) w.at(st, endOf(w.orelse.isEmpty() ? w.body : w.orelse));
        }

        Stmt forStmt(int st, boolean isAsync) {
            expect("for");
            For f = new For();
            f.isAsync = isAsync;
            f.target = targets();
            expect("in");
            f.iter = starExpressions();
            f.body = block();
            f.orelse = new ArrayList<>();
            if (eat("else")) {
                f.orelse = block();
            }
            return (Stmt) f.at(st, endOf(f.orelse.isEmpty() ? f.body : f.orelse));
        }

        Stmt tryStmt() {
            int st = next().start;
            Try tr = new Try();
            tr.body = block();
            tr.handlers = new ArrayList<>();
            tr.orelse = new ArrayList<>();
            tr.finalbody = new ArrayList<>();
            int end = endOf(tr.body);
            while (at("except")) {
                int hs = next().start;
                if (eat("*")) {
                    tr.isStar = true;
                }
                ExceptHandler h = new ExceptHandler();
                if (!at(":")) {
                    int ts = cur().start;
                    Expr type = expression();
                    if (at(",")) {
                        List<Expr> elts = new ArrayList<>();
                        elts.add(type);
                        while (eat(",")) {
                            if (at(":") || at("as")) {
                                break;
                            }
                            elts.add(expression());
                        }
                        Seq tup = new Seq('(');
                        tup.elts = elts;
                        type = (Expr) tup.at(ts, prevEnd);
                    }
                    h.type = type;
                    if (eat("as")) {
                        h.name = ident();
                    }
                }
                h.body = block();
                end = endOf(h.body);
                tr.handlers.add((ExceptHandler) h.at(hs, end));
            }
            if (eat("else")) {
                tr.orelse = block();
                end = endOf(tr.orelse);
            }
            if (eat("finally")) {
                tr.finalbody = block();
                end = endOf(tr.finalbody);
            }
            if (tr.handlers.isEmpty() && tr.finalbody.isEmpty()) {
                throw err("expected except or finally");
            }
            return (Stmt) tr.at(st, end);
        }

        Stmt withStmt(int st, boolean isAsync) {
            expect("with");
            With w = new With();
            w.isAsync = isAsync;
            if (at("(")) {
                int save = p;
                int savePrev = prevEnd;
                try {
                    next();
                    List<WithItem> items = new ArrayList<>();
                    do {
                        if (at(")")) {
                            break;
                        }
                        items.add(withItem());
                    } while (eat(","));
                    expect(")");
                    if (at(":") && !items.isEmpty()) {
                        w.items = items;
                    } else {
                        p = save;
                        prevEnd = savePrev;
                    }
                } catch (ParseException e) {
                    p = save;
                    prevEnd = savePrev;
                }
            }
            if (w.items == null) {
                w.items = new ArrayList<>();
                do {
                    w.items.add(withItem());
                } while (eat(","));
            }
            w.body = block();
            return (Stmt) w.at(st, endOf(w.body));
        }

        WithItem withItem() {
            int st = cur().start;
            WithItem it = new WithItem();
            it.context = expression();
            if (eat("as")) {
                it.vars = target();
            }
            return (WithItem) it.at(st, prevEnd);
        }

        Stmt funcDef(int st, List<Decorator> decs, boolean isAsync) {
            expect("def");
            FunctionDef f = new FunctionDef();
            f.isAsync = isAsync;
            f.decorators = decs;
            f.name = ident();
            if (at("[")) {
                f.typeParams = typeParams();
            }
            int ps = cur().start;
            expect("(");
            f.parameters = parameters(false);
            expect(")");
            f.parameters.at(ps, prevEnd);
            if (eat("->")) {
                f.returns = expression();
            }
            f.body = block();
            return (Stmt) f.at(st, endOf(f.body));
        }

        Stmt classDef(int st, List<Decorator> decs) {
            expect("class");
            ClassDef c = new ClassDef();
            c.decorators = decs;
            c.name = ident();
            if (at("[")) {
                c.typeParams = typeParams();
            }
            if (at("(")) {
                c.arguments = arguments();
            }
            c.body = block();
            return (Stmt) c.at(st, endOf(c.body));
        }

        Stmt decorated() {
            int st = cur().start;
            List<Decorator> decs = new ArrayList<>();
            while (at("@")) {
                int ds = next().start;
                Decorator d = new Decorator();
                d.expression = named();
                decs.add((Decorator) d.at(ds, prevEnd));
                if (cur().kind != T_NEWLINE) {
                    throw err("expected newline after decorator");
                }
                next();
            }
            if (at("def")) {
                return funcDef(st, decs, false);
            }
            if (at("async") && peek(1).is("def")) {
                next();
                return funcDef(st, decs, true);
            }
            if (at("class")) {
                return classDef(st, decs);
            }
            throw err("expected a function or class after decorators");
        }

        TypeParams typeParams() {
            int st = cur().start;
            expect("[");
            TypeParams tp = new TypeParams();
            tp.params = new ArrayList<>();
            do {
                if (at("]")) {
                    break;
                }
                int ps = cur().start;
                TypeParam x = new TypeParam();
                if (eat("*")) {
                    x.kind = 1;
                } else if (eat("**")) {
                    x.kind = 2;
                }
                x.name = ident();
                if (x.kind == 0 && eat(":")) {
                    x.bound = expression();
                }
                if (eat("=")) {
                    x.dflt = at("*") ? starExpression() : expression();
                }
                tp.params.add((TypeParam) x.at(ps, prevEnd));
            } while (eat(","));
            expect("]");
            return (TypeParams) tp.at(st, prevEnd);
        }

        /** Parameters up to (not including) the closing `)` or, for lambdas, `:`. */
        Parameters parameters(boolean lambda) {
            Parameters ps = new Parameters();
            ps.lambda = lambda;
            String close = lambda ? ":" : ")";
            boolean kwonly = false;
            int first = -1;
            while (!at(close)) {
                int st = cur().start;
                if (first < 0) {
                    first = st;
                }
                if (eat("/")) {
                    ps.posonly.addAll(ps.args);
                    ps.args.clear();
                } else if (at("*") && (peek(1).is(",") || peek(1).is(close))) {
                    next();
                    kwonly = true;
                } else if (eat("*")) {
                    ps.vararg = param(st, lambda, true);
                    kwonly = true;
                } else if (eat("**")) {
                    ps.kwarg = param(st, lambda, false);
                } else {
                    ParamWD pw = new ParamWD();
                    pw.param = param(st, lambda, false);
                    if (eat("=")) {
                        pw.dflt = expression();
                    }
                    pw.at(st, prevEnd);
                    (kwonly ? ps.kwonly : ps.args).add(pw);
                }
                if (!eat(",")) {
                    break;
                }
            }
            if (lambda && first >= 0) {
                ps.at(first, prevEnd);
            }
            return ps;
        }

        Param param(int st, boolean lambda, boolean starAnnotation) {
            Param pa = new Param();
            pa.name = ident();
            if (!lambda && eat(":")) {
                pa.annotation = starAnnotation && at("*") ? starExpression() : expression();
            }
            return (Param) pa.at(st, prevEnd);
        }

        boolean tryMatch(List<Stmt> out) {
            Tok n1 = peek(1);
            if (n1.kind == T_NEWLINE || n1.kind == T_END || (n1.kind == T_OP && (n1.text.equals("=") || n1.text.equals(".")
                    || n1.text.equals(",") || n1.text.equals(")") || n1.text.equals(":") || n1.text.equals(";")
                    || AUG_OPS.contains(n1.text)))) {
                return false;
            }
            int save = p;
            int savePrev = prevEnd;
            try {
                int st = next().start;
                Match m = new Match();
                m.subject = matchSubject();
                expect(":");
                if (cur().kind != T_NEWLINE) {
                    throw err("expected newline");
                }
                next();
                if (cur().kind != T_INDENT) {
                    throw err("expected indent");
                }
                next();
                m.cases = new ArrayList<>();
                while (at("case")) {
                    m.cases.add(matchCase());
                }
                if (m.cases.isEmpty()) {
                    throw err("expected case");
                }
                if (cur().kind == T_DEDENT) {
                    next();
                } else if (cur().kind != T_END) {
                    throw err("expected case");
                }
                out.add((Stmt) m.at(st, endOf(m.cases)));
                return true;
            } catch (ParseException e) {
                p = save;
                prevEnd = savePrev;
                return false;
            }
        }

        Expr matchSubject() {
            int st = cur().start;
            Expr first = starNamed();
            if (!at(",")) {
                return first;
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(first);
            while (eat(",")) {
                if (at(":")) {
                    break;
                }
                elts.add(starNamed());
            }
            Seq tup = new Seq('(');
            tup.elts = elts;
            return (Expr) tup.at(st, prevEnd);
        }

        MatchCase matchCase() {
            int st = next().start;
            MatchCase c = new MatchCase();
            int ps = cur().start;
            Pattern first = maybeStarPattern();
            if (at(",")) {
                PSequence sq = new PSequence();
                sq.patterns.add(first);
                while (eat(",")) {
                    if (at(":") || at("if")) {
                        break;
                    }
                    sq.patterns.add(maybeStarPattern());
                }
                first = (Pattern) sq.at(ps, prevEnd);
            }
            c.pattern = first;
            if (eat("if")) {
                c.guard = named();
            }
            c.body = block();
            return (MatchCase) c.at(st, endOf(c.body));
        }

        Pattern maybeStarPattern() {
            if (at("*")) {
                int st = next().start;
                PStar ps = new PStar();
                Ident nm = ident();
                ps.name = nm.id.equals("_") ? null : nm;
                return (Pattern) ps.at(st, prevEnd);
            }
            return pattern();
        }

        Pattern pattern() {
            int st = cur().start;
            Pattern or = orPattern();
            if (eat("as")) {
                PAs as = new PAs();
                as.pattern = or;
                as.name = ident();
                return (Pattern) as.at(st, prevEnd);
            }
            return or;
        }

        Pattern orPattern() {
            int st = cur().start;
            Pattern first = closedPattern();
            if (!at("|")) {
                return first;
            }
            POr or = new POr();
            or.patterns.add(first);
            while (eat("|")) {
                or.patterns.add(closedPattern());
            }
            return (Pattern) or.at(st, prevEnd);
        }

        Pattern closedPattern() {
            Tok k = cur();
            int st = k.start;
            if (k.kind == T_NUMBER || (k.kind == T_OP && k.text.equals("-"))) {
                PValue v = new PValue();
                v.value = signedNumber();
                return (Pattern) v.at(v.value.start, v.value.end);
            }
            if (k.kind == T_STRING || k.kind == T_FSTART) {
                PValue v = new PValue();
                v.value = strings();
                return (Pattern) v.at(v.value.start, v.value.end);
            }
            if (k.kind == T_NAME) {
                switch (k.text) {
                    case "None": case "True": case "False": {
                        next();
                        PSingleton sg = new PSingleton();
                        sg.value = k.text;
                        return (Pattern) sg.at(k.start, k.end);
                    }
                    default:
                        break;
                }
                Ident nm = ident();
                Expr e = (Expr) new Name(nm.id).at(nm.start, nm.end);
                boolean dotted = false;
                while (at(".")) {
                    next();
                    Attribute a = new Attribute();
                    a.value = e;
                    a.attr = ident();
                    e = (Expr) a.at(st, prevEnd);
                    dotted = true;
                }
                if (at("(")) {
                    PClass pc = new PClass();
                    pc.cls = e;
                    pc.arguments = patternArguments();
                    return (Pattern) pc.at(st, prevEnd);
                }
                if (dotted) {
                    PValue v = new PValue();
                    v.value = e;
                    return (Pattern) v.at(st, prevEnd);
                }
                PAs as = new PAs();
                as.name = nm.id.equals("_") ? null : nm;
                return (Pattern) as.at(st, prevEnd);
            }
            if (k.kind == T_OP) {
                switch (k.text) {
                    case "(": {
                        next();
                        if (eat(")")) {
                            return (Pattern) new PSequence().at(st, prevEnd);
                        }
                        Pattern first = maybeStarPattern();
                        if (at(",")) {
                            PSequence sq = new PSequence();
                            sq.patterns.add(first);
                            while (eat(",")) {
                                if (at(")")) {
                                    break;
                                }
                                sq.patterns.add(maybeStarPattern());
                            }
                            expect(")");
                            return (Pattern) sq.at(st, prevEnd);
                        }
                        expect(")");
                        if (first instanceof PStar) {
                            PSequence sq = new PSequence();
                            sq.patterns.add(first);
                            return (Pattern) sq.at(st, prevEnd);
                        }
                        return first;
                    }
                    case "[": {
                        next();
                        PSequence sq = new PSequence();
                        while (!at("]")) {
                            sq.patterns.add(maybeStarPattern());
                            if (!eat(",")) {
                                break;
                            }
                        }
                        expect("]");
                        return (Pattern) sq.at(st, prevEnd);
                    }
                    case "{": {
                        next();
                        PMapping pm = new PMapping();
                        while (!at("}")) {
                            if (eat("**")) {
                                pm.rest = ident();
                            } else {
                                Tok kk = cur();
                                Expr key;
                                if (kk.kind == T_STRING || kk.kind == T_FSTART) {
                                    key = strings();
                                } else if (kk.kind == T_NUMBER || kk.is("-")) {
                                    key = signedNumber();
                                } else if (kk.is("None") || kk.is("True") || kk.is("False")) {
                                    next();
                                    key = (Expr) new Const(kk.text).at(kk.start, kk.end);
                                } else {
                                    int ks = kk.start;
                                    Ident nm = ident();
                                    key = (Expr) new Name(nm.id).at(nm.start, nm.end);
                                    while (eat(".")) {
                                        Attribute a = new Attribute();
                                        a.value = key;
                                        a.attr = ident();
                                        key = (Expr) a.at(ks, prevEnd);
                                    }
                                }
                                expect(":");
                                pm.keys.add(key);
                                pm.patterns.add(pattern());
                            }
                            if (!eat(",")) {
                                break;
                            }
                        }
                        expect("}");
                        return (Pattern) pm.at(st, prevEnd);
                    }
                    default:
                        break;
                }
            }
            throw err("expected a pattern");
        }

        Expr signedNumber() {
            int st = cur().start;
            Expr e;
            if (at("-")) {
                next();
                UnaryOp u = new UnaryOp();
                u.op = "-";
                Tok k = next();
                if (k.kind != T_NUMBER) {
                    throw err("expected a number");
                }
                u.operand = (Expr) new Num().at(k.start, k.end);
                e = (Expr) u.at(st, prevEnd);
            } else {
                Tok k = next();
                e = (Expr) new Num().at(k.start, k.end);
            }
            if (at("+") || at("-")) {
                BinOp b = new BinOp();
                b.left = e;
                b.op = next().text;
                Tok k = next();
                if (k.kind != T_NUMBER) {
                    throw err("expected a number");
                }
                b.right = (Expr) new Num().at(k.start, k.end);
                e = (Expr) b.at(st, prevEnd);
            }
            return e;
        }

        PArguments patternArguments() {
            int st = cur().start;
            expect("(");
            PArguments pa = new PArguments();
            while (!at(")")) {
                if (cur().kind == T_NAME && peek(1).is("=")) {
                    int ks = cur().start;
                    PKeyword kw = new PKeyword();
                    kw.attr = ident();
                    expect("=");
                    kw.pattern = pattern();
                    pa.keywords.add((PKeyword) kw.at(ks, prevEnd));
                } else {
                    pa.patterns.add(pattern());
                }
                if (!eat(",")) {
                    break;
                }
            }
            expect(")");
            return (PArguments) pa.at(st, prevEnd);
        }

        // ------------------------------------------------------------------- expressions

        Expr starExpressionsOrYield() {
            return at("yield") ? yieldExpr() : starExpressions();
        }

        Expr yieldExpr() {
            int st = next().start;
            Yield y = new Yield();
            if (eat("from")) {
                y.from = true;
                y.value = expression();
            } else if (startsExpression()) {
                y.value = starExpressions();
            }
            return (Expr) y.at(st, prevEnd);
        }

        Expr starExpressions() {
            int st = cur().start;
            Expr e = starExpression();
            if (!at(",")) {
                return e;
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(e);
            while (eat(",")) {
                if (!startsExpression()) {
                    break;
                }
                elts.add(starExpression());
            }
            Seq tup = new Seq('(');
            tup.elts = elts;
            return (Expr) tup.at(st, prevEnd);
        }

        Expr starExpression() {
            if (at("*")) {
                int st = next().start;
                Starred sd = new Starred();
                sd.value = bitOr();
                return (Expr) sd.at(st, prevEnd);
            }
            return expression();
        }

        Expr starNamed() {
            if (at("*")) {
                int st = next().start;
                Starred sd = new Starred();
                sd.value = bitOr();
                return (Expr) sd.at(st, prevEnd);
            }
            return named();
        }

        Expr named() {
            int st = cur().start;
            Expr e = expression();
            if (at(":=")) {
                if (!(e instanceof Name)) {
                    throw err("assignment expression target must be a name");
                }
                next();
                Named nm = new Named();
                nm.target = e;
                nm.value = expression();
                return (Expr) nm.at(st, prevEnd);
            }
            return e;
        }

        /** A single assignment target (`for` targets and `as` targets). */
        Expr target() {
            if (at("*")) {
                int st = next().start;
                Starred sd = new Starred();
                sd.value = bitOr();
                return (Expr) sd.at(st, prevEnd);
            }
            return bitOr();
        }

        Expr targets() {
            int st = cur().start;
            Expr e = target();
            if (!at(",")) {
                return e;
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(e);
            while (eat(",")) {
                if (at("in") || at("=")) {
                    break;
                }
                elts.add(target());
            }
            Seq tup = new Seq('(');
            tup.elts = elts;
            return (Expr) tup.at(st, prevEnd);
        }

        Expr expression() {
            if (at("lambda")) {
                return lambda();
            }
            int st = cur().start;
            Expr e = disjunction();
            if (at("if")) {
                next();
                IfExp x = new IfExp();
                x.body = e;
                x.test = disjunction();
                expect("else");
                x.orelse = expression();
                return (Expr) x.at(st, prevEnd);
            }
            return e;
        }

        Expr lambda() {
            int st = next().start;
            Lambda l = new Lambda();
            if (!at(":")) {
                l.parameters = parameters(true);
            }
            expect(":");
            l.body = expression();
            return (Expr) l.at(st, prevEnd);
        }

        Expr disjunction() {
            int st = cur().start;
            Expr e = conjunction();
            if (!at("or")) {
                return e;
            }
            BoolOp b = new BoolOp();
            b.op = "or";
            b.values = new ArrayList<>();
            b.values.add(e);
            while (eat("or")) {
                b.values.add(conjunction());
            }
            return (Expr) b.at(st, prevEnd);
        }

        Expr conjunction() {
            int st = cur().start;
            Expr e = inversion();
            if (!at("and")) {
                return e;
            }
            BoolOp b = new BoolOp();
            b.op = "and";
            b.values = new ArrayList<>();
            b.values.add(e);
            while (eat("and")) {
                b.values.add(inversion());
            }
            return (Expr) b.at(st, prevEnd);
        }

        Expr inversion() {
            if (at("not")) {
                int st = next().start;
                UnaryOp u = new UnaryOp();
                u.op = "not";
                u.operand = inversion();
                return (Expr) u.at(st, prevEnd);
            }
            return comparison();
        }

        String compareOp() {
            Tok k = cur();
            if (k.kind == T_OP) {
                switch (k.text) {
                    case "<": case ">": case "==": case ">=": case "<=": case "!=":
                        next();
                        return k.text;
                    default:
                        return null;
                }
            }
            if (k.kind == T_NAME) {
                switch (k.text) {
                    case "in":
                        next();
                        return "in";
                    case "not":
                        if (peek(1).is("in")) {
                            next();
                            next();
                            return "not in";
                        }
                        return null;
                    case "is":
                        next();
                        if (eat("not")) {
                            return "is not";
                        }
                        return "is";
                    default:
                        return null;
                }
            }
            return null;
        }

        Expr comparison() {
            int st = cur().start;
            Expr e = bitOr();
            String op = compareOp();
            if (op == null) {
                return e;
            }
            Compare c = new Compare();
            c.left = e;
            while (op != null) {
                c.ops.add(op);
                c.comparators.add(bitOr());
                op = compareOp();
            }
            return (Expr) c.at(st, prevEnd);
        }

        Expr bitOr() {
            return binary(0);
        }

        static final String[][] LEVELS = {{"|"}, {"^"}, {"&"}, {"<<", ">>"}, {"+", "-"}, {"*", "/", "//", "%", "@"}};

        Expr binary(int level) {
            if (level == LEVELS.length) {
                return factor();
            }
            int st = cur().start;
            Expr e = binary(level + 1);
            while (true) {
                Tok k = cur();
                if (k.kind != T_OP) {
                    return e;
                }
                String op = null;
                for (String o : LEVELS[level]) {
                    if (o.equals(k.text)) {
                        op = o;
                        break;
                    }
                }
                if (op == null) {
                    return e;
                }
                next();
                BinOp b = new BinOp();
                b.left = e;
                b.op = op;
                b.right = binary(level + 1);
                e = (Expr) b.at(st, prevEnd);
            }
        }

        Expr factor() {
            Tok k = cur();
            if (k.kind == T_OP && (k.text.equals("-") || k.text.equals("+") || k.text.equals("~"))) {
                next();
                UnaryOp u = new UnaryOp();
                u.op = k.text;
                u.operand = factor();
                return (Expr) u.at(k.start, prevEnd);
            }
            return power();
        }

        Expr power() {
            int st = cur().start;
            Expr e;
            if (at("await")) {
                next();
                Await a = new Await();
                a.value = primary();
                e = (Expr) a.at(st, prevEnd);
            } else {
                e = primary();
            }
            if (at("**")) {
                next();
                BinOp b = new BinOp();
                b.left = e;
                b.op = "**";
                b.right = factor();
                e = (Expr) b.at(st, prevEnd);
            }
            return e;
        }

        Expr primary() {
            int st = cur().start;
            Expr e = atom();
            while (true) {
                if (at(".")) {
                    next();
                    Tok k = cur();
                    if (k.kind != T_NAME) {
                        throw err("expected an attribute name");
                    }
                    next();
                    Attribute a = new Attribute();
                    a.value = e;
                    a.attr = (Ident) new Ident(k.text).at(k.start, k.end);
                    e = (Expr) a.at(st, prevEnd);
                } else if (at("(")) {
                    Call c = new Call();
                    c.func = e;
                    c.arguments = arguments();
                    e = (Expr) c.at(st, prevEnd);
                } else if (at("[")) {
                    next();
                    Subscript sb = new Subscript();
                    sb.value = e;
                    sb.slice = slices();
                    expect("]");
                    e = (Expr) sb.at(st, prevEnd);
                } else {
                    return e;
                }
            }
        }

        Arguments arguments() {
            int st = cur().start;
            expect("(");
            Arguments a = new Arguments();
            while (!at(")")) {
                int as = cur().start;
                if (at("*")) {
                    next();
                    Starred sd = new Starred();
                    sd.value = expression();
                    sd.at(as, prevEnd);
                    a.args.add(sd);
                    a.ordered.add(sd);
                } else if (at("**")) {
                    next();
                    Keyword kw = new Keyword();
                    kw.value = expression();
                    kw.at(as, prevEnd);
                    a.keywords.add(kw);
                    a.ordered.add(kw);
                } else if (cur().kind == T_NAME && peek(1).is("=") && !KEYWORDS.contains(cur().text)) {
                    Keyword kw = new Keyword();
                    kw.arg = ident();
                    expect("=");
                    kw.value = expression();
                    kw.at(as, prevEnd);
                    a.keywords.add(kw);
                    a.ordered.add(kw);
                } else {
                    Expr e = named();
                    if (at("for") || (at("async") && peek(1).is("for"))) {
                        Comp g = new Comp('(');
                        g.elt = e;
                        g.generators = comprehensions();
                        e = (Expr) g.at(as, prevEnd);
                    }
                    a.args.add(e);
                    a.ordered.add(e);
                }
                if (!eat(",")) {
                    break;
                }
            }
            expect(")");
            return (Arguments) a.at(st, prevEnd);
        }

        Expr slices() {
            int st = cur().start;
            Expr first = sliceItem();
            if (!at(",")) {
                return first;
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(first);
            while (eat(",")) {
                if (at("]")) {
                    break;
                }
                elts.add(sliceItem());
            }
            Seq tup = new Seq('(');
            tup.elts = elts;
            return (Expr) tup.at(st, prevEnd);
        }

        Expr sliceItem() {
            int st = cur().start;
            if (at("*")) {
                return starNamed();
            }
            Expr lower = null;
            if (!at(":")) {
                lower = named();
                if (!at(":")) {
                    return lower;
                }
            }
            expect(":");
            Slice sl = new Slice();
            sl.lower = lower;
            if (!at(":") && !at("]") && !at(",")) {
                sl.upper = expression();
            }
            if (eat(":")) {
                if (!at("]") && !at(",")) {
                    sl.step = expression();
                }
            }
            return (Expr) sl.at(st, prevEnd);
        }

        List<Comprehension> comprehensions() {
            List<Comprehension> gens = new ArrayList<>();
            while (at("for") || (at("async") && peek(1).is("for"))) {
                int st = cur().start;
                Comprehension c = new Comprehension();
                if (eat("async")) {
                    c.isAsync = true;
                }
                expect("for");
                c.target = targets();
                expect("in");
                c.iter = disjunction();
                while (at("if")) {
                    next();
                    c.ifs.add(disjunction());
                }
                gens.add((Comprehension) c.at(st, prevEnd));
            }
            return gens;
        }

        Expr atom() {
            Tok k = cur();
            switch (k.kind) {
                case T_NAME:
                    switch (k.text) {
                        case "True": case "False": case "None":
                            next();
                            return (Expr) new Const(k.text).at(k.start, k.end);
                        default:
                            if (KEYWORDS.contains(k.text)) {
                                throw err("unexpected keyword '" + k.text + "'");
                            }
                            next();
                            return (Expr) new Name(k.text).at(k.start, k.end);
                    }
                case T_NUMBER:
                    next();
                    return (Expr) new Num().at(k.start, k.end);
                case T_STRING: case T_FSTART:
                    return strings();
                case T_OP:
                    switch (k.text) {
                        case "(":
                            return parenAtom();
                        case "[":
                            return listAtom();
                        case "{":
                            return braceAtom();
                        case "...":
                            next();
                            return (Expr) new Const("...").at(k.start, k.end);
                        default:
                            break;
                    }
                    break;
                default:
                    break;
            }
            throw err("expected an expression");
        }

        Expr parenAtom() {
            int st = next().start;
            if (eat(")")) {
                Seq tup = new Seq('(');
                tup.elts = new ArrayList<>();
                tup.parenthesized = true;
                return (Expr) tup.at(st, prevEnd);
            }
            if (at("yield")) {
                Expr y = yieldExpr();
                expect(")");
                return y;
            }
            Expr first = starNamed();
            if (at("for") || (at("async") && peek(1).is("for"))) {
                Comp g = new Comp('(');
                g.elt = first;
                g.generators = comprehensions();
                expect(")");
                g.parenthesized = true;
                return (Expr) g.at(st, prevEnd);
            }
            if (at(",")) {
                List<Expr> elts = new ArrayList<>();
                elts.add(first);
                while (eat(",")) {
                    if (at(")")) {
                        break;
                    }
                    elts.add(starNamed());
                }
                expect(")");
                Seq tup = new Seq('(');
                tup.elts = elts;
                tup.parenthesized = true;
                return (Expr) tup.at(st, prevEnd);
            }
            expect(")");
            if (first instanceof Starred) {
                throw new ParseException("cannot use starred expression here", first.start);
            }
            return first;
        }

        Expr listAtom() {
            int st = next().start;
            if (eat("]")) {
                Seq l = new Seq('[');
                l.elts = new ArrayList<>();
                return (Expr) l.at(st, prevEnd);
            }
            Expr first = starNamed();
            if (at("for") || (at("async") && peek(1).is("for"))) {
                Comp c = new Comp('[');
                c.elt = first;
                c.generators = comprehensions();
                expect("]");
                return (Expr) c.at(st, prevEnd);
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(first);
            while (eat(",")) {
                if (at("]")) {
                    break;
                }
                elts.add(starNamed());
            }
            expect("]");
            Seq l = new Seq('[');
            l.elts = elts;
            return (Expr) l.at(st, prevEnd);
        }

        Expr braceAtom() {
            int st = next().start;
            if (eat("}")) {
                return (Expr) new DictE().at(st, prevEnd);
            }
            if (at("**")) {
                DictE d = new DictE();
                dictItems(d);
                return (Expr) d.at(st, prevEnd);
            }
            Expr first = starNamed();
            if (at(":")) {
                next();
                Expr v = expression();
                if (at("for") || (at("async") && peek(1).is("for"))) {
                    Comp c = new Comp('d');
                    c.elt = first;
                    c.value = v;
                    c.generators = comprehensions();
                    expect("}");
                    return (Expr) c.at(st, prevEnd);
                }
                DictE d = new DictE();
                d.keys.add(first);
                d.values.add(v);
                if (eat(",")) {
                    dictItems(d);
                } else {
                    expect("}");
                }
                return (Expr) d.at(st, prevEnd);
            }
            if (at("for") || (at("async") && peek(1).is("for"))) {
                Comp c = new Comp('{');
                c.elt = first;
                c.generators = comprehensions();
                expect("}");
                return (Expr) c.at(st, prevEnd);
            }
            List<Expr> elts = new ArrayList<>();
            elts.add(first);
            while (eat(",")) {
                if (at("}")) {
                    break;
                }
                elts.add(starNamed());
            }
            expect("}");
            Seq set = new Seq('{');
            set.elts = elts;
            return (Expr) set.at(st, prevEnd);
        }

        /** Remaining dict items up to and including the closing brace. */
        void dictItems(DictE d) {
            while (!at("}")) {
                if (eat("**")) {
                    d.keys.add(null);
                    d.values.add(bitOr());
                } else {
                    d.keys.add(expression());
                    expect(":");
                    d.values.add(expression());
                }
                if (!eat(",")) {
                    break;
                }
            }
            expect("}");
        }

        Expr strings() {
            Str e = new Str();
            int st = cur().start;
            boolean f = false;
            boolean tt = false;
            boolean bytes = false;
            boolean str = false;
            while (cur().kind == T_STRING || cur().kind == T_FSTART) {
                if (cur().kind == T_STRING) {
                    Tok k = next();
                    e.parts.add(new StrPart().at(k.start, k.end));
                    if (isBytesLiteral(s, k.start)) {
                        bytes = true;
                    } else {
                        str = true;
                    }
                } else {
                    FPart fp = fstring();
                    e.parts.add(fp);
                    if (fp.tstring) {
                        tt = true;
                    } else {
                        f = true;
                    }
                }
            }
            if (bytes && (str || f || tt)) {
                throw new ParseException("cannot mix bytes and nonbytes literals", st);
            }
            if (tt && (str || f)) {
                throw new ParseException("cannot mix t-string literals with other literals", st);
            }
            e.kind = tt ? S_TSTR : f ? S_FSTR : bytes ? S_BYTES : S_STR;
            return (Expr) e.at(st, prevEnd);
        }

        FPart fstring() {
            Tok open = next();
            FPart fp = new FPart();
            for (int j = open.start; j < open.end; j++) {
                char c = s.charAt(j);
                if (c == 't' || c == 'T') {
                    fp.tstring = true;
                }
            }
            fElements(fp.elements, true);
            if (cur().kind != T_FEND) {
                throw err("expected end of f-string");
            }
            Tok close = next();
            return (FPart) fp.at(open.start, close.end);
        }

        void fElements(List<Node> out, boolean top) {
            while (true) {
                Tok k = cur();
                if (k.kind == T_FMIDDLE) {
                    next();
                    out.add(new FLit().at(k.start, k.end));
                } else if (k.kind == T_OP && k.text.equals("{")) {
                    out.add(interpolation());
                } else {
                    return;
                }
            }
        }

        FInterp interpolation() {
            Tok open = next();
            FInterp fi = new FInterp();
            int exprStart = cur().start;
            fi.expr = at("yield") ? yieldExpr() : starExpressions();
            int exprEnd = prevEnd;
            fi.exprStart = exprStart;
            fi.exprEnd = exprEnd;
            if (at("=")) {
                Tok eq = next();
                fi.debugLeading = s.substring(open.end, exprStart);
                int stop = cur().start;
                fi.debugTrailing = s.substring(exprEnd, stop);
                if (eq.end > stop) {
                    throw err("invalid debug expression");
                }
            }
            if (at("!")) {
                next();
                Tok c = cur();
                if (c.kind != T_NAME || c.text.length() != 1 || "rsa".indexOf(c.text.charAt(0)) < 0 || c.start != prevEnd) {
                    throw err("invalid conversion character");
                }
                next();
                fi.conversion = c.text.charAt(0);
            }
            if (at(":")) {
                Tok colon = next();
                fi.spec = new ArrayList<>();
                fi.specStart = colon.end;
                fElements(fi.spec, false);
                fi.specEnd = cur().start;
            }
            if (!at("}")) {
                throw err("f-string: expecting '}'");
            }
            Tok close = next();
            return (FInterp) fi.at(open.start, close.end);
        }
    }

    static boolean isBytesLiteral(String s, int start) {
        for (int j = start; j < s.length(); j++) {
            char c = s.charAt(j);
            if (c == '\'' || c == '"') {
                return false;
            }
            if (c == 'b' || c == 'B') {
                return true;
            }
        }
        return false;
    }

    // ============================================================================================
    // Document IR and printer: a port of ruff_formatter's FormatElement, Printer and FitsMeasurer.
    // ============================================================================================

    static final int FLAT = 0, EXPANDED = 1;
    static final int G_FLAT = 0, G_EXPAND = 1, G_PROPAGATED = 2;
    static final int L_SOFT_OR_SPACE = 0, L_SOFT = 1, L_HARD = 2, L_EMPTY = 3;
    static final int M_FIRST_LINE = 0, M_ALL_LINES = 1, M_OVERFLOW = 2;
    static final int LINE_WIDTH = 88, INDENT_WIDTH = 4;

    static final int E_SPACE = 0, E_TOKEN = 1, E_TEXT = 2, E_LINE = 3, E_EXPAND_PARENT = 4,
            E_SUFFIX_BOUNDARY = 5, E_INTERNED = 6, E_BEST_FITTING = 7;
    // Tags: a start tag is even, its end tag is start + 1; the stack frame kind is the start tag.
    static final int E_GROUP = 10, E_INDENT = 12, E_DEDENT = 14, E_ALIGN = 16, E_COND = 18,
            E_INDENT_IF_BREAKS = 20, E_SUFFIX = 22, E_FITS_EXPANDED = 24, E_ENTRY = 26,
            E_BF_ENTRY = 28, E_BFP = 30, E_VERBATIM = 32, E_CGROUP = 34, K_ROOT = 98;

    static final class El {
        final int k;
        String text;
        /** Text width (-1 multiline), line mode, align count, reserved suffix width, dedent-to-root flag,
         * best-fitting all-lines flag or the condition mode of a conditional group. */
        int n;
        /** Group id of a group, a condition, an indent-if-group-breaks or a best-fit-parenthesize. */
        int id;
        /** Mode of a group, or the expected mode of a condition (-1: no condition). */
        int mode;
        boolean propagate;
        El[] content;
        El[][] variants;

        El(int k) {
            this.k = k;
        }

        boolean isStart() {
            return k >= E_GROUP && (k & 1) == 0;
        }
    }

    static final El SPACE = new El(E_SPACE), EXPAND_PARENT = new El(E_EXPAND_PARENT),
            SUFFIX_BOUNDARY = new El(E_SUFFIX_BOUNDARY), SOFT = line(L_SOFT),
            SOFT_OR_SPACE = line(L_SOFT_OR_SPACE), HARD = line(L_HARD), EMPTY = line(L_EMPTY),
            INDENT = new El(E_INDENT), END_INDENT = new El(E_INDENT + 1),
            END_GROUP = new El(E_GROUP + 1), END_COND = new El(E_COND + 1),
            END_INDENT_IF_BREAKS = new El(E_INDENT_IF_BREAKS + 1), END_SUFFIX = new El(E_SUFFIX + 1),
            END_FITS_EXPANDED = new El(E_FITS_EXPANDED + 1), BF_ENTRY = new El(E_BF_ENTRY),
            END_BF_ENTRY = new El(E_BF_ENTRY + 1), END_BFP = new El(E_BFP + 1),
            END_DEDENT = new El(E_DEDENT + 1), END_ALIGN = new El(E_ALIGN + 1),
            VERBATIM = new El(E_VERBATIM), END_VERBATIM = new El(E_VERBATIM + 1), END_CGROUP = new El(E_CGROUP + 1),
            OPEN_PAREN = token("("), CLOSE_PAREN = token(")");

    static El line(int mode) {
        El e = new El(E_LINE);
        e.n = mode;
        return e;
    }

    static El token(String s) {
        El e = new El(E_TOKEN);
        e.text = s;
        e.n = s.length();
        return e;
    }

    static El text(String s) {
        El e = new El(E_TEXT);
        e.text = s;
        e.n = textWidth(s);
        return e;
    }

    /** ruff's TextWidth::from_text: -1 when the text contains a newline. */
    static int textWidth(String s) {
        int w = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c >= ' ' && c <= '~') {
                w++;
            } else if (c == '\t') {
                w += INDENT_WIDTH;
            } else if (c == '\n') {
                return -1;
            } else {
                int cp = s.codePointAt(i);
                if (cp > 0xFFFF) {
                    i++;
                }
                w += charWidth(cp);
            }
        }
        return w;
    }

    static boolean willBreak(El[] els) {
        int ignore = 0;
        for (El e : els) {
            if (e.k == E_SUFFIX || e.k == E_FITS_EXPANDED) {
                ignore++;
            } else if (e.k == E_SUFFIX + 1 || e.k == E_FITS_EXPANDED + 1) {
                ignore = Math.max(0, ignore - 1);
            } else if (ignore == 0 && willBreak(e)) {
                return true;
            }
        }
        return false;
    }

    static boolean willBreak(El e) {
        switch (e.k) {
            case E_EXPAND_PARENT:
                return true;
            case E_GROUP:
            case E_CGROUP:
                return e.mode != G_FLAT;
            case E_LINE:
                return e.n == L_HARD || e.n == L_EMPTY;
            case E_TEXT:
                return e.n < 0;
            case E_INTERNED:
                return willBreak(e.content);
            case E_BEST_FITTING:
                return willBreak(e.variants[0]);
            default:
                return false;
        }
    }

    /** Document::propagate_expand: expands every group that encloses a forced line break. */
    static void propagateExpand(El[] doc) {
        propagateExpands(doc, new ArrayList<>(), new IdentityHashMap<>());
    }

    private static final Object BEST_FITTING_MARK = new Object();

    /** Enclosing entries: an El (group or fits-expanded), BEST_FITTING_MARK or a Boolean for a best-fit-parenthesize. */
    private static boolean propagateExpands(El[] els, ArrayList<Object> enclosing, IdentityHashMap<El[], Boolean> checked) {
        boolean expands = false;
        for (El e : els) {
            boolean elementExpands;
            switch (e.k) {
                case E_GROUP:
                case E_CGROUP:
                    enclosing.add(e);
                    elementExpands = false;
                    break;
                case E_GROUP + 1:
                case E_CGROUP + 1: {
                    Object top = enclosing.isEmpty() ? null : enclosing.remove(enclosing.size() - 1);
                    elementExpands = top instanceof El && ((El) top).k == e.k - 1 && ((El) top).mode != G_FLAT;
                    break;
                }
                case E_BFP:
                    enclosing.add(expands);
                    expands = false;
                    continue;
                case E_BFP + 1: {
                    Object top = enclosing.isEmpty() ? null : enclosing.remove(enclosing.size() - 1);
                    if (top instanceof Boolean) {
                        expands = (Boolean) top;
                    }
                    continue;
                }
                case E_INTERNED: {
                    Boolean known = checked.get(e.content);
                    if (known == null) {
                        known = propagateExpands(e.content, enclosing, checked);
                        checked.put(e.content, known);
                    }
                    elementExpands = known;
                    break;
                }
                case E_BEST_FITTING:
                    enclosing.add(BEST_FITTING_MARK);
                    for (El[] v : e.variants) {
                        propagateExpands(v, enclosing, checked);
                    }
                    enclosing.remove(enclosing.size() - 1);
                    continue;
                case E_FITS_EXPANDED:
                    enclosing.add(new Object[] {e, expands});
                    elementExpands = false;
                    break;
                case E_FITS_EXPANDED + 1: {
                    Object top = enclosing.isEmpty() ? null : enclosing.remove(enclosing.size() - 1);
                    if (top instanceof Object[]) {
                        expands = (Boolean) ((Object[]) top)[1];
                    }
                    continue;
                }
                case E_TEXT:
                    elementExpands = e.n < 0;
                    break;
                case E_EXPAND_PARENT:
                    elementExpands = true;
                    break;
                case E_LINE:
                    elementExpands = e.n == L_HARD || e.n == L_EMPTY;
                    break;
                default:
                    elementExpands = false;
            }
            if (elementExpands) {
                expands = true;
                Object top = enclosing.isEmpty() ? null : enclosing.get(enclosing.size() - 1);
                if (top instanceof El && ((El) top).mode == G_FLAT) {
                    ((El) top).mode = G_PROPAGATED;
                } else if (top instanceof Object[]) {
                    ((El) ((Object[]) top)[0]).propagate = true;
                }
            }
        }
        return expands;
    }

    // Print element arguments packed into an int: indentation level (bits 16+), align (bits 8-15),
    // measure mode (bits 1-2) and print mode (bit 0).
    static int argsMode(int a) {
        return a & 1;
    }

    static int argsMeasure(int a) {
        return (a >>> 1) & 3;
    }

    static int argsIndent(int a) {
        return a >>> 8;
    }

    static int withMode(int a, int mode) {
        return (a & ~1) | mode;
    }

    static int withMeasure(int a, int measure) {
        return (a & ~6) | (measure << 1);
    }

    static int withIndent(int a, int indent) {
        return (a & 0xFF) | (indent << 8);
    }

    static int indentLevel(int indent) {
        return indent >>> 8;
    }

    static int indentAlign(int indent) {
        return indent & 0xFF;
    }

    static int incrementIndent(int indent) {
        return indent + 256;
    }

    static int decrementIndent(int indent) {
        int level = indentLevel(indent);
        return indentAlign(indent) == 0 ? Math.max(0, level - 1) << 8 : level << 8;
    }

    static int setAlign(int indent, int count) {
        return indentAlign(indent) == 0 ? (indent & ~0xFF) | count : ((indentLevel(indent) + 1) << 8) | count;
    }

    static final class Printer {
        final StringBuilder out = new StringBuilder();
        int lineWidth;
        int lineStart;
        int pendingIndent;
        boolean measuredGroupFits;
        int[] groupModes = new int[64];

        El[][] qa = new El[64][];
        int[] qp = new int[64];
        int qn;

        int[] sk = new int[128];
        int[] sa = new int[128];
        int sn;

        final ArrayList<El> sfx = new ArrayList<>();
        int[] sfxArgs = new int[16];

        // Fits measurer state (FitsMeasurer).
        El[][] fqa = new El[64][];
        int[] fqp = new int[64];
        int fqn;
        int restQ;
        int[] fk = new int[128];
        int[] fa = new int[128];
        int fn;
        int restS;
        int fLineWidth;
        int fPendingIndent;
        boolean fHasSuffix;
        boolean mustBeFlat;

        String print(El[] doc) {
            propagateExpand(doc);
            push(K_ROOT, withMode(0, EXPANDED));
            qpush(doc, 0);
            while (true) {
                El e = qpop();
                if (e != null) {
                    printElement(e);
                } else if (!flushSuffixes(null)) {
                    break;
                }
            }
            return out.toString();
        }

        // ---- print queue (PrintQueue) ----
        void qpush(El[] a, int p) {
            if (p >= a.length) {
                return;
            }
            if (qn == qa.length) {
                qa = Arrays.copyOf(qa, qn * 2);
                qp = Arrays.copyOf(qp, qn * 2);
            }
            qa[qn] = a;
            qp[qn++] = p;
        }

        void qpushOne(El e) {
            qpush(new El[] {e}, 0);
        }

        El qpop() {
            if (qn == 0) {
                return null;
            }
            int t = qn - 1;
            if (qp[t] < qa[t].length) {
                return qa[t][qp[t]++];
            }
            qn--;
            if (qn == 0) {
                return null;
            }
            t = qn - 1;
            return qp[t] < qa[t].length ? qa[t][qp[t]++] : null;
        }

        /** Skips (or collects) the content up to the end tag matching {@code kind}. */
        void qskip(int kind, ArrayList<El> collect) {
            int depth = 1;
            while (true) {
                El e = qpop();
                while (e != null && e.k == E_INTERNED) {
                    qpush(e.content, 0);
                    e = qpop();
                }
                if (e == null) {
                    throw new IllegalStateException("missing end tag");
                }
                if (e.k == kind) {
                    depth++;
                } else if (e.k == kind + 1 && --depth == 0) {
                    return;
                }
                if (collect != null) {
                    collect.add(e);
                }
            }
        }

        // ---- call stack (PrintCallStack) ----
        void push(int kind, int args) {
            if (sn == sk.length) {
                sk = Arrays.copyOf(sk, sn * 2);
                sa = Arrays.copyOf(sa, sn * 2);
            }
            sk[sn] = kind;
            sa[sn++] = args;
        }

        int pop(int kind) {
            if (sn == 0 || sk[sn - 1] != kind) {
                throw new IllegalStateException("unbalanced tag " + kind);
            }
            return sa[--sn];
        }

        int top() {
            return sa[sn - 1];
        }

        void setMode(int id, int mode) {
            if (id >= groupModes.length) {
                groupModes = Arrays.copyOf(groupModes, Math.max(id + 1, groupModes.length * 2));
            }
            groupModes[id] = mode + 1;
        }

        int getMode(int id) {
            if (id >= groupModes.length || groupModes[id] == 0) {
                throw new IllegalStateException("unknown group id " + id);
            }
            return groupModes[id] - 1;
        }

        int getModeOr(int id, int dflt) {
            return id >= groupModes.length || groupModes[id] == 0 ? dflt : groupModes[id] - 1;
        }

        void printElement(El e) {
            int args = top();
            switch (e.k) {
                case E_SPACE:
                    printText(" ", 1);
                    break;
                case E_TOKEN:
                case E_TEXT:
                    printText(e.text, e.n);
                    break;
                case E_LINE:
                    if (argsMode(args) == FLAT && (e.n == L_SOFT || e.n == L_SOFT_OR_SPACE)) {
                        if (e.n == L_SOFT_OR_SPACE) {
                            printText(" ", 1);
                        }
                    } else if (!sfx.isEmpty()) {
                        flushSuffixes(e);
                    } else {
                        if (out.length() > lineStart) {
                            printNewline();
                        }
                        if (e.n == L_EMPTY) {
                            printNewline();
                        }
                        pendingIndent = argsIndent(args);
                    }
                    break;
                case E_EXPAND_PARENT:
                    break;
                case E_SUFFIX_BOUNDARY:
                    flushSuffixes(HARD);
                    break;
                case E_BEST_FITTING:
                    printBestFitting(e);
                    break;
                case E_INTERNED:
                    qpush(e.content, 0);
                    break;
                case E_GROUP: {
                    int mode = e.mode != G_FLAT ? EXPANDED : flatGroupPrintMode(E_GROUP, e.id, args);
                    if (e.id != 0) {
                        setMode(e.id, mode);
                    }
                    push(E_GROUP, withMode(args, mode));
                    break;
                }
                case E_CGROUP: {
                    int expected = e.id == 0 ? argsMode(args) : getMode(e.id);
                    if (expected == e.n) {
                        int mode = e.mode != G_FLAT ? EXPANDED : flatGroupPrintMode(E_CGROUP, 0, args);
                        push(E_CGROUP, withMode(args, mode));
                    } else {
                        push(E_CGROUP, args);
                    }
                    break;
                }
                case E_BFP: {
                    boolean fitsFlat = flatGroupPrintMode(E_BFP, e.id, args) == FLAT;
                    int mode;
                    if (fitsFlat) {
                        mode = FLAT;
                    } else {
                        if (e.id != 0) {
                            setMode(e.id, EXPANDED);
                        }
                        push(E_BFP, withMeasure(args, M_ALL_LINES));
                        qpush(new El[] {OPEN_PAREN, INDENT, HARD}, 0);
                        boolean fitsExpanded = fits();
                        qn--;
                        pop(E_BFP);
                        mode = fitsExpanded ? EXPANDED : FLAT;
                    }
                    if (e.id != 0) {
                        setMode(e.id, mode);
                    }
                    if (mode == EXPANDED) {
                        qpush(new El[] {OPEN_PAREN, INDENT, HARD}, 0);
                    }
                    push(E_BFP, withMode(args, mode));
                    break;
                }
                case E_BFP + 1:
                    if (argsMode(args) == EXPANDED) {
                        pop(E_INDENT);
                        qpush(new El[] {HARD, CLOSE_PAREN}, 0);
                    }
                    pop(E_BFP);
                    break;
                case E_INDENT:
                    push(E_INDENT, withIndent(args, incrementIndent(argsIndent(args))));
                    break;
                case E_DEDENT:
                    push(E_DEDENT, withIndent(args, e.n == 1 ? 0 : decrementIndent(argsIndent(args))));
                    break;
                case E_ALIGN:
                    push(E_ALIGN, withIndent(args, setAlign(argsIndent(args), e.n)));
                    break;
                case E_COND: {
                    int mode = e.id == 0 ? argsMode(args) : getMode(e.id);
                    if (e.mode == mode) {
                        push(E_COND, args);
                    } else {
                        qskip(E_COND, null);
                    }
                    break;
                }
                case E_INDENT_IF_BREAKS: {
                    int mode = getMode(e.id);
                    push(E_INDENT_IF_BREAKS, mode == FLAT ? args : withIndent(args, incrementIndent(argsIndent(args))));
                    break;
                }
                case E_SUFFIX: {
                    lineWidth += e.n;
                    int start = sfx.size();
                    qskip(E_SUFFIX, sfx);
                    sfx.add(null);
                    if (sfxArgs.length < sfx.size()) {
                        sfxArgs = Arrays.copyOf(sfxArgs, sfx.size() * 2);
                    }
                    sfxArgs[sfx.size() - 1] = args;
                    assert start <= sfx.size();
                    break;
                }
                case E_FITS_EXPANDED: {
                    boolean met = true;
                    if (e.mode >= 0) {
                        int mode = e.id == 0 ? argsMode(args) : getMode(e.id);
                        met = e.mode == mode;
                    }
                    if (met) {
                        measuredGroupFits = false;
                    }
                    push(E_FITS_EXPANDED, args);
                    break;
                }
                case E_VERBATIM:
                case E_ENTRY:
                case E_BF_ENTRY:
                    push(e.k, args);
                    break;
                default:
                    if (e.k >= E_GROUP && (e.k & 1) == 1) {
                        pop(e.k - 1);
                        break;
                    }
                    throw new IllegalStateException("unexpected element " + e.k);
            }
        }

        int flatGroupPrintMode(int kind, int id, int args) {
            if (argsMode(args) == FLAT && measuredGroupFits) {
                return FLAT;
            }
            measuredGroupFits = true;
            if (id != 0) {
                setMode(id, FLAT);
            }
            push(kind, withMode(args, FLAT));
            boolean fits = fits();
            pop(kind);
            return fits ? FLAT : EXPANDED;
        }

        void printText(String s, int width) {
            if (pendingIndent != 0) {
                int level = indentLevel(pendingIndent);
                int align = indentAlign(pendingIndent);
                for (int i = 0, n = level * INDENT_WIDTH + align; i < n; i++) {
                    out.append(' ');
                }
                lineWidth += level * INDENT_WIDTH + align;
                pendingIndent = 0;
            }
            if (width >= 0) {
                out.append(s);
                lineWidth += width;
            } else {
                for (int i = 0; i < s.length(); i++) {
                    char c = s.charAt(i);
                    if (c == '\n') {
                        printNewline();
                    } else {
                        int cp = s.codePointAt(i);
                        out.appendCodePoint(cp);
                        if (cp > 0xFFFF) {
                            i++;
                        }
                        lineWidth += cp == '\t' ? INDENT_WIDTH : charWidth(cp);
                    }
                }
            }
        }

        void printNewline() {
            out.append('\n');
            lineWidth = 0;
            lineStart = out.length();
            measuredGroupFits = false;
        }

        boolean flushSuffixes(El lineBreak) {
            if (sfx.isEmpty()) {
                return false;
            }
            if (lineBreak != null) {
                qpushOne(lineBreak);
            }
            for (int i = sfx.size() - 1; i >= 0; i--) {
                El s = sfx.get(i);
                if (s == null) {
                    push(E_SUFFIX, sfxArgs[i]);
                    qpushOne(END_SUFFIX);
                } else {
                    qpushOne(s);
                }
            }
            sfx.clear();
            return true;
        }

        void printBestFitting(El e) {
            int args = top();
            if (argsMode(args) == FLAT && measuredGroupFits) {
                qpush(e.variants[0], 0);
                printEntry(args);
                return;
            }
            measuredGroupFits = true;
            El[][] vs = e.variants;
            for (int i = 0; i < vs.length - 1; i++) {
                El[] v = vs[i];
                qpush(v, 1);
                push(E_BF_ENTRY, withMeasure(withMode(args, FLAT), e.n == 1 ? M_ALL_LINES : M_FIRST_LINE));
                boolean fits = fits();
                pop(E_BF_ENTRY);
                qn--;
                if (fits) {
                    qpush(v, 0);
                    printEntry(withMode(args, FLAT));
                    return;
                }
            }
            qpush(vs[vs.length - 1], 0);
            printEntry(withMode(args, EXPANDED));
        }

        void printEntry(int args) {
            El start = qpop();
            if (start == null || start.k != E_BF_ENTRY) {
                throw new IllegalStateException("expected best fitting entry");
            }
            push(E_BF_ENTRY, args);
            int depth = 1;
            El e;
            while ((e = qpop()) != null) {
                if (e.k == E_ENTRY || e.k == E_BF_ENTRY) {
                    depth++;
                } else if (e.k == E_ENTRY + 1 || e.k == E_BF_ENTRY + 1) {
                    if (--depth == 0) {
                        pop(e.k - 1);
                        return;
                    }
                }
                printElement(e);
            }
            throw new IllegalStateException("unterminated entry");
        }

        // ---- FitsMeasurer ----
        boolean fits() {
            fqn = 0;
            restQ = qn;
            fn = 0;
            restS = sn;
            fLineWidth = lineWidth;
            fPendingIndent = pendingIndent;
            fHasSuffix = !sfx.isEmpty();
            mustBeFlat = false;
            El e;
            while ((e = fpop()) != null) {
                int r = fitsElement(e);
                if (r != 0) {
                    return r > 0;
                }
            }
            return true;
        }

        void fqpush(El[] a, int p) {
            if (p >= a.length) {
                return;
            }
            if (fqn == fqa.length) {
                fqa = Arrays.copyOf(fqa, fqn * 2);
                fqp = Arrays.copyOf(fqp, fqn * 2);
            }
            fqa[fqn] = a;
            fqp[fqn++] = p;
        }

        El fqpopOwn() {
            if (fqn == 0) {
                return null;
            }
            int t = fqn - 1;
            if (fqp[t] < fqa[t].length) {
                return fqa[t][fqp[t]++];
            }
            fqn--;
            if (fqn == 0) {
                return null;
            }
            t = fqn - 1;
            return fqp[t] < fqa[t].length ? fqa[t][fqp[t]++] : null;
        }

        El fpop() {
            El e = fqpopOwn();
            if (e != null) {
                return e;
            }
            if (restQ > 0) {
                restQ--;
                fqpush(qa[restQ], qp[restQ]);
                return fqpopOwn();
            }
            return null;
        }

        void fskip(int kind) {
            int depth = 1;
            while (true) {
                El e = fpop();
                while (e != null && e.k == E_INTERNED) {
                    fqpush(e.content, 0);
                    e = fpop();
                }
                if (e == null) {
                    throw new IllegalStateException("missing end tag");
                }
                if (e.k == kind) {
                    depth++;
                } else if (e.k == kind + 1 && --depth == 0) {
                    return;
                }
            }
        }

        void fpush(int kind, int args) {
            if (fn == fk.length) {
                fk = Arrays.copyOf(fk, fn * 2);
                fa = Arrays.copyOf(fa, fn * 2);
            }
            fk[fn] = kind;
            fa[fn++] = args;
        }

        int ftop() {
            return fn > 0 ? fa[fn - 1] : sa[restS - 1];
        }

        int ftopKind() {
            return fn > 0 ? fk[fn - 1] : restS > 0 ? sk[restS - 1] : -1;
        }

        int fpopFrame(int kind) {
            if (fn > 0) {
                if (fk[fn - 1] != kind) {
                    throw new IllegalStateException("unbalanced tag " + kind);
                }
                return fa[--fn];
            }
            if (restS == 0 || sk[restS - 1] != kind) {
                throw new IllegalStateException("unbalanced tag " + kind);
            }
            return sa[--restS];
        }

        /** 1 = fits, -1 = does not fit, 0 = keep measuring. */
        int fitsElement(El e) {
            int args = ftop();
            switch (e.k) {
                case E_SPACE:
                    return fitsText(" ", 1, args);
                case E_LINE:
                    if (argsMode(args) == FLAT) {
                        if (e.n == L_SOFT_OR_SPACE) {
                            return fitsText(" ", 1, args);
                        }
                        if (e.n == L_HARD || e.n == L_EMPTY) {
                            return mustBeFlat ? -1 : 1;
                        }
                    } else {
                        if (argsMeasure(args) == M_FIRST_LINE) {
                            return 1;
                        }
                        fLineWidth = 0;
                        fPendingIndent = argsIndent(args);
                    }
                    return 0;
                case E_TOKEN:
                case E_TEXT:
                    return fitsText(e.text, e.n, args);
                case E_SUFFIX_BOUNDARY:
                    return fHasSuffix ? -1 : 0;
                case E_EXPAND_PARENT:
                    return mustBeFlat ? -1 : 0;
                case E_BEST_FITTING: {
                    El[] slice;
                    int a = args;
                    if (argsMode(args) == FLAT) {
                        slice = e.variants[0];
                        a = withMeasure(args, e.n == 1 ? M_ALL_LINES : M_FIRST_LINE);
                    } else {
                        slice = e.variants[e.variants.length - 1];
                    }
                    fpush(E_BF_ENTRY, a);
                    fqpush(slice, 1);
                    return 0;
                }
                case E_INTERNED:
                    fqpush(e.content, 0);
                    return 0;
                case E_INDENT:
                    fpush(E_INDENT, withIndent(args, incrementIndent(argsIndent(args))));
                    return 0;
                case E_DEDENT:
                    fpush(E_DEDENT, withIndent(args, e.n == 1 ? 0 : decrementIndent(argsIndent(args))));
                    return 0;
                case E_ALIGN:
                    fpush(E_ALIGN, withIndent(args, setAlign(argsIndent(args), e.n)));
                    return 0;
                case E_GROUP:
                    return fitsGroup(E_GROUP, e.mode, e.id, args);
                case E_CGROUP: {
                    int expected = e.id == 0 ? argsMode(args) : getModeOr(e.id, argsMode(args));
                    if (expected == e.n) {
                        return fitsGroup(E_CGROUP, e.mode, 0, args);
                    }
                    fpush(E_CGROUP, args);
                    return 0;
                }
                case E_BFP:
                    if (e.id != 0) {
                        setMode(e.id, argsMode(args));
                    }
                    fpush(E_BFP, args);
                    return 0;
                case E_BFP + 1:
                    if (argsMode(args) == EXPANDED && ftopKind() == E_INDENT) {
                        fpopFrame(E_INDENT);
                        int unindented = fpopFrame(E_BFP);
                        fLineWidth = 0;
                        fPendingIndent = argsIndent(unindented);
                        return fitsText(")", 1, unindented);
                    }
                    fpopFrame(E_BFP);
                    return 0;
                case E_COND: {
                    int mode = e.id == 0 ? argsMode(args) : getModeOr(e.id, argsMode(args));
                    if (e.mode == mode) {
                        fpush(E_COND, args);
                    } else {
                        fskip(E_COND);
                    }
                    return 0;
                }
                case E_INDENT_IF_BREAKS: {
                    int mode = getModeOr(e.id, argsMode(args));
                    fpush(E_INDENT_IF_BREAKS, mode == FLAT ? args : withIndent(args, incrementIndent(argsIndent(args))));
                    return 0;
                }
                case E_SUFFIX:
                    if (e.n > 0) {
                        fLineWidth += e.n;
                        if (fLineWidth > LINE_WIDTH) {
                            return -1;
                        }
                    }
                    fskip(E_SUFFIX);
                    fHasSuffix = true;
                    return 0;
                case E_SUFFIX + 1:
                    throw new IllegalStateException("unexpected line suffix end");
                case E_FITS_EXPANDED:
                    if (argsMode(args) == EXPANDED) {
                        fpush(E_FITS_EXPANDED, args);
                    } else {
                        boolean met = true;
                        if (e.mode >= 0) {
                            int mode = e.id == 0 ? argsMode(args) : getModeOr(e.id, argsMode(args));
                            met = e.mode == mode;
                        }
                        if (met) {
                            fpush(E_FITS_EXPANDED, withMode(withMeasure(args, M_OVERFLOW), EXPANDED));
                        } else {
                            if (e.propagate) {
                                return -1;
                            }
                            fpush(E_FITS_EXPANDED, args);
                        }
                    }
                    return 0;
                case E_VERBATIM:
                case E_ENTRY:
                case E_BF_ENTRY:
                    fpush(e.k, args);
                    return 0;
                default:
                    if (e.k >= E_GROUP && (e.k & 1) == 1) {
                        fpopFrame(e.k - 1);
                        return 0;
                    }
                    throw new IllegalStateException("unexpected element " + e.k);
            }
        }

        int fitsGroup(int kind, int groupMode, int id, int args) {
            if (mustBeFlat && groupMode != G_FLAT) {
                return -1;
            }
            int mode = groupMode == G_FLAT ? argsMode(args) : EXPANDED;
            fpush(kind, withMode(args, mode));
            if (id != 0) {
                setMode(id, mode);
            }
            return 0;
        }

        int fitsText(String s, int width, int args) {
            int indent = fPendingIndent;
            fPendingIndent = 0;
            fLineWidth += indentLevel(indent) * INDENT_WIDTH + indentAlign(indent);
            if (width >= 0) {
                fLineWidth += width;
            } else {
                for (int i = 0; i < s.length(); i++) {
                    int cp = s.codePointAt(i);
                    if (cp > 0xFFFF) {
                        i++;
                    }
                    if (cp == '\t') {
                        fLineWidth += INDENT_WIDTH;
                    } else if (cp == '\n') {
                        if (mustBeFlat) {
                            return -1;
                        }
                        if (argsMeasure(args) == M_FIRST_LINE) {
                            return fLineWidth > LINE_WIDTH ? -1 : 1;
                        }
                        fLineWidth = 0;
                    } else {
                        fLineWidth += charWidth(cp);
                    }
                }
            }
            if (fLineWidth > LINE_WIDTH && argsMeasure(args) != M_OVERFLOW) {
                return -1;
            }
            return 0;
        }
    }

    /** Display width of a code point the way the unicode-width crate measures it (non-CJK). */
    static int charWidth(int cp) {
        if (cp < 0x7F) {
            return cp >= 0x20 ? 1 : 0;
        }
        if (cp < 0xA0) {
            return 0;
        }
        if (inRanges(ZERO_WIDTH, cp)) {
            return 0;
        }
        return inRanges(WIDE, cp) ? 2 : 1;
    }

    private static boolean inRanges(int[] ranges, int cp) {
        int lo = 0, hi = ranges.length / 2 - 1;
        while (lo <= hi) {
            int mid = (lo + hi) >>> 1;
            if (cp < ranges[2 * mid]) {
                hi = mid - 1;
            } else if (cp > ranges[2 * mid + 1]) {
                lo = mid + 1;
            } else {
                return true;
            }
        }
        return false;
    }

    private static final int[] ZERO_WIDTH = {
            0x300, 0x36F, 0x483, 0x489, 0x591, 0x5BD, 0x5BF, 0x5BF, 0x5C1, 0x5C2, 0x5C4, 0x5C5,
            0x5C7, 0x5C7, 0x600, 0x605, 0x610, 0x61A, 0x61C, 0x61C, 0x64B, 0x65F, 0x670, 0x670,
            0x6D6, 0x6DD, 0x6DF, 0x6E4, 0x6E7, 0x6E8, 0x6EA, 0x6ED, 0x70F, 0x70F, 0x711, 0x711,
            0x730, 0x74A, 0x7A6, 0x7B0, 0x7EB, 0x7F3, 0x7FD, 0x7FD, 0x816, 0x819, 0x81B, 0x823,
            0x825, 0x827, 0x829, 0x82D, 0x859, 0x85B, 0x890, 0x891, 0x897, 0x89F, 0x8CA, 0x902,
            0x93A, 0x93A, 0x93C, 0x93C, 0x941, 0x948, 0x94D, 0x94D, 0x951, 0x957, 0x962, 0x963,
            0x981, 0x981, 0x9BC, 0x9BC, 0x9C1, 0x9C4, 0x9CD, 0x9CD, 0x9E2, 0x9E3, 0x9FE, 0x9FE,
            0xA01, 0xA02, 0xA3C, 0xA3C, 0xA41, 0xA42, 0xA47, 0xA48, 0xA4B, 0xA4D, 0xA51, 0xA51,
            0xA70, 0xA71, 0xA75, 0xA75, 0xA81, 0xA82, 0xABC, 0xABC, 0xAC1, 0xAC5, 0xAC7, 0xAC8,
            0xACD, 0xACD, 0xAE2, 0xAE3, 0xAFA, 0xAFF, 0xB01, 0xB01, 0xB3C, 0xB3C, 0xB3F, 0xB3F,
            0xB41, 0xB44, 0xB4D, 0xB4D, 0xB55, 0xB56, 0xB62, 0xB63, 0xB82, 0xB82, 0xBC0, 0xBC0,
            0xBCD, 0xBCD, 0xC00, 0xC00, 0xC04, 0xC04, 0xC3C, 0xC3C, 0xC3E, 0xC40, 0xC46, 0xC48,
            0xC4A, 0xC4D, 0xC55, 0xC56, 0xC62, 0xC63, 0xC81, 0xC81, 0xCBC, 0xCBC, 0xCBF, 0xCBF,
            0xCC6, 0xCC6, 0xCCC, 0xCCD, 0xCE2, 0xCE3, 0xD00, 0xD01, 0xD3B, 0xD3C, 0xD41, 0xD44,
            0xD4D, 0xD4D, 0xD62, 0xD63, 0xD81, 0xD81, 0xDCA, 0xDCA, 0xDD2, 0xDD4, 0xDD6, 0xDD6,
            0xE31, 0xE31, 0xE34, 0xE3A, 0xE47, 0xE4E, 0xEB1, 0xEB1, 0xEB4, 0xEBC, 0xEC8, 0xECE,
            0xF18, 0xF19, 0xF35, 0xF35, 0xF37, 0xF37, 0xF39, 0xF39, 0xF71, 0xF7E, 0xF80, 0xF84,
            0xF86, 0xF87, 0xF8D, 0xF97, 0xF99, 0xFBC, 0xFC6, 0xFC6, 0x102D, 0x1030, 0x1032,
            0x1037, 0x1039, 0x103A, 0x103D, 0x103E, 0x1058, 0x1059, 0x105E, 0x1060, 0x1071,
            0x1074, 0x1082, 0x1082, 0x1085, 0x1086, 0x108D, 0x108D, 0x109D, 0x109D, 0x1160,
            0x11FF, 0x135D, 0x135F, 0x1712, 0x1714, 0x1732, 0x1733, 0x1752, 0x1753, 0x1772,
            0x1773, 0x17B4, 0x17B5, 0x17B7, 0x17BD, 0x17C6, 0x17C6, 0x17C9, 0x17D3, 0x17DD,
            0x17DD, 0x180B, 0x180F, 0x1885, 0x1886, 0x18A9, 0x18A9, 0x1920, 0x1922, 0x1927,
            0x1928, 0x1932, 0x1932, 0x1939, 0x193B, 0x1A17, 0x1A18, 0x1A1B, 0x1A1B, 0x1A56,
            0x1A56, 0x1A58, 0x1A5E, 0x1A60, 0x1A60, 0x1A62, 0x1A62, 0x1A65, 0x1A6C, 0x1A73,
            0x1A7C, 0x1A7F, 0x1A7F, 0x1AB0, 0x1ACE, 0x1B00, 0x1B03, 0x1B34, 0x1B34, 0x1B36,
            0x1B3A, 0x1B3C, 0x1B3C, 0x1B42, 0x1B42, 0x1B6B, 0x1B73, 0x1B80, 0x1B81, 0x1BA2,
            0x1BA5, 0x1BA8, 0x1BA9, 0x1BAB, 0x1BAD, 0x1BE6, 0x1BE6, 0x1BE8, 0x1BE9, 0x1BED,
            0x1BED, 0x1BEF, 0x1BF1, 0x1C2C, 0x1C33, 0x1C36, 0x1C37, 0x1CD0, 0x1CD2, 0x1CD4,
            0x1CE0, 0x1CE2, 0x1CE8, 0x1CED, 0x1CED, 0x1CF4, 0x1CF4, 0x1CF8, 0x1CF9, 0x1DC0,
            0x1DFF, 0x200B, 0x200F, 0x202A, 0x202E, 0x2060, 0x2064, 0x2066, 0x206F, 0x20D0,
            0x20F0, 0x2CEF, 0x2CF1, 0x2D7F, 0x2D7F, 0x2DE0, 0x2DFF, 0x302A, 0x302D, 0x3099,
            0x309A, 0xA66F, 0xA672, 0xA674, 0xA67D, 0xA69E, 0xA69F, 0xA6F0, 0xA6F1, 0xA802,
            0xA802, 0xA806, 0xA806, 0xA80B, 0xA80B, 0xA825, 0xA826, 0xA82C, 0xA82C, 0xA8C4,
            0xA8C5, 0xA8E0, 0xA8F1, 0xA8FF, 0xA8FF, 0xA926, 0xA92D, 0xA947, 0xA951, 0xA980,
            0xA982, 0xA9B3, 0xA9B3, 0xA9B6, 0xA9B9, 0xA9BC, 0xA9BD, 0xA9E5, 0xA9E5, 0xAA29,
            0xAA2E, 0xAA31, 0xAA32, 0xAA35, 0xAA36, 0xAA43, 0xAA43, 0xAA4C, 0xAA4C, 0xAA7C,
            0xAA7C, 0xAAB0, 0xAAB0, 0xAAB2, 0xAAB4, 0xAAB7, 0xAAB8, 0xAABE, 0xAABF, 0xAAC1,
            0xAAC1, 0xAAEC, 0xAAED, 0xAAF6, 0xAAF6, 0xABE5, 0xABE5, 0xABE8, 0xABE8, 0xABED,
            0xABED, 0xFB1E, 0xFB1E, 0xFE00, 0xFE0F, 0xFE20, 0xFE2F, 0xFEFF, 0xFEFF, 0xFFF9,
            0xFFFB, 0x101FD, 0x101FD, 0x102E0, 0x102E0, 0x10376, 0x1037A, 0x10A01, 0x10A03,
            0x10A05, 0x10A06, 0x10A0C, 0x10A0F, 0x10A38, 0x10A3A, 0x10A3F, 0x10A3F, 0x10AE5,
            0x10AE6, 0x10D24, 0x10D27, 0x10D69, 0x10D6D, 0x10EAB, 0x10EAC, 0x10EFC, 0x10EFF,
            0x10F46, 0x10F50, 0x10F82, 0x10F85, 0x11001, 0x11001, 0x11038, 0x11046, 0x11070,
            0x11070, 0x11073, 0x11074, 0x1107F, 0x11081, 0x110B3, 0x110B6, 0x110B9, 0x110BA,
            0x110BD, 0x110BD, 0x110C2, 0x110C2, 0x110CD, 0x110CD, 0x11100, 0x11102, 0x11127,
            0x1112B, 0x1112D, 0x11134, 0x11173, 0x11173, 0x11180, 0x11181, 0x111B6, 0x111BE,
            0x111C9, 0x111CC, 0x111CF, 0x111CF, 0x1122F, 0x11231, 0x11234, 0x11234, 0x11236,
            0x11237, 0x1123E, 0x1123E, 0x11241, 0x11241, 0x112DF, 0x112DF, 0x112E3, 0x112EA,
            0x11300, 0x11301, 0x1133B, 0x1133C, 0x11340, 0x11340, 0x11366, 0x1136C, 0x11370,
            0x11374, 0x113BB, 0x113C0, 0x113CE, 0x113CE, 0x113D0, 0x113D0, 0x113D2, 0x113D2,
            0x113E1, 0x113E2, 0x11438, 0x1143F, 0x11442, 0x11444, 0x11446, 0x11446, 0x1145E,
            0x1145E, 0x114B3, 0x114B8, 0x114BA, 0x114BA, 0x114BF, 0x114C0, 0x114C2, 0x114C3,
            0x115B2, 0x115B5, 0x115BC, 0x115BD, 0x115BF, 0x115C0, 0x115DC, 0x115DD, 0x11633,
            0x1163A, 0x1163D, 0x1163D, 0x1163F, 0x11640, 0x116AB, 0x116AB, 0x116AD, 0x116AD,
            0x116B0, 0x116B5, 0x116B7, 0x116B7, 0x1171D, 0x1171D, 0x1171F, 0x1171F, 0x11722,
            0x11725, 0x11727, 0x1172B, 0x1182F, 0x11837, 0x11839, 0x1183A, 0x1193B, 0x1193C,
            0x1193E, 0x1193E, 0x11943, 0x11943, 0x119D4, 0x119D7, 0x119DA, 0x119DB, 0x119E0,
            0x119E0, 0x11A01, 0x11A0A, 0x11A33, 0x11A38, 0x11A3B, 0x11A3E, 0x11A47, 0x11A47,
            0x11A51, 0x11A56, 0x11A59, 0x11A5B, 0x11A8A, 0x11A96, 0x11A98, 0x11A99, 0x11C30,
            0x11C36, 0x11C38, 0x11C3D, 0x11C3F, 0x11C3F, 0x11C92, 0x11CA7, 0x11CAA, 0x11CB0,
            0x11CB2, 0x11CB3, 0x11CB5, 0x11CB6, 0x11D31, 0x11D36, 0x11D3A, 0x11D3A, 0x11D3C,
            0x11D3D, 0x11D3F, 0x11D45, 0x11D47, 0x11D47, 0x11D90, 0x11D91, 0x11D95, 0x11D95,
            0x11D97, 0x11D97, 0x11EF3, 0x11EF4, 0x11F00, 0x11F01, 0x11F36, 0x11F3A, 0x11F40,
            0x11F40, 0x11F42, 0x11F42, 0x11F5A, 0x11F5A, 0x13430, 0x13440, 0x13447, 0x13455,
            0x1611E, 0x16129, 0x1612D, 0x1612F, 0x16AF0, 0x16AF4, 0x16B30, 0x16B36, 0x16F4F,
            0x16F4F, 0x16F8F, 0x16F92, 0x16FE4, 0x16FE4, 0x1BC9D, 0x1BC9E, 0x1BCA0, 0x1BCA3,
            0x1CF00, 0x1CF2D, 0x1CF30, 0x1CF46, 0x1D167, 0x1D169, 0x1D173, 0x1D182, 0x1D185,
            0x1D18B, 0x1D1AA, 0x1D1AD, 0x1D242, 0x1D244, 0x1DA00, 0x1DA36, 0x1DA3B, 0x1DA6C,
            0x1DA75, 0x1DA75, 0x1DA84, 0x1DA84, 0x1DA9B, 0x1DA9F, 0x1DAA1, 0x1DAAF, 0x1E000,
            0x1E006, 0x1E008, 0x1E018, 0x1E01B, 0x1E021, 0x1E023, 0x1E024, 0x1E026, 0x1E02A,
            0x1E08F, 0x1E08F, 0x1E130, 0x1E136, 0x1E2AE, 0x1E2AE, 0x1E2EC, 0x1E2EF, 0x1E4EC,
            0x1E4EF, 0x1E5EE, 0x1E5EF, 0x1E8D0, 0x1E8D6, 0x1E944, 0x1E94A, 0xE0000, 0xE0FFF
    };

    private static final int[] WIDE = {
            0x1100, 0x115F, 0x231A, 0x231B, 0x2329, 0x232A, 0x23E9, 0x23EC, 0x23F0, 0x23F0,
            0x23F3, 0x23F3, 0x25FD, 0x25FE, 0x2614, 0x2615, 0x2630, 0x2637, 0x2648, 0x2653,
            0x267F, 0x267F, 0x268A, 0x268F, 0x2693, 0x2693, 0x26A1, 0x26A1, 0x26AA, 0x26AB,
            0x26BD, 0x26BE, 0x26C4, 0x26C5, 0x26CE, 0x26CE, 0x26D4, 0x26D4, 0x26EA, 0x26EA,
            0x26F2, 0x26F3, 0x26F5, 0x26F5, 0x26FA, 0x26FA, 0x26FD, 0x26FD, 0x2705, 0x2705,
            0x270A, 0x270B, 0x2728, 0x2728, 0x274C, 0x274C, 0x274E, 0x274E, 0x2753, 0x2755,
            0x2757, 0x2757, 0x2795, 0x2797, 0x27B0, 0x27B0, 0x27BF, 0x27BF, 0x2B1B, 0x2B1C,
            0x2B50, 0x2B50, 0x2B55, 0x2B55, 0x2E80, 0x2E99, 0x2E9B, 0x2EF3, 0x2F00, 0x2FD5,
            0x2FF0, 0x3029, 0x302E, 0x303E, 0x3041, 0x3096, 0x309B, 0x30FF, 0x3105, 0x312F,
            0x3131, 0x318E, 0x3190, 0x31E5, 0x31EF, 0x321E, 0x3220, 0x3247, 0x3250, 0xA48C,
            0xA490, 0xA4C6, 0xA960, 0xA97C, 0xAC00, 0xD7A3, 0xF900, 0xFAFF, 0xFE10, 0xFE19,
            0xFE30, 0xFE52, 0xFE54, 0xFE66, 0xFE68, 0xFE6B, 0xFF01, 0xFF60, 0xFFE0, 0xFFE6,
            0x16FE0, 0x16FE3, 0x16FF0, 0x16FF1, 0x17000, 0x187F7, 0x18800, 0x18CD5, 0x18CFF,
            0x18D08, 0x1AFF0, 0x1AFF3, 0x1AFF5, 0x1AFFB, 0x1AFFD, 0x1AFFE, 0x1B000, 0x1B122,
            0x1B132, 0x1B132, 0x1B150, 0x1B152, 0x1B155, 0x1B155, 0x1B164, 0x1B167, 0x1B170,
            0x1B2FB, 0x1D300, 0x1D356, 0x1D360, 0x1D376, 0x1F004, 0x1F004, 0x1F0CF, 0x1F0CF,
            0x1F18E, 0x1F18E, 0x1F191, 0x1F19A, 0x1F200, 0x1F202, 0x1F210, 0x1F23B, 0x1F240,
            0x1F248, 0x1F250, 0x1F251, 0x1F260, 0x1F265, 0x1F300, 0x1F320, 0x1F32D, 0x1F335,
            0x1F337, 0x1F37C, 0x1F37E, 0x1F393, 0x1F3A0, 0x1F3CA, 0x1F3CF, 0x1F3D3, 0x1F3E0,
            0x1F3F0, 0x1F3F4, 0x1F3F4, 0x1F3F8, 0x1F43E, 0x1F440, 0x1F440, 0x1F442, 0x1F4FC,
            0x1F4FF, 0x1F53D, 0x1F54B, 0x1F54E, 0x1F550, 0x1F567, 0x1F57A, 0x1F57A, 0x1F595,
            0x1F596, 0x1F5A4, 0x1F5A4, 0x1F5FB, 0x1F64F, 0x1F680, 0x1F6C5, 0x1F6CC, 0x1F6CC,
            0x1F6D0, 0x1F6D2, 0x1F6D5, 0x1F6D7, 0x1F6DC, 0x1F6DF, 0x1F6EB, 0x1F6EC, 0x1F6F4,
            0x1F6FC, 0x1F7E0, 0x1F7EB, 0x1F7F0, 0x1F7F0, 0x1F90C, 0x1F93A, 0x1F93C, 0x1F945,
            0x1F947, 0x1F9FF, 0x1FA70, 0x1FA7C, 0x1FA80, 0x1FA89, 0x1FA8F, 0x1FAC6, 0x1FACE,
            0x1FADC, 0x1FADF, 0x1FAE9, 0x1FAF0, 0x1FAF8, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD
    };

    // =====================================================================================
    // Source access: tokens and trivia (ruff_python_trivia equivalents over the lexer tokens)
    // =====================================================================================

    /** The source text with its tokens; answers the token and trivia questions ruff asks. */
    static final class Src {
        final String s;
        final Tok[] toks;
        final int[] starts;
        final int[] match;
        final Tok[] comments;
        /** For every source offset, the index of the first token starting at or after it. */
        final int[] firstAt;

        Src(String s, List<Tok> tokens) {
            this.s = s;
            this.toks = tokens.toArray(new Tok[0]);
            this.starts = new int[toks.length];
            this.match = new int[toks.length];
            this.firstAt = new int[s.length() + 1];
            ArrayList<Tok> cs = new ArrayList<>();
            int[] stack = new int[16];
            int sp = 0;
            for (int i = 0; i < toks.length; i++) {
                Tok t = toks[i];
                starts[i] = t.start;
                match[i] = -1;
                if (t.kind == T_COMMENT) {
                    cs.add(t);
                } else if (t.kind == T_OP) {
                    String x = t.text;
                    if (x.equals("(") || x.equals("[") || x.equals("{")) {
                        if (sp == stack.length) {
                            stack = Arrays.copyOf(stack, sp * 2);
                        }
                        stack[sp++] = i;
                    } else if ((x.equals(")") || x.equals("]") || x.equals("}")) && sp > 0) {
                        int o = stack[--sp];
                        match[o] = i;
                        match[i] = o;
                    }
                }
            }
            comments = cs.toArray(new Tok[0]);
            int t = 0;
            for (int offset = 0; offset < firstAt.length; offset++) {
                while (t < starts.length && starts[t] < offset) {
                    t++;
                }
                firstAt[offset] = t;
            }
        }

        static boolean trivia(Tok t) {
            int k = t.kind;
            return k == T_NL || k == T_COMMENT || k == T_NEWLINE || k == T_INDENT || k == T_DEDENT;
        }

        /** Index of the first token starting at or after {@code offset}. */
        int at(int offset) {
            return offset <= 0 ? 0 : offset < firstAt.length ? firstAt[offset] : toks.length;
        }

        /** Index of the first non-trivia token starting at or after {@code offset}, or -1. */
        int nextIdx(int offset) {
            for (int i = at(offset); i < toks.length; i++) {
                Tok t = toks[i];
                if (t.kind == T_END) {
                    return -1;
                }
                if (!trivia(t)) {
                    return i;
                }
            }
            return -1;
        }

        /** The first non-trivia token starting at or after {@code offset}, or null. */
        Tok next(int offset) {
            int i = nextIdx(offset);
            return i < 0 ? null : toks[i];
        }

        /** Index of the last non-trivia token ending at or before {@code offset}, or -1. */
        int prevIdx(int offset) {
            for (int i = at(offset) - 1; i >= 0; i--) {
                Tok t = toks[i];
                if (t.end <= offset && !trivia(t) && t.kind != T_END) {
                    return i;
                }
            }
            return -1;
        }

        Tok prev(int offset) {
            int i = prevIdx(offset);
            return i < 0 ? null : toks[i];
        }

        /** The first non-trivia token inside {@code [a, b)}, or null. */
        Tok first(int a, int b) {
            Tok t = next(a);
            return t != null && t.end <= b ? t : null;
        }

        /** The first non-trivia token inside {@code [a, b)} after skipping closing parentheses, or null. */
        Tok firstAfterParens(int a, int b) {
            for (int i = at(a); i < toks.length; i++) {
                Tok t = toks[i];
                if (t.kind == T_END || t.end > b) {
                    return null;
                }
                if (trivia(t) || t.is(")")) {
                    continue;
                }
                return t;
            }
            return null;
        }

        /** Whether any non-trivia token inside {@code [a, b)} is the operator {@code op}. */
        boolean any(int a, int b, String op) {
            for (int i = at(a); i < toks.length; i++) {
                Tok t = toks[i];
                if (t.kind == T_END || t.end > b) {
                    return false;
                }
                if (!trivia(t) && t.is(op)) {
                    return true;
                }
            }
            return false;
        }

        /** Whether any non-trivia token lies inside {@code [a, b)}. */
        boolean anyToken(int a, int b) {
            return first(a, b) != null;
        }

        boolean lineBreak(int a, int b) {
            for (int i = a; i < b; i++) {
                char c = s.charAt(i);
                if (c == '\n' || c == '\r') {
                    return true;
                }
            }
            return false;
        }

        int lineStart(int offset) {
            int i = offset;
            while (i > 0) {
                char c = s.charAt(i - 1);
                if (c == '\n' || c == '\r') {
                    break;
                }
                i--;
            }
            return i;
        }

        int lineEnd(int offset) {
            int i = offset;
            int n = s.length();
            while (i < n) {
                char c = s.charAt(i);
                if (c == '\n' || c == '\r') {
                    break;
                }
                i++;
            }
            return i;
        }

        /** The end of the line containing {@code offset}, including its line break. */
        int fullLineEnd(int offset) {
            int i = lineEnd(offset);
            if (i < s.length()) {
                if (s.charAt(i) == '\r' && i + 1 < s.length() && s.charAt(i + 1) == '\n') {
                    return i + 2;
                }
                return i + 1;
            }
            return i;
        }

        /** Width of the whitespace before {@code offset} on its line, or -1 when other text precedes it. */
        int indentation(int offset) {
            int ls = lineStart(offset);
            for (int i = ls; i < offset; i++) {
                char c = s.charAt(i);
                if (c != ' ' && c != '\t' && c != '\f') {
                    return -1;
                }
            }
            return offset - ls;
        }

        /** Whether only whitespace precedes {@code offset} on its line. */
        boolean ownLine(int offset) {
            for (int i = offset - 1; i >= 0; i--) {
                char c = s.charAt(i);
                if (c == '\n' || c == '\r') {
                    return true;
                }
                if (c != ' ' && c != '\t' && c != '\f') {
                    return false;
                }
            }
            return true;
        }

        /** Whether the expression sits directly inside a matching pair of parentheses. */
        boolean parenthesized(Node e) {
            int a = prevIdx(e.start);
            if (a < 0 || !toks[a].is("(")) {
                return false;
            }
            int b = nextIdx(e.end);
            return b >= 0 && match[a] == b;
        }

        /** Number of empty lines in {@code [a, b)} before the first code, counting comment runs separately. */
        int maxEmptyLines(int a, int b) {
            int newlines = 0;
            int max = 0;
            int i = a;
            while (i < b) {
                char c = s.charAt(i);
                if (c == '\n') {
                    newlines++;
                    i++;
                } else if (c == '\r') {
                    newlines++;
                    i += i + 1 < b && s.charAt(i + 1) == '\n' ? 2 : 1;
                } else if (c == ' ' || c == '\t' || c == '\f') {
                    i++;
                } else if (c == '#') {
                    max = Math.max(max, newlines);
                    newlines = 0;
                    while (i < b && s.charAt(i) != '\n' && s.charAt(i) != '\r') {
                        i++;
                    }
                } else {
                    break;
                }
            }
            max = Math.max(max, newlines);
            return Math.max(0, max - 1);
        }
    }

    // =====================================================================================
    // Comments: extraction and placement (ruff_python_formatter/src/comments)
    // =====================================================================================

    /** A source comment; {@code formatted} marks it as written. */
    static final class Comment {
        final int start;
        final int end;
        final boolean ownLine;
        boolean formatted;

        Comment(int start, int end, boolean ownLine) {
            this.start = start;
            this.end = end;
            this.ownLine = ownLine;
        }
    }

    static final List<Comment> NO_COMMENTS = new ArrayList<>(0);

    /** Leading, dangling and trailing comments of one node, in source order. */
    static final class NodeComments {
        List<Comment> leading = NO_COMMENTS;
        List<Comment> dangling = NO_COMMENTS;
        List<Comment> trailing = NO_COMMENTS;
    }

    static final int PL_DEFAULT = 0, PL_LEADING = 1, PL_TRAILING = 2, PL_DANGLING = 3;

    /** A comment with the nodes around it, before placement decides its owner. */
    static final class DComment {
        final int start;
        final int end;
        final boolean ownLine;
        final Node enclosing;
        final Node preceding;
        final Node following;
        final Node parent;
        int kind;
        Node node;

        DComment(int start, int end, boolean ownLine, Node enclosing, Node preceding, Node following, Node parent) {
            this.start = start;
            this.end = end;
            this.ownLine = ownLine;
            this.enclosing = enclosing;
            this.preceding = preceding;
            this.following = following;
            this.parent = parent;
        }
    }

    /** Walks the tree in source order and attaches every comment to a node, as ruff does. */
    static final class CommentBuilder implements Visit {
        final Src src;
        final Tok[] cs;
        int ci;
        final ArrayList<Node> parents = new ArrayList<>();
        Node preceding;

        CommentBuilder(Src src) {
            this.src = src;
            this.cs = src.comments;
        }

        @Override
        public void visit(Node node) {
            if (enter(node)) {
                node.each(this);
            }
            leave(node);
        }

        boolean enter(Node node) {
            Node enclosing = parents.isEmpty() ? node : parents.get(parents.size() - 1);
            while (ci < cs.length) {
                Tok c = cs[ci];
                if (c.end > node.start) {
                    break;
                }
                push(new DComment(c.start, c.end, src.ownLine(c.start), enclosing, preceding, node,
                        parents.size() >= 2 ? parents.get(parents.size() - 2) : null));
                ci++;
            }
            preceding = null;
            parents.add(node);
            return ci < cs.length && cs[ci].start < node.end;
        }

        void leave(Node node) {
            parents.remove(parents.size() - 1);
            boolean root = parents.isEmpty();
            while (ci < cs.length) {
                Tok c = cs[ci];
                if (c.start >= node.end && !root) {
                    break;
                }
                push(new DComment(c.start, c.end, src.ownLine(c.start), node, preceding, null,
                        parents.isEmpty() ? null : parents.get(parents.size() - 1)));
                ci++;
            }
            preceding = node;
        }

        NodeComments of(Node n) {
            if (n.comments == null) {
                n.comments = new NodeComments();
            }
            return n.comments;
        }

        void push(DComment c) {
            new Placement(src).place(c);
            Comment comment = new Comment(c.start, c.end, c.ownLine);
            int kind = c.kind;
            Node node = c.node;
            if (kind == PL_DEFAULT) {
                if (!c.ownLine) {
                    if (c.preceding != null) {
                        kind = PL_TRAILING;
                        node = c.preceding;
                    } else if (c.following != null) {
                        kind = PL_LEADING;
                        node = c.following;
                    } else {
                        kind = PL_DANGLING;
                        node = c.enclosing;
                    }
                } else if (c.following != null) {
                    kind = PL_LEADING;
                    node = c.following;
                } else if (c.preceding != null) {
                    kind = PL_TRAILING;
                    node = c.preceding;
                } else {
                    kind = PL_DANGLING;
                    node = c.enclosing;
                }
            }
            NodeComments nc = of(node);
            if (kind == PL_LEADING) {
                if (nc.leading == NO_COMMENTS) {
                    nc.leading = new ArrayList<>(2);
                }
                nc.leading.add(comment);
            } else if (kind == PL_TRAILING) {
                if (nc.trailing == NO_COMMENTS) {
                    nc.trailing = new ArrayList<>(2);
                }
                nc.trailing.add(comment);
            } else {
                if (nc.dangling == NO_COMMENTS) {
                    nc.dangling = new ArrayList<>(2);
                }
                nc.dangling.add(comment);
            }
        }
    }

    static boolean empty(List<?> l) {
        return l == null || l.isEmpty();
    }

    static <T> T last(List<T> l) {
        return empty(l) ? null : l.get(l.size() - 1);
    }

    static <T> T first(List<T> l) {
        return empty(l) ? null : l.get(0);
    }

    /** The last child of a compound statement's last body, or null. */
    static Node lastChildInBody(Node node) {
        List<? extends Node> body;
        if (node instanceof FunctionDef) {
            body = ((FunctionDef) node).body;
        } else if (node instanceof ClassDef) {
            body = ((ClassDef) node).body;
        } else if (node instanceof With) {
            body = ((With) node).body;
        } else if (node instanceof MatchCase) {
            body = ((MatchCase) node).body;
        } else if (node instanceof ExceptHandler) {
            body = ((ExceptHandler) node).body;
        } else if (node instanceof ElifElse) {
            body = ((ElifElse) node).body;
        } else if (node instanceof If) {
            If x = (If) node;
            body = empty(x.clauses) ? x.body : last(x.clauses).body;
        } else if (node instanceof For) {
            For x = (For) node;
            body = empty(x.orelse) ? x.body : x.orelse;
        } else if (node instanceof While) {
            While x = (While) node;
            body = empty(x.orelse) ? x.body : x.orelse;
        } else if (node instanceof Match) {
            return last(((Match) node).cases);
        } else if (node instanceof Try) {
            Try x = (Try) node;
            if (!empty(x.finalbody)) {
                body = x.finalbody;
            } else if (!empty(x.orelse)) {
                body = x.orelse;
            } else if (!empty(x.handlers)) {
                return last(x.handlers);
            } else {
                body = x.body;
            }
        } else {
            return null;
        }
        return last(body);
    }

    static boolean isFirstStatementInBody(Node node, Node body) {
        if (body instanceof For) {
            return node == first(((For) body).body) || node == first(((For) body).orelse);
        } else if (body instanceof While) {
            return node == first(((While) body).body) || node == first(((While) body).orelse);
        } else if (body instanceof Try) {
            Try t = (Try) body;
            return node == first(t.body) || node == first(t.orelse) || node == first(t.finalbody);
        } else if (body instanceof If) {
            return node == first(((If) body).body);
        } else if (body instanceof ElifElse) {
            return node == first(((ElifElse) body).body);
        } else if (body instanceof With) {
            return node == first(((With) body).body);
        } else if (body instanceof ExceptHandler) {
            return node == first(((ExceptHandler) body).body);
        } else if (body instanceof MatchCase) {
            return node == first(((MatchCase) body).body);
        } else if (body instanceof FunctionDef) {
            return node == first(((FunctionDef) body).body);
        } else if (body instanceof ClassDef) {
            return node == first(((ClassDef) body).body);
        } else if (body instanceof Match) {
            return node == first(((Match) body).cases);
        }
        return false;
    }

    static boolean isFirstStatementInAlternateBody(Node node, Node body) {
        if (body instanceof For) {
            return node == first(((For) body).orelse);
        } else if (body instanceof While) {
            return node == first(((While) body).orelse);
        } else if (body instanceof Try) {
            Try t = (Try) body;
            return node == first(t.handlers) || node == first(t.orelse) || node == first(t.finalbody);
        } else if (body instanceof If) {
            return node == first(((If) body).clauses);
        }
        return false;
    }

    static boolean isAlternativeBranchWithNode(Node node) {
        return node instanceof ExceptHandler || node instanceof ElifElse;
    }

    /** Ruff's comment placement rules ({@code comments/placement.rs}). */
    static final class Placement {
        final Src src;

        Placement(Src src) {
            this.src = src;
        }

        static boolean lead(DComment c, Node n) {
            c.kind = PL_LEADING;
            c.node = n;
            return true;
        }

        static boolean trail(DComment c, Node n) {
            c.kind = PL_TRAILING;
            c.node = n;
            return true;
        }

        static boolean dangle(DComment c, Node n) {
            c.kind = PL_DANGLING;
            c.node = n;
            return true;
        }

        void place(DComment c) {
            if (parenthesizedComment(c) || endOfLineAroundBody(c) || ownLineAroundBody(c) || enclosed(c)) {
                return;
            }
            c.kind = PL_DEFAULT;
        }

        boolean parenthesizedComment(DComment c) {
            if (c.enclosing instanceof Str && ((Str) c.enclosing).kind == S_FSTR) {
                return false;
            }
            if (c.preceding == null || c.following == null) {
                return false;
            }
            for (int i = src.at(c.preceding.end); i < src.toks.length; i++) {
                Tok t = src.toks[i];
                if (t.kind == T_END || t.start >= c.start) {
                    break;
                }
                if (Src.trivia(t)) {
                    continue;
                }
                if (t.is("as") || t.is("def") || t.is("class")) {
                    break;
                }
                if (t.is("(")) {
                    return lead(c, c.following);
                }
            }
            for (int i = src.at(c.end); i < src.toks.length; i++) {
                Tok t = src.toks[i];
                if (t.kind == T_END || t.end > c.following.start) {
                    break;
                }
                if (Src.trivia(t)) {
                    continue;
                }
                if (t.is("as") || t.is("def") || t.is("class")) {
                    break;
                }
                if (t.is(")")) {
                    return trail(c, c.preceding);
                }
            }
            return false;
        }

        boolean endOfLineAroundBody(DComment c) {
            if (c.ownLine) {
                return false;
            }
            if (c.following != null && isFirstStatementInBody(c.following, c.enclosing)
                    && !src.anyToken(c.end, c.following.start)) {
                return dangle(c, c.enclosing);
            }
            if (c.preceding != null) {
                Node lastChild = lastChildInBody(c.preceding);
                if (lastChild != null) {
                    Node inner = lastChild;
                    for (Node x = lastChildInBody(inner); x != null; x = lastChildInBody(inner)) {
                        inner = x;
                    }
                    return trail(c, inner);
                }
            }
            return false;
        }

        boolean ownLineAroundBody(DComment c) {
            if (!c.ownLine || c.preceding == null) {
                return false;
            }
            if (src.anyToken(c.preceding.end, c.start)) {
                return false;
            }
            return ownLineBetweenBranches(c, c.preceding) || ownLineAfterBranch(c, c.preceding)
                    || ownLineBetweenStatements(c);
        }

        boolean ownLineBetweenStatements(DComment c) {
            if (c.preceding == null || c.following == null) {
                return false;
            }
            if (!(c.preceding instanceof Stmt) || !(c.following instanceof Stmt) || !c.ownLine) {
                return false;
            }
            if (src.maxEmptyLines(c.end, c.following.start) == 0) {
                return lead(c, c.following);
            }
            return trail(c, c.preceding);
        }

        int commentIndentationAfter(Node preceding, DComment c) {
            int from = src.fullLineEnd(preceding.end);
            int min = -1;
            for (Tok t : src.comments) {
                if (t.start < from) {
                    continue;
                }
                if (t.end > c.end) {
                    break;
                }
                int ind = src.indentation(t.start);
                if (ind >= 0 && (min < 0 || ind < min)) {
                    min = ind;
                }
            }
            return Math.max(min, 0);
        }

        boolean ownLineBetweenBranches(DComment c, Node preceding) {
            Node following = c.following;
            if (following == null || !isFirstStatementInAlternateBody(following, c.enclosing)) {
                return false;
            }
            int commentIndent = commentIndentationAfter(preceding, c);
            int precedingIndent = src.indentation(preceding.start);
            if (precedingIndent < 0) {
                precedingIndent = commentIndent + 1;
            }
            if (commentIndent > precedingIndent) {
                return false;
            } else if (commentIndent == precedingIndent) {
                if (isAlternativeBranchWithNode(preceding)) {
                    return dangle(c, c.enclosing);
                }
                return trail(c, preceding);
            } else if (isAlternativeBranchWithNode(following)) {
                return lead(c, following);
            }
            return dangle(c, c.enclosing);
        }

        boolean ownLineAfterBranch(DComment c, Node preceding) {
            Node lastChild = lastChildInBody(preceding);
            if (lastChild == null) {
                if (c.following != null && isFirstStatementInAlternateBody(c.following, c.enclosing)) {
                    lastChild = preceding;
                } else {
                    return false;
                }
            }
            int commentIndent = commentIndentationAfter(preceding, c);
            int precedingIndent = Math.max(0, src.indentation(preceding.start));
            if (commentIndent == precedingIndent) {
                return false;
            }
            Node parent = null;
            Node lastInParent = lastChild;
            while (true) {
                int childIndent = Math.max(0, src.indentation(lastInParent.start));
                if (commentIndent < childIndent) {
                    return parent != null && trail(c, parent);
                } else if (commentIndent == childIndent) {
                    return trail(c, lastInParent);
                }
                Node nested = lastChildInBody(lastInParent);
                if (nested == null) {
                    return trail(c, lastInParent);
                }
                parent = lastInParent;
                lastInParent = nested;
            }
        }

        boolean enclosed(DComment c) {
            Node e = c.enclosing;
            if (e instanceof Parameters) {
                Parameters ps = (Parameters) e;
                if (parametersSeparator(c, ps)) {
                    return true;
                }
                return src.s.startsWith("(", ps.start) && bracketedEndOfLine(c);
            } else if (e instanceof Param) {
                return parameter(c, (Param) e);
            } else if (e instanceof Arguments || e instanceof TypeParams || e instanceof PArguments) {
                return bracketedEndOfLine(c);
            } else if (e instanceof Comprehension) {
                return comprehension(c, (Comprehension) e);
            } else if (e instanceof Attribute) {
                return attribute(c, (Attribute) e);
            } else if (e instanceof BinOp) {
                return binaryLeftOrOperator(c, (BinOp) e);
            } else if (e instanceof BoolOp || e instanceof Compare) {
                return binaryLike(c);
            } else if (e instanceof Keyword) {
                Keyword k = (Keyword) e;
                int start = k.arg != null ? k.arg.end : k.start;
                return !src.any(start, c.start, "(") && lead(c, e);
            } else if (e instanceof PKeyword) {
                return !src.any(((PKeyword) e).attr.end, c.start, "(") && lead(c, e);
            } else if (e instanceof UnaryOp) {
                return unaryOp(c, (UnaryOp) e);
            } else if (e instanceof Named) {
                return namedExpr(c);
            } else if (e instanceof Lambda) {
                return lambda(c, (Lambda) e);
            } else if (e instanceof DictE) {
                return dictUnpacking(c) || bracketedEndOfLine(c) || keyValue(c);
            } else if (e instanceof Comp && ((Comp) e).kind == 'd') {
                return dictUnpacking(c) || keyValue(c) || bracketedEndOfLine(c);
            } else if (e instanceof IfExp) {
                return exprIf(c, (IfExp) e);
            } else if (e instanceof Slice) {
                return slice(c, (Slice) e);
            } else if (e instanceof Starred) {
                if (c.following != null && !src.any(e.start, c.start, "(")) {
                    return lead(c, e);
                }
                return false;
            } else if (e instanceof Subscript) {
                Subscript sub = (Subscript) e;
                if (sub.slice instanceof Slice) {
                    return slice(c, (Slice) sub.slice);
                }
                if (!c.ownLine && sub.value.end < c.start) {
                    int i = src.at(sub.value.end);
                    boolean found = false;
                    for (; i < src.toks.length; i++) {
                        Tok t = src.toks[i];
                        if (t.kind == T_END || t.end > c.start) {
                            break;
                        }
                        if (!Src.trivia(t) && t.is("[")) {
                            found = true;
                            i++;
                            break;
                        }
                    }
                    if (!found) {
                        return false;
                    }
                    for (; i < src.toks.length; i++) {
                        Tok t = src.toks[i];
                        if (t.kind == T_END || t.end > c.start) {
                            return dangle(c, e);
                        }
                        if (!Src.trivia(t)) {
                            return false;
                        }
                    }
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof Module) {
                return trailingModule(c, (Module) e) || moduleLevelBeforeClassOrFunction(c);
            } else if (e instanceof WithItem) {
                return withItem(c);
            } else if (e instanceof PSequence) {
                return sequencePatternParenthesized((PSequence) e) && bracketedEndOfLine(c);
            } else if (e instanceof PClass) {
                PClass cls = (PClass) e;
                if (cls.cls.end < c.start && c.end < cls.arguments.start) {
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof PAs) {
                return patternAs(c);
            } else if (e instanceof PStar) {
                return dangle(c, e);
            } else if (e instanceof PMapping) {
                return bracketedEndOfLine(c) || patternMapping(c, (PMapping) e);
            } else if (e instanceof FunctionDef) {
                if (c.ownLine && c.preceding instanceof Decorator
                        && (c.following instanceof Parameters || c.following instanceof TypeParams)) {
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof ClassDef) {
                ClassDef cd = (ClassDef) e;
                if (c.ownLine && c.start < cd.name.start && !empty(cd.decorators)
                        && last(cd.decorators).end < c.start) {
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof ImportFrom) {
                return importFrom(c, (ImportFrom) e);
            } else if (e instanceof Alias) {
                Alias a = (Alias) e;
                if (a.asname != null) {
                    Tok t = src.next(a.name.end);
                    if (t != null && t.is("as")) {
                        return c.start < t.start ? trail(c, a.name) : lead(c, a.asname);
                    }
                }
                return false;
            } else if (e instanceof With) {
                With w = (With) e;
                if (!c.ownLine && !empty(w.items) && w.start < c.start && c.start < w.items.get(0).start) {
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof Call) {
                if (c.ownLine && c.preceding != null && c.following != null && c.preceding.end < c.start
                        && c.end < c.following.start) {
                    return dangle(c, e);
                }
                return false;
            } else if (e instanceof Str && ((Str) e).kind == S_STR) {
                if (c.parent instanceof FPart) {
                    return dangle(c, c.parent);
                }
                return false;
            } else if (e instanceof FPart) {
                return dangle(c, e);
            } else if (e instanceof FInterp) {
                FInterp fi = (FInterp) e;
                if (c.preceding != null && c.ownLine && fi.spec != null && c.following != null) {
                    return trail(c, c.preceding);
                }
                return bracketedEndOfLine(c);
            } else if (e instanceof Seq) {
                Seq s = (Seq) e;
                if (s.kind != '(' || s.parenthesized) {
                    return bracketedEndOfLine(c);
                }
                return false;
            } else if (e instanceof Comp) {
                Comp s = (Comp) e;
                if (s.kind != '(' || s.parenthesized) {
                    return bracketedEndOfLine(c);
                }
                return false;
            } else if (e instanceof Return) {
                return implicitConcatenatedString(c);
            } else if (e instanceof Assign) {
                return c.preceding != null && c.preceding == ((Assign) e).value && implicitConcatenatedString(c);
            } else if (e instanceof AnnAssign) {
                return c.preceding != null && c.preceding == ((AnnAssign) e).value && implicitConcatenatedString(c);
            } else if (e instanceof AugAssign) {
                return c.preceding != null && c.preceding == ((AugAssign) e).value && implicitConcatenatedString(c);
            } else if (e instanceof TypeAlias) {
                return c.preceding != null && c.preceding == ((TypeAlias) e).value && implicitConcatenatedString(c);
            }
            return false;
        }

        boolean bracketedEndOfLine(DComment c) {
            if (c.ownLine) {
                return false;
            }
            int i = src.nextIdx(c.enclosing.start);
            if (i < 0 || src.toks[i].end > c.start) {
                return false;
            }
            for (i = i + 1; i < src.toks.length; i++) {
                Tok t = src.toks[i];
                if (t.kind == T_END || t.end > c.start) {
                    break;
                }
                if (!Src.trivia(t)) {
                    return false;
                }
            }
            return dangle(c, c.enclosing);
        }

        /** {slash, star} separators as {precedingEnd, start, end, followingStart}, or null. */
        static int[][] parameterSeparators(Src src, Parameters ps) {
            int[] slash = null;
            if (!empty(ps.posonly)) {
                int precedingEnd = last(ps.posonly).end;
                int i = src.nextIdx(precedingEnd);
                int j = src.nextIdx(src.toks[i].end);
                slash = new int[] {precedingEnd, src.toks[j].start, src.toks[j].end, 0};
            }
            int[] star = null;
            if (ps.vararg == null && !empty(ps.kwonly)) {
                int after = !empty(ps.args) ? last(ps.args).end : slash != null ? slash[2] : -1;
                if (after >= 0) {
                    int i = src.nextIdx(after);
                    int j = src.nextIdx(src.toks[i].end);
                    star = new int[] {after, src.toks[j].start, src.toks[j].end, ps.kwonly.get(0).start};
                } else {
                    int i = src.nextIdx(ps.start);
                    Tok t = src.toks[i];
                    if (t.is("(")) {
                        t = src.next(t.end);
                    }
                    star = new int[] {ps.start, t.start, t.end, ps.kwonly.get(0).start};
                }
            }
            if (slash != null) {
                int fs;
                if (!empty(ps.args)) {
                    fs = ps.args.get(0).start;
                } else if (ps.vararg != null) {
                    fs = ps.vararg.start;
                } else if (star != null) {
                    fs = star[1];
                } else if (ps.kwarg != null) {
                    fs = ps.kwarg.start;
                } else {
                    fs = ps.end;
                }
                slash[3] = fs;
            }
            return new int[][] {slash, star};
        }

        boolean parametersSeparator(DComment c, Parameters ps) {
            int[][] sep = parameterSeparators(src, ps);
            return separatorLocation(sep[0], sep[1], c.start, c.ownLine) != 0 && dangle(c, c.enclosing);
        }

        /** 1 slash leading, 2 slash trailing, 3 star leading, 4 star trailing, 0 none. */
        static int separatorLocation(int[] slash, int[] star, int start, boolean ownLine) {
            if (slash != null) {
                if (start > slash[0] && start < slash[1] && ownLine) {
                    return 1;
                }
                if (start > slash[2] && start < slash[3] && !ownLine) {
                    return 2;
                }
            }
            if (star != null) {
                if (start > star[0] && start < star[1] && ownLine) {
                    return 3;
                }
                if (start > star[2] && start < star[3] && !ownLine) {
                    return 4;
                }
            }
            return 0;
        }

        boolean parameter(DComment c, Param p) {
            if (p.annotation != null) {
                Tok colon = src.next(p.name.end);
                return c.start < colon.start && lead(c, p);
            } else if (c.start < p.name.start) {
                if (c.parent instanceof Parameters && c.parent.start == p.start) {
                    return lead(c, c.parent);
                }
                return lead(c, p);
            }
            return false;
        }

        boolean binaryLeftOrOperator(DComment c, BinOp b) {
            if (c.preceding == null || c.following == null) {
                return false;
            }
            Tok op = src.firstAfterParens(b.left.end, b.right.start);
            if (c.end < op.start) {
                return trail(c, b.left);
            } else if (!c.ownLine) {
                if (src.lineBreak(b.left.end, op.start) && src.lineBreak(op.start, b.right.start)) {
                    return dangle(c, b);
                }
            }
            return false;
        }

        boolean binaryLike(DComment c) {
            if (c.preceding == null || c.following == null) {
                return false;
            }
            Tok op = src.firstAfterParens(c.preceding.end, c.following.start);
            return c.end < op.start && trail(c, c.preceding);
        }

        boolean trailingModule(DComment c, Module m) {
            if (c.preceding == null && c.following == null) {
                if (!empty(m.body)) {
                    return trail(c, last(m.body));
                }
                return lead(c, c.enclosing);
            }
            return false;
        }

        boolean moduleLevelBeforeClassOrFunction(DComment c) {
            if (!c.ownLine || c.preceding == null || c.following == null) {
                return false;
            }
            if (!(c.following instanceof FunctionDef) && !(c.following instanceof ClassDef)) {
                return false;
            }
            if (src.maxEmptyLines(c.end, c.following.start) == 0) {
                return lead(c, c.following);
            }
            return trail(c, c.preceding);
        }

        boolean slice(DComment c, Slice sl) {
            Tok before = src.prev(c.start);
            if (!c.ownLine && before != null && before.is("[")) {
                return dangle(c, c.enclosing);
            }
            int section = sliceSection(src, c.start, sl);
            Expr node = section == 0 ? sl.lower : section == 1 ? sl.upper : sl.step;
            if (node != null) {
                return c.start < node.start ? lead(c, node) : trail(c, node);
            }
            return dangle(c, sl);
        }

        boolean dictUnpacking(DComment c) {
            if (c.following == null) {
                return false;
            }
            int precedingEnd = c.preceding != null ? c.preceding.end : c.enclosing.start;
            return src.any(precedingEnd, c.start, "**") && lead(c, c.following);
        }

        boolean keyValue(DComment c) {
            if (c.following == null || c.preceding == null) {
                return false;
            }
            return src.any(c.preceding.end, c.following.start, ":") && dangle(c, c.enclosing);
        }

        boolean attribute(DComment c, Attribute a) {
            if (c.preceding == null) {
                return lead(c, a.value);
            }
            Tok rparen = null;
            for (int i = src.at(a.value.end); i < src.toks.length; i++) {
                Tok t = src.toks[i];
                if (Src.trivia(t)) {
                    continue;
                }
                if (t.is(")")) {
                    rparen = t;
                } else {
                    break;
                }
            }
            if (rparen != null && c.start < rparen.start) {
                return trail(c, a.value);
            }
            if (!c.ownLine) {
                Tok dot = src.firstAfterParens(a.value.end, a.attr.start);
                if (c.end < dot.start) {
                    return trail(c, a.value);
                }
            }
            return dangle(c, c.enclosing);
        }

        boolean exprIf(DComment c, IfExp e) {
            if (c.ownLine) {
                return false;
            }
            Tok ifTok = src.firstAfterParens(e.body.end, e.test.start);
            if (ifTok.start < c.start && c.start < e.test.start) {
                return lead(c, e.test);
            }
            Tok elseTok = src.firstAfterParens(e.test.end, e.orelse.start);
            if (elseTok.start < c.start && c.start < e.orelse.start) {
                return lead(c, e.orelse);
            }
            return false;
        }

        boolean withItem(DComment c) {
            if (c.preceding == null || c.following == null) {
                return false;
            }
            Tok as = src.firstAfterParens(c.preceding.end, c.following.start);
            if (c.end < as.start) {
                return trail(c, c.preceding);
            } else if (!c.ownLine) {
                return dangle(c, c.enclosing);
            }
            return lead(c, c.following);
        }

        boolean patternAs(DComment c) {
            if (c.preceding == null) {
                return false;
            }
            Tok as = src.firstAfterParens(c.preceding.end, src.s.length());
            if (as == null || !as.is("as")) {
                return false;
            }
            if (c.end < as.start) {
                return trail(c, c.preceding);
            }
            return dangle(c, c.enclosing);
        }

        boolean patternMapping(DComment c, PMapping pm) {
            if (c.following != null || pm.rest == null) {
                return false;
            }
            if (c.start > pm.rest.end) {
                return dangle(c, c.enclosing);
            }
            int precedingEnd = c.preceding != null ? c.preceding.end : c.enclosing.start;
            return src.any(precedingEnd, c.start, "**") && dangle(c, c.enclosing);
        }

        boolean namedExpr(DComment c) {
            if (c.preceding == null || c.following == null) {
                return false;
            }
            Tok ce = src.firstAfterParens(c.preceding.end, c.following.start);
            if (c.end < ce.start) {
                return trail(c, c.preceding);
            }
            return dangle(c, c.enclosing);
        }

        boolean lambda(DComment c, Lambda l) {
            Parameters ps = l.parameters;
            if (ps != null) {
                if (c.start < ps.start) {
                    return c.ownLine ? lead(c, ps) : dangle(c, l);
                }
                if (ps.end < c.start && c.start < l.body.start) {
                    if (src.any(ps.end, c.start, "(")) {
                        return false;
                    }
                    return dangle(c, l);
                }
            } else if (c.start < l.body.start) {
                if (src.any(l.start, c.start, "(")) {
                    return false;
                }
                return dangle(c, l);
            }
            return false;
        }

        boolean unaryOp(DComment c, UnaryOp u) {
            int i = src.nextIdx(u.start);
            int upTo = u.operand.start;
            for (i = i + 1; i < src.toks.length; i++) {
                Tok t = src.toks[i];
                if (t.kind == T_END || t.end > u.operand.start) {
                    break;
                }
                if (!Src.trivia(t) && t.is("(")) {
                    upTo = t.start;
                    break;
                }
            }
            if (c.end < upTo && !c.ownLine) {
                return dangle(c, u);
            }
            return false;
        }

        boolean importFrom(DComment c, ImportFrom imp) {
            if (!c.ownLine && !empty(imp.names) && imp.start < c.start && c.start < imp.names.get(0).start) {
                return dangle(c, c.enclosing);
            }
            Tok t = src.next(c.start);
            if (t != null && t.is(",") && c.preceding instanceof Alias) {
                return dangle(c, c.preceding);
            }
            return false;
        }

        boolean comprehension(DComment c, Comprehension comp) {
            boolean own = c.ownLine;
            if (c.end < comp.target.start) {
                return !own && dangle(c, comp);
            }
            Tok in = src.firstAfterParens(comp.target.end, comp.iter.start);
            if (c.start < in.start) {
                return own && dangle(c, comp);
            }
            if (c.start < comp.iter.start) {
                return !own && dangle(c, comp);
            }
            int lastEnd = comp.iter.end;
            for (Expr ifNode : comp.ifs) {
                Tok ifTok = src.firstAfterParens(lastEnd, ifNode.start);
                if (own) {
                    if (lastEnd < c.start && c.start < ifTok.start) {
                        return dangle(c, comp);
                    }
                } else if (ifTok.start < c.start && c.start < ifNode.start) {
                    return dangle(c, comp);
                }
                lastEnd = ifNode.end;
            }
            return false;
        }

        boolean implicitConcatenatedString(DComment c) {
            if (c.ownLine || !(c.preceding instanceof Str)) {
                return false;
            }
            Str s = (Str) c.preceding;
            if (s.parts.size() < 2) {
                return false;
            }
            Node lastPart = s.parts.get(s.parts.size() - 1);
            Node secondLast = s.parts.get(s.parts.size() - 2);
            if (src.lineBreak(secondLast.end, lastPart.start) && src.parenthesized(s)) {
                if (!src.any(lastPart.end, c.start, ")")) {
                    return trail(c, lastPart);
                }
            }
            return false;
        }

        boolean sequencePatternParenthesized(PSequence p) {
            return sequenceType(src, p) != SEQ_TUPLE_NO_PARENS;
        }
    }

    static final int SEQ_LIST = 0, SEQ_TUPLE = 1, SEQ_TUPLE_NO_PARENS = 2;

    static int sequenceType(Src src, PSequence p) {
        Pattern firstP = first(p.patterns);
        String before = src.s.substring(p.start, firstP != null ? firstP.start : p.end);
        String afterLast = src.s.substring(p.start, firstP != null ? firstP.end : p.end);
        if (before.startsWith("[") && !afterLast.endsWith(",")) {
            return SEQ_LIST;
        } else if (before.startsWith("(")) {
            if (firstP == null) {
                return SEQ_TUPLE;
            }
            int open = 0;
            for (int i = src.at(p.start); i < src.toks.length && src.toks[i].end <= firstP.start; i++) {
                if (src.toks[i].is("(")) {
                    open++;
                }
            }
            return open > 0 ? SEQ_TUPLE : SEQ_TUPLE_NO_PARENS;
        }
        return SEQ_TUPLE_NO_PARENS;
    }

    /** Colon tokens of a slice: {first colon, second colon or null}. */
    static Tok[] sliceColons(Src src, Slice sl) {
        int afterLower = sl.lower != null ? sl.lower.end : sl.start;
        Tok first = src.firstAfterParens(afterLower, sl.end);
        int afterUpper = sl.upper != null ? sl.upper.end : first.end;
        Tok second = src.firstAfterParens(afterUpper, sl.end);
        return new Tok[] {first, second};
    }

    /** 0 lower, 1 upper, 2 step. */
    static int sliceSection(Src src, int commentStart, Slice sl) {
        Tok[] colons = sliceColons(src, sl);
        if (commentStart < colons[0].start) {
            return 0;
        }
        if (colons[1] != null) {
            return commentStart < colons[1].start ? 1 : 2;
        }
        return 1;
    }

    // =====================================================================================
    // Formatting context and builders (ruff_formatter builders, PyFormatContext, comments/format.rs)
    // =====================================================================================

    /** Writes formatted content into a formatter. */
    interface Fmt {
        void fmt(F f);
    }

    static final Fmt NOTHING = f -> {
    };

    static final int NL_TOP = 0, NL_COMPOUND = 1, NL_EXPR = 2, NL_PAREN = 3;
    static final int BI_BLOCK = 0, BI_SOFT = 1, BI_SOFT_LINE_OR_SPACE = 2, BI_SOFT_SPACE = 3;
    static final NodeComments NO_NODE_COMMENTS = new NodeComments();

    /** The formatter: output buffer plus ruff's PyFormatContext. */
    static final class F {
        final String s;
        final Src src;
        ArrayList<El> buf = new ArrayList<>(256);
        int ids;
        int level = NL_TOP;
        int levelGroup;
        boolean lastTopLevel;
        int indentLevel;
        char docstringQuote;
        int fstate;
        FStringCtx fctx;

        F(String s, Src src) {
            this.s = s;
            this.src = src;
        }

        NodeComments comments(Node n) {
            NodeComments c = n == null ? null : n.comments;
            return c == null ? NO_NODE_COMMENTS : c;
        }

        List<Comment> leading(Node n) {
            return comments(n).leading;
        }

        List<Comment> dangling(Node n) {
            return comments(n).dangling;
        }

        List<Comment> trailing(Node n) {
            return comments(n).trailing;
        }

        boolean hasLeading(Node n) {
            return !comments(n).leading.isEmpty();
        }

        boolean hasTrailing(Node n) {
            return !comments(n).trailing.isEmpty();
        }

        boolean hasDangling(Node n) {
            return !comments(n).dangling.isEmpty();
        }

        boolean hasComments(Node n) {
            NodeComments c = comments(n);
            return !c.leading.isEmpty() || !c.dangling.isEmpty() || !c.trailing.isEmpty();
        }

        boolean hasTrailingOwnLine(Node n) {
            for (Comment c : comments(n).trailing) {
                if (c.ownLine) {
                    return true;
                }
            }
            return false;
        }

        boolean parenthesized(Node e) {
            return src.parenthesized(e);
        }

        boolean isParenthesizedLevel() {
            return level == NL_EXPR && levelGroup != 0 || level == NL_PAREN;
        }

        // ---- node levels (WithNodeLevel) ----
        void withLevel(int lvl, int group, Fmt c) {
            int sl = level, sg = levelGroup;
            boolean last = lastTopLevel;
            level = lvl;
            levelGroup = group;
            try {
                c.fmt(this);
            } finally {
                level = sl;
                levelGroup = sg;
                lastTopLevel = last;
            }
        }

        // ---- elements ----
        void w(El e) {
            buf.add(e);
        }

        void w(Fmt c) {
            c.fmt(this);
        }

        void tok(String t) {
            buf.add(token(t));
        }

        void txt(String t) {
            buf.add(text(t));
        }

        void space() {
            buf.add(SPACE);
        }

        void soft() {
            buf.add(SOFT);
        }

        void softOrSpace() {
            buf.add(SOFT_OR_SPACE);
        }

        void hard() {
            buf.add(HARD);
        }

        void emptyLine() {
            buf.add(EMPTY);
        }

        void expandParent() {
            buf.add(EXPAND_PARENT);
        }

        int groupId() {
            return ++ids;
        }

        int snapshot() {
            return buf.size();
        }

        void restore(int snap) {
            for (int i = buf.size() - 1; i >= snap; i--) {
                buf.remove(i);
            }
        }

        // ---- containers ----
        void group(Fmt c) {
            group(0, false, c);
        }

        void group(int id, boolean expand, Fmt c) {
            El g = new El(E_GROUP);
            g.id = id;
            g.mode = expand ? G_EXPAND : G_FLAT;
            buf.add(g);
            c.fmt(this);
            buf.add(END_GROUP);
        }

        void indent(Fmt c) {
            buf.add(INDENT);
            c.fmt(this);
            buf.add(END_INDENT);
        }

        void dedent(boolean toRoot, Fmt c) {
            El d = new El(E_DEDENT);
            d.n = toRoot ? 1 : 0;
            buf.add(d);
            c.fmt(this);
            buf.add(END_DEDENT);
        }

        void align(int n, Fmt c) {
            El a = new El(E_ALIGN);
            a.n = n;
            buf.add(a);
            c.fmt(this);
            buf.add(END_ALIGN);
        }

        void blockIndent(int mode, Fmt c) {
            int snap = buf.size();
            buf.add(INDENT);
            buf.add(mode == BI_BLOCK ? HARD : mode == BI_SOFT ? SOFT : SOFT_OR_SPACE);
            int before = buf.size();
            c.fmt(this);
            if (buf.size() == before) {
                restore(snap);
                return;
            }
            buf.add(END_INDENT);
            if (mode == BI_BLOCK) {
                buf.add(HARD);
            } else if (mode == BI_SOFT) {
                buf.add(SOFT);
            } else if (mode == BI_SOFT_SPACE) {
                buf.add(SOFT_OR_SPACE);
            }
        }

        void blockIndent(Fmt c) {
            blockIndent(BI_BLOCK, c);
        }

        void softBlockIndent(Fmt c) {
            blockIndent(BI_SOFT, c);
        }

        void softLineIndentOrSpace(Fmt c) {
            blockIndent(BI_SOFT_LINE_OR_SPACE, c);
        }

        void softSpaceOrBlockIndent(Fmt c) {
            blockIndent(BI_SOFT_SPACE, c);
        }

        void cond(int mode, int id, Fmt c) {
            El e = new El(E_COND);
            e.mode = mode;
            e.id = id;
            buf.add(e);
            c.fmt(this);
            buf.add(END_COND);
        }

        void ifBreaks(Fmt c) {
            cond(EXPANDED, 0, c);
        }

        void ifBreaks(int id, Fmt c) {
            cond(EXPANDED, id, c);
        }

        void ifFits(Fmt c) {
            cond(FLAT, 0, c);
        }

        void ifFits(int id, Fmt c) {
            cond(FLAT, id, c);
        }

        void indentIfBreaks(int id, Fmt c) {
            El e = new El(E_INDENT_IF_BREAKS);
            e.id = id;
            buf.add(e);
            c.fmt(this);
            buf.add(END_INDENT_IF_BREAKS);
        }

        void lineSuffix(int reserved, Fmt c) {
            El e = new El(E_SUFFIX);
            e.n = reserved;
            buf.add(e);
            c.fmt(this);
            buf.add(END_SUFFIX);
        }

        void fitsExpanded(int condMode, int condId, Fmt c) {
            El e = new El(E_FITS_EXPANDED);
            e.mode = condMode;
            e.id = condId;
            buf.add(e);
            c.fmt(this);
            buf.add(END_FITS_EXPANDED);
        }

        void bestFitParenthesize(int id, Fmt c) {
            El e = new El(E_BFP);
            e.id = id;
            buf.add(e);
            c.fmt(this);
            buf.add(END_BFP);
        }

        void bestFitting(boolean allLines, Fmt... variants) {
            El e = new El(E_BEST_FITTING);
            e.n = allLines ? 1 : 0;
            e.variants = new El[variants.length][];
            for (int i = 0; i < variants.length; i++) {
                ArrayList<El> v = record(variants[i], BF_ENTRY);
                v.add(END_BF_ENTRY);
                e.variants[i] = v.toArray(new El[0]);
            }
            buf.add(e);
        }

        /** Formats {@code c} into a fresh buffer that starts with {@code first} (when non-null). */
        ArrayList<El> record(Fmt c, El first) {
            ArrayList<El> saved = buf;
            buf = new ArrayList<>();
            if (first != null) {
                buf.add(first);
            }
            try {
                c.fmt(this);
                return buf;
            } finally {
                buf = saved;
            }
        }

        /** Formatter::intern: null when empty, the element itself when single. */
        El intern(Fmt c) {
            ArrayList<El> r = record(c, null);
            if (r.isEmpty()) {
                return null;
            }
            if (r.size() == 1) {
                return r.get(0);
            }
            El e = new El(E_INTERNED);
            e.content = r.toArray(new El[0]);
            return e;
        }

        // ---- comments (comments/format.rs) ----
        void leadingComments(List<Comment> cs) {
            for (Comment c : cs) {
                if (!c.formatted) {
                    int lines = linesAfter(s, c.end);
                    formatComment(c);
                    emptyLines(lines);
                    c.formatted = true;
                }
            }
        }

        void leadingAlternateBranchComments(List<Comment> cs, Node lastNode) {
            if (!cs.isEmpty()) {
                emptyLines(linesBefore(s, cs.get(0).start));
                leadingComments(cs);
            } else if (lastNode != null) {
                emptyLines(linesAfterIgnoringTrivia(s, lastNode.end));
            }
        }

        void trailingComments(List<Comment> cs) {
            boolean ownLineSeen = false;
            for (Comment c : cs) {
                if (c.formatted) {
                    continue;
                }
                ownLineSeen |= c.ownLine;
                if (ownLineSeen) {
                    int lines = linesBefore(s, c.start);
                    lineSuffix(0, f -> {
                        f.emptyLines(lines);
                        f.formatComment(c);
                    });
                    expandParent();
                } else {
                    trailingEndOfLineComment(c);
                }
                c.formatted = true;
            }
        }

        void danglingComments(List<Comment> cs) {
            boolean first = true;
            for (Comment c : cs) {
                if (c.formatted) {
                    continue;
                }
                if (first) {
                    if (c.ownLine) {
                        hard();
                    } else {
                        space();
                        space();
                    }
                }
                formatComment(c);
                emptyLines(linesAfter(s, c.end));
                c.formatted = true;
                first = false;
            }
        }

        void danglingOpenParenthesisComments(List<Comment> cs) {
            for (Comment c : cs) {
                if (!c.formatted) {
                    trailingEndOfLineComment(c);
                    c.formatted = true;
                }
            }
        }

        void formatComment(Comment c) {
            txt(normalizeComment(s.substring(c.start, c.end)));
        }

        void trailingEndOfLineComment(Comment c) {
            String norm = normalizeComment(s.substring(c.start, c.end));
            int reserved = isPragmaComment(norm) ? 0 : 2 + textWidth(norm);
            lineSuffix(reserved, f -> {
                f.space();
                f.space();
                f.txt(norm);
            });
            expandParent();
        }

        void emptyLines(int lines) {
            if (level == NL_TOP) {
                if (lines <= 1) {
                    hard();
                } else if (lines == 2) {
                    emptyLine();
                } else {
                    emptyLine();
                    emptyLine();
                }
            } else if (level == NL_COMPOUND) {
                if (lines <= 1) {
                    hard();
                } else {
                    emptyLine();
                }
            } else {
                hard();
            }
        }

        void emptyLinesBeforeTrailingComments(List<Comment> cs) {
            for (Comment c : cs) {
                if (c.ownLine) {
                    int want = level == NL_TOP ? 2 : 1;
                    for (int i = Math.max(0, linesBefore(s, c.start) - 1); i < want; i++) {
                        emptyLine();
                    }
                    return;
                }
            }
        }

        void emptyLinesAfterLeadingComments(List<Comment> cs) {
            for (int k = cs.size() - 1; k >= 0; k--) {
                Comment c = cs.get(k);
                if (c.ownLine) {
                    int want = level == NL_TOP ? 2 : 1;
                    int actual = Math.max(0, linesAfter(s, c.end) - 1);
                    if (actual == 0 || actual >= want) {
                        return;
                    }
                    for (int i = actual; i < want; i++) {
                        emptyLine();
                    }
                    return;
                }
            }
        }

        /** FormatNodeRule::fmt: leading comments, the fields, trailing comments. */
        void node(Node n, Fmt fields) {
            NodeComments c = comments(n);
            leadingComments(c.leading);
            fields.fmt(this);
            trailingComments(c.trailing);
        }

        /** has_magic_trailing_comma: the first token after {@code a} (skipping ')') is a comma. */
        boolean magicTrailingComma(int a, int b) {
            if (fstate != 0 && fctx != null && !fctx.multiline) {
                return false;
            }
            Tok t = src.firstAfterParens(a, b);
            return t != null && t.is(",");
        }

        boolean hasTrailingComma(int a, int b) {
            Tok t = src.firstAfterParens(a, b);
            return t != null && t.is(",");
        }
    }

    /** Formatter::intern once and reuse (Memoized). */
    static final class Memo implements Fmt {
        final Fmt inner;
        El el;
        boolean done;

        Memo(Fmt inner) {
            this.inner = inner;
        }

        @Override
        public void fmt(F f) {
            if (!done) {
                el = f.intern(inner);
                done = true;
            }
            if (el != null) {
                f.buf.add(el);
            }
        }

        El[] inspect(F f) {
            if (!done) {
                el = f.intern(inner);
                done = true;
            }
            return el == null ? new El[0] : el.k == E_INTERNED ? el.content : new El[] {el};
        }

        boolean willBreak(F f) {
            return PythonFormatter.willBreak(inspect(f));
        }
    }

    /** JoinCommaSeparatedBuilder: comma-separated entries with magic trailing comma handling. */
    static final class Joiner {
        final F f;
        final int sequenceEnd;
        int entries;
        int lastEnd;
        boolean oneOrMore;

        Joiner(F f, int sequenceEnd) {
            this.f = f;
            this.sequenceEnd = sequenceEnd;
        }

        Joiner oneOrMore() {
            oneOrMore = true;
            return this;
        }

        Joiner entry(Node n, Fmt content) {
            return entry(n.end, content, SOFT_OR_SPACE);
        }

        Joiner entry(int end, Fmt content, El separator) {
            if (entries > 0) {
                f.tok(",");
                if (separator != null) {
                    f.w(separator);
                }
            }
            entries++;
            lastEnd = end;
            content.fmt(f);
            return this;
        }

        void finish() {
            if (f.fstate != 0 && f.fctx != null && !f.fctx.multiline) {
                return;
            }
            if (entries > 0) {
                boolean magic = f.hasTrailingComma(lastEnd, sequenceEnd);
                if (magic || oneOrMore || entries > 1) {
                    f.ifBreaks(ff -> ff.tok(","));
                }
                if (magic) {
                    f.expandParent();
                }
            }
        }
    }

    /** parenthesize_if_expands: optional parentheses that only appear when the content breaks. */
    static void parenthesizeIfExpands(F f, boolean indent, Fmt content) {
        f.withLevel(NL_PAREN, 0, ff -> {
            if (indent) {
                int id = ff.groupId();
                ff.group(id, false, g -> {
                    g.ifBreaks(x -> x.tok("("));
                    g.indentIfBreaks(id, x -> {
                        x.soft();
                        content.fmt(x);
                    });
                    g.soft();
                    g.ifBreaks(x -> x.tok(")"));
                });
            } else {
                ff.group(g -> {
                    g.ifBreaks(x -> x.tok("("));
                    content.fmt(g);
                    g.ifBreaks(x -> x.tok(")"));
                });
            }
        });
    }

    static boolean pyWs(char c) {
        return c == ' ' || c == '\t' || c == '\f';
    }

    static int linesBefore(String s, int offset) {
        int n = 0;
        int i = offset - 1;
        while (i >= 0) {
            char c = s.charAt(i);
            if (c == '\n') {
                if (i > 0 && s.charAt(i - 1) == '\r') {
                    i--;
                }
                n++;
            } else if (c == '\r') {
                n++;
            } else if (!pyWs(c)) {
                break;
            }
            i--;
        }
        return n;
    }

    static int linesAfter(String s, int offset) {
        int n = 0;
        int len = s.length();
        for (int i = offset; i < len; i++) {
            char c = s.charAt(i);
            if (c == '\n') {
                n++;
            } else if (c == '\r') {
                if (i + 1 < len && s.charAt(i + 1) == '\n') {
                    i++;
                }
                n++;
            } else if (!pyWs(c)) {
                break;
            }
        }
        return n;
    }

    /** lines_after_ignoring_trivia: newlines after offset, restarting the count after each comment. */
    static int linesAfterIgnoringTrivia(String s, int offset) {
        int n = 0;
        int len = s.length();
        for (int i = offset; i < len; i++) {
            char c = s.charAt(i);
            if (c == '\n') {
                n++;
            } else if (c == '\r') {
                if (i + 1 < len && s.charAt(i + 1) == '\n') {
                    i++;
                }
                n++;
            } else if (c == '#') {
                n = 0;
                while (i + 1 < len && s.charAt(i + 1) != '\n' && s.charAt(i + 1) != '\r') {
                    i++;
                }
            } else if (!pyWs(c)) {
                break;
            }
        }
        return n;
    }

    /** lines_after_ignoring_end_of_line_trivia: skips the rest of the line's trivia, then counts newlines. */
    static int linesAfterIgnoringEndOfLineTrivia(String s, int offset) {
        int len = s.length();
        int i = offset;
        while (i < len) {
            char c = s.charAt(i);
            if (c == '\n' || c == '\r') {
                break;
            }
            if (c == '#') {
                while (i < len && s.charAt(i) != '\n' && s.charAt(i) != '\r') {
                    i++;
                }
                break;
            }
            if (!pyWs(c)) {
                return 0;
            }
            i++;
        }
        int n = 0;
        while (i < len) {
            char c = s.charAt(i);
            if (c == '\n') {
                n++;
            } else if (c == '\r') {
                if (i + 1 < len && s.charAt(i + 1) == '\n') {
                    i++;
                }
                n++;
            } else if (!pyWs(c)) {
                break;
            }
            i++;
        }
        return n;
    }

    static boolean isPragmaComment(String comment) {
        if (!comment.startsWith("#")) {
            return false;
        }
        String t = stripLeadingWs(comment.substring(1));
        if (t.length() >= 4 && t.substring(0, 4).equalsIgnoreCase("noqa")) {
            return true;
        }
        if (t.startsWith("nosec")) {
            return true;
        }
        int colon = t.indexOf(':');
        if (colon < 0) {
            return false;
        }
        switch (t.substring(0, colon)) {
            case "isort":
            case "type":
            case "pyright":
            case "pyrefly":
            case "pylint":
            case "flake8":
            case "ruff":
            case "ty":
                return true;
            default:
                return false;
        }
    }

    /** Rust's char::is_whitespace (the Unicode White_Space property). */
    static boolean uniWs(char c) {
        return c == ' ' || c >= '\t' && c <= '\r' || c == '\u0085' || c == '\u00A0' || c == '\u1680'
                || c >= '\u2000' && c <= '\u200A' || c == '\u2028' || c == '\u2029' || c == '\u202F'
                || c == '\u205F' || c == '\u3000';
    }

    static String stripTrailingWs(String x) {
        int e = x.length();
        while (e > 0 && uniWs(x.charAt(e - 1))) {
            e--;
        }
        return x.substring(0, e);
    }

    static String stripLeadingWs(String x) {
        int b = 0;
        while (b < x.length() && uniWs(x.charAt(b))) {
            b++;
        }
        return x.substring(b);
    }

    /** normalize_comment: trims the end and inserts the space after {@code #} where needed. */
    static String normalizeComment(String raw) {
        String trimmed = stripTrailingWs(raw);
        String content = trimmed.substring(1);
        if (content.isEmpty()) {
            return "#";
        }
        char c0 = content.charAt(0);
        if (c0 == ' ' || c0 == '!' || c0 == ':' || c0 == '#' || c0 == '\'' || c0 == '|') {
            return trimmed;
        }
        if (c0 == '\u00A0') {
            int i = 0;
            while (i < content.length() && content.charAt(i) == '\u00A0') {
                i++;
            }
            String rest = content.substring(i);
            if (stripLeadingWs(rest).startsWith("type:")) {
                return "# " + content;
            } else if (rest.startsWith(" ")) {
                return "# " + rest;
            } else {
                return "# " + content.substring(1);
            }
        }
        return "# " + content;
    }

    // ---- expression/parentheses.rs builders ----

    /** in_parentheses_only_soft_line_break. */
    static void ipoSoft(F f) {
        if (f.level == NL_EXPR && f.levelGroup != 0) {
            f.ifBreaks(f.levelGroup, x -> x.soft());
        } else if (f.level == NL_PAREN) {
            f.soft();
        }
    }

    /** in_parentheses_only_soft_line_break_or_space. */
    static void ipoSoftOrSpace(F f) {
        if (f.level == NL_EXPR && f.levelGroup != 0) {
            int id = f.levelGroup;
            f.ifBreaks(id, x -> x.softOrSpace());
            f.ifFits(id, x -> x.space());
        } else if (f.level == NL_PAREN) {
            f.softOrSpace();
        } else {
            f.space();
        }
    }

    /** in_parentheses_only_group. */
    static void ipoGroup(F f, Fmt c) {
        if (f.level == NL_EXPR && f.levelGroup != 0) {
            El g = new El(E_CGROUP);
            g.id = f.levelGroup;
            g.n = EXPANDED;
            g.mode = G_FLAT;
            f.w(g);
            c.fmt(f);
            f.w(END_CGROUP);
        } else if (f.level == NL_PAREN) {
            f.group(c);
        } else {
            c.fmt(f);
        }
    }

    /** in_parentheses_only_if_group_breaks. */
    static void ipoIfBreaks(F f, Fmt c) {
        if (f.level == NL_EXPR && f.levelGroup != 0) {
            f.ifBreaks(f.levelGroup, c);
        } else if (f.level == NL_PAREN) {
            f.ifBreaks(c);
        }
    }

    /** parenthesized(left, content, right) with optional dangling open-parenthesis comments and hugging. */
    static void parenthesized(F f, String left, Fmt content, String right, List<Comment> comments, boolean hug) {
        int curLevel = f.level;
        int curGroup = f.levelGroup;
        Fmt indented = ff -> {
            if (comments.isEmpty()) {
                if (hug) {
                    content.fmt(ff);
                } else {
                    ff.group(g -> g.softBlockIndent(content));
                }
            } else {
                ff.group(g -> {
                    g.danglingOpenParenthesisComments(comments);
                    g.softBlockIndent(content);
                });
            }
        };
        f.withLevel(NL_PAREN, 0, ff -> {
            ff.tok(left);
            if (curLevel == NL_EXPR && curGroup != 0) {
                ff.fitsExpanded(FLAT, curGroup, indented);
            } else {
                indented.fmt(ff);
            }
            ff.tok(right);
        });
    }

    static void parenthesized(F f, String left, Fmt content, String right) {
        parenthesized(f, left, content, right, NO_COMMENTS, false);
    }

    /** optional_parentheses: parentheses that only render when the content does not fit. */
    static void optionalParentheses(F f, Fmt content) {
        int id = f.groupId();
        f.withLevel(NL_EXPR, id, ff -> ff.group(id, false, g -> {
            g.ifBreaks(x -> x.tok("("));
            g.indentIfBreaks(id, x -> {
                x.soft();
                content.fmt(x);
            });
            g.soft();
            g.ifBreaks(x -> x.tok(")"));
        }));
    }

    /** empty_parenthesized: an empty pair of brackets holding only comments. */
    static void emptyParenthesized(F f, String left, List<Comment> comments, String right) {
        int split = 0;
        while (split < comments.size() && !comments.get(split).ownLine) {
            split++;
        }
        List<Comment> eol = comments.subList(0, split);
        List<Comment> own = comments.subList(split, comments.size());
        f.group(g -> {
            g.tok(left);
            g.trailingComments(eol);
            if (!eol.isEmpty()) {
                g.hard();
            }
            g.softBlockIndent(x -> x.danglingComments(own));
            g.tok(right);
        });
    }

    // ======================================================================= expression/mod.rs

    static final int P_PRESERVE = 0, P_ALWAYS = 1, P_NEVER = 2;
    static final int PZ_OPTIONAL = 0, PZ_IF_BREAKS = 1, PZ_IF_REQUIRED = 2, PZ_IF_BREAKS_PARENTHESIZED = 3,
            PZ_IF_BREAKS_PARENTHESIZED_NESTED = 4;
    static final int OP_MULTILINE = 0, OP_ALWAYS = 1, OP_BEST_FIT = 2, OP_NEVER = 3;
    static final int OWN_NONE = 0, OWN_EMPTY = 1, OWN_NON_EMPTY = 2;

    /** FormatExpr with Parentheses::Preserve. */
    static void fmtExpr(F f, Expr e) {
        fmtExpr(f, e, P_PRESERVE);
    }

    /** FormatExpr::fmt. */
    static void fmtExpr(F f, Expr e, int parentheses) {
        boolean parenthesize = parentheses == P_PRESERVE ? f.parenthesized(e) : parentheses == P_ALWAYS;
        if (parenthesize) {
            if (!f.hasLeading(e) && !f.hasTrailing(e)) {
                parenthesized(f, "(", ff -> fmtExprNode(ff, e), ")");
            } else {
                fmtWithParenthesesComments(f, e);
            }
        } else if (f.level == NL_TOP || f.level == NL_COMPOUND) {
            f.withLevel(NL_EXPR, 0, ff -> fmtExprNode(ff, e));
        } else {
            fmtExprNode(f, e);
        }
    }

    /** FormatNodeRule::fmt of an expression with its default options. */
    static void fmtExprNode(F f, Expr e) {
        f.node(e, ff -> fmtExprFields(ff, e));
    }

    /** The outermost parentheses range around {@code e} (parentheses_iterator(..).last()), or null. */
    static int[] outerParentheses(F f, Node e) {
        Src src = f.src;
        int count = 0;
        int lastOpen = -1;
        int lastClose = -1;
        int i = src.prevIdx(e.start);
        int j = src.nextIdx(e.end);
        while (i >= 0 && j >= 0 && src.toks[i].is("(") && src.toks[j].is(")")) {
            lastOpen = src.toks[i].start;
            lastClose = src.toks[j].end;
            count++;
            i = src.prevIdx(src.toks[i].start);
            j = src.nextIdx(src.toks[j].end);
        }
        return count == 0 ? null : new int[] {lastOpen, lastClose};
    }

    /** format_with_parentheses_comments. */
    static void fmtWithParenthesesComments(F f, Expr e) {
        List<Comment> leading = f.leading(e);
        List<Comment> trailing = f.trailing(e);
        int[] range = outerParentheses(f, e);
        int ls = 0;
        int ts = trailing.size();
        if (range != null) {
            while (ls < leading.size() && leading.get(ls).start < range[0]) {
                ls++;
            }
            ts = 0;
            while (ts < trailing.size() && trailing.get(ts).start < range[1]) {
                ts++;
            }
        }
        List<Comment> leadingOuter = leading.subList(0, ls);
        List<Comment> rest = leading.subList(ls, leading.size());
        List<Comment> trailingInner = trailing.subList(0, ts);
        List<Comment> trailingOuter = trailing.subList(ts, trailing.size());
        List<Comment> parenComment;
        List<Comment> leadingInner;
        if (!rest.isEmpty() && !rest.get(0).ownLine) {
            parenComment = rest.subList(0, 1);
            leadingInner = rest.subList(1, rest.size());
        } else {
            parenComment = NO_COMMENTS;
            leadingInner = leading;
        }
        f.leadingComments(leadingOuter);
        parenthesized(f, "(", ff -> {
            ff.leadingComments(leadingInner);
            fmtExprFields(ff, e);
            ff.trailingComments(trailingInner);
        }, ")", parenComment, false);
        f.trailingComments(trailingOuter);
    }

    /** maybe_parenthesize_expression. */
    static void maybeParenthesize(F f, Expr e, Node parent, int parenthesize) {
        if (parenthesize == PZ_OPTIONAL && f.parenthesized(e)) {
            fmtExpr(f, e, P_ALWAYS);
            return;
        }
        if (f.hasLeading(e) || f.hasTrailingOwnLine(e)) {
            fmtExpr(f, e, P_ALWAYS);
            return;
        }
        int needs = needsParentheses(f, e, parent);
        if (needs != OP_ALWAYS && f.isParenthesizedLevel()) {
            if (parenthesize == PZ_IF_BREAKS_PARENTHESIZED_NESTED) {
                parenthesizeIfExpands(f, true, ff -> fmtExpr(ff, e, P_NEVER));
            } else {
                fmtExpr(f, e, P_NEVER);
            }
            return;
        }
        Fmt plain = ff -> fmtExpr(ff, e, P_NEVER);
        switch (needs) {
            case OP_MULTILINE:
                if (parenthesize == PZ_IF_REQUIRED) {
                    plain.fmt(f);
                } else if (canOmitOptionalParentheses(f, e)) {
                    optionalParentheses(f, plain);
                } else {
                    parenthesizeIfExpands(f, true, plain);
                }
                return;
            case OP_BEST_FIT:
                if (parenthesize == PZ_IF_BREAKS_PARENTHESIZED || parenthesize == PZ_IF_BREAKS_PARENTHESIZED_NESTED) {
                    if (canOmitOptionalParentheses(f, e)) {
                        optionalParentheses(f, plain);
                    } else {
                        parenthesizeIfExpands(f, true, plain);
                    }
                } else if (parenthesize == PZ_OPTIONAL || parenthesize == PZ_IF_REQUIRED) {
                    plain.fmt(f);
                } else if (f.hasTrailing(e)) {
                    fmtExpr(f, e, P_ALWAYS);
                } else {
                    int id = f.groupId();
                    f.withLevel(NL_EXPR, id, ff -> ff.bestFitParenthesize(id, plain));
                }
                return;
            case OP_NEVER:
                plain.fmt(f);
                return;
            default:
                fmtExpr(f, e, P_ALWAYS);
        }
    }

    // ---- can_omit_optional_parentheses

    static final class CanOmit {
        final F f;
        int maxPrecedence = PREC_NONE;
        int count;
        boolean anyParenthesized;
        Expr last;
        int firstKind; // 0 none, 1 token, 2 expression
        Expr first;

        CanOmit(F f) {
            this.f = f;
        }

        void update(int precedence, int n) {
            if (maxPrecedence < precedence) {
                count = n;
                maxPrecedence = precedence;
            } else if (maxPrecedence == precedence) {
                count += n;
            }
        }

        void firstToken() {
            if (firstKind == 0) {
                firstKind = 1;
            }
        }

        void visitExpr(Expr e) {
            last = e;
            if (f.parenthesized(e)) {
                anyParenthesized = true;
            } else {
                visitSub(e);
            }
            if (firstKind == 0) {
                firstKind = 2;
                first = e;
            }
        }

        void walk(Node n) {
            n.each(c -> {
                if (c instanceof Expr) {
                    visitExpr((Expr) c);
                } else {
                    walk(c);
                }
            });
        }

        void visitSub(Expr e) {
            if (e instanceof DictE || e instanceof Seq && ((Seq) e).kind != '(') {
                anyParenthesized = true;
                return;
            }
            if (e instanceof Comp) {
                Comp c = (Comp) e;
                if (c.kind != '(' || c.parenthesized) {
                    anyParenthesized = true;
                    return;
                }
            } else if (e instanceof Seq) {
                if (((Seq) e).parenthesized) {
                    anyParenthesized = true;
                    return;
                }
            } else if (e instanceof BoolOp) {
                update(PREC_BOOLEAN, Math.max(0, ((BoolOp) e).values.size() - 1));
            } else if (e instanceof BinOp) {
                update(binaryPrecedence(((BinOp) e).op), 1);
            } else if (e instanceof IfExp) {
                update(PREC_CONDITIONAL, 2);
            } else if (e instanceof Compare) {
                update(PREC_COMPARATOR, ((Compare) e).ops.size());
            } else if (e instanceof Call) {
                anyParenthesized = true;
                visitExpr(((Call) e).func);
                last = e;
                return;
            } else if (e instanceof Subscript) {
                anyParenthesized = true;
                visitExpr(((Subscript) e).value);
                last = e;
                return;
            } else if (e instanceof Attribute) {
                Expr v = ((Attribute) e).value;
                visitExpr(v);
                if (hasParentheses(f, v) != OWN_NONE) {
                    update(PREC_ATTRIBUTE, 1);
                }
                last = e;
                return;
            } else if (e instanceof Named) {
                // walk
            } else if (e instanceof UnaryOp) {
                if (((UnaryOp) e).op.equals("~")) {
                    update(PREC_INVERSION, 1);
                }
                firstToken();
            } else if (e instanceof Lambda || e instanceof Await || e instanceof Yield || e instanceof Starred) {
                firstToken();
            } else {
                return;
            }
            walk(e);
        }
    }

    static boolean canOmitOptionalParentheses(F f, Expr e) {
        CanOmit v = new CanOmit(f);
        v.visitSub(e);
        if (!v.anyParenthesized || v.count > 1) {
            return false;
        }
        if (v.maxPrecedence == PREC_NONE || v.maxPrecedence == PREC_ATTRIBUTE) {
            return true;
        }
        return v.last != null && isOmitParenthesized(f, v.last) || v.firstKind == 2 && isOmitParenthesized(f, v.first);
    }

    private static boolean isOmitParenthesized(F f, Expr e) {
        return !(e instanceof Subscript) && hasParentheses(f, e) == OWN_NON_EMPTY;
    }

    static boolean argumentsEmpty(Arguments a) {
        return a.ordered.isEmpty();
    }

    static boolean parametersEmpty(Parameters p) {
        return p.posonly.isEmpty() && p.args.isEmpty() && p.vararg == null && p.kwonly.isEmpty() && p.kwarg == null;
    }

    static int hasOwnParentheses(F f, Expr e) {
        if (e instanceof Subscript) {
            return OWN_NON_EMPTY;
        }
        if (e instanceof Comp) {
            Comp c = (Comp) e;
            return c.kind != '(' || c.parenthesized ? OWN_NON_EMPTY : OWN_NONE;
        }
        if (e instanceof Seq) {
            Seq s = (Seq) e;
            if (s.kind == '(' && !s.parenthesized) {
                return OWN_NONE;
            }
            return !s.elts.isEmpty() || f.hasDangling(e) ? OWN_NON_EMPTY : OWN_EMPTY;
        }
        if (e instanceof DictE) {
            return !((DictE) e).values.isEmpty() || f.hasDangling(e) ? OWN_NON_EMPTY : OWN_EMPTY;
        }
        if (e instanceof Call) {
            return !argumentsEmpty(((Call) e).arguments) || f.hasDangling(e) ? OWN_NON_EMPTY : OWN_EMPTY;
        }
        return OWN_NONE;
    }

    static int hasParentheses(F f, Expr e) {
        int own = hasOwnParentheses(f, e);
        if (own != OWN_NONE) {
            return own;
        }
        return f.parenthesized(e) ? OWN_NON_EMPTY : OWN_NONE;
    }

    static boolean isSplittable(F f, Expr e) {
        if (e instanceof Compare || e instanceof BinOp || e instanceof BoolOp || e instanceof IfExp
                || e instanceof Subscript || e instanceof Await || e instanceof Comp) {
            return true;
        }
        if (e instanceof Seq) {
            return !((Seq) e).elts.isEmpty();
        }
        if (e instanceof DictE) {
            return !((DictE) e).values.isEmpty();
        }
        if (e instanceof UnaryOp) {
            return isSplittable(f, ((UnaryOp) e).operand);
        }
        if (e instanceof Yield) {
            return ((Yield) e).value != null;
        }
        if (e instanceof Call) {
            Call c = (Call) e;
            return !argumentsEmpty(c.arguments) || f.parenthesized(c.func);
        }
        if (e instanceof Str) {
            return ((Str) e).implicit();
        }
        Expr inner = e instanceof Lambda ? ((Lambda) e).body
                : e instanceof Starred ? ((Starred) e).value : e instanceof Attribute ? ((Attribute) e).value : null;
        return inner != null && (f.parenthesized(inner) || isSplittable(f, inner));
    }

    static boolean isInvalidTypeExpression(Expr e) {
        return e instanceof Named || e instanceof Await || e instanceof Yield;
    }

    static Expr leftMost(F f, Expr e) {
        Expr current = e;
        while (true) {
            Expr left = null;
            if (current instanceof BinOp) {
                left = ((BinOp) current).left;
            } else if (current instanceof IfExp) {
                left = ((IfExp) current).body;
            } else if (current instanceof Call) {
                left = ((Call) current).func;
            } else if (current instanceof Attribute) {
                left = ((Attribute) current).value;
            } else if (current instanceof Subscript) {
                left = ((Subscript) current).value;
            } else if (current instanceof BoolOp) {
                left = ((BoolOp) current).values.get(0);
            } else if (current instanceof Compare) {
                left = ((Compare) current).left;
            } else if (current instanceof Comp && ((Comp) current).kind == '(' && !((Comp) current).parenthesized) {
                left = ((Comp) current).elt;
            } else if (current instanceof Seq && ((Seq) current).kind == '(' && !((Seq) current).parenthesized) {
                List<Expr> elts = ((Seq) current).elts;
                left = elts.isEmpty() ? null : elts.get(0);
            } else if (current instanceof Slice) {
                left = ((Slice) current).lower;
            }
            if (left == null || f.parenthesized(left)) {
                return current;
            }
            current = left;
        }
    }

    // ---- call chains (CallChainLayout)

    static final int CCL_DEFAULT = -1, CCL_NON_FLUENT = -2, CCL_FIRST_CALL_LIKE = -3, CCL_BEFORE_FIRST_CALL_LIKE = -4;

    static boolean cclFluent(int layout) {
        return layout >= 0 || layout == CCL_FIRST_CALL_LIKE || layout == CCL_BEFORE_FIRST_CALL_LIKE;
    }

    static int cclDecrement(int layout) {
        return layout >= 0 ? layout > 1 ? layout - 1 : CCL_FIRST_CALL_LIKE : layout;
    }

    static int cclAfterAttribute(int layout) {
        return layout == CCL_FIRST_CALL_LIKE ? CCL_BEFORE_FIRST_CALL_LIKE : layout;
    }

    static int callChainLayout(F f, Expr e) {
        int computed = 0;
        int callLike = 0;
        boolean rootParenthesized = false;
        while (true) {
            if (e instanceof Attribute) {
                Expr v = ((Attribute) e).value;
                if (f.parenthesized(v)) {
                    rootParenthesized = true;
                    break;
                } else if (v instanceof Call || v instanceof Subscript) {
                    computed++;
                }
                e = v;
            } else if (e instanceof Call || e instanceof Subscript) {
                Expr inner = e instanceof Call ? ((Call) e).func : ((Subscript) e).value;
                if (f.parenthesized(inner)) {
                    break;
                }
                if (!(inner instanceof Call) && !(inner instanceof Subscript)) {
                    callLike++;
                }
                e = inner;
            } else {
                break;
            }
        }
        int root = rootParenthesized ? 1 : 0;
        return computed + root < 2 ? CCL_NON_FLUENT : callLike + root;
    }

    static int cclApply(F f, int layout, Expr item) {
        if (layout == CCL_DEFAULT) {
            return f.isParenthesizedLevel() ? callChainLayout(f, item) : CCL_NON_FLUENT;
        }
        return layout;
    }

    /** Formats the value of an attribute, call or subscript in a call chain. */
    private static void fmtChainValue(F f, Expr v, int layout, boolean parenthesized) {
        if (parenthesized) {
            fmtExpr(f, v, P_ALWAYS);
        } else if (v instanceof Attribute) {
            f.node(v, g -> fmtAttribute(g, (Attribute) v, layout));
        } else if (v instanceof Call) {
            f.node(v, g -> fmtCall(g, (Call) v, layout));
        } else if (v instanceof Subscript) {
            f.node(v, g -> fmtSubscript(g, (Subscript) v, layout));
        } else {
            fmtExpr(f, v, P_NEVER);
        }
    }

    // ======================================================================= expr_attribute.rs

    static boolean isBaseTenNumber(F f, Expr e) {
        if (!(e instanceof Num)) {
            return false;
        }
        String t = f.s.substring(e.start, e.end);
        char last = t.charAt(t.length() - 1);
        if (last == 'j' || last == 'J') {
            return false;
        }
        if (t.length() > 1 && t.charAt(0) == '0') {
            char c = Character.toLowerCase(t.charAt(1));
            return c != 'x' && c != 'o' && c != 'b';
        }
        return true;
    }

    static void ident(F f, Ident i) {
        f.txt(f.s.substring(i.start, i.end));
    }

    static void fmtAttribute(F f, Attribute e, int layout0) {
        int layout = cclApply(f, layout0, e);
        Fmt inner = ff -> {
            Expr v = e.value;
            boolean parenValue = isBaseTenNumber(ff, v) || ff.parenthesized(v);
            if (cclFluent(layout)) {
                fmtChainValue(ff, v, cclAfterAttribute(layout), parenValue);
            } else {
                fmtExpr(ff, v, parenValue ? P_ALWAYS : P_NEVER);
            }
            int rparenEnd = -1;
            for (int i = ff.src.nextIdx(v.end); i >= 0 && ff.src.toks[i].is(")"); i = ff.src.nextIdx(rparenEnd)) {
                rparenEnd = ff.src.toks[i].end;
            }
            boolean eol = false;
            if (rparenEnd >= 0) {
                for (Comment c : ff.trailing(v)) {
                    if (!c.ownLine && c.start > rparenEnd) {
                        eol = true;
                        break;
                    }
                }
            }
            if (eol) {
                ff.hard();
            } else if (cclFluent(layout) && (parenValue || v instanceof Call || v instanceof Subscript)) {
                ff.soft();
            }
            List<Comment> dangling = ff.dangling(e);
            int split = 0;
            if (!dangling.isEmpty()) {
                int dot = e.attr.start;
                for (int i = ff.src.nextIdx(v.end); i >= 0 && ff.src.toks[i].start < e.attr.start; i = ff.src.nextIdx(ff.src.toks[i].end)) {
                    if (ff.src.toks[i].is(".")) {
                        dot = ff.src.toks[i].start;
                        break;
                    }
                }
                while (split < dangling.size() && dangling.get(split).start < dot) {
                    split++;
                }
            }
            ff.danglingComments(dangling.subList(0, split));
            ff.tok(".");
            ff.danglingComments(dangling.subList(split, dangling.size()));
            ident(ff, e.attr);
        };
        if (layout0 == CCL_DEFAULT && cclFluent(layout)) {
            f.group(inner);
        } else {
            inner.fmt(f);
        }
    }

    static int needsParensAttribute(F f, Attribute e) {
        if (cclFluent(callChainLayout(f, e))) {
            return OP_MULTILINE;
        }
        if (f.hasDangling(e)) {
            return OP_ALWAYS;
        }
        if (f.parenthesized(e.value)) {
            for (Comment c : f.trailing(e.value)) {
                if (!c.ownLine) {
                    return OP_MULTILINE;
                }
            }
            return OP_NEVER;
        }
        return needsParentheses(f, e.value, e);
    }

    // ======================================================================= expr_call.rs

    static void fmtCall(F f, Call e, int layout0) {
        int layout = cclApply(f, layout0, e);
        Fmt inner = ff -> {
            Expr fn = e.func;
            boolean paren = ff.parenthesized(fn);
            fmtChainValue(ff, fn, fn instanceof Attribute ? cclDecrement(layout) : layout, paren);
            ff.danglingComments(ff.dangling(e));
            fmtArguments(ff, e.arguments);
        };
        if (cclFluent(layout) && layout0 == CCL_DEFAULT) {
            f.group(inner);
        } else {
            inner.fmt(f);
        }
    }

    static int needsParensCall(F f, Call e) {
        if (cclFluent(callChainLayout(f, e))) {
            return OP_MULTILINE;
        }
        if (f.hasDangling(e)) {
            return OP_ALWAYS;
        }
        if (f.parenthesized(e.func)) {
            return OP_NEVER;
        }
        return needsParentheses(f, e.func, e);
    }

    // ======================================================================= expr_subscript.rs

    static void fmtSubscript(F f, Subscript e, int layout0) {
        int layout = cclApply(f, layout0, e);
        List<Comment> dangling = f.dangling(e);
        Fmt inner = ff -> {
            Expr v = e.value;
            boolean paren = ff.parenthesized(v);
            fmtChainValue(ff, v, v instanceof Attribute ? cclDecrement(layout) : layout, paren);
            parenthesized(ff, "[", g -> {
                if (e.slice instanceof Seq && ((Seq) e.slice).kind == '(') {
                    g.node(e.slice, x -> fmtTuple(x, (Seq) e.slice, TP_PRESERVE));
                } else {
                    fmtExpr(g, e.slice);
                }
            }, "]", dangling, false);
        };
        if (layout0 == CCL_DEFAULT && cclFluent(layout)) {
            f.group(inner);
        } else {
            inner.fmt(f);
        }
    }

    static int needsParensSubscript(F f, Subscript e, Node parent) {
        if (cclFluent(callChainLayout(f, e))) {
            return OP_MULTILINE;
        }
        if (f.parenthesized(e.value)) {
            return OP_NEVER;
        }
        int p = needsParentheses(f, e.value, e);
        if (p == OP_BEST_FIT && parent instanceof FunctionDef && ((FunctionDef) parent).returns == e) {
            FunctionDef fd = (FunctionDef) parent;
            return parametersEmpty(fd.parameters) && !f.hasComments(fd.parameters) ? OP_MULTILINE : OP_NEVER;
        }
        return p;
    }

    // ======================================================================= binary_like.rs

    static final int PREC_NONE = 0, PREC_ATTRIBUTE = 1, PREC_EXPONENTIAL = 2, PREC_INVERSION = 3,
            PREC_MULTIPLICATIVE = 4, PREC_ADDITIVE = 5, PREC_SHIFT = 6, PREC_BITAND = 7, PREC_BITXOR = 8,
            PREC_BITOR = 9, PREC_COMPARATOR = 10, PREC_BOOLEAN = 11, PREC_CONDITIONAL = 12;

    static int binaryPrecedence(String op) {
        switch (op) {
            case "+":
            case "-":
                return PREC_ADDITIVE;
            case "*":
            case "@":
            case "/":
            case "%":
            case "//":
                return PREC_MULTIPLICATIVE;
            case "**":
                return PREC_EXPONENTIAL;
            case "<<":
            case ">>":
                return PREC_SHIFT;
            case "|":
                return PREC_BITOR;
            case "^":
                return PREC_BITXOR;
            case "&":
                return PREC_BITAND;
            default:
                throw new IllegalStateException("binary operator " + op);
        }
    }

    static final int OPERAND_LEFT = 0, OPERAND_MIDDLE = 1, OPERAND_RIGHT = 2;

    /** One operand of a flattened binary-like expression; only a left operand carries leading and only a right
     * operand trailing comments of the flattened node. */
    static final class Operand {
        final int kind;
        final Expr expr;
        final List<Comment> leading;
        final List<Comment> trailing;

        Operand(int kind, Expr expr, List<Comment> leading, List<Comment> trailing) {
            this.kind = kind;
            this.expr = expr;
            this.leading = leading;
            this.trailing = trailing;
        }
    }

    static final class Operator {
        final String symbol;
        final int precedence;
        final List<Comment> trailing;

        Operator(String symbol, int precedence, List<Comment> trailing) {
            this.symbol = symbol;
            this.precedence = precedence;
            this.trailing = trailing;
        }
    }

    static void flattenBinary(F f, Expr e, List<Comment> leading, List<Comment> trailing, ArrayList<Object> parts) {
        if (e instanceof BinOp) {
            BinOp b = (BinOp) e;
            flattenOperand(f, new Operand(OPERAND_LEFT, b.left, leading, null), parts);
            parts.add(new Operator(b.op, binaryPrecedence(b.op), f.dangling(b)));
            flattenOperand(f, new Operand(OPERAND_RIGHT, b.right, null, trailing), parts);
        } else if (e instanceof Compare) {
            Compare c = (Compare) e;
            flattenOperand(f, new Operand(OPERAND_LEFT, c.left, leading, null), parts);
            int n = c.comparators.size();
            for (int i = 0; i < n - 1; i++) {
                parts.add(new Operator(c.ops.get(i), PREC_COMPARATOR, NO_COMMENTS));
                flattenOperand(f, new Operand(OPERAND_MIDDLE, c.comparators.get(i), null, null), parts);
            }
            if (n > 0) {
                parts.add(new Operator(c.ops.get(n - 1), PREC_COMPARATOR, NO_COMMENTS));
                flattenOperand(f, new Operand(OPERAND_RIGHT, c.comparators.get(n - 1), null, trailing), parts);
            }
        } else {
            BoolOp b = (BoolOp) e;
            int n = b.values.size();
            flattenOperand(f, new Operand(OPERAND_LEFT, b.values.get(0), leading, null), parts);
            parts.add(new Operator(b.op, PREC_BOOLEAN, NO_COMMENTS));
            for (int i = 1; i < n - 1; i++) {
                flattenOperand(f, new Operand(OPERAND_MIDDLE, b.values.get(i), null, null), parts);
                parts.add(new Operator(b.op, PREC_BOOLEAN, NO_COMMENTS));
            }
            flattenOperand(f, new Operand(OPERAND_RIGHT, b.values.get(n - 1), null, trailing), parts);
        }
    }

    static void flattenOperand(F f, Operand o, ArrayList<Object> parts) {
        Expr x = o.expr;
        if ((x instanceof BinOp || x instanceof Compare || x instanceof BoolOp) && !f.parenthesized(x)) {
            List<Comment> lead = o.kind == OPERAND_LEFT ? o.leading : f.leading(x);
            List<Comment> trail = o.kind == OPERAND_RIGHT ? o.trailing : f.trailing(x);
            flattenBinary(f, x, lead, trail, parts);
        } else {
            parts.add(o);
        }
    }

    /** The first non-trivia token in {@code [a, b)} is {@code op}. */
    static boolean firstTokenIs(F f, int a, int b, String op) {
        Tok t = f.src.first(a, b);
        return t != null && t.is(op);
    }

    static boolean hasUnparenthesizedLeadingComments(F f, Operand o) {
        if (o.kind == OPERAND_LEFT) {
            return !o.leading.isEmpty();
        }
        List<Comment> leading = f.leading(o.expr);
        if (f.parenthesized(o.expr)) {
            for (Comment c : leading) {
                if (!c.formatted && firstTokenIs(f, c.end, o.expr.start, "(")) {
                    return true;
                }
            }
            return false;
        }
        return !leading.isEmpty();
    }

    static boolean isSimplePowerOperand(Expr e) {
        if (e instanceof UnaryOp) {
            UnaryOp u = (UnaryOp) e;
            return !u.op.equals("not") && isSimplePowerOperand(u.operand);
        }
        if (e instanceof Num || e instanceof Name) {
            return true;
        }
        if (e instanceof Const) {
            return !((Const) e).value.equals("...");
        }
        if (e instanceof Attribute) {
            return isSimplePowerOperand(((Attribute) e).value);
        }
        return false;
    }

    static boolean isSimplePowerExpression(F f, Expr left, Expr right) {
        return isSimplePowerOperand(left) && isSimplePowerOperand(right) && !f.parenthesized(left)
                && !f.parenthesized(right);
    }

    static void fmtOperand(F f, Operand o) {
        Expr x = o.expr;
        if (!f.parenthesized(x)) {
            fmtExpr(f, x, P_NEVER);
            return;
        }
        List<Comment> leading = f.leading(x);
        int leadEnd = 0;
        for (int k = leading.size() - 1; k >= 0; k--) {
            Comment c = leading.get(k);
            if (!c.formatted && firstTokenIs(f, c.end, x.start, "(")) {
                leadEnd = k + 1;
                break;
            }
        }
        List<Comment> trailing = f.trailing(x);
        int trailStart = trailing.size();
        for (int k = 0; k < trailing.size(); k++) {
            Comment c = trailing.get(k);
            if (!c.formatted && firstTokenIs(f, x.end, c.start, ")")) {
                trailStart = k;
                break;
            }
        }
        List<Comment> after = trailing.subList(trailStart, trailing.size());
        for (Comment c : after) {
            c.formatted = true;
        }
        if (leadEnd > 0) {
            f.leadingComments(leading.subList(0, leadEnd));
        }
        fmtExpr(f, x, P_ALWAYS);
        for (Comment c : after) {
            c.formatted = false;
        }
        if (!after.isEmpty()) {
            f.trailingComments(after);
        }
    }

    static void fmtOperator(F f, Operator op) {
        f.tok(op.symbol);
        f.trailingComments(op.trailing);
    }

    /** FlatBinaryExpressionSlice::fmt over {@code parts[from, to)}: splits at the lowest-precedence operators. */
    static void fmtBinarySlice(F f, List<Object> parts, int from, int to) {
        if (to - from == 1) {
            fmtOperand(f, (Operand) parts.get(from));
            return;
        }
        int lowest = PREC_NONE;
        for (int i = from + 1; i < to; i += 2) {
            lowest = Math.max(lowest, ((Operator) parts.get(i)).precedence);
        }
        int last = -1;
        for (int i = from + 1; i < to; i += 2) {
            Operator op = (Operator) parts.get(i);
            if (op.precedence != lowest) {
                continue;
            }
            final int leftFrom = last < 0 ? from : last + 1;
            final int leftTo = i;
            Operand leftFirst = (Operand) parts.get(leftFrom);
            Operand leftLast = (Operand) parts.get(leftTo - 1);
            Operand rightFirst = (Operand) parts.get(i + 1);
            boolean isPow = op.symbol.equals("**") && isSimplePowerExpression(f, leftLast.expr, rightFirst.expr);
            if (leftFirst.kind == OPERAND_LEFT) {
                f.leadingComments(leftFirst.leading);
            }
            if (leftTo - leftFrom == 1) {
                fmtOperand(f, leftFirst);
            } else {
                ipoGroup(f, g -> fmtBinarySlice(g, parts, leftFrom, leftTo));
            }
            if (leftLast.kind == OPERAND_RIGHT) {
                f.trailingComments(leftLast.trailing);
            }
            if (isPow) {
                ipoSoft(f);
            } else {
                ipoSoftOrSpace(f);
            }
            fmtOperator(f, op);
            if (!op.trailing.isEmpty() || hasUnparenthesizedLeadingComments(f, rightFirst)) {
                f.hard();
            } else if (isPow) {
                ipoIfBreaks(f, g -> g.space());
            } else {
                f.space();
            }
            last = i;
        }
        final int rightFrom = last + 1;
        Operand rf = (Operand) parts.get(rightFrom);
        if (rf.kind == OPERAND_LEFT) {
            f.leadingComments(rf.leading);
        }
        if (to - rightFrom == 1) {
            fmtOperand(f, rf);
        } else {
            ipoGroup(f, g -> fmtBinarySlice(g, parts, rightFrom, to));
        }
    }

    static void ipoStart(F f) {
        if (f.level == NL_PAREN) {
            El g = new El(E_GROUP);
            g.mode = G_FLAT;
            f.buf.add(g);
        }
    }

    static void ipoEnd(F f) {
        if (f.level == NL_PAREN) {
            f.buf.add(END_GROUP);
        }
    }

    /** BinaryLike::fmt for a binary, compare or boolean expression. */
    static void fmtBinaryLike(F f, Expr e) {
        ArrayList<Object> parts = new ArrayList<>();
        flattenBinary(f, e, NO_COMMENTS, NO_COMMENTS, parts);
        int size = parts.size();
        if (e instanceof BoolOp) {
            ipoGroup(f, g -> fmtBinarySlice(g, parts, 0, size));
            return;
        }
        ArrayList<Integer> strings = new ArrayList<>();
        for (int i = 0; i < size; i += 2) {
            Expr x = ((Operand) parts.get(i)).expr;
            if (x instanceof Str && ((Str) x).implicit() && !f.parenthesized(x)) {
                strings.add(i);
            }
        }
        if (strings.isEmpty()) {
            ipoGroup(f, g -> fmtBinarySlice(g, parts, 0, size));
            return;
        }
        ipoStart(f);
        if (strings.get(0) != 0) {
            ipoStart(f);
        }
        int lastOp = -1;
        int next = 0;
        while (true) {
            if (next < strings.size()) {
                int index = strings.get(next++);
                Operand operand = (Operand) parts.get(index);
                Str string = (Str) operand.expr;
                if (index != 0) {
                    int leftOp = index - 1;
                    if (lastOp == leftOp) {
                        ipoEnd(f);
                    } else {
                        int leftFrom = lastOp < 0 ? 0 : lastOp + 1;
                        Operand leftFirst = (Operand) parts.get(leftFrom);
                        Operand leftLast = (Operand) parts.get(leftOp - 1);
                        Operator leftOperator = (Operator) parts.get(leftOp);
                        if (leftFirst.kind == OPERAND_LEFT) {
                            f.leadingComments(leftFirst.leading);
                        }
                        fmtBinarySlice(f, parts, leftFrom, leftOp);
                        if (leftLast.kind == OPERAND_RIGHT) {
                            f.trailingComments(leftLast.trailing);
                        }
                        ipoSoftOrSpace(f);
                        fmtOperator(f, leftOperator);
                        ipoEnd(f);
                        if (hasUnparenthesizedLeadingComments(f, operand) || !leftOperator.trailing.isEmpty()) {
                            f.hard();
                        } else {
                            f.space();
                        }
                    }
                    if (operand.kind == OPERAND_LEFT) {
                        f.leadingComments(operand.leading);
                    }
                    f.leadingComments(f.leading(string));
                    fmtImplicitConcat(f, string);
                    f.trailingComments(f.trailing(string));
                    if (operand.kind == OPERAND_RIGHT) {
                        f.trailingComments(operand.trailing);
                    }
                    f.w(SUFFIX_BOUNDARY);
                } else {
                    f.leadingComments(f.leading(string));
                    fmtImplicitConcat(f, string);
                    f.trailingComments(f.trailing(string));
                }
                int rightOp = index + 1;
                if (rightOp < size) {
                    Operator rightOperator = (Operator) parts.get(rightOp);
                    ipoStart(f);
                    Operand rightOperand = (Operand) parts.get(rightOp + 1);
                    boolean rightLeading = hasUnparenthesizedLeadingComments(f, rightOperand);
                    if (rightLeading) {
                        f.space();
                    } else {
                        ipoSoftOrSpace(f);
                    }
                    fmtOperator(f, rightOperator);
                    if ((rightLeading && !f.parenthesized(rightOperand.expr)) || !rightOperator.trailing.isEmpty()) {
                        f.hard();
                    } else {
                        f.space();
                    }
                    lastOp = rightOp;
                } else {
                    break;
                }
            } else {
                if (lastOp >= 0) {
                    fmtBinarySlice(f, parts, lastOp + 1, size);
                    ipoEnd(f);
                }
                break;
            }
        }
        ipoEnd(f);
    }

    // ======================================================================= expression dispatch

    /** FormatNodeRule::fmt_fields of every expression kind with its default options. */
    static void fmtExprFields(F f, Expr e) {
        if (e instanceof Name) {
            f.txt(f.s.substring(e.start, e.end));
        } else if (e instanceof Attribute) {
            fmtAttribute(f, (Attribute) e, CCL_DEFAULT);
        } else if (e instanceof Call) {
            fmtCall(f, (Call) e, CCL_DEFAULT);
        } else if (e instanceof Subscript) {
            fmtSubscript(f, (Subscript) e, CCL_DEFAULT);
        } else if (e instanceof Const) {
            f.tok(((Const) e).value);
        } else if (e instanceof Num) {
            fmtNumber(f, (Num) e);
        } else if (e instanceof Str) {
            fmtStr(f, (Str) e, SK_DEFAULT);
        } else if (e instanceof BinOp || e instanceof BoolOp || e instanceof Compare) {
            fmtBinaryLike(f, e);
        } else if (e instanceof UnaryOp) {
            fmtUnary(f, (UnaryOp) e);
        } else if (e instanceof Seq) {
            Seq q = (Seq) e;
            if (q.kind == '(') {
                fmtTuple(f, q, TP_DEFAULT);
            } else {
                fmtListOrSet(f, q);
            }
        } else if (e instanceof DictE) {
            fmtDict(f, (DictE) e);
        } else if (e instanceof Comp) {
            fmtComp(f, (Comp) e, false);
        } else if (e instanceof IfExp) {
            fmtIfExp(f, (IfExp) e, false);
        } else if (e instanceof Lambda) {
            fmtLambda(f, (Lambda) e, false);
        } else if (e instanceof Named) {
            fmtNamed(f, (Named) e);
        } else if (e instanceof Await) {
            f.tok("await");
            f.space();
            maybeParenthesize(f, ((Await) e).value, e, PZ_IF_BREAKS);
        } else if (e instanceof Yield) {
            Yield y = (Yield) e;
            f.tok(y.from ? "yield from" : "yield");
            if (y.value != null) {
                f.space();
                maybeParenthesize(f, y.value, e, PZ_OPTIONAL);
            }
        } else if (e instanceof Starred) {
            f.tok("*");
            f.danglingComments(f.dangling(e));
            fmtExpr(f, ((Starred) e).value);
        } else if (e instanceof Slice) {
            fmtSlice(f, (Slice) e);
        } else {
            throw new IllegalStateException("unknown expression " + e.getClass().getSimpleName());
        }
    }

    /** NeedsParentheses for Expr. */
    static int needsParentheses(F f, Expr e, Node parent) {
        if (e instanceof Name || e instanceof Num || e instanceof Const) {
            return OP_BEST_FIT;
        } else if (e instanceof Attribute) {
            return needsParensAttribute(f, (Attribute) e);
        } else if (e instanceof Call) {
            return needsParensCall(f, (Call) e);
        } else if (e instanceof Subscript) {
            return needsParensSubscript(f, (Subscript) e, parent);
        } else if (e instanceof Str) {
            return needsParensStr(f, (Str) e, parent);
        } else if (e instanceof BoolOp || e instanceof IfExp || e instanceof Lambda) {
            return parent instanceof Await ? OP_ALWAYS : OP_MULTILINE;
        } else if (e instanceof BinOp) {
            BinOp b = (BinOp) e;
            if (parent instanceof Await) {
                return OP_ALWAYS;
            }
            if (b.left instanceof Str) {
                Str s = (Str) b.left;
                return !s.implicit() && strIsMultiline(f, s) && hasParentheses(f, b.right) != OWN_NONE
                        && !f.hasDangling(b) && !f.hasComments(s) && !f.hasComments(b.right)
                        ? OP_NEVER : OP_MULTILINE;
            }
            return OP_MULTILINE;
        } else if (e instanceof Compare) {
            Compare c = (Compare) e;
            if (parent instanceof Await) {
                return OP_ALWAYS;
            }
            if (c.left instanceof Str) {
                Str s = (Str) c.left;
                Expr right = c.comparators.isEmpty() ? null : c.comparators.get(0);
                return !s.implicit() && strIsMultiline(f, s) && !f.hasComments(s) && right != null
                        && hasParentheses(f, right) != OWN_NONE && !f.hasComments(right) ? OP_NEVER : OP_MULTILINE;
            }
            return OP_MULTILINE;
        } else if (e instanceof UnaryOp) {
            UnaryOp u = (UnaryOp) e;
            if (parent instanceof Await || unaryNeedsLineBreak(f, u)) {
                return OP_ALWAYS;
            }
            if (f.parenthesized(u.operand)) {
                return OP_NEVER;
            }
            if (f.hasComments(u.operand)) {
                return OP_ALWAYS;
            }
            return needsParentheses(f, u.operand, u);
        } else if (e instanceof Named) {
            return parent instanceof AnnAssign || parent instanceof Assign || parent instanceof AugAssign
                    || parent instanceof Assert || parent instanceof Return || parent instanceof ExceptHandler
                    || parent instanceof WithItem || parent instanceof Yield || parent instanceof Await
                    || parent instanceof Delete || parent instanceof For || parent instanceof FunctionDef
                    || parent instanceof Lambda ? OP_ALWAYS : OP_MULTILINE;
        } else if (e instanceof Seq || e instanceof DictE) {
            return OP_NEVER;
        } else if (e instanceof Comp) {
            return ((Comp) e).kind == '(' && parent instanceof Await ? OP_ALWAYS : OP_NEVER;
        } else if (e instanceof Await) {
            Expr v = ((Await) e).value;
            if (parent instanceof Await || isTypeAnnotationOf(e, parent)) {
                return OP_ALWAYS;
            }
            if (f.parenthesized(v)) {
                return OP_NEVER;
            }
            return needsParentheses(f, v, e);
        } else if (e instanceof Yield) {
            Expr v = ((Yield) e).value;
            if (isTypeAnnotationOf(e, parent)) {
                return OP_ALWAYS;
            }
            if (parent instanceof Assign || parent instanceof AnnAssign || parent instanceof AugAssign) {
                if (v == null || f.parenthesized(v)) {
                    return OP_NEVER;
                }
                int p = needsParentheses(f, v, e);
                return p == OP_BEST_FIT ? OP_NEVER : p;
            }
            return OP_ALWAYS;
        } else if (e instanceof Starred || e instanceof Slice) {
            return OP_MULTILINE;
        }
        throw new IllegalStateException("unknown expression " + e.getClass().getSimpleName());
    }

    /** is_type_annotation_of. */
    static boolean isTypeAnnotationOf(Expr e, Node parent) {
        if (parent instanceof AnnAssign) {
            return ((AnnAssign) parent).annotation == e;
        }
        return parent instanceof FunctionDef && ((FunctionDef) parent).returns == e;
    }

    // ======================================================================= expr_number_literal.rs

    static void fmtNumber(F f, Num e) {
        String t = f.s.substring(e.start, e.end);
        char last = t.charAt(t.length() - 1);
        boolean radix = t.length() > 1 && t.charAt(0) == '0' && "xXoObB".indexOf(t.charAt(1)) >= 0;
        if (radix) {
            f.txt(normalizeInteger(t));
        } else if (last == 'j' || last == 'J') {
            f.txt(normalizeFloat(t.substring(0, t.length() - 1)));
            f.tok("j");
        } else if (t.indexOf('.') >= 0 || t.indexOf('e') >= 0 || t.indexOf('E') >= 0) {
            f.txt(normalizeFloat(t));
        } else {
            f.txt(normalizeInteger(t));
        }
    }

    static String normalizeInteger(String in) {
        if (in.length() < 2 || in.charAt(0) != '0') {
            return in;
        }
        char c = in.charAt(1);
        StringBuilder out = new StringBuilder(in.length());
        out.append('0').append(Character.toLowerCase(c));
        boolean hex = c == 'x' || c == 'X';
        for (int i = 2; i < in.length(); i++) {
            char d = in.charAt(i);
            out.append(hex && d >= 'a' && d <= 'f' ? Character.toUpperCase(d) : d);
        }
        return out.toString();
    }

    static String normalizeFloat(String in) {
        StringBuilder out = new StringBuilder(in.length() + 2);
        int i = 0;
        boolean prevDot = false;
        if (in.startsWith(".")) {
            out.append("0.");
            i = 1;
            prevDot = true;
        }
        for (; i < in.length(); i++) {
            char c = in.charAt(i);
            if (c == 'e' || c == 'E') {
                if (prevDot) {
                    out.append('0');
                }
                out.append('e');
                i++;
                if (i < in.length() && in.charAt(i) == '+') {
                    i++;
                }
                out.append(in, i, in.length());
                return out.toString();
            }
            prevDot = c == '.';
            out.append(c);
        }
        if (prevDot) {
            out.append('0');
        }
        return out.toString();
    }

    // ======================================================================= expr_unary_op.rs

    static void fmtUnary(F f, UnaryOp e) {
        f.tok(e.op);
        f.trailingComments(f.dangling(e));
        if (unaryNeedsLineBreak(f, e)) {
            f.hard();
        } else if (e.op.equals("not")) {
            f.space();
        }
        if (e.operand instanceof BinOp && ((BinOp) e.operand).op.equals("**")) {
            fmtExpr(f, e.operand, P_ALWAYS);
        } else {
            fmtExpr(f, e.operand);
        }
    }

    static boolean unaryNeedsLineBreak(F f, UnaryOp e) {
        List<Comment> lead = f.leading(e.operand);
        if (lead.isEmpty()) {
            return false;
        }
        if (!f.parenthesized(e.operand)) {
            return true;
        }
        int[] range = outerParentheses(f, e.operand);
        if (range != null) {
            for (Comment c : lead) {
                if (c.start < range[0]) {
                    return true;
                }
            }
        }
        return false;
    }

    // ======================================================================= expr_named.rs

    static void fmtNamed(F f, Named e) {
        List<Comment> dangling = f.dangling(e);
        f.group(g -> {
            fmtExpr(g, e.target);
            ipoSoftOrSpace(g);
        });
        f.tok(":=");
        if (dangling.isEmpty()) {
            f.space();
        } else {
            f.danglingComments(dangling);
            f.hard();
        }
        fmtExpr(f, e.value);
    }

    // ======================================================================= expr_if.rs

    static void fmtIfExp(F f, IfExp e, boolean nested) {
        Fmt inner = g -> {
            fmtExpr(g, e.body);
            ipoSoftOrSpace(g);
            g.leadingComments(g.leading(e.test));
            g.tok("if");
            g.space();
            fmtExpr(g, e.test);
            ipoSoftOrSpace(g);
            g.leadingComments(g.leading(e.orelse));
            g.tok("else");
            g.space();
            if (e.orelse instanceof IfExp && !g.parenthesized(e.orelse)) {
                g.node(e.orelse, x -> fmtIfExp(x, (IfExp) e.orelse, true));
            } else {
                ipoGroup(g, x -> fmtExpr(x, e.orelse));
            }
        };
        if (nested) {
            inner.fmt(f);
        } else {
            ipoGroup(f, inner);
        }
    }

    // ======================================================================= expr_lambda.rs

    static void fmtLambda(F f, Lambda e, boolean assignment) {
        List<Comment> dangling = f.dangling(e);
        f.tok("lambda");
        List<Comment> header = dangling;
        Parameters ps = e.parameters;
        if (ps != null) {
            int split = 0;
            while (split < dangling.size() && dangling.get(split).end < ps.start) {
                split++;
            }
            List<Comment> before = dangling.subList(0, split);
            header = dangling.subList(split, dangling.size());
            if (before.isEmpty()) {
                if (f.hasLeading(ps)) {
                    f.hard();
                } else {
                    f.space();
                }
            } else {
                f.danglingComments(before);
            }
            if (!containsComments(f, ps)) {
                removeSoftLines(f, g -> fmtParametersNode(g, ps, PP_NEVER));
            } else {
                fmtParametersNode(f, ps, PP_NEVER);
            }
        }
        f.tok(":");
        if (header.isEmpty()) {
            f.space();
        }
        List<Comment> headerComments = header;
        int needs = needsParentheses(f, e.body, e);
        Fmt body = g -> fmtLambdaBody(g, e.body, headerComments, needs);
        if (assignment) {
            f.fitsExpanded(-1, 0, body);
        } else {
            body.fmt(f);
        }
    }

    static void fmtLambdaBody(F f, Expr body, List<Comment> header, int needs) {
        if (!header.isEmpty()) {
            int split = 0;
            while (split < header.size() && !header.get(split).ownLine) {
                split++;
            }
            List<Comment> trailingHeader = header.subList(0, split);
            List<Comment> leadingBody = header.subList(split, header.size());
            if (f.parenthesized(body) && f.hasLeading(body)) {
                f.trailingComments(header);
                if (leadingBody.isEmpty()) {
                    f.space();
                } else {
                    f.hard();
                }
                fmtExpr(f, body, P_ALWAYS);
            } else {
                f.space();
                f.tok("(");
                f.trailingComments(trailingHeader);
                f.blockIndent(g -> {
                    g.leadingComments(leadingBody);
                    fmtExpr(g, body, P_NEVER);
                });
                f.tok(")");
            }
        } else if (f.hasLeading(body) || f.hasTrailingOwnLine(body)) {
            fmtExpr(f, body, P_ALWAYS);
        } else if (needs == OP_ALWAYS) {
            fmtExpr(f, body, P_ALWAYS);
        } else if (needs == OP_MULTILINE) {
            parenthesizeIfExpands(f, true, g -> fmtExpr(g, body, P_NEVER));
        } else if (body instanceof Call || body instanceof Subscript) {
            Memo un = new Memo(g -> fmtExpr(g, body, P_NEVER));
            if (un.willBreak(f)) {
                f.expandParent();
            }
            f.bestFitting(false, un, g -> g.group(0, true, un), g -> {
                g.tok("(");
                g.blockIndent(un);
                g.tok(")");
            });
        } else if (hasOwnParentheses(f, body) != OWN_NONE) {
            fmtExpr(f, body);
        } else {
            parenthesizeIfExpands(f, true, g -> fmtExpr(g, body, P_NEVER));
        }
    }

    /** CommentsMap::contains_comments: whether {@code n} or any descendant has comments. */
    static boolean containsComments(F f, Node n) {
        if (f.hasComments(n)) {
            return true;
        }
        boolean[] found = {false};
        n.each(c -> {
            if (!found[0] && c != null && containsComments(f, c)) {
                found[0] = true;
            }
        });
        return found[0];
    }

    // ======================================================================= expr_dict.rs

    static void fmtDict(F f, DictE e) {
        List<Comment> dangling = f.dangling(e);
        if (e.values.isEmpty()) {
            emptyParenthesized(f, "{", dangling, "}");
            return;
        }
        int firstStart = e.keys.get(0) != null ? e.keys.get(0).start : e.values.get(0).start;
        int split = 0;
        while (split < dangling.size() && dangling.get(split).end < firstStart) {
            split++;
        }
        List<Comment> open = dangling.subList(0, split);
        List<Comment> kv = dangling.subList(split, dangling.size());
        parenthesized(f, "{", g -> {
            Joiner j = new Joiner(g, e.end);
            List<Comment> rest = kv;
            for (int i = 0; i < e.values.size(); i++) {
                Expr key = e.keys.get(i);
                Expr value = e.values.get(i);
                int p = 0;
                while (p < rest.size() && rest.get(p).start < value.end) {
                    p++;
                }
                List<Comment> mine = rest.subList(0, p);
                rest = rest.subList(p, rest.size());
                j.entry(value.end, x -> fmtKeyValue(x, key, value, mine), SOFT_OR_SPACE);
            }
            j.finish();
        }, "}", open, false);
    }

    static void fmtKeyValue(F f, Expr key, Expr value, List<Comment> comments) {
        if (key != null) {
            f.group(g -> {
                fmtExpr(g, key);
                g.tok(":");
                if (comments.isEmpty()) {
                    g.space();
                } else {
                    g.danglingComments(comments);
                }
                fmtExpr(g, value);
            });
        } else {
            f.leadingComments(f.leading(value));
            f.group(g -> {
                g.tok("**");
                fmtExpr(g, value);
            });
        }
    }

    // ======================================================================= expr_list.rs / expr_set.rs

    static void fmtListOrSet(F f, Seq e) {
        List<Comment> dangling = f.dangling(e);
        String open = e.kind == '[' ? "[" : "{";
        String close = e.kind == '[' ? "]" : "}";
        if (e.elts.isEmpty()) {
            emptyParenthesized(f, open, dangling, close);
            return;
        }
        parenthesized(f, open, g -> fmtSequence(g, e), close, dangling, false);
    }

    /** join_comma_separated(end).nodes(elts).finish(). */
    static void fmtSequence(F f, Seq e) {
        Joiner j = new Joiner(f, e.end);
        for (Expr x : e.elts) {
            j.entry(x, g -> fmtExpr(g, x));
        }
        j.finish();
    }

    // ======================================================================= expr_tuple.rs

    static final int TP_DEFAULT = 0, TP_PRESERVE = 1, TP_OPTIONAL_PARENTHESES = 2, TP_NEVER = 3,
            TP_NEVER_PRESERVE = 4;

    static void fmtTuple(F f, Seq e, int mode) {
        List<Comment> dangling = f.dangling(e);
        List<Expr> elts = e.elts;
        if (elts.isEmpty()) {
            emptyParenthesized(f, "(", dangling, ")");
        } else if (elts.size() == 1) {
            Expr single = elts.get(0);
            if (mode == TP_PRESERVE && !e.parenthesized) {
                fmtExpr(f, single);
                if (f.hasTrailingComma(single.end, e.end)) {
                    f.tok(",");
                }
            } else {
                parenthesized(f, "(", g -> {
                    fmtExpr(g, single);
                    g.tok(",");
                }, ")", dangling, false);
            }
        } else if (e.parenthesized && !(mode == TP_NEVER_PRESERVE && dangling.isEmpty())) {
            parenthesized(f, "(", g -> fmtSequence(g, e), ")", dangling, false);
        } else if (mode == TP_NEVER) {
            boolean first = true;
            for (Expr x : elts) {
                if (!first) {
                    f.group(g -> {
                        g.tok(",");
                        g.space();
                    });
                }
                first = false;
                fmtExpr(f, x);
            }
        } else if (mode == TP_PRESERVE) {
            f.group(g -> fmtSequence(g, e));
        } else if (mode == TP_NEVER_PRESERVE || (mode == TP_OPTIONAL_PARENTHESES && elts.size() == 2)) {
            optionalParentheses(f, g -> fmtSequence(g, e));
        } else {
            parenthesizeIfExpands(f, true, g -> fmtSequence(g, e));
        }
    }

    // ======================================================================= expr_*_comp.rs, expr_generator.rs

    /** List, set and dict comprehensions and generators; {@code preserve} is GeneratorExpParentheses::Preserve. */
    static void fmtComp(F f, Comp e, boolean preserve) {
        List<Comment> dangling = f.dangling(e);
        Fmt generators = g -> {
            boolean first = true;
            for (Comprehension c : e.generators) {
                if (!first) {
                    g.softOrSpace();
                }
                first = false;
                g.node(c, x -> fmtComprehension(x, c));
            }
        };
        if (e.kind == 'd') {
            Expr key = e.elt;
            Expr firstExpr = key != null ? key : e.value;
            int split = 0;
            while (split < dangling.size() && dangling.get(split).end < firstExpr.start) {
                split++;
            }
            List<Comment> open = dangling.subList(0, split);
            List<Comment> kv = dangling.subList(split, dangling.size());
            parenthesized(f, "{", g -> g.group(x -> {
                if (key != null) {
                    x.group(y -> fmtExpr(y, key));
                    x.tok(":");
                    if (kv.isEmpty()) {
                        x.space();
                    } else {
                        x.danglingComments(kv);
                    }
                } else {
                    x.tok("**");
                    List<Comment> lead = x.leading(e.value);
                    if (!lead.isEmpty()) {
                        if (lead.get(0).ownLine) {
                            x.hard();
                        } else {
                            x.space();
                            x.space();
                        }
                    }
                }
                fmtExpr(x, e.value);
                x.softOrSpace();
                generators.fmt(x);
            }), "}", open, false);
            return;
        }
        Fmt body = g -> {
            g.group(x -> fmtExpr(x, e.elt));
            g.softOrSpace();
            generators.fmt(g);
        };
        if (e.kind == '(' && preserve && dangling.isEmpty() && !e.parenthesized) {
            body.fmt(f);
            return;
        }
        String open = e.kind == '[' ? "[" : e.kind == '{' ? "{" : "(";
        String close = e.kind == '[' ? "]" : e.kind == '{' ? "}" : ")";
        parenthesized(f, open, g -> g.group(body), close, dangling, false);
    }

    // ======================================================================= expr_slice.rs

    static void fmtSlice(F f, Slice e) {
        Tok[] colons = sliceColons(f.src, e);
        List<Comment> dangling = f.dangling(e);
        int a = 0;
        while (a < dangling.size() && dangling.get(a).start < colons[0].start) {
            a++;
        }
        int b = a;
        if (colons[1] != null) {
            while (b < dangling.size() && dangling.get(b).start < colons[1].start) {
                b++;
            }
        } else {
            b = dangling.size();
        }
        List<Comment> lowerComments = dangling.subList(0, a);
        List<Comment> upperComments = dangling.subList(a, b);
        List<Comment> stepComments = dangling.subList(b, dangling.size());
        boolean allSimple = simpleSliceExpr(e.lower) && simpleSliceExpr(e.upper) && simpleSliceExpr(e.step);
        if (e.lower != null) {
            fmtExpr(f, e.lower);
            f.w(SUFFIX_BOUNDARY);
        } else {
            f.danglingComments(lowerComments);
        }
        if (!allSimple && e.lower != null) {
            f.space();
        }
        f.tok(":");
        if (!allSimple && e.upper != null) {
            f.space();
        }
        if (e.upper != null) {
            sliceLeadingSpacing(f, f.leading(e.upper));
            fmtExpr(f, e.upper);
            f.w(SUFFIX_BOUNDARY);
        } else {
            if (!upperComments.isEmpty() && upperComments.get(0).ownLine) {
                f.hard();
            }
            f.danglingComments(upperComments);
        }
        if (colons[1] != null) {
            if (!allSimple && (e.upper != null || e.step == null)) {
                f.space();
            }
            f.tok(":");
            if (!allSimple && e.step != null) {
                f.space();
            }
            if (e.step != null) {
                sliceLeadingSpacing(f, f.leading(e.step));
                fmtExpr(f, e.step);
            } else if (!stepComments.isEmpty()) {
                f.hard();
                f.danglingComments(stepComments);
            }
        }
    }

    static boolean simpleSliceExpr(Expr e) {
        if (e == null) {
            return true;
        }
        if (e instanceof UnaryOp && !((UnaryOp) e).op.equals("not")) {
            return simpleSliceExpr(((UnaryOp) e).operand);
        }
        return e instanceof Name || e instanceof Num || e instanceof Const
                || (e instanceof Str && (((Str) e).kind == S_STR || ((Str) e).kind == S_BYTES));
    }

    static void sliceLeadingSpacing(F f, List<Comment> lead) {
        if (!lead.isEmpty()) {
            if (lead.get(0).ownLine) {
                f.hard();
            } else {
                f.space();
                f.space();
            }
        }
    }

    // ======================================================================= RemoveSoftLinesBuffer

    /** Writes {@code c} through ruff's RemoveSoftLinesBuffer. */
    static void removeSoftLines(F f, Fmt c) {
        ArrayList<El> rec = f.record(c, null);
        int[] state = {0};
        IdentityHashMap<El, El> cache = new IdentityHashMap<>();
        for (El e : rec) {
            writeWithoutSoftLines(e, state, f.buf, cache);
        }
    }

    private static void writeWithoutSoftLines(El e, int[] state, List<El> out, IdentityHashMap<El, El> cache) {
        if (dropSoftLine(e, state)) {
            return;
        }
        if (e.k == E_LINE && e.n == L_SOFT_OR_SPACE) {
            out.add(SPACE);
        } else if (e.k == E_INTERNED) {
            out.add(cleanInterned(e, cache));
        } else if (e.k == E_BEST_FITTING) {
            for (El x : e.variants[0]) {
                writeWithoutSoftLines(x, state, out, cache);
            }
        } else {
            out.add(e);
        }
    }

    /** RemoveSoftLineBreaksState::should_drop; state[0] is the if-group-breaks nesting level. */
    private static boolean dropSoftLine(El e, int[] state) {
        if (state[0] == 0) {
            if (e.k == E_LINE && e.n == L_SOFT || e.k == E_BF_ENTRY || e.k == E_BF_ENTRY + 1 || e.k == E_COND + 1) {
                return true;
            }
            if (e.k == E_COND) {
                if (e.mode == EXPANDED) {
                    state[0] = 1;
                }
                return true;
            }
            return false;
        }
        if (e.k == E_COND) {
            state[0]++;
        } else if (e.k == E_COND + 1) {
            state[0]--;
        }
        return true;
    }

    private static El cleanInterned(El interned, IdentityHashMap<El, El> cache) {
        El cached = cache.get(interned);
        if (cached != null) {
            return cached;
        }
        El[] content = interned.content;
        int[] state = {0};
        ArrayList<El> cleaned = null;
        int restFrom = 0;
        for (int i = 0; i < content.length && cleaned == null; i++) {
            El e = content[i];
            if (e.k == E_LINE && e.n == L_SOFT_OR_SPACE) {
                cleaned = new ArrayList<>(Arrays.asList(content).subList(0, i));
                restFrom = i + 1;
            } else if (e.k == E_INTERNED) {
                El inner = cleanInterned(e, cache);
                if (inner != e) {
                    cleaned = new ArrayList<>(Arrays.asList(content).subList(0, i));
                    cleaned.add(inner);
                    restFrom = i + 1;
                }
            } else if (e.k == E_BEST_FITTING) {
                cleaned = new ArrayList<>(Arrays.asList(content).subList(0, i));
                restFrom = i;
            } else if (dropSoftLine(e, state)) {
                cleaned = new ArrayList<>(Arrays.asList(content).subList(0, i));
                restFrom = i + 1;
            }
        }
        El result = interned;
        if (cleaned != null) {
            ArrayDeque<El> stack = new ArrayDeque<>();
            for (int i = content.length - 1; i >= restFrom; i--) {
                stack.push(content[i]);
            }
            while (!stack.isEmpty()) {
                El e = stack.pop();
                if (dropSoftLine(e, state)) {
                    continue;
                }
                if (e.k == E_LINE && e.n == L_SOFT_OR_SPACE) {
                    cleaned.add(SPACE);
                } else if (e.k == E_INTERNED) {
                    cleaned.add(cleanInterned(e, cache));
                } else if (e.k == E_BEST_FITTING) {
                    El[] flat = e.variants[0];
                    for (int i = flat.length - 1; i >= 0; i--) {
                        stack.push(flat[i]);
                    }
                } else {
                    cleaned.add(e);
                }
            }
            result = new El(E_INTERNED);
            result.content = cleaned.toArray(new El[0]);
        }
        cache.put(interned, result);
        return result;
    }

    // ======================================================================= other/arguments.rs

    /** FormatArguments including the node's own comments. */
    static void fmtArguments(F f, Arguments a) {
        f.node(a, g -> fmtArgumentsFields(g, a));
    }

    static void fmtArgumentsFields(F f, Arguments a) {
        List<Comment> dangling = f.dangling(a);
        if (a.ordered.isEmpty()) {
            emptyParenthesized(f, "(", dangling, ")");
            return;
        }
        Fmt all = g -> {
            Joiner j = new Joiner(g, a.end);
            if (a.args.size() == 1 && a.keywords.isEmpty()) {
                Expr arg = a.args.get(0);
                if (arg instanceof Comp && ((Comp) arg).kind == '(') {
                    j.entry(arg, x -> x.node(arg, y -> fmtComp(y, (Comp) arg, true)));
                } else {
                    int p = singleArgumentParenthesized(g, arg, a.end) ? P_ALWAYS : P_NEVER;
                    j.entry(arg, x -> fmtExpr(x, arg, p));
                }
            } else {
                for (Node n : a.ordered) {
                    if (n instanceof Keyword) {
                        j.entry(n, x -> x.node(n, y -> fmtKeyword(y, (Keyword) n)));
                    } else {
                        j.entry(n, x -> fmtExpr(x, (Expr) n));
                    }
                }
            }
            j.finish();
        };
        parenthesized(f, "(", g -> g.group(all), ")", dangling, argumentsHuggable(f, a));
    }

    static boolean singleArgumentParenthesized(F f, Expr arg, int callEnd) {
        boolean seen = false;
        Src src = f.src;
        for (int i = src.nextIdx(arg.end); i >= 0 && src.toks[i].start < callEnd; i = src.nextIdx(src.toks[i].end)) {
            Tok t = src.toks[i];
            if (t.is(")")) {
                if (seen) {
                    return true;
                }
                seen = true;
            } else if (!t.is(",")) {
                break;
            }
        }
        return false;
    }

    static boolean argumentsHuggable(F f, Arguments a) {
        Expr arg;
        if (a.args.size() == 1 && a.keywords.isEmpty()) {
            arg = a.args.get(0);
        } else if (a.args.isEmpty() && a.keywords.size() == 1 && a.keywords.get(0).arg == null
                && !f.hasComments(a.keywords.get(0))) {
            arg = a.keywords.get(0).value;
        } else {
            return false;
        }
        if (!(arg instanceof Str && huggableStringArgument(f, (Str) arg, a))) {
            return false;
        }
        if (f.hasLeading(arg) || f.hasTrailing(arg)) {
            return false;
        }
        return !f.hasTrailingComma(arg.end, a.end);
    }

    static boolean huggableStringArgument(F f, Str s, Arguments a) {
        if (s.implicit() || !strIsMultiline(f, s) || !partTripleQuoted(f, s.parts.get(0))) {
            return false;
        }
        String between = f.s.substring(a.start + 1, s.start);
        int e = between.length();
        while (e > 0 && pyWs(between.charAt(e - 1))) {
            e--;
        }
        return !(e > 0 && (between.charAt(e - 1) == '\n' || between.charAt(e - 1) == '\r'));
    }

    // ======================================================================= other/keyword.rs

    static void fmtKeyword(F f, Keyword k) {
        if (k.arg != null) {
            ident(f, k.arg);
            f.tok("=");
        } else {
            f.tok("**");
        }
        fmtExpr(f, k.value);
    }

    // ======================================================================= other/comprehension.rs

    static void fmtComprehension(F f, Comprehension c) {
        if (c.isAsync) {
            f.tok("async");
            f.space();
        }
        List<Comment> dangling = f.dangling(c);
        int a = 0;
        while (a < dangling.size() && dangling.get(a).end < c.target.start) {
            a++;
        }
        Tok in = f.src.first(c.target.end, c.iter.start);
        while (in != null && !in.is("in")) {
            in = f.src.next(in.end);
        }
        int inStart = in != null ? in.start : c.iter.start;
        int b = a;
        while (b < dangling.size() && dangling.get(b).end < inStart) {
            b++;
        }
        int d = b;
        while (d < dangling.size() && dangling.get(d).start < c.iter.start) {
            d++;
        }
        List<Comment> beforeTarget = dangling.subList(0, a);
        List<Comment> beforeIn = dangling.subList(a, b);
        List<Comment> trailingIn = dangling.subList(b, d);
        List<Comment> ifComments = dangling.subList(d, dangling.size());
        f.tok("for");
        f.trailingComments(beforeTarget);
        comprehensionSpacer(f, c.target, !(c.target instanceof Seq && ((Seq) c.target).kind == '('));
        if (c.target instanceof Seq && ((Seq) c.target).kind == '(') {
            f.node(c.target, x -> fmtTuple(x, (Seq) c.target, TP_NEVER));
        } else {
            fmtExpr(f, c.target);
        }
        if (beforeIn.isEmpty()) {
            f.space();
        } else {
            f.softOrSpace();
        }
        f.leadingComments(beforeIn);
        f.tok("in");
        f.trailingComments(trailingIn);
        comprehensionSpacer(f, c.iter, true);
        fmtExpr(f, c.iter);
        if (!c.ifs.isEmpty()) {
            f.softOrSpace();
            List<Comment> rest = ifComments;
            boolean first = true;
            for (Expr cond : c.ifs) {
                int p = 0;
                while (p < rest.size() && rest.get(p).start < cond.start) {
                    p++;
                }
                List<Comment> mine = rest.subList(0, p);
                rest = rest.subList(p, rest.size());
                int q = 0;
                while (q < mine.size() && mine.get(q).ownLine) {
                    q++;
                }
                if (!first) {
                    f.softOrSpace();
                }
                first = false;
                f.leadingComments(mine.subList(0, q));
                f.tok("if");
                f.trailingComments(mine.subList(q, mine.size()));
                comprehensionSpacer(f, cond, true);
                fmtExpr(f, cond);
            }
        }
    }

    static void comprehensionSpacer(F f, Expr e, boolean preserveParentheses) {
        boolean leading = f.hasLeading(e);
        boolean parenthesized = preserveParentheses && f.parenthesized(e);
        if (leading && !parenthesized) {
            f.softOrSpace();
        } else {
            f.space();
        }
    }

    // ======================================================================= other/parameters.rs

    static final int PP_DEFAULT = 0, PP_NEVER = 1;

    /** FormatParameters (with the node's own comments). */
    static void fmtParametersNode(F f, Parameters ps, int mode) {
        f.node(ps, g -> fmtParameters(g, ps, mode));
    }

    static int parameterCount(Parameters ps) {
        return ps.posonly.size() + ps.args.size() + (ps.vararg != null ? 1 : 0) + ps.kwonly.size()
                + (ps.kwarg != null ? 1 : 0);
    }

    static void fmtParameters(F f, Parameters ps, int mode) {
        int[][] sep = Placement.parameterSeparators(f.src, ps);
        List<Comment> dangling = f.dangling(ps);
        int parenEnd = 0;
        if (!dangling.isEmpty() && !dangling.get(0).ownLine) {
            Src src = f.src;
            int i = src.nextIdx(ps.start);
            while (i >= 0 && src.toks[i].start < dangling.get(0).start
                    && (src.toks[i].is("(") || src.toks[i].is("[") || src.toks[i].is("{"))) {
                i = src.nextIdx(src.toks[i].end);
            }
            if (i < 0 || src.toks[i].start >= dangling.get(0).start) {
                parenEnd = 1;
            }
        }
        List<Comment> parenDangling = dangling.subList(0, parenEnd);
        List<Comment> paramDangling = dangling.subList(parenEnd, dangling.size());
        Fmt inner = g -> {
            Node[] last = {null};
            int[] count = {0};
            Fmt separator = x -> {
                x.tok(",");
                if (x.isParenthesizedLevel()) {
                    x.softOrSpace();
                } else {
                    x.space();
                }
            };
            java.util.function.Consumer<Fmt> entry = c -> {
                if (count[0]++ > 0) {
                    separator.fmt(g);
                }
                c.fmt(g);
            };
            for (ParamWD p : ps.posonly) {
                entry.accept(x -> x.node(p, y -> fmtParamWD(y, p)));
                last[0] = p;
            }
            int slashEnd = 0;
            if (!ps.posonly.isEmpty()) {
                while (slashEnd < paramDangling.size()) {
                    Comment c = paramDangling.get(slashEnd);
                    int loc = Placement.separatorLocation(sep[0], sep[1], c.start, c.ownLine);
                    if (loc != 1 && loc != 2) {
                        break;
                    }
                    slashEnd++;
                }
                List<Comment> slashComments = paramDangling.subList(0, slashEnd);
                entry.accept(x -> commentsAroundText(x, "/", slashComments));
            }
            for (ParamWD p : ps.args) {
                entry.accept(x -> x.node(p, y -> fmtParamWD(y, p)));
                last[0] = p;
            }
            if (ps.vararg != null) {
                Param v = ps.vararg;
                entry.accept(x -> {
                    x.leadingComments(x.leading(v));
                    x.tok("*");
                    x.node(v, y -> fmtParam(y, v));
                });
                last[0] = v;
            } else if (!ps.kwonly.isEmpty()) {
                List<Comment> starComments = paramDangling.subList(slashEnd, paramDangling.size());
                entry.accept(x -> commentsAroundText(x, "*", starComments));
            }
            for (ParamWD p : ps.kwonly) {
                entry.accept(x -> x.node(p, y -> fmtParamWD(y, p)));
                last[0] = p;
            }
            if (ps.kwarg != null) {
                Param k = ps.kwarg;
                entry.accept(x -> {
                    x.leadingComments(x.leading(k));
                    x.tok("**");
                    x.node(k, y -> fmtParam(y, k));
                });
                last[0] = k;
            }
            int commaFrom = last[0] == null ? -1 : last[0].end;
            if (last[0] != null && !ps.posonly.isEmpty() && ps.args.isEmpty() && ps.vararg == null
                    && ps.kwonly.isEmpty() && ps.kwarg == null) {
                // `def a(b, c, /)`: the slash has no node of its own, so skip `, /` first.
                Tok comma = g.src.next(commaFrom);
                Tok slash = comma == null ? null : g.src.next(comma.end);
                commaFrom = slash == null ? commaFrom : slash.end;
            }
            boolean trailingComma = last[0] != null && g.hasTrailingComma(commaFrom, ps.end);
            if (mode == PP_NEVER) {
                if (trailingComma) {
                    g.tok(",");
                }
            } else {
                g.ifBreaks(x -> x.tok(","));
                if (trailingComma) {
                    g.hard();
                }
            }
        };
        int n = parameterCount(ps);
        if (mode == PP_NEVER) {
            f.group(inner);
            f.danglingComments(dangling);
        } else if (n == 0) {
            f.withLevel(NL_PAREN, 0, g -> emptyParenthesized(g, "(", dangling, ")"));
        } else if (n == 1 && ps.posonly.isEmpty() && ps.kwonly.isEmpty()) {
            f.withLevel(NL_PAREN, 0, g -> {
                g.tok("(");
                g.danglingOpenParenthesisComments(parenDangling);
                g.softBlockIndent(inner);
                g.tok(")");
            });
        } else {
            f.withLevel(NL_PAREN, 0, g -> {
                g.tok("(");
                g.danglingOpenParenthesisComments(parenDangling);
                g.softBlockIndent(x -> x.group(inner));
                g.tok(")");
            });
        }
    }

    static void commentsAroundText(F f, String text, List<Comment> comments) {
        if (comments.isEmpty()) {
            f.tok(text);
            return;
        }
        int p = 0;
        while (p < comments.size() && comments.get(p).ownLine) {
            p++;
        }
        f.leadingComments(comments.subList(0, p));
        f.tok(text);
        f.trailingComments(comments.subList(p, comments.size()));
    }

    // ======================================================================= other/parameter.rs

    static void fmtParam(F f, Param p) {
        ident(f, p.name);
        if (p.annotation != null) {
            f.tok(":");
            if (f.hasLeading(p.annotation) && !f.parenthesized(p.annotation)) {
                f.hard();
            } else {
                f.space();
            }
            fmtExpr(f, p.annotation);
        }
    }

    // ======================================================================= other/parameter_with_default.rs

    static void fmtParamWD(F f, ParamWD p) {
        f.node(p.param, x -> fmtParam(x, p.param));
        if (p.dflt != null) {
            boolean annotated = p.param.annotation != null;
            boolean needsLineBreak = f.hasTrailing(p.param);
            List<Comment> lead = f.leading(p.dflt);
            if (!needsLineBreak && !lead.isEmpty()) {
                // A comment between `=` and the default needs a line break unless a parenthesis follows `=`.
                int commentStart = lead.get(0).start;
                Tok eq = f.src.firstAfterParens(p.param.end, commentStart);
                needsLineBreak = eq == null || f.src.first(eq.end, commentStart) == null;
            }
            if (annotated) {
                f.space();
            }
            f.tok("=");
            if (needsLineBreak) {
                f.hard();
            } else if (annotated) {
                f.space();
            }
            fmtExpr(f, p.dflt);
        }
    }

    // =====================================================================================
    // Strings: flags, quote selection and normalization (string/normalize.rs, string/mod.rs),
    // implicit concatenation (string/implicit.rs) and f/t-strings (other/f_string.rs,
    // other/interpolated_string*.rs). Target version is Python 3.9: no PEP 701 nesting.
    // =====================================================================================

    static final char Q_PRESERVE = 0;
    static final int SK_DEFAULT = 0, SK_DOCSTRING = 1;

    /** AnyStringFlags of one string part. */
    static final class SFlags {
        final boolean raw, upperR, bytes, fstr, tstr, triple;
        final char quote;

        SFlags(boolean raw, boolean upperR, boolean bytes, boolean fstr, boolean tstr, char quote, boolean triple) {
            this.raw = raw;
            this.upperR = upperR;
            this.bytes = bytes;
            this.fstr = fstr;
            this.tstr = tstr;
            this.quote = quote;
            this.triple = triple;
        }

        SFlags withQuote(char q) {
            return new SFlags(raw, upperR, bytes, fstr, tstr, q, triple);
        }

        char opposite() {
            return quote == '"' ? '\'' : '"';
        }

        boolean interpolated() {
            return fstr || tstr;
        }

        /** The normalized prefix: `u` dropped, lowercase except an uppercase `R`. */
        String prefix() {
            String k = bytes ? "b" : fstr ? "f" : tstr ? "t" : "";
            return raw ? (upperR ? "R" : "r") + k : k;
        }

        String quotes() {
            return triple ? (quote == '"' ? "\"\"\"" : "'''") : (quote == '"' ? "\"" : "'");
        }

        int quoteLen() {
            return triple ? 3 : 1;
        }
    }

    /** Enclosing f-string of the interpolation being formatted (InterpolatedStringContext). */
    static final class FStringCtx {
        final SFlags flags;
        final boolean multiline;

        FStringCtx(SFlags flags, boolean multiline) {
            this.flags = flags;
            this.multiline = multiline;
        }
    }

    static SFlags partFlags(String s, int start) {
        boolean raw = false, upper = false, bytes = false, fstr = false, tstr = false;
        int i = start;
        while (true) {
            char c = s.charAt(i);
            if (c == '\'' || c == '"') {
                break;
            }
            if (c == 'r') {
                raw = true;
            } else if (c == 'R') {
                raw = true;
                upper = true;
            } else if (c == 'b' || c == 'B') {
                bytes = true;
            } else if (c == 'f' || c == 'F') {
                fstr = true;
            } else if (c == 't' || c == 'T') {
                tstr = true;
            }
            i++;
        }
        char q = s.charAt(i);
        boolean triple = i + 2 < s.length() && s.charAt(i + 1) == q && s.charAt(i + 2) == q;
        return new SFlags(raw, upper, bytes, fstr, tstr, q, triple);
    }

    static int prefixLen(String s, int start) {
        int i = start;
        while (s.charAt(i) != '\'' && s.charAt(i) != '"') {
            i++;
        }
        return i - start;
    }

    static int contentStart(F f, Node part, SFlags flags) {
        return part.start + prefixLen(f.s, part.start) + flags.quoteLen();
    }

    static int contentEnd(Node part, SFlags flags) {
        return part.end - flags.quoteLen();
    }

    static boolean containsLineBreak(String s, int a, int b) {
        for (int i = a; i < b; i++) {
            char c = s.charAt(i);
            if (c == '\n' || c == '\r') {
                return true;
            }
        }
        return false;
    }

    static boolean partTripleQuoted(F f, Node part) {
        return partFlags(f.s, part.start).triple;
    }

    // ---- quote preference and selection ----

    static char preferredQuoteStyle(F f, Node part, SFlags flags, char configured) {
        if (f.fstate == 2) {
            return Q_PRESERVE;
        }
        if (f.fstate == 1 || f.fstate == 2) {
            SFlags parent = f.fctx.flags;
            if (!parent.triple || flags.triple) {
                return parent.opposite();
            }
        }
        if (configured == Q_PRESERVE) {
            return Q_PRESERVE;
        }
        if (part instanceof FPart) {
            FPart fp = (FPart) part;
            if (!fp.tstring) {
                if (isFstringWithQuotedDebugExpression(f, fp, flags)
                        || isFstringWithTripleQuotedLiteralExpressionContainingQuotes(f, fp)) {
                    return Q_PRESERVE;
                }
            }
            if (isInterpolatedStringWithQuotedFormatSpecAndDebug(f, fp.elements, flags)) {
                return Q_PRESERVE;
            }
        }
        return flags.triple ? '"' : configured;
    }

    /** QuoteSelection: the new flags and the first quote/escape/CR offset in the content (-1 when none). */
    static final class QuoteSel {
        final SFlags flags;
        final int first;

        QuoteSel(SFlags flags, int first) {
            this.flags = flags;
            this.first = first;
        }
    }

    static QuoteSel chooseQuotes(F f, Node part, SFlags flags, char configured) {
        int cs = contentStart(f, part, flags), ce = contentEnd(part, flags);
        String s = f.s;
        int first = -1;
        for (int i = cs; i < ce; i++) {
            char c = s.charAt(i);
            if (c == '\\' || c == '"' || c == '\'' || c == '\r') {
                first = i - cs;
                break;
            }
        }
        char preferred = preferredQuoteStyle(f, part, flags, configured);
        SFlags nf;
        if (preferred == Q_PRESERVE) {
            nf = flags;
        } else if (first < 0) {
            nf = flags.withQuote(preferred);
        } else {
            QuoteMeta m = part instanceof FPart
                    ? QuoteMeta.fromPart(f, part, flags, preferred)
                    : QuoteMeta.fromStr(s, cs + first, ce, flags, preferred);
            nf = flags.withQuote(m.choose(preferred));
        }
        return new QuoteSel(nf, first);
    }

    /** NormalizedString: new flags plus the normalized content text. */
    static final class NormStr {
        final SFlags flags;
        final int cs, ce;
        final String text;

        NormStr(SFlags flags, int cs, int ce, String text) {
            this.flags = flags;
            this.cs = cs;
            this.ce = ce;
            this.text = text;
        }

        void fmt(F f) {
            String p = flags.prefix();
            if (!p.isEmpty()) {
                f.tok(p);
            }
            f.tok(flags.quotes());
            if (!text.isEmpty()) {
                f.txt(text);
            }
            f.tok(flags.quotes());
        }
    }

    static NormStr normalizeStr(F f, Node part, SFlags flags, char configured) {
        int cs = contentStart(f, part, flags), ce = contentEnd(part, flags);
        String raw = f.s.substring(cs, ce);
        QuoteSel sel = chooseQuotes(f, part, flags, configured);
        String text = sel.first >= 0 ? normalizeString(raw, sel.first, sel.flags, false) : raw;
        return new NormStr(sel.flags, cs, ce, text);
    }

    static final class QuoteMeta {
        static final int RAW = 0, TRIPLE = 1, REGULAR = 2;
        final int kind;
        final boolean containsPreferred;
        final int single, dbl;
        final char source;

        QuoteMeta(int kind, boolean containsPreferred, int single, int dbl, char source) {
            this.kind = kind;
            this.containsPreferred = containsPreferred;
            this.single = single;
            this.dbl = dbl;
            this.source = source;
        }

        char choose(char preferred) {
            if (kind != REGULAR) {
                return containsPreferred ? source : preferred;
            }
            return single < dbl ? '\'' : single == dbl ? preferred : '"';
        }

        QuoteMeta merge(QuoteMeta o) {
            if (kind != o.kind) {
                return null;
            }
            return new QuoteMeta(kind, containsPreferred || o.containsPreferred, single + o.single, dbl + o.dbl, source);
        }

        static QuoteMeta fromPart(F f, Node part, SFlags flags, char preferred) {
            if (part instanceof FPart) {
                QuoteMeta m = fromStr("", 0, 0, flags, preferred);
                return m.mergeElements(f, ((FPart) part).elements, flags, preferred);
            }
            return fromStr(f.s, contentStart(f, part, flags), contentEnd(part, flags), flags, preferred);
        }

        static QuoteMeta fromStr(String s, int a, int b, SFlags flags, char preferred) {
            if (flags.raw) {
                return new QuoteMeta(RAW, rawContainsPreferred(s, a, b, preferred, flags.triple), 0, 0, flags.quote);
            } else if (flags.triple) {
                return new QuoteMeta(TRIPLE, tripleContainsPreferred(s, a, b, preferred), 0, 0, flags.quote);
            }
            int single = 0, dbl = 0;
            for (int i = a; i < b; i++) {
                char c = s.charAt(i);
                if (c == '\'') {
                    single++;
                } else if (c == '"') {
                    dbl++;
                }
            }
            return new QuoteMeta(REGULAR, false, single, dbl, flags.quote);
        }

        QuoteMeta mergeElements(F f, List<Node> elements, SFlags flags, char preferred) {
            QuoteMeta merged = this;
            for (Node el : elements) {
                if (el instanceof FLit) {
                    merged = merged.merge(fromStr(f.s, el.start, el.end, flags, preferred));
                } else {
                    FInterp fi = (FInterp) el;
                    if (fi.spec != null && fi.debugLeading == null) {
                        merged = merged.mergeElements(f, fi.spec, flags, preferred);
                    }
                }
            }
            return merged;
        }

        static boolean tripleContainsPreferred(String s, int a, int b, char pq) {
            int i = a;
            while (i < b) {
                char c = s.charAt(i++);
                if (c == '\\') {
                    if (i < b && (s.charAt(i) == '"' || s.charAt(i) == '\\')) {
                        i++;
                    }
                } else if (c == pq) {
                    if (i >= b) {
                        return true;
                    }
                    if (s.charAt(i) == pq) {
                        i++;
                        if (i >= b) {
                            return true;
                        }
                        if (s.charAt(i) == pq) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        static boolean rawContainsPreferred(String s, int a, int b, char pq, boolean triple) {
            int i = a;
            while (i < b) {
                char c = s.charAt(i++);
                if (c == '\\') {
                    i++;
                } else if (c == pq) {
                    if (!triple || i >= b) {
                        return true;
                    }
                    if (s.charAt(i) == pq) {
                        i++;
                        if (i >= b || s.charAt(i) == pq) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }
    }

    /** normalize_string: quote escaping, CR normalization and escape-sequence case normalization. */
    static String normalizeString(String input, int startOffset, SFlags nf, boolean escapeBraces) {
        StringBuilder out = new StringBuilder();
        int last = 0;
        char pq = nf.quote, oq = nf.opposite();
        int n = input.length();
        int i = startOffset;
        while (i < n) {
            int index = i;
            char c = input.charAt(i++);
            if ((c == '{' || c == '}') && escapeBraces) {
                out.append(input, last, index + 1).append(c);
                last = index + 1;
                continue;
            }
            if (c == '\r') {
                out.append(input, last, index);
                if (i < n && input.charAt(i) == '\n') {
                    i++;
                } else {
                    out.append('\n');
                }
                last = index + 1;
            } else if (!nf.raw) {
                if (c == '\\') {
                    if (i < n) {
                        char next = input.charAt(i);
                        if (next == '\\') {
                            i++;
                        } else {
                            String norm = normalizeEscape(next, !nf.bytes, input, index + 2);
                            if (norm != null) {
                                int escStart = index + 2;
                                if (!input.regionMatches(escStart, norm, 0, norm.length())) {
                                    out.append(input, last, escStart).append(norm);
                                    last = escStart + norm.length();
                                }
                                i += 1 + norm.length();
                            }
                        }
                        if (!nf.triple) {
                            if (next == oq) {
                                i++;
                                out.append(input, last, index);
                                last = index + 1;
                            } else if (next == pq) {
                                i++;
                            }
                        }
                    }
                } else if (!nf.triple && c == pq) {
                    out.append(input, last, index).append('\\').append(c);
                    last = index + 1;
                }
            }
        }
        if (last == 0) {
            return input;
        }
        out.append(input, last, n);
        return out.toString();
    }

    /** UnicodeEscape::normalize for the escape body at {@code a}; null when not a valid escape. */
    static String normalizeEscape(char first, boolean allowUnicode, String s, int a) {
        int len;
        if (first == 'x') {
            len = 2;
        } else if (first == 'u' && allowUnicode) {
            len = 4;
        } else if (first == 'U' && allowUnicode) {
            len = 8;
        } else if (first == 'N' && allowUnicode) {
            if (a >= s.length() || s.charAt(a) != '{') {
                return null;
            }
            StringBuilder norm = null;
            int k = a + 1;
            while (true) {
                if (k >= s.length()) {
                    return null;
                }
                char c = s.charAt(k);
                int index = k - a;
                if (c == '}') {
                    if (norm != null) {
                        norm.append('}');
                    }
                    if (index < 3) {
                        return null;
                    }
                    len = index + 1;
                    break;
                } else if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || c == ' ' || c == '-') {
                    if (norm != null) {
                        norm.append(c);
                    }
                } else if (c >= 'a' && c <= 'z') {
                    if (norm == null) {
                        norm = new StringBuilder().append(s, a, k);
                    }
                    norm.append((char) (c - 32));
                } else {
                    return null;
                }
                k++;
            }
            return norm == null ? s.substring(a, a + len) : norm.toString();
        } else {
            return null;
        }
        if (s.length() - a < len) {
            return null;
        }
        StringBuilder norm = null;
        for (int k = 0; k < len; k++) {
            char c = s.charAt(a + k);
            if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) {
                if (norm != null) {
                    norm.append(c);
                }
            } else if (c >= 'A' && c <= 'F') {
                if (norm == null) {
                    norm = new StringBuilder().append(s, a, a + k);
                }
                norm.append((char) (c + 32));
            } else {
                return null;
            }
        }
        return norm == null ? s.substring(a, a + len) : norm.toString();
    }

    static boolean isFstringWithQuotedDebugExpression(F f, FPart fp, SFlags flags) {
        for (Node el : fp.elements) {
            if (el instanceof FInterp && ((FInterp) el).debugLeading != null
                    && containsOppositeQuote(f.s, el.start, el.end, flags)) {
                return true;
            }
        }
        return false;
    }

    static boolean isFstringWithTripleQuotedLiteralExpressionContainingQuotes(F f, FPart fp) {
        boolean[] found = {false};
        fp.each(ch -> tripleQuotedLiteralWalk(f, ch, found));
        return found[0];
    }

    static void tripleQuotedLiteralWalk(F f, Node n, boolean[] found) {
        if (found[0]) {
            return;
        }
        if (n instanceof StrPart || (n instanceof FPart && !((FPart) n).tstring)) {
            SFlags flags = partFlags(f.s, n.start);
            if (!flags.triple) {
                return;
            }
            if (n instanceof StrPart) {
                found[0] = indexOf(f.s, flags.quote, contentStart(f, n, flags), contentEnd(n, flags)) >= 0;
            } else {
                for (Node el : ((FPart) n).elements) {
                    if (el instanceof FLit && indexOf(f.s, flags.quote, el.start, el.end) >= 0) {
                        found[0] = true;
                        break;
                    }
                }
            }
            return;
        }
        n.each(ch -> tripleQuotedLiteralWalk(f, ch, found));
    }

    static int indexOf(String s, char c, int a, int b) {
        for (int i = a; i < b; i++) {
            if (s.charAt(i) == c) {
                return i;
            }
        }
        return -1;
    }

    static boolean isInterpolatedStringWithQuotedFormatSpecAndDebug(F f, List<Node> elements, SFlags flags) {
        for (Node el : elements) {
            if (el instanceof FInterp) {
                FInterp fi = (FInterp) el;
                if (fi.spec != null && formatSpecWithOppositeQuote(f, fi.spec, flags, fi.debugLeading != null)) {
                    return true;
                }
            }
        }
        return false;
    }

    static boolean formatSpecWithOppositeQuote(F f, List<Node> elements, SFlags flags, boolean inDebug) {
        for (Node el : elements) {
            if (el instanceof FLit) {
                if (inDebug && containsOppositeQuote(f.s, el.start, el.end, flags)) {
                    return true;
                }
            } else {
                FInterp fi = (FInterp) el;
                if (fi.spec != null
                        && formatSpecWithOppositeQuote(f, fi.spec, flags, inDebug || fi.debugLeading != null)) {
                    return true;
                }
            }
        }
        return false;
    }

    static boolean containsOppositeQuote(String s, int a, int b, SFlags flags) {
        if (flags.triple) {
            String t = flags.quote == '\'' ? "\"\"\"" : "'''";
            int k = s.indexOf(t, a);
            return k >= 0 && k + 3 <= b;
        }
        char oq = flags.opposite();
        int from = a;
        while (true) {
            int idx = indexOf(s, oq, from, b);
            if (idx < 0) {
                return false;
            }
            if (flags.raw) {
                return true;
            }
            int backslashes = 0;
            for (int k = idx - 1; k >= from && s.charAt(k) == '\\'; k--) {
                backslashes++;
            }
            if (backslashes % 2 == 0) {
                return true;
            }
            from = idx + 1;
        }
    }

    // ---- string expressions ----

    /** StringLike::is_multiline. */
    static boolean strIsMultiline(F f, Str e) {
        for (Node p : e.parts) {
            SFlags flags = partFlags(f.s, p.start);
            if (p instanceof FPart) {
                if (elementsMultilineOrComments(f, ((FPart) p).elements, flags.triple)) {
                    return true;
                }
            } else if (flags.triple && containsLineBreak(f.s, p.start, p.end)) {
                return true;
            }
        }
        return false;
    }

    static boolean elementsMultilineOrComments(F f, List<Node> elements, boolean triple) {
        for (Node el : elements) {
            if (el instanceof FLit) {
                if (triple && containsLineBreak(f.s, el.start, el.end)) {
                    return true;
                }
            } else {
                FInterp fi = (FInterp) el;
                if (containsComments(f, fi)
                        || (fi.spec != null && elementsMultilineOrComments(f, fi.spec, triple))
                        || (fi.debugLeading != null && (hasNewline(fi.debugLeading) || hasNewline(fi.debugTrailing)))) {
                    return true;
                }
            }
        }
        return false;
    }

    static boolean hasNewline(String x) {
        return x.indexOf('\n') >= 0 || x.indexOf('\r') >= 0;
    }

    /** InterpolatedStringLayout::from_interpolated_string_elements is Multiline. */
    static boolean layoutMultiline(F f, List<Node> elements) {
        for (Node el : elements) {
            if (el instanceof FInterp && containsLineBreak(f.s, el.start, el.end)) {
                return true;
            }
        }
        return false;
    }

    static int needsParensStr(F f, Str e, Node parent) {
        if (e.implicit()) {
            return OP_MULTILINE;
        }
        if (strIsMultiline(f, e)) {
            return OP_NEVER;
        }
        Node p = e.parts.get(0);
        if (p instanceof FPart && layoutMultiline(f, ((FPart) p).elements)) {
            return OP_NEVER;
        }
        return OP_BEST_FIT;
    }

    static void fmtStr(F f, Str e, int kind) {
        boolean docstring = kind == SK_DOCSTRING && e.kind == S_STR;
        if (!e.implicit()) {
            fmtStrPart(f, e.parts.get(0), docstring);
            return;
        }
        if (!f.isParenthesizedLevel()) {
            SFlags flat = implicitFlatFlags(f, e);
            if (flat != null) {
                fmtImplicitFlat(f, e, flat, docstring);
                return;
            }
            if (docstring) {
                parenthesizeIfExpands(f, true, x -> fmtImplicitExpanded(x, e, true));
                return;
            }
        }
        ipoGroup(f, x -> fmtImplicitConcat(x, e));
    }

    /** One part formatted as its own node (FormatStringLiteral / FormatBytesLiteral / FormatFString). */
    static void fmtStrPart(F f, Node part, boolean docstring) {
        f.node(part, x -> {
            if (part instanceof FPart) {
                fmtFPart(x, (FPart) part);
            } else {
                NormStr n = normalizeStr(x, part, partFlags(x.s, part.start), '"');
                if (docstring) {
                    fmtDocstring(x, n);
                } else {
                    n.fmt(x);
                }
            }
        });
    }

    /** FormatImplicitConcatenatedString. */
    static void fmtImplicitConcat(F f, Str e) {
        SFlags flat = implicitFlatFlags(f, e);
        if (flat != null) {
            f.ifFits(x -> fmtImplicitFlat(x, e, flat, false));
            f.ifBreaks(x -> fmtImplicitExpanded(x, e, false));
        } else {
            fmtImplicitExpanded(f, e, true);
        }
    }

    static void fmtImplicitExpanded(F f, Str e, boolean multipart) {
        if (multipart) {
            for (int i = 1; i < e.parts.size(); i++) {
                if (containsLineBreak(f.s, e.parts.get(i - 1).end, e.parts.get(i).start)) {
                    f.expandParent();
                    break;
                }
            }
        }
        boolean first = true;
        for (Node part : e.parts) {
            if (!first) {
                ipoSoftOrSpace(f);
            }
            first = false;
            NodeComments c = f.comments(part);
            f.leadingComments(c.leading);
            fmtStrPart(f, part, false);
            f.trailingComments(c.trailing);
        }
    }

    /** FormatImplicitConcatenatedStringFlat::new: the merged flags, or null when the parts can't be joined. */
    static SFlags implicitFlatFlags(F f, Str e) {
        if (!e.implicit() || strIsMultiline(f, e)) {
            return null;
        }
        char preserveQuote = 0;
        for (Node part : e.parts) {
            SFlags pf = partFlags(f.s, part.start);
            if (pf.triple || pf.raw) {
                return null;
            }
            NodeComments c = f.comments(part);
            if (!c.leading.isEmpty() || !c.trailing.isEmpty()) {
                return null;
            }
            if (part instanceof FPart) {
                FPart fp = (FPart) part;
                boolean needsPreserve;
                if (!fp.tstring) {
                    if (e.kind == S_TSTR) {
                        return null;
                    }
                    needsPreserve = isFstringWithQuotedDebugExpression(f, fp, pf)
                            || isFstringWithTripleQuotedLiteralExpressionContainingQuotes(f, fp);
                } else {
                    needsPreserve = isInterpolatedStringWithQuotedFormatSpecAndDebug(f, fp.elements, pf);
                }
                if (needsPreserve) {
                    if (preserveQuote != 0 && preserveQuote != pf.quote) {
                        return null;
                    }
                    preserveQuote = pf.quote;
                }
            }
        }
        boolean bytes = e.kind == S_BYTES, fstr = e.kind == S_FSTR, tstr = e.kind == S_TSTR;
        char quote;
        if (preserveQuote != 0) {
            quote = preserveQuote;
        } else {
            Node firstPart = e.parts.get(0);
            SFlags ff = partFlags(f.s, firstPart.start);
            char preferred = preferredQuoteStyle(f, firstPart, ff, '"');
            if (preferred != Q_PRESERVE) {
                QuoteMeta merged = null;
                for (Node part : e.parts) {
                    QuoteMeta m = QuoteMeta.fromPart(f, part, partFlags(f.s, part.start), preferred);
                    if (merged == null) {
                        merged = m;
                    } else {
                        merged = m.merge(merged);
                        if (merged == null) {
                            return null;
                        }
                    }
                }
                quote = merged.choose(preferred);
            } else {
                quote = ff.quote;
            }
        }
        return new SFlags(false, false, bytes, fstr, tstr, quote, false);
    }

    static void fmtImplicitFlat(F f, Str e, SFlags flags, boolean docstring) {
        String q = flags.quotes();
        String p = flags.prefix();
        if (!p.isEmpty()) {
            f.tok(p);
        }
        f.tok(q);
        int last = e.parts.size();
        if (docstring) {
            while (last > 0) {
                Node part = e.parts.get(last - 1);
                SFlags pf = partFlags(f.s, part.start);
                if (isBlankWs(f.s.substring(contentStart(f, part, pf), contentEnd(part, pf)))) {
                    last--;
                } else {
                    break;
                }
            }
        }
        boolean firstNonEmpty = docstring;
        for (int i = 0; i < last; i++) {
            Node part = e.parts.get(i);
            if (part instanceof FPart) {
                List<Node> elements = ((FPart) part).elements;
                for (Node el : elements) {
                    if (el instanceof FLit) {
                        fmtLiteralContent(f, el.start, el.end, flags, true, false, false);
                    } else {
                        FStringCtx ctx = new FStringCtx(flags, layoutMultiline(f, elements));
                        fmtFInterp(f, (FInterp) el, ctx);
                    }
                }
            } else {
                SFlags pf = partFlags(f.s, part.start);
                int cs = contentStart(f, part, pf), ce = contentEnd(part, pf);
                fmtLiteralContent(f, cs, ce, flags, false, firstNonEmpty && docstring, docstring && i + 1 == last);
                if (firstNonEmpty) {
                    firstNonEmpty = stripLeadingWs(f.s.substring(cs, ce)).isEmpty();
                }
            }
        }
        f.tok(q);
    }

    static void fmtLiteralContent(F f, int a, int b, SFlags flags, boolean interpolatedElement, boolean trimStart, boolean trimEnd) {
        String normalized = normalizeString(f.s.substring(a, b), 0, flags, flags.interpolated() && !interpolatedElement);
        if (trimStart) {
            normalized = stripLeadingWs(normalized);
        }
        if (trimEnd) {
            normalized = stripTrailingWs(normalized);
        }
        if (!normalized.isEmpty()) {
            f.txt(normalized);
            if (trimEnd && needsChaperoneSpace(flags, normalized)) {
                f.space();
            }
        }
    }

    // ---- f-strings and t-strings ----

    static void fmtFPart(F f, FPart fp) {
        SFlags flags = partFlags(f.s, fp.start);
        SFlags nf = chooseQuotes(f, fp, flags, '"').flags;
        FStringCtx ctx = new FStringCtx(nf, layoutMultiline(f, fp.elements));
        String p = nf.prefix();
        if (!p.isEmpty()) {
            f.tok(p);
        }
        f.tok(nf.quotes());
        for (Node el : fp.elements) {
            fmtFElement(f, el, ctx);
        }
        f.tok(nf.quotes());
    }

    static void fmtFElement(F f, Node el, FStringCtx ctx) {
        if (el instanceof FLit) {
            String normalized = normalizeString(f.s.substring(el.start, el.end), 0, ctx.flags, false);
            if (!normalized.isEmpty()) {
                f.txt(normalized);
            }
        } else {
            fmtFInterp(f, (FInterp) el, ctx);
        }
    }

    static String normalizeCr(String x) {
        return x.indexOf('\r') < 0 ? x : x.replace("\r\n", "\n").replace('\r', '\n');
    }

    static void verbatimText(F f, int a, int b) {
        if (b > a) {
            f.txt(normalizeCr(f.s.substring(a, b)));
        }
    }

    static void markVerbatimFormatted(F f, Node n) {
        NodeComments c = f.comments(n);
        for (Comment x : c.leading) {
            x.formatted = true;
        }
        for (Comment x : c.dangling) {
            x.formatted = true;
        }
        for (Comment x : c.trailing) {
            x.formatted = true;
        }
        n.each(ch -> markVerbatimFormatted(f, ch));
    }

    static void fmtConversion(F f, char conversion) {
        if (conversion != 0) {
            f.txt("!" + conversion);
        }
    }

    static void fmtFInterp(F f, FInterp fi, FStringCtx ctx) {
        if (fi.debugLeading != null) {
            f.tok("{");
            markVerbatimFormatted(f, fi);
            if (!fi.debugLeading.isEmpty()) {
                f.txt(normalizeCr(fi.debugLeading));
            }
            verbatimText(f, fi.exprStart, fi.exprEnd);
            if (!fi.debugTrailing.isEmpty()) {
                f.txt(normalizeCr(fi.debugTrailing));
            }
            fmtConversion(f, fi.conversion);
            if (fi.spec != null) {
                f.tok(":");
                verbatimText(f, fi.specStart, fi.specEnd);
            }
            f.tok("}");
            return;
        }
        List<Comment> dangling = f.dangling(fi);
        SFlags flags = ctx.flags;
        boolean multiline = ctx.multiline
                && (flags.triple || containsLineBreak(f.s, fi.start, fi.spec != null ? fi.specStart : fi.end));
        FStringCtx inner = multiline ? ctx : new FStringCtx(flags, false);
        boolean spacing = needsBracketSpacing(f, fi.expr);
        Fmt item = x -> {
            int savedState = x.fstate;
            FStringCtx savedCtx = x.fctx;
            x.fstate = savedState == 0 ? 1 : 2;
            x.fctx = inner;
            if (spacing) {
                if (multiline) {
                    x.softOrSpace();
                } else {
                    x.space();
                }
            }
            fmtExpr(x, fi.expr);
            fmtConversion(x, fi.conversion);
            if (fi.spec != null) {
                if (x.hasTrailing(fi.expr)) {
                    x.soft();
                }
                x.tok(":");
                for (Node el : fi.spec) {
                    fmtFElement(x, el, inner);
                }
            }
            if (fi.conversion == 0 && fi.spec == null && spacing) {
                if (multiline) {
                    x.softOrSpace();
                } else {
                    x.space();
                }
            }
            x.fstate = savedState;
            x.fctx = savedCtx;
        };
        f.tok("{");
        f.withLevel(NL_PAREN, -1, x -> {
            if (ctx.multiline) {
                if (fi.spec == null) {
                    x.group(y -> {
                        y.danglingOpenParenthesisComments(dangling);
                        y.softBlockIndent(item);
                    });
                } else {
                    x.group(y -> {
                        y.danglingOpenParenthesisComments(dangling);
                        y.indent(z -> {
                            z.soft();
                            item.fmt(z);
                        });
                    });
                }
            } else {
                removeSoftLines(x, y -> {
                    y.danglingOpenParenthesisComments(dangling);
                    item.fmt(y);
                });
            }
        });
        f.tok("}");
    }

    static boolean needsBracketSpacing(F f, Expr e) {
        if (e instanceof Seq && ((Seq) e).kind == '(' && !((Seq) e).parenthesized && ((Seq) e).elts.size() == 1) {
            return false;
        }
        Expr l = leftMost(f, e);
        return l instanceof DictE || (l instanceof Comp && (((Comp) l).kind == 'd' || ((Comp) l).kind == '{'))
                || (l instanceof Seq && ((Seq) l).kind == '{');
    }

    // =====================================================================================
    // Docstrings (string/docstring.rs; docstring code formatting is disabled by default)
    // =====================================================================================

    /** Whether every character is Unicode whitespace (Rust's {@code str::trim().is_empty()}). */
    static boolean isBlankWs(String x) {
        for (int i = 0; i < x.length(); i++) {
            if (!uniWs(x.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    static int countTrailing(String x, char c) {
        int n = 0;
        for (int i = x.length() - 1; i >= 0 && x.charAt(i) == c; i--) {
            n++;
        }
        return n;
    }

    /**
     * Whether the last docstring line needs a chaperone space before the closing quotes: after an odd
     * number of backslashes, or when a triple-quoted string would otherwise end in four quotes.
     */
    static boolean needsChaperoneSpace(SFlags flags, String trimEnd) {
        if (countTrailing(trimEnd, '\\') % 2 == 1) {
            return true;
        }
        if (flags.triple && !trimEnd.isEmpty() && trimEnd.charAt(trimEnd.length() - 1) == flags.quote) {
            return countTrailing(trimEnd.substring(0, trimEnd.length() - 1), '\\') % 2 == 0;
        }
        return false;
    }

    static boolean containsUnescapedNewline(String haystack) {
        int i = haystack.indexOf('\\');
        while (i >= 0) {
            int j = i + 1;
            while (j < haystack.length() && pyWs(haystack.charAt(j))) {
                j++;
            }
            if (j < haystack.length() && haystack.charAt(j) == '\n') {
                return true;
            }
            i = haystack.indexOf('\\', j);
        }
        return false;
    }

    static int utf8Len(char c) {
        return c < 0x80 ? 1 : c < 0x800 ? 2 : Character.isSurrogate(c) ? 2 : 3;
    }

    static int tabOffset(int column) {
        return 8 - column % 8;
    }

    /** Docstring line indentation (ruff's Indentation, tabs expand to multiples of 8): {columns, textLen}. */
    static int[] docIndent(String s) {
        int n = s.length(), i = 0;
        int spaces = 0, tabs = 0, align = 0;
        while (i < n && s.charAt(i) == ' ') {
            spaces++;
            i++;
        }
        while (i < n && s.charAt(i) == '\t') {
            tabs++;
            i++;
        }
        if (tabs == 0) {
            return new int[] {spaces, spaces};
        }
        while (i < n && s.charAt(i) == ' ') {
            align++;
            i++;
        }
        if (spaces == 0) {
            if (align == 0) {
                return new int[] {tabs * 8, tabs};
            }
            if (i >= n || s.charAt(i) != '\t') {
                return new int[] {tabs * 8 + align, tabs + align};
            }
        } else if (align == 0) {
            return new int[] {spaces + tabOffset(spaces) + (tabs - 1) * 8, spaces + tabs};
        }
        int width = spaces + tabs * 8 + align, len = spaces + tabs + align;
        for (; i < n; i++) {
            char c = s.charAt(i);
            if (c == '\t') {
                width += tabOffset(width);
                len += 1;
            } else if (uniWs(c)) {
                width += utf8Len(c);
                len += utf8Len(c);
            } else {
                break;
            }
        }
        return new int[] {width, len};
    }

    /** Formats a normalized docstring: trims whitespace, re-indents lines and collapses one-liners. */
    static void fmtDocstring(F f, NormStr n) {
        String docstring = n.text;
        if (containsUnescapedNewline(docstring)) {
            n.fmt(f);
            return;
        }
        SFlags flags = n.flags;
        String quotes = flags.quotes();
        String prefix = flags.prefix();
        if (!prefix.isEmpty()) {
            f.tok(prefix);
        }
        f.tok(quotes);
        String[] lines = docstring.split("\n", -1);
        String first = lines[0];
        String trimEnd = stripTrailingWs(first);
        String trimBoth = stripLeadingWs(trimEnd);
        if (!trimBoth.isEmpty() && trimBoth.charAt(0) == flags.quote) {
            f.space();
        }
        if (!trimEnd.isEmpty()) {
            f.txt(trimBoth);
        }
        if (isBlankWs(docstring.substring(first.length()))) {
            if (needsChaperoneSpace(flags, trimEnd) || trimEnd.isEmpty() && !docstring.isEmpty()) {
                f.space();
            }
            f.tok(quotes);
            return;
        }
        f.hard();
        int[] stripped = null;
        for (int i = 1; i < lines.length; i++) {
            if (!isBlankWs(lines[i])) {
                int[] ind = docIndent(lines[i]);
                if (stripped == null || ind[0] < stripped[0]) {
                    stripped = ind;
                }
            }
        }
        if (stripped == null) {
            stripped = new int[] {0, 0};
        }
        for (int i = 1; i < lines.length; i++) {
            boolean isLast = i + 1 == lines.length;
            String line = stripTrailingWs(lines[i]);
            if (line.isEmpty()) {
                if (!isLast) {
                    f.emptyLine();
                }
                continue;
            }
            boolean tabOrNonAsciiSpace = false;
            for (int j = 0; j < line.length() && uniWs(line.charAt(j)); j++) {
                if (line.charAt(j) != ' ') {
                    tabOrNonAsciiSpace = true;
                    break;
                }
            }
            if (!tabOrNonAsciiSpace) {
                f.txt(line.substring(stripped[1]));
            } else {
                int indentLen = docIndent(line)[0] - stripped[0];
                f.txt(" ".repeat(indentLen) + stripLeadingWs(line));
            }
            if (!isLast) {
                f.hard();
            }
        }
        int e = docstring.length();
        while (e > 0 && uniWs(docstring.charAt(e - 1)) && docstring.charAt(e - 1) != '\n') {
            e--;
        }
        if (needsChaperoneSpace(flags, docstring.substring(0, e))) {
            f.space();
        }
        f.tok(quotes);
    }

    // =====================================================================================
    // Assignment-like statements (statement/stmt_assign.rs and friends)
    // =====================================================================================

    /** OptionalParenthesesInlinedComments: end-of-line comments of the value and its statement. */
    static final class InlineComments implements Fmt {
        final List<Comment> expression;
        final List<Comment> statement;

        InlineComments(List<Comment> expression, List<Comment> statement) {
            this.expression = expression;
            this.statement = statement;
        }

        static InlineComments of(F f, Expr value, Node statement) {
            NodeComments ec = f.comments(value);
            if (!ec.leading.isEmpty() || hasOwnLineComment(ec.trailing)) {
                return null;
            }
            return new InlineComments(endOfLinePrefix(ec.trailing), endOfLinePrefix(f.trailing(statement)));
        }

        static List<Comment> endOfLinePrefix(List<Comment> cs) {
            int n = 0;
            while (n < cs.size() && !cs.get(n).ownLine) {
                n++;
            }
            return n == cs.size() ? cs : cs.subList(0, n);
        }

        boolean isEmpty() {
            return expression.isEmpty() && statement.isEmpty();
        }

        void markFormatted() {
            for (Comment c : expression) {
                c.formatted = true;
            }
        }

        void markUnformatted() {
            for (Comment c : expression) {
                c.formatted = false;
            }
        }

        @Override
        public void fmt(F f) {
            for (Comment c : expression) {
                c.formatted = false;
            }
            for (Comment c : statement) {
                c.formatted = false;
            }
            f.trailingComments(expression);
            f.trailingComments(statement);
        }
    }

    static boolean hasOwnLineComment(List<Comment> cs) {
        for (Comment c : cs) {
            if (c.ownLine) {
                return true;
            }
        }
        return false;
    }

    static boolean isInterpolated(Str s) {
        return s.kind == S_FSTR || s.kind == S_TSTR;
    }

    /** format_interpolated_string_assignment: a single f/t-string part whose layout is multiline. */
    static FPart interpolatedStringAssignment(F f, Str s) {
        if (!isInterpolated(s) || s.parts.size() != 1 || !(s.parts.get(0) instanceof FPart)) {
            return null;
        }
        FPart part = (FPart) s.parts.get(0);
        if (!layoutMultiline(f, part.elements) || strIsMultiline(f, s)) {
            return null;
        }
        return part;
    }

    static boolean shouldInlineComments(F f, Expr e, Node parent) {
        if (e instanceof Name || e instanceof Num) {
            return true;
        }
        if (e instanceof Const) {
            return !((Const) e).value.equals("...");
        }
        if (e instanceof Str) {
            return needsParentheses(f, e, parent) == OP_BEST_FIT;
        }
        return false;
    }

    static boolean shouldNonInlineableUseBestFit(F f, Expr e, Node parent) {
        if (e instanceof Attribute || e instanceof Call || e instanceof Subscript) {
            return needsParentheses(f, e, parent) == OP_BEST_FIT;
        }
        return false;
    }

    static boolean hasTargetOwnParentheses(F f, Expr target) {
        return (target instanceof Seq && ((Seq) target).kind == '(') || hasOwnParentheses(f, target) != OWN_NONE;
    }

    static boolean shouldParenthesizeTarget(F f, Expr target) {
        return !(hasTargetOwnParentheses(f, target) || isAttributeWithParenthesizedValue(f, target));
    }

    static boolean isAttributeWithParenthesizedValue(F f, Expr target) {
        if (target instanceof Attribute) {
            Expr value = ((Attribute) target).value;
            return hasParentheses(f, value) != OWN_NONE || isAttributeWithParenthesizedValue(f, value);
        }
        if (target instanceof Subscript) {
            return true;
        }
        if (target instanceof Call) {
            return !argumentsEmpty(((Call) target).arguments);
        }
        return false;
    }

    /** MaybeParenthesizeValue. */
    static void maybeParenthesizeValue(F f, Expr e, Node parent) {
        if (e instanceof Lambda && !f.hasLeading(e)) {
            Lambda l = (Lambda) e;
            parenthesizeIfExpands(f, true, x -> x.node(l, y -> fmtLambda(y, l, true)));
        } else {
            maybeParenthesize(f, e, parent, PZ_IF_BREAKS);
        }
    }

    /** AnyBeforeOperator::Expression. */
    static Fmt beforeOperator(Expr e) {
        return f -> {
            if (f.hasLeading(e) || f.hasTrailing(e)) {
                fmtExpr(f, e, P_PRESERVE);
            } else if (shouldParenthesizeTarget(f, e)) {
                if (canOmitOptionalParentheses(f, e)) {
                    optionalParentheses(f, x -> fmtExpr(x, e, P_NEVER));
                } else {
                    parenthesizeIfExpands(f, true, x -> fmtExpr(x, e, P_NEVER));
                }
            } else {
                fmtExpr(f, e, P_NEVER);
            }
        };
    }

    /** FormatTargetWithEqualOperator. */
    static void fmtTargetWithEqual(F f, Expr target, boolean preserveParentheses) {
        if (preserveParentheses || f.hasLeading(target) || f.hasTrailing(target)) {
            fmtExpr(f, target);
        } else if (shouldParenthesizeTarget(f, target)) {
            parenthesizeIfExpands(f, true, x -> fmtExpr(x, target, P_NEVER));
        } else {
            fmtExpr(f, target, P_NEVER);
        }
        f.space();
        f.tok("=");
        f.space();
    }

    /** The implicitly concatenated string, or the single-part f-string, formatted flat. */
    static Fmt flatString(Str s, SFlags flat) {
        if (isInterpolated(s)) {
            return x -> removeSoftLines(x, y -> fmtImplicitFlat(y, s, flat, false));
        }
        return x -> fmtImplicitFlat(x, s, flat, false);
    }

    /** FormatStatementsLastExpression::LeftToRight. */
    static void fmtLastExpr(F f, Expr value, Node statement) {
        boolean canInline = shouldInlineComments(f, value, statement);
        Str str = value instanceof Str ? (Str) value : null;
        FPart interp = str != null ? interpolatedStringAssignment(f, str) : null;
        SFlags flat = str != null ? implicitFlatFlags(f, str) : null;
        if (!canInline && flat == null && interp == null) {
            maybeParenthesizeValue(f, value, statement);
            return;
        }
        InlineComments inline = InlineComments.of(f, value, statement);
        if (inline == null) {
            fmtExpr(f, value, P_ALWAYS);
            return;
        }
        int groupId = f.groupId();
        if (flat != null) {
            inline.markFormatted();
            Memo flatFmt = new Memo(flatString(str, flat));
            if (isInterpolated(str) && flatFmt.willBreak(f)) {
                inline.markUnformatted();
                maybeParenthesize(f, value, statement, PZ_IF_BREAKS);
                return;
            }
            Fmt expanded = x -> x.withLevel(NL_EXPR, groupId, y -> fmtImplicitExpanded(y, str, false));
            f.bestFitting(true,
                    x -> {
                        flatFmt.fmt(x);
                        inline.fmt(x);
                    },
                    x -> x.group(groupId, true, g -> {
                        g.tok("(");
                        g.softBlockIndent(y -> {
                            flatFmt.fmt(y);
                            inline.fmt(y);
                        });
                        g.tok(")");
                    }),
                    x -> x.group(groupId, true, g -> {
                        g.tok("(");
                        g.blockIndent(expanded);
                        g.tok(")");
                        inline.fmt(g);
                    }));
        } else if (interp != null) {
            inline.markFormatted();
            Memo interpFlat = new Memo(x -> removeSoftLines(x, y -> fmtStrPart(y, interp, false)));
            if (interpFlat.willBreak(f)) {
                inline.markUnformatted();
                maybeParenthesize(f, value, statement, PZ_IF_BREAKS);
                return;
            }
            f.bestFitting(true,
                    x -> {
                        interpFlat.fmt(x);
                        inline.fmt(x);
                    },
                    x -> x.group(groupId, true, g -> {
                        g.tok("(");
                        g.softBlockIndent(y -> {
                            interpFlat.fmt(y);
                            inline.fmt(y);
                        });
                        g.tok(")");
                    }),
                    x -> {
                        fmtStrPart(x, interp, false);
                        inline.fmt(x);
                    });
        } else {
            f.bestFitParenthesize(groupId, x -> {
                inline.markFormatted();
                fmtExpr(x, value, P_NEVER);
                if (!inline.isEmpty()) {
                    x.ifBreaks(inline);
                }
            });
            if (!inline.isEmpty()) {
                f.ifFits(groupId, inline);
            }
        }
    }

    /** FormatStatementsLastExpression::RightToLeft. */
    static void fmtLastExprRightToLeft(F f, Fmt before, String operator, Expr value, Node statement) {
        boolean shouldInline = shouldInlineComments(f, value, statement);
        Str str = value instanceof Str ? (Str) value : null;
        FPart interp = str != null ? interpolatedStringAssignment(f, str) : null;
        SFlags flat = str != null ? implicitFlatFlags(f, str) : null;
        Fmt op = x -> x.tok(operator);
        if (!shouldInline && !shouldNonInlineableUseBestFit(f, value, statement) && flat == null && interp == null) {
            before.fmt(f);
            f.space();
            op.fmt(f);
            f.space();
            maybeParenthesizeValue(f, value, statement);
            return;
        }
        NodeComments ec = f.comments(value);
        InlineComments inline;
        if (shouldInline || flat != null || interp != null) {
            inline = InlineComments.of(f, value, statement);
        } else if (!ec.leading.isEmpty() || hasOwnLineComment(ec.trailing)) {
            inline = null;
        } else {
            inline = new InlineComments(NO_COMMENTS, NO_COMMENTS);
        }
        if (inline == null) {
            before.fmt(f);
            f.space();
            op.fmt(f);
            f.space();
            fmtExpr(f, value, P_ALWAYS);
            return;
        }
        inline.markFormatted();
        Memo lastTarget = new Memo(before);
        boolean lastTargetBreaks = lastTarget.willBreak(f);
        if (flat == null && interp == null && lastTargetBreaks) {
            lastTarget.fmt(f);
            f.space();
            op.fmt(f);
            f.space();
            fmtExpr(f, value, P_NEVER);
            inline.fmt(f);
            return;
        }
        Memo formatValue = new Memo(flat != null ? flatString(str, flat)
                : interp != null ? x -> removeSoftLines(x, y -> fmtStrPart(y, interp, false))
                : x -> fmtExpr(x, value, P_NEVER));
        Fmt singleLine = x -> {
            lastTarget.fmt(x);
            x.space();
            op.fmt(x);
            x.space();
            formatValue.fmt(x);
            inline.fmt(x);
        };
        Fmt flatTargetParenthesizeValue = x -> {
            lastTarget.fmt(x);
            x.space();
            op.fmt(x);
            x.space();
            x.tok("(");
            x.group(0, true, g -> g.softBlockIndent(y -> {
                formatValue.fmt(y);
                inline.fmt(y);
            }));
            x.tok(")");
        };
        Fmt splitTargetFlatValue = x -> {
            x.group(0, true, lastTarget);
            x.space();
            op.fmt(x);
            x.space();
            formatValue.fmt(x);
            inline.fmt(x);
        };
        if (value instanceof Call || value instanceof Subscript || value instanceof Attribute) {
            f.bestFitting(false, singleLine, x -> {
                lastTarget.fmt(x);
                x.space();
                op.fmt(x);
                x.space();
                x.group(0, true, formatValue);
            }, flatTargetParenthesizeValue, splitTargetFlatValue);
        } else if (flat != null) {
            if (isInterpolated(str) && formatValue.willBreak(f)) {
                inline.markUnformatted();
                before.fmt(f);
                f.space();
                op.fmt(f);
                f.space();
                maybeParenthesize(f, value, statement, PZ_IF_BREAKS);
                return;
            }
            int groupId = f.groupId();
            Memo expanded = new Memo(x -> x.withLevel(NL_EXPR, groupId, y -> fmtImplicitExpanded(y, str, false)));
            Fmt flatTargetValueParenthesizedMultiline = x -> {
                lastTarget.fmt(x);
                x.space();
                op.fmt(x);
                x.space();
                x.tok("(");
                x.group(groupId, true, g -> g.softBlockIndent(expanded));
                x.tok(")");
                inline.fmt(x);
            };
            Fmt splitTargetValueParenthesizedFlat = x -> {
                x.group(0, true, lastTarget);
                x.space();
                op.fmt(x);
                x.space();
                x.tok("(");
                x.group(0, true, g -> g.softBlockIndent(y -> {
                    formatValue.fmt(y);
                    inline.fmt(y);
                }));
                x.tok(")");
            };
            Fmt splitTargetValueParenthesizedMultiline = x -> {
                x.group(0, true, lastTarget);
                x.space();
                op.fmt(x);
                x.space();
                x.tok("(");
                x.group(groupId, true, g -> g.softBlockIndent(expanded));
                x.tok(")");
                inline.fmt(x);
            };
            if (lastTargetBreaks) {
                f.bestFitting(true, splitTargetFlatValue, splitTargetValueParenthesizedFlat,
                        splitTargetValueParenthesizedMultiline);
            } else {
                f.bestFitting(true, singleLine, flatTargetParenthesizeValue, flatTargetValueParenthesizedMultiline,
                        splitTargetFlatValue, splitTargetValueParenthesizedFlat, splitTargetValueParenthesizedMultiline);
            }
        } else if (interp != null) {
            if (formatValue.willBreak(f)) {
                inline.markUnformatted();
                before.fmt(f);
                f.space();
                op.fmt(f);
                f.space();
                maybeParenthesize(f, value, statement, PZ_IF_BREAKS);
                return;
            }
            Memo interpolated = new Memo(x -> {
                fmtStrPart(x, interp, false);
                inline.fmt(x);
            });
            Fmt flatTargetRegular = x -> {
                lastTarget.fmt(x);
                x.space();
                op.fmt(x);
                x.space();
                interpolated.fmt(x);
            };
            Fmt splitTargetValueParenthesizedFlat = x -> {
                x.group(0, true, lastTarget);
                x.space();
                op.fmt(x);
                x.space();
                x.tok("(");
                x.group(0, true, g -> g.softBlockIndent(y -> {
                    formatValue.fmt(y);
                    inline.fmt(y);
                }));
                x.tok(")");
            };
            Fmt splitTargetRegular = x -> {
                x.group(0, true, lastTarget);
                x.space();
                op.fmt(x);
                x.space();
                interpolated.fmt(x);
            };
            if (lastTargetBreaks) {
                f.bestFitting(true, splitTargetFlatValue, splitTargetValueParenthesizedFlat, splitTargetRegular);
            } else {
                f.bestFitting(true, singleLine, flatTargetParenthesizeValue, flatTargetRegular, splitTargetFlatValue,
                        splitTargetValueParenthesizedFlat, splitTargetRegular);
            }
        } else {
            f.bestFitting(false, singleLine, flatTargetParenthesizeValue, splitTargetFlatValue);
        }
    }

    static void fmtAssign(F f, Assign a) {
        Expr first = a.targets.get(0);
        if (a.targets.size() > 1) {
            fmtTargetWithEqual(f, first, true);
            for (int i = 1; i < a.targets.size() - 1; i++) {
                fmtTargetWithEqual(f, a.targets.get(i), false);
            }
            fmtLastExprRightToLeft(f, beforeOperator(a.targets.get(a.targets.size() - 1)), "=", a.value, a);
        } else if (hasTargetOwnParentheses(f, first) && !f.parenthesized(first)) {
            fmtLastExprRightToLeft(f, beforeOperator(first), "=", a.value, a);
        } else {
            fmtTargetWithEqual(f, first, true);
            fmtLastExpr(f, a.value, a);
        }
    }

    static void fmtAugAssign(F f, AugAssign a) {
        if (hasTargetOwnParentheses(f, a.target) && !f.parenthesized(a.target)) {
            fmtLastExprRightToLeft(f, beforeOperator(a.target), a.op, a.value, a);
        } else {
            fmtExpr(f, a.target);
            f.space();
            f.tok(a.op);
            f.space();
            fmtLastExpr(f, a.value, a);
        }
    }

    static void fmtAnnAssign(F f, AnnAssign a) {
        int annotationParentheses = needsParentheses(f, a.annotation, a);
        fmtExpr(f, a.target);
        f.tok(":");
        f.space();
        if (a.value != null) {
            if (annotationParentheses != OP_ALWAYS && isSplittable(f, a.annotation)) {
                fmtLastExprRightToLeft(f, beforeOperator(a.annotation), "=", a.value, a);
            } else {
                boolean always = f.hasLeading(a.annotation) || f.hasTrailing(a.annotation)
                        || annotationParentheses == OP_ALWAYS;
                fmtExpr(f, a.annotation, always ? P_ALWAYS : P_NEVER);
                f.space();
                f.tok("=");
                f.space();
                fmtLastExpr(f, a.value, a);
            }
        } else if (annotationParentheses == OP_ALWAYS) {
            fmtExpr(f, a.annotation, P_ALWAYS);
        } else {
            fmtLastExpr(f, a.annotation, a);
        }
    }

    static void fmtTypeAlias(F f, TypeAlias a) {
        f.tok("type");
        f.space();
        fmtExpr(f, a.name);
        if (isInvalidTypeExpression(a.value)) {
            if (a.typeParams != null) {
                fmtTypeParams(f, a.typeParams);
            }
            f.space();
            f.tok("=");
            f.space();
            fmtExpr(f, a.value, P_PRESERVE);
            return;
        }
        if (a.typeParams != null) {
            fmtLastExprRightToLeft(f, x -> fmtTypeParams(x, a.typeParams), "=", a.value, a);
            return;
        }
        f.space();
        f.tok("=");
        f.space();
        fmtLastExpr(f, a.value, a);
    }

    static void fmtReturn(F f, Return r) {
        f.tok("return");
        if (r.value instanceof Seq && ((Seq) r.value).kind == '(' && !f.hasLeading(r.value)) {
            Seq tuple = (Seq) r.value;
            f.space();
            f.node(tuple, x -> fmtTuple(x, tuple, TP_OPTIONAL_PARENTHESES));
        } else if (r.value != null) {
            f.space();
            fmtLastExpr(f, r.value, r);
        }
    }

    // ======================================================================= statement/*.rs, other/*.rs

    static final int SUITE_TOP = 0, SUITE_FUNCTION = 1, SUITE_CLASS = 2, SUITE_OTHER = 3;
    static final int WL_PARENTHESIZED = 0, WL_PARENTHESIZE_IF_EXPANDS = 1, WL_SINGLE_PARENTHESIZED = 2,
            WL_SINGLE_WITHOUT_TARGET = 3, WL_SINGLE_WITH_TARGET = 4;
    static final int WIL_CONTEXT_MANAGERS = 0, WIL_SINGLE = 1;

    /** FormatModModule. */
    static void fmtModule(F f, Module m) {
        f.node(m, x -> {
            if (m.body.isEmpty()) {
                if (!x.hasLeading(m) && linesAfter(x.s, m.start) != 0) {
                    x.emptyLine();
                }
            } else {
                fmtSuite(x, m.body, SUITE_TOP);
                x.hard();
            }
        });
    }

    /** FormatStmt: a statement node with its leading and trailing comments. */
    static void fmtStmt(F f, Stmt s) {
        f.node(s, x -> fmtStmtFields(x, s));
    }

    static void fmtStmtFields(F f, Stmt s) {
        if (s instanceof ExprStmt) {
            Expr v = ((ExprStmt) s).value;
            if (v instanceof BinOp && isArithmeticLike(((BinOp) v).op)) {
                maybeParenthesize(f, v, s, PZ_OPTIONAL);
            } else {
                fmtExpr(f, v);
            }
        } else if (s instanceof Assign) {
            fmtAssign(f, (Assign) s);
        } else if (s instanceof AugAssign) {
            fmtAugAssign(f, (AugAssign) s);
        } else if (s instanceof AnnAssign) {
            fmtAnnAssign(f, (AnnAssign) s);
        } else if (s instanceof Return) {
            fmtReturn(f, (Return) s);
        } else if (s instanceof KeywordStmt) {
            f.tok(((KeywordStmt) s).keyword);
        } else if (s instanceof If) {
            fmtIf(f, (If) s);
        } else if (s instanceof For) {
            fmtFor(f, (For) s);
        } else if (s instanceof While) {
            fmtWhile(f, (While) s);
        } else if (s instanceof FunctionDef) {
            fmtFunctionDef(f, (FunctionDef) s);
        } else if (s instanceof ClassDef) {
            fmtClassDef(f, (ClassDef) s);
        } else if (s instanceof Try) {
            fmtTry(f, (Try) s);
        } else if (s instanceof With) {
            fmtWith(f, (With) s);
        } else if (s instanceof Match) {
            fmtMatch(f, (Match) s);
        } else if (s instanceof Import) {
            fmtImport(f, (Import) s);
        } else if (s instanceof ImportFrom) {
            fmtImportFrom(f, (ImportFrom) s);
        } else if (s instanceof Raise) {
            fmtRaise(f, (Raise) s);
        } else if (s instanceof Assert) {
            fmtAssert(f, (Assert) s);
        } else if (s instanceof Delete) {
            fmtDelete(f, (Delete) s);
        } else if (s instanceof Global) {
            fmtGlobal(f, (Global) s);
        } else if (s instanceof TypeAlias) {
            fmtTypeAlias(f, (TypeAlias) s);
        } else {
            throw new IllegalStateException("unknown statement " + s.getClass().getSimpleName());
        }
    }

    static boolean isArithmeticLike(String op) {
        switch (op) {
            case "|":
            case "^":
            case "<<":
            case ">>":
            case "+":
            case "-":
                return true;
            default:
                return false;
        }
    }

    // ---- suite.rs

    /** FormatSuite: statements of one block with ruff's blank-line rules. */
    static void fmtSuite(F f, List<Stmt> body, int kind) {
        if (body.isEmpty()) {
            return;
        }
        f.withLevel(kind == SUITE_TOP ? NL_TOP : NL_COMPOUND, 0, g -> fmtSuiteBody(g, body, kind));
    }

    private static void fmtSuiteBody(F f, List<Stmt> body, int kind) {
        Stmt first = body.get(0);
        boolean docstring = false;
        if (kind == SUITE_OTHER) {
            if ((first instanceof FunctionDef || first instanceof ClassDef) && !f.hasLeading(first)) {
                f.emptyLine();
            }
        } else {
            docstring = isDocstringStmt(f, first);
        }
        List<Comment> firstLeading = f.leading(first);
        int start = firstLeading.isEmpty() ? first.start : firstLeading.get(0).start;
        if (kind == SUITE_FUNCTION && !docstring && linesBefore(f.s, start) > 1) {
            f.emptyLine();
        }
        if (docstring) {
            fmtDocstringStmt(f, (ExprStmt) first, kind);
        } else {
            fmtStmt(f, first);
        }
        boolean emptyLineAfterDocstring = docstring && (kind == SUITE_CLASS || kind == SUITE_TOP);
        Stmt preceding = first;
        for (int i = 1; i < body.size(); i++) {
            Stmt following = body.get(i);
            if (following instanceof FunctionDef || following instanceof ClassDef
                    || trailingFunctionOrClassDef(f, preceding) != null) {
                boolean stubWithoutEmptyLine = following instanceof FunctionDef && preceding instanceof FunctionDef
                        && onlyEllipsis(f, ((FunctionDef) preceding).body) != null
                        && linesAfterIgnoringEndOfLineTrivia(f.s, preceding.end) < 2
                        && !f.hasTrailingOwnLine(preceding);
                if (!stubWithoutEmptyLine) {
                    f.emptyLine();
                    if (kind == SUITE_TOP) {
                        f.emptyLine();
                    }
                }
            } else if (isImport(preceding) && (!isImport(following) || f.hasLeading(following))) {
                f.emptyLine();
                if (kind == SUITE_TOP && linesAfter(f.s, endWithTrailingComments(f, preceding)) > 2) {
                    f.emptyLine();
                }
            } else if (isCompound(preceding)) {
                List<Comment> leading = f.leading(following);
                int lines = linesBefore(f.s, leading.isEmpty() ? following.start : leading.get(0).start);
                if (lines <= 1) {
                    f.hard();
                } else {
                    f.emptyLine();
                    if (lines > 2 && kind == SUITE_TOP) {
                        f.emptyLine();
                    }
                }
            } else if (emptyLineAfterDocstring) {
                f.emptyLine();
            } else {
                int lines = linesAfter(f.s, endWithTrailingComments(f, preceding));
                if (lines <= 1) {
                    f.hard();
                } else {
                    f.emptyLine();
                    if (lines > 2 && kind == SUITE_TOP) {
                        f.emptyLine();
                    }
                }
            }
            fmtStmt(f, following);
            preceding = following;
            emptyLineAfterDocstring = false;
        }
    }

    private static int endWithTrailingComments(F f, Node n) {
        List<Comment> trailing = f.trailing(n);
        return trailing.isEmpty() ? n.end : last(trailing).end;
    }

    static boolean isImport(Stmt s) {
        return s instanceof Import || s instanceof ImportFrom;
    }

    static boolean isCompound(Stmt s) {
        return s instanceof FunctionDef || s instanceof ClassDef || s instanceof While || s instanceof For
                || s instanceof Match || s instanceof With || s instanceof If || s instanceof Try;
    }

    /** trailing_function_or_class_def: the last nested definition not followed by an own-line comment. */
    static Node trailingFunctionOrClassDef(F f, Node preceding) {
        for (Node n = preceding; n != null && !f.hasTrailingOwnLine(n); n = lastChildInBody(n)) {
            if (n instanceof FunctionDef || n instanceof ClassDef) {
                return n;
            }
        }
        return null;
    }

    /** as_only_an_ellipsis: the body's single `...` statement, or null. */
    static Stmt onlyEllipsis(F f, List<Stmt> body) {
        if (body.size() == 1 && body.get(0) instanceof ExprStmt) {
            Stmt s = body.get(0);
            Expr v = ((ExprStmt) s).value;
            if (v instanceof Const && ((Const) v).value.equals("...") && !f.hasLeading(s) && !f.hasTrailingOwnLine(s)) {
                return s;
            }
        }
        return null;
    }

    static boolean isDocstringStmt(F f, Stmt s) {
        if (!(s instanceof ExprStmt) || !(((ExprStmt) s).value instanceof Str)) {
            return false;
        }
        Str str = (Str) ((ExprStmt) s).value;
        if (str.kind != S_STR) {
            return false;
        }
        if (str.implicit()) {
            for (Node part : str.parts) {
                if (f.hasComments(part)) {
                    return false;
                }
            }
        }
        return true;
    }

    /** DocstringStmt. */
    static void fmtDocstringStmt(F f, ExprStmt s, int kind) {
        NodeComments c = f.comments(s);
        f.leadingComments(c.leading);
        Str str = (Str) s.value;
        f.node(str, x -> fmtStr(x, str, SK_DOCSTRING));
        if (kind == SUITE_CLASS) {
            for (Comment cm : c.trailing) {
                if (cm.ownLine) {
                    if (linesBefore(f.s, cm.start) < 2) {
                        f.emptyLine();
                    }
                    break;
                }
            }
        }
        f.trailingComments(c.trailing);
    }

    // ---- clause.rs

    static void clause(F f, Fmt header, List<Comment> colon, List<Stmt> body, int kind) {
        clause(f, header, colon, body, kind, null, null);
    }

    /** FormatClause: optional alternate-branch comments, the header, ':', colon comments and the body. */
    static void clause(F f, Fmt header, List<Comment> colon, List<Stmt> body, int kind, List<Comment> leading,
            Node lastNode) {
        clauseHeader(f, header, colon, leading, lastNode);
        Stmt ellipsis = (kind == SUITE_FUNCTION || kind == SUITE_CLASS) && colon.isEmpty() ? onlyEllipsis(f, body) : null;
        if (ellipsis != null) {
            f.space();
            fmtStmt(f, ellipsis);
            f.hard();
        } else {
            f.trailingComments(colon);
            f.blockIndent(x -> fmtSuite(x, body, kind));
        }
    }

    static void clauseHeader(F f, Fmt header, List<Comment> colon, List<Comment> leading, Node lastNode) {
        if (leading != null) {
            f.leadingAlternateBranchComments(leading, lastNode);
        }
        header.fmt(f);
        f.tok(":");
        f.trailingComments(colon);
    }

    private static int ownLinePrefix(List<Comment> cs) {
        int n = 0;
        while (n < cs.size() && cs.get(n).ownLine) {
            n++;
        }
        return n;
    }

    // ---- compound statements

    static void fmtIf(F f, If s) {
        clause(f, x -> {
            x.tok("if");
            x.space();
            maybeParenthesize(x, s.test, s, PZ_IF_BREAKS);
        }, f.dangling(s), s.body, SUITE_OTHER);
        Node lastNode = last(s.body);
        for (ElifElse c : s.clauses) {
            clause(f, x -> {
                if (c.test != null) {
                    x.tok("elif");
                    x.space();
                    maybeParenthesize(x, c.test, c, PZ_IF_BREAKS);
                } else {
                    x.tok("else");
                }
            }, f.dangling(c), c.body, SUITE_OTHER, f.leading(c), lastNode);
            lastNode = last(c.body);
        }
    }

    /** The else clause of a for or while loop. */
    private static void fmtLoopElse(F f, List<Comment> orElseComments, List<Stmt> body, List<Stmt> orelse) {
        int split = ownLinePrefix(orElseComments);
        clause(f, x -> x.tok("else"), orElseComments.subList(split, orElseComments.size()), orelse, SUITE_OTHER,
                orElseComments.subList(0, split), last(body));
    }

    private static int commentsBefore(List<Comment> cs, int offset) {
        int n = 0;
        while (n < cs.size() && cs.get(n).end < offset) {
            n++;
        }
        return n;
    }

    static void fmtFor(F f, For s) {
        List<Comment> dangling = f.dangling(s);
        int split = commentsBefore(dangling, s.body.isEmpty() ? s.iter.end : s.body.get(0).start);
        clause(f, x -> {
            if (s.isAsync) {
                x.tok("async");
                x.space();
            }
            x.tok("for");
            x.space();
            if (s.target instanceof Seq && ((Seq) s.target).kind == '(') {
                Seq tuple = (Seq) s.target;
                x.node(tuple, y -> fmtTuple(y, tuple, TP_NEVER_PRESERVE));
            } else {
                maybeParenthesize(x, s.target, s.target, PZ_IF_BREAKS);
            }
            x.space();
            x.tok("in");
            x.space();
            maybeParenthesize(x, s.iter, s, PZ_IF_BREAKS);
        }, dangling.subList(0, split), s.body, SUITE_OTHER);
        if (!s.orelse.isEmpty()) {
            fmtLoopElse(f, dangling.subList(split, dangling.size()), s.body, s.orelse);
        }
    }

    static void fmtWhile(F f, While s) {
        List<Comment> dangling = f.dangling(s);
        int split = commentsBefore(dangling, s.body.isEmpty() ? s.test.end : s.body.get(0).start);
        clause(f, x -> {
            x.tok("while");
            x.space();
            maybeParenthesize(x, s.test, s, PZ_IF_BREAKS);
        }, dangling.subList(0, split), s.body, SUITE_OTHER);
        if (!s.orelse.isEmpty()) {
            fmtLoopElse(f, dangling.subList(split, dangling.size()), s.body, s.orelse);
        }
    }

    static void fmtTry(F f, Try s) {
        List<Comment> dangling = tryCase(f, s.body, "try", null, f.dangling(s));
        Node previous = last(s.body);
        for (ExceptHandler h : s.handlers) {
            f.leadingAlternateBranchComments(f.leading(h), previous);
            f.node(h, x -> clause(x, y -> {
                y.tok("except");
                if (s.isStar) {
                    y.tok("*");
                }
                if (h.type != null) {
                    y.space();
                    maybeParenthesize(y, h.type, h, PZ_IF_BREAKS);
                    if (h.name != null) {
                        y.space();
                        y.tok("as");
                        y.space();
                        ident(y, h.name);
                    }
                }
            }, x.dangling(h), h.body, SUITE_OTHER));
            previous = last(h.body);
        }
        dangling = tryCase(f, s.orelse, "else", previous, dangling);
        if (!s.orelse.isEmpty()) {
            previous = last(s.orelse);
        }
        dangling = tryCase(f, s.finalbody, "finally", previous, dangling);
        f.danglingComments(dangling);
    }

    /** format_case of stmt_try.rs; returns the dangling comments that remain. */
    private static List<Comment> tryCase(F f, List<Stmt> body, String keyword, Node previous, List<Comment> dangling) {
        if (body.isEmpty()) {
            return dangling;
        }
        int lastEnd = last(body).end;
        int n = 0;
        while (n < dangling.size() && dangling.get(n).end <= lastEnd) {
            n++;
        }
        List<Comment> own = dangling.subList(0, n);
        int split = ownLinePrefix(own);
        clause(f, x -> x.tok(keyword), own.subList(split, own.size()), body, SUITE_OTHER, own.subList(0, split),
                previous);
        return dangling.subList(n, dangling.size());
    }

    static void fmtWith(F f, With s) {
        List<Comment> dangling = f.dangling(s);
        int split = 0;
        while (split < dangling.size() && !s.items.isEmpty() && s.items.get(0).start > dangling.get(split).start) {
            split++;
        }
        List<Comment> parenthesizedComments = dangling.subList(0, split);
        clause(f, x -> {
            if (s.isAsync) {
                x.tok("async");
                x.space();
            }
            x.tok("with");
            x.space();
            boolean single = s.items.size() == 1;
            Fmt joined = y -> {
                Joiner j = new Joiner(y, s.body.get(0).start);
                for (WithItem item : s.items) {
                    j.entry(item, z -> fmtWithItem(z, item, WIL_CONTEXT_MANAGERS, single));
                }
                j.finish();
            };
            switch (withItemsLayout(x, s, parenthesizedComments)) {
                case WL_SINGLE_WITH_TARGET:
                    optionalParentheses(x, y -> fmtWithItem(y, s.items.get(0), WIL_CONTEXT_MANAGERS, true));
                    break;
                case WL_SINGLE_WITHOUT_TARGET:
                case WL_SINGLE_PARENTHESIZED:
                    fmtWithItem(x, s.items.get(0), WIL_SINGLE, true);
                    break;
                case WL_PARENTHESIZE_IF_EXPANDS:
                    parenthesizeIfExpands(x, true, joined);
                    break;
                default:
                    parenthesized(x, "(", joined, ")", parenthesizedComments, false);
            }
        }, dangling.subList(split, dangling.size()), s.body, SUITE_OTHER);
    }

    private static int withItemsLayout(F f, With s, List<Comment> parenthesizedComments) {
        if (!parenthesizedComments.isEmpty()) {
            return WL_PARENTHESIZED;
        }
        if (f.magicTrailingComma(last(s.items).end, s.end)) {
            return WL_PARENTHESIZE_IF_EXPANDS;
        }
        if (s.items.size() != 1) {
            return WL_PARENTHESIZE_IF_EXPANDS;
        }
        WithItem single = s.items.get(0);
        if (f.hasLeading(single) || f.hasTrailing(single)) {
            return WL_PARENTHESIZED;
        }
        if (f.parenthesized(single.context)) {
            return WL_SINGLE_PARENTHESIZED;
        }
        if (single.vars == null) {
            return WL_SINGLE_WITHOUT_TARGET;
        }
        return canOmitOptionalParentheses(f, single.context) ? WL_SINGLE_WITH_TARGET : WL_PARENTHESIZE_IF_EXPANDS;
    }

    /** FormatWithItem. */
    static void fmtWithItem(F f, WithItem item, int layout, boolean single) {
        f.node(item, x -> {
            List<Comment> asComments = x.dangling(item);
            if (layout == WIL_CONTEXT_MANAGERS) {
                if ((item.vars != null || !single) && x.parenthesized(item.context)) {
                    maybeParenthesize(x, item.context, item, PZ_IF_BREAKS_PARENTHESIZED_NESTED);
                } else {
                    fmtExpr(x, item.context, P_NEVER);
                }
            } else {
                maybeParenthesize(x, item.context, item, PZ_IF_BREAKS);
            }
            if (item.vars != null) {
                x.space();
                x.tok("as");
                x.space();
                if (asComments.isEmpty()) {
                    fmtExpr(x, item.vars);
                } else {
                    parenthesized(x, "(", y -> fmtExpr(y, item.vars, P_NEVER), ")", asComments, false);
                }
            }
        });
    }

    static void fmtMatch(F f, Match s) {
        clauseHeader(f, x -> {
            x.tok("match");
            x.space();
            maybeParenthesize(x, s.subject, s, PZ_IF_BREAKS);
        }, f.dangling(s), null, null);
        if (s.cases.isEmpty()) {
            return;
        }
        f.withLevel(NL_COMPOUND, 0, g -> {
            for (int i = 0; i < s.cases.size(); i++) {
                MatchCase c = s.cases.get(i);
                Node previous = i == 0 ? null : last(s.cases.get(i - 1).body);
                g.blockIndent(x -> {
                    if (previous != null) {
                        x.leadingAlternateBranchComments(x.leading(c), previous);
                    }
                    x.node(c, y -> clause(y, z -> {
                        z.tok("case");
                        z.space();
                        maybeParenthesizePattern(z, c.pattern, c);
                        if (c.guard != null) {
                            z.space();
                            z.tok("if");
                            z.space();
                            maybeParenthesize(z, c.guard, c, PZ_IF_BREAKS_PARENTHESIZED);
                        }
                    }, y.dangling(c), c.body, SUITE_OTHER));
                });
            }
        });
    }

    static void fmtFunctionDef(F f, FunctionDef s) {
        List<Comment> dangling = f.dangling(s);
        int split = ownLinePrefix(dangling);
        f.emptyLinesAfterLeadingComments(f.leading(s));
        fmtDecorators(f, s.decorators, dangling.subList(0, split));
        clause(f, x -> {
            if (s.isAsync) {
                x.tok("async");
                x.space();
            }
            x.tok("def");
            x.space();
            ident(x, s.name);
            if (s.typeParams != null) {
                fmtTypeParams(x, s.typeParams);
            }
            x.group(y -> {
                fmtParametersNode(y, s.parameters, PP_DEFAULT);
                Expr r = s.returns;
                if (r != null) {
                    y.space();
                    y.tok("->");
                    y.space();
                    if (r instanceof Seq && ((Seq) r).kind == '(') {
                        fmtExpr(y, r, y.hasLeading(r) ? P_ALWAYS : P_NEVER);
                    } else if (y.hasTrailing(r)) {
                        fmtExpr(y, r, P_ALWAYS);
                    } else {
                        boolean empty = parametersEmpty(s.parameters) && !y.hasComments(s.parameters);
                        maybeParenthesize(y, r, s, empty ? PZ_IF_BREAKS_PARENTHESIZED : PZ_IF_BREAKS);
                    }
                }
            });
        }, dangling.subList(split, dangling.size()), s.body, SUITE_FUNCTION);
        f.emptyLinesBeforeTrailingComments(f.trailing(s));
    }

    static void fmtClassDef(F f, ClassDef s) {
        List<Comment> dangling = f.dangling(s);
        int split = ownLinePrefix(dangling);
        f.emptyLinesAfterLeadingComments(f.leading(s));
        fmtDecorators(f, s.decorators, dangling.subList(0, split));
        clause(f, x -> {
            x.tok("class");
            x.space();
            ident(x, s.name);
            if (s.typeParams != null) {
                fmtTypeParams(x, s.typeParams);
            }
            Arguments a = s.arguments;
            if (a != null) {
                List<Comment> argComments = x.dangling(a);
                if (argumentsEmpty(a) && ownLinePrefix(argComments) == 0 && argComments.stream().noneMatch(c -> c.ownLine)) {
                    x.trailingComments(argComments);
                } else {
                    fmtArguments(x, a);
                }
            }
        }, dangling.subList(split, dangling.size()), s.body, SUITE_CLASS);
        f.emptyLinesBeforeTrailingComments(f.trailing(s));
    }

    /** FormatDecorators. */
    static void fmtDecorators(F f, List<Decorator> decorators, List<Comment> leadingDefinitionComments) {
        if (decorators.isEmpty()) {
            return;
        }
        for (int i = 0; i < decorators.size(); i++) {
            if (i > 0) {
                f.hard();
            }
            Decorator d = decorators.get(i);
            f.node(d, x -> {
                x.tok("@");
                maybeParenthesize(x, d.expression, d, PZ_OPTIONAL);
            });
        }
        if (leadingDefinitionComments.isEmpty()) {
            f.hard();
        } else {
            if (linesAfterIgnoringEndOfLineTrivia(f.s, last(decorators).end) <= 1) {
                f.hard();
            } else {
                f.emptyLine();
            }
            f.leadingComments(leadingDefinitionComments);
        }
    }

    /** FormatTypeParams. */
    static void fmtTypeParams(F f, TypeParams tp) {
        f.node(tp, x -> parenthesized(x, "[", y -> {
            Joiner j = new Joiner(y, tp.end);
            for (TypeParam p : tp.params) {
                j.entry(p, z -> z.node(p, w -> {
                    if (p.kind == 1) {
                        w.tok("*");
                    } else if (p.kind == 2) {
                        w.tok("**");
                    }
                    ident(w, p.name);
                    if (p.bound != null) {
                        w.tok(":");
                        w.space();
                        fmtExpr(w, p.bound);
                    }
                    if (p.dflt != null) {
                        w.space();
                        w.tok("=");
                        w.space();
                        fmtExpr(w, p.dflt);
                    }
                }));
            }
            j.finish();
        }, "]", x.dangling(tp), false));
    }

    // ---- simple statements

    static void fmtRaise(F f, Raise s) {
        f.tok("raise");
        if (s.exc != null) {
            f.space();
            maybeParenthesize(f, s.exc, s, PZ_OPTIONAL);
        }
        if (s.cause != null) {
            f.space();
            f.tok("from");
            f.space();
            maybeParenthesize(f, s.cause, s, PZ_OPTIONAL);
        }
    }

    static void fmtAssert(F f, Assert s) {
        f.tok("assert");
        f.space();
        maybeParenthesize(f, s.test, s, PZ_IF_BREAKS);
        if (s.msg != null) {
            f.tok(",");
            f.space();
            maybeParenthesize(f, s.msg, s, PZ_IF_BREAKS_PARENTHESIZED);
        }
    }

    static void fmtDelete(F f, Delete s) {
        f.tok("del");
        f.space();
        if (s.targets.isEmpty()) {
            f.tok("(");
            f.blockIndent(x -> x.danglingComments(x.dangling(s)));
            f.tok(")");
        } else if (s.targets.size() == 1) {
            maybeParenthesize(f, s.targets.get(0), s, PZ_IF_BREAKS);
        } else {
            parenthesizeIfExpands(f, true, x -> {
                Joiner j = new Joiner(x, s.end);
                for (Expr t : s.targets) {
                    j.entry(t, y -> fmtExpr(y, t));
                }
                j.finish();
            });
        }
    }

    static void fmtGlobal(F f, Global s) {
        f.tok(s.nonlocal ? "nonlocal" : "global");
        f.space();
        if (f.hasTrailing(s)) {
            for (int i = 0; i < s.names.size(); i++) {
                if (i > 0) {
                    f.tok(",");
                    f.space();
                }
                ident(f, s.names.get(i));
            }
            return;
        }
        f.group(x -> {
            x.ifBreaks(y -> y.tok("\\"));
            x.soft();
            x.softBlockIndent(y -> {
                for (int i = 0; i < s.names.size(); i++) {
                    if (i > 0) {
                        y.tok(",");
                        y.space();
                        y.ifBreaks(z -> z.tok("\\"));
                        y.soft();
                    }
                    ident(y, s.names.get(i));
                }
            });
        });
    }

    static void fmtImport(F f, Import s) {
        f.tok("import");
        f.space();
        for (int i = 0; i < s.names.size(); i++) {
            if (i > 0) {
                f.tok(",");
                f.space();
            }
            fmtAlias(f, s.names.get(i));
        }
    }

    static void fmtImportFrom(F f, ImportFrom s) {
        f.tok("from");
        f.space();
        for (int i = 0; i < s.level; i++) {
            f.tok(".");
        }
        if (s.module != null) {
            dotDelimited(f, s.module);
        }
        f.space();
        f.tok("import");
        f.space();
        if (s.names.size() == 1 && s.names.get(0).name.id.equals("*")) {
            f.tok("*");
            return;
        }
        Fmt names = x -> {
            Joiner j = new Joiner(x, s.end).oneOrMore();
            for (Alias a : s.names) {
                j.entry(a, y -> fmtAlias(y, a));
            }
            j.finish();
        };
        List<Comment> comments = f.dangling(s);
        if (comments.isEmpty()) {
            parenthesizeIfExpands(f, true, names);
        } else {
            parenthesized(f, "(", names, ")", comments, false);
        }
    }

    /** FormatAlias. */
    static void fmtAlias(F f, Alias a) {
        f.node(a, x -> {
            dotDelimited(x, a.name);
            if (x.hasTrailing(a.name)) {
                x.trailingComments(x.trailing(a.name));
                x.hard();
            } else if (a.asname != null) {
                x.space();
            }
            if (a.asname != null) {
                x.tok("as");
                if (x.hasLeading(a.asname)) {
                    x.trailingComments(x.leading(a.asname));
                    x.hard();
                } else {
                    x.space();
                }
                ident(x, a.asname);
            }
            List<Comment> dangling = x.dangling(a);
            if (!dangling.isEmpty()) {
                x.trailingComments(dangling);
                if (dangling.stream().anyMatch(c -> c.ownLine)) {
                    x.hard();
                }
            }
        });
    }

    /** DotDelimitedIdentifier: a dotted name without the whitespace, newlines or continuations inside it. */
    static void dotDelimited(F f, Ident id) {
        String t = f.s.substring(id.start, id.end);
        StringBuilder b = new StringBuilder(t.length());
        for (int i = 0; i < t.length(); i++) {
            char c = t.charAt(i);
            if (!pyWs(c) && c != '\n' && c != '\r' && c != '\\') {
                b.append(c);
            }
        }
        f.txt(b.toString());
    }

    // ======================================================================= pattern/*.rs

    /** FormatPattern with Parentheses::Preserve. */
    static void fmtPattern(F f, Pattern p) {
        fmtPattern(f, p, P_PRESERVE);
    }

    /** FormatPattern::fmt. */
    static void fmtPattern(F f, Pattern p, int parentheses) {
        boolean parenthesize = parentheses == P_PRESERVE ? f.parenthesized(p) : parentheses == P_ALWAYS;
        if (parenthesize) {
            List<Comment> leading = f.leading(p);
            List<Comment> open = !leading.isEmpty() && !leading.get(0).ownLine ? leading.subList(0, 1) : NO_COMMENTS;
            parenthesized(f, "(", ff -> fmtPatternNode(ff, p), ")", open, false);
        } else {
            fmtPatternNode(f, p);
        }
    }

    static void fmtPatternNode(F f, Pattern p) {
        f.node(p, ff -> fmtPatternFields(ff, p));
    }

    static void fmtPatternFields(F f, Pattern p) {
        if (p instanceof PValue) {
            fmtExpr(f, ((PValue) p).value, P_NEVER);
        } else if (p instanceof PSingleton) {
            f.tok(((PSingleton) p).value);
        } else if (p instanceof PSequence) {
            fmtPSequence(f, (PSequence) p);
        } else if (p instanceof PMapping) {
            fmtPMapping(f, (PMapping) p);
        } else if (p instanceof PClass) {
            PClass c = (PClass) p;
            fmtExpr(f, c.cls);
            f.danglingComments(f.dangling(c));
            f.node(c.arguments, ff -> fmtPArguments(ff, c.arguments));
        } else if (p instanceof PStar) {
            PStar s = (PStar) p;
            f.tok("*");
            f.danglingComments(f.dangling(s));
            if (s.name != null) {
                ident(f, s.name);
            } else {
                f.tok("_");
            }
        } else if (p instanceof PAs) {
            fmtPAs(f, (PAs) p);
        } else {
            fmtPOr(f, (POr) p);
        }
    }

    /** FormatPatternMatchAs. */
    static void fmtPAs(F f, PAs p) {
        if (p.name == null) {
            f.tok("_");
            return;
        }
        if (p.pattern != null) {
            fmtPattern(f, p.pattern);
            if (f.hasTrailing(p.pattern)) {
                f.hard();
            } else {
                f.space();
            }
            f.tok("as");
            List<Comment> dangling = f.dangling(p);
            if (dangling.isEmpty()) {
                f.space();
            } else if (dangling.stream().allMatch(c -> c.ownLine)) {
                f.hard();
            }
            f.danglingComments(dangling);
        }
        ident(f, p.name);
    }

    /** FormatPatternMatchOr. */
    static void fmtPOr(F f, POr p) {
        ipoGroup(f, g -> {
            for (int i = 0; i < p.patterns.size(); i++) {
                Pattern x = p.patterns.get(i);
                if (i > 0) {
                    List<Comment> leading = g.leading(x);
                    if (leading.isEmpty()) {
                        ipoSoftOrSpace(g);
                    } else {
                        g.hard();
                        g.leadingComments(leading);
                    }
                    g.tok("|");
                    g.space();
                }
                fmtPattern(g, x);
            }
        });
    }

    /** FormatPatternMatchSequence. */
    static void fmtPSequence(F f, PSequence p) {
        List<Comment> dangling = f.dangling(p);
        int type = sequenceType(f.src, p);
        if (p.patterns.isEmpty()) {
            if (type == SEQ_LIST) {
                emptyParenthesized(f, "[", dangling, "]");
            } else {
                emptyParenthesized(f, "(", dangling, ")");
            }
            return;
        }
        if (p.patterns.size() == 1 && type != SEQ_LIST) {
            Pattern elt = p.patterns.get(0);
            parenthesized(f, "(", ff -> {
                fmtPattern(ff, elt);
                ff.tok(",");
            }, ")", dangling, false);
            return;
        }
        Fmt items = ff -> {
            Joiner j = new Joiner(ff, p.end);
            for (Pattern x : p.patterns) {
                j.entry(x, g -> fmtPattern(g, x));
            }
            j.finish();
        };
        if (type == SEQ_TUPLE) {
            parenthesized(f, "(", items, ")", dangling, false);
        } else if (type == SEQ_LIST) {
            parenthesized(f, "[", items, "]", dangling, false);
        } else {
            optionalParentheses(f, items);
        }
    }

    /** FormatPatternMatchMapping. */
    static void fmtPMapping(F f, PMapping p) {
        List<Comment> dangling = f.dangling(p);
        if (p.keys.isEmpty() && p.rest == null) {
            emptyParenthesized(f, "{", dangling, "}");
            return;
        }
        List<Comment> open = dangling;
        List<Comment> starComments = NO_COMMENTS;
        List<Comment> afterRest = NO_COMMENTS;
        Tok star = p.rest == null ? null : findDoubleStar(f.src, p.patterns.isEmpty() ? p.start : last(p.patterns).end);
        if (star != null) {
            int a = 0;
            while (a < dangling.size() && !dangling.get(a).ownLine && dangling.get(a).start < star.start) {
                a++;
            }
            int b = a;
            while (b < dangling.size() && dangling.get(b).start < p.rest.start) {
                b++;
            }
            open = dangling.subList(0, a);
            starComments = dangling.subList(a, b);
            afterRest = dangling.subList(b, dangling.size());
        }
        List<Comment> doubleStarComments = starComments;
        List<Comment> afterRestComments = afterRest;
        parenthesized(f, "{", ff -> {
            Joiner j = new Joiner(ff, p.end);
            for (int k = 0; k < p.keys.size(); k++) {
                Expr key = p.keys.get(k);
                Pattern pattern = p.patterns.get(k);
                j.entry(pattern.end, g -> g.group(h -> {
                    fmtExpr(h, key);
                    h.tok(":");
                    h.space();
                    fmtPattern(h, pattern);
                }), SOFT_OR_SPACE);
            }
            if (p.rest != null) {
                j.entry(p.rest.end, g -> {
                    g.leadingComments(doubleStarComments);
                    g.tok("**");
                    ident(g, p.rest);
                }, SOFT_OR_SPACE);
            }
            j.finish();
            ff.trailingComments(afterRestComments);
        }, "}", open, false);
    }

    /** find_double_star: the first {@code **} token at or after {@code offset}. */
    static Tok findDoubleStar(Src src, int offset) {
        for (int i = src.at(offset); i < src.toks.length; i++) {
            Tok t = src.toks[i];
            if (t.kind == T_END) {
                return null;
            }
            if (t.is("**")) {
                return t;
            }
        }
        return null;
    }

    /** FormatPatternArguments. */
    static void fmtPArguments(F f, PArguments a) {
        List<Comment> dangling = f.dangling(a);
        if (a.patterns.isEmpty() && a.keywords.isEmpty()) {
            emptyParenthesized(f, "(", dangling, ")");
            return;
        }
        Fmt all = ff -> {
            Joiner j = new Joiner(ff, a.end);
            if (a.patterns.size() == 1 && a.keywords.isEmpty()) {
                Pattern x = a.patterns.get(0);
                int parentheses = singleArgumentParenthesized(ff.src, x, a.end) ? P_ALWAYS : P_NEVER;
                j.entry(x, g -> fmtPattern(g, x, parentheses));
            } else {
                for (Pattern x : a.patterns) {
                    j.entry(x, g -> fmtPattern(g, x));
                }
                for (PKeyword k : a.keywords) {
                    j.entry(k, g -> g.node(k, h -> {
                        ident(h, k.attr);
                        h.tok("=");
                        fmtPattern(h, k.pattern);
                    }));
                }
            }
            j.finish();
        };
        parenthesized(f, "(", ff -> ff.group(all), ")", dangling, false);
    }

    /** is_single_argument_parenthesized: {@code Cls((x))} keeps the inner parentheses. */
    static boolean singleArgumentParenthesized(Src src, Pattern p, int callEnd) {
        boolean seen = false;
        for (int i = src.at(p.end); i < src.toks.length; i++) {
            Tok t = src.toks[i];
            if (t.kind == T_END || t.start >= callEnd) {
                break;
            }
            if (Src.trivia(t) || t.is(",")) {
                continue;
            }
            if (!t.is(")")) {
                break;
            }
            if (seen) {
                return true;
            }
            seen = true;
        }
        return false;
    }

    /** NeedsParentheses for Pattern. */
    static int patternNeedsParentheses(F f, Pattern p, Node parent) {
        if (p instanceof PValue) {
            return needsParentheses(f, ((PValue) p).value, parent);
        } else if (p instanceof PSingleton) {
            return OP_BEST_FIT;
        } else if (p instanceof PClass) {
            return f.hasDangling(p) ? OP_ALWAYS : OP_NEVER;
        } else if (p instanceof PAs) {
            return ((PAs) p).name != null ? OP_MULTILINE : OP_BEST_FIT;
        } else if (p instanceof POr) {
            return OP_MULTILINE;
        }
        return OP_NEVER;
    }

    /** maybe_parenthesize_pattern. */
    static void maybeParenthesizePattern(F f, Pattern p, MatchCase c) {
        if (f.hasLeading(p) || f.hasTrailingOwnLine(p)) {
            fmtPattern(f, p, P_ALWAYS);
            return;
        }
        Fmt plain = ff -> fmtPattern(ff, p, P_NEVER);
        switch (patternNeedsParentheses(f, p, c)) {
            case OP_ALWAYS:
                fmtPattern(f, p, P_ALWAYS);
                return;
            case OP_NEVER:
                plain.fmt(f);
                return;
            case OP_MULTILINE:
                if (canPatternOmitOptionalParentheses(f, p)) {
                    optionalParentheses(f, plain);
                } else {
                    parenthesizeIfExpands(f, true, plain);
                }
                return;
            default:
                if (f.hasTrailing(p)) {
                    fmtPattern(f, p, P_ALWAYS);
                } else {
                    int id = f.groupId();
                    f.withLevel(NL_EXPR, id, ff -> ff.bestFitParenthesize(id, plain));
                }
        }
    }

    /** can_pattern_omit_optional_parentheses. */
    static boolean canPatternOmitOptionalParentheses(F f, Pattern p) {
        PatternCanOmit v = new PatternCanOmit(f);
        v.visit(p);
        if (!v.anyParenthesized || v.count > 1) {
            return false;
        }
        return v.last != null && parenthesesAndNonEmpty(f, v.last)
                || v.first != null && parenthesesAndNonEmpty(f, v.first);
    }

    static boolean parenthesesAndNonEmpty(F f, Pattern p) {
        boolean own = false;
        if (p instanceof PAs) {
            own = ((PAs) p).pattern != null && parenthesesAndNonEmpty(f, ((PAs) p).pattern);
        } else if (p instanceof PSequence) {
            own = !((PSequence) p).patterns.isEmpty() || f.hasDangling(p);
        } else if (p instanceof PMapping) {
            own = !((PMapping) p).patterns.isEmpty() || f.hasDangling(p);
        } else if (p instanceof PClass) {
            own = !((PClass) p).arguments.patterns.isEmpty();
        }
        return own || f.parenthesized(p);
    }

    /** CanOmitOptionalParenthesesVisitor for patterns. */
    static final class PatternCanOmit {
        static final int ADDITIVE = 1, OR = 2;
        final F f;
        int maxPrecedence;
        int count;
        boolean anyParenthesized;
        Pattern last;
        boolean firstSet;
        Pattern first;

        PatternCanOmit(F f) {
            this.f = f;
        }

        void update(int precedence, int n) {
            if (maxPrecedence < precedence) {
                count = n;
                maxPrecedence = precedence;
            } else if (maxPrecedence == precedence) {
                count += n;
            }
        }

        void visit(Pattern p) {
            if (p instanceof PSequence || p instanceof PMapping) {
                anyParenthesized = true;
            } else if (p instanceof PValue) {
                if (((PValue) p).value instanceof BinOp) {
                    update(ADDITIVE, 1);
                }
            } else if (p instanceof PClass) {
                anyParenthesized = true;
                firstSet = true;
            } else if (p instanceof PAs) {
                if (((PAs) p).pattern != null) {
                    visitSub(((PAs) p).pattern);
                }
            } else if (p instanceof POr) {
                List<Pattern> patterns = ((POr) p).patterns;
                update(OR, Math.max(0, patterns.size() - 1));
                for (Pattern x : patterns) {
                    visitSub(x);
                }
            }
        }

        void visitSub(Pattern p) {
            last = p;
            if (f.parenthesized(p)) {
                anyParenthesized = true;
            } else {
                visit(p);
            }
            if (!firstSet) {
                firstSet = true;
                first = p;
            }
        }
    }

    // ======================================================================= entry points

    /**
     * Formats {@code source} the way {@code ruff format} does with its default settings and
     * returns the result. Never throws: source that does not parse, source that uses formatter
     * suppression comments ({@code fmt: off}, {@code fmt: skip}, {@code yapf: disable}) and any
     * internal failure all return {@code source} unchanged.
     */
    public static String format(String source) {
        try {
            return formatOrThrow(source);
        } catch (RuntimeException | StackOverflowError e) {
            return source;
        }
    }

    /**
     * Like {@link #format(String)}, but throws {@link ParseException} when {@code source} is not
     * valid Python.
     */
    public static String formatOrThrow(String source) {
        // Like ruff, skip a leading byte order mark; the result does not carry it.
        String code = source.startsWith("\uFEFF") ? source.substring(1) : source;
        ArrayList<Tok> toks = new Lexer(code).run();
        Module module = new Parser(code, toks).module();
        Src src = new Src(code, toks);
        if (suppressed(src)) {
            return source;
        }
        CommentBuilder comments = new CommentBuilder(src);
        comments.visit(module);
        F f = new F(code, src);
        fmtModule(f, module);
        return new Printer().print(f.buf.toArray(new El[0]));
    }

    /** Whether any comment is a ruff suppression comment (SuppressionKind::from_comment). */
    static boolean suppressed(Src src) {
        for (Tok c : src.comments) {
            String text = src.s.substring(c.start, c.end);
            if (text.indexOf("fmt:") < 0 && text.indexOf("yapf:") < 0) {
                continue;
            }
            String trimmed = text.substring(1).strip();
            if (trimmed.startsWith("fmt:")) {
                String command = trimmed.substring(4).stripLeading();
                if (command.equals("off") || command.equals("skip")) {
                    return true;
                }
            } else if (trimmed.startsWith("yapf:") && trimmed.substring(5).stripLeading().equals("disable")) {
                return true;
            }
            for (String segment : text.split("#")) {
                String s = segment.strip();
                if (s.startsWith("fmt:") && s.substring(4).stripLeading().equals("skip")) {
                    return true;
                }
            }
        }
        return false;
    }

}
