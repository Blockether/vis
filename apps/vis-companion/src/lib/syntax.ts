import Prism from './prism-core';
import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-clojure';
import 'prismjs/components/prism-css';
import 'prismjs/components/prism-diff';
import 'prismjs/components/prism-java';
import 'prismjs/components/prism-json';
import 'prismjs/components/prism-markdown';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-rust';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-jsx';
import 'prismjs/components/prism-tsx';
import 'prismjs/components/prism-yaml';

// The code colours of the transcript and of a review: one Prism pass, one class per token.

const languageAliases: Record<string, string> = {
  clj: 'clojure',
  cljc: 'clojure',
  cljs: 'clojure',
  edn: 'clojure',
  js: 'javascript',
  jsx: 'jsx',
  md: 'markdown',
  py: 'python',
  rs: 'rust',
  sh: 'bash',
  shell: 'bash',
  ts: 'typescript',
  yml: 'yaml',
};

const syntaxTokenClasses: Record<string, string> = {
  boolean: 'text-code-syntax-number',
  builtin: 'text-code-syntax-special',
  char: 'text-code-syntax-string',
  className: 'text-code-syntax-special',
  comment: 'italic text-code-syntax-comment',
  constant: 'text-code-syntax-number',
  decorator: 'text-code-syntax-special',
  function: 'text-code-syntax-special',
  important: 'font-semibold text-code-syntax-keyword',
  keyword: 'font-medium text-code-syntax-keyword',
  number: 'text-code-syntax-number',
  operator: 'text-code-syntax-special',
  regex: 'text-code-syntax-string',
  string: 'text-code-syntax-string',
  symbol: 'text-code-syntax-number',
};

function syntaxClass(token: Prism.Token): string {
  const aliases = Array.isArray(token.alias) ? token.alias : token.alias ? [token.alias] : [];
  for (const candidate of [token.type, ...aliases]) {
    const normalized = candidate === 'class-name' ? 'className' : candidate;
    if (syntaxTokenClasses[normalized]) return syntaxTokenClasses[normalized];
  }
  return 'text-code-foreground';
}

export type SyntaxSegment = { text: string; className: string };

// The colour the `<pre>` already paints. A segment that only asks for it needs
// no element of its own — see `SyntaxCodeBlock`.
export const CODE_DEFAULT_CLASS = 'text-code-foreground';

function flattenSyntax(
  tokens: (string | Prism.Token)[],
  inherited: string,
  out: SyntaxSegment[],
): void {
  for (const token of tokens) {
    if (typeof token === 'string') {
      if (token) out.push({ text: token, className: inherited });
      continue;
    }
    const className = syntaxClass(token);
    if (Array.isArray(token.content)) {
      flattenSyntax(token.content as (string | Prism.Token)[], className, out);
    } else if (typeof token.content === 'string') {
      if (token.content) out.push({ text: token.content, className });
    } else {
      flattenSyntax([token.content as Prism.Token], className, out);
    }
  }
}

export function highlightSegments(value: string, language: string): SyntaxSegment[] {
  const normalized = languageAliases[language] ?? language;
  const grammar = Prism.languages[normalized];
  if (!grammar) return [{ text: value, className: CODE_DEFAULT_CLASS }];
  const out: SyntaxSegment[] = [];
  flattenSyntax(Prism.tokenize(value, grammar), CODE_DEFAULT_CLASS, out);
  return out;
}

/** The Prism language of a file, from the end of its name; '' when no grammar knows it. */
export function languageOfPath(path: string): string {
  const name = path.slice(path.lastIndexOf('/') + 1).toLowerCase();
  const dot = name.lastIndexOf('.');
  if (dot <= 0) return '';
  const extension = name.slice(dot + 1);
  const language = languageAliases[extension] ?? extension;
  return Prism.languages[language] ? language : '';
}
