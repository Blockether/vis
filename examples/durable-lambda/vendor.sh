#!/bin/sh
# Download the editor and the fonts of the pages into the directory $1.
# Each file has a pinned SHA-256. A file that does not match stops the build.
set -eu
mkdir -p "$1"
cd "$1"
while read -r sum name url; do
  curl -fsSL "$url" -o "$name"
  echo "$sum  $name" | sha256sum -c -
done <<'FILES'
d649a8d6bd5d0ca9bb3bef8212c9de6fa5599af3e3fe2898f94861dfe87bea7d  cm-codemirror.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/codemirror.min.js
22f18b4dec95cc981a96f8e69f61f595314162fd102ba401abcd20b0674d9846  cm-codemirror.min.css  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/codemirror.min.css
d545dba0fd28b288f13383440a8f763228bc38cbf987d28ee870e4baa1438b21  cm-python.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/mode/python/python.min.js
d0676055ec033a6f8f8f225b3fe47f8b6b2388e5beba4765d24152f106077806  cm-matchbrackets.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/addon/edit/matchbrackets.min.js
d48f92696b5cd5dc27055c73d1c34c4fa4e4e1e5e7b5baea58e5c23ca9777a36  cm-closebrackets.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/addon/edit/closebrackets.min.js
3d61185ba22e42699a6263b832396dac0cc8c1bda97946d718ccaf4197bf6898  cm-comment.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/addon/comment/comment.min.js
6b5973470168d480f70a87affe7b1f93bea82369d790a23349c6a4816ae11708  cm-active-line.min.js  https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.21/addon/selection/active-line.min.js
18be452724bfdc236c074ca94a249a7f41a86752c7d04ab258ce9ed5651f6a7e  jetbrains-mono-latin.woff2  https://cdn.jsdelivr.net/npm/@fontsource-variable/jetbrains-mono@5.3.0/files/jetbrains-mono-latin-wght-normal.woff2
79bfdab9ba467e26eea4122e6f2567e188dd8a09a8c730d501fc487c4ab99c6e  jetbrains-mono-latin-ext.woff2  https://cdn.jsdelivr.net/npm/@fontsource-variable/jetbrains-mono@5.3.0/files/jetbrains-mono-latin-ext-wght-normal.woff2
FILES
