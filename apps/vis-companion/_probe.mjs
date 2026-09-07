import Prism from "prismjs";
import "prismjs/components/prism-python.js";
const src = process.argv[2];
for (const line of src.split("\n")) {
  const toks = Prism.tokenize(line, Prism.languages.python);
  console.log(JSON.stringify(toks.map(t => typeof t === "string" ? t : [t.type, typeof t.content === "string" ? t.content.slice(0,30) : "…"])));
}