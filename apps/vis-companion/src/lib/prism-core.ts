import Prism from 'prismjs';

// Evaluate Prism's CommonJS factory before the grammar side-effect imports.
// Importing it alongside those scripts lets the production bundler defer the
// factory until after grammars have already tried to read the global Prism.
export default Prism;
