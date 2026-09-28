// The first module `main.tsx` evaluates. The memory probes only see what is registered
// after they are installed, so they go in before any other module can add a listener.
import { installPerfProbes, perfEnabled } from './lib/perf';

if (perfEnabled()) installPerfProbes();
