# Architecture Overview

Vis is a recursive language model runtime around SCI.

Core loop:

1. assemble prompt and environment;
2. ask provider for Clojure forms;
3. evaluate forms in SCI;
4. append concrete observations to the journal;
5. continue or answer.

State is persisted as conversations, turns, iterations, evaluated blocks, vars, extension sidecars, and logs.

There is no extra workflow ledger.
