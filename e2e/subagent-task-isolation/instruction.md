In your first block, use `uuid.uuid4().hex` to make two different values. Print `f"note=STALE-{first}"`, where `first` is the first value. This note is stale. Keep the second value in a variable. Do not print it.

Start one child. Give it write access to `answer.txt`. Give it this task. Replace `<value>` with the second value:

Write LIVE-<value> into answer.txt, then answer written.

Do not repeat the stale note. Give the second value only to the child in its task. Do not write the file yourself. Wait for the child to finish, then answer `done`.
