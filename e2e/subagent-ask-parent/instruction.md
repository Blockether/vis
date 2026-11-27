In your first block, make a release code with `uuid.uuid4().hex`. Keep the code in a variable. Print `f"release-code={code}"`, where `code` is your variable.

Start one child. Give it write access to `code.txt`. Give it this exact task:

Write the release code alone into code.txt, then answer done. You do not have the code; your leader has it.

Wait for the child's question. Reply to that question with the code alone. Do not give the code to the child before it asks. Do not write `code.txt` yourself. Wait for the child to finish, then answer `done`.
