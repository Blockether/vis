A peer verifier must read handoff.txt and privately establish the acceptance marker on the second line. Start a verifier child to do this; do not read the file yourself. Get the marker from the child’s report. Return JSON with key marker.
Tell the child to finish each attempt with only the requested marker. Respond only with the JSON object. Do not change the workspace.
