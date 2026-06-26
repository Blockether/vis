-- The model can return markdown PROSE (`:content`) alongside a tool call — its
-- commentary while it acts (native tool calling). Persist it next to the code so
-- a resumed turn / the web trace history still shows what the model SAID, not
-- only what it ran. Nullable; empty for turns with no accompanying prose.
ALTER TABLE session_turn_iteration ADD COLUMN llm_assistant_prose TEXT;
