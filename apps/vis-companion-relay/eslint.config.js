import tseslint from "typescript-eslint";

export default tseslint.config(
  { ignores: ["src/rooms/generated/**"] },
  ...tseslint.configs.recommended,
  { rules: { "@typescript-eslint/no-explicit-any": "off" } },
);
