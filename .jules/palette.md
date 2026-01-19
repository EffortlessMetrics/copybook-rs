## 2024-06-03 - CLI Color Configuration
**Learning:** Explicitly disabling colors in CLI tools (`ColorChoice::Never`) significantly degrades the user experience by making help text and error messages harder to parse.
**Action:** Default to `ColorChoice::Auto` for all CLI tools unless there is a specific technical constraint preventing it (e.g., parsing logs in a strict legacy environment).
