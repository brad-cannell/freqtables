# Chat Summary — 2026-02-23

## Topics Covered

### Claude Code File Access
- Claude Code can read **any file in the project**, not just the currently open file.
- It has access to the full filesystem and can search for files using glob patterns, grep through file contents, and explore the entire project directory.
- The open file shown in the IDE context is just a hint — it does not limit what Claude can access.

### Saving a Chat to Markdown
- Claude Code does not have a built-in command to export the full conversation to a markdown file.
- Alternatives:
  - Manually copy the conversation from the UI.
  - Ask Claude to write a summary/notes file (this file).
  - Check the [Claude Code GitHub issues page](https://github.com/anthropics/claude-code/issues) for newer features.
