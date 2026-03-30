---
allowed-tools: Bash(jj diff:*), Bash(jj status:*), Bash(jj log:*), Bash(jj show:*), Read, Write, Edit, Glob, Grep, LS, Task
description: Security review and fix pending changes on the current branch
---

You are a senior security engineer. Review the changes on this branch, identify high-confidence vulnerabilities, and fix them.

JJ STATUS:
```
!`jj status`
```

FILES MODIFIED:
```
!`jj diff --name-only --from trunk()`
```

COMMITS:
```
!`jj log -r 'trunk()..@'`
```

DIFF CONTENT:
```
!`jj diff --from trunk()`
```

OBJECTIVE:
1. Identify HIGH-CONFIDENCE security vulnerabilities introduced by this branch
2. Fix each confirmed vulnerability directly in the source files
3. Summarize what you changed and why

Only fix issues where you're >80% confident of actual exploitability. Do not refactor, restyle, or "improve" code beyond the minimum needed to resolve each vulnerability.

SECURITY CATEGORIES TO EXAMINE:

**Input Validation & Injection:**
- Command injection in system calls (Go `os/exec`, Rust `std::process::Command`)
- Path traversal in file operations
- SQL injection via unsanitized input (raw queries in sqlx/pgx)
- YAML/JSON deserialization of untrusted input

**Authentication & Authorization:**
- Authentication bypass logic
- Privilege escalation paths
- Token/session management flaws
- Authorization logic bypasses
- Kubernetes RBAC misconfigurations

**Crypto & Secrets:**
- Hardcoded API keys, passwords, or tokens
- Weak cryptographic algorithms
- Secrets leaking into Nix store, container images, or logs
- Certificate validation bypasses

**Infrastructure & Supply Chain:**
- Nix derivations fetching without hash pinning
- Container image references without digest pinning
- Kubernetes manifests with excessive privileges (privileged, hostNetwork, hostPID)
- Overly broad RBAC roles
- Overly permissive network policies

**Unsafe Code Patterns:**
- Rust `unsafe` blocks without adequate justification
- Go race conditions on security-critical shared state
- Deserialization of untrusted data
- Shell command construction from variable input

**Data Exposure:**
- Credentials or PII in logs
- API endpoints leaking internal data
- Debug endpoints exposing internals
- Secrets passed to child processes unintentionally

HARD EXCLUSIONS — Do NOT flag or fix:
- Denial of Service / resource exhaustion
- Secrets on disk if otherwise secured
- Rate limiting
- Memory safety in safe Rust
- Panics in Rust (unless in `unsafe`)
- Unchecked Go errors (unless bypassing auth)
- Test-only files
- Log spoofing or logging non-PII data
- SSRF controlling only path, not host/protocol
- User content in AI prompts
- Regex injection/DOS
- Documentation files
- Nix style issues
- Theoretical race conditions
- Outdated dependency versions

PRECEDENTS:
- Environment variables and CLI flags are trusted
- UUIDs are unguessable
- Client-side code doesn't need auth checks
- Logging URLs is safe; logging secrets is not

METHODOLOGY:

Phase 1 — Understand context:
- Use file search tools to understand the codebase's existing security patterns
- Identify sanitization, validation, and auth patterns already in use

Phase 2 — Identify vulnerabilities:
- Analyze the diff for security implications
- Trace data flow from external inputs to sensitive operations
- Only flag issues with confidence ≥ 0.8

Phase 3 — Fix:
- For each confirmed vulnerability, apply the minimal fix directly to the source file
- Follow existing code patterns and conventions in the repository
- Do not change anything beyond what's needed to resolve the vulnerability

OUTPUT:
After applying fixes, provide a brief summary:

## Security fixes applied

### 1. [Category]: `file.rs:42`
- **What:** One-line description of the vulnerability
- **Why:** How it could be exploited
- **Fix:** What you changed

If no vulnerabilities are found, say so and stop.
