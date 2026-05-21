#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SBCL_WRAP="${SBCL_WRAP:-sbcl-wrap}"

pass() { echo "PASS: $1"; }
fail() { echo "FAIL: $1"; exit 1; }

# ── Test 1: bad args → exit 88 ────────────────────────────────────────────────
set +e
"$SBCL_WRAP" 2>/dev/null
EXIT=$?
set -e
[ "$EXIT" -eq 88 ] && pass "exit 88 on missing args" || fail "expected exit 88, got $EXIT"

# ── Test 2: smoke (no libraries) ─────────────────────────────────────────────
OUT=$("$SBCL_WRAP" -- "$SCRIPT_DIR/smoke.lisp")
[ "$OUT" = "smoke-ok" ] && pass "smoke (first run, image built)" || fail "smoke: expected 'smoke-ok', got '$OUT'"

OUT=$("$SBCL_WRAP" -- "$SCRIPT_DIR/smoke.lisp")
[ "$OUT" = "smoke-ok" ] && pass "smoke (second run, cached image)" || fail "smoke cached: expected 'smoke-ok', got '$OUT'"

# ── Test 3: with-libs (alexandria + puri) ────────────────────────────────────
OUT=$("$SBCL_WRAP" alexandria puri -- "$SCRIPT_DIR/with-libs.lisp")
[ "$OUT" = "libs-ok" ] && pass "with-libs (first run, image built)" || fail "with-libs: expected 'libs-ok', got '$OUT'"

OUT=$("$SBCL_WRAP" alexandria puri -- "$SCRIPT_DIR/with-libs.lisp")
[ "$OUT" = "libs-ok" ] && pass "with-libs (second run, cached image)" || fail "with-libs cached: expected 'libs-ok', got '$OUT'"

echo ""
echo "All tests passed."
