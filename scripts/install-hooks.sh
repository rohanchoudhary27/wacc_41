#!/bin/bash
# Install git hooks for WACC project
# Run this script after cloning the repository

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}Installing git hooks...${NC}"

# Get the git root directory
GIT_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)

if [ -z "$GIT_ROOT" ]; then
  echo -e "${RED}Error: Not in a git repository${NC}"
  exit 1
fi

# Path to hooks directory
HOOKS_DIR="$GIT_ROOT/.git/hooks"
SCRIPTS_DIR="$GIT_ROOT/scripts/git-hooks"

# Check if scripts directory exists
if [ ! -d "$SCRIPTS_DIR" ]; then
  echo -e "${RED}Error: $SCRIPTS_DIR not found${NC}"
  exit 1
fi

# Install pre-commit hook
if [ -f "$SCRIPTS_DIR/pre-commit" ]; then
  cp "$SCRIPTS_DIR/pre-commit" "$HOOKS_DIR/pre-commit"
  chmod +x "$HOOKS_DIR/pre-commit"
  echo -e "${GREEN}✓ Installed pre-commit hook${NC}"
else
  echo -e "${YELLOW}Warning: pre-commit hook not found${NC}"
fi

# Install commit-msg hook
if [ -f "$SCRIPTS_DIR/commit-msg" ]; then
  cp "$SCRIPTS_DIR/commit-msg" "$HOOKS_DIR/commit-msg"
  chmod +x "$HOOKS_DIR/commit-msg"
  echo -e "${GREEN}✓ Installed commit-msg hook${NC}"
else
  echo -e "${YELLOW}Warning: commit-msg hook not found${NC}"
fi

echo ""
echo -e "${GREEN}Git hooks installed successfully${NC}"
echo ""
echo "The following hooks are now active:"
echo "  - pre-commit: Runs scalafix checks on staged Scala files"
echo "  - commit-msg: Validates commit messages (Conventional Commits with shortcodes)"
echo ""
echo "Commit message format:"
echo "  [shortcode] type(scope): description"
echo ""
echo "Example:"
echo "  [sak24] feat(parser): add while loop support"
echo ""
