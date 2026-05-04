#!/usr/bin/env bash
set -euo pipefail

# Workflow
node scripts/semantic-release.js "$1"
npm run prettier:fix NEWS.md
R CMD build . --no-build-vignettes

# Verify exactly one correctly-named tarball was created
TARBALL_COUNT=$(find . -maxdepth 1 -name 'MStargetR_*.tar.gz' -printf '.' | wc -c)
if [ "$TARBALL_COUNT" -eq 0 ]; then
  echo "::error::R CMD build did not produce a tarball"
  exit 1
elif [ "$TARBALL_COUNT" -gt 1 ]; then
  echo "::error::Multiple tarballs found — expected exactly one MStargetR_*.tar.gz"
  exit 1
fi

TARBALL=$(ls MStargetR_*.tar.gz)
if ! echo "$TARBALL" | grep -qE '^MStargetR_[0-9]+\.[0-9]+\.[0-9]+\.tar\.gz$'; then
  echo "::error::Tarball name '$TARBALL' does not match expected pattern MStargetR_X.Y.Z.tar.gz"
  exit 1
fi
