#!/usr/bin/env bash

set -euo pipefail

pushd /github/workspace
raco test -j 4 deta-test/ || true
popd
