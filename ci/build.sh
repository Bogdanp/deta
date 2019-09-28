#!/usr/bin/env bash

set -euo pipefail

pushd /github/workspace
# TODO: remove once libsqlite3-x86_64-linux is on the pkg server
raco pkg install --name libsqlite3-x86_64-linux https://racket.defn.io/libsqlite3-x86_64-linux-3.29.0.tar.gz
raco pkg install --auto --batch deta-lib/ deta-doc/ deta-test/
raco test -j 4 deta-test/
popd
