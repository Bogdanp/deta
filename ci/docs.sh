#!/usr/bin/env bash

set -euo pipefail

pushd /github/workspace
scribble +m --dest doc --redirect-main 'http://docs.racket-lang.org/' deta-doc/deta.scrbl
popd
