#!/usr/bin/env bash

set -euo pipefail

apt-get update
apt-get install -y --no-install-recommends gettext-base gpg ssh rsync

pushd /github/workspace
scribble +m --dest doc --redirect-main 'http://docs.racket-lang.org/' deta-doc/deta.scrbl
mv doc/deta.html doc/index.html

mkdir -p /tmp/secrets
gpg -q --batch --yes --decrypt --passphrase="$DETA_DOCS_DEPLOY_KEY_PASSPHRASE" -o /tmp/secrets/deploy ci/deploy.gpg
chmod 0600 /tmp/secrets/deploy
rsync \
    -e "ssh -P $DETA_DOCS_SSH_PORT -o StrictHostKeyChecking=no -i /tmp/secrets/deploy" \
    -a doc/ deta@"$DETA_DOCS_SSH_HOST":~/www/
popd
