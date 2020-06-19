#!/usr/bin/env bash

set -euo pipefail

sudo apt-get update
sudo apt-get install -y --no-install-recommends gettext-base gpg ssh rsync

raco scribble +m --dest doc --redirect 'https://docs.racket-lang.org/local-redirect/index.html' deta-doc/deta.scrbl
mv doc/deta.html doc/index.html

mkdir -p /tmp/secrets
gpg -q --batch --yes --decrypt --passphrase="$DETA_DOCS_DEPLOY_KEY_PASSPHRASE" -o /tmp/secrets/deploy ci/deploy.gpg
chmod 0600 /tmp/secrets/deploy
rsync \
    -e "ssh -p $DETA_DOCS_SSH_PORT -o StrictHostKeyChecking=no -i /tmp/secrets/deploy" \
    -a doc/ deta@"$DETA_DOCS_SSH_HOST":~/www/
