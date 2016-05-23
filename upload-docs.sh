#!/bin/sh

. env-postgres-osx.sh
. venv/bin/activate

set -ex

# Check that we have aws
aws help > /dev/null

export STACK_YAML=stack-ghc-8.0.yaml

# Generate documentation
stack haddock

aws s3 --profile docs.futurice.com --region eu-west-1 sync --delete \
    $(stack path --local-doc-root) \
    s3://docs.futurice.com/haskell-futurice-prelude
