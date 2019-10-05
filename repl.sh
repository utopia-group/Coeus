#!/usr/bin/env sh

DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

export COEUS_HOUDINI_TIMEOUT=5
export COEUS_SPACER_TIMEOUT=10

exec ${DIR}/_build/install/default/bin/coeus-repl "$@"
