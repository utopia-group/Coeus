#!/usr/bin/env sh

DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

export LD_LIBRARY_PATH=${DIR}/_opam/lib/z3/:$LD_LIBRARY_PATH
exec ${DIR}/_build/install/default/bin/coeus run "$@"
