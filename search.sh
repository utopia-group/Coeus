#!/usr/bin/env sh

DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

export COEUS_HOUDINI_TIMEOUT=5
export COEUS_SPACER_TIMEOUT=10
export COEUS_PRECISE_ARITH=1
export COEUS_DEPTH_LIMIT=20
export COEUS_TACTIC_DEPTH_LIMIT=1024
export COEUS_AST_SIZE_LIMIT=500000
export LD_LIBRARY_PATH=${DIR}/_opam/lib/z3/:$LD_LIBRARY_PATH

exec ${DIR}/misc/parallel_singlerun.py -e ${DIR}/_build/install/default/bin/coeus "$@"
