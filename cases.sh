#!/usr/bin/env bash

PROG=$(cabal list-bin quant-avg-consensus)

GRAPHS=("petersen" "twenty" "d3k4" "d4k3" "d4k4" "d5k3" "d5k4" "hoffman")

for GRAPH in "${GRAPHS[@]}"; do
    echo "Processing graph: ${GRAPH}"
    $PROG cases -g "${GRAPH}" -p 1000 -c 60 | jq
done

