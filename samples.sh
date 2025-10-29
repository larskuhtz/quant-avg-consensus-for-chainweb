#!/usr/bin/env bash

PROG=$(cabal list-bin quant-avg-consensus)

GRAPHS=("petersen" "twenty" "d3k4" "d4k3" "d4k4" "d5k3" "d5k4" "hoffman")

mkdir -p data

for GRAPH in "${GRAPHS[@]}"; do
    file="data/out-${GRAPH}.csv"
    echo "Processing graph: ${GRAPH}, output file: ${file}"
    rm -f "${file}"
    $PROG samples -g "${GRAPH}" -s 100000 -p 1000 > "${file}"
done

