# Quantized Average Consensus for Degree-Diameter Graphs in Chainweb

This repository implements experiments for the convergence of quantized average
consensus for degree-diameter graphs that are relevant for Chainweb. The
algorithm is used in the fork voting implementation of Chainweb.

The goal of the experiments in this repository is to estimate the number of
steps that is required for all chains in a Chainweb to reach consensus with high
probability. Ideally, the achieved consensus should be close to the the true
average of the initial values. But this is not a strict requirement as long as
the result value is within a small error margin.

The results show that both objectives can be achieved in less that than 120
steps which corresponds to one difficulty-adjustment epoch of Chainweb.

## Usage

Build:

```bash
cabal build
```

Generate experimental results for all graphs:

```bash
bash samples.sh
```

This will take a while. Resulting data is stored in the `./data` folder.

To view and analyze the results and generate plots open and run the Jupyter
notebook `quant-avg-consensus.ipynb`.

To estimate error margins and convergence for a given step count use:

```bash
cabal run quant-avg-consensus -g GRAPH -s SAMPLE_COUNT -p PRECISION -c STEP_COUNT
```

For instance, for the twenty chain graph you can run:

```bash
for ((i=43; i<47; i++)) ; do
    cabal run quant-avg-consensus -g 'twenty' -s 100000 -p 1000 -c $i 2>/dev/null
done
```

This generates an output that looks like similar to the following:


```json
{"convergenceCount":999547,"delta":{"avg":4.53e-4,"max":1,"median":0,"min":0},"err":{"avg":2.5327500000000343e-2,"max":0.5,"median":4.9999999999272404e-2,"min":-0.4500000000007276},"steps":43}
{"convergenceCount":999980,"delta":{"avg":2.0e-5,"max":1,"median":0,"min":0},"err":{"avg":2.5574049999999855e-2,"max":0.5,"median":4.9999999999272404e-2,"min":-0.4500000000007276},"steps":44}
{"convergenceCount":1000000,"delta":{"avg":0,"max":0,"median":0,"min":0},"err":{"avg":2.4516199999999887e-2,"max":0.5,"median":0,"min":-0.4500000000007276},"steps":45}
{"convergenceCount":1000000,"delta":{"avg":0,"max":0,"median":0,"min":0},"err":{"avg":2.485050000000043e-2,"max":0.5,"median":0,"min":-0.4500000000007276},"steps":46}
```
