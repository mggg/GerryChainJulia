# GerryChain

[![Build Status](https://api.travis-ci.com/mggg/GerryChainJulia.svg?branch=main)](https://travis-ci.com/mggg/GerryChainJulia)
[![Code Coverage](https://codecov.io/gh/mggg/GerryChainJulia/branch/main/graph/badge.svg)](https://codecov.io/gh/mggg/GerryChainJulia/branch/main)
[![DOI](https://zenodo.org/badge/239854101.svg)](https://zenodo.org/badge/latestdoi/239854101)

`GerryChain` is a Julia package for building ensembles of districting plans using [Markov Chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) techniques. It is developed and maintained by the [Metric Geometry and Gerrymandering Group](https://www.mggg.org/) and our network of volunteers. We also have [a Python package of the same name](https://github.com/mggg/GerryChain).

The basic workflow is to start with the geometry of an initial plan and generate a large collection of sample plans for comparison. Usually, we will constrain these sampled plans in such a way that they perform at least as well as the initial plan according to traditional districting principles, such as population balance or compactness. Comparing the initial plan to the ensemble provides quantitative tools for measuring whether or not it is an outlier among the sampled plans.

## Getting set up
If you already have Julia installed, then installing `GerryChain` is easy using Julia's in-built package manager `Pkg`.
```
using Pkg; Pkg.add("GerryChain")
using GerryChain
```
If you need to install Julia, use Julia in Jupyter Notebooks or use learn how to use `GerryChain` in a specific virtual environment, head over to [our more detailed setup instructions](https://github.com/mggg/GerryChainJulia/wiki/Setting-up-your-environment).

## Useful links
[Running Your First Chain](https://github.com/mggg/GerryChainJulia/wiki/Getting-started-with-a-chain)

[API Reference](https://github.com/mggg/GerryChainJulia/wiki/API-Reference)

If you have any issues or requests, please do not hesitate to [raise an issue.](https://github.com/mggg/GerryChainJulia/issues)

## Citations
You can cite the Recombination algorithm as
```
@misc{deford2019recombination,
    title={Recombination: A family of Markov chains for redistricting},
    author={Daryl DeFord and Moon Duchin and Justin Solomon},
    year={2019},
    eprint={1911.05725},
    archivePrefix={arXiv},
    primaryClass={cs.CY}
}
```
The current version of the code can be cited as
```
@software{gerrychain_julia_2020_4064975,
  author       = {Parker Rule and
                  Matthew Sun and
                  Bhushan Suwal},
  title        = {mggg/GerryChainJulia: v0.1.1},
  month        = oct,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v0.1.1},
  doi          = {10.5281/zenodo.4064975},
  url          = {https://doi.org/10.5281/zenodo.4064975}
}
```
