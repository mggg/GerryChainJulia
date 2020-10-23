# GerryChain

[![Build Status](https://api.travis-ci.com/mggg/GerryChainJulia.svg?branch=main)](https://travis-ci.com/mggg/GerryChainJulia)
[![Code Coverage](https://codecov.io/gh/mggg/GerryChainJulia/branch/main/graph/badge.svg)](https://codecov.io/gh/mggg/GerryChainJulia/branch/main)
[![DOI](https://zenodo.org/badge/239854101.svg)](https://zenodo.org/badge/latestdoi/239854101)
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://mggg.github.io/GerryChainJulia/stable)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://mggg.github.io/GerryChainJulia/dev)

`GerryChain` is a Julia package for building ensembles of districting plans using [Markov Chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) techniques. It is developed and maintained by the [Metric Geometry and Gerrymandering Group](https://www.mggg.org/) and our network of volunteers. We also have [a Python package of the same name](https://github.com/mggg/GerryChain).

The basic workflow is to start with the geometry of an initial plan and generate a large collection of sample plans for comparison. Usually, we will constrain these sampled plans in such a way that they perform at least as well as the initial plan according to traditional districting principles, such as population balance or compactness. Comparing the initial plan to the ensemble provides quantitative tools for measuring whether or not it is an outlier among the sampled plans.

## Getting set up
If you already have Julia installed, then installing `GerryChain` is easy using Julia's in-built package manager `Pkg`.
```
using Pkg; Pkg.add("GerryChain")
using GerryChain
```
If you need to install Julia, use Julia in Jupyter Notebooks or use learn how to use `GerryChain` in a specific virtual environment, head over to [our more detailed setup instructions](https://mggg.github.io/GerryChainJulia/stable/installation/).

## Useful links
Please refer to [our documentation pages](https://mggg.github.io/GerryChainJulia/stable) for API references.

Follow [this link](https://mggg.github.io/GerryChainJulia/stable/getting_started/) if you want to get started on your first chain already.

If you want to contribute to this project, please check [our contributing guidelines](https://github.com/mggg/GerryChainJulia/blob/main/CONTRIBUTING.md).

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
@software{gerrychain_julia_2020_4111000,
  author       = {Bhushan Suwal and
                  Matthew Sun and
                  Parker Rule},
  title        = {mggg/GerryChainJulia: v0.1.2},
  month        = oct,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v0.1.2},
  doi          = {10.5281/zenodo.4111000},
  url          = {https://doi.org/10.5281/zenodo.4111000}
}
```
