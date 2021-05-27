# GerryChain

![Build and Test status](https://github.com/mggg/GerryChainJulia/workflows/Build%20and%20Test/badge.svg)
[![Code Coverage](https://codecov.io/gh/mggg/GerryChainJulia/branch/main/graph/badge.svg)](https://codecov.io/gh/mggg/GerryChainJulia/branch/main)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4649464.svg)](https://doi.org/10.5281/zenodo.4649464)

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
@article{DeFord2021Recombination,
journal = {Harvard Data Science Review},
doi = {10.1162/99608f92.eb30390f},
note = {https://hdsr.mitpress.mit.edu/pub/1ds8ptxu},
title = {Recombination: A Family of Markov Chains for Redistricting},
url = {https://hdsr.mitpress.mit.edu/pub/1ds8ptxu},
author = {DeFord, Daryl and Duchin, Moon and Solomon, Justin},
date = {2021-03-31},
year = {2021},
month = {3},
day = {31},
}
```
The current version of the code can be cited as
```
@software{gerrychain_julia_4649464,
  author       = {Parker Rule and
                  Matthew Sun and
                  Bhushan Suwal
                  },
  title        = {mggg/GerryChainJulia: Minor fixes + Save as HDF5},
  month        = mar,
  year         = 2021,
  publisher    = {Zenodo},
  version      = {v0.1.3},
  doi          = {10.5281/zenodo.4649464},
  url          = {https://doi.org/10.5281/zenodo.4649464}
}
```
