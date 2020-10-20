
[![Build Status](https://api.travis-ci.com/mggg/GerryChainJulia.svg?branch=main)](https://travis-ci.com/mggg/GerryChainJulia)
[![Code Coverage](https://codecov.io/gh/mggg/GerryChainJulia/branch/main/graph/badge.svg)](https://codecov.io/gh/mggg/GerryChainJulia/branch/main)
[![DOI](https://zenodo.org/badge/239854101.svg)](https://zenodo.org/badge/latestdoi/239854101)

Welcome to the GerryChain Julia! Thank you for taking the time to use and/or
learn about our package. If you were looking for the Python wiki, go [here](https://gerrychain.readthedocs.io/).

This project is actively maintained by the [Metric Geometry
and Gerrymandering Group](https://mggg.org/) and is distributed under [the GPL v3.0 license](https://github.com/mggg/GerryChainJulia/blob/main/LICENSE).

If you would like to help support the project, please star the repository
[![STARS](https://img.shields.io/github/stars/mggg/GerryChainJulia?style=social)]
(https://github.com/mggg/GerryChainJulia) and share it with your colleagues.

### What is GerryChain?

GerryChain is a package [(also, written in Python)](https://github.com/mggg/GerryChain) for building ensembles of districting plans by using [Markov Chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) (MCMC). From the Python GerryChain page: "The basic workflow is to start with the geometry of an initial plan and generate a large collection of sample plans for comparison. Usually, we will constrain these sampled plans in such a way that they perform at least as well as the initial plan according to traditional districting principles, such as population balance or compactness. Comparing the initial plan to the ensemble provides quantitative tools for measuring whether or not it is an outlier among the sampled plans." You can think of each step in the Markov Chain as generating a new districting plan.

### What is Julia?

Julia is a [highly-performant](https://julialang.org/benchmarks/), [dynamically-typed](https://docs.julialang.org/en/v1/manual/types/) programming language suitable for scientific and numerical computing. You can see a comparison of performance between Julia and Python performed by NASA [here](https://modelingguru.nasa.gov/docs/DOC-2783); long story short, Julia tends to be much, much faster than Python. At the same time, Julia has the benefit of "looking like Python"; users of the Python GerryChain package will find that GerryChainJulia will feel and look familiar.

## _Why_ is GerryChain in Julia?

At a fundamental level, GerryChain exists because it is infeasible to compare an initial plan to the complete universe of possible districting plans for a particular region. (In many cases, there are more possible plans than atoms in the universe!) Through the GerryChain approach, we instead take a "random walk" in the universe of possible plans, generating some finite number of plans, to which we can then compare our initial plan. Speeding up the GerryChain computations with Julia means that we can generate a number of plans an order of magnitude greater than what we could achieve with the Python library in the same amount of time, with the intention that the subsequent analysis is more robust.

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

The most up to date citation of the code would be at [![DOI](https://zenodo.org/badge/239854101.svg)](https://zenodo.org/badge/latestdoi/239854101).

## How do I get started?

You'll want to start with [setting up your computing environment](installation.md). Next, you should check out [our tutorial on running your first GerryChain](getting_started.md)!. If you wish to contribute to the development of this project, please refer to our [contributing guidelines](https://github.com/mggg/GerryChainJulia/blob/main/CONTRIBUTING.md).
