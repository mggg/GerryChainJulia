# Acceptance Functions

Acceptance functions are user-defined functions passed to `recom_chain`/`flip_chain`
that are used to evaluate a proposal for the next state in a chain and generate
a probability for "accepting" the new plan.
If the plan is rejected, then the step in the Markov chain is considered to
be a "self-loop." Note that acceptance functions are conceptually different
from _constraints_, which are hard, deterministic requirements on every plan
in a chain. If a plan is generated that does not satisfy a constraint, then new
plans are generated until the constraint is satisfied, at which point the chain
makes a step to the constraint-satisfying plan. Contrast this to acceptance
functions, which generate _probabilities_ for accepting a plan, and when a plan
is not accepted, produce a self-loop in the chain.

Practically, acceptance functions are expected to accept a `Partition` object
and return a probability between 0 and 1. The reason for this is that acceptance
functions are really most useful in cases like the
[Metropolis-Hasting algorithm](https://en.wikipedia.org/wiki/Metropolis%E2%80%93Hastings_algorithm)
and "[burning in](http://background.uchicago.edu/~whu/Courses/Ast321_11/Projects/mcmc_helsby.pdf)".
If, for some reason, you need a deterministic acceptance function, you can simply have it return 0 or 1.

```@index
Order = [:type, :function]
Pages   = ["accept.md"]
```

The following is a sample acceptance function included in the `GerryChain` library:

```@docs
always_accept
```

## API
```@autodocs
Modules = [GerryChain]
Pages   = ["accept.jl"]
Private = false
Filter = t -> t != always_accept
```
