# Markov Chains

The following methods are used to "run" the chain; i.e., initiate the process of
sequentially generating and evaluating districting plans.

**We highly recommend using the ReCom plan proposal method.**

You can think of the chain as a loop that progresses like this:

(generate proposal for new plan that fits within constraints
➡ decide whether to accept the proposal
➡ update partition to reflect new districting plan
➡ record value of scores on the new plan)
x `num_steps`.

```@index
Order = [:type, :function]
Pages   = ["chains.md"]
```

## ReCom Chain

Runs a Markov Chain for `num_steps` steps using ReCom. In summary, the ReCom proposal method works as follows: merge two districts in the plan, generate a minimum spanning tree for the precincts in the merged district, then "split" the merged district into two new districts by finding a population-balanced cut of the MST. This method could potentially be used to merge/split an arbitrary number of districts, but currently, our implementation only supports merging 2 districts and splitting into 2 new districts.

```@docs
recom_chain
```

## Flip chain

The `flip_chain` method is quite similar to the `recom_chain` method. The only difference is how new plans are generated at each step of the chain. Out of the set of cut edges in a given plan, where a cut edge is defined to be an edge in the dual graph that crosses from a node in one district to a node in a different district, one cut edge is randomly selected, and one of the two precincts is "flipped" to the district of the other precinct.

Runs a Markov Chain for `num_steps` steps using Flip proposals.

```@docs
flip_chain
```

## API
```@autodocs
Modules = [GerryChain]
Pages   = ["recom.jl", "flip.jl"]
Private = false
Filter = t -> t != recom_chain && t != flip_chain
```
