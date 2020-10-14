# Partitions
A partition is a fancy word for a districting plan.
It assigns every node in the graph (e.g. precincts, counties, blocks) with a
particular label (e.g., Congressional district). We need an initial partition
because it will serve as the "seed" plan for our Markov chain.

## Properties
The `Partition`, once constructed, has the following properties:

| Partition Properties                       | Description                                                                              |
|--------------------------------------------|------------------------------------------------------------------------------------------|
| `num_dists` (Int)                          | Number of districts in the Partition                                                     |
| `num_cut_edges` (Int)                      | Number of cut edges in the Partition                                                     |
| `assignments` Array{Int, 1}                | An array of length(num_nodes) where `assignments[i]` is the assignment of node `i`       |
| `dist_populations` (Array{Int, 1}          | An array of length(num_districts) where `dist_populations[i]` is the population of district `i` |
| `cut_edges` Array{Int, 1}                  | An array of length(num_edges) where `cut_edges[i]` is 1 if edge `i` is a cut edge, and 0 otherwise  |
|`dist_adj` SparseMatrixCSC{Int, Int}        | An adjacency matrix of size length(num_districts) x length(num_districts) where districts `i` and `j` are adjacent if `dist_adj[i, j]` is the number of cut-edges between districts `i` and `j`. If the districts are not adjacent, this value is 0 |
| `dist_nodes` BitSet                        | An array of sets where `dist_nodes[i]` is the set of all district nodes of district `i`   |
| `parent` Union{Partition, Nothing}         | A field that holds the parent of the Partition. This value is `Nothing` in the first step of the chain when the Partition has no parent |

*Partition*

```@index
Order = [:type, :function]
Pages   = ["partition.md"]
```

## Initializing a Partition
A Partition can be initialized in the following way:

```@docs
Partition
```

## Full Docs

```@autodocs
Modules = [GerryChain]
Pages   = ["partition.jl"]
Private = false
Filter = t -> t != Partition
```
