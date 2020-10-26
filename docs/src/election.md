# Election

Unsurprisingly, a key use of `GerryChain` is to analyze the electoral outcomes
under different districting plans.
If you wanted to, you could write a bunch of `AbstractScore`s to measure
election outcomes - or you could use the API we've already made for you!

```@index
Order = [:type, :function]
Pages   = ["election.md"]
```

Once initialized, the properties you can access in the `Election` struct are:
## Properties
| Field                             | Description                                            |
|-----------------------------------|--------------------------------------------------------|
| `name` (String)                   | name of the Election                                   |
| `parties` (Array{String, 1})      | array of names of different parties                    |
| `vote_counts` (Array{Int64, 2})   | matrix of vote counts (row = district, column = party) |
| `vote_shares` (Array{Float64, 2}) | matrix of vote shares (row = district, column = party) |

The way to initialize the `Election` object would be
```@docs
Election
```

## ElectionTracker

The ElectionTracker method returns a CompositeScore that first updates the vote count / share for changed districts and then proceeds to calculate other partisan metrics, as desired by the user. Re-calculating vote counts only for changed districts means that the CompositeScore does not perform redundant computations for all of the partisan metrics.

```@docs
ElectionTracker
```

## Election-related metrics

```@autodocs
Modules = [GerryChain]
Pages   = ["election.jl"]
Private = false
Filter = t -> t != ElectionTracker && t != Election
```

## Usage

```
election = Election("SEN10", ["SEN10D", "SEN10R"], partition.num_dists)
election_metrics = [   # optional
    vote_count("count_d", election, "SEN10D"),
    vote_share("share_d", election, "SEN10D"),
    efficiency_gap("efficiency_gap", election, "SEN10D"),
    seats_won("seats_won", election, "SEN10D"),
]
...
scores = [
    ...
    ElectionTracker(election, election_metrics)
    ...
]
...
chain_data = recom_chain(graph, partition, population_constraint, num_steps, scores)
```
