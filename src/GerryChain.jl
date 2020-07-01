module GerryChain
using JSON
using SparseArrays
using LightGraphs
using Random
using DataStructures
using Statistics

export

AbstractGraph, BaseGraph, Partition,

get_attributes, get_populations_and_assignments, get_district_nodes,
get_district_populations, get_district_adj_and_cut_edges,
weighted_kruskal_mst, get_subgraph_population,
induced_subgraph_edges, update_partition_adjacency,

# proposals
RecomProposal, FlipProposal, DummyProposal,

# constraints
PopulationConstraint,
ContiguityConstraint,
satisfy_constraint,

# recom
update_partition!, recom_chain,

# flip
flip_chain,

# scores
DistrictAggregate,
DistrictScore,
PlanScore,
score_initial_partition, score_partition_from_proposal, eval_score_on_district,
get_scores, get_scores_at_step, eval_score_on_partition, 

# acceptance functions
always_accept, satisfies_acceptance_fn,

# election
AbstractElection, Election, update_elections!, seats_won, total_vote_counts,
vote_counts_by_district, vote_shares_by_district,

# partisan metric
mean_median, efficiency_gap

include("./graph.jl")
include("./partition.jl")
include("./proposals.jl")
include("./constraints.jl")
include("./scores.jl")
include("./recom.jl")
include("./flip.jl")
include("./accept.jl")
include("./election.jl")
include("./partisan_metrics.jl")

end # module
