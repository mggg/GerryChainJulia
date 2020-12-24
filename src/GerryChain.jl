module GerryChain
using JSON
using SparseArrays
using LightGraphs
using Random
using DataStructures
using Statistics
using DelimitedFiles
using PyPlot
import Shapefile
import LibGEOS
import LibSpatialIndex
using Logging

export

AbstractGraph, BaseGraph, Partition,

get_attributes, get_populations_and_assignments, get_district_nodes,
get_district_populations, get_district_adj_and_cut_edges,
get_subgraph_population,
induced_subgraph_edges, update_partition_adjacency,

# balance edges
random_kruskal_mst,

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
CompositeScore,
AbstractScore,
ChainScoreData,
score_initial_partition, score_partition_from_proposal, eval_score_on_district,
get_scores_at_step, eval_score_on_partition, save_scores_to_csv,
save_scores_to_json, save_scores_to_hdf5, get_score_values, num_cut_edges,
coerce_aggregated_attributes!,

# acceptance functions
always_accept, satisfies_acceptance_fn,

# election
AbstractElection, Election, ElectionTracker, vote_count, vote_share, seats_won,
mean_median, wasted_votes, efficiency_gap,

# plot
score_boxplot, score_histogram

include("./graph.jl")
include("./partition.jl")
include("./balance_edges.jl")
include("./geo.jl")
include("./proposals.jl")
include("./constraints.jl")
include("./scores.jl")
include("./recom.jl")
include("./flip.jl")
include("./accept.jl")
include("./election.jl")
include("./plot.jl")

end # module
