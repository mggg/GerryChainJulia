module GerryChain
using JSON
using SparseArrays
using LightGraphs
using Random
using DataStructures

export

AbstractGraph, BaseGraph, Partition,

get_attributes, get_populations_and_assignments, get_district_nodes,
get_district_populations, get_district_adj_and_cut_edges,
weighted_kruskal_mst, get_subgraph_population,
induced_subgraph_edges,

# proposals
RecomProposal,

# constraints
PopulationConstraint,
satisfy_constraint,

# recom
update_partition!, recom_chain,

# scores
get_scores, get_scores_at_step

include("./graph.jl")
include("./partition.jl")
include("./proposals.jl")
include("./constraints.jl")
include("./scores.jl")
include("./recom.jl")

end # module
