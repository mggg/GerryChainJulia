

"""
    kruskal_mst(graph::BaseGraph,
                edges::Array{Int, 1},
                nodes::Array{Int, 1},
                weights::Array{Float64, 1},
                rng=MersenneTwister(1234))::BitSet

Generates and returns a minimum spanning tree from the subgraph induced
by `edges` and `nodes`, using Kruskal's MST algorithm. The `edges` are weighted
by `weights`.

## Note:
The `graph` represents the entire graph of the plan, where as `edges` and
`nodes` represent only the sub-graph on which we want to draw the MST.

*Arguments:*
- graph: Underlying Graph object
- edges: Array of edges of the sub-graph
- nodes: Set of nodes of the sub-graph
- weights: Array of weights of `length(edges)` where `weights[i]` is the
           weight of `edges[i]`

*Returns* a BitSet of edges that form a mst.
"""
function kruskal_mst(graph::BaseGraph,
                     edges::Array{Int, 1},
                     nodes::Array{Int, 1},
                     weights::Array{Float64, 1},
                     rng=MersenneTwister(1234))::Dict{Int, Array{Int, 1}}
    num_nodes = length(nodes)

    # sort the edges arr by their weights
    sorted_indices = sortperm(weights)
    sorted_edges = edges[sorted_indices]

    mst_edges = BitSet()
    connected_vs = DisjointSets{Int}(nodes)

    for edge in sorted_edges
        if !in_same_set(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
            union!(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
            push!(mst_edges, edge)
            (length(mst_edges) >= num_nodes - 1) && break
        end
    end
    return build_mst(graph, BitSet(nodes), mst_edges)
end

"""
    random_kruskal_mst(graph::BaseGraph,
                       edges::Array{Int, 1},
                       nodes::Array{Int, 1},
                       rng=MersenneTwister(1234))::BitSet

Generates and returns a random minimum spanning tree from the subgraph induced
by `edges` and `nodes`, using Kruskal's MST algorithm.

## Note:
The `graph` represents the entire graph of the plan, where as `edges` and
`nodes` represent only the sub-graph on which we want to draw the MST.

*Arguments:*
- graph: Underlying Graph object
- edges: Array of edges of the sub-graph
- nodes: Set of nodes of the sub-graph

*Returns* a BitSet of edges that form a mst.
"""
function random_kruskal_mst(graph::BaseGraph,
                            edges::Array{Int, 1},
                            nodes::Array{Int, 1},
                            rng=MersenneTwister(1234))::Dict{Int, Array{Int, 1}}
    weights = rand(rng, length(edges))
    return kruskal_mst(graph, edges, nodes, weights)
end
