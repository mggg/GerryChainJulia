

"""
    kruskal_mst(graph::BaseGraph,
                edges::Array{Int, 1},
                nodes::Array{Int, 1},
                weights::Array{Float64, 1})::BitSet

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

*Returns* the MST represented in adjacency list format.
"""
function kruskal_mst(
    graph::BaseGraph,
    edges::Array{Int,1},
    nodes::Array{Int,1},
    weights::Array{Float64,1},
)::Dict{Int, Array{Int, 1}}
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
    return build_mst(graph, nodes, mst_edges)
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

*Returns* the MST represented in adjacency list format.
"""
function random_kruskal_mst(
    graph::BaseGraph,
    edges::Array{Int,1},
    nodes::Array{Int,1},
    rng = MersenneTwister(1234),
)::Dict{Int, Array{Int, 1}}
    weights = rand(rng, length(edges))
    return kruskal_mst(graph, edges, nodes, weights)
end

function wilson_ust(graph::BaseGraph,
                    edges::Array{Int, 1},
                    nodes::Array{Int, 1},
                    rng=MersenneTwister(1234))::Dict{Int, Array{Int, 1}}
    """ Generates and returns a random uniform spanning tree from the subgraph induced
        by `edges` and `nodes` using Wilson's algorithm.

        This implementation is copied nearly verbatim from pseudocode in David B. Wilson's
        1996 paper "Generating Random Spanning Trees More Quickly than the Cover Time"
        (Figure 1).

        Note: the `graph` represents the entire graph of
        the plan, where as `edges` and `nodes` represent only the sub-graph on
        which we want to draw the MST.


        Arguments:
            graph: Underlying Graph object
            edges: Array of edges of the subgraph
            nodes: Array of nodes of the subgraph
            rng: Random number generator used to generate edge weights

        Returns:
            mst: A set of edges that form an MST.
    """
    node_to_idx = Dict(node => idx for (idx, node) in enumerate(nodes))
    n = length(nodes)
    root = rand(1:n)
    in_tree = zeros(Bool, n)
    next = -ones(Int, n)
    in_tree[root] = true
    for i in 1:n
        u = i
        while !in_tree[u]
            neighbors = [n for n in graph.neighbors[nodes[u]]
                         if n in keys(node_to_idx)]
            next_node = rand(rng, neighbors)
            next[u] = node_to_idx[next_node]
            u = next[u]
        end
        u = i
        while !in_tree[u]
            in_tree[u] = true
            u = next[u]
        end
    end

    mst_edges = BitSet([])
    idx_pair_to_edge = Dict(tuple(sort([node_to_idx[graph.edge_dst[e]],
                                        node_to_idx[graph.edge_src[e]]])) => e
                            for e in edges)
    for (curr_idx, next_idx) in enumerate(next)
        if next_idx != -1
            edge_pair = tuple(sort([curr_idx, next_idx]))
            push!(mst_edges, idx_pair_to_edge[edge_pair])
        end
    end
    @assert length(mst_edges) == length(nodes) - 1
    return build_mst(graph, nodes, mst_edges)
end

function add_edge_to_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Adds an edge to the graph built by `build_mst`.
    """
    push!(mst[graph.edge_src[edge]], graph.edge_dst[edge])
    push!(mst[graph.edge_dst[edge]], graph.edge_src[edge])
end