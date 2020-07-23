abstract type AbstractGraph end

struct BaseGraph<:AbstractGraph
    num_nodes::Int
    num_edges::Int
    num_dists::Int
    total_pop::Int
    populations::Array{Int, 1}             # of length(num_nodes)
    adj_matrix::SparseMatrixCSC{Int, Int}
    edge_src::Array{Int, 1}                # of length(num_edges)
    edge_dst::Array{Int, 1}                # of length(num_edges)
    neighbors::Array{Array{Int64,1},1}
    simple_graph::SimpleGraph              # the base SimpleGraph, if we need it
    attributes::Array{Dict{String, Any}}
end


function graph_from_shp(filepath::AbstractString,
                        pop_col::AbstractString,
                        assignment_col::AbstractString)::BaseGraph
    """ Constructs BaseGraph from .shp file.
    """
    # TODO(matthew): implement
    return graph_from_json(filepath, pop_col, assignment_col)
end


function graph_from_json(filepath::AbstractString,
                         pop_col::AbstractString,
                         assignment_col::AbstractString)::BaseGraph
    """ Arguments:

        filepath:       file path to the .json file that contains the graph.
                        This file is expected to be generated by the `Graph.to_json()`
                        'function of the Python implementation of Gerrychain. [1]
                        We assume that the JSON file has the structure of a dictionary
                        where (1) the key "nodes" yields an array of dictionaries
                        of node attributes, (2) the key "adjacency" yields an
                        array of edges (represented as dictionaries), and (3)
                        the key "id" within the edge dictionary indicates the
                        destination node of the edge.
        pop_col:        the node attribute key whose accompanying value is the
                        population of that node
        assignment_col: the node attribute key whose accompanying value is the
                        population of that node

    [1]: https://github.com/mggg/GerryChain/blob/c87da7e69967880abc99b781cd37468b8cb18815/gerrychain/graph/graph.py#L38
    """
    raw_graph = JSON.parsefile(filepath)
    nodes = raw_graph["nodes"]
    num_nodes = length(nodes)

    # get populations, assignments and num_districts
    populations, assignments = get_populations_and_assignments(nodes, pop_col, assignment_col)
    num_districts = length(Set(assignments))
    total_pop = sum(populations)

    # Generate the base SimpleGraph.
    simple_graph = SimpleGraph(num_nodes)
    for (index, edges) in enumerate(raw_graph["adjacency"])
        for edge in edges
            if edge["id"] + 1 > index
                add_edge!(simple_graph, index, edge["id"] + 1)
            end
        end
    end

    num_edges = ne(simple_graph)

    # edge `i` would connect nodes edge_src[i] and edge_dst[i]
    edge_src = zeros(Int, num_edges)
    edge_dst = zeros(Int, num_edges)
    neighbors = [Int[] for i in 1:num_nodes, j=1]

    # each entry in adj_matrix is the edge id that connects the two nodes.
    adj_matrix = spzeros(Int, num_nodes, num_nodes)
    for (index, edge) in enumerate(edges(simple_graph))
        adj_matrix[src(edge), dst(edge)] = index
        adj_matrix[dst(edge), src(edge)] = index

        edge_src[index] = src(edge)
        edge_dst[index] = dst(edge)

        push!(neighbors[src(edge)], dst(edge))
        push!(neighbors[dst(edge)], src(edge))
    end

    # get attributes
    attributes = get_attributes(nodes)

    return BaseGraph(num_nodes, num_edges, num_districts, total_pop,
                     populations, adj_matrix, edge_src, edge_dst, neighbors,
                     simple_graph, attributes)
end


function BaseGraph(filepath::AbstractString,
                   pop_col::AbstractString,
                   assignment_col::AbstractString)::BaseGraph
    """ Builds the base Graph object. This is the underlying network of our
        districts, and its properties are immutable i.e they will not change
        from step to step in our Markov Chains.

        Arguments:
        filepath:       A path to a .json or .shp file which contains the
                        information needed to construct the graph.
        pop_col:        the node attribute key whose accompanying value is the
                        population of that node
        assignment_col: the node attribute key whose accompanying value is the
                        population of that node
    """
    extension = splitext(filepath)[2]
    if extension == ".json"
        return graph_from_json(filepath, pop_col, assignment_col)
    elseif extension == ".shp"
        return graph_from_shp(filepath, pop_col, assignment_col)
    else
        throw(ArgumentError("Filename must be a path to a .json or a .shp file."))
    end
end


function get_attributes(nodes::Array{Any, 1})
    """ Returns an array of dicts where the attributes of the i'th node is
        at attributes[i] as a dictionary.
    """
    attributes = Array{Dict{String, Any}}(undef, length(nodes))
    for (index, node) in enumerate(nodes)
        attributes[index] = node
    end
    return attributes
end

function induced_subgraph_edges(graph::BaseGraph, vlist::Array{Int, 1})::Array{Int, 1}
    """ Returns a list of edges of the subgraph induced by vlist, which is an array of vertices.
    """
    allunique(vlist) || throw(ArgumentError("Vertices in subgraph list must be unique"))
    induced_edges = Array{Int, 1}()

    vset = Set(vlist)
    for src in vlist
        for dst in graph.neighbors[src]
            if dst in vset
                push!(induced_edges, graph.adj_matrix[src, dst])
            end
        end
    end
    return induced_edges
end

function get_subgraph_population(graph::BaseGraph, nodes::BitSet)::Int
    """ Returns the population of the subgraph induced by `nodes`.
    """
    total_pop = 0
    for node in nodes
        total_pop += graph.populations[node]
    end
    return total_pop
end

function weighted_kruskal_mst(graph::BaseGraph,
                              edges::Array{Int, 1},
                              nodes::Array{Int, 1},
                              weights::Array{Float64, 1},
                              rng=MersenneTwister(1234))::BitSet
    """ Generates and returns a minimum spanning tree from the subgraph induced by
        `edges` and `nodes`, using Kruskal's MST algorithm.

        Note: the `graph` represents the entire graph of
        the plan, where as `edges` and `nodes` represent only the sub-graph on
        which we want to draw the MST.

        Arguments:
            graph: Underlying Graph object
            edges: Arr of edges of the sub-graph
            nodes: Set of nodes of the sub-graph

        Returns:
            mst: Array{Int, 1} of edges that form a mst
    """
    num_nodes = length(nodes)

    # sort the edges arr by their weights
    sorted_indices = sortperm(weights)
    sorted_edges = edges[sorted_indices]

    # mst = Array{Int, 1}()
    mst = BitSet()
    connected_vs = DisjointSets{Int}(nodes)

    for edge in sorted_edges
        if !in_same_set(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
            union!(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
            push!(mst, edge)
            (length(mst) >= num_nodes - 1) && break
        end
    end
    return mst
end
