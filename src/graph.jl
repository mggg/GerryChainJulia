abstract type AbstractGraph end

struct BaseGraph<:AbstractGraph
    num_nodes::Int
    num_edges::Int
    total_pop::Int
    populations::Array{Int, 1}             # of length(num_nodes)
    adj_matrix::SparseMatrixCSC{Int, Int}
    edge_src::Array{Int, 1}                # of length(num_edges)
    edge_dst::Array{Int, 1}                # of length(num_edges)
    neighbors::Array{Array{Int64,1},1}
    simple_graph::SimpleGraph              # the base SimpleGraph, if we need it
    attributes::Array{Dict{String, Any}}
end


function read_table(filepath::AbstractString)::Shapefile.Table
    """ Read table from shapefile. If a .shp and a .dbf file of the same name
        are not found, then we throw an error.
    """
    prefix = splitext(filepath)[1]
    if !(isfile(prefix * ".shp") || isfile(prefix * ".SHP")) || !(isfile(prefix * ".dbf") || isfile(prefix * ".DBF"))
        throw(ArgumentError("Error when processing filepath as shapefile: to read a graph from a shapefile, we require a .shp and .dbf file of the same name in the same folder."))
    end
    return Shapefile.Table(prefix)
end


function all_node_properties(table::Shapefile.Table)::Array{Dict{String, Any}}
    """ Returns an Array of Dictionaries. Each dictionary corresponds to one
        node in the graph.
    """
    properties = propertynames(table) # returns array of symbols
    string_keys = String.(properties) # convert by broadcasting

    # internal function because we want to use both properties and values
    function get_node_properties(row::Shapefile.Row)
        values = map(p -> getproperty(row, p), properties)
        return Dict(string_keys .=> values)
    end

    return get_node_properties.(table)
end


function get_attribute_by_key(node_attributes::Array,
                              column_name::String,
                              process_value::Function=identity)::Array
    """ Returns an array whose values correspond to the value of an attribute
        for each node.

        Arguments:

        node_attributes :   An array of Dict{String, Any}, where each dictionary
                            represents a mapping from attribute names to values
                            for a particular node.
        column_name     :   The name of the attribute (i.e., the key of the
                            attribute in the dictionaries)
        process_value   :   An optional argument that processes the raw value
    """
    return [process_value(n[column_name]) for n in node_attributes]
end


function population_to_int(raw_value::Number)::Int
    """ Tiny helper function to coerce population counts (whether they are
        ints or floats) to int.
    """
    return raw_value isa Int ? raw_value : convert(Int, round(raw_value))
end

function get_node_coordinates(row::Shapefile.Row)::Vector{Vector{Vector{Vector{Float64}}}}
    """ Construct an array of LibGEOS.Polygons from the given coordinates. The
        coordinates are structured in the following way. Each element in the
        outermost array represents one polygon. (One node can be made up of
        multiple polygons).
        [
            [                       # one array = one polygon
                [                   # points corresponding to outer ring
                    [1.0, 2.0],     # single x,y coordinate of a point
                    ...
                ],
                [                   # points corresponding to a hole in polygon
                    [1.5, 1.7],
                    ...
                ],
                ...                 # any other subsequent arrays would
                                    # correspond to other holes
            ],
            ...                     #  subsequent arrays correspond to other
                                    #  polygons
        ]
    """
    return LibGEOS.GeoInterface.coordinates(getfield(row, :geometry))
end


function graph_from_shp(filepath::AbstractString,
                        pop_col::AbstractString,
                        adjacency::String="rook")::BaseGraph
    """ Constructs BaseGraph from .shp file.
    """
    table = read_table(filepath)

    attributes = all_node_properties(table)
    coords = get_node_coordinates.(table)
    # these will be used in the adjacency method
    node_polys = polygon_array.(coords)
    node_mbrs = min_bounding_rect.(coords)

    graph = simple_graph_from_polygons(node_polys, node_mbrs, adjacency)

    # edge `i` would connect nodes edge_src[i] and edge_dst[i]
    edge_src, edge_dst = edges_from_graph(graph)
    # each entry in adj_matrix is the edge id that connects the two nodes
    adj_matrix = adjacency_matrix_from_graph(graph)
    neighbors = neighbors_from_graph(graph)

    populations =  get_attribute_by_key(attributes, pop_col, population_to_int)
    total_pop = sum(populations)

    return BaseGraph(nv(graph), ne(graph), total_pop, populations,
                     adj_matrix, edge_src, edge_dst, neighbors,
                     graph, attributes)
end


function edges_from_graph(graph::SimpleGraph)
    """ Extract edges from graph. Returns two arrays; the first contains the
        indices of the source nodes and the second contains the indices
        of the destination nodes.
    """
    num_edges = ne(graph)

    # edge `i` would connect nodes edge_src[i] and edge_dst[i]
    edge_src = zeros(Int, num_edges)
    edge_dst = zeros(Int, num_edges)

    for (index, edge) in enumerate(edges(graph))
        edge_src[index] = src(edge)
        edge_dst[index] = dst(edge)
    end
    return edge_src, edge_dst
end


function adjacency_matrix_from_graph(graph::SimpleGraph)
    """ Extract sparse adjacency matrix from graph.
    """
    # each entry in adj_matrix is the edge id that connects the two nodes.
    num_nodes = nv(graph)
    adj_matrix = spzeros(Int, num_nodes, num_nodes)
    for (index, edge) in enumerate(edges(graph))
        adj_matrix[src(edge), dst(edge)] = index
        adj_matrix[dst(edge), src(edge)] = index
    end
    return adj_matrix
end


function neighbors_from_graph(graph::SimpleGraph)
    """ Extract each node's neighbors from graph.
    """
    # each entry in adj_matrix is the edge id that connects the two nodes.
    neighbors = [ Int[] for n in 1:nv(graph) ]
    for (index, edge) in enumerate(edges(graph))
        push!(neighbors[src(edge)], dst(edge))
        push!(neighbors[dst(edge)], src(edge))
    end
    return neighbors
end


function graph_from_json(filepath::AbstractString,
                         pop_col::AbstractString)::BaseGraph
    """ Arguments:

        filepath:       file path to the .json file that contains the graph.
                        This file is expected to be generated by the `Graph.to_json()`
                        function of the Python implementation of Gerrychain. [1]
                        We assume that the JSON file has the structure of a dictionary
                        where (1) the key "nodes" yields an array of dictionaries
                        of node attributes, (2) the key "adjacency" yields an
                        array of edges (represented as dictionaries), and (3)
                        the key "id" within the edge dictionary indicates the
                        destination node of the edge.
        pop_col:        the node attribute key whose accompanying value is the
                        population of that node

    [1]: https://github.com/mggg/GerryChain/blob/c87da7e69967880abc99b781cd37468b8cb18815/gerrychain/graph/graph.py#L38
    """
    raw_graph = JSON.parsefile(filepath)
    nodes = raw_graph["nodes"]
    num_nodes = length(nodes)

    # get populations
    populations =  get_attribute_by_key(nodes, pop_col, population_to_int)
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
    edge_src, edge_dst = edges_from_graph(simple_graph)
    # each entry in adj_matrix is the edge id that connects the two nodes
    adj_matrix = adjacency_matrix_from_graph(simple_graph)
    neighbors = neighbors_from_graph(simple_graph)

    # get attributes
    attributes = get_attributes(nodes)

    return BaseGraph(num_nodes, num_edges, total_pop, populations,
                     adj_matrix, edge_src, edge_dst, neighbors,
                     simple_graph, attributes)
end


function BaseGraph(filepath::AbstractString,
                   pop_col::AbstractString;
                   adjacency::String="rook")::BaseGraph
    """ Builds the base Graph object. This is the underlying network of our
        districts, and its properties are immutable i.e they will not change
        from step to step in our Markov Chains.

        Arguments:
        filepath:       A path to a .json or .shp file which contains the
                        information needed to construct the graph.
        pop_col:        the node attribute key whose accompanying value is the
                        population of that node
        adjacency:      (Only used if the user specifies a filepath to a .shp
                        file.) Should be either "queen" or "rook"; "rook" by
                        default.
    """
    extension = uppercase(splitext(filepath)[2])
    if uppercase(extension) == ".JSON"
        return graph_from_json(filepath, pop_col)
    elseif uppercase(extension) == ".SHP"
        return graph_from_shp(filepath, pop_col, adjacency)
    else
        throw(DomainError(filepath, "Filepath must lead to valid JSON file or valid .shp/.dbf file."))
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

function induced_subgraph_edges(graph::BaseGraph, vlist::Array{Int, 1})::Vector{Int}
    """ Returns a list of edges of the subgraph induced by vlist, which is an array of vertices.
    """
    allunique(vlist) || throw(ArgumentError("Vertices in subgraph list must be unique"))
    induced_edges = Vector{Int}()

    vset = Set(vlist)
    for src in vlist
        for dst in graph.neighbors[src]
            if dst in vset
                push!(induced_edges, graph.adj_matrix[src, dst])
            end
        end
    end
    return collect(induced_edges)
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

function kruskal_mst(graph::BaseGraph,
                     edges::Array{Int, 1},
                     nodes::Array{Int, 1},
                     weights::Array{Float64, 1}) ::BitSet
    """Generates and returns a minimum spanning tree from the subgraph induced by
        `edges` and `nodes`, using Kruskal's MST algorithm.

        Note: the `graph` represents the entire graph of
        the plan, where as `edges` and `nodes` represent only the sub-graph on
        which we want to draw the MST.

        Arguments:
            graph: Underlying Graph object
            edges: Array of edges of the subgraph
            nodes: Array of nodes of the subgraph
            weights: Weights on the edges

        Returns:
            mst: A set of edges that form an MST.
    """
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


function build_mst(graph::BaseGraph, nodes::BitSet,
                   edges::BitSet)::Dict{Int, Array{Int, 1}}
    """ Builds a graph as an adjacency list from the `mst_nodes` and `mst_edges`.
    """
    mst = Dict{Int, Array{Int, 1}}()
    for node in nodes
        mst[node] = Array{Int, 1}()
    end
    for edge in edges
        add_edge_to_mst!(graph, mst, edge)
    end
    return mst
end

function random_kruskal_mst(graph::BaseGraph,
                            edges::Array{Int, 1},
                            nodes::Array{Int, 1},
                            rng=MersenneTwister(1234))::BitSet
    """Generates and returns a random minimum spanning tree from the subgraph induced
       by `edges` and `nodes`, using Kruskal's MST algorithm.

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
    weights = rand(rng, length(edges))
    return kruskal_mst(graph, edges, nodes, weights)
end

function wilson_ust(graph::BaseGraph,
                    edges::Array{Int, 1},
                    nodes::Array{Int, 1},
                    rng=MersenneTwister(1234))::BitSet
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
    #println("mst edges: ", mst_edges)
    #println("missing nodes: ", setdiff(Set(1:n), Set(next)))
    @assert length(mst_edges) == length(nodes) - 1
    return mst_edges
end


function remove_edge_from_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Removes an edge from the graph built by `build_mst`.
    """
    filter!(e -> e != graph.edge_dst[edge], mst[graph.edge_src[edge]])
    filter!(e -> e != graph.edge_src[edge], mst[graph.edge_dst[edge]])
end


function add_edge_to_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Adds an edge to the graph built by `build_mst`.
    """
    push!(mst[graph.edge_src[edge]], graph.edge_dst[edge])
    push!(mst[graph.edge_dst[edge]], graph.edge_src[edge])
end

function traverse_mst(mst::Dict{Int, Array{Int, 1}},
                      start_node::Int,
                      avoid_node::Int,
                      stack::Stack{Int},
                      traversed_nodes::BitSet)::BitSet
    """ Returns the component of the MST `mst` that contains the vertex
        `start_node`.

        Arguments:
            mst:        mst to traverse
            start_node: the node to start traversing from
            avoid_node: the node to avoid adn which seperates the mst into
                        two components
            stack:      an empty Stack
            traversed_nodes: an empty BitSet that is to be populated.

        `stack` and `traversed_nodes` are are pre-allocated and passed in to
        reduce the number of memory allocations and consequently, time taken.
        In the course of calling this function multiple times, it is intended that
        we pass in the same (empty) objects repeatedly.
    """
    @assert isempty(stack)
    empty!(traversed_nodes)

    push!(stack, start_node)

    while !isempty(stack)
        new_node = pop!(stack)
        push!(traversed_nodes, new_node)

        for neighbor in mst[new_node]
            if !(neighbor in traversed_nodes) && neighbor != avoid_node
                push!(stack, neighbor)
            end
        end
    end
    return traversed_nodes
end