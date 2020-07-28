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


function polygon_array(coords::Vector{Vector{Vector{Vector{Float64}}}})::Array{LibGEOS.Polygon}
    """ Returns an array of Polygons for every entry in the `coords` array.
    """
    return LibGEOS.Polygon.(coords)
end


function min_bounding_rect(coords::Vector{Vector{Vector{Vector{Float64}}}})::Tuple
    """ Takes a complex vector object and returns an (axis-aligned) minimum
        bounding rectangle. A rectangle is represented by
        ([lower_left_x, lower_left_y], [upper_right_x, upper_right_y]).
    """
    if isempty(coords)
        throw(ArgumentError("Cannot create minimum bounding rectangle for empty coordinate array"))
    end
    xmin, ymin , xmax, ymax = Inf, Inf, -Inf, -Inf
    for c in coords # for each polygon in the array
      points = reduce(hcat, c[1]) # we only care about the exterior ring, not holes
      xmin = minimum([xmin, minimum(points[1, :])])
      ymin = minimum([ymin, minimum(points[2, :])])
      xmax = maximum([xmax, maximum(points[1, :])])
      ymax = maximum([ymax, maximum(points[2, :])])
    end
    return ([xmin, ymin], [xmax, ymax])
end


function create_rtree(minimum_bounding_rects::Vector{Tuple{Vector{Float64},Vector{Float64}}})::LibSpatialIndex.RTree
    """ Given an array of minimum bounding rectangles, constructs an R-Tree
        that will make it easier to identify candidates for intersecting nodes.
        We expect that minimum bounding rectangles come in this form:
        [
            (
                [lower_left_x, lower_left_y],
                [upper_right_x, upper_right_y]
            ),
            ...
        ]
    """
    rtree = LibSpatialIndex.RTree(2)
    # insert an MBR for each polygon in the RTree
    for (i, mbr) in enumerate(minimum_bounding_rects)
        # mbr[1] is the coordinate of the lower left corner,
        # mbr[2] is the coordinate of the upper right corner
        LibSpatialIndex.insert!(rtree, i, mbr[1], mbr[2])
    end
    return rtree
end


function queen_intersection(intersection)::Bool
  """ Returns true if the LibGEOS.GEOSGeom is non-empty.
  """
  return !LibGEOS.isEmpty(intersection)
end


function rook_intersection(intersection)::Bool
  """ Returns true if the LibGEOS.GEOSGeom is non-empty and there exists
      a shared perimeter in the intersection (i.e., the intersection is not
      just a point or a set of points.)
  """
  return queen_intersection(intersection) && !(intersection isa LibGEOS.Point || intersection isa LibGEOS.MultiPoint)
end


function adjacent(node₁_polygons::Array{LibGEOS.Polygon, 1},
                  node₂_polygons::Array{LibGEOS.Polygon, 1},
                  adjacency_fn::Function)::Bool
    """ Returns true if there exists an adjacency between any of the polygons
        in node 1 and any of the polygons in node 2. The user provides
        an adjacency_fn (such as queen_intersection or rook_intersection) to
        evaluate the output of the LibGEOS.intersection() function.
    """
    for p₁ in node₁_polygons
        for p₂ in node₂_polygons
            intersection = LibGEOS.intersection(p₁, p₂)
            if adjacency_fn(intersection)
                return true
            end
        end
    end
    return false
end


function simple_graph_from_polygons(polygons::Array{Array{LibGEOS.Polygon, 1}, 1},
                                    minimum_bounding_rects::Array{Tuple{Array{Float64,1},Array{Float64,1}},1},
                                    adjacency::String="rook")::SimpleGraph
    """ Constructs a simple graph from polygons and the associated minimum
        bounding rectangles.

        Arguments:

        polygons                : Each entry in the array represents one node,
                                  which is itself represented as an Array of
                                  LibGEOS.Polygon objects
        minimum_bounding_rects  : An Array of Tuples, where each tuple of
                                  the array has the coordinates of the lower left
                                  corner of the minimum bounding rectangle and
                                  the coordinates of the upper left corner. The
                                  indices of this array should match up with
                                  the indices of the polys array.
        adjacency               : A String that is either "queen" or "rook"
                                  representing the style of adjacency the user
                                  prefers.
    """
    if !(adjacency in ["queen", "rook"])
        throw(ArgumentError("Adjacency must be either \"queen\" or \"rook\"."))
    elseif length(polygons) != length(minimum_bounding_rects)
        throw(ArgumentError("Length of polygons array must match length of minimum bounding rectangles array."))
    end
    adjacency_fn = adjacency == "queen" ? queen_intersection : rook_intersection
    graph = SimpleGraph(length(polygons))
    rtree = create_rtree(minimum_bounding_rects)

    for (i, p) in enumerate(polygons)
        # get all candidate nodes
        lower_left = minimum_bounding_rects[i][1]
        upper_right = minimum_bounding_rects[i][2]
        candidate_idxs = LibSpatialIndex.intersects(rtree, lower_left, upper_right)
        for c_idx in candidate_idxs
            # no self loops
            if c_idx != i && !has_edge(graph, i, c_idx) && adjacent(p, polygons[c_idx], adjacency_fn)
                add_edge!(graph, i, c_idx)
            end
        end
    end
    return graph
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
                        assignment_col::AbstractString,
                        adjacency::String="rook")::BaseGraph
    """ Constructs BaseGraph from .shp file.
    """
    table = read_table(filepath)

    attributes = all_node_properties(table)
    coords = get_node_coordinates.(table)
    # these will be used in the adjacency method
    node_polys = polygon_array.(coords)
    node_mbrs = min_bounding_rect.(coords)

    graph = simple_graph_from_shapes(node_polys, node_mbrs, adjacency)

    # edge `i` would connect nodes edge_src[i] and edge_dst[i]
    edge_src, edge_dst = edges_from_graph(graph)
    # each entry in adj_matrix is the edge id that connects the two nodes
    adj_matrix = adjacency_matrix_from_graph(graph)
    neighbors = neighbors_from_graph(graph)

    populations, assignments = get_populations_and_assignments(attributes, pop_col, assignment_col)
    num_districts = length(Set(assignments))
    total_pop = sum(populations)

    return BaseGraph(nv(graph), ne(graph), num_districts, total_pop,
                     populations, adj_matrix, edge_src, edge_dst, neighbors,
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
                        assignment of that node (i.e., to a district)

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
    edge_src, edge_dst = edges_from_graph(simple_graph)
    # each entry in adj_matrix is the edge id that connects the two nodes
    adj_matrix = adjacency_matrix_from_graph(simple_graph)
    neighbors = neighbors_from_graph(simple_graph)

    # get attributes
    attributes = get_attributes(nodes)

    return BaseGraph(num_nodes, num_edges, num_districts, total_pop,
                     populations, adj_matrix, edge_src, edge_dst, neighbors,
                     simple_graph, attributes)
end


function BaseGraph(filepath::AbstractString,
                   pop_col::AbstractString,
                   assignment_col::AbstractString;
                   adjacency::String="rook")::BaseGraph
    """ Builds the base Graph object. This is the underlying network of our
        districts, and its properties are immutable i.e they will not change
        from step to step in our Markov Chains.

        Arguments:
        filepath:       A path to a .json or .shp file which contains the
                        information needed to construct the graph.
        pop_col:        the node attribute key whose accompanying value is the
                        population of that node
        assignment_col: the node attribute key whose accompanying value is the
                        assignment of that node (i.e., to a district)
        adjacency:      (Only used if the user specifies a filepath to a .shp
                        file.) Should be either "queen" or "rook"; "rook" by
                        default.
    """
    extension = splitext(filepath)[2]
    try
        try
            return graph_from_json(filepath, pop_col, assignment_col)
        catch e
            return graph_from_shp(filepath, pop_col, assignment_col, adjacency)
        end
    catch
        throw(ArgumentError("Shapefile could not be processed. Please ensure the file is a valid JSON or .shp/.dbf."))
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
