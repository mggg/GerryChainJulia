function polygon_array(
    coords::Vector{Vector{Vector{Vector{Float64}}}},
)::Array{LibGEOS.Polygon}
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
        throw(
            ArgumentError(
                "Cannot create minimum bounding rectangle for empty coordinate array",
            ),
        )
    end
    xmin, ymin, xmax, ymax = Inf, Inf, -Inf, -Inf
    for c in coords # for each polygon in the array
        points = reduce(hcat, c[1]) # we only care about the exterior ring, not holes
        xmin = minimum([xmin, minimum(points[1, :])])
        ymin = minimum([ymin, minimum(points[2, :])])
        xmax = maximum([xmax, maximum(points[1, :])])
        ymax = maximum([ymax, maximum(points[2, :])])
    end
    return ([xmin, ymin], [xmax, ymax])
end


function create_rtree(
    minimum_bounding_rects::Vector{Tuple{Vector{Float64},Vector{Float64}}},
)::LibSpatialIndex.RTree
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
    return queen_intersection(intersection) &&
           !(intersection isa LibGEOS.Point || intersection isa LibGEOS.MultiPoint)
end


function adjacent(
    node₁_polygons::Array{LibGEOS.Polygon,1},
    node₂_polygons::Array{LibGEOS.Polygon,1},
    adjacency_fn::Function,
)::Bool
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


function simple_graph_from_polygons(
    polygons::Array{Array{LibGEOS.Polygon,1},1},
    minimum_bounding_rects::Array{Tuple{Array{Float64,1},Array{Float64,1}},1},
    adjacency::String = "rook",
)::SimpleGraph
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
        throw(
            ArgumentError(
                "Length of polygons array must match length of minimum bounding rectangles array.",
            ),
        )
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
            if c_idx != i &&
               !has_edge(graph, i, c_idx) &&
               adjacent(p, polygons[c_idx], adjacency_fn)
                add_edge!(graph, i, c_idx)
            end
        end
    end
    return graph
end
