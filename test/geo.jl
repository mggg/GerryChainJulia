@testset "Extracting coordinates and minimum bounding rectangles" begin
    # file should have a .json or .shp extension
    table = GerryChain.read_table(square_shp_filepath)
    correct_coords = [
        [[[[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0], [0.0, 0.0]]]],
        [[[[0.0, 1.0], [0.0, 2.0], [1.0, 2.0], [1.0, 1.0], [0.0, 1.0]]]],
        [[[[1.0, 0.0], [1.0, 1.0], [2.0, 1.0], [2.0, 0.0], [1.0, 0.0]]]],
        [[[[1.0, 1.0], [1.0, 2.0], [2.0, 2.0], [2.0, 1.0], [1.0, 1.0]]]],
    ]
    # extract coordinates
    coords = GerryChain.get_node_coordinates.(table)
    @test correct_coords == coords

    # create minimum bounding rectangles
    correct_mbrs = [
        ([0.0, 0.0], [1.0, 1.0]),
        ([0.0, 1.0], [1.0, 2.0]),
        ([1.0, 0.0], [2.0, 1.0]),
        ([1.0, 1.0], [2.0, 2.0]),
    ]
    node_mbrs = GerryChain.min_bounding_rect.(coords)
    @test correct_mbrs == node_mbrs
    # test edge case
    empty_coords = Vector{Vector{Vector{Vector{Float64}}}}([])
    @test_throws ArgumentError GerryChain.min_bounding_rect.([empty_coords])
end

@testset "Queen and rook intersection" begin
    coords₁ = [[[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0], [0.0, 0.0]]]
    coords₂ = [[[1.0, 0.0], [1.0, 1.0], [2.0, 1.0], [2.0, 0.0], [1.0, 0.0]]]
    coords₃ = [[[1.0, 1.0], [1.0, 2.0], [2.0, 2.0], [2.0, 1.0], [1.0, 1.0]]]

    p₁ = LibGEOS.Polygon(coords₁)
    p₂ = LibGEOS.Polygon(coords₂)
    p₃ = LibGEOS.Polygon(coords₃)

    # p₁ and p₂ share a border, while p₁ and p₃ only share a single point
    @test GerryChain.queen_intersection(LibGEOS.intersection(p₁, p₂))
    @test GerryChain.queen_intersection(LibGEOS.intersection(p₁, p₃))

    @test GerryChain.rook_intersection(LibGEOS.intersection(p₁, p₂))
    @test !GerryChain.rook_intersection(LibGEOS.intersection(p₁, p₃))
end

@testset "R-tree tests" begin
    mbrs = [([0.0, 0.0], [1.0, 1.0]), ([0.5, 0.5], [1.5, 1.5]), ([2.0, 2.0], [3.0, 3.0])]
    rtree = GerryChain.create_rtree(mbrs)
    # first square should intersect with itself and second, but not third
    candidates = LibSpatialIndex.intersects(rtree, mbrs[1][1], mbrs[1][2])
    @test candidates == [1, 2]
end

@testset "Test for adjacency between arrays of polygons" begin
    coords = [
        [[[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0], [0.0, 0.0]]],
        [[[1.0, 0.0], [1.0, 1.0], [2.0, 1.0], [2.0, 0.0], [1.0, 0.0]]],
        [[[1.0, 1.0], [1.0, 2.0], [2.0, 2.0], [2.0, 1.0], [1.0, 1.0]]],
        [[[3.0, 3.0], [3.0, 4.0], [4.0, 4.0], [4.0, 3.0], [3.0, 3.0]]],
    ]

    polys = LibGEOS.Polygon.(coords)
    # polygon 1 shares a border with polygon 2 and an edge with polygon 3
    # polygon 2 and polygon 3 share a border
    # polygon 4 does not touch any other polygons

    @test GerryChain.adjacent([polys[1]], polys[3:4], GerryChain.queen_intersection)
    @test !GerryChain.adjacent([polys[1]], polys[3:4], GerryChain.rook_intersection)
    @test GerryChain.adjacent(polys[1:2], polys[3:4], GerryChain.queen_intersection)
    @test GerryChain.adjacent(polys[1:2], polys[3:4], GerryChain.rook_intersection)
    @test !GerryChain.adjacent(polys[1:3], [polys[4]], GerryChain.queen_intersection)
end

@testset "simple_graph_from_polygons()" begin
    table = GerryChain.read_table(square_shp_filepath)
    coords = GerryChain.get_node_coordinates.(table)
    node_polys = GerryChain.polygon_array.(coords)
    node_mbrs = GerryChain.min_bounding_rect.(coords)

    simple_graph = GerryChain.simple_graph_from_polygons(node_polys, node_mbrs)
    # there should be edges between 1 & 2, 1 & 3, 2 & 4, 3 & 4

    @test has_edge(simple_graph, 1, 2)
    @test has_edge(simple_graph, 1, 3)
    @test has_edge(simple_graph, 2, 4)
    @test !has_edge(simple_graph, 1, 4)
    @test !has_edge(simple_graph, 2, 3)
end
