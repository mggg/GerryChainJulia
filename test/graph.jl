using LightGraphs
using Random
using DataStructures

"""
    the test graph being loaded is labeled as

    01 - 02 - 03 - 04
     |    |   |    |
    05 - 06 - 07 - 08
     |    |   |    |
    09 - 10 - 11 - 12
     |    |   |    |
    13 - 14 - 15 - 16

    the initial assignment is

    01 - 01 - 02 - 02
     |    |   |    |
    01 - 01 - 02 - 02
     |    |   |    |
    03 - 03 - 04 - 04
     |    |   |    |
    03 - 03 - 04 - 04

    the population distribution is

    20 - 10 - 10 - 10
     |    |   |    |
    01 - 10 - 20 - 01
     |    |   |    |
    20 - 10 - 10 - 10
     |    |   |    |
    01 - 10 - 01 - 20
"""

@testset "Graph tests" begin
    @testset "Bad extension" begin
        # file should have a .json or .shp extension
        @test_throws ArgumentError BaseGraph("nonexistent.txt", "population", "assignment")
    end

    @testset "Reading node attributes from shapefile" begin
        # file should have a .json or .shp extension
        table = GerryChain.read_table(square_shp_filepath)
        node_attributes = GerryChain.all_node_properties(table)
        correct_attributes = [ # refer to maps/make_simple_shp.py
            Dict("assignment" => 1, "population" => 2),
            Dict("assignment" => 2, "population" => 4),
            Dict("assignment" => 3, "population" => 6),
            Dict("assignment" => 4, "population" => 8),
        ]
        @test node_attributes == correct_attributes
    end

    @testset "Extracting coordinates and minimum bounding rectangles" begin
        # file should have a .json or .shp extension
        table = GerryChain.read_table(square_shp_filepath)
        correct_coords = [
            [ [ [ [0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0], [0.0, 0.0] ] ] ],
            [ [ [ [0.0, 1.0], [0.0, 2.0], [1.0, 2.0], [1.0, 1.0], [0.0, 1.0] ] ] ],
            [ [ [ [1.0, 0.0], [1.0, 1.0], [2.0, 1.0], [2.0, 0.0], [1.0, 0.0] ] ] ],
            [ [ [ [1.0, 1.0], [1.0, 2.0], [2.0, 2.0], [2.0, 1.0], [1.0, 1.0] ] ] ]
        ]
        # extract coordinates
        coords = GerryChain.get_node_coordinates.(table)
        @test correct_coords == coords

        # create minimum bounding rectangles
        correct_mbrs = [
            ([0.0, 0.0], [1.0, 1.0]),
            ([0.0, 1.0], [1.0, 2.0]),
            ([1.0, 0.0], [2.0, 1.0]),
            ([1.0, 1.0], [2.0, 2.0])
        ]
        node_mbrs = GerryChain.min_bounding_rect.(coords)
        @test correct_mbrs == node_mbrs
    end

    graph = BaseGraph(square_grid_filepath, "population", "assignment")

    @test graph.num_nodes == 16
    @test graph.num_edges == 24
    @test graph.total_pop == 164
    @test graph.num_dists == 4

    @testset "Populations" begin
        for i in [1, 7, 9, 16]
            @test graph.populations[i] == 20
        end
        for i in [5, 8, 13, 15]
            @test graph.populations[i] == 1
        end
        for i in [2, 3, 4, 6, 10, 11, 12, 14]
            @test graph.populations[i] == 10
        end
    end

    # test adjacencies
    @test graph.adj_matrix[1,2] != 0
    @test graph.adj_matrix[1,5] != 0
    @test graph.adj_matrix[1,6] == 0

    @testset "Graph Adjacency Symmetry" begin
        for i in 1:graph.num_nodes
            for j in 1:graph.num_nodes
                @test graph.adj_matrix[i,j] == graph.adj_matrix[j,i]
            end
        end
    end

    # test the edge arrays
    @test graph.edge_src[graph.adj_matrix[10,11]] in (10,11)
    @test graph.edge_dst[graph.adj_matrix[11,10]] in (10,11)
    @test graph.edge_src[graph.adj_matrix[9,13]] in (9,13)
    @test graph.edge_dst[graph.adj_matrix[13,9]] in (13,9)
    @test length(graph.edge_src) == graph.num_edges
    @test length(graph.edge_dst) == graph.num_edges

    # test the node neighbors
    @test sort(graph.neighbors[1]) == [2,5]
    @test sort(graph.neighbors[6]) == [2,5, 7, 10]
    @test sort(graph.neighbors[14]) == [10, 13, 15]

    # test the simple graph
    @test LightGraphs.nv(graph.simple_graph) == graph.num_nodes
    @test LightGraphs.ne(graph.simple_graph) == graph.num_edges

    # test induced_subgraph
    @test begin
        induced_edges = induced_subgraph_edges(graph, [1, 2, 3, 4])
        induced_vertices = Set{Int}()
        for edge in induced_edges
            push!(induced_vertices, graph.edge_src[edge], graph.edge_dst[edge])
        end
        induced_vertices == Set{Int}([1, 2, 3, 4])
    end
    @test_throws ArgumentError induced_subgraph_edges(graph, [1, 1, 4])

    # get_subgraph_population()
    @test get_subgraph_population(graph, BitSet([1, 2, 3, 4])) == 50
    @test get_subgraph_population(graph, BitSet([5])) == 1
    @test get_subgraph_population(graph, BitSet([1, 6, 11, 16])) == 60

    # test random_weighted_kruskal_mst
    @testset "Kruskal MST" begin
        rng = MersenneTwister(1234)
        nodes = [1, 2, 3, 4, 5, 6, 7, 8]
        edges = [graph.adj_matrix[1,2], graph.adj_matrix[2,3], graph.adj_matrix[3,4],
                 graph.adj_matrix[5,6], graph.adj_matrix[6,7], graph.adj_matrix[7,8],
                 graph.adj_matrix[1,5], graph.adj_matrix[2,6], graph.adj_matrix[3,7],
                 graph.adj_matrix[4,8]]
        weights = rand(rng, length(edges))
        mst = weighted_kruskal_mst(graph, edges, nodes, weights, rng)
        @test length(mst) == length(nodes) - 1
        @test begin # are there loops in the tree?
            # find by union-find algorithm
            connected_vs = DisjointSets{Int}(nodes)
            cycle_found = false
            for edge in mst
                if in_same_set(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
                    cycle_found = true
                    break
                else
                    union!(connected_vs, graph.edge_src[edge], graph.edge_dst[edge])
                end
            end
            !cycle_found
        end
    end

    # test that attributes can be accessed
    @test graph.attributes[1]["purple"] == 15
    @test graph.attributes[1]["pink"] == 5
end
