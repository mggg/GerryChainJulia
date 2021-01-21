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
        @test_throws DomainError BaseGraph("nonexistent.txt", "population")
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

    @testset "BaseGraph from shp() - rook adjacency" begin
        graph = BaseGraph(square_shp_filepath, "population")
        @test graph.num_nodes == 4
        @test graph.num_edges == 4
        @test graph.total_pop == 20
        @test graph.populations == [2, 4, 6, 8]
        @test graph.adj_matrix[1, 2] != 0
        @test graph.adj_matrix[1, 3] != 0
        # upper left corner and bottom right corner should be non-adjacent
        @test graph.adj_matrix[1, 4] == 0
    end

    @testset "BaseGraph from shp() - queen adjacency" begin
        graph = BaseGraph(square_shp_filepath, "population", adjacency = "queen")
        @test graph.num_nodes == 4
        @test graph.num_edges == 6 # queen adjacency means all 6 edges
        @test graph.total_pop == 20

        @test graph.populations == [2, 4, 6, 8]
        # with queen adjacency, all squares should be adjacent to each other
        @test graph.adj_matrix[1, 2] != 0
        @test graph.adj_matrix[1, 3] != 0
        @test graph.adj_matrix[1, 4] != 0
        @test graph.adj_matrix[2, 3] != 0
        @test graph.adj_matrix[2, 4] != 0
        @test graph.adj_matrix[3, 4] != 0
    end

    graph = BaseGraph(square_grid_filepath, "population")

    @test graph.num_nodes == 16
    @test graph.num_edges == 24
    @test graph.total_pop == 164

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
    @test graph.adj_matrix[1, 2] != 0
    @test graph.adj_matrix[1, 5] != 0
    @test graph.adj_matrix[1, 6] == 0

    @testset "Graph Adjacency Symmetry" begin
        for i = 1:graph.num_nodes
            for j = 1:graph.num_nodes
                @test graph.adj_matrix[i, j] == graph.adj_matrix[j, i]
            end
        end
    end

    @testset "Check type of district assignments - get_assignments()" begin
        partition = Partition(graph, "assignment")
        # assignment that is a Float should throw an error
        foreach(d -> d["assignment"] *= 1.0, graph.attributes) # convert Int to Float
        @test_throws DomainError GerryChain.get_assignments(graph.attributes, "assignment")
    end

    # test the edge arrays
    @test graph.edge_src[graph.adj_matrix[10, 11]] in (10, 11)
    @test graph.edge_dst[graph.adj_matrix[11, 10]] in (10, 11)
    @test graph.edge_src[graph.adj_matrix[9, 13]] in (9, 13)
    @test graph.edge_dst[graph.adj_matrix[13, 9]] in (13, 9)
    @test length(graph.edge_src) == graph.num_edges
    @test length(graph.edge_dst) == graph.num_edges

    # test the node neighbors
    @test sort(graph.neighbors[1]) == [2, 5]
    @test sort(graph.neighbors[6]) == [2, 5, 7, 10]
    @test sort(graph.neighbors[14]) == [10, 13, 15]

    # test the simple graph
    @test LightGraphs.nv(graph.simple_graph) == graph.num_nodes
    @test LightGraphs.ne(graph.simple_graph) == graph.num_edges

    # test induced_subgraph
    @testset "induced_subgraph_edges()" begin
        induced_edges = induced_subgraph_edges(graph, [1, 2, 3, 4])
        @test sort(induced_edges) == sort([1, 3, 5])

        induced_vertices = Set{Int}()
        for edge in induced_edges
            push!(induced_vertices, graph.edge_src[edge], graph.edge_dst[edge])
        end
        @test induced_vertices == Set{Int}([1, 2, 3, 4])
    end
    @test_throws ArgumentError induced_subgraph_edges(graph, [1, 1, 4])

    # get_subgraph_population()
    @test get_subgraph_population(graph, BitSet([1, 2, 3, 4])) == 50
    @test get_subgraph_population(graph, BitSet([5])) == 1
    @test get_subgraph_population(graph, BitSet([1, 6, 11, 16])) == 60

    # test that attributes can be accessed
    @test graph.attributes[1]["purple"] == 15
    @test graph.attributes[1]["pink"] == 5
end
