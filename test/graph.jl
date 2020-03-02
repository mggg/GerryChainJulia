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
    # raw_graph = JSON.parsefile(filepath)
    graph = BaseGraph(filepath, "population", "assignment")

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
    @test nv(graph.simple_graph) == graph.num_nodes
    @test ne(graph.simple_graph) == graph.num_edges

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
end
