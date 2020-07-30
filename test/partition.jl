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

# partition tests
@testset "Partition tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(graph, "assignment")

    @test partition.num_cut_edges == 8
    @test partition.dist_populations[1] == 41
    @test partition.dist_populations[2] == 41
    @test partition.dist_populations[3] == 41
    @test partition.dist_populations[4] == 41

    @testset "District Adjacency" begin
        for i in 1:graph.num_dists
            for j in 1:graph.num_dists
                @test partition.dist_adj[i,j] == partition.dist_adj[j,i]
            end
        end
    end

    @test partition.dist_adj[1,2] == 2
    @test partition.dist_adj[1,3] == 2
    @test partition.dist_adj[1,4] == 0

    @test partition.dist_nodes[1] == Set{Int}([1, 2, 5, 6])
    @test partition.dist_nodes[2] == Set{Int}([3, 4, 7, 8])
    @test partition.dist_nodes[3] == Set{Int}([9, 10, 13, 14])
    @test partition.dist_nodes[4] == Set{Int}([11, 12, 15, 16])

    @testset "Assignments" begin
        for i in [1, 2, 5, 6]
            @test partition.assignments[i] == 1
        end
        for i in [3, 4, 7, 8]
            @test partition.assignments[i] == 2
        end
        for i in [9, 10, 13, 14]
            @test partition.assignments[i] == 3
        end
        for i in [11, 12, 15, 16]
            @test partition.assignments[i] == 4
        end
    end
end;
