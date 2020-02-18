# Partition tests

# the graph being loaded is labeled as
#
#  01 - 02 - 03 - 04
#   |    |   |    |
#  05 - 06 - 07 - 08
#   |    |   |    |
#  09 - 10 - 11 - 12
#   |    |   |    |
#  13 - 14 - 15 - 16

# the initial assignment is

#  01 - 01 - 02 - 02
#   |    |   |    |
#  01 - 01 - 02 - 02
#   |    |   |    |
#  03 - 03 - 04 - 04
#   |    |   |    |
#  03 - 03 - 04 - 04

# the population distribution is

#  20 - 10 - 10 - 10
#   |    |   |    |
#  01 - 10 - 20 - 01
#   |    |   |    |
#  20 - 10 - 10 - 10
#   |    |   |    |
#  01 - 10 - 01 - 20

# partition tests
@testset "Partition tests" begin
    raw_graph = JSON.parsefile(filepath)
    graph = Graph(raw_graph)
    partition = Partition(graph, raw_graph, "population", "assignment")
    @test partition.num_districts == 4
    @test partition.num_cut_edges == 8
    @test partition.total_population == 164
    @test partition.district_populations[1] == 41
    @test partition.district_populations[2] == 41
    @test partition.district_populations[3] == 41
    @test partition.district_populations[4] == 41

    @testset "District Adjacency" begin
        for i=1:partition.num_districts
            for j=1:partition.num_districts
                @test partition.district_adj[i,j] == partition.district_adj[j,i]
            end
        end
    end
    @test partition.district_adj[1,2] == 2
    @test partition.district_adj[1,3] == 2
    @test partition.district_adj[1,4] == 0

    @test partition.district_nodes[1] == Set{Int}([1, 2, 5, 6])
    @test partition.district_nodes[2] == Set{Int}([3, 4, 7, 8])
    @test partition.district_nodes[3] == Set{Int}([9, 10, 13, 14])
    @test partition.district_nodes[4] == Set{Int}([11, 12, 15, 16])

    @test partition.assignments[1] == 1
    @test partition.assignments[2] == 1
    @test partition.assignments[5] == 1
    @test partition.assignments[6] == 1
    @test partition.assignments[3] == 2
    @test partition.assignments[4] == 2
    @test partition.assignments[7] == 2
    @test partition.assignments[8] == 2
    @test partition.assignments[9] == 3
    @test partition.assignments[10] == 3
    @test partition.assignments[13] == 3
    @test partition.assignments[14] == 3
    @test partition.assignments[11] == 4
    @test partition.assignments[12] == 4
    @test partition.assignments[15] == 4
    @test partition.assignments[16] == 4

    @test partition.populations[1] == 20
    @test partition.populations[2] == 10
    @test partition.populations[3] == 10
    @test partition.populations[4] == 10
    @test partition.populations[5] == 1
    @test partition.populations[6] == 10
    @test partition.populations[7] == 20
    @test partition.populations[8] == 1
    @test partition.populations[9] == 20
    @test partition.populations[10] == 10
    @test partition.populations[11] == 10
    @test partition.populations[12] == 10
    @test partition.populations[13] == 1
    @test partition.populations[14] == 10
    @test partition.populations[15] == 1
    @test partition.populations[16] == 20
end;
