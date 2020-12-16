using LightGraphs
using Random
using DataStructures

""" Refer to test/graph.jl to see the test graph being loaded
"""

# test random_weighted_kruskal_mst
@testset "Random Kruskal MST" begin
    graph = BaseGraph(square_grid_filepath, "population")

    rng = MersenneTwister(1234)
    nodes = [1, 2, 3, 4, 5, 6, 7, 8]
    edges = [graph.adj_matrix[1,2], graph.adj_matrix[2,3], graph.adj_matrix[3,4],
             graph.adj_matrix[5,6], graph.adj_matrix[6,7], graph.adj_matrix[7,8],
             graph.adj_matrix[1,5], graph.adj_matrix[2,6], graph.adj_matrix[3,7],
             graph.adj_matrix[4,8]]

    mst = random_kruskal_mst(graph, edges, nodes, rng)
    @test length(keys(mst)) == length(nodes)

    @test begin
        mst_edges = Set{Tuple{Int, Int}}()
        for src in keys(mst), dst in mst[src]
            push!(mst_edges, Tuple([min(src, dst), max(src, dst)]))
        end
        length(mst_edges) == length(nodes) - 1
    end

    @test begin # are there loops in the tree?
        # find by union-find algorithm
        connected_vs = DisjointSets{Int}(nodes)
        cycle_found = false

        mst_edges = Set{Tuple{Int, Int}}()
        for src in keys(mst), dst in mst[src]
            push!(mst_edges, Tuple([min(src, dst), max(src, dst)]))
        end

        for edge in mst_edges
            src, dst = edge
            if in_same_set(connected_vs, src, dst)
                cycle_found = true
                break
            else
                union!(connected_vs, src, dst)
            end
        end
        !cycle_found
    end
end
