
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

@testset "Correctness of MST" begin
    graph = BaseGraph(square_grid_filepath, "population")

    nodes = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
    heavy_edges = [Edge(1, 2), Edge(2, 3), Edge(3, 4), Edge(4, 8),
                   Edge(7, 8), Edge(6, 7), Edge(5, 6), Edge(5, 9),
                   Edge(9, 10), Edge(10, 11), Edge(11, 12), Edge(12, 16),
                   Edge(15, 16), Edge(14, 15), Edge(13, 14)]

    is = Array{Int}([]) # store edge indices
    weights = Array{Float64}([])
    correct_mst = BitSet()

    for (i, e) in enumerate(LightGraphs.edges(graph.simple_graph))
        push!(is, i)
        if e in heavy_edges
            push!(weights, 0.0)
            push!(correct_mst, i)
        else
            push!(weights, 1.0)
        end
    end

    @test correct_mst == GerryChain.kruskal_mst(graph, is, nodes, weights)
end
