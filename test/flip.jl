using DataStructures

@testset "Flip tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(square_grid_filepath, graph, "population", "assignment")

    @testset "propose_random_flip()" begin
        flip_prop = GerryChain.propose_random_flip(graph, partition)
        pops = partition.dist_populations[[flip_prop.D₁, flip_prop.D₂]]
        # test that population counts look right
        @test sum(pops) == flip_prop.D₁_pop + flip_prop.D₂_pop
        # test that a node is not being 'flipped' to its original district
        @test flip_prop.D₁ != flip_prop.D₂
        neighbors = graph.neighbors[flip_prop.Node]
        neighbor_districts = [partition.assignments[n] for n in neighbors]
        # a node should only be flipped to a district belonging to one of its
        # neighboring nodes
        @test flip_prop.D₂ in neighbor_districts
    end
end
