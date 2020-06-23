using DataStructures

@testset "Flip tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(square_grid_filepath, graph, "population", "assignment")

    @testset "propose_random_flip()" begin
        flip_prop = GerryChain.propose_random_flip(graph, partition)
        pops = partition.dist_populations[[flip_prop.D₁, flip_prop.D₂]]
        @test sum(pops) == flip_prop.D₁_pop + flip_prop.D₂_pop
        @test flip_prop.D₁ != flip_prop.D₂
    end
end
