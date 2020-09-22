@testset "Flip tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(graph, "assignment")

    @testset "propose_random_flip()" begin
        flip_prop = GerryChain.propose_random_flip(graph, partition)
        pops = partition.dist_populations[[flip_prop.D₁, flip_prop.D₂]]
        # test that population counts look right
        @test sum(pops) == flip_prop.D₁_pop + flip_prop.D₂_pop
        # test that a node is not being 'flipped' to its original district
        @test flip_prop.D₁ != flip_prop.D₂
        neighbors = graph.neighbors[flip_prop.node]
        neighbor_districts = [partition.assignments[n] for n in neighbors]
        # a node should only be flipped to a district belonging to one of its
        # neighboring nodes
        @test flip_prop.D₂ in neighbor_districts
    end

    @testset "flip_chain()" begin
        # this is a dummy constraint
        pop_constraint = PopulationConstraint(graph,  10.0)
        cont_constraint = cont_constraint = ContiguityConstraint()
        scores = [
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
        ]
        num_steps = 1000

        function run_chain()
            try
                flip_chain(graph, partition, pop_constraint, cont_constraint, num_steps, scores)
            catch ex
                return ex
            end
        end
        # hacky way to run flip chain and test that it doesn't yield an exception
        @test !isa(run_chain(), Exception)
    end
end
