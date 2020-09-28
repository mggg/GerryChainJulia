@testset "Flip tests" begin
    graph = BaseGraph(square_grid_filepath, "population")
    partition = Partition(graph, "assignment")

    function accept_on_third_try()
        counter = 0
        return p -> (counter += 1) < 3 ? 0.0 : 1.0
    end

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
        pop_constraint = PopulationConstraint(graph, partition, 10.0)
        cont_constraint = ContiguityConstraint()
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

    @testset "no_self_loops" begin
        partition = Partition(graph, "assignment")
        # this is a dummy constraint
        pop_constraint = PopulationConstraint(graph, "population", 10.0)
        cont_constraint = ContiguityConstraint()
        scores = [
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
        ]
        num_steps = 1 # test 1 step (2 states) for now
        f = accept_on_third_try()
        chain_data = flip_chain(graph, partition, pop_constraint, cont_constraint, num_steps, scores, acceptance_fn=f)
        @test get_scores_at_step(chain_data, 0) == get_scores_at_step(chain_data, 1)
        # acceptance function should still return 0, because the acceptance function
        # should only have been called once
        @test f(nothing) == 0.0

        f = accept_on_third_try() # reset f
        chain_data = flip_chain(graph, partition, pop_constraint, cont_constraint, num_steps, scores, acceptance_fn=f, no_self_loops=true)
        # acceptance function should now return 1, because the acceptance function
        # should have been called until it started returning 1.0
        @test f(nothing) == 1.0
    end
end
