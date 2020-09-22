@testset "Population Constraint" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    pop_constraint = PopulationConstraint(graph, 0.1)
    balanced_proposal = RecomProposal(1, 3, 40, 42,
                                      BitSet([1, 2, 6]),
                                      BitSet([5, 9, 10, 13, 14]))

    @test satisfy_constraint(pop_constraint, balanced_proposal)

    unbalanced_proposal = RecomProposal(1, 3, 61, 21,
                                        BitSet([1, 2, 5, 6, 9]),
                                        BitSet([10, 13, 14]))

    @test !satisfy_constraint(pop_constraint, unbalanced_proposal)

    @test satisfy_constraint(pop_constraint, 40, 42)
    @test !satisfy_constraint(pop_constraint, 61, 21)
end

@testset "Contiguity Constraint" begin
    graph = BaseGraph(cols_grid_filepath, "population", "assignment")
    partition = Partition(graph, "assignment")
    cont_constraint = ContiguityConstraint()
    discont_proposal = FlipProposal(5, 1, 0, 30, 42,
                                    BitSet([1, 9, 13]),
                                    BitSet([0, 4, 5, 8, 12]))

    @test !satisfy_constraint(cont_constraint, graph, partition, discont_proposal)

    cont_proposal = FlipProposal(1, 1, 0, 30, 42,
                                 BitSet([5, 9, 13]),
                                 BitSet([0, 1, 4, 8, 12]))

    @test satisfy_constraint(cont_constraint, graph, partition, cont_proposal)
end
