@testset "Population Constraint" begin
    graph = BaseGraph(filepath, "population", "assignment")
    pop_constraint = PopulationConstraint(graph, "population", 0.1)
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
