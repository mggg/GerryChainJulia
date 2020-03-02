@testset "Population Constraint" begin
    graph = BaseGraph(filepath, "population", "assignment")
    pop_constraint = PopulationConstraint(graph, "population", 0.1)
    balanced_proposal = RecomProposal(1, 3, 40, 42,
                                      Set{Int}([1, 2, 6]),
                                      Set{Int}([5, 9, 10, 13, 14]))

    @test satisfy_constraint(pop_constraint, balanced_proposal)

    unbalanced_proposal = RecomProposal(1, 3, 61, 21,
                                        Set{Int}([1, 2, 5, 6, 9]),
                                        Set{Int}([10, 13, 14]))

    @test !satisfy_constraint(pop_constraint, unbalanced_proposal)
end
