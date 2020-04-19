
@testset "Partisan Metrics tests" begin
    graph = BaseGraph(filepath, "population", "assignment")

    @testset "Mean-Median Tests" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("Test Election", "electionD", "electionR")
        update_elections!([election], graph, partition, DummyProposal(""), 1)
        @test mean_median(election, "electionD") == 0
        @test mean_median(election, "electionR") == 0
        @test_throws ArgumentError mean_median(election, "fake_party")
    end
end
