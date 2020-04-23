
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

    @testset "Efficiency Gap Tests" begin
        graph = BaseGraph(filepath, "population", "assignment")
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("Test Election", "electionD", "electionR")

        # Seed elections
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        # Take two steps so the vote counts are not equal in any district
        proposal₁ = RecomProposal(1, 2, 100, 100, BitSet([1, 5, 6, 7]), BitSet([2, 3, 4, 8]))
        update_partition!(partition, graph, proposal₁)
        update_elections!([election], graph, partition, proposal₁, 2)

        proposal₂ = RecomProposal(3, 4, 100, 100, BitSet([9, 10, 11, 13]), BitSet([12, 14, 15, 16]))
        update_partition!(partition, graph, proposal₂)
        update_elections!([election], graph, partition, proposal₂, 3)

        @test efficiency_gap(election, "electionD") == 4/48
        @test efficiency_gap(election, "electionR") == -4/48
        @test_throws ArgumentError efficiency_gap(election, "fake party")
    end
end
