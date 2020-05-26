
@testset "Election tests" begin
    graph = BaseGraph(filepath, "population", "assignment")

    @testset "update_elections_detailed()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("Test Election", Array{AbstractString, 1}(["electionD", "electionR"]))
        update_elections!([election], graph, partition, DummyProposal(""), 1)
        @test vote_counts_by_district(election, "electionD") == [6, 6, 6, 6]
        @test vote_counts_by_district(election, "electionR") == [6, 6, 6, 6]
        @test vote_shares_by_district(election, "electionD") == [0.5, 0.5, 0.5, 0.5]
        @test vote_shares_by_district(election, "electionR") == [0.5, 0.5, 0.5, 0.5]
    end

    @testset "update_elections_Δ()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("Test Election", Array{AbstractString, 1}(["electionD", "electionR"]))

        # Seed step
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        # make a proposal and update Partition and Election
        proposal = RecomProposal(1, 2, 100, 100, BitSet([1, 5, 6, 7]), BitSet([2, 3, 4, 8]))
        update_partition!(partition, graph, proposal)
        update_elections!([election], graph, partition, proposal, 2)

        @test vote_counts_by_district(election, "electionD") == [5, 7, 6, 6]
        @test vote_counts_by_district(election, "electionR") == [9, 3, 6, 6]
        @test vote_shares_by_district(election, "electionD") == [5/14, 7/10, 6/12, 6/12]
        @test vote_shares_by_district(election, "electionR") == [9/14, 3/10, 6/12, 6/12]
    end

    @testset "Getting Election Results" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("Test Election", Array{AbstractString, 1}(["electionD", "electionR"]))

        # Seed step
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        # Take two steps so the vote counts are not equal in any district
        proposal₁ = RecomProposal(1, 2, 100, 100, BitSet([1, 5, 6, 7]), BitSet([2, 3, 4, 8]))
        update_partition!(partition, graph, proposal₁)
        update_elections!([election], graph, partition, proposal₁, 2)

        proposal₂ = RecomProposal(3, 4, 100, 100, BitSet([9, 10, 11, 13]), BitSet([12, 14, 15, 16]))
        update_partition!(partition, graph, proposal₂)
        update_elections!([election], graph, partition, proposal₂, 3)

        @test seats_won(election, "electionD") == 2
        @test seats_won(election, "electionR") == 2
        # @test_throws ArgumentError seats_won(election, "fake_party")

        @test total_vote_counts(election, "electionD") == 24
        @test total_vote_counts(election, "electionR") == 24
        # @test_throws ArgumentError total_vote_counts(election, "fake_party")

        @test vote_counts_by_district(election, "electionD") == [5, 7, 7, 5]
        @test vote_counts_by_district(election, "electionR") == [9, 3, 3, 9]
        # @test_throws ArgumentError vote_counts_by_district(election, "fake_party")

        @test vote_shares_by_district(election, "electionD") == [5/14, 7/10, 7/10, 5/14]
        @test vote_shares_by_district(election, "electionR") == [9/14, 3/10, 3/10, 9/14]
        # @test_throws ArgumentError vote_shares_by_district(election, "fake_party")
    end
end
