
@testset "Election tests" begin
    function update_vote_counts(graph, partition, election_tracker::CompositeScore)
        """ Helper function to update the vote counts. We could use
            `score_partition_from_proposal` or `score_initial_partition`, but
            having this small helper function helps ensure that these tests
            are relatively isolated from other parts of the codebase.
        """
        for d = 1:partition.num_dists
            # the first score should update all the vote counts
            election_tracker.scores[1].score_fn(graph, partition.dist_nodes[d], d)
        end
    end

    @testset "vote_updater()" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)
        election_tracker = ElectionTracker(election)

        update_vote_counts(graph, partition, election_tracker)

        # just test vote counts / shares on district 1
        @test election.vote_counts[1, 1] == 6.0 # votes for electionD
        @test election.vote_shares[1, 2] == 0.5 # votes for electionR
    end

    @testset "vote_count()" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)

        d_count = vote_count("votes_d", election, "electionD")
        r_count = vote_count("votes_r", election, "electionR")
        @test d_count isa DistrictScore
        @test r_count isa DistrictScore

        election_tracker = ElectionTracker(election, [d_count, r_count])
        update_vote_counts(graph, partition, election_tracker)

        num_d_votes = d_count.score_fn(graph, partition.dist_nodes[1], 1)
        @test num_d_votes == 6
        num_r_votes = r_count.score_fn(graph, partition.dist_nodes[1], 1)
        @test num_r_votes == 6
    end

    @testset "vote_share()" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)

        d_share = vote_share("share_d", election, "electionD")
        r_share = vote_share("share_r", election, "electionR")
        @test d_share isa DistrictScore
        @test r_share isa DistrictScore

        election_tracker = ElectionTracker(election, [d_share, r_share])
        update_vote_counts(graph, partition, election_tracker)

        perc_d = d_share.score_fn(graph, partition.dist_nodes[1], 1)
        @test perc_d == 0.5
        perc_r = r_share.score_fn(graph, partition.dist_nodes[1], 1)
        @test perc_r == 0.5
    end

    @testset "seats_won()" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)
        election_tracker = ElectionTracker(election)
        update_vote_counts(graph, partition, election_tracker)
        # Count seats won
        d_seats_score = seats_won("test", election, "electionD")
        @test d_seats_score isa PlanScore
        d_seats_won = d_seats_score.score_fn(graph, partition)
        @test d_seats_won == 0

        r_seats_score = seats_won("test", election, "electionR")
        @test r_seats_score isa PlanScore
        r_seats_won = r_seats_score.score_fn(graph, partition)
        @test r_seats_won == 0

        # modify election counts so Democrats win districts 1 & 2, Republicans
        # win districts 3 & 4
        graph.attributes[1]["electionD"] = 3
        graph.attributes[3]["electionD"] = 3
        graph.attributes[9]["electionD"] = 1
        graph.attributes[11]["electionD"] = 1
        update_vote_counts(graph, partition, election_tracker)

        d_seats_score = seats_won("test", election, "electionD")
        @test d_seats_score isa PlanScore
        d_seats_won = d_seats_score.score_fn(graph, partition)
        @test d_seats_won == 2

        r_seats_score = seats_won("test", election, "electionR")
        @test r_seats_score isa PlanScore
        r_seats_won = r_seats_score.score_fn(graph, partition)
        @test r_seats_won == 2
    end

    @testset "mean_median()" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)
        election_tracker = ElectionTracker(election)
        update_vote_counts(graph, partition, election_tracker)
        # Measure mean median
        d_mean_median = mean_median("test", election, "electionD")
        @test d_mean_median isa PlanScore
        d_mm_score = d_mean_median.score_fn(graph, partition)
        @test d_mm_score == 0

        r_mean_median = mean_median("test", election, "electionR")
        @test r_mean_median isa PlanScore
        r_mm_score = r_mean_median.score_fn(graph, partition)
        @test r_mm_score == 0

        # District 1 vote counts: 4 D, 6 R
        graph.attributes[1]["electionD"] = 0
        update_vote_counts(graph, partition, election_tracker)

        # Measure mean median
        d_mm_score = d_mean_median.score_fn(graph, partition)
        @test d_mm_score ≈ 1 / 40 # we use ≈ because of floating point issues

        r_mm_score = r_mean_median.score_fn(graph, partition)
        @test r_mm_score ≈ -1 / 40 # we use ≈ because of floating point issues
    end

    @testset "wasted_votes()" begin
        party₁_waste, party₂_waste = wasted_votes(52.0, 50.0)
        @test party₁_waste == 1.
        @test party₂_waste == 50.
    end

    @testset "efficiency_gap()" begin
        # in each district, D now win by 2 votes out of a total of 14 votes,
        # so wasted votes for R is (6*4) = 24, while wasted votes for D is
        # (1 * 4) = 4
        graph = BaseGraph(square_grid_filepath, "population")
        graph.attributes[1]["electionD"] = 4
        graph.attributes[3]["electionD"] = 4
        graph.attributes[9]["electionD"] = 4
        graph.attributes[11]["electionD"] = 4

        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)
        election_tracker = ElectionTracker(election)
        update_vote_counts(graph, partition, election_tracker)

        @test election.vote_counts[:, 1] == [8, 8, 8, 8] # votes for electionD
        @test election.vote_counts[:, 2] == [6, 6, 6, 6] # votes for electionD

        # Measure mean median
        d_gap = efficiency_gap("test", election, "electionD")
        @test d_gap isa PlanScore
        d_gap_score = d_gap.score_fn(graph, partition)
        @test d_gap_score ≈ (4 - 24) / (14 * 4)

        # efficiency gap should break when there are more than two elections
        bad_election = Election("Test Election", ["A", "B", "C"], partition.num_dists)
        a_gap = efficiency_gap("test", bad_election, "A")
        @test_throws ArgumentError a_gap.score_fn(graph, partition)
    end

    @testset "ElectionTracker updates votes" begin
        graph = BaseGraph(square_grid_filepath, "population")
        partition = Partition(graph, "assignment")
        election =
            Election("Test Election", ["electionD", "electionR"], partition.num_dists)
        election_tracker = ElectionTracker(election)
        update_vote_counts(graph, partition, election_tracker)

        @test election.vote_counts[:, 1] == [6, 6, 6, 6] # votes for electionD
        @test election.vote_counts[:, 2] == [6, 6, 6, 6] # votes for electionR
        @test election.vote_shares[:, 1] == [0.5, 0.5, 0.5, 0.5] # vote share for electionD
        @test election.vote_shares[:, 2] == [0.5, 0.5, 0.5, 0.5] # vote share for election R

        # make a proposal
        proposal₁ =
            RecomProposal(1, 2, 100, 100, BitSet([1, 5, 6, 7]), BitSet([2, 3, 4, 8]))
        # update partition with proposal
        update_partition!(partition, graph, proposal₁, false) # no custom acceptance function
        # run the vote count / share update after proposal
        update_vote_counts(graph, partition, election_tracker)

        @test election.vote_counts[:, 1] == [5, 7, 6, 6]
        @test election.vote_counts[:, 2] == [9, 3, 6, 6]
        @test election.vote_shares[:, 1] == [5 / 14, 7 / 10, 6 / 12, 6 / 12]
        @test election.vote_shares[:, 2] == [9 / 14, 3 / 10, 6 / 12, 6 / 12]

        proposal₂ =
            RecomProposal(3, 4, 100, 100, BitSet([9, 10, 11, 13]), BitSet([12, 14, 15, 16]))
        update_partition!(partition, graph, proposal₂, false) # no custom acceptance function
        # run the vote count / share update after proposal
        update_vote_counts(graph, partition, election_tracker)

        @test election.vote_counts[:, 1] == [5, 7, 7, 5] # votes for electionD
        @test election.vote_counts[:, 2] == [9, 3, 3, 9] # votes for electionR
        @test election.vote_shares[:, 1] == [5 / 14, 7 / 10, 7 / 10, 5 / 14] # vote share for electionD
        @test election.vote_shares[:, 2] == [9 / 14, 3 / 10, 3 / 10, 9 / 14] # vote share for election R
    end
end
