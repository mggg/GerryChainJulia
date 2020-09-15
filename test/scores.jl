
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Score tests" begin
    graph = BaseGraph(square_grid_filepath, "population")

    function calc_disparity(graph, nodes, district)
        diff = 0
        for node in nodes
            diff += graph.attributes[node]["purple"]
            diff -= graph.attributes[node]["pink"]
        end
        return diff
    end

    function district_void(dict)
        return (graph, nodes, district) -> dict["district_void"] = true
    end

    function plan_void(dict)
        return (graph, partition) -> dict["plan_void"] = true
    end

    @testset "constructors" begin
        @test_throws MethodError DistrictAggregate("abc", 1)
        # district scores and plan scores must take function
        @test_throws MethodError DistrictScore("abc", "abc")
        @test_throws MethodError PlanScore("abc", "abc")
        @test_throws MethodError CompositeScore("abc", "abc")
        @test_throws MethodError CompositeScore("abc", x -> 2*x)

        # series of functions that will help us ensure that instantiating
        # various scores does not result in an error
        function create_valid_district_aggregate_1()
            try
                DistrictAggregate("abc")
            catch ex
                return ex
            end
        end

        function create_valid_district_aggregate_2()
            try
                DistrictAggregate("abc", "abc")
            catch ex
                return ex
            end
        end

        function create_valid_district_score()
            try
                DistrictScore("abc", x -> 2*x)
            catch ex
                return ex
            end
        end

        function create_valid_plan_score()
            try
                DistrictScore("abc", x -> 2*x)
            catch ex
                return ex
            end
        end

        function create_valid_composite_score()
            try
                s₁ = DistrictScore("abc", x -> 1*x)
                s₂ = DistrictScore("abc", x -> 2*x)
                CompositeScore("abc", [s₁, s₂])
            catch ex
                return ex
            end
        end

        @test !isa(create_valid_district_aggregate_1(), Exception)
        @test !isa(create_valid_district_aggregate_2(), Exception)
        @test !isa(create_valid_district_score(), Exception)
        @test !isa(create_valid_plan_score(), Exception)
        @test !isa(create_valid_composite_score(), Exception)
    end

    @testset "eval_score_on_district()" begin
        partition = Partition(graph, "assignment")
        score_race = DistrictAggregate("purple")
        score_election_d = DistrictAggregate("electionD")
        @test eval_score_on_district(graph, partition, score_race, 1) == 28
        @test eval_score_on_district(graph, partition, score_election_d, 2) == 6

        broken_fn = x -> 2*x # district score functions must accept graph, node array, and district index
        bad_score = DistrictScore("break", broken_fn)
        @test_throws ArgumentError eval_score_on_district(graph, partition, bad_score, 1)

        race_gap = DistrictScore("race_gap", calc_disparity)
        @test eval_score_on_district(graph, partition, race_gap, 3) == -15
    end

    @testset "eval_score_on_partition()" begin
        partition = Partition(graph, "assignment")
        score_race = DistrictAggregate("purple")
        purples_by_district = [28, 28, 13, 13]
        @test eval_score_on_partition(graph, partition, score_race) == purples_by_district
        score_election_d = DistrictAggregate("electionD")
        d_by_district = [6, 6, 6, 6]
        @test eval_score_on_partition(graph, partition, score_election_d) == d_by_district

        race_gap = DistrictScore("race_gap", calc_disparity)
        gaps = [15, 15, -15, -15]
        @test eval_score_on_partition(graph, partition, race_gap) == gaps

        # PlanScore should take a graph & partition
        function bad_plan_fn(x)
            return x
        end
        broken_score = PlanScore("broken", bad_plan_fn)
        @test_throws ArgumentError eval_score_on_partition(graph, partition, broken_score)

        cut_edges_score = num_cut_edges("cut_edges")
        @test eval_score_on_partition(graph, partition, cut_edges_score) == 8

        group_score = CompositeScore("group", Array{AbstractScore, 1}([race_gap, cut_edges_score]))
        expected_results = Dict{}("race_gap" => [15, 15, -15, -15], "cut_edges" => 8)
        @test eval_score_on_partition(graph, partition, group_score) == expected_results
    end

    @testset "score_initial_partition()" begin
        partition = Partition(graph, "assignment")
        votes_d = DistrictAggregate("electionD")
        votes_r = DistrictAggregate("electionR")
        check_done = Dict("district_void" => false, "plan_void" => false)
        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictScore("race_gap", calc_disparity),
            num_cut_edges("cut_edges"),
            CompositeScore("votes", [votes_d, votes_r]),
            # create nameless scores that should not be recorded by the chain
            DistrictScore(district_void(check_done)),
            PlanScore(plan_void(check_done))
        ]

        score_vals = score_initial_partition(graph, partition, scores)
        @test score_vals["cut_edges"] == 8
        @test score_vals["purple"] == [28, 28, 13, 13]
        @test score_vals["pink"] == [13, 13, 28, 28]
        @test score_vals["votes"] == Dict{}("electionD" => [6, 6, 6, 6], "electionR" => [6, 6, 6, 6])
        @test score_vals["race_gap"] == [15, 15, -15, -15]
        @test ("d_void" in keys(score_vals)) == false
        @test ("p_void" in keys(score_vals)) == false
        # verify that scoring functions still ran
        @test check_done["district_void"]
        @test check_done["plan_void"]
    end

    @testset "score_partition_from_proposal()" begin
        partition = Partition(graph, "assignment")
        # create RecomProposal
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)

        votes_d = DistrictAggregate("electionD")
        votes_r = DistrictAggregate("electionR")
        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictScore("race_gap", calc_disparity),
            num_cut_edges("cut_edges"),
            CompositeScore("votes", [votes_d, votes_r])
        ]

        score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        @test score_vals["purple"] == [34, 22]
        @test score_vals["pink"] == [17, 9]
        @test score_vals["votes"] == Dict{}("electionD" => [8, 4], "electionR" => [6, 6])
        @test score_vals["race_gap"] == [17, 13]
        @test score_vals["cut_edges"] == 9
        @test score_vals["dists"] == [1, 2]
    end

    @testset "get_scores_at_step()" begin
        partition = Partition(graph, "assignment")
        # initialize scores
        votes_d = DistrictAggregate("electionD")
        votes_r = DistrictAggregate("electionR")
        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictScore("race_gap", calc_disparity),
            num_cut_edges("cut_edges"),
            CompositeScore("votes", [votes_d, votes_r])
        ]
        chain_data = ChainScoreData(scores, [])
        # get scores for initial plan
        init_score_vals = score_initial_partition(graph, partition, scores)
        push!(chain_data.step_values, init_score_vals)

        # generate RecomProposal, update partition, and generate new set of scores
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        step_score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(chain_data.step_values, step_score_vals)

        # fetch first two scores for step 1
        all_score_names = ["purple", "pink", "race_gap", "cut_edges", "votes"]
        parsed_scores = get_scores_at_step(chain_data, 0, score_names=all_score_names[1:2])
        @test sort(collect(keys(parsed_scores))) == sort(all_score_names[1:2])
        for key in keys(parsed_scores)
            @test init_score_vals[key] == parsed_scores[key]
        end

        updated_score_vals = score_initial_partition(graph, partition, scores)
        parsed_scores = get_scores_at_step(chain_data, 1, score_names=all_score_names[3:5])
        @test sort(collect(keys(parsed_scores))) == sort(all_score_names[3:5])
        for key in keys(parsed_scores)
            @test updated_score_vals[key] == parsed_scores[key]
        end

        # passing in an empty array should yield all scores
        parsed_scores = get_scores_at_step(chain_data, 1)
        @test sort(collect(keys(parsed_scores))) == sort(all_score_names)
        for key in keys(parsed_scores)
            @test updated_score_vals[key] == parsed_scores[key]
        end
    end

    @testset "get_score_values()" begin
        partition = Partition(graph, "assignment")
        # initialize scores
        all_scores = Array{Dict{String, Any}, 1}()
        votes_d = DistrictAggregate("electionD")
        votes_r = DistrictAggregate("electionR")
        scores = [
            DistrictAggregate("purple"),
            num_cut_edges("cut_edges"),
            CompositeScore("votes", [votes_d, votes_r])
        ]
        chain_data = ChainScoreData(scores, [])
        # get scores for initial plan
        init_score_vals = score_initial_partition(graph, partition, scores)
        push!(chain_data.step_values, init_score_vals)

        # generate RecomProposal, update partition, and generate new set of scores
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        step_score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(chain_data.step_values, step_score_vals)

        # check that return values look correct
        purple_vals = get_score_values(chain_data, "purple")
        @test size(purple_vals) == (2, 4)
        @test purple_vals == [[28 28 13 13]; [34 22 13 13]]
        cut_edge_vals = get_score_values(chain_data, "cut_edges")
        @test size(cut_edge_vals) == (2,)
        @test cut_edge_vals == [8, 9]
        vote_vals = get_score_values(chain_data, "votes")
        @test vote_vals isa Dict
        @test vote_vals == Dict{}("electionD" => [[6 6 6 6]; [8 4 6 6]],
                                  "electionR" => [[6 6 6 6]; [6 6 6 6]])
        d_vote_vals = get_score_values(chain_data, "electionD")
        @test size(d_vote_vals) == (2, 4)
        @test d_vote_vals == [[6 6 6 6]; [8 4 6 6]]
        @test_throws ArgumentError get_score_values(chain_data, "nonexistent")
    end
end
