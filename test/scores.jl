
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Score tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(square_grid_filepath, graph, "population", "assignment")

    function calc_disparity(graph, nodes)
        diff = 0
        for node in nodes
            diff += graph.attributes[node]["purple"]
            diff -= graph.attributes[node]["pink"]
        end
        return diff
    end

    function cut_edges(graph, partition)
        return partition.num_cut_edges
    end

    @testset "constructors" begin
        @test_throws MethodError DistrictAggregate("abc", 1)
        # district scores and plan scores must take function
        @test_throws MethodError DistrictScore("abc", "abc")
        @test_throws MethodError PlanScore("abc", "abc")

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

        @test !isa(create_valid_district_aggregate_1(), Exception)
        @test !isa(create_valid_district_aggregate_2(), Exception)
        @test !isa(create_valid_district_score(), Exception)
        @test !isa(create_valid_plan_score(), Exception)
    end

    @testset "eval_score_on_district()" begin
        score_race = DistrictAggregate("purple")
        score_election_d = DistrictAggregate("electionD")
        @test eval_score_on_district(graph, partition, score_race, 1) == 28
        @test eval_score_on_district(graph, partition, score_election_d, 2) == 6

        broken_fn = x -> 2*x # district score functions must accept graph & node array
        bad_score = DistrictScore("break", broken_fn)
        @test_throws MethodError eval_score_on_district(graph, partition, bad_score, 1)

        race_gap = DistrictScore("race_gap", calc_disparity)
        @test eval_score_on_district(graph, partition, race_gap, 3) == -15
    end

    @testset "eval_score_on_partition()" begin
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
        @test_throws MethodError eval_score_on_partition(graph, partition, broken_score)

        cut_edges_score = PlanScore("cut_edges", cut_edges)
        @test eval_score_on_partition(graph, partition, cut_edges_score) == 8
    end

    @testset "score_initial_partition()" begin
        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictScore("race_gap", calc_disparity),
            PlanScore("cut_edges", cut_edges)
        ]

        score_vals = score_initial_partition(graph, partition, scores)
        @test score_vals["cut_edges"] == 8
        @test score_vals["purple"] == [28, 28, 13, 13]
        @test score_vals["pink"] == [13, 13, 28, 28]
        @test score_vals["electionD"] == [6, 6, 6, 6]
        @test score_vals["electionR"] == [6, 6, 6, 6]
        @test score_vals["race_gap"] == [15, 15, -15, -15]
    end

    @testset "score_partition_from_proposal()" begin
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)

        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictScore("race_gap", calc_disparity),
            PlanScore("cut_edges", cut_edges)
        ]

        score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        @test score_vals["dists"] == [1, 2]
        @test score_vals["purple"] == [34, 22]
        @test score_vals["pink"] == [17, 9]
        @test score_vals["electionD"] == [8, 4]
        @test score_vals["electionR"] == [6, 6]
        @test score_vals["race_gap"] == [17, 13]
    end

    @testset "get_scores_at_step()" begin
        all_scores = Array{Dict{String, Any}, 1}()
        scores = [
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictScore("race_gap", calc_disparity),
            PlanScore("cut_edges", cut_edges)
        ]
        init_score_vals = score_initial_partition(graph, partition, scores)
        push!(all_scores, init_score_vals)

        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        step_score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(all_scores, step_score_vals)

        parsed_scores = get_scores_at_step(all_scores, 1, scores=scores[1:2])
        @test sort(collect(keys(parsed_scores))) == sort(["purple", "pink"])
        for key in keys(parsed_scores)
            @test init_score_vals[key] == parsed_scores[key]
        end

        # actually update partition and then score it
        update_partition!(partition, graph, proposal)
        updated_score_vals = score_initial_partition(graph, partition, scores)
        parsed_scores = get_scores_at_step(all_scores, 2, scores=scores[5:6])
        @test sort(collect(keys(parsed_scores))) == sort(["race_gap", "cut_edges"])
        for key in keys(parsed_scores)
            @test updated_score_vals[key] == parsed_scores[key]
        end
    end
end
