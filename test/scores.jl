
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Score tests" begin
    graph = BaseGraph(filepath, "population", "assignment")
    dist_score_keys = Array{NamedTuple, 1}()
    push!(dist_score_keys, (name="dems", key="electionD"))
    push!(dist_score_keys, (name="reps", key="electionR"))
    push!(dist_score_keys, (name="purples", key="purple"))
    push!(dist_score_keys, (name="pinks", key="pink"))

    @testset "get_scores() detailed" begin
        partition = Partition(filepath, graph, "population", "assignment")
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=(partition,)))

        scores = get_scores(graph, partition, dist_score_keys, plan_score_keys, 1)
        @test scores["num_cut_edges"] == 8  # TODO: remove this line
        @test sort(scores["purples"]) == sort([28, 28, 13, 13])
        @test sort(scores["pinks"]) == sort([13, 13, 28, 28])
        @test sort(scores["dems"]) == sort([6, 6, 6, 6])
        @test sort(scores["reps"]) == sort([6, 6, 6, 6])
    end

    @testset "get_scores() Δ" begin
        partition = Partition(filepath, graph, "population", "assignment")
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=(partition,)))

        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        scores = get_scores(graph, partition, dist_score_keys, plan_score_keys, 2, proposal)
        @test scores["num_cut_edges"] == 9 #TODO: remove this line
        @test sort(scores["purples"]) == sort([34, 22])
        @test sort(scores["pinks"]) == sort([17, 9])
        @test sort(scores["dems"]) == sort([8, 4])
        @test sort(scores["reps"]) == sort([6, 6])
    end

    @testset "get_scores_at_step()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=(partition,)))

        all_scores = Array{Dict{String, Any}, 1}()
        scores = get_scores(graph, partition, dist_score_keys, plan_score_keys, 1)
        push!(all_scores, scores)

        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        Δ_scores = get_scores(graph, partition, dist_score_keys, plan_score_keys, 2, proposal)
        push!(all_scores, Δ_scores)
        detailed_scores = get_scores(graph, partition, dist_score_keys, plan_score_keys, 1)

        parsed_scores = get_scores_at_step(all_scores, dist_score_keys, plan_score_keys, 2)
        @test keys(detailed_scores) == keys(parsed_scores)
        for key in keys(detailed_scores)
            @test detailed_scores[key] == parsed_scores[key]
        end
    end
end
